(******************************************************************************)
(* The Libra Toolkit                                                          *)
(*                                                                            *)
(* Copyright (C) 2015 by Daniel Lowd and Amirmohammad Rooshenas               *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided that the following conditions are     *)
(* met:                                                                       *)
(*                                                                            *)
(* 1. Redistributions of source code must retain the above copyright          *)
(* notice, this list of conditions and the following disclaimer.              *)
(*                                                                            *)
(* 2. Redistributions in binary form must reproduce the above copyright       *)
(* notice, this list of conditions and the following disclaimer in the        *)
(* documentation and/or other materials provided with the distribution.       *)
(*                                                                            *)
(* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        *)
(* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          *)
(* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR      *)
(* A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT       *)
(* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      *)
(* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT           *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,      *)
(* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY      *)
(* THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT        *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE      *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.       *)
(******************************************************************************)

open Printf
open Ext
open Mn

include BnType 

type schema_t = int array

(* Accessors *)
let numvars bn = Array.length bn.vars
let varname bn i = bn.vars.(i).vname
let idx v = v.idx 
let parents bn i = bn.parents.(i)
let children bn i = bn.children.(i)

let numparents bn i = List.length bn.parents.(i) 
let numchildren bn i = List.length bn.children.(i)
let numparams bn i = dist_params bn.dists.(i)

(* SPEED HACK:
 *   Simplify tree-structured CPDs given evidence.  This speeds up
 *   inference slightly by reducing the depth of each tree.  This
 *   modifies the BN.
 * 
 * NOTE: Resulting CPDs may not be normalized, and hence be merely
 * potential functions, not conditional probability distributions. 
 *)

let rec simplify_node v x n =
  match n with 
    Leaf a -> 
      if x.(v) < 0 || Array.length a = 1 then Leaf a
      else Leaf [|a.(x.(v))|]
  | Vertex (var,value,left,right) ->
      let l () = simplify_node v x left in
      let r () = simplify_node v x right in
      if x.(var) < 0 then
        Vertex (var, value, l(), r())
      else if x.(var) = value then
        l()
      else
        r()

let simplify bn x =
  let simplify_dist var = function
    | Table t -> Table t (* TODO: simplify tables as well...? *)
    | Tree root -> Tree (simplify_node var x root) 
    | FactorSet fl -> 
        FactorSet (List.map (Mn.Factor.simplify x) fl) in
  let dists' = Array.mapi simplify_dist bn.dists in
  {bn with dists=dists'}


(*
 * Get the conditional probability of a variable, given its parents.
 *)

(* Get a conditional probability from a tree-based CPD *)
let rec tree_logprob x = function
  | Leaf a -> a
  | Vertex (var, value, left, right) ->
      if x.(var) = value then 
        tree_logprob x left
      else
        tree_logprob x right 

(* Log probability of the child variable given its parents. *)
let cond_logprob bn state v =
  match bn.dists.(v) with
    Table t -> t.(state_to_pidx bn v state)
  | Tree t ->  tree_logprob state t
  | FactorSet fl -> 
    let v_old = state.(v) in
    let dist = Array.make (num_values bn v) 0.0 in
    for i = 0 to Array.length dist - 1 do
      state.(v) <- i;
      dist.(i) <- List.sumf_map (Mn.Factor.log_value state) fl
    done;
    state.(v) <- v_old;
    normalize_inplace_log dist;
    dist

(* Log probability of the variable's current state (X_i = x_i), 
 * given its parents (Pi_i = pi_i) *)
let node_logscore bn state v =
  let la = cond_logprob bn state v in
  (* HACK: We have fewer probs if the network was simplified. *)
  if Array.length la = 1 then 
    la.(0) 
  else 
    la.(state.(v)) 

(* Log probability of X_i given its Markov blanket *)
let mb_logprob bn x i = 
  if not bn.acyclic then
    cond_logprob bn x i
  else begin
    (* For variable i and each neighbor, compute conditional log prob
     * of each value conditioned on everything else.
     *)
    let orig_ival = x.(i) in
    let childprob ival =
      x.(i) <- ival;
      List.sumf_map (node_logscore bn x) (i :: children bn i) in
    let logprobs = Array.init (get_range bn i) childprob in
    x.(i) <- orig_ival;

    (* Normalize into the Markov blanket distribution *)
    (* TODO: Beware of underflow! *)
    let probs = Array.map exp logprobs in
    let logtotal = log (Array.sumf probs) in
    Array.map (fun p -> p -. logtotal) logprobs
  end


(* Probabilities *)
let mb_prob bn state v = 
  Array.map exp (mb_logprob bn state v)

let cond_prob bn state v =
  Array.map exp (cond_logprob bn state v)


(* 
 * Compute the log likelihood of the network on a single example.  
 * All values must be specified -- no missing data! 
 *)
let loglikelihood bn x =
  let total = ref 0. in
  for v = 0 to numvars bn - 1 do
    assert(x.(v) >= 0);
    total := !total +. node_logscore bn x v  
  done;
  !total

let pll bn x =
  let total = ref 0. in
  for v = 0 to numvars bn - 1 do
    assert(x.(v) >= 0);
    let value = x.(v) in
    total := !total +. (mb_logprob bn x v).(value)
  done;
  !total

(* Utility function for sampling a multinomial given an array
   containing the probability of each state. *)
let sample_array a =
  (* assert(Array.sumf a < 1.0001 && Array.sumf a > 0.9999); *)
  let len = Array.length a in
  let p = ref (Random.float 1.0) in
  let idx = ref 0 in
  while !idx < len && !p > a.(!idx) do
    p := !p -. a.(!idx);
    incr idx
  done;
  if !idx < len then 
    !idx
  else
    len - 1

(* 
 * Generate a single iid sample from the BN probability distribution. 
 *)
let sample bn =
  if not bn.acyclic then
    invalid_arg "sample"
  else begin
    let x = Array.make (numvars bn) 0 in
    let sample_from_parents var =
      let v = var.idx in
      x.(v) <- sample_array (cond_prob bn x v) in
    Array.iter sample_from_parents bn.topo_vars ;
    x
  end


(*
 * File input/output
 *)
let load_bif  = BifFile.load
let load_xmod = XmodFile.load
let load_cn   = CnFile.load

let output_bif  = BifFile.output
let output_xmod = XmodFile.output
let output_cn   = CnFile.output

(* Infer filetype from filename *)
let filename_is_xmod filename =
  Str.string_match (Str.regexp ".*\\.xmod$") filename 0

let filename_is_bif filename =
  Str.string_match (Str.regexp ".*\\.bif$") filename 0

let filename_is_cn filename =
  Str.string_match (Str.regexp ".*\\.bn$") filename 0
  || Str.string_match (Str.regexp ".*\\.dn$") filename 0
  || Str.string_match (Str.regexp ".*\\.cn$") filename 0

let filename_is_dn filename =
  Str.string_match (Str.regexp ".*\\.dn.*") filename 0

(* Load a BN, inferring its filetype by its filename *)
let load_auto filename = 
  let channel = open_in filename in
  let bn = 
    if filename_is_xmod filename then 
      load_xmod channel
    else if filename_is_bif filename then 
      load_bif channel
    else if filename_is_cn filename then
      load_cn channel
    else
      (* Unknown filetype.  Assume it's a .cn. *)
      load_cn channel in
  close_in channel; bn

(* Write a BN to disk, inferring its filetype by its filename *)
let write_auto filename bn = 
  let channel = open_out filename in
  if filename_is_xmod filename then 
    output_xmod channel bn
  else if filename_is_bif filename then 
    output_bif channel bn
  else if filename_is_cn filename then 
    output_cn channel bn
  else
    (* Unknown filetype.  Assume it's a .cn. *)
    output_cn channel bn;
  close_out channel


