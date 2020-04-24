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

(* Utility sub-modules *)
module Varstate =
struct 
  include Varstate
end

module Factor =
struct 
  include Factor
end

(*
 * Type declarations
 *)

type network = {schema: int array;
                factors: Factor.factor array;
                var_to_factors: Factor.factor list array}

let numvars mn = Array.length mn.schema
let numweights mn = Array.sum_map Factor.numparams mn.factors
let get_range mn i = mn.schema.(i) 
let num_values = get_range
let schema mn = Array.copy mn.schema
let factors mn = mn.factors

let build_var_to_factors schema factors =
  let a = Array.make (Array.length schema) [] in
  let add_factor f var = a.(var) <- f :: a.(var) in
  Array.iter (fun f -> List.iter (add_factor f) (Factor.vars f)) factors;
  a

let create schema factors =
  let var_to_factors = build_var_to_factors schema factors in
  {schema=schema; factors=factors; var_to_factors=var_to_factors}

(* Unnormalized log probability of a given state. *)
let raw_logprob mn state =
  if Array.exists (fun x -> x < 0) state then
    invalid_arg "raw_logprob"
  else 
    Array.sumf_map (Factor.log_value state) mn.factors

(* Log probability distribution over X_i, given its Markov blanket *)
let mb_logdist mn x i = 
  (*if Array.exists (fun x -> x < 0) x then
    invalid_arg "mb_logdist";
  *)
  let orig_ival = x.(i) in
  let valsum v =
    x.(i) <- v;
    List.sumf_map (Factor.log_value x) mn.var_to_factors.(i) in
  let dist = Array.init mn.schema.(i) valsum in
  x.(i) <- orig_ival;
  normalize_log dist

(* Pseudo-log-likelihood *)
let pll mn x = 
  if Array.length x != Array.length mn.schema then
    invalid_arg "pll"
  else begin
    let mb_logscore mn x i = 
      (mb_logdist mn x i).(x.(i)) in
    let total = ref 0. in
    for i = 0 to Array.length mn.schema - 1 do
      total := !total +. mb_logscore mn x i
    done;
    !total
  end

(* Build a new MN with simpler factors, conditioned on evidence *)
let simplify mn ev =
  let schema = Array.copy mn.schema in
  let factors = Array.map (Factor.simplify ev) mn.factors in
  let var_to_factors = build_var_to_factors schema factors in
  {schema=schema;
   factors=factors;
   var_to_factors=var_to_factors}

let to_features mn =
  List.flatten (Array.to_list (Array.map Factor.to_features mn.factors))

(* Update weights using a weight vector.
 * Modifies the MN in place. *)
let set_weights mn w = 
  let wi = ref 0 in
  for i = 0 to Array.length mn.factors - 1 do
    dlogf "Setting weights for factor %d\n" i;
    let (f', nw) = Factor.set_weights w !wi mn.factors.(i) in
    dlogf "Number of weights: %d\n" nw;
    mn.factors.(i) <- f';
    wi := !wi + nw
  done;
  (* Update var to factors mapping for new factors *)
  let new_v2f = build_var_to_factors mn.schema mn.factors in
  for i = 0 to Array.length mn.schema - 1 do
    mn.var_to_factors.(i) <- new_v2f.(i)
  done

let input_features_lex lexbuf =
  let fl = MnParser.featurelist MnLexer.lexer lexbuf in
  let pf2f (weight_id, weight, cond) =
    {Factor.weight_id=weight_id; 
     Factor.weight=weight; 
     Factor.cond=Array.of_list cond} in
  List.rev (List.rev_map pf2f fl)

let input_features chan =
  let lexbuf = Lexing.from_channel chan in
  input_features_lex lexbuf

(* I/O *)
module MP = MnParseTypes

(* Infer filetype from filename *)
let filename_is_mn filename =
  Str.string_match (Str.regexp ".*\\.mn.*") filename 0

let filename_is_uai filename =
  Str.string_match (Str.regexp ".*\\.uai.*") filename 0
  || Str.string_match (Str.regexp ".*\\.simple.*") filename 0

let load chan =
  let schema = Data.input_example chan in
  let lexbuf = Lexing.from_channel chan in
  let pmn = MnParser.mn MnLexer.lexer lexbuf in
  (* Convert the parsed factors to actual factors.
     TODO: Eventually support shared weights. *)
  let factors_l = 
    List.rev (List.rev_map (Factor.pfactor_to_factor schema) pmn.MP.factors) in
  let factors = Array.of_list factors_l in 
  let var_to_factors = build_var_to_factors schema factors in
  {schema=schema;
   factors=factors;
   var_to_factors=var_to_factors}

let load_uai chan =
  let (schema, factors) = UaiFormat.input chan in
  create schema factors

let output out mn =
  Data.output_schema out mn.schema;
  output_string out "MN {\n";
  Array.iter (Factor.output_factor out) mn.factors;
  output_string out "}\n"

let output_uai out mn =
  UaiFormat.output out mn.schema mn.factors 

(* Load an MN, inferring its filetype by its filename *)
let load_auto filename =
  let channel = open_in filename in
  let mn = 
    if filename_is_mn filename then 
      load channel
    else if filename_is_uai filename then 
      load_uai channel
    else
      (* Unknown filetype.  Assume it's a .mn. *)
      load channel in
  close_in channel; mn
  
(* Write an MN to disk, inferring its filetype by its filename *)
let write_auto filename mn = 
  let channel = open_out filename in
  if filename_is_mn filename then 
    output channel mn
  else if filename_is_uai filename then 
    output_uai channel mn
  else
    (* Unknown filetype.  Assume it's a .mn. *)
    output channel mn ;
  close_out channel
