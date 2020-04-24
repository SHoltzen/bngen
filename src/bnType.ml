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

(*
 * Type declarations
 *)

type variable = {vname: string;
                 idx: int;
                 range: int;
                 valnames: string array}

type cpnode = (* array contains log conditional probabilities for this leaf *)
              Leaf of float array
              (* split var, value, true branch, false branch *)
            | Vertex of int * int * cpnode * cpnode 

type cpd = Table of float array array
         | Tree  of cpnode
         | FactorSet of Mn.Factor.factor list

type network = {name: string;
                mutable acyclic: bool;
                vars: variable array;
                (* Indices of parents and children for each var *)
                parents: int list array;
                dists: cpd array;
                (* The following fields can all be derived from the
                   previous fields using provided functions.  This
                   makes them redundant, but convenient. *)
                children: int list array; 
                name_to_varidx: (string, int) Hashtbl.t;
                name_to_validx: (string, int) Hashtbl.t array;
                topo_vars: variable array}

let numvars bn = Array.length bn.vars
let varname bn i = bn.vars.(i).vname
let get_range bn i = bn.vars.(i).range
let num_values = get_range
let schema bn = Array.map (fun v -> v.range) bn.vars

let rec tree_params = function
  | Leaf a -> Array.length a
  | Vertex (_,_,l,r) -> tree_params l + tree_params r

let dist_params = function
  | Table t -> 
    if Array.length t = 0 then 0
    else (Array.length t) * (Array.length t.(0))
  | Tree n -> tree_params n
  | FactorSet fl -> List.sum_map Mn.Factor.numparams fl


(*
 * Functions for creating Bayesian networks from scratch 
 *)

let create_var idx dim =
  {vname=(sprintf "Variable_%d" idx); 
   idx = idx;
   range = dim;
   valnames = Array.init dim string_of_int} 

let create_default_cpt var =
  let dim = var.range in 
  let uniform = Array.make dim (1. /. (float_of_int dim)) in
  Table [| uniform |]
 
(* Create hash maps from variable name -> index, and
 * from each variable's values -> value index
 *)
let build_namehashes vars =
  let numvars = Array.length vars in
  let varhash = Hashtbl.create 100 in
  let valhash = Array.init numvars (fun _ -> Hashtbl.create 100) in
  let proc_var v =
    Hashtbl.add varhash v.vname v.idx;
    Array.iteri (fun i s -> Hashtbl.add valhash.(v.idx) s i) v.valnames in
  Array.iter proc_var vars;
  (varhash, valhash)

(* Create array of child lists, given an array of parent lists *)
let make_children allparents = 
  let children = Array.make (Array.length allparents) [] in
  let add_child i i_parent =
    children.(i_parent) <- i :: children.(i_parent) in
  Array.iteri (fun i pl -> List.iter (add_child i) pl) allparents;
  children

(* Create list of variables in topographic order *)
let make_topo_vars vars allparents allchildren = 
  let numv = Array.length allchildren in 
  let a = Array.make numv vars.(0) in
  let nump v = List.length allparents.(v.idx) in 
  let parents_left = Array.map nump vars in 
  let i = ref 0 in
  let rec visited_parent v = 
    parents_left.(v) <- parents_left.(v) - 1;
    if parents_left.(v) = 0 then begin
      a.(!i) <- vars.(v);
      incr i;
      List.iter visited_parent allchildren.(v)
    end in
  for v = 0 to Array.length vars - 1 do
    if allparents.(v) = [] then begin 
      parents_left.(v) <- parents_left.(v) + 1;
      visited_parent v 
    end 
  done;
  a

(* Create an empty network with the given variable schema *)
let create_empty_network schema = 
  let numvars = Array.length schema in
  let vars = Array.mapi create_var schema in
  let cpts = Array.map create_default_cpt vars in
  let (varhash, valhash) = build_namehashes vars in
  let topo_vars = Array.copy vars in
  let children = Array.make numvars [] in
  let parents = Array.make numvars [] in
  {name = "empty";
   acyclic = true;
   vars = vars;
   parents = parents;
   children = children;
   dists = cpts;
   name_to_varidx = varhash;
   name_to_validx = valhash;
   topo_vars = topo_vars}

(* Update the children lists and topological variable order *)
let update_children_and_topo_vars bn =
  let new_children = make_children bn.parents in
  let new_topo = make_topo_vars bn.vars bn.parents new_children in 
  for i = 0 to (Array.length bn.vars) - 1 do
    bn.children.(i) <- new_children.(i);
    bn.topo_vars.(i) <- new_topo.(i)
  done

(* Update a CPT.  (Modifies BN in-place) *)
(* NOTE: cpt_values array is never copied! Caller should not modify. *)
let set_cpt bn var_idx parents cpt_values =
  bn.parents.(var_idx) <- parents;
  bn.dists.(var_idx) <- Table cpt_values;
  update_children_and_topo_vars bn

(* Convert a bitvector to a list of true indices *)
let bitvector_to_list a =
  let head = ref [] in
  for i = 0 to Array.length a - 1 do
    if a.(i) then
      head := i :: !head
  done;
  List.rev !head 

let factorset_parents numvars var fl =
  (* Build up bitvector of parents *)
  let parents = Array.make numvars false in
  let add_vars f =
    List.iter (fun v -> parents.(v) <- true) (Mn.Factor.vars f) in
  List.iter add_vars fl;
  parents.(var) <- false;
  bitvector_to_list parents

(* Get the parent list, given a tree *)
let tree_parents numvars root =
  let parents = Array.make numvars false  in
  let rec build_parents = function
    | Vertex(var, value, l, r) ->
        parents.(var) <- true;
        build_parents l; 
        build_parents r
    | Leaf _ -> () in
  build_parents root;
  let parents_l = ref [] in
  for i = 0 to Array.length parents - 1 do
    if parents.(i) then
      parents_l := i :: !parents_l
  done;
  bitvector_to_list parents

(* Update a tree-CPT.  (Modifies BN in-place) *)
let set_cptree bn var_idx root =
  bn.parents.(var_idx) <- tree_parents (numvars bn) root;
  bn.dists.(var_idx) <- Tree root;
  update_children_and_topo_vars bn


let set_factorset bn var_idx fl =
  bn.parents.(var_idx) <- factorset_parents (numvars bn) var_idx fl;
  bn.dists.(var_idx) <- FactorSet fl;
  update_children_and_topo_vars bn


(** Given a state `state` that conditions the parents of variable `v`, produce
    an index into the probability table *)
let state_to_pidx bn v state =
  let idx = ref 0 in
  let inc_idx p =
    idx := !idx * (get_range bn p) + state.(p);
  in
  List.iter inc_idx bn.parents.(v);
  !idx


(*
 * BN --> MN methods
 *)

(* Convert a tree with distributions at the leaves to one with
   potential function values at the leaves. *)
let rec tree_to_factor cvar = function
| Leaf a ->
  let rec make_leaf i =
    let l = Mn.Factor.Leaf a.(i) in
    if i = Array.length a - 1 then l
    else Mn.Factor.Vertex(cvar, i, l, make_leaf (i+1)) in
  make_leaf 0
| Vertex(var, value, left, right) ->
  let l' = tree_to_factor cvar left in
  let r' = tree_to_factor cvar right in
  Mn.Factor.Vertex(var, value, l', r')
  
let rec table_to_factor schema cvar parents t =
  if Array.length t == 0 then 
    [||]
  else
    let len = Array.length t in
    let getval i = t.(i mod len).(i/len) in
    Array.init (Array.length t * Array.length t.(0)) getval
  
let cpd_to_factors bn cvar = function
| Tree t -> 
  [Mn.Factor.Tree (tree_to_factor cvar t)]
| Table t -> 
  let t' = table_to_factor (schema bn) cvar bn.parents.(cvar) t in
  let vars = Array.of_list (cvar :: bn.parents.(cvar)) in
  let ranges = Array.map (get_range bn) vars in
  [Mn.Factor.Table (vars, ranges, t')]
| FactorSet fl -> fl

let to_mn bn =
  let schema = schema bn in
  let factors = Array.mapi (cpd_to_factors bn) bn.dists in
  Mn.create schema (Array.flattenl factors)
