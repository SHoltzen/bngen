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

(*
 * factor.ml: Module for representing a factor in a factor graph.
 * 
 * Supports many different types of representations.
 * 
 * Main operations on a factor:
 *
 * log_value and raw_value: 
 *   Get the value of a factor on a given instance, in log space or not.
 *
 * simplify:
 *   Simplify a factor by conditioning it on evidence.
 *
 * to_features:
 *   Convert factor to a set of conjunctive features.
 *
 * vars:
 *   Get list of vars used by a factor.
 *
 * Most other functions are helper for handling different factor types.
 *)

open Ext

type variable = int
type varvalue = int

type condition = bool * variable * varvalue

(** [valset] specifies a set of possible values for a variable,
    where the [i]th element is true if the variable can assume its
    [i]th value. *)
type valset = variable * bool array

type feature = {cond: condition array;
                weight_id: int; (* TODO: Eventually support tied weights. *)
                mutable weight: float}

type tree = (* array contains feature weight for this leaf *)
            Leaf of float
            (* split var, value, true branch, false branch *)
          | Vertex of variable * varvalue * tree * tree 

type factor = 
    Feature of feature
  | FeatureSet of feature list
  | Table of int array * int array * float array 
  | Tree of tree
  | Const of float

(* Simple sorting of conditions *)
let simple_cond_cmp (sense,var,value) (sense',var',value') = 
  if var <> var' then
    var - var'
  else if sense <> sense' then
    (if sense then (-1) else 1)
  else
    value - value'

(* Remove redundant conditions from a condition list.  Assumes that 
 * conditions are sorted by simple_cond_cmp and the condition is satisfiable
 * (not contradictory). *)
let rec remove_redundant_conds_rec lastposvar lastcond = function
 | c :: condl -> 
     let (sense,var,value) = c in
     let lastposvar' = if sense then var else lastposvar in
     let rest = remove_redundant_conds_rec lastposvar' c condl in
     (* Exclude condition if there's a positive condition for the
      * same variable, or if it's identical to the last condition. 
      * If conditions are sorted, negative conditions should follow
      * positive conditions for the same variable, and identical 
      * conditions should be grouped together. *)
     if var = lastposvar || c = lastcond then
       rest
     else
       c :: rest
 | [] -> []

let remove_redundant_conds condl = 
  remove_redundant_conds_rec (-1) (false,-1,-1) condl

(* Convert a list of conditions to a list of variable value sets,
 * so that each variable appears only once in the list. *)
let rec condl_to_valsetl_rec schema lastvar lastvalues = function
  | [] -> if lastvar < 0 then [] else [(lastvar, lastvalues)] 
  | (sense,var,value) :: condl ->
    (* Get conditions from last var, if new var is different *)
    let accu = 
      if var = lastvar || lastvar < 0 then [] 
      else [(lastvar, lastvalues)] in
    (* Create new satisfiable values array, if new var is different *)
    let values =
      if var = lastvar then lastvalues
      else Array.make schema.(var) true in
    (* Update values array with current condition *)
    for i = 0 to Array.length values - 1 do
      if (i = value) <> sense then
        values.(i) <- false
    done;
    (* Recurse *)
    accu @ (condl_to_valsetl_rec schema var values condl)

let condl_to_valsetl schema condl = 
  let condl = List.sort simple_cond_cmp condl in
  condl_to_valsetl_rec schema (-1) [||] condl


(*
 * Get value from each type of factor representation 
 *)

(*
let rec fmatch x = function
| (sense, var, value) :: l ->
    if x.(var) >= 0 && (x.(var) = value) <> sense then
      false
    else
      fmatch x l
| [] -> true 
*)

exception CondFail

let fmatch x cond = 
  try 
    for i = 0 to Array.length cond - 1 do
      let (sense, var, value) = cond.(i) in
      if x.(var) >= 0 && (x.(var) = value) <> sense then
        raise CondFail
    done;
    true
  with CondFail -> false

let fweight x f =
  if fmatch x f.cond then f.weight else 0.

let table_index (vars, ranges, t) x =
  let idx = ref 0 in
  for i = 0 to Array.length vars - 1 do
    idx := !idx * ranges.(i) + x.(vars.(i))
  done;
  !idx

let table_lookup (vars, ranges, t) x =
  t.(table_index (vars, ranges, t) x)

let rec tree_lookup x = function
| Leaf w -> w
| Vertex (var, value, left, right) ->
    if x.(var) = value then 
      tree_lookup x left 
    else 
      tree_lookup x right

(* Value of the factor, given all other values in x *)
let log_value x = function
| Feature feat -> fweight x feat
| FeatureSet fs -> List.sumf_map (fweight x) fs
| Table (var, values, t) -> table_lookup (var, values, t) x
| Tree t -> tree_lookup x t
| Const w -> w


(*
 * Get expected value from each type of factor representation,
 * given a fully factorized distribution.
 *)

let e_fweight logmarg negmarg f =
  let cond_to_prob (sense, var, value) =
    if sense then logmarg.(var).(value)
    else negmarg.(var).(value) in
  let logp = Array.sumf_map cond_to_prob f.cond in
  f.weight *. (exp logp)

(* TODO: Test. Maybe optimize. *)
let e_table_value (vars, ranges, t) logmarg negmarg = 
  (* Construct ad hoc schema *)
  let numvars = Array.max vars in
  let schema = Array.make numvars 1 in
  Array.iter2 (fun v r -> schema.(v) <- r) vars ranges;
  let table_prob x =
    let idx = Varstate.state_to_idx vars ranges x in
    let base_prob = 
      Array.sumf_map (fun v -> logmarg.(v).(x.(v))) vars in
    base_prob +. t.(idx) in
  List.sumf (Varstate.map_state schema (Array.to_list vars) table_prob)

let rec e_tree_value logmarg negmarg = function
| Leaf w -> w
| Vertex (var, value, left, right) ->
    (* Perform weighted sum of both branches. *)
    let l = e_tree_value logmarg negmarg left in
    let r = e_tree_value logmarg negmarg right in
    logsumexp2 (logmarg.(var).(value) +. l) (negmarg.(var).(value) +. r)

let expected_log_value f logmarg negmarg =
  match f with 
  | Feature feat -> e_fweight logmarg negmarg feat
  | FeatureSet fs -> List.sumf_map (e_fweight logmarg negmarg) fs
  | Table (var, values, t) -> e_table_value (var, values, t) logmarg negmarg
  | Tree t -> e_tree_value logmarg negmarg t
  | Const w -> w
  

let raw_value x f = exp (log_value x f)

(* Log probability distribution over all values of the specified variable,
   according to this factor. TODO -- Remove this?  Is it useful? 
let dist f x v dim =
  let oldval = x.(v) in
  let a = Array.init dim (fun i -> x.(v) <- i; log_value f x) in
  x.(v) <- oldval;
  normalize_inplace_log a;
  a
 *)

(*
 * Compute number of parameters in each factor.
 *)
let rec num_tree_params = function
  | Leaf w -> 1
  | Vertex (_,_,l,r) -> num_tree_params l + num_tree_params r

let numparams = function
  | Feature f -> 1
  | FeatureSet fl -> List.length fl
  | Table (vars, values, t) -> Array.length t
  | Tree t -> num_tree_params t
  | Const w -> 0


(*
 * Simplify a factor, given a state vector containing evidence.
 * Negative values represent unknowns; non-negative values represent
 * evidence.
 *)

(* Remove all irrelevant conditions.  If a list of conditions is
 * always or never satisfied by the evidence, returns []. *)
let rec simplify_condl ev accu = function
| (sense, var, value) :: l ->
  (* Check for disagreement -> never satisfied by evidence *)
  if ev.(var) >= 0 && (ev.(var) = value) <> sense then [||]
  else
    let accu = 
      if ev.(var) < 0 then (sense, var, value) :: accu
      (* If evidence satisfies condition, then we can omit the condition. *)
      else accu in
    simplify_condl ev accu l
| [] -> Array.of_list (List.rev accu)
    
(* Simplify the conditions of each feature, then select just those
 * that have non-trivial conditions. *)
let simplify_feature ev f = 
  {f with cond=simplify_condl ev [] (Array.to_list f.cond)}


let simplify_table ev vars ranges t =
  (* Only simplify table if evidence is relevant *)
  let vars' = Array.filter (fun v -> ev.(v) < 0) vars in
  if vars' = vars then
    Table (vars, ranges, t)
  else if vars' = [||] then
    Const (log_value ev (Table (vars,ranges,t)))
  else begin
    (* Construct an ad hoc schema *)
    let schema = Array.map (( + ) 1) ev in
    Array.iter2 (fun v r -> schema.(v) <- r) vars ranges;

    let ranges' = Array.map (fun v -> schema.(v)) vars' in
    let dim' = Array.fold_left ( * ) 1 ranges' in
    let t' = Array.make dim' 0.0 in
    let set_entry x =
      (* Set evidence *)
      Array.iteri (fun i value -> if value >= 0 then x.(i) <- value) ev;
      (* Get indices in old and new tables *)
      let idx = Varstate.state_to_idx vars ranges x in
      let idx' = Varstate.state_to_idx vars' ranges' x in
      (* Set table entry *)
      t'.(idx') <- t.(idx) in
    Varstate.iter_state schema (Array.to_list vars') set_entry;
    Table (vars', ranges', t')
  end

let rec simplify_node ev = function
| Leaf w -> Leaf w
| Vertex(var, value, left, right) ->
  if ev.(var) < 0 then
    let l' = simplify_node ev left in
    let r' = simplify_node ev right in
    Vertex(var, value, l', r')
  else if ev.(var) = value then
    simplify_node ev left
  else
    simplify_node ev right

let simplify_tree ev r =
  match simplify_node ev r with
  | Leaf w -> Const w
  | Vertex (var, value, left, right) ->
    Tree (Vertex (var, value, left, right))

(* Simplify a distribution, given evidence. *)
let simplify ev = function
| Feature f -> 
  let f' = simplify_feature ev f in
  if f'.cond = [||] then Const f'.weight else Feature f'
| FeatureSet fl ->
  let fl = List.map (simplify_feature ev) fl in
  let fl = List.filter (fun f -> f.cond <> [||]) fl in
  (match fl with 
   | [] -> Const 0.
   | [f] -> Feature f
   | f :: l -> FeatureSet (f :: l))
| Table (vars, ranges, t) ->
  simplify_table ev vars ranges t
| Tree t -> simplify_tree ev t
| Const w -> Const w


(*
 * Generate an equivalent set of features for a given factor
 *)
let rec tree_to_features accu = function
| Leaf w -> [{cond=Array.of_list accu; weight_id=(-1); weight=w}]
| Vertex(var, value, l, r) ->
  let accu_l = (true, var, value) :: accu in
  let accu_r = (false, var, value) :: accu in
  (* TODO -- make this tail recursive, via a second accumulator *)
  (tree_to_features accu_l l) @ (tree_to_features accu_r r)


(* Convert table CPD to features *)
let table_to_features (vars, ranges, t) =
  (* Create a makeshift schema *)
  let s = Array.make (Array.max vars + 1) 0 in
  Array.iter2 (fun var dim -> s.(var) <- dim) vars ranges;
  let table = Table (vars, ranges, t) in
  (* Get the conditional log probability for each specific of the 
     family (both parents and child) *)
  let f varstate = 
    let cond = Array.map (fun i -> (true, i, varstate.(i))) vars in
    let weight = log_value varstate table in
    {cond=cond; weight_id=(-1); weight=weight} in
  (* Loop through all configurations of the variables *)
  Varstate.map_state s (Array.to_list vars) f


let to_features = function
| Feature f -> [f]
| FeatureSet fl -> fl
| Table (vars, ranges, t) -> table_to_features (vars, ranges, t) 
| Tree t -> tree_to_features [] t
| Const w -> []

let to_features factor =
  let fl = to_features factor in
  let clean_feature f =
    Array.sort simple_cond_cmp f.cond;
    let condl = remove_redundant_conds (Array.to_list f.cond) in
    {f with cond=Array.of_list condl} in
  List.map clean_feature fl

(*
 * Setting weights
 *)

let rec set_tree_weights w wi = function
  | Leaf wt -> (Leaf w.(wi), 1)
  | Vertex (var, value, l, r) ->
    let (l', ln) = set_tree_weights w wi l in
    let (r', rn) = set_tree_weights w (wi + ln) r in
    (Vertex (var, value, l', r'), ln + rn)

(* UNTESTED! *)
let set_table_weights w wi (vars, ranges, t) =
  (* Create a makeshift schema *)
  let s = Array.make (Array.max vars + 1) 0 in
  Array.iter2 (fun var dim -> s.(var) <- dim) vars ranges;
  let t' = Array.make (Array.length t) 0.0 in
  (* Ugly -- imperative style works but is hackish here. *)
  let idx = ref wi in
  let f varstate = 
    t'.(!idx) <- w.(!idx);
    incr idx in
  (* Loop through all configurations of the variables *)
  Varstate.iter_state s (Array.to_list vars) f;
  (Table (vars, ranges, t'), Array.length t')

let set_weights w wi = function
  | Feature f -> 
    (Feature {f with weight = w.(wi)}, 1)
  | FeatureSet fl ->
    let fl' = List.mapi (fun i f -> {f with weight = w.(wi + i)}) fl in
    (FeatureSet fl', List.length fl')
  | Table (vars, ranges, t) -> 
    set_table_weights w wi (vars, ranges, t)
  | Tree t -> 
    let (root, nw) = set_tree_weights w wi t in
    (Tree root, nw)
  | Const w -> 
    (Const w, 0)


(*
 * Output
 *)

open Printf

let output_feature out f =
  let print_cond (sense, var, value) =
    fprintf out " %cv%d_%d" (if sense then '+' else '-') var value in
  if f.weight_id >= 0 then fprintf out "%d " f.weight_id;
  fprintf out "%e" f.weight;
  Array.iter print_cond f.cond;
  fprintf out "\n" 

let output_featurelist out fl =
  fprintf out "{\n";
  List.iter (output_feature out) fl;
  fprintf out "}\n\n"

let rec output_tree out s = function
| Leaf w -> 
    fprintf out " %e" w
| Vertex (var,value,l,r) ->
    fprintf out "\n%s(v%d_%d" s var value;
    let s' = s ^ "    " in
    output_tree out s' l;
    output_tree out s' r;
    fprintf out ")"


let output_factor out f =
  match f with 
  | Feature f -> output_feature out f
  | Tree t -> 
      begin match t with 
      | Leaf w -> fprintf out "tree {\n%f\n}\n" w
      | _ -> 
        output_string out "tree {";
        output_tree out "" t;
        output_string out "\n}\n\n"
      end
  | Const w -> ()
  | _ -> 
  begin
    let txt = match f with
      | FeatureSet fl -> "features"
      | Table (vars,ranges,t) -> "table"
      (* Last three cases are covered above. *)
      | Const w -> assert false
      | Feature f -> assert false 
      | Tree t -> assert false in
    output_string out (txt ^ " ");
    output_featurelist out (to_features f)
  end


(*
 * Retrieve set of variables mentioned in each factor
 *)
let feature_set_vars fl = 
  let h = Hashset.create 10 in
  let addcond (sense, var, value) = Hashset.add h var in
  List.iter (fun f -> Array.iter addcond f.cond) fl;
  Hashset.to_list h

let tree_vars root = 
  let h = Hashset.create 10 in
  let rec addnode = function
  | Vertex (var, value, l, r) -> 
    Hashset.add h var;
    addnode l;
    addnode r
  | Leaf _ -> () in
  addnode root;
  Hashset.to_list h

let vars = function
| Feature f -> feature_set_vars [f]
| FeatureSet fl -> feature_set_vars fl
| Table (vars, ranges, t) -> Array.to_list vars
| Tree t -> tree_vars t
| Const w -> []


(*
 * Convert a factor to a table
 *)

let to_table schema f =
  let vars_l = vars f in 
  let vars_a = Array.of_list vars_l in
  let ranges = Array.map (fun var -> schema.(var)) vars_a in
  let dim = Array.fold_right ( * ) ranges 1 in
  let t = Array.make dim 0.0 in
  let state = Array.make (Array.length schema) 0 in
  (* Set the value of each configuration *)
  begin try while true do
    let idx = Varstate.state_to_idx vars_a ranges state in
    t.(idx) <- log_value state f;
    Varstate.incstate schema state vars_l
  done with Varstate.NoMoreStates -> () end;
  Table (vars_a, ranges, t)

(* 
 * Duplicate factors, if mutable 
 *)
let copy_feature f =
  {cond=Array.copy f.cond; weight_id=f.weight_id; weight=f.weight} 

let copy = function
| Feature f -> 
    Feature (copy_feature f)
| FeatureSet fl -> 
    FeatureSet (List.map copy_feature fl)
| Table (vars, ranges, t) -> 
    Table (Array.copy vars, Array.copy ranges, Array.copy t)
| Tree t -> Tree t
| Const w -> Const w

(*
 * Rescale a factor by a factor of alpha.  Rescaling is done in log
 * space, so this is equivalent to raising a potential function to the
 * power of alpha.
 *)
let rescale_feature alpha f =
  {cond=Array.copy f.cond; weight_id=f.weight_id; weight=alpha *. f.weight} 

let rec rescale_tree alpha = function
 | Leaf w -> Leaf (alpha *. w)
 | Vertex(var, value, l, r) ->
   Vertex(var, value, rescale_tree alpha l, rescale_tree alpha r)

let rescale alpha = function
 | Feature f -> Feature (rescale_feature alpha f)
 | FeatureSet fl -> FeatureSet (List.map (rescale_feature alpha) fl)
 | Table (vars, ranges, t) -> 
     Table (Array.copy vars, Array.copy ranges, Array.map (( *. ) alpha) t)
 | Tree t -> 
     Tree (rescale_tree alpha t)
 | Const w -> Const (-.w)


(*
 * Input
 *)

module MP = MnParseTypes

let pf_to_f (weight_id, weight, condl) =
  (* Put feature condition lists in standard format. *)
  let condl = List.sort simple_cond_cmp condl in
  let condl = remove_redundant_conds condl in
  {cond=Array.of_list condl; weight_id=weight_id; weight=weight}

let pfl_to_factor pfl = FeatureSet (List.map pf_to_f pfl)

let rec ptree_to_tree = function
| MP.PLeaf w -> Leaf w
| MP.PVertex(var, value, l, r) -> 
    let l = ptree_to_tree l in
    let r = ptree_to_tree r in
    Vertex(var, value, l, r)

let pfactor_to_factor schema = function
| MP.PFeature pf -> Feature (pf_to_f pf)
| MP.PFeatureSet pfl -> pfl_to_factor pfl
| MP.PFeatureTree ptree -> Tree (ptree_to_tree ptree)
| MP.PFeatureTable pfl -> to_table schema (pfl_to_factor pfl) 
