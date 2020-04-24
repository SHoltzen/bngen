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

open Ext
open Printf
open Mn
open BnType
open Str

exception UnsupportedFactorType

(*
 * PRINTING a .xmod file
 *)

let output_var o var =
  fprintf o "\t\t<Variable name=\"%s\" type=\"categorical\">\n" var.vname;
  Array.iteri 
    (fprintf o "\t\t\t<State index=\"%d\" name=\"%s\"/>\n") var.valnames;
  fprintf o "\t\t</Variable>\n"

let rec rec_output_tree o bn tabs = function
| Leaf a ->
  fprintf o "%s<Leaf>\n" tabs;
  fprintf o "%s\t<Multinomial>\n" tabs;
  let prob_str = string_of_farray (Array.map exp a) in 
  fprintf o "%s\t\t<Probs>%s</Probs>\n" tabs prob_str;
  let count_str = string_of_iarray (Array.make (Array.length a) 0) in 
  fprintf o "%s\t\t<Counts>%s</Counts>\n" tabs count_str;
  fprintf o "%s\t</Multinomial>\n" tabs;
  fprintf o "%s</Leaf>\n" tabs

| Vertex(var, value, left, right) ->
  fprintf o "%s<Vertex split=\"%s\">\n" tabs (varname bn var);
  fprintf o "%s\t<Branch>\n" tabs;
  fprintf o "%s\t\t<Values>%d</Values>\n" tabs value;
  rec_output_tree o bn ("\t\t" ^ tabs) left;
  fprintf o "%s\t</Branch>\n" tabs;
  fprintf o "%s\t<Branch>\n" tabs;
  fprintf o "%s\t\t<Values>" tabs;
  (* TODO: Exclude all values that the ancestor splits made illegal, not
     just the one that was excluded by the other branch of this vertex. *)
  let print_space = ref false in
  for i = 0 to bn.vars.(var).range - 1 do
    if i <> value then begin
      if !print_space then
        fprintf o " "
      else
        print_space := true;
      fprintf o "%d" i
    end
  done;
  fprintf o "</Values>\n";
  rec_output_tree o bn ("\t\t" ^ tabs) right;
  fprintf o "%s\t</Branch>\n" tabs;
  fprintf o "%s</Vertex>\n" tabs


let output_table_entry o tabs state probs  =
  let state_str = String.concat " " 
    (Array.to_list (Array.map string_of_int state)) in
  let probs = Array.map exp probs in
  let prob_str = String.concat " " 
    (Array.to_list (Array.map string_of_float probs)) in
  fprintf o "%s\t<TableEntry>\n" tabs;
  fprintf o "%s\t\t<InputStates>%s</InputStates>\n" tabs state_str;
  fprintf o "%s\t\t<Multinomial>\n" tabs;
  fprintf o "%s\t\t\t<Probs>%s</Probs>\n" tabs prob_str;
  fprintf o "%s\t\t</Multinomial>\n" tabs;
  fprintf o "%s\t</TableEntry>\n" tabs

let output_table o bn tabs v table =
  fprintf o "%s<Table>\n" tabs;

  (* If there are no parents, just print the marginals *)
  if bn.parents.(v) = [] then 
    output_table_entry o tabs [||] table.(0)

  (* Else, print out each line (one for each parent config) *)
  else begin
    let varstate = Array.make (Array.length bn.vars) 0 in
    let lparents = List.rev bn.parents.(v) in
    let s = schema bn in
    let pa = Array.of_list bn.parents.(v) in
    try for i = 0 to Array.length table - 1 do
      let parentvals = Array.map (fun p -> varstate.(p)) pa in 
      output_table_entry o tabs parentvals table.(i);
      Varstate.incstate s varstate lparents
    done with Varstate.NoMoreStates -> ()
  end;
  fprintf o "%s</Table>\n" tabs


let output_dist o bn tabs i =
  fprintf o "%s<LocalModel variable=\"%s\">\n" tabs (varname bn i);
  if bn.parents.(i) <> [] then begin
    fprintf o "%s\t<Inputs>\n" tabs;
    let print_input i =
      fprintf o "%s\t\t<Input variable=\"%s\"/>\n" tabs (varname bn i) in
    List.iter print_input (List.sort ( - ) bn.parents.(i));
    fprintf o "%s\t</Inputs>\n" tabs;
  end;
  (match bn.dists.(i) with 
     Tree t -> 
       fprintf o "%s\t<DecisionTree>\n" tabs;
       rec_output_tree o bn ("\t\t" ^ tabs) t;
       fprintf o "%s\t</DecisionTree>\n" tabs
   | Table t -> output_table o bn ("\t" ^ tabs) i t
   | FactorSet fl -> raise UnsupportedFactorType);
  fprintf o "%s</LocalModel>\n" tabs


let output o bn =
  output_string o "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
  output_string o "<AnalysisNotebook>\n";
  output_string o "\t<Variables>\n";
  Array.iter (output_var o) bn.vars;
  output_string o "\t</Variables>\n";
  output_string o "\t<Model name=\"\">\n";
  output_string o "\t\t<LocalModels>\n";
  for i = 0 to numvars bn - 1 do
    output_dist o bn "\t\t\t" i
  done;
  output_string o "\t\t</LocalModels>\n";
  output_string o "\t</Model>\n";
  output_string o "</AnalysisNotebook>\n"

let print = output stdout


(*
 * PARSING a .xmod file
 *)

(*
let _ = 
  try let _ = load stdin in ()
  with Failure s -> print_string (s ^ "\n")
  *)

type parse_var = {pidx: int;
                  pname: string;
                  pstates: (int * string) list}

type parse_data = {
                   (* Whether or not we're done with the Variables
                      section and have moved on to the Model section. *)
                   mutable in_model: bool;
                   (* Keep track of state within <Variables> section *)
                   mutable in_type_variables: bool;
                   (* Variable types *)
                   typehash: (string, parse_var) Hashtbl.t;
                   (* Stuff for keeping track of vars, as we read them *)
                   mutable max_var: int;
                   mutable curr_var: string;
                   mutable curr_states: (int * string) list; 
                   (* Result of variable parsing: Array of vars and
                      a name-to-var look-up. *) 
                   mutable pvars: parse_var list;
                   mutable pvars_a: parse_var array;
                   varhash: (string, parse_var) Hashtbl.t;
                   (* Keep track of state within <Model> section *)
                   mutable curr_dvar: int;
                   mutable curr_parents: int list;
                   mutable in_table: bool;
                   mutable curr_table: float array array;
                   mutable curr_input_state: int array;
                   mutable curr_root: cpnode option;
                   mutable varstack: int list;
                   mutable valstack: int list;
                   mutable cstack: (cpnode option * cpnode option) list;
                   mutable cdata: string;
                   (* Results of model parsing: List of dists. *)
                   mutable dists: cpd array;
                   mutable parents: int list array
                  }

exception Unknown_var of string
exception Unknown_value of string * string

let varname_to_idx d name =
  (* DEBUG 
  printf "Var name = %s" name; flush stdout; *)
  try (Hashtbl.find d.varhash name).pidx
  with Not_found -> raise (Unknown_var name)

(*
let valname_to_idx d var name = 
  (* DEBUG 
  printf "Var = %d\n" var; flush stdout; *)
  let v = d.pvars_a.(var) in
  try fst (List.find (fun (idx, s) -> s=name) v.pstates)
  with Not_found -> raise (Unknown_value (v.pname, name)) *)


(* Start element event handler, when parsing the variables *)
let start_elem_invar d name atts =
  match name with 
  | "TypeVariables" -> 
      d.in_type_variables <- true
  | "Variable" -> 
      d.curr_var <- List.assoc "name" atts;
      if List.mem_assoc "typeVariable" atts then
        let typename = List.assoc "typeVariable" atts in
        d.curr_states <- (Hashtbl.find d.typehash typename).pstates
      else
        d.curr_states <- []
  | "State" ->
      (* Add the new state to the list *)
      let validx = int_of_string (List.assoc "index" atts) in
      let valname = List.assoc "name" atts in
      d.curr_states <- (validx, valname) :: d.curr_states
  | "Model" ->
      d.in_model <- true;
      d.pvars_a <- Array.of_list (List.rev d.pvars);
      let numvars = Array.length d.pvars_a in
      d.parents <- Array.make numvars [];
      d.dists <- Array.make numvars (Table [||])
  | _ -> ()


(* Start element event handler, when parsing the model *)
let start_elem_inmodel d name atts =
  match name with 
  | "LocalModel" -> 
      d.curr_dvar <- varname_to_idx d (List.assoc "variable" atts) 
  | "Input" -> 
      let parent = varname_to_idx d (List.assoc "variable" atts) in
      d.curr_parents <- parent :: d.curr_parents 
  | "Vertex" -> 
      let splitvar = varname_to_idx d (List.assoc "split" atts) in
      d.varstack <- splitvar :: d.varstack  ;
      d.cstack <- (None,None) :: d.cstack
  | "Table" -> 
      d.in_table <- true;
      (* Create an empty table of the proper dimension *)
      let pa = Array.of_list (List.rev d.curr_parents) in
      let numstates v = List.length d.pvars_a.(v).pstates in
      let pardim = Array.map numstates pa in
      let maxdim = Array.fold_right ( * ) pardim 1 in
      d.curr_table <- Array.make maxdim [||]
  | "TableEntry" -> ()
  | "InputStates" -> ()
  | _ -> () (* Do nothing for all other tags *)


let start_elem d name atts =
  (if d.in_model = false then
     start_elem_invar d name atts
   else
     start_elem_inmodel d name atts);
  d.cdata <- ""


(* End element event handler, when parsing the variables *)
let end_elem_invar d name =
  match name with 
  | "TypeVariables" ->
      d.in_type_variables <- false
  | "Variable" -> 
    if d.in_type_variables then
      let tv = {pidx=(-1); pname=d.curr_var; pstates=d.curr_states} in
      Hashtbl.add d.typehash d.curr_var tv
    else begin
      d.max_var <- d.max_var + 1;
      let v = {pidx=d.max_var; pname=d.curr_var; pstates=d.curr_states} in
      Hashtbl.add d.varhash d.curr_var v;
      d.pvars <- v :: d.pvars
    end
  | _ -> () (* Do nothing for all other tags *)


(* End element event handler, when parsing the model *)
let end_elem_inmodel d name =
  match name with 
  | "LocalModel" -> 
      d.parents.(d.curr_dvar) <- List.rev d.curr_parents;
      d.dists.(d.curr_dvar) <- 
        if d.in_table then
          Table d.curr_table
        else
          (match d.curr_root with Some x -> Tree x | None -> assert false);
      d.curr_dvar <- (-1);
      d.curr_parents <- [];
      d.curr_root <- None;
      d.in_table <- false;
      assert (d.varstack = []);
      assert (d.valstack = []);
      assert (d.cstack = [])
  | "Vertex" -> 
      (match d.cstack with 
         (Some c1, Some c2) :: l -> 
           let (var, value) = (List.hd d.varstack, List.hd d.valstack) in
           d.varstack <- List.tl d.varstack;
           d.valstack <- List.tl d.valstack;
           let v = Vertex (var, value, c1, c2) in
           (match l with 
              (None, None) :: l' -> d.cstack <- (Some v, None) :: l'
            | (Some v1, None) :: l' -> d.cstack <- (Some v1, Some v) :: l'
            | [] -> d.cstack <- []; d.curr_root <- Some v
            | _ -> assert false) 
       | _ -> assert false)
  | "Values" ->
      (* We should see two branches, the first with the value being 
       * selected, the second with all alternatives.  We only want 
       * to record one of those values, the first one. *)
      if List.length d.valstack < List.length d.varstack then
        d.valstack <- int_of_string d.cdata ::d.valstack
        (* OLD: d.valstack <- (valname_to_idx d (List.hd d.varstack) (d.cdata)) :: d.valstack *)
  | "Probs" -> 
      let probstrs = Array.of_list (Str.split (regexp "[ \t]+") d.cdata) in
      let probs = Array.map log (Array.map float_of_string probstrs) in
      (* Case 1: Table CPD *)
      if d.in_table then begin
        (* Get dimension of each parent *)
        let pa = Array.of_list (List.rev d.curr_parents) in 
        let numstates v = List.length d.pvars_a.(v).pstates in
        let pardim = Array.map numstates pa in
        (* Compute state index and add probs to table *)
        let idx = Varstate.varstate_to_idx pardim d.curr_input_state in
        (* TODO: Make sure input_state has proper dimension... *)
        (* TODO: Proper error messages here... *)
        assert(d.curr_table.(idx) = [||]);
        d.curr_table.(idx) <- probs;
        d.curr_input_state <- [||]
      (* Case 1: Tree CPD *)
      end else begin
        let leaf = Leaf probs in
        (match d.cstack with 
          (None, None) :: l -> d.cstack <- (Some leaf, None) :: l
        | (Some l1, None) :: l -> d.cstack <- (Some l1, Some leaf) :: l
        | [] -> d.curr_root <- Some leaf
        | _ -> assert false)
      end
  | "InputStates" -> 
      let statestrs = Str.split (regexp "[ \t]+") d.cdata in
      d.curr_input_state <- Array.of_list (List.map int_of_string statestrs)
  | _ -> () (* Do nothing for all other tags *)


let end_elem d name =
  (if d.in_model = false then
     end_elem_invar d name
   else
     end_elem_inmodel d name);
  d.cdata <- ""


let char_data d s = d.cdata <- d.cdata ^ s

let pvar2var v =
  let sa = Array.of_list v.pstates in
  let range = Array.max (Array.map fst sa) + 1 in
  let valnames = Array.make range "" in
  Array.iter (fun (idx, name) -> valnames.(idx) <- name) sa;
  {vname=v.pname; idx=v.pidx; range=range; valnames=valnames}

(* open Expat
 * let parse_xmod channel =
 *   let p = parser_create None in
 *   let d = {in_model=false;
 *            in_type_variables=false;
 *            typehash=Hashtbl.create 100;
 *            max_var=(-1);
 *            curr_var="";
 *            curr_states=[];
 *            pvars=[];
 *            pvars_a=[||];
 *            varhash=Hashtbl.create 100;
 *            curr_dvar=(-1);
 *            curr_parents=[];
 *            in_table=false;
 *            curr_table=[||];
 *            curr_input_state=[||];
 *            curr_root = None;
 *            varstack = [];
 *            valstack = [];
 *            dists = [||];
 *            parents = [||];
 *            cstack = [];
 *            cdata = ""} in
 *   set_start_element_handler p (start_elem d) ;
 *   set_end_element_handler p (end_elem d) ;
 *   set_character_data_handler p (char_data d) ;
 *   (try while true do
 *     let s = input_line channel in
 *     (\* DEBUG 
 *     print_endline s; flush stdout; *\)
 *     parse p s
 *   done with End_of_file -> ()) ;
 *   final p ;
 *   let vars = Array.map pvar2var d.pvars_a in
 *   let children = make_children d.parents in
 *   let topo = make_topo_vars vars d.parents children in
 *   let (varhash, valhash) = build_namehashes vars in
 *   {name=""; acyclic=true; vars=vars; BnType.parents=d.parents;
 *    children=children; BnType.dists=d.dists; name_to_varidx=varhash;
 *    name_to_validx=valhash; topo_vars=topo} *)

let load channel = failwith "xxx"
