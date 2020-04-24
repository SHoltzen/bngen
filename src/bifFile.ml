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

exception UnsupportedFactorType

(*
 * Read a file in BIF format
 *)

let of_parse (netname, vars, probs) = 

  (* Handle all the variable declarations *)
  let numvars = List.length vars in
  let name_to_varidx = Hashtbl.create (2 * numvars) in
  let hash_create i = Hashtbl.create 10 in
  let name_to_validx = Array.init numvars hash_create in 
  let idx = ref (-1) in

  let parse_var (n, vnl) =
    incr idx ;
    Hashtbl.add name_to_varidx n !idx ;
    let h = name_to_validx.(!idx) in 
    Array.iteri (fun i s -> Hashtbl.add h s i) (Array.of_list vnl) ;
    {vname = n; idx = !idx; range = List.length vnl; 
     valnames = Array.of_list vnl} in
  let avars = Array.of_list (List.map parse_var vars) in
  let lookup_var n =
    try Hashtbl.find name_to_varidx n
    with Not_found -> failwith
      ("Error loading network: unknown variable " ^ n ^
       "\nin conditional probaiblity distribution.") in

  (* Create arrays for distributions and parent lists.
   * We'll fill them in later.
   *)
  let dists = Array.make numvars (Table [||]) in
  let parents = Array.make numvars [] in

  (* Construct CPTs *)
  let build_cpt ((varname, parnames), probs) =
    let var = avars.(lookup_var varname) in
    let aparnames = Array.of_list parnames in
    let parindex = Array.map lookup_var aparnames  in
    let parvalh = Array.map (Array.get name_to_validx) parindex in
    let pardim = Array.map Hashtbl.length parvalh in

    let get_idx state =
      let astate = Array.of_list state in
      let idx = ref 0 in
      for i = 0 to Array.length aparnames - 1 do
        idx := !idx * pardim.(i);
        let offset = 
          try Hashtbl.find parvalh.(i) astate.(i)
          with Not_found -> failwith
("Error loading network: unknown state " ^ astate.(i) 
 ^ "of variable " ^ aparnames.(i) 
 ^ " in conditional probability distribution.") in
        idx := !idx + offset
      done ; !idx in
    let maxdim = Array.fold_right ( * ) pardim 1 in
    let mkdist _ = Array.make var.range 0.0 in
    let cpt = Array.init maxdim mkdist in
    let assign (state,dist) = 
      cpt.(get_idx state) <- Array.of_list (List.map log dist) in 
    List.iter assign probs;
    parents.(var.idx) <- List.map lookup_var parnames; 
    dists.(var.idx) <- Table cpt in

  (* This builds all of the CPTs, places them in the dists array,
   * and fills in the parents array. *)
  List.iter build_cpt probs;
  let children = make_children parents in
  let topo_vars = make_topo_vars avars parents children in
  
  (* Put it all together in the final BN *)
  {name=netname; acyclic=true; vars=avars; dists=dists; 
   name_to_varidx=name_to_varidx; name_to_validx=name_to_validx;
   topo_vars=topo_vars; parents=parents; children=children}


let parse channel =
  let lexbuf = Lexing.from_channel channel in
  BifParser.bn BifLexer.lexer lexbuf 


let load channel = of_parse (parse channel)


(*
 * Print a file in BIF format
 *)

let output_var out v =
  fprintf out "variable %s {\n" v.vname;
  fprintf out "  type discrete [ %d ] { " v.range;
  output_sep out ", " v.valnames;
  output_string out " };\n}\n" 

let output_dist out dist =
  let dstrs = Array.map (fun p -> string_of_float (exp p)) dist in
  output_sep out ", " dstrs;
  output_string out ";\n"

let output_cpd out bn v =
  (* Header, e.g. "probability ( WetGrass | Sprinkler,Rain ) {" *)
  fprintf out "probability ( %s" bn.vars.(v).vname;
  let numparents = List.length bn.parents.(v) in
  if numparents > 0 then begin
    output_string out " | ";
    let pa = Array.of_list bn.parents.(v) in
    output_sep out "," (Array.map (fun p -> bn.vars.(p).vname) pa)
  end;
  output_string out " ) {\n";

  let table = match bn.dists.(v) with 
    Tree _ -> raise UnsupportedFactorType
  | Table t -> t 
  | FactorSet _ -> raise UnsupportedFactorType in

  (* If there are no parents, just print the marginals *)
  if numparents = 0 then begin
    output_string out "  table ";
    output_dist out table.(0)

  (* Else, print out each line (one for each parent config) *)
  end else begin
    let varstate = Array.make (Array.length bn.vars) 0 in
    let lparents = List.rev bn.parents.(v) in
    let pa = Array.of_list bn.parents.(v) in
    try for i = 0 to Array.length table - 1 do
      let parentvals = Array.map 
        (fun p -> bn.vars.(p).valnames.(varstate.(p))) pa in 
      output_string out "  (";
      output_sep out "," parentvals;
      output_string out ") ";
      output_dist out table.(i);
      Varstate.incstate (schema bn) varstate lparents
    done with Varstate.NoMoreStates -> ()
  end;
  fprintf out "}\n\n"


let output out bn =
  fprintf out "network %s {\n}\n\n" 
    (if bn.name = "" then "noname" else bn.name);
  Array.iter (output_var out) bn.vars;
  for i = 0 to Array.length bn.vars - 1 do
    output_cpd out bn i
  done
