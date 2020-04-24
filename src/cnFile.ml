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
open BnType

(*
 * Read a file in CN format
 *)

let of_parse (schema, (acyclic, varfactors)) = 

  let numvars = Array.length schema in
  let bn = create_empty_network schema in

  (* Create arrays for distributions and parent lists.
   * We'll fill them in later.
   *)
  let dists = Array.make numvars (Table [||]) in
  let parents = Array.make numvars [] in

  (* Construct CPTs *)
  let build_dist (var, pfactors) =
    let factors = List.map (Mn.Factor.pfactor_to_factor schema) pfactors in
    dists.(var) <- FactorSet factors;
    parents.(var) <- factorset_parents numvars var factors in

  (* This builds all of the CPTs, places them in the dists array,
   * and fills in the parents array. *)
  List.iter build_dist varfactors;
  let children = make_children parents in
  let topo_vars = make_topo_vars bn.vars parents children in
  
  (* Put it all together in the final BN *)
  {bn with acyclic=acyclic; dists=dists; topo_vars=topo_vars; 
   parents=parents; children=children}


let parse channel =
  let schema = Data.input_example channel in
  let lexbuf = Lexing.from_channel channel in
  (schema, CnParser.cn CnLexer.lexer lexbuf)


let load channel = of_parse (parse channel)


(*
 * Print a file in CN format
 *)

let output out cn =
  Data.output_schema out (BnType.schema cn);
  if cn.acyclic then
    output_string out "BN {\n"
  else
    output_string out "DN {\n";
  for v = 0 to numvars cn - 1 do
    fprintf out "v%d {\n" v;
    let fl = cpd_to_factors cn v cn.dists.(v) in
    List.iter (Mn.Factor.output_factor out) fl;
    fprintf out "}\n";
  done;
  output_string out "}\n"
