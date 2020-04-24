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
open Scanf
open Printf

let is_whitespace = function
 | ' ' | '\n' | '\r' | '\t' -> true
 | _ -> false

let input_token ic buf len =
  (* Skip leading whitespace *)
failwith "ono"
  (* let c = ref ' ' in
   * while is_whitespace !c do
   *   c := input_char ic
   * done;
   * (\* Read token *\)
   * let i = ref 0 in
   * while !i < len && not (is_whitespace !c) do
   *   String.set buf !i !c;
   *   c := input_char ic;
   *   incr i
   * done;
   * let s = String.create !i in
   * String.blit buf 0 s 0 !i;
   * s *)

let input_buf = String.create 1024

let input_int ic = 
  int_of_string (input_token ic input_buf 1024)

let input_float ic =
  float_of_string (input_token ic input_buf 1024)

let input_word ic =
  input_token ic input_buf 1024

let input ic =
  (* Read in "Markov" or "Bayes" *)
  let w = String.uppercase (input_word ic) in
  if String.compare "MARKOV" w <> 0 
    && String.compare "BAYES" w <> 0 then begin
    (* TODO: Friendlier notification of errors... *)
    print_string "ERROR: UAI file does not start with MARKOV or BAYES.\n";
    flush stdout;
    raise Parsing.Parse_error
  end;

  (* Read schema *)
  let num_vars = input_int ic in 
  let schema = Array.init num_vars (fun i -> input_int ic) in

  (* Read factor scopes *)
  let num_factors = input_int ic in
  let read_scope i =
    let num_args = input_int ic in
    Array.init num_args (fun j -> input_int ic) in
  let scopes = Array.init num_factors (fun i -> read_scope i) in

  (* Read factor values *)
  let read_factor i =
    let num_values = input_int ic in
    Array.init num_values (fun j -> log (input_float ic)) in
  let factor_values = Array.init num_factors (fun i -> read_factor i) in

  (* Build factors (tables only) *)
  let range i = schema.(i) in 
  let build_table scope values =
    Factor.Table (scope, (Array.map range scope), values) in
  let factors = Array.map2 build_table scopes factor_values in
  (schema, factors)

let output o schema factors =
  output_string o "MARKOV\n";
  fprintf o "%d\n" (Array.length schema);
  fprintf o "%s\n" (string_of_iarray schema);
  fprintf o "%d\n" (Array.length factors);
  let print_scope = function
  | Factor.Table (vars, ranges, values) ->
    fprintf o "%d %s\n" (Array.length vars) (string_of_iarray vars)
  | Factor.Const w ->
    fprintf o "0\n" 
  | _ ->
    print_string "ERROR: only table factors are supported.\n";
    flush stdout;
    assert false (* TODO: Better error reporting... *) in
  Array.iter print_scope factors;

  let print_values = function
  | Factor.Table (vars, ranges, values) ->
    let raw_values = Array.map exp values in
    fprintf o "\n%d %s\n" (Array.length values) (string_of_farray raw_values)
  | Factor.Const w ->
    fprintf o "\n1 %f\n" (exp w)
  | _ -> assert false in
  Array.iter print_values factors
