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
open Str
(* FORMAT:

   Data examples are one per line, consisting of a comma-separated
   list of variable values.  Asterisks [*] indicate unknown values.
   e.g.
   
0,0,1,0,3
1,*,*,1,2

etc. *)

(*
 * Input 
 *)
exception Eof


type schema_t = int array
type example_t = int array
type wexample_t = float * int array
type marginal_t = float array

let comma = Str.regexp "[ \t]*,[ \t]*"

let input_example channel =
  try
    let s = input_line channel in
    let tokens = Str.split comma s in
    let vals = List.map (function "*" -> (-1) | t -> int_of_string t) tokens in
    Array.of_list vals
  with End_of_file -> raise Eof
     | Failure _ ->
        printf "ERROR: Token was neither an integer nor * in input_example.\n";
        [||]


let input_evidence channel =
  try
    let s = input_line channel in
    let tokens = Str.split comma s in
    let vals = List.map (function "*" -> (-1.0) | t -> float_of_string t) tokens in
    Array.of_list vals
  with End_of_file -> raise Eof
     | Failure _ ->
        printf "ERROR: Token was neither an integer nor * in input_example.\n";
        [||]


let bar = Str.regexp "[ \t]*|[ \t]*"

exception Parse_error of string

let input_wexample channel =
  try 
    let s = input_line channel in
    (* Separate out weight from instance *)
    let (wt, s) = match Str.split bar s with
    | w :: s :: [] -> (float_of_string w, s)
    | s :: [] -> (1.0, s)
    | _ -> raise (Parse_error "incorrect number of vertical bars (|)") in

    let tokens = Str.split comma s in
    let vals = List.map (function "*" -> (-1) | t -> int_of_string t) tokens in
    (wt, Array.of_list vals)
  with End_of_file -> raise Eof
     | Failure _ ->
       raise (Parse_error "Token was neither an integer nor * in input_example.")

let input_example_list channel =
  let l = ref [] in
  try
    while true do
      let x = input_example channel in
      l := x :: !l
    done; List.rev !l
  with Eof -> List.rev !l


let input_evidence_list channel =
  let l = ref [] in
  try
    while true do
      let x = input_evidence channel in
      l := x :: !l
    done; List.rev !l
  with Eof -> List.rev !l

let input_wexample_list channel =
  let l = ref [] in
  try
    while true do
      let x = input_wexample channel in
      l := x :: !l
    done; List.rev !l
  with Eof -> List.rev !l

let input_marginals channel =
  let re = Str.regexp "[ \t]" in
  let lines = ref [] in
  let linenum = ref 0 in
  try 
    while true do 
      incr linenum;
      let currline = input_line channel in
      (* TODO: Support multiple marginals in one file, eventually.
      if currline = "END" then raise End_of_file; *)
      let probstrs = Str.split re currline in
      let probs = Array.of_list (List.map float_of_string probstrs) in
      lines := probs :: !lines
    done; [||]
   with End_of_file -> Array.of_list (List.rev !lines)
      | Failure _ -> 
          printf "ERROR: Non-float marginal value read on line %d\n" !linenum; 
          [||]

let load_schema file =
  let channel = open_in file in 
  input_example channel

let load_data file =
  let channel = open_in file in
  let l = input_example_list channel in
  close_in_noerr channel;
  l



let load_data_ar file = 
  Array.of_list (load_data file)

let load_evidence file =
  let channel = open_in file in
  let l = input_evidence_list channel in
  close_in_noerr channel;
  l

let load_evidence_ar file = 
  Array.of_list (load_evidence file)


(* Function for simple input validation *)
exception Invalid_length of (int array)      (* length, expected length *)
exception Invalid_value of (int array * int) (* var, value, schema range *)
let check_point schema example =
  if Array.length schema != Array.length example then
    raise (Invalid_length example);
  for i = 0 to Array.length schema - 1 do
    if example.(i) >= schema.(i) then 
      raise (Invalid_value (example, i))
  done

let check_evidence schema ev =
  if ev = [||] then
    Array.make (Array.length schema) (-1)
  else
    (check_point schema ev; ev)

(* Standardized error reporting *)
let report_invalid_value linenum x var =
  printf "Data error on line %d: value %d for variable %d is out of range.\n"
      linenum x.(var) var;
  exit (-1)

let report_invalid_length schema linenum x =
  printf "Data error on line %d: number of variables is %d; expected %d\n"
      linenum (Array.length x) (Array.length schema);
  exit (-1)

(* Read in data, with basic error checking *)
let input_example_list_schema schema channel =
  let l = ref [] in
  try
    while true do
      let x = input_example channel in
      check_point schema x;
      l := x :: !l
    done; List.rev !l
  with Eof -> List.rev !l
     | Invalid_value (x, var) ->
       report_invalid_value (List.length !l) x var
     | Invalid_length x -> 
       report_invalid_length schema (List.length !l) x 

let input_wexample_list_schema schema channel =
  let l = ref [] in
  try
    while true do
      let x = input_wexample channel in
      check_point schema (snd x);
      l := x :: !l
    done; List.rev !l
  with Eof -> List.rev !l
     | Invalid_value (x, var) -> 
       report_invalid_value (List.length !l) x var
     | Invalid_length x -> 
       report_invalid_length schema (List.length !l) x 


(*
 * Output
 *)

let output o x =
  for i = 0 to Array.length x - 1 do
    if i > 0 then fprintf o ",";
    if x.(i) < 0 then 
      output_string o "*" 
    else 
      output_string o (string_of_int x.(i))
  done;
  output_string o "\n"

let print = output stdout

let output_schema o x = output o x
let output_example o x = output o x

let to_string x =
  let string_of_value xi =
    if xi < 0 then "*"
    else string_of_int xi in
  let elements = List.map string_of_value (Array.to_list x) in
  (String.concat "," elements) ^ "\n"

let to_string_example x = to_string x

let to_string_schema x = to_string x


let dump_data data file =
  let out = open_out file in
  Array.iter (output out) data

let dump_schema schema file =
  dump_data [|schema|] file

let output_marginals o m =
  for i = 0 to Array.length m - 1 do 
    for j = 0 to Array.length m.(i) - 1 do
      if j > 0 then fprintf o " ";
      output_string o (string_of_float m.(i).(j))
    done;
    output_string o "\n"
  done
 (* TODO: Support multiple marginals in one file, eventually.
  ; output_string o "END\n" *)

let print_marginals = output_marginals stdout


(*
 * Analysis
 *)

(* Copied from ext.ml. *)
let array_map2 f a1 a2 = Array.mapi (fun i x -> f x a2.(i)) a1

let data_max = function
| x :: l -> List.fold_left (array_map2 max) x l
| [] -> [||]

let schema example_list = 
  let maxvalues = data_max example_list in
  let s = Array.map (( + ) 1) maxvalues in
  Array.map (( max ) 2) s

let stream_schema lexbuf =
  let a = input_example lexbuf in
  (try while true do
    let a' = input_example lexbuf in
    for i = 0 to Array.length a - 1 do
      a.(i) <- max a.(i) a'.(i)
    done
  done with Eof -> ());
  let s = Array.map (( + ) 1) a in
  Array.map (( max ) 2) s
