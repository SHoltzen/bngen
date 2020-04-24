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
 * Functions for indexing and iterating over the states of sets of variables
 *)

(* State index --> state of variables *)
let get_offsets ranges = 
  let offsets = Array.make (Array.length ranges) 0 in
  offsets.(Array.length offsets - 1) <- 1 ;
  for i = Array.length offsets - 2 downto 0 do
    offsets.(i) <- ranges.(i+1) * offsets.(i+1)
  done ; offsets

let idx_to_varstate ranges state_idx =
  let offsets = get_offsets ranges in
  let state = Array.make (Array.length ranges) (-1) in
  let remainder = ref state_idx in
  for i = 0 to Array.length ranges - 1 do 
    state.(i) <- !remainder / offsets.(i) ;
    remainder := !remainder mod offsets.(i)
  done ;
  state

(* State of variables --> state index *)
let varstate_to_idx ranges values =
  let idx = ref 0 in
  for i = 0 to Array.length ranges - 1 do 
    idx := !idx * ranges.(i) + values.(i)
  done ;
  !idx

(* State of *all* variables --> state index *)
let state_to_idx vars ranges values =
  let idx = ref 0 in
  for i = 0 to Array.length ranges - 1 do 
    idx := !idx * ranges.(i) + values.(vars.(i))
  done ;
  !idx


exception NoMoreStates

(* Helper function for iterating over all states of the parent variables *)
(* TODO -- this interface is a bit awkward. *)
let rec incstate schema varstate parents =
  match parents with
    x :: l ->
      if varstate.(x) < schema.(x) - 1 then
        varstate.(x) <- varstate.(x) + 1
      else
       (varstate.(x) <- 0;
        incstate schema varstate l)
  | [] -> raise NoMoreStates


let map_state schema vars f =
  let varstate = Array.make (Array.length schema) (-1) in
  List.iter (fun i -> varstate.(i) <- 0) vars ;
  let results = ref [] in
  (try 
    while true do
      results := (f varstate) :: !results;
      incstate schema varstate vars
    done
  with NoMoreStates -> ()) ;
  !results

let iter_state schema vars f = ignore (map_state schema vars f)
