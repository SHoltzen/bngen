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
 * Named timers.  Each timer is associated with a string name.
 * Operations are starting, stopping (or pausing), and checking
 * the time elapsed on the named timer.  A single timer may be
 * started and stopped multiple times, and the elapsed time will
 * always be the total time spent running.
 *)


(* Hashes of start times and elapsed times *)
let start_time_hash = Hashtbl.create 100 
let elapsed_time_hash = Hashtbl.create 100 
let last_elapsed_time_hash = Hashtbl.create 100

(* Helper functions, for internal use only. *)
let _get_elapsed s =
  try Hashtbl.find elapsed_time_hash s
  with Not_found -> 0.0

let _get_start s =
  try Hashtbl.find start_time_hash s
  with Not_found -> Sys.time() 

(* Start a timer with the given name.  Does nothing if timer has
   already been started. *)
let start (s:string) =
  if not (Hashtbl.mem start_time_hash s) then
    Hashtbl.add start_time_hash s (Sys.time())

(* Returns time passed since the timer was most recently started or
 * delta was last called for this timer.  Useful for timing sequential
 * pieces. *)
let delta (s:string) =
  let now = Sys.time() in
  let old_elapsed = _get_elapsed s in 
  let new_elapsed = now -. _get_start s in 
  Hashtbl.replace elapsed_time_hash s (new_elapsed +. old_elapsed);
  Hashtbl.replace start_time_hash s now;
  new_elapsed

(* Amount of time elapsed on the named timer.  Counts time that the timer was running. *)
let elapsed (s:string) =
  if Hashtbl.mem start_time_hash s then 
      _get_elapsed s  +. (Sys.time() -. _get_start s) 
  else  _get_elapsed s  


(* Returns the amount of time elapsed between last start and stop. *) 
let last_elapsed (s:string) = 
  try Hashtbl.find last_elapsed_time_hash s
  with Not_found -> 0.0


(* Stop named timer.  This pauses it until restarted with start. *)
let stop (s:string) =
  Hashtbl.replace elapsed_time_hash s (_get_elapsed s  +. (Sys.time() -. _get_start s) );
  Hashtbl.replace last_elapsed_time_hash s (Sys.time() -. _get_start s );
  Hashtbl.remove start_time_hash s



(* Stop named timer and set its elapsed time to zero. *)
let clear (s:string) =
  Hashtbl.remove start_time_hash s;
  Hashtbl.remove elapsed_time_hash s;
  Hashtbl.remove last_elapsed_time_hash s
