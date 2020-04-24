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

(** The Data module reads/writes data and schema files, which have a
    common format throughout {{: http://libra.cs.uoregon.edu} Libra} *)

exception Eof
exception Invalid_length of int array
exception Invalid_value of (int array * int)
exception Parse_error of string

type schema_t = int array
type example_t = int array
type wexample_t = float * int array
type marginal_t = float array

(** {5 Read/Write schema files } *)

(** [load_schema filename] loads a schema from file [filename]. *) 
val load_schema : string -> schema_t

(** [dump_schema s filename] writes schema [s] to file [filename]. *)
val dump_schema : schema_t -> string -> unit

(** [dump_schema o s] writes schema [s] to output channel [o]. *)
val output_schema : out_channel -> schema_t -> unit



(** {5 Read/Write data files } *)

(** Reads one data example from the input. *)
val input_example : in_channel -> example_t

(** Reads a list of data examples from the input. *)
val input_example_list : in_channel -> example_t list

(** Reads one weighted data example from the input, e.g., 10 | 0 1 1 1
    is equivalent to 10 copies of the example 0 1 1 1. *)
val input_wexample : in_channel -> wexample_t

(** Reads a list of weighted data examples from the input. *) 
val input_wexample_list : in_channel -> wexample_t list

(** Reads real-valued evidence vector from the input. Used only for
    conditional distributions - for general-purpose ACs, MNs, and BNs, use
    input_example to load discrete evidence values. *)
val input_evidence: in_channel -> float array

(** Reads a list of real-valued evidence vectors from the input. *)
val input_evidence_list : in_channel -> float array list
  
(** Reads a list of data examples and validates them against the given
    schema. *)
val input_example_list_schema : schema_t -> in_channel -> example_t list

(** Reads a list of weighted data examples and validates them against
    the given schema. *)
val input_wexample_list_schema : schema_t -> in_channel -> wexample_t list

(** Writes an example to the output channel. *)
val output_example: out_channel -> example_t -> unit


(** [load_data file] reads examples from file [filename] and returns 
    them as a list. *)
val load_data : string -> example_t list

(** [load_data_ar file] reads examples from file [filename] and returns 
    them as an array. *)
val load_data_ar : string -> example_t array

(** [load_evidence filename] reads real-valued evidence vectors from 
    file [filename] and returns them as a list. *)
val load_evidence: string -> float array list

(** [load_evidence_ar filename] reads real-valued evidence vectors from
    file [filename] and returns them as an array. *)
val load_evidence_ar: string -> float array array

(** [dump_data data filename] writes [data] examples to file [filename]. *)
val dump_data : example_t array -> string -> unit



(** {5 Read/write marginals} *)

(** Reads vector of marginal distributions from the input. *) 
val input_marginals : in_channel -> marginal_t array

(** Writes vector of marginal distributions to the output. *)
val output_marginals : out_channel -> marginal_t array -> unit

(** Prints marginals to stdout. *)
val print_marginals : marginal_t array -> unit



(** {5 Inferring schema} *) 

(** Infers the data schema from the given examples. *)
val schema : example_t list -> schema_t

(** Infers the data schema from the given input stream. *)
val stream_schema : in_channel -> schema_t


(** {5 Validating examples} *)

(** [check_point schema example] is used for simple input validation.
@raise Invalid_length for inconsistent length compared to the number
of variables in the schema, or @raise Invalid_value for a variable
value that is inconsistent with the schema. *)
val check_point : schema_t -> example_t -> unit

(** [check_evidence schema ev] runs {!check_point} for evidence [ev]
and returns [ev]. If [ev] is an empty array, returns a new evidence
array of the appropriate dimension. *)
val check_evidence : schema_t -> example_t -> int array


(** {5 Debug} *)

(** Converts the given example to a string. *)
val to_string_example : example_t -> string

(** Converts the given schema to a string. *)
val to_string_schema : schema_t -> string
