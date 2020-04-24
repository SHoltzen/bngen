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

(** The MN library reads, writes, and represents Markov networks with
    factors represented as tables, trees, sets of features, or individual
    features. *)

(** {5 Auxilary modules} *)

(** Set of functions for indexing and iterating over the states of
    sets of variables. *)
module Varstate :
  sig
    exception NoMoreStates
    val get_offsets : int array -> int array
    val idx_to_varstate : int array -> int -> int array
    val varstate_to_idx : int array -> int array -> int
    val state_to_idx : int array -> int array -> int array -> int

    (** Helper function for iterating over all states of the
        parent variables. {b Usage}: [incstate schema varstate parents] *)
    val incstate : int array -> int array -> int list -> unit
    val map_state : int array -> int list -> (int array -> 'a) -> 'a list
    val iter_state : int array -> int list -> (int array -> 'a) -> unit
  end


(** The Factor module is used for representing a factor in a factor graph. *)  
module Factor :
  sig

    (** {6 Data structures} *)

    (** Variables are represented as integer indices. *)
    type variable = int

    (** A variable state is an integer index over a variable's range
        of values. *)
    type varvalue = int

    (** [condition] represents an equality or inequality constraint on
        a variable's value.  For example, (true, 10, 1) says that variable
        10 has a value of 1, and (false, 5, 0) says that variable 5 does
        not have a value of 0. *)
    type condition = bool * variable * varvalue
    
    (** A feature is a set of conditions and a weight parameter. *)
    type feature =
      Factor.feature = {
      cond : condition array;
      weight_id : int;
      mutable weight : float;
    }
    
    (** Recursive structure for tree representation of a factor. *)
    type tree =
      Factor.tree =
        Leaf of float
      | Vertex of variable * varvalue * tree * tree
        
    (** Different factor representations for factor graphs.  
        See {{: http://libra.cs.uoregon.edu/manual.pdf } Section 1.1} of
        the user manual for more information about representations. *)
    type factor =
      Factor.factor =
        Feature of feature 
      | FeatureSet of feature list
      | Table of variable array * int array * float array 
        (** Table representation of a factor consists of an array of
            variables, an array of variable ranges, and array of
            parameters. *)
      | Tree of tree
      | Const of float
    

    (** {6 Factor operations} *)
     
    (** Comparision function used for sorting conditions. *)
    val simple_cond_cmp : condition -> condition -> int

    (** Removes redundant conditions from a condition list.  Assumes
        that conditions are sorted by simple_cond_cmp and the condition is
        satisfiable (not contradictory). *)
    val remove_redundant_conds : condition list -> condition list

    (** [condl_to_valsetl schema condl] converts list of conditions
        [condl] to a list of variable value sets, so that each variable
        appears only once in the list. *)   
    val condl_to_valsetl :
      int array -> condition list -> (int * bool array) list
    
    (** [fmatch x cond_ar] checks whether example [x] satisfies set of
        conditions [cond_ar]. *)
    val fmatch : varvalue array -> condition array -> bool 

    (** [fweight x f] returns the weight of feature [f] if example [x]
        satisfies the feature. Otherwise returns 0. *)
    val fweight : varvalue array -> feature -> float

    (** [log_value x f] returns the value of the factor for the
        configuration given by example [x]. *)  
    val log_value : varvalue array -> factor -> float

    (** [expected_log_value f logmarg negmarg] returns the expected
        value of factor [f] given a fully factorized distribution.
        [logmarg.(var).(value)] is the log-probability that variable 
        [var] has value [value], and [negmarg] is log-probability that
        variable [var] does not have value [value]. *)
    val expected_log_value :
      factor -> float array array -> float array array -> float
    
    (** @return the number of parameters in a factor. *)
    val numparams : factor -> int

    (** [simplify_feature ev f] removes all conditions of feature [f]
        that are satisfied by evidence [ev] and returns a new, simpler
        feature.  If [f] is inconsistent with [ev], returns a feature with
        no conditions instead. *)
    val simplify_feature : varvalue array -> feature -> feature
  
    (** [simplify ev f] simplifies factor [f] by conditioning it on
        evidence [ev]. *)
    val simplify : varvalue array -> factor -> factor
  
    (** Generates an equivalent set of features for a given tree factor. *) 
    val tree_to_features : condition list -> tree -> feature list
    
    (** Converts table CPD to features *)
    val table_to_features :
      variable array * int array * float array -> feature list
    
    (**  Converts factor to a set of conjunctive features. *)
    val to_features : factor -> feature list
    
    (** [set_weights w wi f] updates the weight in factor [f] using a
        global weight vector [w]. The weights of features in [f] starts
        from index [wi]. [set_weights] returns a tuple [(nf, nw)], in
        which [nf] is the new modified feature and [nw] is the number of
        features in [f]. *) 
    val set_weights : float array -> int -> factor -> factor * int

    (** @return a list of variables used by a factor. *)
    val vars : factor -> variable list

    (** See {! vars}. *)
    val feature_set_vars : feature list -> variable list

    (** See {! vars}. *)
    val tree_vars : tree -> variable list

    (** [to_table schema f] converts factor [f] to another factor with
        a table representation. *)
    val to_table : int array -> factor -> factor
    
    (** [copy f] creates a duplicate of factor [f]. *) 
    val copy : factor -> factor

    (** [rescale alpha f] rescales factor [f] by a factor of [alpha].
        Rescaling is done in log space, so this is equivalent to raising a
        potential function to the power of [alpha]. *)
    val rescale : float -> factor -> factor


    (** {6 Writing factors} *)

    (** [output_factor out f] writes factor [f] to channel [out]. *) 
    val output_factor : out_channel -> factor -> unit

    (** See {!output_factor}. *)
    val output_feature : out_channel -> feature -> unit
   
    (** See {!output_factor}. *)
    val output_featurelist : out_channel -> feature list -> unit
    
    (** See {!output_factor}. *)
    val output_tree : out_channel -> string -> tree -> unit
   
   
    (** {6 Parsing factors } *)

    (** Auxilary module for parsing factors. This interface is public
        so that other modules can reuse some of the parsing machinery. *)
    module MP :
      sig
        type pcond = condition
        type pfeature = int * float * pcond list
        type pfeaturelist = pfeature list
        type ptree =
          MnParseTypes.ptree =
            PLeaf of float
          | PVertex of int * int * ptree * ptree
        type pfactor =
          MnParseTypes.pfactor =
            PFeatureSet of pfeaturelist
          | PFeatureTable of pfeaturelist
          | PFeatureTree of ptree
          | PFeature of pfeature
        type pmn =
          MnParseTypes.pmn = {
          factors : pfactor list;
          weights : (int * float) list;
        }
      end

    (** Build a factor from parser output. *)
    val pfactor_to_factor : int array -> MP.pfactor -> factor
  end


(** {5 Markov networks} *) 

(** [network] data structure holds the Markov network factors. *)   
type network = {
  schema : int array; 
  (** Variable schema. Specifies the range of each variable, in order. *)
  factors : Factor.factor array; 
  (** Array of factors in the Markov network. Order is arbitrary. *)
  var_to_factors : Factor.factor list array; 

  (** For each variable, [var_to_factors] maintains a list of factors
      that the variable participates in. The order of variable is
      consistent with schema. *) 
}

(** [pll mn x] computes the pseudo-likelihood of sample [x] given
    network [mn]. *)
val pll : network -> Factor.varvalue array -> float

(** @return number of variables in the network. *)
val numvars : network -> int

(** @return number of parameters in the network. *)
val numweights : network -> int

(** [get_range mn v] returns the cardinality of variable [v] in
    network [mn]. *)  
val get_range : network -> int -> int

(** @return the schema of the given network. *)
val schema : network -> int array

(** @return an array of all factors of the given network. *)
val factors : network -> Factor.factor array

(** [create schema factors] returns a {!network} for the given
    [schema] and [factors]. *) 
val create : int array -> Factor.factor array -> network

(** [raw_logprob mn state] returns the unnormalized log probability of
    the variable configuration [state] according to network [mn]. *)
val raw_logprob : network -> Factor.varvalue array -> float

(** [mb_logdist mn x i] returns the log probability distribution over
    variable [i] in network [mn], given the values of its Markov blanket
    variables specified in instance [x]. *)
val mb_logdist : network -> Factor.varvalue array -> int -> float array

(** [simplify mn ev] builds a new MN with simpler factors, conditioned
    on evidence [ev]. *)
val simplify : network -> Factor.varvalue array -> network

(** [to_features mn] converts the factors in network [mn] to a list of
    features. *) 
val to_features : network -> Factor.feature list

(** [set_weights mn w] updates weights using weight vector [w]. 
    Modifies [mn] in place. *)
val set_weights : network -> float array -> unit


(** {6 Read/write Markov networks} *)

(** Returns true if filename ends with .mn. *)
val filename_is_mn : string -> bool

(** Returns true if filename ends with .uai. *)
val filename_is_uai : string -> bool

(** Load features from an existing lexing buffer. *)
val input_features_lex : Lexing.lexbuf -> Factor.feature list

(** Load features from an input channel. *)
val input_features : in_channel -> Factor.feature list

(** Loads a Markov network in [.mn] format from a channel. *)
val load : in_channel -> network

(** Writes a Markov network in [.mn] format to a channel. *)
val output : out_channel -> network -> unit

(** Loads a Markov network written in the UAI file format. *) 
val load_uai : in_channel -> network

(** Writes a Markov network in the UAI file format. *) 
val output_uai : out_channel -> network -> unit 

(** [load_auto filename] loads an MN, inferring its filetype by its
    filename. *)
val load_auto : string -> network

(** [write_auto filename mn] writes Markov network [mn] to the output
    file [filename], inferring its filetype by its filename. *)
val write_auto : string -> network -> unit
