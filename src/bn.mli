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

(** The BN library reads, writes, and represents Bayesian networks
    (BNs) and dependency networks (DNs) with conditional probability
    distributions (CPDs) that are tree-structured, tables, or arbitrary
    sets of factors. *)


(** {6 Data structures} *)

(** A domain schema specifies the cardinality of each (discrete) variable. *)
type schema_t = int array

(** A BN variable structure includes additional information about variable 
    and value names, for compatibility with some standard BN file
    formats. (Variable and value names are not supported by most of Libra.) *)
type variable =
  BnType.variable = {
  vname : string;  (** Variable name. *)
  idx : int;  (** Variable index in the network (the variable is located at [network.vars.(idx))].  *)
  range : int;  (** The cardinality of the variable. *)
  valnames : string array;  (** String name for each possible value of the variable. *)
}

(** Represents a node in a tree-structured CPD. *) 
type cpnode =
  BnType.cpnode =
    Leaf of float array (** Array contains log conditional probabilities for this leaf *)
  | Vertex of int * int * cpnode * cpnode (** Split var, value, true branch, false branch *)

(** A CPD can be represented as a table, tree, or set of factors. *)
type cpd =
  BnType.cpd =
    Table of float array array (** Table representation of CPDs. *)
  | Tree of cpnode (** Tree representation of CPDs. *)
  | FactorSet of Mn.Factor.factor list (** Factor representation of CPDs. *)


(** Represents a Bayesian network or dependency network. The fields
    [children], [name_to_varidx], [name_to_validx], and [topo_vars] can
    all be derived from the previous fields using provided functions.
    This makes them redundant, but they exist for convenience. *) 
type network =
  BnType.network = {
  name : string; (** Name of the network or domain. Arbitrary and mostly unused. *)
  mutable acyclic : bool; (** True if network is a BN; false for DNs. *)
  vars : variable array; (** Set of variables in the network. *)
  parents : int list array; (** Indices of parents for each var. *)
  dists : cpd array; (** Set of CPDs in the network. *)
  children : int list array; (** Indices of children for each var. *)
  name_to_varidx : (string, int) Hashtbl.t; (** Map from variable names to indices. *)
  name_to_validx : (string, int) Hashtbl.t array; (** Map from the names of each variable's values to their indices. *)
  topo_vars : variable array; (** Set of variables in the network, sorted in topologocal order. *)
}

(** {6 Utility methods} *)

(** [get_range bn i] @return the cardinality of variable [i] in
    network [bn]. *)
val get_range : network -> int -> int

(** @return the schema of the given network. *)
val schema : network -> schema_t

(** @return the number of parameters in the given tree CPD. *)
val tree_params : cpnode -> int

(** @return the number of parameters in the given CPD. *)
val dist_params : cpd -> int

(** [tree_parents numvars root] @return the list of nodes that have at
    least one child, given a tree with the root node [root] *)
val tree_parents : int -> cpnode -> int list

(** @return the number of variables in the network. *)
val numvars : network -> int

(** [varname bn idx] @return the variable name of the variable at
    index [idx] of array [bn.vars]. *)
val varname : network -> int -> string


(** @return the index of the variable. *)
val idx : variable -> int

(** [parents bn idx] @return the list of parents of the variable with
    index [idx] in network [bn]. *)
val parents : network -> int -> int list

(** [children bn idx] @return the list of children of the variable
    with index [idx] in network [bn]. *)
val children : network -> int -> int list


(** [numparents bn idx] @return the number of parents of the variable
    with index [idx] in network [bn]. *)
val numparents : network -> int -> int

(** [numchildren bn idx] @return the number of children of the
    variable with index [idx] in network [bn]. *)
val numchildren : network -> int -> int

(** [numparams bn idx] @return the number of children of the variable
    with index [idx] in network [bn]. *)
val numparams : network -> int -> int


(** {6 Create/Modify BN structure and distribution} *)

(** [create_var idx dim] @return a {!variable} whose idx is [idx] and
    range is [dim]. *)
val create_var : int -> int -> variable

(** @return a table cpd which represents a uniform distribution for
    the given variable. *) 
val create_default_cpt : variable -> cpd

(** Create hash maps from variable name -> index, and from each
    variable's values -> value index. *)
val build_namehashes :
  variable array ->
  (string, int) Hashtbl.t * (string, int) Hashtbl.t array

(** Creates an array of child lists, given an array of parent lists *)
val make_children : int list array -> int list array

(** Creates list of variables in topographic order *)
val make_topo_vars :
  variable array -> 'a list array -> int list array -> variable array

(** [create_empty_network s] creates an empty network with given
    variable schema [s] *)
val create_empty_network : int array -> network

(** Updates the children lists and topological variable order *)
val update_children_and_topo_vars : network -> unit

(** [set_cpt bn var_idx parents cpt_values] updates a CPT. Modifies BN
    in-place by replacing the parents of [var_idx] with [parents] and the
    distribtion of [var_idx] with [cpt_values] *)
val set_cpt : network -> int -> int list -> float array array -> unit

(** [set_cptree bn var_idx root] updates the CPD of variable [var_idx]
    in network [bn] with a CPD with root node [root]. Modifies [bn]
    in-place. *)
val set_cptree : network -> int -> cpnode -> unit

(** [set_factorset bn var_idx fl] updates the CPD of variable
    [var_idx] in network [bn] with factor list [fl]. Modifies [bn]
    in-place. *)
val set_factorset : network -> int -> Mn.Factor.factor list -> unit

(** [cpd_to_factors bn cvar cpd] converts [cpd] associated with
    variable [cvar] of network [bn] to Markov network factors *) 
val cpd_to_factors :
  network -> Mn.Factor.variable -> cpd -> Mn.Factor.factor list

(** Converts the Bayesian network to a Markov network by introducing
    one factor for each CPD. *) 
val to_mn : network -> Mn.network

(** [simplify bn ev] simplifies CPDs of netwprk [bn] given evidence
    [ev] if the CPDs are represented using trees or factor sets. This
    speeds up inference slightly by reducing the depth of each tree.  This
    modifies the BN.
{b NOTE}: Resulting CPDs may not be normalized, and hence be merely
  potential functions, not conditional probability distributions. *)
val simplify : network -> Mn.Factor.varvalue array -> network

(** {6 Evaluation of BNs} *)

(** [tree_logprob x cpdnode] @return a log conditional probability
    from a tree-based CPD [cpdnode] for example [x]. *)
val tree_logprob : int array -> cpnode -> float array

(** [node_logscore bn x v]  @return log conditional probability of the
    current state of variable [v] given its parents for example [x]. *)
val node_logscore : network -> Mn.Factor.varvalue array -> int -> float

(** [mb_logprob bn x i] @return log probability of X_i given its
    Markov blanket. *)
val mb_logprob : network -> Mn.Factor.varvalue array -> int -> float array

(** [mb_prob bn x i] @return probability of X_i given its Markov
    blanket. *)
val mb_prob : network -> Mn.Factor.varvalue array -> int -> float array

(** [cond_prob bn x i] @return probability of X_i given its parents. *)
val cond_prob : network -> Mn.Factor.varvalue array -> int -> float array


(** [loglikelihood bn x] computes the log-likelihood of the network on
    a single example [x]. {b Note:} All values must be specified -- no
    missing data! *)
val loglikelihood : network -> Mn.Factor.varvalue array -> float

(** [pll bn x] computes the pseudo-log-likelihood of the network on a
    single example [x]. {b Note:} All values must be specified -- no
    missing data! *)
val pll : network -> Mn.Factor.varvalue array -> float

(** Utility function for sampling a multinomial given an array
    containing the probability of each state. *)
val sample_array : float array -> int

(** Generates a single iid sample from the BN probability
    distribution. *)
val sample : network -> Mn.Factor.varvalue array


(** {6 Read/Write BNs} *)

(** Input a BN in BIF format. *)
val load_bif : in_channel -> BnType.network

(** Output a BN in BIF format. *)
val output_bif : out_channel -> BnType.network -> unit

(** Input a BN in .xmod format. *)
val load_xmod : in_channel -> BnType.network

(** Output a BN in .xmod format. *)
val output_xmod : out_channel -> BnType.network -> unit

(** Input a BN in CN format. *)
val load_cn : in_channel -> BnType.network

(** Output a BN in CN format. *)
val output_cn : out_channel -> BnType.network -> unit


(** Checks whether the filename ends with .xmod *)
val filename_is_xmod : string -> bool

(** Checks whether the filename ends with .bif *)
val filename_is_bif : string -> bool

(** Checks whether the filename ends with .cn, .dn, or .bn *)
val filename_is_cn : string -> bool

(** Checks whether the filename ends with .dn *)
val filename_is_dn : string -> bool

(** Read a BN from a file, inferring the format from [filename]. 
    See Section 4 of the {{: http://libra.cs.uoregon.edu/manual.pdf } 
    user manual}. *)
val load_auto : string -> BnType.network

(** Write the BN to a file, inferring the format from [filename]. 
    See Section 4 of the {{: http://libra.cs.uoregon.edu/manual.pdf } 
    user manual}. *)
val write_auto : string -> BnType.network -> unit

