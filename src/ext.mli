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

(** The {b ext} library includes miscellaneous utility functions,
    extensions to the OCaml standard library (including List and Array),
    and two utility modules, Timer and Heap (priority queue implemented as
    a binary heap). {b ext} also includes common arguments, shared
    mechanisms for working with log files, probability distributions, and
    more. *)


(** Standard Ocaml List module with extended functionality. *)
module List :
  sig

    (** {5 Extended functionality} *)

    (** [range n] creates a list with values from 0 to [n-1]. *)  
    val range : int -> int list
    
    (** [wrap x] returns a list that contains [x]. *)
    val wrap : 'a -> 'a list

    (** [cons x l] appends the value [x] to the list [l]. [x] is the
        head of the result list. *) 
    val cons : 'a -> 'a list -> 'a list
    
    (** [remove_fast x l] removes the value [x] from the list [l]. *)
    val remove_fast : 'a -> 'a list -> 'a list
    
    (** [sum l] return the sum of the elements of the int list [l]. *)
    val sum : int list -> int
    
    (** [sum l] returns the sum of the elements of the float list [l]. *)
    val sumf : float list -> float

    (** [sum_map f l] applies the function [f] on each element of the
        int list [l] and then returns the sum of the results. *) 
    val sum_map : ('a -> int) -> 'a list -> int

    (** [sumf_map f l] applies the function [f] on each element of the
        float list [l] and then returns the sum of the results. *) 
    val sumf_map : ('a -> float) -> 'a list -> float

    (** [count f l] applies boolean the function [f] on each element
        of the list [l], and returns the number of times that [f] returns
        [true]. *)
    val count : ('a -> bool) -> 'a list -> int
    
    (** [rev_iter f l] is the same as [iter f l], but iterates from
        right to left. *)
    val rev_iter : ('a -> unit) -> 'a list -> unit
    
    (** [iter2 f \[a1; ...; an\] \[b1; ...; bn\] \[c1; ...; c_n\]]
        calls in turn [f a1 b1 c1; ...; f an bn cn\] ]. *) 
    val iter3 :
      ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> unit
    
    (** [map f \[a1; ...; an\] \[b1; ...; bn\] \[c1; ...; c_n\]]
        returns the list [f a1 b1 c1; ...; f an bn cn]. Not
        tail-recursive. *) 
    val map3 :
      ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
    
    (** Transform a list of triples into a triple of lists: 
        [split3 \[(a1,b1,c1); ...; (an,bn,cn)\]] is 
        [(\[a1; ...; an\], \[b1; ...; bn\])]. Not tail-recursive. *)
    val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
    
    (** [transpose m] transposes the matrix [m] represented as a list
        of lists. *) 
    val transpose : 'a list list -> 'a list list

    (** [rem_first l] returns the tail of the list [l]. *) 
    val rem_first : 'a list -> 'a list

    (** [rem_item l i] returns a list that has the same order as the
        list [l], which does not include the value [i]. Not
        tail-recursive.  *)   
    val rem_item: 'a list -> 'a -> 'a list 

    (** {5 Functions from the original List module} *)

    val length : 'a list -> int
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val nth : 'a list -> int -> 'a
    val rev : 'a list -> 'a list
    val append : 'a list -> 'a list -> 'a list
    val rev_append : 'a list -> 'a list -> 'a list
    val concat : 'a list list -> 'a list
    val flatten : 'a list list -> 'a list
    
    val iter : ('a -> unit) -> 'a list -> unit
    val iteri : (int -> 'a -> unit) -> 'a list -> unit
    val map : ('a -> 'b) -> 'a list -> 'b list
    val rev_map : ('a -> 'b) -> 'a list -> 'b list
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
    val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val fold_right2 :
      ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
    val for_all : ('a -> bool) -> 'a list -> bool
    val exists : ('a -> bool) -> 'a list -> bool
    val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val mem : 'a -> 'a list -> bool
    val memq : 'a -> 'a list -> bool
    val find : ('a -> bool) -> 'a list -> 'a
    val filter : ('a -> bool) -> 'a list -> 'a list
    val find_all : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
    val assoc : 'a -> ('a * 'b) list -> 'b
    val assq : 'a -> ('a * 'b) list -> 'b
    val mem_assoc : 'a -> ('a * 'b) list -> bool
    val mem_assq : 'a -> ('a * 'b) list -> bool
    val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
    val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
    val split : ('a * 'b) list -> 'a list * 'b list
    val combine : 'a list -> 'b list -> ('a * 'b) list
    val sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
    val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
    val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  end

  
(** Standard Ocaml Array module with extended functionality. *)
module Array :
  sig
    (** {5 Extended functionality } *)

    (** [sum a] returns the sum of the elements of the int array [a]. *)
    val sum : int array -> int
    
    (** [sum a] returns the sum of the elements of the float array [a]. *)
    val sumf : float array -> float
    
    (** [sum_map f a] applies the function [f] on each element of the
        int array [a] and then returns the sum of the results. *) 
    val sum_map : ('a -> int) -> 'a array -> int
 
    (** [sum_map f a] applies the function [f] on each element of the
        float array [a] and then returns the sum of the results. *) 
    val sumf_map : ('a -> float) -> 'a array -> float

    (** [find p a] returns the first element of the array [a] 
        that satisfies the predicate [p].
        @raise Not_found if there is no value that satisfies [p] in the
        list [l]. *)
    val find : ('a -> bool) -> 'a array -> 'a
    
    (** [map2 f \[|a1; ...; an|\] \[|b1; ...; bn|\]] is 
        [\[|f a1 b1; ...; f an bn|\]]. *)
    val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
    
    (** [iter2 f \[|a1; ...; an|\] \[|b1; ...; bn|\]] calls in turn 
        [f a1 b1; ...; f an bn]. *)
    val iter2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> unit
   
    (** [rev_iter f a] is the same as [iter f a], but iterates from
        right to left. *)
    val rev_iter : ('a -> 'b) -> 'a array -> unit

    exception Found
    
    (** [exists p \[|a1; ...; an|\]] checks if at least one element of
        the array satisfies the predicate [p]. 
        @raise Found when the predicate is satisfied. *)
    val exists : ('a -> bool) -> 'a array -> bool

    (** [for_all p \[|a1; ...; an|\]] checks if all elements of the
        array satisfy the predicate [p]. *) 
    val for_all : ('a -> bool) -> 'a array -> bool
   
    (** [count f a] applies the boolean function [f] on each element
        of the array [a] and returns the number of times that [f] returns
        [true]. *)
    val count : ('a -> bool) -> 'a array -> int
    
    (** [min a] returns the mininum value of the array [a]. *)
    val min : 'a array -> 'a

    (** [max a] returns the maximum value of the array [a]. *)
    val max : 'a array -> 'a

    (** [argmin a] returns the index of the smallest value in the
        array [a]. *)
    val argmin : 'a array -> int
 
    (** [argmax a] returns the index of the largest value in the array
        [a]. *)
    val argmax : 'a array -> int

    (** [rev_inplace a] reverses the array [a] in place. *)
    val rev_inplace : 'a array -> unit

    (** [transpose m] transposes the matrix [m] represented as an
        array of arrays. *)
    val transpose : 'a array array -> 'a array array
    
    (** The elements of the argument are all concatenated together (in
        the same order) to give the result. Not tail-recursive. *)
    val flattenl : 'a list array -> 'a array

    (** Flatten a matrix (array of array) into an array. *)
    val flatten : 'a array array -> 'a array
		
    (** [rev_flatten a dimx dimy] creates a matrix given an array and
        the dimension of the matrix. *)
    val rev_flatten : 'a array -> int -> int -> 'a array array 

    (** [filter p a] returns all the elements of the array [a] that
        satisfy the predicate [p]. *)
    val filter : ('a -> bool) -> 'a array -> 'a array

    (** [partition p a] returns a pair of arrays [(a1, a2)], where
        [a1] is the array of all the elements of [a] that satisfy the
        predicate [p], and [a2] is the array of all the elements of [a]
        that do not satisfy [p]. The order of the elements in the input
        array is preserved. *)
    val partition : ('a -> bool) -> 'a array -> 'a array * 'a array

    (** @return a new array which is the reverse of the argument. *)
    val rev : 'a array -> 'a array
    
    (** @deprecated  Use [transpose] instead. *)
    val trans: 'a array array -> 'a array array
    
    (** [take a ai] removes the indices [ai] from the array [a]. *) 
    val take: 'a array -> int array -> 'a array

    (** [takeCol m cols] removes the columns [cols] from the matrix
        [m]. The matrix [m] is represented using an array of arrays. *) 
    val takeCol: 'a array array -> int array -> 'a array array

    (** [takeRow m rows] removes the rows [rows] from the matrix [m].
        The matrix [m] is represented using an array of arrays. *) 
    val takeRow: 'a array array -> int array -> 'a array array
    
    (** [range n] creates an array with values from 0 to [n-1]. *)  
    val range: int -> int array

    (** Creates a new array such that if the [i]th entry of the given
        array has value [v], then the [i]th entry of the new array
        contains the pair [(v,i)]. *) 
    val augment: 'a array -> ('a*int) array

    (** {5 Functions from the original Array module} *)

    external length : 'a array -> int = "%array_length"
    external get : 'a array -> int -> 'a = "%array_safe_get"
    external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
    external make : int -> 'a -> 'a array = "caml_make_vect"
    external create : int -> 'a -> 'a array = "caml_make_vect"
    val init : int -> (int -> 'a) -> 'a array
    val make_matrix : int -> int -> 'a -> 'a array array
    val create_matrix : int -> int -> 'a -> 'a array array
    val append : 'a array -> 'a array -> 'a array
    val concat : 'a array list -> 'a array
    val sub : 'a array -> int -> int -> 'a array
    val copy : 'a array -> 'a array
    val fill : 'a array -> int -> int -> 'a -> unit
    val blit : 'a array -> int -> 'a array -> int -> int -> unit
    val to_list : 'a array -> 'a list
    val of_list : 'a list -> 'a array
    val iter : ('a -> unit) -> 'a array -> unit
    val map : ('a -> 'b) -> 'a array -> 'b array
    val iteri : (int -> 'a -> unit) -> 'a array -> unit
    val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
    val sort : ('a -> 'a -> int) -> 'a array -> unit
    val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
    val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
    external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
    external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
    
  end

(** [Hashset] is similar to [Hashtbl], but it stores only keys, and
the value is [unit] for all keys. @see
"http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html" for
the description of the methods for hash tables. *)  
module Hashset :
  sig
    type 'a t = ('a, unit) Hashtbl.t
    
    (** [create n] creates a new, empty hash set with initial size [n]. *)
    val create : ?random:bool -> int -> ('a, 'b) Hashtbl.t
    
    (** [add hs x] adds [x] to hash set [hs]. *)
    val add : ('a, unit) Hashtbl.t -> 'a -> unit

    (** [mem hs x] checks if [x] exists in [hs]. *)
    val mem : ('a, 'b) Hashtbl.t -> 'a -> bool

    (** [iter f hs] applies f to all items in the hash set [hs]. *)
    val iter : ('a -> unit) -> ('a, unit) Hashtbl.t -> unit

    (** [fold f hs init] computes [(f iN ... (f i1 init)...)], where
        [i1 ... iN] are the items in [hs]. *)
    val fold : ('a -> 'b -> 'b) -> ('a, unit) Hashtbl.t -> 'b -> 'b

    (** Converts the given hash set into a list. *) 
    val to_list : ('a, unit) Hashtbl.t -> 'a list

    (** [sum_map f hs] applies f to all items in [hs], returns the sum
        of all the results. [f] returns an int. *)  
    val sum_map : ('a -> int) -> ('a, unit) Hashtbl.t -> int

    (** [sum_map f hs] applies f to all items in [hs], returns the sum
        of all the results. [f] returns a float. *)  
    val sumf_map : ('a -> float) -> ('a, unit) Hashtbl.t -> float

    (** [filter f hs] applies the boolean function [f] on every item
        in [hs] and remove the item if [f] returns [false]. *)
    val filter : ('a -> bool) -> ('a, unit) Hashtbl.t -> unit

    type 'a t' = ('a, unit) Hashtbl.t
    (*module type S =
      sig
        type key
        type t
        val create : int -> t
        val clear : t -> unit
        val copy : t -> t
        val add : t -> key -> unit
        val remove : t -> key -> unit
        val mem : t -> key -> bool
        val iter : (key -> unit) -> t -> unit
        val fold : (key -> 'b -> 'b) -> t -> 'b -> 'b
        val length : t -> int
        val to_list : t -> key list
        val filter : (key -> bool) -> t -> unit
      end*)
  end


(** Simple implementation of an array-based binary heap in OCaml.
 Automatically resizes when attempting to add to a full array.
 Click {{: http://en.wikipedia.org/wiki/Heapsort} here } for the pseudo-code.
*)
module Heap :
  sig
    type 'a heap =
      'a Heap.heap = {
      mutable data : 'a array;
      mutable size : int;
      lessthan : 'a -> 'a -> bool;
    }

    (** [create lessthan n] creates a heap with the capacity [n] and
        the comparison function [lessthan]. *)
    val create : ('a -> 'a -> bool) -> int -> 'a heap

    (** [realloc h n] grows the capacity of the heap [h] to [n]. *)
    val realloc : 'a heap -> int -> unit

    (** [size h] returns the number of items in the heap [h]. *)
    val size : 'a heap -> int

    (** [is_empty h] checks if the heap [h] is empty. *)
    val is_empty : 'a heap -> bool
    
    (** [grow h] automatically resizes the size of the heap [h]. *)
    val grow : 'a heap -> unit

    (** [swap h x y] swaps two items [x] and [y] in the heap [h]. *)
    val swap : 'a heap -> int -> int -> unit

    (** [sift_down h root] moves [root] down to satisfy the heap
        property for the heap [h]. *)
    val sift_down : 'a heap -> int -> unit
    
    (** [sift_up h child] moves [child] up to satisfy the heap
        property for the heap [h]. *)
    val sift_up : 'a heap -> int -> unit

    (** [add h x] adds the element [x] to the heap [h], growing if
        necessary. *)
    val add : 'a heap -> 'a -> unit

    (** [remove_min h] removes the smallest element in heap [h] (root). *)
    val remove_min : 'a heap -> unit

    (** [get h i] gets the [i]th element in the heap [h]. *)
    val get : 'a heap -> int -> 'a

    (** [remove h i] removes the [i]th element in the heap [h]. *) 
    val remove : 'a heap -> int -> unit

    (** [min h] accesses the smallest element in the heap [h]. *)
    val min : 'a heap -> 'a

    (** [rebuild h] fixes all out-of-order elements in the heap [h] in
        linear time. *)    
    val rebuild : 'a heap -> unit

    (** [build lessthan a] builds a heap from the existing array [a]
        with respect to the comparison function [lessthan].  
        Runs in O(n) time. *)
    val build : ('a -> 'a -> bool) -> 'a array -> 'a heap

    (** [remove_all h f] removes all elements from the heap [h] that
        match the criterion [f]. Runs in O(n) time. *)
    val remove_all : 'a heap -> ('a -> bool) -> unit

    (** [clear h] removes all elements from the heap [h]. *)
    val clear : 'a heap -> unit

    (** [iter f h] applies the function [f] over the elements of the
        heap [h]. *)
    val iter : ('a -> 'b) -> 'a heap -> unit

    (** [to_array h] constructs an array from the elements of the heap [h]. *)
    val to_array : 'a heap -> 'a array

    (** [to_list h] constructs a list from the elements of the heap [h]. *)
    val to_list : 'a heap -> 'a list
  end

(** Named timers.  Each timer is associated with a string name.
 Operations are starting, stopping (or pausing), and checking
 the time elapsed on the named timer.  A single timer may be
 started and stopped multiple times, and the elapsed time will
 always be the total time spent running.
 *)
module Timer :
  sig

    (** Hash of start times. *)
    val start_time_hash : (string, float) Hashtbl.t
 
 
    (** Hash of accumulated elapsed times. *)
    val elapsed_time_hash : (string, float) Hashtbl.t
    
    (** Hash of last elapsed times. *)
    val last_elapsed_time_hash : (string, float) Hashtbl.t
    
    (** [starts name] starts a timer with the name [name].  Does
        nothing if timer has already been started. *)
    val start : string -> unit

    (** [delta name] returns time passed since the timer [name] was
        most recently started or delta was last called for this timer.
        Useful for timing sequential pieces. *)
    val delta : string -> float

    (** [elapsed name] returns the amount of time elapsed on the timer
        [name].  Counts time since the timer was started, excluding time
        while it was stopped. *)
    val elapsed : string -> float

    (** [last_elapsed name] returns the amount of time elapsed between
        last start and stop on timer [name]. *) 
    val last_elapsed: string -> float
    
    (** [stop name] stops the timer [name].  This pauses it until
        restarted with start. *)
    val stop : string -> unit

    (** [clear name] stops the timer [name] and sets its elapsed time
        to zero. *)
    val clear : string -> unit
  end


(** {5 Supported filetypes} *)

type libra_filetype =
    ACFile (** [.ac] files for arithmetic circuits. *)
  | BNFile (** [.bn] files for Bayesian networks. *)
  | MNFile (** [.mn] files for Markov networks. *)
  | DataFile (** [.data] files for data. *) 
  | SPNFile (** [.spn] files for sum-product networks. *)
  | UnknownFile
val filetype : string -> libra_filetype


(** {5 Utility functions } *)

val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

val ( $ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(** @return its arguement. *)
val identity : 'a -> 'a


(** {5 Mathematical functions } *)


(** Returns the absolute value. *)
val absf : float -> float

(** Tautology function, i.e. always returns [true]. *)
val f_true : 'a -> bool

(** Negative infinity constant. *)
val log_zero : float

(** Zero constant. *)
val log_one : float

(** [maxf x y] returns the maximum of the float values [x] and [y]. *)
val maxf : float -> float -> float

(** [minf x y] returns the minmum of the float values [x] and [y]. *)
val minf : float -> float -> float

(** [fast_exp x] returns the exponential of the given argument, but
    approximates very small return values with zero. If [x] is smaller
    than -37.0 it returns zero.*)
val fast_exp : float -> float

(** [logsumexp xl] computes [log(\sum_i e^{x_i})], where [x_i] is the
    [i]th entry of the float list [xl]. This function avoids numerical
    underflow. *)
val logsumexp : float list -> float

(** [logsumexp xa] computes [log(\sum_i e^{x_i})], where [x_i] is the
    [i]th entry of the float array [xa]. This function avoids numerical
    underflow. *)
val alogsumexp : float array -> float

(** [logsumexp2 x y] computes [log(exp(x)+exp(y)], but avoids
    numerical underflow. *)
val logsumexp2 : float -> float -> float


exception EmptyList

(** [dotprod xl yl] returns the dot product of the two float lists
    [xl] and [yl]. @raise EmptyList if the lists have different lengths. *)
val dotprod : float list -> float list -> float

(** [dotprod_bias xl yl bias] returns the dot product of the two float
    lists [xl] and [yl] plus the bias [bias]. If the size of [yl] is
    greater than [xl] it ignores the extra elements in [yl]. 
    @raise EmptyList if the size of [xl] is greater than the size of [yl]. *)
val dotprod_bias : float list -> float list -> float -> float

(** [adotprod xa ya] returns the dot product of the two float array
    [xa] and [ya]. If the size of [ya] is greater than the size of [xa] it
    ignores the extra elements in [ya]. *)
val adotprod : float array -> float array -> float

(** [adotprod_bias xa ya] returns the dot product of the two float
    array [xa] and [ya] plus the bias term [bias]. if the size [ya] is
    greater than the size of [xl] it ignores the extra elements in [ya]. *)
val adotprod_bias : float array -> float array -> float -> float

(** [adotprod_autobias xa ya] returns the dot product of the two float
    array [xa] and [ya] and assumes that the last entry in [ya] is the
    bias term. It assumes that the size of [ya] is one item more than the
    size of [xa]. *)
val adotprod_autobias  : float array -> float array -> float

(** {5 Probability related functions} *)

(** [normalize_inplace_raw xa] Normalizes the elements of [xa] so that
    they represent a valid probability distribution. Modifies [xa] in
    place. *)
val normalize_inplace_raw : float array -> unit

(** [normalize_inplace_log xa] Normalizes the elements of [xa] so that
    they represent the log probabilities of a valid probability
    distribution. Modifies [xa] in place. *)
val normalize_inplace_log : float array -> unit

(** Similar to [normalize_inplace_raw], but returns a new array as the
    result (does not modify the argument. *)  
val normalize_raw : float array -> float array

(** Similar to [normalize_inplace_log], but returns a new array as the
    result (does not modify the argument. *)  
val normalize_log : float array -> float array


(** {5 Functions for gathering statistics conveniently } *)

type stats_counter = float ref * float ref * float ref

(** Create an object to help compute of mean and variance statistics. *)
val stats_make : unit -> stats_counter

(** [stats_add s w x] adds one observation of x to the counters *)
val stats_add : stats_counter -> float -> unit

(** [stats_wadd s w x] adds w observations of x to the counters *)
val stats_wadd : stats_counter -> float -> float -> unit

(** [stats_n s] returns the number of (weighted) observations *)
val stats_n : stats_counter -> float

(** [stats_sum s] returns the sum of the observations *)
val stats_sum : stats_counter -> float

(** [stats_mean s] returns the arithmetic mean of the observations *)
val stats_mean : stats_counter -> float

(** [stats_var s] returns the estimated variance of the observations *)
val stats_var : stats_counter -> float

(** [stats_stddev s] returns the standard deviation of the observations *)
val stats_stddev : stats_counter -> float

(** [stats_stderr s] returns the standard deviation of the mean *)
val stats_stderr : stats_counter -> float 


(** {5 Common argument handling} *)
 
(** Set of arguments common to most programs *)
val common_arguments : (string * Arg.spec * string) list


(** {5 I/O and Debuging functions} *)


(** [output_int out i] writes the integer value [i] on the output
    channel [out]. *)
val output_int : out_channel -> int -> unit

(** [output_float out f] writes the float value [f] on the output
    channel [out]. *)
val output_float : out_channel -> float -> unit


(** [output_string ouput sep sa] concatinates the string elements of
    the string array [sa] using the separator [sep], and then writes the
    result on the output channel [out]. *)
val output_sep : out_channel -> string -> string array -> unit

(** Constant string [normal] for logging in the normal mode. *)
val log_normal : string

(** Constant string [verbose] for logging in the verbose mode. *)
val log_verbose : string

(** Constant string [debug] for logging in the debug mode. *)
val log_debug : string

(** Used for mapping output channel to a specific name. Libra uses
    [normal, verbose, debug] for the output channel name. *)
val log_hash : (string, out_channel) Hashtbl.t

(** [register_log name out] maps the output channel [out] to the
    string [name]. *)
val register_log : string -> out_channel -> unit

(** [unregister_log name] removes the channel-name mapping for the
    given [name].*)
val unregister_log : string -> unit

(** Checks if the given name maps to any output channel. *) 
val log_exists : string -> bool

(** Returns the output channel corresponding to the given name. *)
val log_stream : string -> out_channel

(** Outputs a string to the specified log, if defined. *)
val log_string : string -> string -> unit

(** [logf outname fmt arg1 ... argN] prints to a log named [outname] with printf-style formatting. [outname] can be one of the default log names, or a registered log. (A new log can be registered using [register_log channel outname]).  *)
val logf : string -> ('a, unit, string, unit) format4 -> 'a

(** Prints to a log with printf-style formatting in the normal mode. *)
val nlogf : ('a, unit, string, unit) format4 -> 'a

(** Prints to a log with printf-style formatting in the verbose mode. *)
val vlogf : ('a, unit, string, unit) format4 -> 'a

(** Prints to a log with printf-style formatting in the verbose mode.
    [vlogfz] performes lazy argument evaluation, so its arguments should
    be lazy expressions, e.g. [vlogfz "%d" (lazy (exp)) ]. [vlogfz]
    accepts only one argument.*)
val vlogfz : ('a -> unit, unit, string, unit) format4  -> 'a lazy_t -> unit

(** Similar to [vlogfz], but accepts two lazy arguments. *)
val vlogfz2 : ('a -> 'b -> unit, unit, string, unit) format4  -> 'a lazy_t -> 'b lazy_t -> unit

(** Similar to [vlogfz], but accepts three lazy arguments. *)
val vlogfz3 : ('a -> 'b -> 'c -> unit, unit, string, unit) format4  -> 'a lazy_t -> 'b lazy_t -> 'c lazy_t -> unit

(** Similar to [vlogfz], but accepts four arguments. *)
val vlogfz4 : ('a -> 'b -> 'c -> 'd -> unit, unit, string, unit) format4  -> 'a lazy_t -> 'b lazy_t -> 'c lazy_t -> 'd lazy_t -> unit

(** Prints to a log with printf-style formatting in the debug mode. *)
val dlogf : ('a, unit, string, unit) format4 -> 'a

(** Prints to a log with printf-style formatting in the debug mode.
    [dlogfz] performes lazy argument evaluation, so its arguments should
    be lazy expressions, e.g. [dlogfz "%d" (lazy (exp)) ]. [dlogfz]
    accepts only one argument.*)
val dlogfz : ('a -> unit, unit, string, unit) format4  -> 'a lazy_t -> unit

(** Similar to [dlogfz], but accepts two lazy arguments. *)
val dlogfz2 : ('a -> 'b -> unit, unit, string, unit) format4  -> 'a lazy_t -> 'b lazy_t -> unit

(** Similar to [dlogfz], but accepts three lazy arguments. *)
val dlogfz3 : ('a -> 'b -> 'c -> unit, unit, string, unit) format4  -> 'a lazy_t -> 'b lazy_t -> 'c lazy_t -> unit

(** Similar to [dlogfz], but accepts four lazy arguments. *)
val dlogfz4 : ('a -> 'b -> 'c -> 'd -> unit, unit, string, unit) format4  -> 'a lazy_t -> 'b lazy_t -> 'c lazy_t -> 'd lazy_t -> unit

(** Prints the arguments to a log in the verbose mode. *)
val log_args : unit -> unit

(** Initializes the common log channels in Libra. *)
val common_log_init : unit -> unit

(** Prints the given string into the standard output. *)
val debug : string -> unit

(** Converts an int list to a string. *)  
val string_of_ilist : int list -> string

(** Converts a float list to a string. *)  
val string_of_flist : float list -> string

(** Converts an int array to a string. *)  
val string_of_iarray : int array -> string

(** Converts a float array to a string. *)  
val string_of_farray : float array -> string

