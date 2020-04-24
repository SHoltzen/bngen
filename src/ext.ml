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


exception Error
(** Complementary functions for the standard module List *)
module List =
struct
  include List

  let wrap x = [x]

  let cons x l = x :: l

  let rec rec_remove_fast item accu = function
    | [] -> accu
    | x :: l when x == item -> rec_remove_fast item accu l
    | x :: l -> rec_remove_fast item (x :: accu) l

  let remove_fast x l = rec_remove_fast x [] l

  let sum l  = fold_left ( +  ) 0   l
  
  (** Computes the sum of a float list *) 
  let sumf l = fold_left ( +. ) 0.0 l

  let sum_map f l  = fold_left (fun accu x -> f x +  accu) 0   l
  let sumf_map f l = fold_left (fun accu x -> f x +. accu) 0.0 l

  let rec rec_count f accu = function
    | [] -> accu
    | x :: l -> if f x then rec_count f (accu + 1) l
                else rec_count f accu l

  let count f l = rec_count f 0 l

  let rev_iter f l = fold_right (fun n accu -> f n) l ()

  let rec iter3 f l1 l2 l3 =
    match (l1, l2, l3) with
      ([], [], []) -> ()
    | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; iter3 f l1 l2 l3
    | (_, _, _) -> invalid_arg "List.iter3"

    (* Note: not tail recursive! *)
  (*
    let rec rec_mapi f i = function
    | [] -> []
    | x :: l -> (f i x) :: rec_mapi f (i+1) l 
   *)
  (*implementated in 4.0 *)
  (*
  let mapi f l = rec_mapi f 0 l

  let rec fold_left2 f l accu = match l with
      [] -> accu
    | x :: l' -> fold_left2 f l' (f x accu)

  *)

    (* Note: not tail recursive! *)
  let rec map3 f l1 l2 l3 =
    match (l1, l2, l3) with
      ([], [], []) -> [] 
    | (a1::l1, a2::l2, a3::l3) -> (f a1 a2 a3) :: (map3 f l1 l2 l3)
    | (_, _, _) -> invalid_arg "List.map3"

  let rec split3 = function
      [] -> ([], [], [])
    | (x,y,z)::l ->
        let (rx, ry, rz) = split3 l in (x::rx, y::ry, z::rz)


  let rec transpose l =
    try map hd l :: transpose (map tl l) with _ -> []


  let rem_first l = 
      match l with            
        | [] -> []
    | h::t -> t 

  let rem_item l item = 
    let newl = ref [] in
    let la = Array.of_list l in
    for i = (List.length l) - 1 downto 0 do 
    if la.(i) <> item then newl := la.(i)::!newl
    done;
    !newl


  let rec rev_range x =
    if x < 0 then [] 
    else x :: rev_range (x-1)

  let range n = List.rev (rev_range (n-1))

end

module Array =
struct
  include Array

  let sum a  = 
    let total = ref 0 in
    for i = 0 to Array.length a - 1 do
      total := !total + a.(i)
    done; !total

  let sumf a = 
    let total = ref 0.0 in
    for i = 0 to Array.length a - 1 do
      total := !total +. a.(i)
    done; !total

  let sum_map f a = 
    let total = ref 0 in
    for i = 0 to Array.length a - 1 do
      total := !total + f a.(i)
    done; !total

  let sumf_map f a = 
    let total = ref 0.0 in
    for i = 0 to Array.length a - 1 do
      total := !total +. f a.(i)
    done; !total

  let find f a = 
    let rec recfind offset =
      if offset >= Array.length a then 
        raise Not_found
      else if f a.(offset) then 
        a.(offset) 
      else 
        recfind (offset+1) in
    recfind 0

    (* TODO: check lengths... *)
  let map2 f a1 a2 = mapi (fun i x -> f x a2.(i)) a1

  let iter2 f a1 a2 = (* iteri (fun i x -> f x a2.(i)) a1 *)
    for i = 0 to length a1 - 1 do
      f a1.(i) a2.(i)
    done

  let rev_iter f a =
    for i = length a - 1 downto 0 do
      f a.(i)
    done

  exception Found

  let exists f a =
    try 
      for i = 0 to length a - 1 do
        if f (unsafe_get a i) then raise Found
      done ; false
    with Found -> true
 
  let for_all f a =
    try
      for i = 0 to length a - 1 do
        if not (f (unsafe_get a i)) then raise Found
      done ; true
    with Found -> false

  let count f a =
    let cnt = ref 0 in
    for i = 0 to length a - 1 do
      if f (unsafe_get a i) then incr cnt
    done ; !cnt

  let argmin a =
    let best = ref 0 in
    for i = 1 to Array.length a - 1 do
      if a.(i) < a.(!best) then
        best := i
    done ;
    !best

  let argmax a =
    let best = ref 0 in
    for i = 1 to Array.length a - 1 do
      if a.(i) > a.(!best) then
        best := i
    done ;
    !best

  let min a =
    let best = ref a.(0) in
    for i = 1 to Array.length a - 1 do
      if a.(i) < !best then
        best := a.(i)
    done ;
    !best

  let max a =
    let best = ref a.(0) in
    for i = 1 to Array.length a - 1 do
      if a.(i) > !best then
        best := a.(i)
    done ;
    !best

  let rev_inplace a =
    let l = Array.length a in
    for i = 0 to (l - 1)/2 do
      let j = l - 1 - i in 
      let tmp = a.(j) in
      a.(j) <- a.(i);
      a.(i) <- tmp
    done

  let transpose a =
    let rows = Array.length a in
    let cols = Array.length a.(0) in
    let a' = Array.make_matrix cols rows a.(0).(0) in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        a'.(j).(i) <- a.(i).(j)
      done
    done;
    a'

  let rec flattenl_helper result offset = function
  | [] -> result
  | a :: l ->
    for i = 0 to Array.length a - 1 do 
      result.(offset + i) <- a.(i)
    done;
    flattenl_helper result (offset + Array.length a) l

  let rec copy_list result offset = function
  | [] -> offset
  | x :: l -> 
    result.(offset) <- x; 
    copy_list result (offset+1) l

  let flattenl la =
    let len = sum_map List.length la in
    if len = 0 then [||]
    else
      let init = List.hd (find ((<>) []) la) in
      let a = Array.make len init in
      let offset = ref 0 in
      for i = 0 to Array.length la - 1 do
        offset := copy_list a !offset la.(i)
      done;
      a

  let flatten a = 
    let dimx = Array.length a in
    let dimy = Array.length a.(0) in
    let newA = Array.make (dimx * dimy) a.(0).(0) in
    for i = 0 to dimx - 1 do
      for j = 0 to dimy - 1 do
        newA.(i * dimy + j ) <- a.(i).(j)
      done
    done;
    newA

  let rev_flatten a dimx dimy =
    let newA = Array.make_matrix dimx dimy a.(0) in
    for i = 0 to dimx - 1 do
      for j = 0 to dimy - 1 do
        newA.(i).(j)  <- a.(i * dimy + j )
      done
    done;
    newA
     

  let filter f a =
    let y = ref [] in
    for i = Array.length a - 1 downto 0 do 
      if f a.(i) then
        y := a.(i) :: !y
    done;
    Array.of_list !y

  let partition f a =
    let y = ref [] and n = ref [] in
    for i = Array.length a - 1 downto 0 do 
      if f a.(i) then
        y := a.(i) :: !y
      else
        n := a.(i) :: !n
    done;
    (Array.of_list !y, Array.of_list !n)

  let rev a =
    let a' = Array.copy a in
    rev_inplace a'; a'

  let trans a = 
    let m = Array.length a in
    let n = Array.length a.(0) in
      for i = 1 to m-1 do 
      if Array.length a.(i) <> n then raise Error 
    done;
      Array.init n (fun i -> Array.init m (fun j -> a.(j).(i)))

  let take a indexes =
    let n = Array.length indexes in
    let newAr = if n > 0 then Array.make n a.(0) else [||] in
    for i=0 to n - 1 do
      newAr.(i) <- a.(indexes.(i))
    done;
    newAr
  
  let takeCol matrix indexes =
    let matrix' = trans matrix in
    let augemented = Array.mapi (fun i row->(row, i)) matrix' in
    let result = take augemented indexes in
    let reg = Array.map (fst) result in
    let reg' = trans reg in
    reg'

  let takeRow matrix indexes =
    let augemented = Array.mapi (fun i row->(row, i)) matrix in
    let result = take augemented indexes in
    let reg = Array.map (fst) result in
    reg


  let range n = 
    let ar = Array.make n 0 in
    for i = 0 to n - 1 do
      ar.(i) <- i
    done;
    ar

(*
  let subtract a b =
    let y = ref [] in
    for i = (Array.length a) downto 0 do
      if (Array.find (fun x-> x == a.(i)) b ) then 
        ignore()
      else
        y := a.(i) :: !y
    done;
    Array.of_list !y
*)
  let augment a =
    Array.mapi ( fun i x-> (x,i) ) a  



end

module Hashtbl =
struct
  include Hashtbl
  (* let keys_to_list h = Hashtbl.fold (fun x k l -> x :: l) h []
   * let dfind h key default_value =
   *   try Hashtbl.find h key with Not_found -> default_value *)
end

(* Define hash set type, along with functor interface *)
module Hashset =
struct
  type 'a t = ('a, unit) Hashtbl.t
  open Hashtbl

  let create = create
  let add hs x = if not (mem hs x) then add hs x ()
  let mem hs x = mem hs x
  let iter f hs = iter (fun x () -> f x) hs
  let fold f hs = fold (fun x () l -> f x l) hs
  let to_list hs = fold (fun x l -> x :: l) hs []
  let sum_map f hs = fold (fun x accu -> f x + accu) hs 0
  let sumf_map f hs = fold (fun x accu -> f x +. accu) hs 0.0
  let filter f hs = iter (fun n -> if not (f n) then remove hs n) hs
  type 'a t' = ('a, unit) Hashtbl.t

  module type S =
    sig
      type key
      type t
      val create: int -> t
      val clear: t -> unit
      val copy: t -> t
      val add: t -> key -> unit
      val remove: t -> key -> unit
      val mem : t -> key -> bool
      val iter: (key -> unit) -> t -> unit
      val fold: (key -> 'b -> 'b) -> t -> 'b -> 'b
      val length: t -> int
      val to_list: t -> key list
      val filter: (key -> bool) -> t -> unit
    end
end

module String =
struct
  include String
  let rec match_beg s1 s2 i =
    if i <= 0 then 
      true
    else if String.get s1 i != String.get s2 i then
      false
    else
      match_beg s1 s2 (i-1)

  let rec match_end s1 s2 i =
    let s1l = String.length s1 in
    let s2l = String.length s2 in
    if i <= 0 then
      true
    else if s1l >= i && s2l >= i &&
        String.get s1 (s1l - i) != String.get s2 (s2l - i) then
      false
    else
      match_end s1 s2 (i-1)

  let suffix s1 s2 = match_end s1 s2 (length s2)
  let prefix s1 s2 = match_beg s1 s2 (length s2)
end

(*
 * External modules included with this library
 *)
module Heap =
struct
  include Heap
end

module Timer =
struct
  include Timer
end


(* Utility methods *)
    
let compose f g x = f(g(x))
let ($) f g x = f (g x)

let identity x = x

(** tautology function, i.e. always returns [true] *)
let f_true x = true

let absf f = if f >= 0.0 then f else -.f


(* Important math stuffs *)

(* Useful constants *)
let log_zero = neg_infinity
let log_one = 0.0

exception EmptyList

let rec dotprod_bias a b bias = 
 (match a, b with
    ah :: at, bh :: bt -> (ah *. bh) +. dotprod_bias at bt bias
  | [], _ -> bias
  | _ -> raise EmptyList)


let rec dotprod a b = 
 (match a, b with
    ah :: at, bh :: bt -> (ah *. bh) +. dotprod at bt
  | [], [] -> 0.0
  | _ -> raise EmptyList)

let adotprod a b = 
  let total = ref 0.0 in
  for i = 0 to Array.length a - 1 do
    total := !total +. a.(i) *. b.(i)
  done ;
  !total

let adotprod_bias a b bias = 
  let total = ref bias in
  for i = 0 to Array.length a - 1 do
    total := !total +. a.(i) *. b.(i)
  done ;
  !total


let adotprod_autobias a b = 
  let len = Array.length a in
  let total = ref b.(len) in
  for i = 0 to len - 1 do
    total := !total +. a.(i) *. b.(i)
  done ;
  !total

let maxf (a : float) (b : float) =
  if a > b then a else b

let minf (a : float) (b : float) =
  if a < b then a else b

let fast_exp x =
  if x > (-37.0) then exp x
  else 0.0

let logsumexp lv =
  match lv with x::[] -> x | [] -> log_zero | x::l ->
  let base = List.fold_left maxf log_zero lv in
  if base = log_zero then log_zero
  else
    let normlv = List.map (fun x -> x -. base) lv in
    let nonzero = List.filter (fun x -> x > (-37.0)) normlv in
    base +. log (List.sumf (List.map exp nonzero))

let logsumexp2 a b =
  if a > b then
    a +. log (1.0 +. exp(b -. a))
  else if a < b then
    b +. log (1.0 +. exp(a -. b))
  else
    a +. log 2.0
(*
let logsumexp2 a b =
  let (big, small) = if a > b then (a,b) else (b,a) in
  let diff = big -. small in
  if diff > 10.0 then big
  else big +. log (1.0 +. exp (small -. big))
 *)

let alogsumexp lv =
  if Array.length lv = 1 then lv.(0) else
  let base = Array.fold_left maxf log_zero lv in
  if base = log_zero then log_zero 
  else begin
    let total = ref (fast_exp(lv.(0) -. base)) in
    for i = 1 to Array.length lv - 1 do
      total := !total +. fast_exp(lv.(i) -. base)
    done;
    base +. log(!total)
  end

let output_int channel i = output_string channel (string_of_int i)

let output_float channel i = output_string channel (string_of_float i)

let output_sep channel sep a =
  for i = 0 to Array.length a - 1 do
    if i > 0 then output_string channel sep;
    output_string channel a.(i)
  done

(*
let print_objsize x = 
  let t = Objsize.objsize x in
  Printf.printf "Data: %d;  Headers: %d;  Depth: %d\n" 
    t.Objsize.data t.Objsize.headers t.Objsize.depth ;
  Printf.printf "Bytes with headers: %d\n" (Objsize.size_with_headers t) ;
  Printf.printf "Bytes without headers: %d\n" (Objsize.size_without_headers t)
  *)

(*
 * Common argument handling
 *)

(* These module-level variables shouldn't be too dangerous,
 * since there is presumably one set of arguments per program,
 * so these shouldn't get used twice.
 *)
let __verbose = ref false
let __debugging = ref false
let __logfile = ref ""
let common_arguments =
   [("-log", Arg.Set_string __logfile, " Log file") ;
    ("-v", Arg.Set __verbose, " Enable verbose output");
    ("-debug", Arg.Set __debugging, " Enable debugging output")]

(*
 * Unified logging
 *)
let log_hash = Hashtbl.create 100

let register_log (name:string) stream =
  Hashtbl.add log_hash name stream

let unregister_log name =
  Hashtbl.remove log_hash name

let log_exists name =
  Hashtbl.mem log_hash name

let log_stream name =
  Hashtbl.find log_hash name

(* Output a string to the specified log, if defined *)
let log_string name s =
  if Hashtbl.mem log_hash name then 
    output_string (Hashtbl.find log_hash name) s

(* Print to a log with printf-style formatting *)
let logf name fmt = Printf.ksprintf (log_string name) fmt

let log_normal = "normal"
let log_verbose = "verbose"
let log_debug = "debug"

let nlogf fmt = logf log_normal fmt
let vlogf fmt = logf log_verbose fmt
let dlogf fmt = logf log_debug fmt


let vlogfz fmt arg1 = if log_exists log_verbose then logf log_verbose fmt (Lazy.force arg1) else ignore()
let vlogfz2 fmt arg1 arg2 = if log_exists log_verbose then logf log_verbose fmt (Lazy.force arg1) (Lazy.force arg2) else ignore()
let vlogfz3 fmt arg1 arg2 arg3 = if log_exists log_verbose then logf log_verbose fmt (Lazy.force arg1) (Lazy.force arg2) (Lazy.force arg3) else ignore()
let vlogfz4 fmt arg1 arg2 arg3 arg4 = if log_exists log_verbose then logf log_verbose fmt (Lazy.force arg1) (Lazy.force arg2) (Lazy.force arg3) (Lazy.force arg4) else ignore()

let dlogfz fmt arg1 = if log_exists log_debug then logf log_debug fmt (Lazy.force arg1) else ignore()
let dlogfz2 fmt arg1 arg2 = if log_exists log_debug then logf log_debug fmt (Lazy.force arg1) (Lazy.force arg2) else ignore()
let dlogfz3 fmt arg1 arg2 arg3 = if log_exists log_debug then logf log_debug fmt (Lazy.force arg1) (Lazy.force arg2) (Lazy.force arg3) else ignore()
let dlogfz4 fmt arg1 arg2 arg3 arg4 = if log_exists log_debug then logf log_debug fmt (Lazy.force arg1) (Lazy.force arg2) (Lazy.force arg3) (Lazy.force arg4) else ignore()


let log_args () =
  (* Convert the first argument from: "...path.../_libra_<command>" 
     to: "...path.../libra command". *)
  let cmd = Sys.argv.(0) in
  if String.contains cmd '_' 
      && String.rindex cmd '_' + 1 >= String.length "_libra_" then begin
    let i = String.rindex cmd '_' in
    let prelibra = String.sub cmd 0 (i+1 - String.length "_libra_") in
    let postlibra = String.sub cmd (i+1) (String.length cmd - (i+1)) in 
    Sys.argv.(0) <- String.concat "" [prelibra; "libra "; postlibra]
  end;
  let argstr = String.concat " " (Array.to_list Sys.argv) in
  vlogf "Arguments: %s\n" argstr

let common_log_init () =
  let debugging = !__debugging in
  let verbose = !__verbose || !__debugging in
  let logout = 
    if !__logfile = "" then stdout else open_out !__logfile in
  register_log log_normal logout;
  if verbose then register_log log_verbose logout;
  if debugging then register_log log_debug logout;
  log_args ()


 (*
  * Debug stuff
  *)

let debug s = print_string s; print_string "\n"; flush stdout

let string_of_ilist l = String.concat " " (List.map string_of_int l)
let string_of_flist l = String.concat " " (List.map string_of_float l)
let string_of_iarray a = string_of_ilist (Array.to_list a)
let string_of_farray a = string_of_flist (Array.to_list a)


 (*
  * Probability stuff
  *)

let normalize_inplace_raw a =
  let total = Array.sumf a in
  for i = 0 to Array.length a - 1 do
    a.(i) <- a.(i) /. total
  done

let normalize_inplace_log a =
  let logz = alogsumexp a in
  for i = 0 to Array.length a - 1 do
    a.(i) <- a.(i) -. logz
  done


let normalizelog a =
  let logsum = alogsumexp a in (*Array.sumf (Array.map (exp) a) in *)
  let n_a = Array.map (fun x->  x  -. logsum) a in
  n_a

let normalize_raw a = 
  let a = Array.copy a in
  normalize_inplace_raw a;
  a

let normalize_log a = 
  let a = Array.copy a in
  normalize_inplace_log a;
  a

type stats_counter = float ref * float ref * float ref

(* Keep track of statistics conveniently *)
let stats_make () = (ref 0., ref 0., ref 0.)

let stats_wadd (sum, sumsq, n) w x =
  sum := !sum +. x *. w;
  sumsq := !sumsq +. x *. x *. w;
  n := !n +. w

let stats_add s x = stats_wadd s 1. x

let stats_n (sum, sumsq, n) = 
  !n

let stats_sum (sum, sumsq, n) = 
  !sum

let stats_mean (sum, sumsq, n) = 
  if !n = 0. then 0.
  else !sum /. !n

let stats_var (sum, sumsq, n) = 
  if !n = 0. then 0.
  else (!sumsq -. !sum *. !sum /. !n) /. !n

let stats_epsilon = 1.0e-10

let stats_stddev s = 
  let v = stats_var s in
  if v < 0.0 && v > -.stats_epsilon then
    0.0
  else
    sqrt(stats_var s)

let stats_stderr (sum, sumsq, n) =
  (stats_stddev (sum, sumsq, n)) /. (sqrt !n)


(* TODO -- is this the right place for this? *)
type libra_filetype = ACFile | BNFile | MNFile | DataFile | SPNFile | UnknownFile

let filetype filename =
  let filename = String.lowercase filename in
  if String.suffix filename ".ac" then ACFile
  else if String.suffix filename ".xmod" then BNFile
  else if String.suffix filename ".bif" then BNFile
  else if String.suffix filename ".dn" then BNFile
  else if String.suffix filename ".bn" then BNFile
  else if String.suffix filename ".mn" then MNFile
  else if String.suffix filename ".uai" then MNFile
  else if String.suffix filename ".simple" then MNFile
  else if String.suffix filename ".data" then DataFile
  else if String.suffix filename ".spn" then SPNFile
  else UnknownFile
