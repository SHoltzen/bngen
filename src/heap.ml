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

(* Simple implementation of an array-based binary heap in OCaml.
 * Automatically resizes when attempting to add to a full array.
 * Based on pseudo-code from http://en.wikipedia.org/wiki/Heapsort.
 *)

(* Type declaration *)
type 'a heap = {mutable data: 'a array;      
                mutable size: int;           
                lessthan: 'a -> 'a -> bool} 

(* Create a heap with given ordering and capacity. *)
let create lessthan capacity = 
  (* Initially, size holds the intended capacity.
   * Once a single element has been added, the array can be
   * constructed and size holds the number of elements in the array. *)
  {data=[||]; size=capacity; lessthan=lessthan}

(* Grow to specified capacity *)
let realloc h capacity = 
  if capacity < 0 then invalid_arg "Heap.realloc";
  (* If array hasn't been allocated yet, just update its capacity *)
  if h.data = [||] then
    h.size <- capacity
  (* Otherwise, create new larger array and copy data *)
  else if capacity > h.size then begin
    let data = Array.make capacity h.data.(0) in
    for i = 0 to h.size - 1 do
      data.(i) <- h.data.(i)
    done;
    h.data <- data
  end

(* Number of items in heap *)
let size h = if h.data = [||] then 0 else h.size

let is_empty h = size h == 0

(* Automatic resizing *)
let grow h = realloc h (max (h.size*3/2) (h.size + 10))

(* Swap two elements in the heap array. *)
let swap h i j =
  let temp = h.data.(i) in
  h.data.(i) <- h.data.(j);
  h.data.(j) <- temp

(* Move element down to satisfy heap constraint *)
let rec sift_down h root =
  let lchild = root * 2 + 1 in
  let rchild = lchild + 1 in
  if lchild < h.size then
    let swapnode = ref root in
    if h.lessthan h.data.(lchild) h.data.(!swapnode) then
      swapnode := lchild;
    if rchild < h.size && h.lessthan h.data.(rchild) h.data.(!swapnode) then
      swapnode := rchild; 
    if !swapnode != root then begin
      swap h root !swapnode;
      sift_down h !swapnode
    end

(* Move element up to satisfy heap constraint *)
let rec sift_up h child =
  if child > 0 then begin
    let parent = (child - 1)/2 in
    if h.lessthan h.data.(child) h.data.(parent) then begin
      swap h parent child;
      sift_up h parent
    end
  end

(* Add element to heap, growing if necessary *)
let add h x =
  if h.data = [||] then begin
    (* Lazy array construction *)
    h.data <- Array.make h.size x;
    h.size <- 1
  end else begin
    (* Automatic resizing *)
    if Array.length h.data == h.size then grow h;
    h.size <- h.size + 1;
    h.data.(h.size - 1) <- x;
    sift_up h (h.size - 1)
  end

(* Remove smallest element in heap (root) *)
let remove_min h =
  if is_empty h then invalid_arg "Heap.remove_min";
  h.size <- h.size - 1;
  h.data.(0) <- h.data.(h.size);
  sift_down h 0

(* Get ith element in heap *)
let get h i = h.data.(i)

(* Remove ith element *)
let remove h i =
  swap h i (h.size - 1);
  sift_up h i;
  sift_down h i;
  h.size <- h.size - 1

(* Access smallest element in heap *)
let min h = 
  if is_empty h then raise Not_found
  else h.data.(0)

(* Fix all out-of-order elements in the heap in linear time *)
let rebuild h =
  for i = (h.size - 1)/2 downto 0 do
    sift_down h i
  done

(* Build a heap from an existing array.  Runs in O(n) time. *)
let build lessthan a =
  let h = {data = Array.copy a; size = Array.length a; lessthan = lessthan} in
  rebuild h;
  h

(* Remove all elements from the heap that match the criterion.
 * Runs in O(n) time.
 *)
let remove_all h f =
  (* Remove all elements matching the criterion *)
  let oldsize = h.size in
  for i = h.size - 1 downto 0 do 
    if f h.data.(i) then begin
      h.data.(i) <- h.data.(h.size - 1);
      h.size <- h.size - 1
    end
  done;
  (* Rebuild the heap if necessary *)
  if oldsize <> h.size then 
    rebuild h 

(* Remove all elements *)
let clear h =
  (* Save capacity to reconstruct later *)
  h.size <- Array.length h.data;
  h.data <- [||]

(* Iterate over heap elements *)
let iter f h =
  for i = 0 to h.size - 1 do
    f h.data.(i)
  done

(* Construct array from heap elements *)
let to_array h =
  if h.data = [||] then [||]
  else begin
    let a = Array.make h.size h.data.(0) in
    for i = 0 to Array.length a - 1 do
      a.(i) <- h.data.(i)
    done;
    a
  end

(* Construct list from heap elements *)
let to_list h =
  let l = ref [] in
  for i = h.size - 1 downto 0 do
    l := h.data.(i) :: !l
  done;
  !l
