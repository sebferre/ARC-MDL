
open Madil_common

(* error handling *)
              
exception Undefined_result of string (* for undefined computations *)

(* configuration, options *)
                   
let def_param (name : string) (v : 'a) (to_str : 'a -> string) : 'a ref =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v

(* syntactic sugar and utilities for lists *)
                    
let ( let$ ) (init,l) f =
  l |> List.fold_left (fun res x -> f (res, x)) init [@@inline]
let ( let& ) l f =
  l |> List.iter f [@@inline]

(* lists *)

let rec list_range a b =
  if a > b then []
  else a :: list_range (a+1) b

let list_product la lb =
  List.fold_left
    (fun res a ->
      List.fold_left
        (fun res b ->
          (a,b)::res)
        res lb)
    [] la

let list_best (better_than : 'a -> 'a -> bool) (l : 'a list) : 'a option =
  let rec aux best = function
    | [] -> best
    | x::l ->
       let best = if better_than x best then x else best in
       aux best l
  in
  match l with
  | [] -> None
  | x::l -> Some (aux x l)
  
let list_mins (cmp : 'a -> 'a -> int) (l : 'a list) : 'a list =
  let rec aux min acc = function
    | [] -> acc
    | x::l ->
       let c = cmp x min in
       if c < 0 then aux x [x] l
       else if c = 0 then aux min (x::acc) l
       else aux min acc l
  in
  match l with
  | [] -> []
  | x::l -> aux x [x] l

let list_rotate (l : 'a list) (shift : int) : 'a list =
  let n = List.length l in
  let rec aux2 shift rev_xs ys =
    if shift = 0
    then ys @ List.rev rev_xs
    else
      match ys with
      | [] -> aux2 shift [] (List.rev rev_xs)
      | y::ys1 -> aux2 (shift-1) (y::rev_xs) ys1 in
  let rec aux shift l =
    if shift = 0 then l
    else if shift > 0 then aux2 shift [] l
    else (* shift < 0 *) aux (shift+n) l
  in
  if n <= 1 then l
  else aux shift l

let list_unique_vals (l : 'a list) : 'a list =
  let rec aux seen = function
    | [] -> []
    | x::r ->
       if Bintree.mem x seen
       then aux seen r
       else x :: aux (Bintree.add x seen) r
  in
  aux Bintree.empty l

let list_unique_assoc (l : ('a * 'b) list) : 'b list =
  let rec aux seen = function
    | [] -> []
    | (x,y)::r ->
       if Bintree.mem x seen
       then aux seen r
       else y :: aux (Bintree.add x seen) r
  in
  aux Bintree.empty l

let list_unique_ranks (l : 'a list) : 'a list * int list = (* l1=unique items, l2=rank list *)
  (* l = map (fun rank -> l1[rank]) l2 *)
  let rec aux seen rank = function
    | [] -> [], []
    | x::r ->
       if Mymap.mem x seen
       then
         let r1, r2 = aux seen rank r in
         r1, Mymap.find x seen :: r2
       else
         let r1, r2 = aux (Mymap.add x rank seen) (rank+1) r in
         x :: r1, rank :: r2
  in
  aux Mymap.empty 0 l

let rec list_map_unpairs (f : 'a -> 'c) (g : 'b -> 'c) (l : ('a * 'b) list) : 'c list =
  match l with
  | [] -> []
  | (x,y)::r -> f x :: g y :: list_map_unpairs f g r

let rec list_map_pairs (f : 'a -> 'b) (g : 'a -> 'c) (l : 'a list) : ('b * 'c) list =
  match l with
  | [] -> []
  | [x] -> invalid_arg "Arc_common.list_assoc_unflatten: the input list must have an even length"
  | x::y::r -> (f x, g y) :: list_map_pairs f g r

(* mymap *)

let mymap_keys (m : ('a,'b) Mymap.t) : 'a list =
  m |> Mymap.bindings |> List.map fst

let mymap_of_list (l : ('a * 'b) list) : ('a,'b) Mymap.t =
  List.fold_left
    (fun res (k,v) -> Mymap.add k v res)
    Mymap.empty l

(* myseq *)

let myseq_bind_list_interleave (k : int) (l : 'a list) (f : 'a * 'a list -> 'b Myseq.t) : 'b Myseq.t =
  let rec aux k rev_l r =
    fun () ->
    match r with
    | [] -> Myseq.Nil
    | [x] -> f (x, List.rev rev_l) ()
    | x :: r1 ->
       (match f (x, List.rev_append rev_l r1) () with
       | Myseq.Nil ->
          if k > 1
          then aux (k-1) (x::rev_l) r1 () (* here, k-1 differs from Myseq.bind_interleave_at_most *)
          else Myseq.Nil
       | Myseq.Cons (y,next) as node ->
          if k > 1
          then Cons (y, Myseq.interleave [aux (k-1) (x::rev_l) r1; next])
          else node)
  in
  if k > 0
  then aux k [] l
  else Myseq.empty

(* memoization *)

module Memo = (* appears to be more efficient than Common versions *)
  struct
let log_on = ref false

let memoize (type k)
      ?(name : string option)    
      ?(equal : k -> k -> bool = (=))
      ?(hash : k -> int = Hashtbl.hash)
      ~(size : int)
      (f : k -> 'a) : (k -> 'a) * (unit -> unit) =
  let module H =
    Hashtbl.Make
      (struct
        type t = k
        let equal = equal
        let hash = hash
      end) in
  let ht : 'a H.t = H.create size in
  let log_memsize () =
    match name with
    | None -> ()
    | Some name -> Printf.printf "MEMSIZE %s\t%dk\n" name (Common.memsize ht / 1000) in
  let reset () =
    if !log_on then log_memsize ();
    H.clear ht in
  let memoized_f =
    fun x ->
    match H.find_opt ht x with
    | Some y -> y
    | None ->
       let y = f x in
       H.add ht x y;
       if !log_on then log_memsize ();
       y
  in
  memoized_f, reset

let memoize2 ?name ?equal ~size f2 =
  let mem_f2, reset = memoize ?name ?equal ~size (fun (x1,x2) -> f2 x1 x2) in
  (fun x1 x2 -> mem_f2 (x1,x2)), reset
let memoize3 ?name ?equal ~size f3 =
  let mem_f3, reset = memoize ?name ?equal ~size (fun (x1,x2,x3) -> f3 x1 x2 x3) in
  (fun x1 x2 x3 -> mem_f3 (x1,x2,x3)), reset
let memoize4 ?name ?equal ~size f4 =
  let mem_f4, reset = memoize ?name ?equal ~size (fun (x1,x2,x3,x4) -> f4 x1 x2 x3 x4) in
  (fun x1 x2 x3 x4 -> mem_f4 (x1,x2,x3,x4)), reset

  end
