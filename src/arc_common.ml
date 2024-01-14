
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

(* mymap *)

let mymap_keys (m : ('a,'b) Mymap.t) : 'a array =
  m |> Mymap.bindings |> List.map fst |> Array.of_list
          
(* memoization *)

module Memo = (* appears to be more efficient than Common versions *)
  struct
let memoize (type k)
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
  let reset () = H.clear ht in
  let memoized_f =
    fun x ->
    match H.find_opt ht x with
    | Some y -> y
    | None ->
       let y = f x in
       H.add ht x y;
       y
  in
  memoized_f, reset

let memoize2 ?equal ~size f2 =
  let mem_f2, reset = memoize ?equal ~size (fun (x1,x2) -> f2 x1 x2) in
  (fun x1 x2 -> mem_f2 (x1,x2)), reset
let memoize3 ?equal ~size f3 =
  let mem_f3, reset = memoize ?equal ~size (fun (x1,x2,x3) -> f3 x1 x2 x3) in
  (fun x1 x2 x3 -> mem_f3 (x1,x2,x3)), reset
let memoize4 ?equal ~size f4 =
  let mem_f4, reset = memoize ?equal ~size (fun (x1,x2,x3,x4) -> f4 x1 x2 x3 x4) in
  (fun x1 x2 x3 x4 -> mem_f4 (x1,x2,x3,x4)), reset

  end
