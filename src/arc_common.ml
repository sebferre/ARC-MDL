
(* pretty printing *)

module Xprint = Arc_xprint

(* error handling *)
              
exception TODO (* for something to be defined later *)
exception Undefined_result of string (* for undefined computations *)

type 'a result = ('a,exn) Result.t

let ( let| ) res f = Result.bind res f [@@inline]
let ( let|? ) res f = Option.bind res f [@@inline]

(* configuration, options *)
                   
let def_param (name : string) (v : 'a) (to_str : 'a -> string) : 'a ref =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v

(* syntactic sugar and utilities for sequences *)

let ( %* ) = Myseq.empty
let ( !* ) = Myseq.return
let ( ** ) = Myseq.cons
let ( @* ) = fun seq1 seq2 -> Myseq.concat [seq1; seq2] [@@inline]

let ( let* ) seq f = seq |> Myseq.flat_map f [@@inline]
let ( let*? ) seq f = seq |> Myseq.filter_map f [@@inline]
let ( let*! ) seq f = seq |> Myseq.map f [@@inline]    

(* syntactic sugar and utilities for lists *)
                    
let ( let$ ) (init,l) f =
  l |> List.fold_left (fun res x -> f (res, x)) init [@@inline]
let ( let& ) l f =
  l |> List.iter f [@@inline]

let rec list_map_result (f : 'a -> ('b,'c) Result.t) (lx : 'a list) : ('b list, 'c) Result.t =
  match lx with
  | [] -> Result.Ok []
  | x::lx1 ->
     let| y = f x in
     let| ly1 = list_map_result f lx1 in
     Result.Ok (y::ly1)

let rec list_mapi_result (f : int -> 'a -> ('b,'c) Result.t) (i : int) (lx : 'a list) : ('b list, 'c) Result.t =
  match lx with
  | [] -> Result.Ok []
  | x::lx1 ->
     let| y = f i x in
     let| ly1 = list_mapi_result f (i+1) lx1 in
     Result.Ok (y::ly1)

let result_list_bind_some (lx_res : ('a list,'c) Result.t) (f : 'a -> ('b list,'c) Result.t) : ('b list, 'c) Result.t =
  let rec aux = function
  | [] -> Result.Error (Failure "Model2.result_list_bind_some: empty list")
  | [x] -> f x
  | x::lx1 ->
     let open Result in
     match f x, aux lx1 with
     | Ok ly0, Ok ly1 -> Ok (List.append ly0 ly1)
     | Ok ly0, Error _ -> Ok ly0
     | Error _, Ok ly1 -> Ok ly1
     | Error e1, Error _ -> Error e1
  in
  let| lx = lx_res in
  aux lx
let ( let+|+ ) = result_list_bind_some

let rec option_list_bind (lx : 'a list) (f : 'a -> 'b option) : 'b list option =
  match lx with
  | [] -> Some []
  | x::lx1 ->
     match f x with
     | None -> None
     | Some y ->
        match option_list_bind lx1 f with
        | None -> None
        | Some ly1 -> Some (y::ly1)

(* dummy versions for the JS code *)

module JS = (* for profiling visually, used for the JS version *)
  struct
    let prof name f =
      print_endline (name ^ "...");
      let res = f () in
      print_endline ("DONE " ^ name);
      res
  end
