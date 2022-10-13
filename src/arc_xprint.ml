(* extension of module Xprint
   TODO: make Xprint more generic to avoid this *)

class virtual t = (* printer extending Xprint.pr_stdout with ANSITerminal styles (e.g. colors) *)
  object
    inherit Xprint.t
    method virtual style_string : ANSITerminal.style list -> string -> unit
  end

type 'a xp = t -> 'a -> unit (* type of polymorphic printings *)

let pr_stdout =
  object
    inherit t
    method string s = print_string s
    method style_string style s = ANSITerminal.print_string style s
    method int i = print_int i
    method float f = print_float f
    method float_1 f = Printf.printf "%.1f" f
    method float_2 f = Printf.printf "%.2f" f
    method float_3 f = Printf.printf "%.3f" f
  end

let to_stdout (xp : 'a xp) : 'a -> unit =
  fun x ->
  xp pr_stdout x

class pr_buffer =
  object (self)
    inherit t
    inherit Xprint.pr_buffer
    method style_string _ s = self#string s (* ignoring style *)
  end
  
let to_string (xp : 'a xp) : 'a -> string =
  fun x ->
  let pr = new pr_buffer in
  xp (pr :> t) x;
  pr#contents
    
(* combinators for polymorphic printings *)
  
let sep_list (sep : string) (xp : 'a xp) : 'a list xp =
  fun pr l ->
  match l with
  | [] -> ()
  | [x] -> xp pr x
  | x::ys ->
     xp pr x;
     ys |> List.iter (fun y -> pr#string sep; xp pr y)

let bracket (left, right : string * string) (xp : 'a xp) : 'a xp =
  fun pr x ->
  pr#string left;
  xp pr x;
  pr#string right

let infix (op : string) (xp : 'a xp) : ('a * 'a) xp =
  fun pr (x1,x2) ->
  xp pr x1;
  pr#string op;
  xp pr x2
