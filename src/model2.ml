
open Task

let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v
   
let alpha = def_param "alpha" 10. string_of_float
let max_nb_parse = def_param "max_nb_parse" 64 (* TEST 256 *) string_of_int (* max nb of considered grid parses *)
let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
let max_relaxation_level_parse_layers = def_param "max_relaxation_level_parse_layers" 16 string_of_int (* see parse_layers *)
let max_nb_diff = def_param "max_nb_diff" 3 string_of_int (* max nb of allowed diffs in grid parse *)
let max_nb_grid_reads = def_param "max_nb_grid_reads" 3 string_of_int (* max nb of selected grid reads, passed to the next stage *)
let max_expressions = def_param "max_expressions" 10000 string_of_int (* max nb of considered expressions when generating defs-refinements *)
let max_refinements = def_param "max_refinements" 20 string_of_int (* max nb of considered refinements *)

exception TODO

module TEST = (* for profiling visually, used for the JS version *)
  struct
    let prof name f =
      print_endline (name ^ "...");
      let res = f () in
      print_endline ("DONE " ^ name);
      res
  end
        
(* binders and syntactic sugar *)

let ( %* ) = Myseq.empty
let ( !* ) = Myseq.return
let ( ** ) = Myseq.cons
let ( @* ) = fun seq1 seq2 -> Myseq.concat [seq1; seq2]

type 'a result = ('a,exn) Result.t

let ( let| ) res f = Result.bind res f [@@inline]
                   
let ( let* ) seq f = seq |> Myseq.flat_map f [@@inline]
let ( let*? ) seq f = seq |> Myseq.filter_map f [@@inline]
let ( let*! ) seq f = seq |> Myseq.map f [@@inline]

let ( let$ ) (init,l) f =
  l |> List.fold_left (fun res x -> f (res, x)) init [@@inline]
let ( let& ) l f =
  l |> List.iter (fun x -> f x) [@@inline]

let rec list_map_result (f : 'a -> ('b,'c) Result.t) (lx : 'a list) : ('b list, 'c) Result.t =
  match lx with
  | [] -> Result.Ok []
  | x::lx1 ->
     let| y = f x in
     let| ly1 = list_map_result f lx1 in
     Result.Ok (y::ly1)
let ( let+| ) lx f = list_map_result f lx [@@inline]
                   
let result_list_bind_some (lx_res : ('a list,'c) Result.t) (f : 'a -> ('b list,'c) Result.t) : ('b list, 'c) Result.t =
  let rec aux = function
  | [] -> invalid_arg "Model2.bind_map_ok: empty list"
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


(** Part 1: grids *)

(* common data structures *)

type 'a triple = 'a * 'a * 'a

type 'a ilist = (* insertable list *)
  [ `Nil
  | `Insert of 'a ilist * 'a * 'a ilist ]
type ilist_revpath =
  [ `Root
  | `Left of ilist_revpath
  | `Right of ilist_revpath ]

let rec ilist_length : 'a ilist -> int = function
  | `Nil -> 0
  | `Insert (left,elt,right) ->
     ilist_length left + 1 + ilist_length right

let rec map_ilist (f : ilist_revpath -> 'a -> 'b) (lp : ilist_revpath) (l : 'a ilist) : 'b ilist =
  match l with
  | `Nil -> `Nil
  | `Insert (left,elt,right) ->
     let left = map_ilist f (`Left lp) left in
     let elt = f lp elt in
     let right = map_ilist f (`Right lp) right in
     `Insert (left,elt,right)

let rec map_ilist_result (f : ilist_revpath -> 'a -> ('b,'c) Result.t) (lp : ilist_revpath) (l : 'a ilist) : ('b ilist,'c) Result.t =
  match l with
  | `Nil -> Result.Ok `Nil
  | `Insert (left,elt,right) ->
     let| left = map_ilist_result f (`Left lp) left in
     let| elt = f lp elt in
     let| right = map_ilist_result f (`Right lp) right in
     Result.Ok (`Insert (left,elt,right))

let rec fold_ilist (f : 'b -> ilist_revpath -> 'a -> 'b) (acc : 'b) (lp : ilist_revpath) (l : 'a ilist) : 'b =
  match l with
  | `Nil -> acc
  | `Insert (left,elt,right) ->
     let acc = fold_ilist f acc (`Left lp) left in
     let acc = f acc lp elt in
     let acc = fold_ilist f acc (`Right lp) right in
     acc
       
let rec fold2_ilist (f : 'c -> ilist_revpath -> 'a -> 'b -> 'c) (acc : 'c) (lp : ilist_revpath) (l1 : 'a ilist) (l2 : 'b ilist) : 'c =
  match l1, l2 with
  | `Nil, `Nil -> acc
  | `Insert (left1,elt1,right1), `Insert (left2,elt2,right2) ->
     let acc = fold2_ilist f acc (`Left lp) left1 left2 in
     let acc = f acc lp elt1 elt2 in
     let acc = fold2_ilist f acc (`Right lp) right1 right2 in
     acc
  | _ -> invalid_arg "Model2.fold2_ilist: inconsistent ilists"
            
let rec fill_ilist_with_list il l = (* QUICK *)
  (* replacing elements in il with elements in l, taken in order *)
  (* first element goes leftmost, last element rightmost *)
  match il with
  | `Nil -> l, `Nil
  | `Insert (left,_,right) ->
     let l, left' = fill_ilist_with_list left l in
     match l with
     | [] -> assert false
     | x::l ->
        let l, right' = fill_ilist_with_list right l in
        l, `Insert (left', x, right')

(* let rec fill_ilist_with_rev_list il l = (* QUICK *)
  (* replacing elements in il with elements in l, taken in reverse order *)
  (* first element goes rightmost, last element leftmost *)
  match il with
  | `Nil -> l, `Nil
  | `Insert (left,_,right) ->
     let l, right' = fill_ilist_with_rev_list right l in
     match l with
     | [] -> assert false
     | x::l ->
        let l, left' = fill_ilist_with_rev_list left l in
        l, `Insert (left', x, right') *)

(* type definitions for data, expressions, templates *)

(*
class virtual func ~name ~arity =
object
  method name : string = name
  method arity : int = arity
  method virtual to_string : string list -> string
  method virtual kind : kind
  method virtual arg_kind : parent_kind:(unit -> kind) -> int -> kind
  method virtual arg_role : parent_role:(unit -> role) -> int -> role
  method virtual apply : template list -> template
end
 *)
        
type kind =
  Int | Bool | Color | Mask | Vec | Shape | Object | Layer | Grid
let all_kinds : kind list =
  [ Int;  Bool;  Color;  Mask;  Vec;  Shape;  Object; Layer;  Grid ]

module KindMap =
  struct
    type 'a t = 'a array (* indices over kinds *)

    let make ~int ~bool ~color ~mask ~vec ~shape ~object_ ~layer ~grid : 'a t =
      [| int; bool; color; mask; vec; shape; object_; layer; grid |]

    let init (f : kind -> 'a) : 'a t =
      [| f Int; f Bool; f Color; f Mask; f Vec; f Shape; f Object; f Layer; f Grid |]

    let find (k : kind) (map : 'a t) : 'a =
      map.((Obj.magic k : int)) [@@inline]

    let map (f : kind -> 'a -> 'b) (map : 'a t) : 'b t =
      Array.mapi
        (fun i v -> f (Obj.magic i : kind) v)
        map

    let fold_left (f : 'b -> kind -> 'a -> 'b) (init : 'b) (map : 'a t) : 'b =
      let res = ref init in
      for i = 0 to Array.length map - 1 do
        res := f !res (Obj.magic i : kind) map.(i)
      done;
      !res
  end

let ( .&() ) map k = KindMap.find k map [@@inline]
  
type role = (* same information as kind + contextual information *)
  [ `Int of [`I | `J] * role_vec
  | `Color of role_frame
  | `Mask
  | `Vec of role_vec
  | `Shape
  | `Object
  | `Layer
  | `Grid ]
and role_vec =
  [ `Pos (* coordinates in grid, range [0,size-1] *)
  | `Size of role_frame (* strictly positive, unbounded *)
  | `Move ] (* translation (pos delta), can be negative *)
and role_frame =
  [ `Shape | `Grid ]

let kind_of_role : role -> kind = function
  | `Int _ -> Int
  | `Color _ -> Color
  | `Mask -> Mask
  | `Vec _ -> Vec
  | `Shape -> Shape
  | `Object -> Object
  | `Layer -> Layer
  | `Grid -> Grid

type role_poly = (* polymorphic extension of role *)
  [ `Int of [`I | `J | `X] * role_vec_poly
  | `Color of role_frame_poly
  | `Mask
  | `Vec of role_vec_poly
  | `Shape
  | `Object
  | `Layer
  | `Grid ]
and role_vec_poly =
  [ `Pos | `Size of role_frame_poly | `Move | `X ]
and role_frame_poly =
  [ `Shape | `Grid | `X ]

let role_poly_matches (role_x : role_poly) (role : role) : bool =
  let rec aux_role r_x r =
    match r_x, r with
    | `Int (_,vec_x), `Int (_,vec) -> aux_vec vec_x vec
    | `Color fr_x, `Color fr -> true
    | `Mask, `Mask -> true
    | `Vec vec_x, `Vec vec -> aux_vec vec_x vec
    | `Shape, `Shape -> true
    | `Object, `Object -> true
    | `Layer, `Layer -> true
    | `Grid, `Grid -> true
    | _ -> false
  and aux_vec vec_x vec =
    match vec_x, vec with
    | `X, _ -> true
    | `Pos, `Pos -> true
    | `Size fr_x, `Size fr -> true
    | `Move, `Move -> true
    | _ -> false
  in
  aux_role role_x role
           

type 'a patt =
  [ `Bool of bool
  | `Int of int
  | `Color of Grid.color
  | `Mask of Grid.Mask_model.t
  | `Vec of 'a * 'a (* i, j -> vec *)
  | `Point of 'a (* color -> shape *)
  | `Rectangle of 'a * 'a * 'a (* size, color, mask -> shape *)
  | `PosShape of 'a * 'a (* pos, shape -> object *)
  | `Background of 'a * 'a * 'a ilist (* size, color, layers (top first) -> grid *)
  ]

type field =
  [ `I
  | `J
  | `Pos
  | `Color
  | `Size
  | `Mask
  | `Shape
  | `Layer of ilist_revpath ]
type revpath =
  [ `Root
  | `Field of field * revpath
  | `Item of int * revpath
  | `Arg of int * role option * revpath ] (* if no role, parent role *)
let path0 = `Root

let rec (++) p f = `Field (f,p)
       
let rec path_parent : revpath -> revpath option = function
  | `Root -> None
  | `Field (f,p) -> Some p
  | `Item (i,p) -> Some p
  | `Arg (i,role_opt,p) -> Some p

(* reversing a revpath, to get paths starting from the root: used in path_factorize *)
(* must be its own inverse: reverse(reverse(p,`Root),`Root) = p *)
let rec path_reverse (p : revpath) (acc : revpath) : revpath =
  match p with
  | `Root -> acc
  | `Field (`Layer lp, p1) -> path_reverse p1 (`Field (`Layer (ilist_reverse lp `Root), acc))
  | `Field (f,p1) -> path_reverse p1 (`Field (f,acc))
  | `Item (i,p1) -> path_reverse p1 (`Item (i,acc))
  | `Arg (i,role_opt,p1) -> path_reverse p1 (`Arg (i,role_opt,acc))
and ilist_reverse (lp : ilist_revpath) acc =
  match lp with
  | `Root -> acc
  | `Left lp1 -> ilist_reverse lp1 (`Left acc)
  | `Right lp1 -> ilist_reverse lp1 (`Right acc)
       
let path_factorize (p1 : revpath) (p2 : revpath) : revpath * revpath * revpath = (* QUICK *)
  (* returns q0, q1, q2 s.t. p1 = q0/q1, p2 = q0/q2, and q0 is the longest so *)
  let rev_p1 = path_reverse p1 `Root in
  let rev_p2 = path_reverse p2 `Root in
  let rec aux_path p0 rp1 rp2 =
    match rp1, rp2 with
    | `Field (f1,rq1), `Field (f2,rq2) ->
       ( match f1, f2 with
         | `Layer rlp1, `Layer rlp2 ->
            let lp0, lp1, lp2 = aux_ilist `Root rlp1 rlp2 in
            if lp1 = `Root && lp2 = `Root (* rlp1 = rlp2 *)
            then aux_path (`Field (`Layer lp0, p0)) rq1 rq2
            else `Field (`Layer lp0, p0),
                 path_reverse rq1 (if lp1=`Root then `Root else `Field (`Layer lp1,`Root)),
                 path_reverse rq2 (if lp2=`Root then `Root else `Field (`Layer lp2,`Root))
         | _ ->
            if f1 = f2
            then aux_path (`Field (f1,p0)) rq1 rq2
            else p0, path_reverse rp1 `Root, path_reverse rp2 `Root )
    | `Item (i1,rq1), `Item (i2,rq2) ->
       if i1 = i2
       then aux_path (`Item (i1,p0)) rq1 rq2
       else p0, path_reverse rp1 `Root, path_reverse rp2 `Root
    | `Arg (i1,role1_opt,rq1), `Arg (i2,role2_opt,rq2) ->
       if i1 = i2 && role1_opt = role2_opt
       then aux_path (`Arg (i1,role1_opt,p0)) rq1 rq2
       else p0, path_reverse rp1 `Root, path_reverse rp2 `Root
    | _ -> p0, path_reverse rp1 `Root, path_reverse rp2 `Root
  and aux_ilist lp0 rlp1 rlp2 =
    match rlp1, rlp2 with
    | `Left rlq1, `Left rlq2 ->
       aux_ilist (`Left lp0) rlq1 rlq2
    | `Right rlq1, `Right rlq2 ->
       aux_ilist (`Right lp0) rlq1 rlq2
    | _ ->
       lp0, ilist_reverse rlp1 `Root, ilist_reverse rlp2 `Root
  in
  aux_path `Root rev_p1 rev_p2

let get_pos : 'a -> (int * int) option =
  function
  | `PosShape (`Vec (`Int i, `Int j), _) -> Some (i,j)
  | `Background _ -> Some (0, 0)
  | _ -> None
          
let rec get_size : 'a -> (int * int) option =
  function
  | `Point _ -> Some (1,1)
  | `Rectangle (`Vec (`Int h, `Int w), _, _) -> Some (h,w)
  | `PosShape (_, shape) -> get_size shape
  | `Background (`Vec (`Int h, `Int w), _, _) -> Some (h,w)
  | _ -> None


(* abstract type for concrete types including sequences of themselves *)
type 'a seq = ([> `Seq of 'a list] as 'a)
  
(* broadcasting functions to align sequences and atoms of templates when evaluating expressions *) 
let broadcast1 (t : 'a seq) (f : 'a -> 'b) : 'b seq =
  match t with
  | `Seq lt ->
     let lr = List.map f lt in
     `Seq lr
  | _ -> f t [@@inline]
let broadcast2 (t : 'a seq * 'b seq) (f : 'a * 'b -> 'c) : 'c seq =
  let rec aux lt1 lt2 =
    match lt1, lt2 with
    | [], _ | _, [] -> []
    | t1::l1, t2::l2 ->
       let r = f (t1,t2) in
       let rs = aux l1 l2 in
       r::rs
  in
  match t with
  | `Seq lt1, `Seq lt2 ->
     let lr = aux lt1 lt2 in
     `Seq lr
  | `Seq lt1, t2 ->
     let lr = List.map (fun t1 -> f (t1,t2)) lt1 in
     `Seq lr
  | t1, `Seq lt2 ->
     let lr = List.map (fun t2 -> f (t1,t2)) lt2 in
     `Seq lr
  | _ -> f t [@@inline]
let broadcast_list (ts : 'a seq list) (f : (* non-seq *) 'a list -> 'b) : 'b seq =
  let rec aux (ts : 'a seq list) =
    let heads_tails_opt =
      List.fold_right
        (fun t -> function
          | None -> None
          | Some (heads,tails) ->
             match t with
             | `Seq [] -> None
             | `Seq (head0::tail0) -> Some (head0::heads, (`Seq tail0)::tails)
             | _ -> Some (t::heads, t::tails))
        ts (Some ([],[])) in
    match heads_tails_opt with
    | None -> []
    | Some (heads,tails) ->
       let r = f heads in
       let rs = aux tails in
       r::rs
  in
  if List.exists (function `Seq _ -> true | _ -> false) ts
  then
    let lr = aux ts in
    `Seq lr
  else f ts

let broadcast1_result (t : 'a seq) (f : 'a -> 'b result) : 'b seq result =
  match t with
  | `Seq lt ->
     let| lr = list_map_result f lt in
     Result.Ok (`Seq lr)
  | _ -> f t [@@inline]
let broadcast2_result (t : 'a seq * 'b seq) (f : 'a * 'b -> 'c result) : 'c seq result =
  let rec aux lt1 lt2 =
    match lt1, lt2 with
    | [], _ | _, [] -> Result.Ok []
    | t1::l1, t2::l2 ->
       let| r = f (t1,t2) in
       let| rs = aux l1 l2 in
       Result.Ok (r::rs)
  in
  match t with
  | `Seq lt1, `Seq lt2 ->
     let| lr = aux lt1 lt2 in
     Result.Ok (`Seq lr)
  | `Seq lt1, t2 ->
     let| lr = list_map_result (fun t1 -> f (t1,t2)) lt1 in
     Result.Ok (`Seq lr)
  | t1, `Seq lt2 ->
     let| lr = list_map_result (fun t2 -> f (t1,t2)) lt2 in
     Result.Ok (`Seq lr)
  | _ -> f t [@@inline]
let broadcast_list_result (ts : 'a seq list) (f : (* non-seq *) 'a list -> 'b result) : 'b seq result =
  let rec aux (ts : 'a seq list) =
    let heads_tails_opt =
      List.fold_right
        (fun t -> function
          | None -> None
          | Some (heads,tails) ->
             match t with
             | `Seq [] -> None
             | `Seq (head0::tail0) -> Some (head0::heads, (`Seq tail0)::tails)
             | _ -> Some (t::heads, t::tails))
        ts (Some ([],[])) in
    match heads_tails_opt with
    | None -> Result.Ok []
    | Some (heads,tails) ->
       let| r = f heads in
       let| rs = aux tails in
       Result.Ok (r::rs)
  in
  if List.exists (function `Seq _ -> true | _ -> false) ts
  then
    let| lr = aux ts in
    Result.Ok (`Seq lr)
  else f ts

       
type data =
  [ data patt
  | `Seq of data list ]
let data0 : data = `Background (`Vec (`Int 0, `Int 0), `Color Grid.black, `Nil)
let _ = (data0 :> data seq) (* data is an instance of data seq *)

type var = revpath

type 'a expr =
  [ `Ref of revpath
  | `ConstInt of int (* Int *)
  | `ConstVec of int * int (* Vec *)
  | `Plus of 'a * 'a (* on Int, Vec *)
  | `Minus of 'a * 'a (* on Int, Vec *)
  | `IncrInt of 'a * int (* on Int *)
  | `DecrInt of 'a * int (* in Int *)
  | `IncrVec of 'a * int * int (* on Vec *)
  | `DecrVec of 'a * int * int (* in Vec *)
  | `Modulo of 'a * 'a (* on Int *)
  | `ScaleUp of 'a * int (* on Int, Vec, Mask *)
  | `ScaleDown of 'a * int (* on Int, Vec, Mask *)
  | `ScaleTo of 'a * 'a (* Mask, Vec -> Mask *)
  | `Corner of 'a * 'a (* on Vec *)
  | `Min of 'a list (* on Int, Vec *)
  | `Max of 'a list (* on Int, Vec *)
  | `Average of 'a list (* on Int, Vec *)
  | `Span of 'a * 'a (* on Vec *)
  | `Norm of 'a (* Vec -> Int *)
  | `Diag1 of 'a * int (* Vec -> Int *)
  | `Diag2 of 'a * int (* Vec -> Int *)
  | `LogAnd of 'a * 'a (* on Mask *)
  | `LogOr of 'a * 'a (* on Mask *)
  | `LogXOr of 'a * 'a (* on Mask *)
  | `LogAndNot of 'a * 'a (* on Mask *)
  | `LogNot of 'a (* on Mask *)
  | `Area of 'a (* on Shape *)
  | `Left of 'a (* on Object *)
  | `Right of 'a (* on Object *)
  | `Center of 'a (* on Object *)
  | `Top of 'a (* on Object *)
  | `Bottom of 'a (* on Object *)
  | `Middle of 'a (* on Object *)
  | `ProjI of 'a (* on Vec *)
  | `ProjJ of 'a (* on Vec *)
  | `TranslationOnto of 'a * 'a (* Obj, Obj -> Vec *)
  | `Tiling of 'a * int * int (* on Vec/Mask/Shape *)
  | `ResizeAlikeTo of 'a * 'a (* on Mask/Shape/Object as T, Vec -> T *)
  | `ApplySym of symmetry * 'a * role (* on Vec, Mask, Shape, Object; role of the argument as computation depends on it *)
  | `UnfoldSym of symmetry list list * 'a (* on Mask, Shape, Object *)
     (* sym list list = matrix to be filled with symmetries of some mask *)
  | `TranslationSym of symmetry * 'a * 'a (* Obj, Obj/Grid -> Vec *)
  | `Coloring of 'a * 'a (* Shape/Obj, Color -> Shape/Obj *)
  ]
and symmetry = [
  | `Id
  | `FlipHeight | `FlipWidth | `FlipDiag1 | `FlipDiag2
  | `Rotate180 | `Rotate90 | `Rotate270 ]

let sym_matrix_flipHeightWidth = [[`Id; `FlipWidth]; [`FlipHeight; `Rotate180]]
             
type template = (* a template seq *)
  [ `U
  | template patt
  | `Seq of template list
  | template expr ] (* TODO: should sub-expressions be restricted to patt and expr ? *)
let template0 : template = `U
let _ = (template0 :> template seq) (* template is an instance of template seq *)

let u_vec : template = `Vec (`U, `U)

type signature = (kind * revpath list) list (* map from kinds to path lists *)
let signature0 = []
  
type diff = revpath list (* paths to data parts differing from a template *)
let diff0 = []
          
type delta = Grid.pixel list (* pixels not explained by a template *)
let delta0 = []

type grid_data =
  { data: data;
    diff: diff;
    delta: delta }
let grid_data0 =
  { data = data0;
    diff = diff0;
    delta = delta0 }


(* stringifiers and pretty-printing *)

let rec xp_ilist_path (print : Xprint.t) : ilist_revpath -> unit =
  function
  | `Root -> print#string "0"
  | `Left p -> xp_ilist_path print p; print#string "0"
  | `Right p -> xp_ilist_path print p; print#string "1"

let string_of_kind : kind -> string = function
  | Bool -> "bool"
  | Int -> "int"
  | Color -> "color"
  | Mask -> "mask"
  | Vec -> "vector"
  | Shape -> "shape"
  | Object -> "object"
  | Layer -> "layer"
  | Grid -> "grid"

let rec xp_role (print : Xprint.t) = function
  | `Int (`I, rv) -> print#string "i/"; xp_role_vec print rv
  | `Int (`J, rv) -> print#string "j/"; xp_role_vec print rv
  | `Color rf -> print_string "color/"; xp_role_frame print rf
  | `Mask -> print#string "mask"
  | `Vec rv -> print#string "vec/"; xp_role_vec print rv
  | `Shape -> print#string "shape"
  | `Object -> print#string "object"
  | `Layer -> print#string "layer"
  | `Grid -> print#string "grid"
and xp_role_vec print = function
  | `Pos -> print#string "pos"
  | `Size rf -> print#string "size/"; xp_role_frame print rf
  | `Move -> print#string "move"
and xp_role_frame print = function
  | `Shape -> print#string "shape"
  | `Grid -> print#string "grid"
           
let xp_field (print : Xprint.t) : field -> unit = function
  | `I -> print#string "i"
  | `J -> print#string "j"
  | `Pos -> print#string "pos"
  | `Color -> print#string "color"
  | `Size -> print#string "size"
  | `Mask -> print#string "mask"
  | `Shape -> print#string "shape"
  | `Layer lp -> print#string "layer_"; xp_ilist_path print lp
let pp_field = Xprint.to_stdout xp_field

let rec xp_path (print : Xprint.t) : revpath -> unit = function
  | `Root -> print#string "^"
  | `Field (f,p) -> xp_path print p; print#string "."; xp_field print f
  | `Item (i,p) -> xp_path print p; print#string "["; print#int i; print#string "]"
  | `Arg (i,role_opt,p) -> xp_path print p; print#string "."; print#int i
let pp_path = Xprint.to_stdout xp_path
let string_of_path = Xprint.to_string xp_path

let xp_path_list (print : Xprint.t) lp =
  print#string "(";
  List.iter (fun path -> xp_path print path; print#string " in ") lp;
  print#string ")"
let pp_path_list = Xprint.to_stdout xp_path_list

let rec xp_ilist (xp : Xprint.t -> 'a -> unit) (print : Xprint.t) (l : 'a ilist) : unit =
  let rec aux lp = function
    | `Nil -> ()
    | `Insert (left,elt,right) ->
       aux (`Left lp) left;
       print#string "\n  _"; xp_ilist_path print lp; print#string ": "; xp print elt;
       aux (`Right lp) right
  in
  aux `Root l

let xp_mask_model (print : Xprint.t) : Grid.Mask_model.t -> unit = function
  | `Full false -> print#string "Full"
  | `Full true -> print#string "Full (and Border)" 
  | `Border -> print#string "Border"
  | `EvenCheckboard -> print#string "Even Checkboard"
  | `OddCheckboard -> print#string "Odd Checkboard"
  | `PlusCross -> print#string "+-cross"
  | `TimesCross -> print#string "x-cross"
  | `Mask m -> print#string (Grid.Mask.to_string m)
                 
let rec xp_patt (xp : Xprint.t -> 'a -> unit) (print : Xprint.t) : 'a patt -> unit = function
  | `Bool b -> print#string (if b then "true" else "false")
  | `Int i -> print#int i
  | `Color c -> print#string (Grid.name_of_color c)
  | `Mask m -> xp_mask_model print m
  | `Vec (i,j) ->
     print#string "("; xp print i; print#string ","; xp print j; print#string ")"
  | `Point (color) ->
     print#string "a point";
     print#string " with color "; xp print color
  | `Rectangle (size,color,mask) ->
     print#string "a rectangle";
     print#string " with size "; xp print size;
     print#string " and color "; xp print color;
     print#string " and mask "; xp print mask
  | `PosShape (pos,shape) ->
     xp print shape; print#string " at "; xp print pos
  | `Background (size,color,layers) ->
     print#string "a background with size "; xp print size;
     print#string " and color "; xp print color;
     print#string " and layers"; xp_ilist xp print layers
let pp_patt xp patt = Xprint.to_stdout (xp_patt xp) patt
let pp_patt_dummy patt = pp_patt (fun print _ -> print#string "_") patt

let rec xp_data (print : Xprint.t) : data -> unit = function
  | #patt as patt -> xp_patt xp_data print patt
  | `Seq items ->
     Xprint.bracket
       ("<\n\t", " >")
       (Xprint.sep_list ",\n\t" xp_data)
       print
       items
let pp_data = Xprint.to_stdout xp_data
let string_of_data = Xprint.to_string xp_data

let string_of_index = "$index"
              
let xp_var (print : Xprint.t) : var -> unit =
  fun p -> xp_path print p
let string_of_var = Xprint.to_string xp_var

let xp_apply (func : string) (xp : Xprint.t -> 'a -> unit) (print : Xprint.t) (args : 'a list) : unit =
  print#string func;
  print#string "(";
  Xprint.sep_list ", " xp print args;
  print#string ")"

let xp_apply_poly (func : string) print (xp_args : (Xprint.t -> unit) list) : unit =
  print#string func;
  print#string "(";
  (match xp_args with
   | [] -> ()
   | xp::xps ->
      xp print;
      List.iter
        (fun xp1 -> print#string ", "; xp1 print)
        xps); 
  print#string ")"

let rec xp_expr (xp : Xprint.t -> 'a -> unit) (print : Xprint.t) : 'a expr -> unit = function
  | `Ref p -> xp_path print p
  | `ConstInt k -> print#string "'"; print#int k
  | `ConstVec (k,l) -> xp_apply_poly "'" print
                         [(fun print -> print#int k);
                          (fun print -> print#int l)]
  | `Plus (a,b) -> Xprint.infix " + " xp print (a, b)
  | `Minus (a,b) -> Xprint.infix " - " xp print (a, b)
  | `IncrInt (a,k) -> xp print a; print#string " + "; print#int k
  | `DecrInt (a,k) -> xp print a; print#string " - "; print#int k
  | `IncrVec (a,k,l) -> xp print a; print#string " + ("; print#int k; print#string ", "; print#int l; print#string ")"
  | `DecrVec (a,k,l) -> xp print a; print#string " - ("; print#int k; print#string ", "; print#int l; print#string ")"
  | `Modulo (a,b) -> Xprint.infix " % " xp print (a, b)
  | `ScaleUp (a,k) -> xp print a; print#string " * "; print#int k
  | `ScaleDown (a,k) -> xp print a; print#string " / "; print#int k
  | `ScaleTo (a,b) -> xp_apply "scaleTo" xp print [a;b]
  | `Corner (a,b) -> xp_apply "corner" xp print [a;b]
  | `Min la -> xp_apply "min" xp print la
  | `Max la -> xp_apply "max" xp print la
  | `Average la -> xp_apply "average" xp print la
  | `Span (a,b) -> xp_apply "span" xp print [a;b]
  | `Norm a -> Xprint.bracket ("|","|") xp print a
  | `Diag1 (a,k) -> xp_apply_poly "diag1" print
                      [(fun print -> xp print a);
                       (fun print -> print#int k)]
  | `Diag2 (a,k) -> xp_apply_poly "diag2" print
                      [(fun print -> xp print a);
                       (fun print -> print#int k)]
  | `LogAnd (a,b) -> Xprint.infix " and " xp print (a, b)
  | `LogOr (a,b) -> Xprint.infix " or " xp print (a, b)
  | `LogXOr (a,b) -> Xprint.infix " xor " xp print (a, b)
  | `LogAndNot (a,b) -> Xprint.infix " and not " xp print (a, b)
  | `LogNot (a) -> print#string "not "; xp print a
  | `Area a -> xp_apply "area" xp print [a]
  | `Left a -> xp_apply "left" xp print [a]
  | `Right a -> xp_apply "right" xp print [a]
  | `Center a -> xp_apply "center" xp print [a]
  | `Top a -> xp_apply "top" xp print [a]
  | `Bottom a -> xp_apply "bottom" xp print [a]
  | `Middle a -> xp_apply "middle" xp print [a]
  | `ProjI a -> xp_apply "projI" xp print [a]
  | `ProjJ a -> xp_apply "projJ" xp print [a]
  | `TranslationOnto (a,b) -> xp_apply "translationOnto" xp print [a;b]
  | `Tiling (a,k,l) -> xp_apply_poly "tiling" print
                         [(fun print -> xp print a);
                          (fun print -> print#int k);
                          (fun print -> print#int l)]
  | `ResizeAlikeTo (a,b) -> xp_apply "resizeAlikeTo" xp print [a;b]
  | `ApplySym (sym,a,_) -> xp_apply_poly "applySym" print
                           [(fun print -> xp_symmetry print sym);
                            (fun print -> xp print a)]
  | `UnfoldSym (sym_array,a) ->
     xp_apply_poly "unfoldSym" print
       [(fun print ->
           List.iter
             (fun sym_row ->
               print#string " [ ";
               List.iter
                 (fun sym -> xp_symmetry print sym; print#string " ")
                 sym_row;
               print#string "]")
             sym_array);
        (fun print -> xp print a)]
  | `TranslationSym (sym,a,b) -> xp_apply_poly "translationSym" print
                                   [(fun print -> xp_symmetry print sym);
                                    (fun print -> xp print a);
                                    (fun print -> xp print b)]
  | `Coloring (a,b) -> xp_apply "coloring" xp print [a;b] 
and xp_symmetry print : symmetry -> unit = function
  | `Id -> print#string "id"
  | `FlipHeight -> print#string "flipHeight"
  | `FlipWidth -> print#string "flipWidth"
  | `FlipDiag1 -> print#string "flipDiag1"
  | `FlipDiag2 -> print#string "flipDiag2"
  | `Rotate180 -> print#string "rotate180"
  | `Rotate90 -> print#string "rotate90"
  | `Rotate270 -> print#string "rotate270"
                  
let string_of_expr xp = Xprint.to_string (xp_expr xp)
                         
                   
let rec xp_template (print : Xprint.t) : template -> unit = function
  | `U -> print#string "?"
  | #patt as patt -> xp_patt xp_template print patt
  | `Seq items ->
     Xprint.bracket
       ("<\n\t", " >")
       (Xprint.sep_list ",\n\t" xp_template)
       print
       items
  | #expr as e -> xp_expr xp_template print e
let pp_template = Xprint.to_stdout xp_template
let string_of_template = Xprint.to_string xp_template

let xp_signature (print : Xprint.t) (sg : signature) : unit =
  Xprint.sep_list "\n"
    (fun print (k,ps) ->
      print#string (string_of_kind k); print#string ": ";
      Xprint.sep_list ", " xp_path print ps)
    print
    sg
(*  String.concat "\n"
    (List.map
       (fun (k,ps) ->
         string_of_kind k ^ ": "
         ^ String.concat ", "
             (List.map string_of_path ps))
       sg) *)
                  
let xp_diff (print : Xprint.t) diff =
  diff
  |> List.iter (fun p1 -> print#string "  "; xp_path print p1)
let pp_diff = Xprint.to_stdout xp_diff
                  
let pp_delta delta =
  delta
  |> List.sort Stdlib.compare
  |> List.iter (fun (i,j,c) -> Printf.printf " (%d,%d)=" i j; Grid.pp_color c)
    
let pp_grid_data gd =
  print_string "data: "; pp_data gd.data; print_newline ();
  print_string "diff: "; pp_diff gd.diff; print_newline ();
  print_string "delta:"; pp_delta gd.delta; print_newline ()


(* data utilities *)

let rec path_kind (p : revpath) : kind =
  match p with
  | `Root -> Grid
  | `Field (f,p1) ->
     (match f with
      | (`I | `J) -> Int
      | `Color -> Color
      | `Mask -> Mask
      | (`Pos | `Size) -> Vec
      | `Shape -> Shape
      | `Layer _ -> Layer)
  | `Item (_,p1) -> path_kind p1
  | `Arg (i,None,p1) -> path_kind p1
  | `Arg (i, Some role, p1) -> kind_of_role role

let rec path_role (p : revpath) : role =
  match p with
  | `Root -> `Grid
  | `Field ((`I | `J as f), p1) -> `Int (f, path_role_vec p1)
  | `Field (`Color, p1) -> `Color (path_role_frame p1)
  | `Field (`Mask, _) -> `Mask
  | `Field (`Pos, _) -> `Vec `Pos
  | `Field (`Size, p1) -> `Vec (`Size (path_role_frame p1))
  | `Field (`Shape, _) -> `Shape
  | `Field (`Layer _, _) -> `Layer
  | `Item (_, p1) -> path_role p1
  | `Arg (i, None, p1) -> path_role p1
  | `Arg (i, Some role, p1) -> role
and path_role_vec : revpath -> role_vec = function
  | `Root -> assert false
  | `Field (`Pos, _) -> `Pos
  | `Field (`Size, p1) -> `Size (path_role_frame p1)
  | `Item (i, p1) -> path_role_vec p1
  | `Arg (i, None, p1) -> path_role_vec p1
  | p ->
     pp_path p; print_newline ();
     assert false
and path_role_frame : revpath -> role_frame = function
  | `Root -> `Grid
  | `Field (`Shape, _) -> `Shape
  | `Item (i, p1) -> path_role_frame p1
  | `Arg (i, None, p1) -> path_role_frame p1
  | p -> pp_path p; print_newline (); assert false

         
let find_ilist (lp : ilist_revpath) (l : 'a ilist) : 'a option = (* QUICK *)
  let rec aux lp =
    match lp with
    | `Root -> l
    | `Left lp1 ->
       ( match aux lp1 with
         | `Nil -> `Nil
         | `Insert (left,_,_) -> left )
    | `Right lp1 ->
       ( match aux lp1 with
         | `Nil -> `Nil
         | `Insert (_,_,right) -> right )
  in
  match aux lp with
  | `Nil -> None
  | `Insert (_,elt,_) -> Some elt
  
let find_field_patt (f : field) (patt_parent : 'a patt) : 'a option =
  match f, patt_parent with
  | `I, `Vec (i,j) -> Some i
  | `J, `Vec (i,j) -> Some j
  | `Color, `Point (color) -> Some color
  | `Size, `Rectangle (size,color,mask) -> Some size
  | `Color, `Rectangle (size,color,mask) -> Some color
  | `Mask, `Rectangle (size,color,mask) -> Some mask
  | `Pos, `PosShape (pos,shape) -> Some pos
  | `Shape, `PosShape (pos,shape) -> Some shape
  | `Size, `Background (size,color,layers) -> Some size
  | `Color, `Background (size,color,layers) -> Some color
  | `Layer lp, `Background (size,color,layers) -> find_ilist lp layers
  | _ ->
     pp_field f; print_string ": ";
     pp_patt_dummy patt_parent;
     print_newline ();
     assert false
       
let rec find_field_seq (f : field) (items : 'a seq list) : 'a option =
  option_list_bind items
    (function
     | #patt as patt -> find_field_patt f patt
     | `Seq items1 -> find_field_seq f items1
     | _ -> None)
  |> Option.map (fun lx -> `Seq lx)

let rec find_data (p : revpath) (d : data) : data option = (* QUICK *)
  match p with
  | `Root -> Some d
  | `Field (f, p1) ->
     (match find_data p1 d with
      | None -> None
      | Some (#patt as patt1) -> find_field_patt f patt1
      | Some (`Seq items) -> find_field_seq f items) 
  | `Item (i, p1) ->
     (match find_data p1 d with
      | None -> None
      | Some (#patt as patt1) -> Some (patt1 :> data)
      | Some (`Seq items) -> List.nth_opt items i)
  | `Arg _ -> assert false
  
let rec find_template (p : revpath) (t : template) : template option =
  Common.prof "Model2.find_template" (fun () ->
  match p with
  | `Root -> Some t
  | `Field (f,p1) ->
     (match find_template p1 t with
      | None -> None
      | Some (#patt as patt1) -> (find_field_patt f patt1 :> template option)
      | Some (`Seq items) -> find_field_seq f items
      | Some _ -> assert false)
  | `Item (i, p1) ->
     (match find_template p1 t with
      | None -> None
      | Some (#patt as patt1) -> Some (patt1 :> template)
      | Some (`Seq items) -> List.nth_opt items i
      | Some _ -> assert false) (* `U and expr not explorable *)
  | `Arg _ -> assert false)


let fold_patt (fold : 'b -> revpath -> 'a -> 'a list (* ancestry *) -> 'b) (acc : 'b) (p : revpath) (patt : 'a patt) (patt_ancestry : 'a list) : 'b =
  match patt with
  | `Bool _ | `Int _ | `Color _ | `Mask _ -> acc
  | `Vec (i,j) ->
     let acc = fold acc (p ++ `I) i patt_ancestry in
     let acc = fold acc (p ++ `J) j patt_ancestry in
     acc
  | `Point (color) ->
     let acc = fold acc (p ++ `Color) color patt_ancestry in
     acc
  | `Rectangle (size,color,mask) ->
     let acc = fold acc (p ++ `Size) size patt_ancestry in
     let acc = fold acc (p ++ `Color) color patt_ancestry in
     let acc = fold acc (p ++ `Mask) mask patt_ancestry in
     acc
  | `PosShape (pos,shape) ->
     let acc = fold acc (p ++ `Pos) pos patt_ancestry in
     let acc = fold acc (p ++ `Shape) shape patt_ancestry in
     acc
  | `Background (size,color,layers) ->
     let acc = fold acc (p ++ `Size) size patt_ancestry in
     let acc = fold acc (p ++ `Color) color patt_ancestry in
     let acc =
       fold_ilist
         (fun acc lp shape ->
           fold acc (p ++ `Layer lp) shape patt_ancestry)
         acc `Root layers in
     acc

let rec fold_data (f : 'b -> revpath -> data -> data list (* ancestry *) -> 'b) (acc : 'b) (p : revpath) (d : data) (ancestry : data list) : 'b =
  let acc = f acc p d ancestry in
  let d_ancestry = d::ancestry in
  match d with
  | #patt as d -> fold_patt (fold_data f) acc p d d_ancestry
  | `Seq items ->
     let _, acc =
       items
       |> List.fold_left
            (fun (i,acc) item ->
              i+1, fold_data f acc (`Item (i,p)) item d_ancestry)
            (0,acc) in
     acc
  
let rec fold_template (f : 'b -> revpath -> template -> template list (* ancestry *) -> 'b) (acc : 'b) (p : revpath) (t : template) (ancestry : template list) : 'b =
  let acc = f acc p t ancestry in
  let t_ancestry = t::ancestry in
  match t with
  | `U -> acc
  | #patt as patt ->
     fold_patt
       (fun acc p t anc -> fold_template f acc p t anc)
       acc p patt t_ancestry
  | `Seq items ->
     let _, acc =
       items
       |> List.fold_left
            (fun (i,acc) item ->
              i+1, fold_template f acc (`Item (i,p)) item t_ancestry)
            (0,acc) in
     acc
  | #expr -> acc
       
let size_of_data (d : data) : int =
  Common.prof "Model2.size_of_data" (fun () ->
  fold_data (fun res _ _ _ -> res+1) 0 path0 d [])
let size_of_template (t : template) : int =
  fold_template (fun res _ _ _ -> res+1) 0 path0 t []

let signature_of_template (t : template) : signature =
  Common.prof "Model2.signature_of_template" (fun () ->
  let ht = Hashtbl.create 13 in
  let () =
    fold_template
      (fun () p t1 anc1 ->
        let k = path_kind p in
        let ps0 =
          match Hashtbl.find_opt ht k with
          | None -> []
          | Some ps -> ps in
        let ps = p::ps0 in
        Hashtbl.replace ht k ps;
        if k = Vec && t1 = `U then (
          let ps0 =
            match Hashtbl.find_opt ht Int with
            | None -> []
            | Some ps -> ps in
          Hashtbl.replace ht Int (p ++ `I :: p ++ `J :: ps0)
        ))
      () path0 t [] in
  Hashtbl.fold
    (fun k ps res -> (k, List.rev ps)::res) (* reverse to put them in order *)
    ht [])

let signature_of_kind (sg : signature) (k : kind) : revpath list =
  match List.assoc_opt k sg with
  | Some ps -> ps
  | None -> []

let default_pos = `Vec (`Int 0, `Int 0)
let default_shape_color = `Color Grid.no_color
let default_grid_color = `Color Grid.black
let default_shape_size = `Vec (`Int 2, `Int 2)
let default_grid_size = `Vec (`Int 10, `Int 10)
let default_move = `Vec (`Int 0, `Int 0)
let default_mask = `Mask (`Full false)
let default_shape = `Rectangle (default_shape_size, default_shape_color, default_mask)
let default_object = `PosShape (default_pos, default_shape)
let default_layer = default_object
let default_grid = `Background (default_grid_size, default_grid_color, `Nil)
let default_data_of_path (p : revpath) : data =
  match path_role p with
  | `Int (_, `Pos) -> `Int 0
  | `Int (_, `Size `Grid) -> `Int 10
  | `Int (_, `Size `Shape) -> `Int 2
  | `Int (_, `Move) -> `Int 0
  | `Color `Grid -> default_grid_color
  | `Color `Shape -> default_shape_color
  | `Mask -> default_mask
  | `Vec `Pos -> default_pos
  | `Vec (`Size `Grid) -> default_grid_size
  | `Vec (`Size `Shape) -> default_shape_size
  | `Vec `Move -> default_move
  | `Shape -> default_shape
  | `Object -> default_object
  | `Layer -> default_layer
  | `Grid -> default_grid

let rec root_template_of_data ~(in_output : bool) (d : data) : template list = (* QUICK *)
  match d with
  | `Bool _ -> []
  | `Int i as d ->
     if in_output && i >= 1 && i <= 3
     then [(d :> template)]
     else [] (* position- and size-invariance of inputs *)
  | `Color _ as d -> [(d :> template)] (* colors can be seen as patterns *)
  | `Mask (`Full true) -> [`Mask `Border; `Mask (`Full false)]
  | `Mask _ as d -> [(d :> template)] (* masks can be seen as patterns *)
  | `Vec _ -> [`Vec (`U, `U)]
  | `Point _ -> [`Point (`U)]
  | `Rectangle _ -> [`Rectangle (u_vec, `U, `U)]
  | `PosShape _ -> [`PosShape (u_vec, `U)]
  | `Background _ -> assert false (* should not happen *)
  | `Seq [] -> []
  | `Seq (item::items as l) ->
     let common_patterns =
       List.fold_left
         (fun res item ->
           let item_patterns = root_template_of_data ~in_output item in
           List.filter
             (fun t -> List.mem t item_patterns)
             res)
         (root_template_of_data ~in_output item) items in
     `Seq (List.map
             (function
              | `Vec _ -> u_vec
              | _ -> `U)
             l)
     :: common_patterns

let matches_ilist (matches : 'a -> 'b -> bool)
      (il1 : 'a ilist) (il2 : 'b ilist) : bool =
  let rev_l1 = fold_ilist (fun res _ t -> t::res) [] `Root il1 in
  let rev_l2 = fold_ilist (fun res _ d -> d::res) [] `Root il2 in
  List.for_all2 matches rev_l1 rev_l2

let rec matches_template (t : template) (d : data) : bool = (* QUICK *)
  match t, d with
  (* explicit broadcasting *)
  | `Seq lt, `Seq ld ->
     let rec aux lt ld =
       match lt, ld with
       | [], [] -> true
       | [], _ | _, [] -> false
       | t::lt1, d::ld1 -> matches_template t d && aux lt1 ld1
     in
     aux lt ld
  | `Seq lt, _ -> List.for_all (fun t -> matches_template t d) lt
  | _, `Seq ld -> List.for_all (fun d -> matches_template t d) ld
  (* on patterns *)
  | `U, _ -> true
  | `Bool b1, `Bool b2 when b1 = b2 -> true
  | `Int i1, `Int i2 when i1 = i2 -> true
  | `Color c1, `Color c2 when c1 = c2 -> true
  | `Mask m1, `Mask m2 -> Grid.Mask_model.subsumes m1 m2
  | `Vec (i1,j1), `Vec (i2,j2) ->
     matches_template i1 i2 && matches_template j1 j2
  | `Point (color1), `Point (color2) ->
     matches_template color1 color2
  | `Rectangle (size1,color1,mask1), `Rectangle (size2,color2,mask2) ->
     matches_template size1 size2 && matches_template color1 color2 && matches_template mask1 mask2
  | `PosShape (pos1,shape1), `PosShape (pos2,shape2) ->
     matches_template pos1 pos2 && matches_template shape1 shape2
  | `Background (size1,color1,layers1), `Background (size2,color2,layers2) ->
     matches_template size1 size2 && matches_template color1 color2
     && matches_ilist matches_template layers1 layers2
  | _ -> false
    
(* description lengths *)

type dl = Mdl.bits
let dl0 = 0.

let dl_round dl = Float.round (dl *. 1e9) /. 1e9

let dl_compare (dl1 : float) (dl2 : float) =
  if dl1 < dl2 then -1
  else if dl1 = dl2 then 0
  else 1 [@@inline]
                
let dl_zero = Mdl.Code.universal_int_star 0
                
let dl_bool : bool -> dl =
  fun b -> 1.
let dl_nat : int -> dl =
  fun i -> Mdl.Code.universal_int_star i
let dl_int_pos ~bound : int -> dl = (* all positions are alike *)
  fun i -> Mdl.Code.uniform bound
let dl_int_size ~bound : int -> dl = (* longer lengths cover more pixels *)
  fun i -> Mdl.Code.universal_int_plus i (* TODO: would uniform be better ? would avoid bias to smaller shapes *)
let dl_int_index : int -> dl =
  fun i -> Mdl.Code.universal_int_star i
let dl_color : Grid.color -> dl =
  fun c -> Mdl.Code.uniform Grid.nb_color
let dl_background_color : Grid.color -> dl =
  function
  | 0 -> Mdl.Code.usage 0.91
  | c ->
     if c > 0 && c < 10 (* 9 colors *)
     then Mdl.Code.usage 0.01
     else invalid_arg "Unexpected color"
let dl_mask : Grid.Mask_model.t -> dl =
  function
  | `Full _ -> Mdl.Code.usage 0.5 (* TODO: give equal prob to all specific masks ? *)
  | `Border -> Mdl.Code.usage 0.1
  | `EvenCheckboard
    | `OddCheckboard
    | `PlusCross
    | `TimesCross -> Mdl.Code.usage 0.025
  | `Mask m ->
     let n = Grid.Mask.height m * Grid.Mask.width m in
     let k = Grid.Mask.area m in
     Mdl.Code.usage 0.3 +. Mdl.Code.partition [k; n-k] (* prequential coding *)
     (* Mdl.Code.usage 0.3 +. float n (* basic bitmap *) *)
     (* let missing = n - Grid.Mask.area m in
     Mdl.Code.usage 0.5
     +. Mdl.Code.universal_int_plus missing
     +. Mdl.Code.comb missing n (* TODO: penalize more sparse masks ? also consider min area 50% in grid.ml *) *)

     
type dl_ctx =
  { box_height : int;
    box_width : int }
let dl_ctx0 =
  { box_height = Grid.max_size;
    box_width = Grid.max_size }
  
let dl_ctx_of_data (d : data) : dl_ctx = (* QUICK *)
  (* retrieving grid size: make assumption on paths *)
  let box_height =
    match find_data (`Field (`I, `Field (`Size, `Root))) d with
    | Some (`Int i) -> i
    | _ -> Grid.max_size in
  let box_width =
    match find_data (`Field (`J, `Field (`Size, `Root))) d with
    | Some (`Int j) -> j
    | _ -> Grid.max_size in
  { box_height; box_width }


let dl_patt_as_template = Mdl.Code.usage 0.2

let dl_patt
      (dl : ctx:dl_ctx -> path:revpath -> 'a -> dl)
      ~(ctx : dl_ctx) ~(path : revpath) (patt : 'a patt) : dl =
  match patt with
  | `Int n ->
     ( match path_role path with
       | `Int (`I, `Pos) -> dl_int_pos ~bound:ctx.box_height n
       | `Int (`J, `Pos) -> dl_int_pos ~bound:ctx.box_width n
       | `Int (`I, `Size _) -> dl_int_size ~bound:ctx.box_height n
       | `Int (`J, `Size _) -> dl_int_size ~bound:ctx.box_width n
       | `Int (_, `Move) -> assert false (* only computation intermediate value *)
       | _ -> assert false )

  | `Color c ->
     ( match path_role path with
       | `Color `Grid -> dl_background_color c
       | `Color `Shape -> dl_color c
       | _ -> assert false )

  | `Mask m -> dl_mask m

  | `Vec (i,j) ->
     dl ~ctx i ~path:(path ++ `I)
     +. dl ~ctx j ~path:(path ++ `J)  

  | `Point (color) ->
     Mdl.Code.usage 0.5
     +. dl ~ctx color ~path:(path ++ `Color)
  | `Rectangle (size,color,mask) ->
     Mdl.Code.usage 0.5
     +. dl ~ctx size ~path:(path ++ `Size)
     +. dl ~ctx color ~path:(path ++ `Color)
     +. dl ~ctx mask ~path:(path ++ `Mask)

  | `PosShape (pos,shape) ->
     ( match path_role path with `Layer -> Mdl.Code.universal_int_plus 1 | _ -> 0.) (* singleton layer *)
     +. dl ~ctx pos ~path:(path ++ `Pos)
     +. dl ~ctx shape ~path:(path ++ `Shape)

  | `Background (size,color,layers) ->
     let box_height =
       match size with
       | `Vec (`Int i, _) -> i
       | _ -> ctx.box_height in
     let box_width =
       match size with
       | `Vec (_, `Int j) -> j
       | _ -> ctx.box_width in
     let nb_layers = ilist_length layers in
     let ctx_layers = { box_height; box_width } in
     dl ~ctx size ~path:(path ++ `Size)
     +. dl ~ctx color ~path:(path ++ `Color)
     +. fold_ilist
          (fun sum lp shape ->
            sum +. dl ~ctx:ctx_layers shape ~path:(path ++ `Layer lp))
          (Mdl.Code.universal_int_star nb_layers)
          `Root layers
     
  | _ ->
     pp_path path; print_string ": ";
     pp_patt_dummy patt;
     print_newline ();
     assert false                        

let dl_data, reset_dl_data =
  let rec aux ~ctx ~path (d : data) =
    match d with
    | #patt as d ->
       dl_patt_as_template (* NOTE: to align with dl_template on patterns *)
       +. dl_patt aux ~ctx ~path d
    | `Seq items ->
       Mdl.Code.list_star (fun item -> aux ~ctx ~path item) items
  in
  (*  aux *)
  let mem = Hashtbl.create 1003 in
  let reset () = Hashtbl.clear mem in
  let f =
    fun ~(ctx : dl_ctx) ?(path = `Root) (d : data) -> (* QUICK *)
    let role = path_role path in
    let key = (ctx,role,d) in (* dl_patt in dl_data does not depend on exact path, only on role *)
    match Hashtbl.find_opt mem key with
    | Some dl -> dl
    | None ->
       let dl = aux ~ctx ~path d in
       Hashtbl.add mem key dl;
       dl in
  f, reset


(* functions for computing the DL of paths, used as references
   - the coding takes into account the context of use (kind and role)
   - the coding takes into account the similarity between the reference path and the context path
   - the coding is stable, it does not change when the environment signature is extended (unlike the previous solution)
 *)
  
let rec dl_path_length : revpath -> int = function
  | `Root -> 0
  | `Field (`Layer lp, p1) -> 1 + dl_ilist_path_length lp + dl_path_length p1
  | `Field (_, p1) -> 1 + dl_path_length p1
  | `Item (_, p1) -> 1 + dl_path_length p1
  | `Arg (_,_,p1) -> dl_path_length p1 (* expr args ignored *)
and dl_ilist_path_length = function
  | `Root -> 0
  | `Left p1 -> 1 + dl_ilist_path_length p1
  | `Right p1 -> 1 + dl_ilist_path_length p1
          
let rec dl_path_role (role : role) (p : revpath) : dl =
  (* assuming [p] does not contain [`Arg] *)
  (* assuming the length of [p] is known => no need to code for `Root *)
  (* TODO: handle `Item field for each role *)
  match role with
  | `Int (rij,rvec) -> dl_path_int rij rvec  p
  | `Color _ -> dl_path_color p
  | `Mask -> dl_path_mask p
  | `Vec rvec -> dl_path_vec rvec p
  | `Shape -> dl_path_shape p
  | `Object -> dl_path_layer p
  | `Layer -> dl_path_layer p
  | `Grid -> dl_path_grid p
and dl_path_int (rij : [`I|`J]) (rvec : role_vec) = function
  | `Root -> 0.
  | `Field ((`I | `J as ij), p1) ->
     dl_zero
     +. Mdl.Code.usage (if ij = rij then 0.75 else 0.25)
     +. dl_path_vec rvec p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_int rij rvec p1
  | _ -> assert false
and dl_path_color = function
  | `Root -> 0.
  | `Field (`Color, p1) ->
     dl_zero
     +. dl_path_shape p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_color p1
  | _ -> assert false
and dl_path_mask = function
  | `Root -> 0.
  | `Field (`Mask, p1) ->
     dl_zero
     +. dl_path_shape p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_mask p1
  | _ -> assert false
and dl_path_vec (rvec : role_vec) = function
  | `Root -> 0.
  | `Field ((`Pos|`Size as f), p1) ->
     let dl_choice =
       match rvec, f with
       | `Pos, `Pos 
         | `Size _, `Size -> Mdl.Code.usage 0.9
       | _ -> Mdl.Code.usage 0.1 in
     dl_zero
     +. dl_choice
     +. ( match f with
          | `Pos -> dl_path_layer p1
          | `Size -> dl_path_shape p1 )
  | `Item (i, p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_vec rvec p1
  | _ -> assert false
and dl_path_shape = function
  | `Root -> 0.
  | `Field (`Shape, p1) ->
     dl_zero
     +. dl_path_layer p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_shape p1
  | _ -> assert false
and dl_path_layer = function
  | `Root -> 0.
  | `Field (`Layer lp, p1) ->
     dl_zero
     +. Mdl.Code.universal_int_star (dl_ilist_path_length lp)
     +. dl_ilist_path lp
     +. dl_path_grid p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_layer p1
  | _ -> assert false
and dl_path_grid = function
  | `Root -> 0.
  | _ -> assert false
and dl_ilist_path lp = (* assuming lp length known *)
  let rec aux = function
    | `Root -> 0, 0
    | `Left lp1 -> let nl, nr = aux lp1 in nl+1, nr
    | `Right lp1 -> let nl, nr = aux lp1 in nl, nr+1
  in
  let nleft, nright = aux lp in
  Mdl.Code.partition [nleft; nright]
          
let rec dl_path ~(env_sig : signature) ~(ctx_path : revpath) (p : revpath) : dl = (* QUICK *)
  (* [env_sig] not used *)
  let ctx_role = path_role ctx_path in
  let dl =
    let common_prefix, ctx_branch, p_branch =
      path_factorize ctx_path p in
    Mdl.Code.universal_int_star (dl_path_length ctx_branch)
    +. Mdl.Code.universal_int_star (dl_path_length p_branch)
    +. dl_path_role ctx_role p_branch in
  (*Printf.printf "dl_path(%s, %s) = %s / %s / %s = %f\n"
    (string_of_path ctx_path) (string_of_path p)
    (string_of_path common_prefix) (string_of_path ctx_branch) (string_of_path p_branch)
    dl;*)
  dl
          

let code_expr_by_kind : Mdl.bits KindMap.t = (* code of expressions, excluding Ref *)
  (* according to a uniform distribution *)
  let uniform_among (l : [`X] expr list) =
    if l = [] then 0. else Mdl.Code.uniform (List.length l) in
  KindMap.make
    ~int:(uniform_among [
              `ConstInt 0;
              `Area `X;
              (*`Left `X; *) `Right `X; `Center `X;
              (*`Top `X; *) `Bottom `X; `Middle `X;
              `Plus (`X,`X); `Minus (`X,`X); (*`Modulo (`X,`X);*)
              `IncrInt (`X,1); `DecrInt (`X,1);
              `ScaleUp (`X,2); `ScaleDown (`X,2);
              `Min [`X;`X]; `Max [`X;`X]; `Average [`X;`X]; `Span (`X,`X);
              (* `Norm `X; `Diag1 (`X,2); `Diag2 (`X,2);*) ])
    ~bool:(uniform_among [])
    ~color:(uniform_among [])
    ~mask:(uniform_among [
               `ScaleUp (`X,2); `ScaleTo (`X,`X);
               `Tiling (`X,1,1); `ResizeAlikeTo (`X,`X);
               `ApplySym (`FlipHeight, `X, `Mask);
               `UnfoldSym (sym_matrix_flipHeightWidth, `X);
               `LogAnd (`X,`X); `LogOr (`X,`X); `LogXOr (`X,`X);
               `LogAndNot (`X,`X); `LogNot `X ])
    ~vec:(uniform_among [
              `ProjI `X; `ProjJ `X;
              `ConstVec (0,0);
              `Plus (`X,`X); `Minus (`X,`X);
              `IncrVec (`X,1,1); `DecrVec (`X,1,1);
              `ScaleUp (`X,2); `ScaleDown (`X,2);
              `Tiling (`X,1,1);
              `Corner (`X,`X); `Min [`X; `X]; `Max [`X;`X];`Average [`X;`X]; `Span (`X,`X);
              `TranslationOnto (`X,`X);
              `TranslationSym (`FlipHeight,`X,`X) ])
    ~shape:(uniform_among [
                `ScaleUp (`X,2); `ScaleTo (`X,`X);
                `Tiling (`X,1,1); `ResizeAlikeTo (`X,`X);
                `ApplySym (`FlipHeight, `X, `Shape);
                `UnfoldSym (sym_matrix_flipHeightWidth, `X);
                `Coloring (`X,`X) ])
    ~object_:(uniform_among [ ])
    ~layer:(uniform_among [
                `ResizeAlikeTo (`X,`X);
                `ApplySym (`FlipHeight, `X, `Layer);
                `UnfoldSym (sym_matrix_flipHeightWidth, `X);
                `Coloring (`X,`X) ])
    ~grid:(uniform_among [])
  
let rec dl_expr
          (dl : ctx:dl_ctx -> path:revpath -> 'a -> dl)
          ~(env_sig : signature) ~(ctx : dl_ctx) ~(path : revpath) (e : 'a expr) : dl =
  (* for overloaded functions, rather infer type bottom-up *)
  let k = path_kind path in (* TODO: would be better to use more fine-grained role *)
  let code_expr = code_expr_by_kind.&(k) in
  match e with
  | `Ref p -> assert false
  | `ConstInt k ->
     code_expr
     +. Mdl.Code.universal_int_star k
  | `ConstVec (k,l) ->
     code_expr
     +. Mdl.Code.universal_int_star k
     +. Mdl.Code.universal_int_star l
  | `Plus (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,None,path)) e2 (* TODO: better constraint wrt Pos vs Size *)
  | `Minus (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,None,path)) e2
  | `IncrInt (e1,k) | `DecrInt (e1,k) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. Mdl.Code.universal_int_plus k
  | `IncrVec (e1,k,l) | `DecrVec (e1,k,l) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. Mdl.Code.universal_int_star k
     +. Mdl.Code.universal_int_star l
  | `Modulo (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,None,path)) e2
  | `ScaleUp (e1,k) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. Mdl.Code.universal_int_plus k
  | `ScaleDown (e1,k) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. Mdl.Code.universal_int_plus k
  | `ScaleTo (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,Some (`Vec (`Size `Shape)),path)) e2
  | `Corner (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,None,path)) e2
  | `Min le1 ->
     code_expr
     +. Mdl.Code.universal_int_plus (List.length le1)
     +. Mdl.sum le1
          (fun e1 -> dl ~ctx ~path:(`Arg (1,None,path)) e1)
  | `Max le1 ->
     code_expr
     +. Mdl.Code.universal_int_plus (List.length le1)
     +. Mdl.sum le1
          (fun e1 -> dl ~ctx ~path:(`Arg (1,None,path)) e1)
  | `Average le1 ->
     code_expr
     +. Mdl.Code.universal_int_plus (List.length le1)
     +. Mdl.sum le1
          (fun e1 -> dl ~ctx ~path:(`Arg (1,None,path)) e1)
  | `Span (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,None,path)) e2
  | `Norm e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some (`Vec `Pos), path)) e1
  | `Diag1 (e1,k) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some (`Vec `Pos), path)) e1
     +. Mdl.Code.universal_int_plus k
  | `Diag2 (e1,k) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some (`Vec `Pos), path)) e1
     +. Mdl.Code.universal_int_plus k
  | `LogAnd (e1,e2) | `LogOr (e1,e2) | `LogXOr (e1,e2) | `LogAndNot (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,None,path)) e2
  | `LogNot e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
  | `Area e1 ->
     code_expr +. dl ~ctx ~path:(`Arg (1, Some `Shape, path)) e1
  | `Left e1 | `Right e1 | `Center e1
    | `Top e1 | `Bottom e1 | `Middle e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some `Layer, path)) e1
  | `ProjI e1 | `ProjJ e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, None, path)) e1
  | `TranslationOnto (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some `Layer, path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some `Layer, path)) e2
  | `Tiling (e1,k,l) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, None, path)) e1
     +. Mdl.Code.universal_int_plus k
     +. Mdl.Code.universal_int_plus l
  | `ResizeAlikeTo (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, None, path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some (`Vec (`Size `Grid)), path)) e2
  | `ApplySym (sym,e1,role_e1) ->
     code_expr (* no need to encode role_e1, deducible from model *)
     +. Mdl.Code.uniform 8 (* encoding sym *)
     +. dl ~ctx ~path:(`Arg (2, Some role_e1, path)) e1
  | `UnfoldSym (sym_array,e1) ->
     code_expr (* includes encoding of sym list list *)
     +. dl ~ctx ~path:(`Arg (2, None, path)) e1
  | `TranslationSym (sym,e1,e2) ->
     code_expr (* includes encoding of sym *)
     +. Mdl.Code.uniform 8 (* encoding sym *)
     +. dl ~ctx ~path:(`Arg (2, Some `Layer, path)) e1
     +. dl ~ctx ~path:(`Arg (3, Some `Layer, path)) e2 (* TODO: can be a Grid too *)
  | `Coloring (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, None, path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some (`Color `Shape), path)) e2

type code_template = (* dls must correspond to a valid prob distrib *)
  { c_u : dl;
    c_patt : dl;
    c_ref : dl;
    c_expr : dl;
    c_seq : dl }
let code_template0 =
  { c_u = Mdl.Code.usage 0.1;
    c_patt = dl_patt_as_template (* Mdl.Code.usage 0.2 *);
    c_ref = Mdl.Code.usage 0.4;
    c_expr = Mdl.Code.usage 0.2;
    c_seq = Mdl.Code.usage 0.1 }

let code_template_by_kind : code_template KindMap.t =
  KindMap.make
    ~int:code_template0
    ~bool:code_template0
    ~color:code_template0
    ~mask:code_template0
    ~vec:code_template0
    ~shape:code_template0
    ~object_:code_template0
    ~layer:code_template0
    ~grid:code_template0
    
let dl_template ~(env_sig : signature) ~(ctx : dl_ctx) ?(path = `Root) (t : template) : dl =
  Common.prof "Model2.dl_template" (fun () ->
  let rec aux ~ctx ~path t =
    let k = path_kind path in
    let code = KindMap.find k code_template_by_kind in
    match t with
    | `U ->
       code.c_u
    | #patt as patt ->
       code.c_patt
       +. dl_patt aux ~ctx ~path patt
    | `Ref p ->
       code.c_ref
       +. dl_path ~env_sig ~ctx_path:path p
    | #expr as e ->
       code.c_expr
       +. dl_expr aux ~env_sig ~ctx ~path e
    | `Seq items ->
       code.c_seq
       +. Mdl.Code.list_plus (fun item -> aux ~ctx ~path item) items
  in
  aux ~ctx ~path t)

type dl_seq = [`DL of dl | `Seq of dl_seq list]

let rec dl_dl_seq : dl_seq -> dl = function  (* TODO: should we encode choice? *)
  | `DL dl -> dl
  | `Seq l -> Mdl.Code.list_star dl_dl_seq l
    
let rec dl_data_given_patt
          (dl : ctx:dl_ctx -> path:revpath -> 'a -> data -> dl_seq)
      ~ctx ~(path : revpath) (patt : 'a patt) (d : data) : dl_seq =
  match patt, d with
  | `Int _, `Int _ -> `DL 0.
  | `Color _, `Color _ -> `DL 0.
  | `Mask _, `Mask _ -> `DL 0.
  | `Vec (i,j), `Vec (di,dj) ->
     broadcast2
       (dl ~ctx i di ~path:(path ++ `I),
        dl ~ctx j dj ~path:(path ++ `J))
       (function
        | (`DL dli, `DL dlj) -> `DL (dli +. dlj)
        | _ -> assert false) (* TODO: revise type 'a seq to avoid this? *)
  | `Point (color), `Point (dcolor) ->
     broadcast1
       (dl ~ctx color dcolor ~path:(path ++ `Color))
       (fun dl1 -> dl1)
  | `Rectangle (size,color,mask), `Rectangle (dsize,dcolor,dmask) ->
     broadcast_list
       [dl ~ctx size dsize ~path:(path ++ `Size);
        dl ~ctx color dcolor ~path:(path ++ `Color);
        dl ~ctx mask dmask ~path:(path ++ `Mask)]
       (function
        | [`DL dl1; `DL dl2; `DL dl3] -> `DL (dl1 +. dl2 +. dl3)
        | _ -> assert false)
  | `PosShape (pos,shape), `PosShape (dpos,dshape) ->
     broadcast2
       (dl ~ctx pos dpos ~path:(path ++ `Pos),
        dl ~ctx shape dshape ~path:(path ++ `Shape))
       (function
        | (`DL dl1, `DL dl2) -> `DL (dl1 +. dl2)
        | _ -> assert false)
  | `Background (size,color,layers), `Background (dsize,dcolor,dlayers) ->
     broadcast_list
       [dl ~ctx size dsize ~path:(path ++ `Size);
        dl ~ctx color dcolor ~path:(path ++ `Color);
        `DL (fold2_ilist
               (fun sum lp shape dshape ->
                 sum +. dl_dl_seq (dl ~ctx shape dshape ~path:(path ++ `Layer lp)))
               0. `Root layers dlayers)]
       (function
        | [`DL dl1; `DL dl2; `DL dl3] -> `DL (dl1 +. dl2 +. dl3)
        | _ -> assert false)
  | _ -> assert false (* data inconsistent with pattern *)
    
let rec dl_data_given_template_aux ~(ctx : dl_ctx) ~(path : revpath) (t : template) (d : data) : dl_seq = (* cannot be profiled because of indirect recursion *)
  broadcast2 (t,d)
    (function
     | `U, d -> `DL (dl_data ~ctx ~path d)
     | #patt as patt, d -> dl_data_given_patt dl_data_given_template_aux ~ctx ~path patt d
     | #expr, _ -> `DL 0. (* will be evaluated out *)
     | _ -> assert false)
  
let dl_data_given_template ~(ctx : dl_ctx) ?(path : revpath = `Root) (t : template) (d : data) : dl =
  dl_dl_seq (dl_data_given_template_aux ~ctx ~path t d)
              
let dl_diff ~(ctx : dl_ctx) (diff : diff) (data : data) : dl = (* QUICK *)
  if diff = []
  then 0.
  else
    let dl_data_size = Mdl.Code.uniform (size_of_data data) in
    -. 1. (* some normalization to get 0 for empty grid data *)
    +. Mdl.Code.list_star
         (fun p1 ->
           let d1 =
             match find_data p1 data with
             | Some d1 -> d1
             | None -> assert false in
           dl_data_size
           +. dl_data ~ctx d1 ~path:p1)
         diff

let dl_delta_path = `Item (0, `Field (`Layer `Root, `Root)) (* dummy path with kind Shape *)
(* let dl_delta_shape = `PosShape (`Vec (`Int 0, `Int 0), `Point (`Color Grid.blue)) (* dummy point shape *) *)
let dl_delta_shape = `PosShape (`Vec (`Int 0, `Int 0), `Rectangle (`Vec (`Int 1, `Int 1), `Color Grid.blue, `Mask (`Full false))) (* dummy point shape as rectangle *)
let dl_delta ~(ctx : dl_ctx) (delta : delta) : dl = (* QUICK *)
  if delta = []
  then 0.
  else (* assuming dl_data is constant over point shapes *)
    let n = List.length delta in
    -. 1. (* some normalization to get 0 for empty grid data *)
    +. Mdl.Code.universal_int_star n
    +. float n *. dl_data ~ctx ~path:dl_delta_path dl_delta_shape
  
(* OLD
  -. 1. (* some normalization to get 0 for empty grid data *)
  +. Mdl.Code.list_star
       (fun (i,j,c) -> (* TODO: optimize: hint, all points have the same DL ? *)
         dl_data ~ctx ~path:(any_item (`Field (`Layer `Root, `Root))) (* dummy path with kind Shape *)
           (`Point (`Vec (`Int i, `Int j), `Color c)))
       delta)
   *)
(* NOT using optimized DL below for fair comparisons with model points: 
  +. Mdl.Code.comb nb_pixels area (* where they are *)
  +. float nb_pixels *. Mdl.Code.uniform (Grid.nb_color - 1) (* what are their color, different from the color generated by the model *) *)

  
(* evaluation of expression and templates on environment data *)

exception Unbound_var of var
exception Invalid_expr of template expr (* this expression is ill-formed or ill-typed *)
exception Undefined_result of string (* to ignore parses where some expression is undefined *)
(* special cases of undefined result *)
exception Negative_integer
let _ =
  Printexc.register_printer
    (function
     | Unbound_var v -> Some ("unbound variable: " ^ string_of_var v)
     | Invalid_expr e -> Some ("invalid expression: " ^ string_of_expr xp_template e)
     | Undefined_result msg -> Some ("undefined expression: " ^ msg)
     | Negative_integer -> Some ("negative integer")
     | _ -> None)

type apply_lookup = var -> data result

let lookup_of_env (env : data) : apply_lookup =
  fun p ->
  match find_data p env with
  | Some d -> Result.Ok d
  | None -> Result.Error (Unbound_var (p :> var))

let apply_patt
      (apply : lookup:apply_lookup -> revpath -> 'a -> 'b result)
      ~(lookup : apply_lookup) (p : revpath) : 'a patt -> 'b patt result = function
  (* SHOULD NOT use [p] *)
  | (`Bool _ | `Int _ | `Color _ | `Mask _ as d) -> Result.Ok d
  | `Vec (i,j) ->
     let| ri = apply ~lookup (p ++ `I) i in
     let| rj = apply ~lookup (p ++ `J) j in
     Result.Ok (`Vec (ri,rj))
  | `Point (color) ->
     let| rcolor = apply ~lookup (p ++ `Color) color in
     Result.Ok (`Point rcolor)
  | `Rectangle (size,color,mask) ->
     let| rsize = apply ~lookup (p ++ `Size) size in
     let| rcolor = apply ~lookup (p ++ `Color) color in
     let| rmask = apply ~lookup (p ++ `Mask) mask in
     Result.Ok (`Rectangle (rsize,rcolor,rmask))
  | `PosShape (pos,shape) ->
     let| rpos = apply ~lookup (p ++ `Pos) pos in
     let| rshape = apply ~lookup (p ++ `Shape) shape in
     Result.Ok (`PosShape (rpos,rshape))
  | `Background (size,color,layers) ->
     let| rsize = apply ~lookup (p ++ `Size) size in
     let| rcolor = apply ~lookup (p ++ `Color) color in
     let| rlayers =
       map_ilist_result
         (fun lp shape -> apply ~lookup (p ++ `Layer lp) shape)
         `Root layers in
     Result.Ok (`Background (rsize,rcolor,rlayers))

let apply_symmetry ~lookup (sym : symmetry) (role_e1 : role) e d1 = (* : template expr -> template -> template = *)
  (* let flip_size, sym_mask_model = flip_size__f_sym sym in *)
  let sym_pos d = (* symmetry of a point relative to the grid *)
    let p_grid_size = `Field (`Size, `Root) in
    match lookup p_grid_size, d with (* getting the grid size *)
    | Result.Ok (`Vec (`Int h, `Int w)), `Vec (`Int i, `Int j) ->
       let i', j' =
         match sym with
         | `Id -> i, j
         | `FlipHeight -> h-1-i, j
         | `FlipWidth -> i, w-1-j
         | `FlipDiag1 -> j, i
         | `FlipDiag2 -> w-1-j, h-1-i
         | `Rotate180 -> h-1-i, w-1-j
         | `Rotate90 -> j, h-1-i
         | `Rotate270 -> w-1-j, i in
       `Vec (`Int i', `Int j')
    (* | None, _ -> raise (Unbound_var p_grid_size) *)
    | _ -> assert false in
  let sym_size = function
    | `Vec (`Int h, `Int w) ->
       let h', w' =
         match sym with
         | `Id | `FlipHeight | `FlipWidth | `Rotate180 -> h, w
         | `FlipDiag1 | `FlipDiag2 | `Rotate90 | `Rotate270 -> w, h in
       `Vec (`Int h', `Int w')
    | _ -> assert false in
  let sym_move = function (* symmetry relative to position (0,0) *)
    | `Vec (`Int i, `Int j) ->
       let i', j' =
         match sym with
         | `Id -> i, j
         | `FlipHeight -> -i, j
         | `FlipWidth -> i, -j
         | `FlipDiag1 -> j, i
         | `FlipDiag2 -> -j, -i
         | `Rotate180 -> -i, -j
         | `Rotate90 -> j, -i
         | `Rotate270 -> -j, i in
       `Vec (`Int i', `Int j')
    | _ -> assert false in
  let sym_mask = function
    | `Mask mm ->
       let| mm' =
         match sym with
         | `Id -> Result.Ok mm
         | `FlipHeight -> Grid.Mask_model.flipHeight mm
         | `FlipWidth -> Grid.Mask_model.flipWidth mm
         | `FlipDiag1 -> Grid.Mask_model.flipDiag1 mm
         | `FlipDiag2 -> Grid.Mask_model.flipDiag2 mm
         | `Rotate180 -> Grid.Mask_model.rotate180 mm
         | `Rotate90 -> Grid.Mask_model.rotate90 mm
         | `Rotate270 -> Grid.Mask_model.rotate270 mm in
       Result.Ok (`Mask mm')
    | _ -> assert false in
  let sym_shape = function
    | `Point col -> Result.Ok (`Point col)
    | `Rectangle (size, col, mask) ->
       let| mask' = sym_mask mask in
       Result.Ok (`Rectangle (sym_size size, col, mask'))
    | _ -> assert false
  in
  match role_e1, d1 with
  | `Vec `Pos, _ -> Result.Ok (sym_pos d1)
  | `Vec (`Size _), _ -> Result.Ok (sym_size d1)
  | `Vec `Move, _ -> Result.Ok (sym_move d1)
  | `Mask, _ -> sym_mask d1
  | `Shape, _ -> sym_shape d1
  | `Layer, `PosShape (pos, shape) ->
     let| shape' = sym_shape shape in
     Result.Ok (`PosShape (pos, shape')) (* NOTE: do not use sym_pos because pos in PosShape must be the top-left corner of the shape, see def of TranslationSym *)
  | _ -> Result.Error (Invalid_expr e)

let unfold_symmetry (sym_matrix : symmetry list list) =
  let rec mask_matrix m = function
    | [] -> assert false
    | [row] -> mask_row m row
    | row::rows -> Grid.Mask.concatHeight (mask_row m row) (mask_matrix m rows)
  and mask_row m = function
    | [] -> assert false
    | [sym] -> mask_sym m sym
    | sym::syms -> Grid.Mask.concatWidth (mask_sym m sym) (mask_row m syms)
  and mask_sym m = function
    | `Id -> m
    | `FlipHeight -> Grid.Mask.flipHeight m
    | `FlipWidth -> Grid.Mask.flipWidth m
    | `FlipDiag1 -> Grid.Mask.flipDiag1 m
    | `FlipDiag2 -> Grid.Mask.flipDiag2 m
    | `Rotate180 -> Grid.Mask.rotate180 m
    | `Rotate90 -> Grid.Mask.rotate90 m
    | `Rotate270 -> Grid.Mask.rotate270 m
  in
  let unfold_size = function
    | `Vec (`Int h, `Int w) ->
       let k, l =
         match sym_matrix with
         | (_::syms)::rows -> 1 + List.length rows, 1 + List.length syms
         | _ -> assert false in
       `Vec (`Int (k*h), `Int (l*w))
    | _ -> assert false in
  let unfold_mask = function
    | `Mask m -> Result.Ok (`Mask (mask_matrix m sym_matrix))
    | `Full _ -> Result.Ok (`Full false)
    | _ -> Result.Error (Undefined_result "Model2.unfold_symmetry: not a custom mask")  
  in
  fun e d ->
  match d with
  | `Mask mm ->
     let| mm = unfold_mask mm in
     Result.Ok (`Mask mm)
  | `Rectangle (size, col, `Mask mm) ->
     let| mm = unfold_mask mm in
     Result.Ok (`Rectangle (unfold_size size, col, `Mask mm))
  | `Point _ -> Result.Error (Undefined_result "Model2.unfold_symmetry: point")
  | `PosShape (pos, `Rectangle (size, col, `Mask mm)) ->
     let| mm = unfold_mask mm in
     Result.Ok (`PosShape (pos, `Rectangle (unfold_size size, col, `Mask mm)))
  | `PosShape (_, `Point _) -> Result.Error (Undefined_result "Model2.unfold_symmetry: point")
  | _ -> Result.Error (Invalid_expr e)

let apply_expr_gen
      (apply : lookup:apply_lookup -> revpath -> 'a -> template result)
      ~(lookup : apply_lookup) (p : revpath) (e : 'a expr) : template result = (* QUICK *)
  (* SHOULD NOT use [p] *)
  match e with
  | `Ref p -> (lookup (p :> var) :> template result)
  | `ConstInt k -> Result.Ok (`Int k)
  | `ConstVec (k,l) -> Result.Ok (`Vec (`Int k, `Int l))
  | `Plus (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Int i1, `Int i2 -> Result.Ok (`Int (i1 + i2))
        | `Vec (`Int i1, `Int j1), `Vec (`Int i2, `Int j2) -> Result.Ok (`Vec (`Int (i1+i2), `Int (j1+j2)))
        | _ -> Result.Error (Invalid_expr e))
  | `Minus (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Int i1, `Int i2 -> Result.Ok (`Int (i1-i2))
        | `Vec (`Int i1, `Int j1), `Vec (`Int i2, `Int j2) -> Result.Ok (`Vec (`Int (i1-i2), `Int (j1-j2)))
        | _ -> Result.Error (Invalid_expr e))
  | `IncrInt (e1,k) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Int i1 -> Result.Ok (`Int (i1 + k))
        | _ -> Result.Error (Invalid_expr e))
  | `DecrInt (e1,k) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Int i1 -> Result.Ok (`Int (i1 - k))
        | _ -> Result.Error (Invalid_expr e))
  | `IncrVec (e1,k,l) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Vec (`Int i1, `Int j1) -> Result.Ok (`Vec (`Int (i1 + k), `Int (j1 + l)))
        | _ -> Result.Error (Invalid_expr e))
  | `DecrVec (e1,k,l) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Int i1 -> Result.Ok (`Int (i1 - k))
        | `Vec (`Int i1, `Int j1) -> Result.Ok (`Vec (`Int (i1 - k), `Int (j1 - l)))
        | _ -> Result.Error (Invalid_expr e))
  | `Modulo (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Int i1, `Int i2 -> Result.Ok (`Int (i1 mod i2))
        | _ -> Result.Error (Invalid_expr e))
  | `ScaleUp (e1,k) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Int i1 -> Result.Ok (`Int (i1 * k))
        | `Vec (`Int i1, `Int j1) -> Result.Ok (`Vec (`Int (i1 * k), `Int (j1 * k)))
        | `Mask mm ->
           let| mm' = Grid.Mask_model.scale_up k k mm in
           Result.Ok (`Mask mm')
        | `Point col ->
           Result.Ok (`Rectangle (`Vec (`Int k, `Int k), col, `Mask (`Full false)))
        | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm) ->
           let| mm' = Grid.Mask_model.scale_up k k mm in
           Result.Ok (`Rectangle (`Vec (`Int (h * k), `Int (w * k)), col, `Mask mm'))
        | _ -> Result.Error (Invalid_expr e))
  | `ScaleDown (e1,k) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Int i1 ->
           let rem = i1 mod k in
           if rem = 0 || rem = k - 1 (* account for separators *)
           then Result.Ok (`Int (i1 / k))
           else Result.Error (Undefined_result "ScaleDown: not an integer")
        | `Vec (`Int i1, `Int j1) ->
           let remi, remj = i1 mod k, j1 mod k in
           if remi = remj && (remi = 0 || remi = k-1) (* account for separators *)
           then Result.Ok (`Vec (`Int (i1 / k), `Int (j1 / k)))
           else Result.Error (Undefined_result "ScaleDown: not an integer")
        | _ -> Result.Error (Invalid_expr e))
  | `ScaleTo (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask mm, `Vec (`Int new_h, `Int new_w) ->
           let| mm' = Grid.Mask_model.scale_to new_h new_w mm in
           Result.Ok (`Mask mm')
        | `Point col, `Vec (`Int new_h, `Int new_w) ->
           Result.Ok (`Rectangle (`Vec (`Int new_h, `Int new_w), col, `Mask (`Full false)))
        | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm), `Vec (`Int new_h, `Int new_w) ->
           let| mm' = Grid.Mask_model.scale_to new_h new_w mm in
           Result.Ok (`Rectangle (`Vec (`Int new_h, `Int new_w), col, `Mask mm'))
        | _ -> Result.Error (Invalid_expr e))
  | `Corner (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Vec (`Int i1, `Int j1), `Vec (`Int i2, `Int j2) ->
           if i1 <> i2 && j1 <> j2
           then Result.Ok (`Vec (`Int i1, `Int j2))
           else Result.Error (Undefined_result "Corner: vectors on same row/column")
        | _ -> Result.Error (Invalid_expr e))
  | `Min le1 ->
     let| lres1 = list_map_result (apply ~lookup p) le1 in
     broadcast_list_result lres1
       (fun lt1 ->
         let| is_int, is_vec, mini, minj =
           lt1
           |> List.fold_left
                (fun res t ->
                  let| is_int,is_vec,mini,minj = res in
                  match t with
                  | `Int i -> Result.Ok (true, is_vec, min i mini, minj)
                  | `Vec (`Int i, `Int j) -> Result.Ok (is_int, true, min i mini, min j minj)
                  | _ -> Result.Error (Invalid_expr e))
                (Result.Ok (false, false, max_int, max_int)) in
         match is_int, is_vec with
         | true, false -> Result.Ok (`Int mini)
         | false, true -> Result.Ok (`Vec (`Int mini, `Int minj))
         | _ -> assert false)
  | `Max le1 ->
     let| lres1 = list_map_result (apply ~lookup p) le1 in
     broadcast_list_result lres1
       (fun lt1 ->
         let| is_int,is_vec,maxi,maxj =
           lt1
           |> List.fold_left
                (fun res t ->
                  let| is_int,is_vec,maxi,maxj = res in
                  match t with
                  | `Int i -> Result.Ok (true, is_vec, max i maxi, maxj)
                  | `Vec (`Int i, `Int j) -> Result.Ok (is_int, true, max i maxi, max j maxj)
                  | _ -> Result.Error (Invalid_expr e))
                (Result.Ok (false, false, min_int, min_int)) in
         match is_int, is_vec with
         | true, false -> Result.Ok (`Int maxi)
         | false, true -> Result.Ok (`Vec (`Int maxi, `Int maxj))
         | _ -> assert false)
  | `Average le1 ->
     let| lres1 = list_map_result (apply ~lookup p) le1 in
     broadcast_list_result lres1
       (fun lt1 ->
         let| is_int,is_vec,n,sumi,sumj =
           lt1
           |> List.fold_left
                (fun res t ->
                  let| is_int,is_vec,n,sumi,sumj = res in
                  match t with
                  | `Int i -> Result.Ok (true, is_vec, n+1, sumi+i, sumj)
                  | `Vec (`Int i, `Int j) -> Result.Ok (is_int, true, n+1, sumi+i, sumj+j)
                  | _ -> Result.Error (Invalid_expr e))
              (Result.Ok (false, false, 0, 0, 0)) in
         match is_int, is_vec with
         | true, false ->
            if sumi mod n = 0
            then Result.Ok (`Int (sumi / n))
            else Result.Error (Undefined_result "Average: not an integer")
         | false, true ->
            if sumi mod n = 0 && sumj mod n = 0
            then Result.Ok (`Vec (`Int (sumi / n), `Int (sumj / n)))
            else Result.Error (Undefined_result "Average: not an integer")
         | _ -> assert false) (* empty or ill-typed list *)
  | `Span (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Int i1, `Int i2 ->
           if i1=i2
           then Result.Error (Undefined_result "Span: same int")
           else Result.Ok (`Int (abs (i2-i1) + 1))
        | `Vec (`Int i1, `Int j1), `Vec (`Int i2, `Int j2) ->
           if i1=i2 && j1=j2
           then Result.Error (Undefined_result "Span: same vector")
           else Result.Ok (`Vec (`Int (abs (i2-i1) + 1), `Int (abs (j2-j1) + 1)))
        | _ -> Result.Error (Invalid_expr e))
  | `Norm e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Vec (`Int i, `Int j) -> Result.Ok (`Int (i+j))
        | _ -> Result.Error (Invalid_expr e))
  | `Diag1 (e1,k) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Vec (`Int i, `Int j) -> Result.Ok (`Int ((i+j) mod k))
        | _ -> Result.Error (Invalid_expr e))
  | `Diag2 (e1,k) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Vec (`Int i, `Int j) -> Result.Ok (`Int ((i-j) mod k))
        | _ -> Result.Error (Invalid_expr e))
  | `LogAnd (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask m1, `Mask m2 ->
           (match m1, m2 with
            | `Full _, _ -> Result.Ok (`Mask m2)
            | _, `Full _ -> Result.Ok (`Mask m1)
            | `Mask bm1, `Mask bm2 when Grid.Mask.same_size bm1 bm2 ->
               Result.Ok (`Mask (`Mask (Grid.Mask.inter bm1 bm2)))
            | _ -> Result.Error (Undefined_result "LogAnd: undefined"))
        | _ -> Result.Error (Invalid_expr e))
  | `LogOr (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask m1, `Mask m2 ->
           (match m1, m2 with
            | `Full _, _ -> Result.Ok (`Mask m1)
            | _, `Full _ -> Result.Ok (`Mask m2)
            | `Mask bm1, `Mask bm2 when Grid.Mask.same_size bm1 bm2 ->
               Result.Ok (`Mask (`Mask (Grid.Mask.union bm1 bm2)))
            | _ -> Result.Error (Undefined_result "LogOr: undefined"))
        | _ -> Result.Error (Invalid_expr e))
  | `LogXOr (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask m1, `Mask m2 ->
           (match m1, m2 with
            | `Full _, `Mask bm2 -> Result.Ok (`Mask (`Mask (Grid.Mask.compl bm2)))
            | `Mask bm1, `Full _ -> Result.Ok (`Mask (`Mask (Grid.Mask.compl bm1)))
            | `Mask bm1, `Mask bm2 when Grid.Mask.same_size bm1 bm2 ->
               Result.Ok (`Mask (`Mask (Grid.Mask.diff_sym bm1 bm2)))
            | _ -> Result.Error (Undefined_result "LogXOr: undefined"))
        | _ -> Result.Error (Invalid_expr e))
  | `LogAndNot (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask m1, `Mask m2 ->
           (match m1, m2 with
            | `Full _, `Mask bm2 -> Result.Ok (`Mask (`Mask (Grid.Mask.compl bm2)))
            | `Mask bm1, `Mask bm2 when Grid.Mask.same_size bm1 bm2 ->
               Result.Ok (`Mask (`Mask (Grid.Mask.diff bm1 bm2)))
            | _ -> Result.Error (Undefined_result "LogAndNot: undefined"))
        | _ -> Result.Error (Invalid_expr e))
  | `LogNot e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Mask m1 ->
           (match m1 with
            | `Mask bm1 -> Result.Ok (`Mask (`Mask (Grid.Mask.compl bm1)))
          | _ -> Result.Error (Undefined_result "LogNot: undefined"))
        | _ -> Result.Error (Invalid_expr e))
  | `Area e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Point _ -> Result.Ok (`Int 1)
        | `Rectangle (`Vec (`Int height, `Int width), _, `Mask m) ->
           Result.Ok (`Int (Grid.Mask_model.area ~height ~width m))
        | _ -> Result.Error (Invalid_expr e))
  | `Left e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `PosShape (`Vec (_, `Int j), `Rectangle _) -> Result.Ok (`Int j)
        | `PosShape _ -> Result.Error (Undefined_result "Left: not a rectangle")
        | _ -> Result.Error (Invalid_expr e))
  | `Right e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `PosShape (`Vec (_, `Int j), `Rectangle (`Vec (_, `Int w), _, _)) -> Result.Ok (`Int (j+w-1))
        | `PosShape _ -> Result.Error (Undefined_result "Right: not a rectangle")
        | _ -> Result.Error (Invalid_expr e))
  | `Center e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `PosShape (`Vec (_, `Int j), `Rectangle (`Vec (_, `Int w), _, _)) ->
           if w mod 2 = 0
           then Result.Error (Undefined_result "Center: no center, even width")
           else Result.Ok (`Int (j + w/2 + 1))
        | `PosShape _ -> Result.Error (Undefined_result "Center: not a rectangle")
        | _ -> Result.Error (Invalid_expr e))
  | `Top e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `PosShape (`Vec (`Int i, _), `Rectangle _) -> Result.Ok (`Int i)
        | `PosShape _ -> Result.Error (Undefined_result "Top: not a rectangle")
        | _ -> Result.Error (Invalid_expr e))
  | `Bottom e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `PosShape (`Vec (`Int i, _), `Rectangle (`Vec (`Int h, _), _, _)) -> Result.Ok (`Int (i+h-1))
        | `PosShape _ -> Result.Error (Undefined_result "Bottom: not a rectangle")
        | _ -> Result.Error (Invalid_expr e))
  | `Middle e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `PosShape (`Vec (`Int i, _), `Rectangle (`Vec (`Int h, _), _, _)) ->
           if h mod 2 = 0
           then Result.Error (Undefined_result "Middle: no middle, even height")
           else Result.Ok (`Int (i + h/2 + 1))
        | `PosShape _ -> Result.Error (Undefined_result "Middle: not a rectangle")
        | _ -> Result.Error (Invalid_expr e))
  | `ProjI e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Vec (`Int i, _) -> Result.Ok (`Vec (`Int i, `Int 0))
        | _ -> Result.Error (Invalid_expr e))
  | `ProjJ e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Vec (_, `Int j) -> Result.Ok (`Vec (`Int 0, `Int j))
        | _ -> Result.Error (Invalid_expr e))         
  | `TranslationOnto (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
    (fun (d1,d2) ->
      match get_pos d1, get_size d1, get_pos d2, get_size d2 with
      | Some (mini1,minj1), Some (h1,w1), Some (mini2,minj2), Some (h2,w2) ->
         let maxi1, maxj1 = mini1 + h1 - 1, minj1 + w1 - 1 in
         let maxi2, maxj2 = mini2 + h2 - 1, minj2 + w2 - 1 in
         let ti =
           if maxi1 < mini2 then mini2 - maxi1 - 1
           else if maxi2 < mini1 then - (mini1 - maxi2 - 1)
           else 0 in
         let tj =
           if maxj1 < minj2 then minj2 - maxj1 - 1
           else if maxj2 < minj1 then - (minj1 - maxj2 - 1)
           else 0 in
         Result.Ok (`Vec (`Int ti, `Int tj))
      | _ -> Result.Error (Invalid_expr e))
  | `Tiling (e1,k,l) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Vec (`Int h, `Int w) -> Result.Ok (`Vec (`Int (h*k), `Int (w*l)))
        | `Mask mm ->
           let| mm' = Grid.Mask_model.tile k l mm in
           Result.Ok (`Mask mm')
        | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm) ->
           let| mm' = Grid.Mask_model.tile k l mm in
           Result.Ok (`Rectangle (`Vec (`Int (h*k), `Int (w*l)), col, `Mask mm'))
        | `Point _ -> Result.Error (Undefined_result "Tiling: undefined on points")
        | _ -> Result.Error (Invalid_expr e))
  | `ResizeAlikeTo (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | d1, `Vec (`Int h, `Int w) ->
           let aux = function
             | `Point col ->
                Result.Ok (`Rectangle (`Vec (`Int h, `Int w), col, `Mask (`Full false)))
             | `Rectangle (_size, col, `Mask mm) ->
                let| mm' = Grid.Mask_model.resize_alike h w mm in
                Result.Ok (`Rectangle (`Vec (`Int h, `Int w), col, `Mask mm'))
             | _ -> assert false in             
           (match d1 with
            | `Mask mm ->
               let| mm' = Grid.Mask_model.resize_alike h w mm in
               Result.Ok (`Mask mm')
            | (`Point _ | `Rectangle _ as shape) -> aux shape
            | `PosShape (pos,shape) ->
               let| shape' = aux shape in
               Result.Ok (`PosShape (pos, shape'))
            | _ -> Result.Error (Invalid_expr e))
        | _ -> Result.Error (Invalid_expr e))
  | `ApplySym (sym,e1,role_e1) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (apply_symmetry ~lookup sym role_e1 e)
  | `UnfoldSym (sym_matrix,e1) ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (unfold_symmetry sym_matrix e)
  | `TranslationSym (sym,e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (fun (d1,d2) ->
         match get_pos d1, get_size d1, get_pos d2, get_size d2 with
         | Some (mini1,minj1), Some (h1,w1), Some (mini2,minj2), Some (h2,w2) ->
            let| ti, tj =
              match sym with
              | `Id -> Result.Ok (0, 0)
              | `FlipHeight -> Result.Ok (2 * (mini2-mini1) + (h2-h1), 0)
              | `FlipWidth -> Result.Ok (0, 2 * (minj2-minj1) + (w2-w1))
              | `Rotate180 -> Result.Ok (2 * (mini2-mini1) + (h2-h1), 2 * (minj2-minj1) + (w2-w1))
              | `FlipDiag1 ->
                 if h2 = w2
                 then
                   let ti = (mini2 - mini1) - (minj2 - minj1) (* + (h2 - w2) / 2 *) in
                   Result.Ok (ti, - ti)
                 else Result.Error (Undefined_result "TranslationSym: FlipDiag1: non-square pivot object")
              | `FlipDiag2 ->
                 if h2 = w2 && (h2 - h1 + w2 - w1 mod 2 = 0)
                 then
                   let ti = (mini2 - mini1) + (minj2 - minj1) + (h2 - h1 + w2 - w1) / 2 in
                   Result.Ok (ti, - ti)
                 else Result.Error (Undefined_result "TranslationSym: FlipDiag2: non-square pivot object")
              | `Rotate90 ->
                 if h2 = w2
                 then
                   Result.Ok
                     ((mini2 - mini1) - (minj2 - minj1) (* + (h2 - w2) / 2 *),
                      (mini2 - mini1) + (minj2 - minj1) + (h2 + w2) / 2 - h1) (* /2 OK because h2=w2 *)
                 else Result.Error (Undefined_result "TranslationSym: Rotate90: non-square pivot object")
              | `Rotate270 ->
                 if h2 = w2
                 then
                   Result.Ok
                     ((minj2 - minj1) + (mini2 - mini1) + (h2 + w2) / 2 - w1 (* /2 OK because h2=w2 *),
                      (minj2 - minj1) - (mini2 - mini1)) (* - (h2 - w2) / 2 *)
                 else Result.Error (Undefined_result "TranslationSym: Rotate90: non-square pivot object")
            in
            Result.Ok (`Vec (`Int ti, `Int tj))
         | _ -> Result.Error (Invalid_expr e))
  | `Coloring (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | d1, (`Color c as new_col) ->
           let aux = function
             | `Point _ -> Result.Ok (`Point new_col)
             | `Rectangle (size, _, mask)  -> Result.Ok (`Rectangle (size, new_col, mask))
             | _ -> Result.Error (Invalid_expr e) in
           (match d1 with
            | `PosShape (pos, shape) ->
               let| shape = aux shape in
               Result.Ok (`PosShape (pos, shape))
            | _ -> aux d1)
        | _ -> Result.Error (Invalid_expr e))

let apply_expr apply ~(env : data) e =
  apply_expr_gen apply ~lookup:(lookup_of_env env) `Root e
  
let rec apply_template_gen ~(lookup : apply_lookup) (p : revpath) (t : template) : template result = (* QUICK *)
  (* SHOULD NOT use [p] *)
  match t with
  | `U -> Result.Ok `U
  | #patt as patt -> (apply_patt apply_template_gen ~lookup p patt :> template result)
  | #expr as e -> apply_expr_gen apply_template_gen ~lookup p e
  | `Seq items ->
     let| applied_items =
       list_map_result
         (fun item -> apply_template_gen ~lookup p item)
         items in
     Result.Ok (`Seq applied_items)

let rec apply_template ~(env : data) (t : template) : template result =
  apply_template_gen ~lookup:(lookup_of_env env) `Root t
  (* try Result.Ok (apply_template_gen ~lookup:(lookup_of_env env) `Root t)
  with (* catching runtime error in expression eval *)
  | (Grid.Undefined_result _ as exn) -> Result.Error exn
  | (Unbound_var _ as exn) -> Result.Error exn
  | (Undefined_result _ as exn) -> Result.Error exn
  | (Negative_integer  as exn) -> Result.Error exn *)
(* DO NOT remove path argument, useful in generate_template (through apply_patt) *)


(* grid generation from data and template *)

let generate_patt (generate : revpath -> 'b -> 'a seq) (p : revpath) : 'b patt -> 'a seq = function
  | (`Bool _ | `Int _ | `Color _ | `Mask _ as d) -> d
  | `Vec (si,sj) ->
     broadcast2 (generate (p ++ `I) si,
                 generate (p ++ `J) sj)
       (fun (i,j) -> `Vec (i,j))
  | `Point (scolor) ->
     broadcast1 (generate (p ++ `Color) scolor)
       (fun color -> `Point color)
  | `Rectangle (ssize,scolor,smask) ->
     broadcast_list [generate (p ++ `Size) ssize;
                     generate (p ++ `Color) scolor;
                     generate (p ++ `Mask) smask]
       (function
        | [size; color; mask] -> `Rectangle (size, color, mask)
        | _ -> assert false)
  | `PosShape (spos,sshape) ->
     broadcast2 (generate (p ++ `Pos) spos,
                 generate (p ++ `Shape) sshape)
       (fun (pos,shape) -> `PosShape (pos,shape))
  | `Background (size,color,layers) ->
     `Background (generate (p ++ `Size) size, (* assumes size <> `Seq _ *)
                  generate (p ++ `Color) color, (* assumes color <> `Seq _ *)
                  map_ilist
                    (fun lp shape ->
                      generate (p ++ `Layer lp) shape) (* keep `Seq's as layers *)
                    `Root layers)

let rec generate_template (p : revpath) (t : template) : data = (* QUICK *)
  (* should be named 'ground_template' *)
  match t with
  | `U -> default_data_of_path p (* default data *)
  | #patt as patt -> (generate_patt generate_template p patt :> data)
  | #expr -> assert false (* should be eliminated by call to apply_template *)
  | `Seq items -> `Seq (List.map (fun item -> generate_template p item) items)

  
exception Invalid_data_as_grid of data
let _ = Printexc.register_printer
          (function
           | Invalid_data_as_grid d ->
              Some ("the data does not represent a valid grid specification:\n" ^ string_of_data d)
           | _ -> None)
          
let rec grid_of_data : data -> (Grid.t, exn) Result.t = function
  | `Background (`Vec (`Int h, `Int w), `Color c, l) when h>0 && w>0 ->
     let g = Grid.make h w c in
     (try draw_layers g l; Result.Ok g
      with exn -> Result.Error exn)
  | d -> Result.Error (Invalid_data_as_grid d)
and draw_layers g = function
  | `Nil -> ()
  | `Insert (above, layer, below) ->
     draw_layers g below;
     draw_layer g layer;
     draw_layers g above
and draw_layer g = function
  | `PosShape (`Vec (`Int i, `Int j), `Point (`Color c)) ->
     Grid.set_pixel g i j c
  | `PosShape (`Vec (`Int mini, `Int minj), `Rectangle (`Vec (`Int h, `Int w), `Color c, `Mask m)) when h>0 && w>0 ->
     let maxi = mini + h - 1 in
     let maxj = minj + w - 1 in
     for i = mini to maxi do
       for j = minj to maxj do
	 if Grid.Mask_model.mem ~height:h ~width:w (i-mini) (j-minj) m
	 then Grid.set_pixel g i j c
       done;
     done
  | `Seq items ->
     items |> List.rev |> List.iter (draw_layer g)
  | d -> raise (Invalid_data_as_grid d)

let undefined_grid = Grid.make 1 1 Grid.no_color
let grid_of_data_failsafe d =
  match grid_of_data d with
  | Result.Ok g -> g
  | Result.Error _ -> undefined_grid

let write_grid ~(env : data) ?(delta = delta0) (t : template) : (Grid.t, exn) Result.t = Common.prof "Model2.write_grid" (fun () ->
  let| t' = apply_template ~env t in
  let d = generate_template `Root t' in
  let| g = grid_of_data d in
  List.iter
    (fun (i,j,c) -> Grid.set_pixel g i j c)
    delta;
  Result.Ok g)

(* parsing grids with templates *)

type parse_state =
  { quota_diff: int; (* nb of allowed additional diffs *)
    diff: diff; (* paths to data that differ from template patterns *)
    delta: delta; (* pixels that are not explained by the template *)
    mask: Grid.Mask.t; (* remaining part of the grid to be explained *)
    parts: Grid.part list; (* remaining parts that can be used *)
    grid: Grid.t; (* the grid to parse *)
  }

let add_diff path state =
  { state with quota_diff = state.quota_diff - 1;
               diff = path::state.diff }

let add_delta_with_mask ~mask delta new_delta =
  List.fold_left
    (fun delta (i,j,c as pixel) ->
      if Grid.Mask.mem i j mask
      then pixel::delta
      else delta)
    delta new_delta
  
let filter_parts_with_mask ~new_mask parts = (* QUICK *)
  List.filter
    (fun p ->
      not (Grid.Mask.is_empty
	     (Grid.Mask.inter p.Grid.pixels new_mask)))
    parts

type ('a,'b) parseur = (* input -> state -> results *)
  Parseur of ('a -> parse_state -> ('b * parse_state * bool * ('a,'b) parseur) Myseq.t)
(* each result contains: a parsed value, the new state, a valid sequence 'stop' flag, and a parser for the sequence continuation *) 

let parseur_empty : ('a,'b) parseur =
  Parseur (fun x state -> Myseq.empty)

let rec parseur_const ?(history : 'b option = None) (f : 'a -> parse_state -> ('b * parse_state) Myseq.t) : ('a,'b) parseur =
  match history with
  | None ->
     Parseur (fun x state ->
         let*! y, state = f x state in
         (y, state, true, parseur_const f ~history:(Some y)))
  | Some y0 ->
     Parseur (fun x state ->
         let* y, state = f x state in
         if y = y0
         then Myseq.return (y, state, true, parseur_const f ~history)
         else Myseq.empty)

    
let rec parseur_rec (f : 'a -> parse_state -> ('b * parse_state) Myseq.t) : ('a,'b) parseur =
  Parseur (fun x state ->
      let*! y, state = f x state in
      (y, state, true, parseur_rec f))
let rec parseur_rec1 (Parseur parse1) f =
  Parseur (fun x state ->
      let*! y, state, stop1, parseur1 = f parse1 x state in
      (y, state, stop1, parseur_rec1 parseur1 f))
let rec parseur_rec2 (Parseur parse1) (Parseur parse2) f =
  Parseur (fun x state ->
      let*! y, state, stop1, stop2, parseur1, parseur2 =
        f parse1 parse2 x state in
      (y, state, stop1 && stop2, parseur_rec2 parseur1 parseur2 f))
let rec parseur_rec3 (Parseur parse1) (Parseur parse2) (Parseur parse3) f =
  Parseur (fun x state ->
      let*! y, state, stop1, stop2, stop3, parseur1, parseur2, parseur3 =
        f parse1 parse2 parse3 x state in
      (y, state, stop1 && stop2 && stop3, parseur_rec3 parseur1 parseur2 parseur3 f))
let rec parseur_rec4 (Parseur parse1) (Parseur parse2) (Parseur parse3) (Parseur parse4) f =
  Parseur (fun x state ->
      let*! y, state, stop1, stop2, stop3, stop4, parseur1, parseur2, parseur3, parseur4 =
        f parse1 parse2 parse3 parse4 x state in
      (y, state, stop1 && stop2 && stop3 && stop4, parseur_rec4 parseur1 parseur2 parseur3 parseur4 f))  

let rec parseur_seq (lparseur : ('a,'b) parseur list) : ('a,'b seq) parseur =
  let rec aux = function
    | [] -> parseur_empty
    | (Parseur parse_item)::l1 ->
       Parseur (fun x state ->
           let* y, state, stop, _next = parse_item x state in (* assuming no sequence nesting *)
           if stop
           then Myseq.return (y, state, (l1=[]), parseur_seq l1)
           else Myseq.empty)
  in
  aux lparseur
  
let parseur_collect ?(max_depth : int option) (Parseur parse) : ('a, 'b list) parseur =
  let rec aux ~depth x state sols =
    match max_depth, sols () with
    | Some maxd, _ when depth >= maxd -> Myseq.return ([], state)
    | _, Myseq.Nil -> Myseq.return ([], state)
    | _, Myseq.Cons ((y, state1, stop, Parseur parse_seq), next_sols) ->
       Myseq.concat [ (* sequence not in fair order *)
           (let* ly, stateN = aux ~depth:(depth+1) x state1 (parse_seq x state1) in
            if ly=[] && not stop
            then Myseq.empty (* failure, not stopping at a valid step *)
            else Myseq.return (y::ly, stateN));
           (fun () -> aux ~depth x state next_sols ())
         ] in
  Parseur (fun x state ->
      let* ly, state = aux ~depth:0 x state (parse x state) in
      Myseq.return (ly, state, true, parseur_empty))

(* let _ = (* unit test for parseur_collect *)
  let rec parseur_count n =
    Parseur (fun x state ->
        if n >= 9
        then Myseq.empty
        else
          let*! k = Myseq.range n 9 in
          (k, state, true, parseur_count (k+1))) in
  let rec parseur_perm l =
    Parseur (fun x state ->
        let* y = Myseq.from_list l in
        let l1 = List.filter ((<>) y) l in
        Myseq.return (y, state, l1=[], parseur_perm l1)) in
  let Parseur parse_all = parseur_collect (* ~max_depth:1 *) (parseur_perm [1;2;3;4]) in
  parse_all () (Obj.magic [|1;2;3|] : parse_state)
  |> Myseq.slice ~limit:10000
  |> Myseq.iter (fun (l,_,_,_) ->
         print_endline (String.concat " " (List.map string_of_int l))) *)

  

let rec parseur_template
          ~(parseur_u : unit -> ('a,data) parseur)
          ~(parseur_patt : template patt -> ('a,data) parseur)
          (t : template) (p : revpath)
        : ('a,data) parseur =
  match t with
  | `U -> parseur_u ()
  | #patt as patt -> parseur_patt patt
  | #expr -> assert false
  | `Seq items ->
     parseur_seq
       (List.map
          (fun item -> parseur_template ~parseur_u ~parseur_patt item p)
          items)
         

let parseur_bool t p : (bool,data) parseur = (* QUICK *)
  parseur_template
    ~parseur_u:(fun () ->
      parseur_rec (fun b state -> Myseq.return (`Bool b, state)))
    ~parseur_patt:(function
      | `Bool b0 ->
         parseur_rec (fun b state ->
             if b=b0 then Myseq.return (`Bool b, state)
             else if state.quota_diff > 0 then
               Myseq.return (`Bool b, add_diff p state)
             else Myseq.empty)
      | _ -> parseur_empty)
    t p

let parseur_int t p : (int,data) parseur = (* QUICK *)
  parseur_template
    ~parseur_u:(fun () ->
      parseur_rec (fun i state -> Myseq.return (`Int i, state)))
    ~parseur_patt:(function
      | `Int i0 ->
         parseur_rec (fun i state ->
             if i=i0 then Myseq.return (`Int i, state)
             else if state.quota_diff > 0 then
               Myseq.return (`Int i, add_diff p state)
             else Myseq.empty)
      | _ -> parseur_empty)
    t p

let parseur_color t p : (Grid.color,data) parseur = (* QUICK *)
  parseur_template
    ~parseur_u:(fun () ->
      parseur_rec (fun c state -> Myseq.return (`Color c, state)))
    ~parseur_patt:(function
      | `Color c0 ->
         parseur_rec (fun c state ->
             if c0 = c then Myseq.return (`Color c0, state)
             else if state.quota_diff > 0 then
               Myseq.return (`Color c, add_diff p state)
             else Myseq.empty)
      | _ -> parseur_empty)
    t p
  
let parseur_mask t p : (Grid.Mask_model.t list, data) parseur = (* QUICK *)
  parseur_template
    ~parseur_u:
    (fun () ->
      parseur_rec (fun ms state ->
          let* m = Myseq.from_list ms in
          Myseq.return (`Mask m, state)))
    ~parseur_patt:(function
      | `Mask m0 ->
         parseur_rec (fun ms state ->
             if List.exists (Grid.Mask_model.subsumes m0) ms then
               Myseq.return (`Mask m0, state)
             else if state.quota_diff > 0 then
               let* m = Myseq.from_list ms in
               Myseq.return (`Mask m, add_diff p state)
             else Myseq.empty)
      | _ -> parseur_empty)
    t p

let parseur_vec t p : (int * int, data) parseur = (* QUICK *)
  parseur_template
    ~parseur_u:(fun () ->
      parseur_rec (fun (vi,vj) state ->
          Myseq.return (`Vec (`Int vi, `Int vj), state)))
    ~parseur_patt:(function
      | `Vec (i,j) ->
         parseur_rec2
           (parseur_int i (p ++ `I))
           (parseur_int j (p ++ `J))
           (fun parse_i parse_j (i,j) state ->
             let* di, state, stop_i, next_i = parse_i i state in
             let* dj, state, stop_j, next_j = parse_j j state in
             Myseq.return (`Vec (di,dj), state, stop_i, stop_j, next_i, next_j))
      | _ -> parseur_empty)
    t p

let state_minus_shape_gen state occ_delta occ_new_cover =
  Common.prof "Model2.state_minus_shape_gen" (fun () ->
  let new_mask = Grid.Mask.diff state.mask occ_new_cover in
  if Grid.Mask.equal new_mask state.mask
  then None (* the shape is fully hidden, explains nothing new *)
  else
    let new_state =
      { state with
	mask = new_mask;
        delta = add_delta_with_mask ~mask:state.mask state.delta occ_delta;
	parts = filter_parts_with_mask ~new_mask state.parts
                |> List.filter (fun p -> not (Grid.Mask.is_subset occ_new_cover p.Grid.pixels))
                               (* that would make occ useless if selecting p later *)
      } in
    Some new_state)
let state_minus_point state (i,j,c) =
  let occ_delta = [] in
  let occ_new_cover = Grid.Mask.singleton state.grid.height state.grid.width i j in
  state_minus_shape_gen state occ_delta occ_new_cover
let state_minus_rectangle state (rect : Grid.rectangle) =
  state_minus_shape_gen state rect.delta rect.new_cover
  
let rec parseur_shape (t : template) (p : revpath) : (unit,data) parseur =
  let parseur_point pos color p : (Grid.pixel, data) parseur =
    parseur_rec2
      (parseur_vec pos (p ++ `Pos))
      (parseur_color color (p ++ `Shape ++ `Color))
      (fun parse_pos parse_color (i,j,c) state ->
        let* dpos, state, stop_pos, next_pos = parse_pos (i,j) state in
        let* dcolor, state, stop_color, next_color = parse_color c state in
        Myseq.return (`PosShape (dpos, `Point (dcolor)), state,
                      stop_pos, stop_color, next_pos, next_color)) in
  let parseur_rectangle pos size color mask p : (Grid.rectangle, data) parseur = (* QUICK *)
    parseur_rec4
      (parseur_vec pos (p ++ `Pos))
      (parseur_vec size (p ++ `Shape ++ `Size))
      (parseur_color color (p ++ `Shape ++ `Color))
      (parseur_mask mask (p ++ `Shape ++ `Mask))
      (fun parse_pos parse_size parse_color parse_mask rect state ->
        let open Grid in
        let* dpos, state, stop_pos, next_pos = parse_pos (rect.offset_i,rect.offset_j) state in
        let* dsize, state, stop_size, next_size = parse_size (rect.height,rect.width) state in
        let* dcolor, state, stop_color, next_color = parse_color rect.color state in
        let* dmask, state, stop_mask, next_mask = parse_mask rect.mask_models state in
        Myseq.return (`PosShape (dpos, `Rectangle (dsize,dcolor,dmask)), state,
                      stop_pos, stop_size, stop_color, stop_mask,
                      next_pos, next_size, next_color, next_mask))
  in
  let parse_all_points state =
    let points = Grid.points state.grid state.mask state.parts in
    Myseq.prof "Model2.parse_all_points/seq" (
    let* point = Myseq.from_list points in
    let* state = Myseq.from_option (state_minus_point state point) in
    Myseq.return (point, state)) in
  let parse_all_rectangles state =
    let rectangles = Grid.rectangles state.grid state.mask state.parts in
    Myseq.prof "Model2.parse_all_rectangles/seq" (
    let* rect = Myseq.from_list rectangles in
    let* state = Myseq.from_option (state_minus_rectangle state rect) in
    Myseq.return (rect, state))
  in
  let parseur_single_point pos color p : (unit,data) parseur =
    parseur_rec1
      (parseur_point pos color p)
      (fun parse_point () state ->
        let points = Grid.points state.grid state.mask state.parts in
        Myseq.prof "Model2.parse_single_point/seq" (
        let* point = Myseq.from_list points in
        let* dpoint, state, stop_point, next_point = parse_point point state in
        let* state = Myseq.from_option (state_minus_point state point) in
        Myseq.return (dpoint, state, stop_point, next_point))) in
  let parseur_single_rectangle pos size color mask p : (unit,data) parseur =    
    parseur_rec1
      (parseur_rectangle pos size color mask p)
      (fun parse_rectangle () state ->
        let rectangles = Grid.rectangles state.grid state.mask state.parts in
        Myseq.prof "Model2.parse_single_rectangle/seq" (
        let* rect = Myseq.from_list rectangles in
        let* drect, state, stop_rectangle, next_rectangle = parse_rectangle rect state in
        let* state = Myseq.from_option (state_minus_rectangle state rect) in
        Myseq.return (drect, state, stop_rectangle, next_rectangle)))
  in
  parseur_template
    ~parseur_u:
    (fun () ->
      parseur_rec (fun () state ->
          Myseq.concat
            [(let* r, state = parse_all_rectangles state in
              let open Grid in
              let* m = Myseq.from_list r.mask_models in
              if r.height = 1 && r.width = 1 (* point *)
              then Myseq.empty
              else Myseq.return
                     (`PosShape (`Vec (`Int r.offset_i, `Int r.offset_j),
                                 `Rectangle (`Vec (`Int r.height, `Int r.width),
                                             `Color r.color,
                                             `Mask m)),
                      state));
             (let* (i,j,c), state = parse_all_points state in
              Myseq.return
                (`PosShape (`Vec (`Int i, `Int j), `Point (`Color c)),
                 state))
    ]))        
    ~parseur_patt:(function
      | `PosShape (pos, `Point (color)) ->
         parseur_single_point pos color p
      | `PosShape (pos, `Rectangle (size,color,mask)) ->
         parseur_single_rectangle pos size color mask p
      | `PosShape (pos, `Seq shapes) ->
         parseur_rec2
           (parseur_vec pos (p ++ `Pos))
           (parseur_seq
              (List.map
                 (fun shape -> parseur_shape (`PosShape (`U, shape)) p)
                 shapes))
           (fun parse_pos parse_object () state ->
             let* dobj, state, stop_object, next_object = parse_object () state in
             match dobj with
             | `PosShape (`Vec (`Int di, `Int dj), dshape) ->
                let* dpos, state, stop_pos, next_pos = parse_pos (di,dj) state in
                Myseq.return (`PosShape (dpos, dshape), state,
                              stop_pos, stop_object, next_pos, next_object)
             | _ -> assert false)
             
      | _ -> assert false)
    t p

(* TEST *)
  (*
let parseur_layer (shape : template) (p : revpath) : (unit,data) parseur =
  let Parseur parse_sh = parseur_shape shape p in
  Parseur (fun () state ->
  let parse_seq_sh =
    Myseq.star_dependent_fair
      (*~max_relaxation_level:(!max_relaxation_level_parse_layers)*)
      (Myseq.const (parse_sh ())) in
  let* items, state = parse_seq_sh state in
  let items, state = (* deterministically parsing other items, as much as possible *)
    let rec aux res state =
      match Myseq.hd_opt (parse_sh () state) with
      | None -> res, state
      | Some (item,state) -> aux (item::res) state
    in
    let rev_items, state = aux (List.rev items) state in
    List.rev rev_items, state in
  Myseq.return (`Seq items, state))
   *)
let parseur_layer (shape : template) (p : revpath) : (unit,data) parseur =
  let Parseur parse_shapes = parseur_collect ~max_depth:1 (* TEST *) (parseur_shape shape p) in
  Parseur (fun () state ->
      let* ld, state, _, _ = parse_shapes () state in
      match ld with
      | [] -> Myseq.empty
      | [d] -> Myseq.return (d, state, true, parseur_empty)
      | _ -> Myseq.return (`Seq ld, state, true, parseur_empty))
  
let parseur_layers layers p : (unit, data ilist) parseur =
  let rev_layer_parses =
    fold_ilist
      (fun revl lp layer ->
        let Parseur parse = parseur_layer layer (p ++ `Layer lp) in
        let simple_parse =
          fun state ->
          let* d, state, _stop, _next = parse () state in (* TODO: handle _next to get sequences *)
          Myseq.return (d,state) in
        simple_parse::revl)
      [] `Root layers in
  let gen = Myseq.product_dependent_fair
              ~max_relaxation_level:(!max_relaxation_level_parse_layers) (* TODO: pb when nesting fair iterations *)
              (List.rev rev_layer_parses) in
  Parseur (fun () state ->
  Myseq.prof "Model2.parse_layers/seq" (
      let* ld, state = gen state in
      let l, dlayers = fill_ilist_with_list layers ld in
      assert (l = []);
      Myseq.return (dlayers, state, true, parseur_empty)))
  
let parseur_grid t p : (Grid.t, data) parseur = Common.prof "Model2.parse_grid" (fun () ->
  parseur_template
    ~parseur_u:(fun () -> parseur_empty)
    ~parseur_patt:
    (function
     | `Background (size,color,layers) ->
        let Parseur parse_size = parseur_vec size (p ++ `Size) in
        let Parseur parse_color = parseur_color color (p ++ `Color) in
        let seq_background_colors =
          match color with
          | `Color bc -> (fun g -> Myseq.return bc)
          | `U -> (fun g -> Myseq.from_list (Grid.background_colors g))
          | `Seq _ -> (fun g -> Myseq.empty) (* TODO: avoid sequence insertion where not expected *)
          | _ -> assert false in
        let parse_bg_color g state =
          let* bc = seq_background_colors g in
          let* dcolor, state, _, _ = parse_color bc state in
          let state = { state with (* ignoring parts belonging to background *)
                        parts = List.filter (fun (p : Grid.part) -> p.color <> bc) state.parts } in
          Myseq.return ((bc,dcolor),state,true,parseur_empty) in          
        let Parseur parse_layers = parseur_layers layers p in
        Parseur (fun (g : Grid.t) state -> Myseq.prof "Model2.parse_grid/seq" (
          let* dsize, state, _, _ = parse_size (g.height,g.width) state in
          let* (bc,dcolor), state, _, _ = parse_bg_color g state in
          let* dlayers, state, _, _ = parse_layers () state in
          (*let* ((bc,dcolor),dlayers), state =
            Myseq.pair_dependent_fair (parse_bg_color g) (parse_layers ()) state in*)
          let data = `Background (dsize,dcolor,dlayers) in
	  (* adding mask pixels with other color than background to delta *)
          let new_state =
            let new_delta = ref state.delta in
	    Grid.Mask.iter
	      (fun i j ->
                let c = g.matrix.{i,j} in
	        if c <> bc then
	          new_delta := (i,j,c)::!new_delta)
	      state.mask;
            { state with
              delta = (!new_delta);
	      mask = Grid.Mask.empty g.height g.width;
	      parts = [] } in
	  Myseq.return (data, new_state, true, parseur_empty)))
     | _ -> parseur_empty)
    t p)

exception Parse_failure
let _ = Printexc.register_printer
          (function
           | Parse_failure -> Some "the grid could not be parsed with the given template"
           | _ -> None)

(* reading grids *)
      
(* result of reading a grid *)
type grid_read = data * grid_data * dl (* env, grid_data, dl *)

let limit_dl (f_dl : 'a -> dl) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x0::_ ->
     let dl0 = f_dl x0 in
     let min_dl = !max_parse_dl_factor *. dl0 in
     List.filter (fun x -> f_dl x <= min_dl) l

let read_grid
      ~(quota_diff : int)
      ~(env : data) (t : template) (g : Grid.t)
    : (grid_read list, exn) Result.t =
  Common.prof "Model2.read_grid" (fun () ->
  let| t = apply_template ~env t in (* reducing expressions *)
  let Parseur parse_grid = parseur_grid t path0 in    
  let state = { quota_diff;
                diff = diff0;
                delta = delta0;
                mask = Grid.Mask.full g.height g.width;
                parts = Grid.segment_by_color g;
                grid = g } in
  let parses =
    let* qdiff = Myseq.range 0 quota_diff in (* for increasing diff quota *)
    let* data, state, stop, _next = parse_grid g {state with quota_diff = qdiff} in (* parse with this quota *)
    assert stop;
    let* () = Myseq.from_bool (state.quota_diff = 0) in (* check quota fully used to avoid redundancy *)
    let ctx = dl_ctx_of_data data in
    let dl = Common.prof "Model2.read_grid/first_parses/dl" (fun () ->
      let dl_data = dl_data_given_template ~ctx t data in
      let dl_diff = dl_diff ~ctx state.diff data in
      let dl_delta = dl_delta ~ctx state.delta in
      (* rounding before sorting to absorb float error accumulation *)
      dl_round (dl_data +. dl_diff +. dl_delta)) in
    let gd = {data; diff=state.diff; delta=state.delta} in
    Myseq.return (env, gd, dl) in
  let l_parses =
    Common.prof "Model2.read_grid/first_parses" (fun () ->
        parses
        |> Myseq.slice ~offset:0 ~limit:(!max_nb_parse)
        |> Myseq.to_list) in
  if l_parses = []
  then Result.Error Parse_failure
  else
    let best_parses =
      Common.prof "Model2.read_grid/best_parses" (fun () ->
          l_parses
          |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
          |> (fun l -> Common.sub_list l 0 !max_nb_grid_reads)
          |> limit_dl (fun (_,_,dl) -> dl)) in
    Result.Ok best_parses)

(* result of reading a list of grids with a grid model *)
type grids_read =
  { dl_m : dl; (* DL of the model *)
    reads : grid_read list list; (* outer list over grids, inner list over parses, sorted in increasing DL *)
  }

let pp_grids_read title gsr =
  print_endline title;
  let _ =
    List.fold_left
      (fun i grs ->
        Printf.printf "Grid %d\n" i;
        List.iter
          (fun (_,gd,_) ->
            pp_data gd.data;
            print_newline ())
          grs;
        print_newline ();
        i+1)
      1 gsr.reads in
  ()
  
let grids_read_has_delta (gsr : grids_read) : bool =
  gsr.reads
  |> List.exists (* NOT for_all: see be94 *)
       (fun egdls ->
         egdls
         |> List.exists (* NOT for_all: see e48d *)
              (fun (_env, (gd : grid_data), _dl) ->
                gd.delta <> []))

let read_grids ~quota_diff ~env_sig (t : template) (egrids: (data * Grid.t) list) : (grids_read, exn) Result.t =
  let dl_m =
    dl_template
      ~env_sig
      ~ctx:{box_height=Grid.max_size; box_width=Grid.max_size}
      t in
  let| reads =
    let+| env, g = egrids in
    read_grid ~quota_diff ~env t g in
  Result.Ok {dl_m; reads}

  
(** TASKS *)
       
(* task models, articulating parsing and evaluation *)

type model = (* input->output models *)    
  { input_pattern : template; (* only consts and unknowns allowed *)
    output_template : template (* complex expressions allowed *)
  }

let init_template =
(*  let u_rect = `PosShape (u_vec, `Rectangle (u_vec, `U, `U)) in
  let u_point = `PosShape (u_vec, `Point (`U)) in *)
  let u_layers = `Nil in
  (*let u_layers = `Insert (`Nil, u_layer, `Nil) in*)
  `Background (u_vec, `U, u_layers )
let init_model =
  { input_pattern = init_template;
    output_template = init_template }

let xp_model (print : Xprint.t) m =
  print#string "CONSTRUCT (Mo)\n";
  xp_template print m.output_template;
  print#string "\n";
  print#string "WHERE (Mi)\n";
  xp_template print m.input_pattern;
  print#string "\n"
let pp_model = Xprint.to_stdout xp_model
let string_of_model = Xprint.to_string xp_model

let apply_model ?(env = data0) (m : model) (g : Grid.t) : ((grid_data * Grid.t) list, exn) Result.t =
  Common.prof "Model2.apply_model" (fun () ->
  let+|+ _, gdi, _ =
    read_grid ~quota_diff:(!max_nb_diff) ~env m.input_pattern g in
  let| grid =
    write_grid ~env:gdi.data m.output_template in
  Result.Ok [(gdi, grid)])

(* result of reading a list of pairs of grids *)
type grid_pairs_read =
  { dl_mi : dl; (* input model DL *)
    dl_mo : dl; (* output model DL *)
    input_reads : grid_read list list; (* outer list over grids, inner list over parses *)
    reads : (grid_read * grid_read * dl) list list; (* outer list over grids, inner list over parses, sorted in increasing DL *)
  }

let split_grid_pairs_read (gprs: grid_pairs_read) : grids_read * grids_read =
  let project_reads proj =
    List.map
      (fun reads_pair ->
        reads_pair
        |> List.map proj)
      gprs.reads in
  let reads_i = project_reads (fun (gri,_,_) -> gri) in
  let reads_o = project_reads (fun (_,gro,_) -> gro) in
  let gsri = { dl_m = gprs.dl_mi; reads = reads_i } in
  let gsro = { dl_m = gprs.dl_mo; reads = reads_o } in
  gsri, gsro
  
let read_grid_pairs ?(env = data0) (m : model) (pairs : Task.pair list) : (grid_pairs_read, exn) Result.t =
  Common.prof "Model2.read_grid_pairs" (fun () ->
  (* takes model, input env+grid, output grids *)
  let dl_mi =
    dl_template
      ~env_sig:signature0
      ~ctx:{box_height=Grid.max_size; box_width=Grid.max_size}
      m.input_pattern in    
  let env_sig =
    signature_of_template m.input_pattern in
  let dl_mo =
    dl_template
      ~env_sig
      ~ctx:{box_height=Grid.max_size; box_width=Grid.max_size}
      m.output_template in
  let| input_reads_reads =
    let+| {input; output} = pairs in
    let| reads_input =
      read_grid ~quota_diff:0 ~env m.input_pattern input in (* no diff allowed during training *)
    let| reads_pair = 
      let+|+ (envi,gdi,dli as gri) = Result.Ok reads_input in      
      let+|+ (envo,gdo,dlo as gro) =
        read_grid ~quota_diff:0 ~env:gdi.data m.output_template output in
      let dl = dli +. dlo in
      Result.Ok [(gri,gro,dl)] in
    let reads_pair =
      reads_pair
      |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
      |> limit_dl (fun (_,_,dl) -> dl) in (* bounding by dl_factor *) 
    Result.Ok (reads_input, reads_pair) in
  let input_reads, reads = List.split input_reads_reads in
  Result.Ok {dl_mi; dl_mo; input_reads; reads})
  
(* template transformations *)

let insert_ilist  (f : 'a option -> 'a) (lp : ilist_revpath) (l : 'a ilist) : 'a ilist =
  Common.prof "Model2.insert_ilist" (fun () ->
  let rec aux lp insert =
    match lp with
    | `Root -> insert l
    | `Left lp1 ->
       aux lp1
         (function
          | `Nil -> assert false
          | `Insert (left,elt,right) -> `Insert (insert left, elt, right))
    | `Right lp1 ->
       aux lp1
       (function
        | `Nil -> assert false
        | `Insert (left,elt,right) -> `Insert (left, elt, insert right))
  in
  aux lp
    (function
     | `Nil -> `Insert (`Nil, f None, `Nil)
     | `Insert (left,elt,right) -> `Insert (left, f (Some elt), right)))

let rec insert_seq (f : 'a option -> 'a) (i : int) (l : 'a list) : 'a list =
  match l with
  | [] -> failwith "Model2.insert_seq: wrong position"
  | x::r ->
     if i = 0
     then f (Some x) :: r
     else x :: insert_seq f (i-1) r
  
let rec insert_patt (f : 'a option -> 'a) (field : field) (patt_parent : 'a patt) : 'a patt = (* one-step down insertion only *)
  Common.prof "Model2.insert_patt" (fun () ->
  match field, patt_parent with
  | `I, `Vec (i,j) -> `Vec (f (Some i), j)
  | `J, `Vec (i,j) -> `Vec (i, f (Some j))

  | `Color, `Point (color) -> `Point (f (Some color))

  | `Size, `Rectangle (size,color,mask) -> `Rectangle (f (Some size), color, mask)
  | `Color, `Rectangle (size,color,layers) -> `Rectangle (size, f (Some color), layers)
  | `Mask, `Rectangle (size,color,mask) -> `Rectangle (size, color, f (Some mask))

  | `Pos, `PosShape (pos,shape) -> `PosShape (f (Some pos), shape)
  | `Shape, `PosShape (pos,shape) -> `PosShape (pos, f (Some shape))

  | `Color, `Background (size,color,layers) -> `Background (size, f (Some color), layers)
  | `Size, `Background (size,color,layers) -> `Background (f (Some size), color, layers)
  | `Layer lp, `Background (size,color,layers) ->
     let new_layers = insert_ilist f lp layers in
     `Background (size, color, new_layers)

  | _ ->
     pp_field field; print_string ": ";
     pp_patt_dummy patt_parent;
     print_newline ();
     assert false)

       
let rec insert_template (f : template option -> template) (p : revpath) (t : template) : template =
  Common.prof "Model2.insert_template" (fun () ->
  match path_parent p with
  | None -> f (Some t)
  | Some parent ->
     insert_template
       (function
        | None -> assert false
        | Some (t_parent : template) ->
           match p, t_parent with (* TODO: match on p too *)
           | `Field (field,_), (#patt as patt_parent) ->
              (insert_patt f field patt_parent :> template)
           | `Item (i,_), `Seq items ->
              let new_items = insert_seq f i items in 
              `Seq new_items
           (* `U and other #expr are not explorable *)
           | _ -> assert false)
       parent t)
                                                                                    
(* model refinements and learning *)

type grid_refinement =
  | RGridInit
  | RDef of revpath * template * bool (* partial: only true for some items if many items *)
  | RObject of revpath * template (* object *)

let xp_grid_refinement (print : Xprint.t) = function
  | RGridInit -> ()
  | RDef (p,t,partial) ->
     print#string "DEF: "; xp_path print p;
     print#string "="; xp_template print t;
     if partial then print#string " (partial)"
  | RObject (path,obj) ->
     print#string "OBJECT at ";
     xp_path print path;
     print#string ": ";
     xp_template print obj
let pp_grid_refinement = Xprint.to_stdout xp_grid_refinement
let string_of_grid_refinement = Xprint.to_string xp_grid_refinement

exception Refinement_no_change
let apply_grid_refinement (r : grid_refinement) (t : template) : (grid_refinement * template) option (* None if no change *) =
  Common.prof "Model2.apply_grid_refinement" (fun () ->
  try
    let t =
      match r with
      | RGridInit -> raise Refinement_no_change
      | RDef (modified_p,new_t,partial) ->
         t
         |> insert_template
              (function
               | Some x when x = new_t -> raise Refinement_no_change
               | _ -> new_t)
              modified_p
      | RObject (path,obj) ->
         t
         |> insert_template
              (function
               | Some x when x = obj -> raise Refinement_no_change
               | _ -> obj)
              path
         |> insert_template (fun _ -> `U) (`Field (`Color,`Root)) (* because background color is defined as remaining color after covering shapes *)
    in
(*    print_string "New grid template: ";
    pp_template t;
    print_newline (); *)
    Some (r,t)
  with
  | Refinement_no_change -> None
  | exn ->
     print_endline "ERROR in apply_grid_refinement";
     pp_grid_refinement r; print_newline ();
     pp_template t; print_newline ();
     raise exn) (* does not seem to occur any more *)

let rec defs_refinements ~(env_sig : signature) (t : template) (grss : grid_read list list) : grid_refinement Myseq.t =
  Common.prof "Model2.defs_refinements" (fun () ->
  assert (grss <> []);
  let in_output = (env_sig <> []) in    
  let u_vars = (* QUICK *)
    List.rev
      (fold_template
         (fun res p t0 anc0 ->
           match t0 with
           | #expr -> res
           | _ ->
              let role = path_role p in
              let definable_var =
                match role with
                | `Grid -> false (* not grids *)
                | _ -> true in
              if definable_var
              then
                let dl0 = dl_template ~env_sig ~ctx:dl_ctx0 ~path:p t0 in
                (p,role,t0,dl0)::res
              else res)
         [] path0 t []) in
  let module PMap = (* mappings from defining templates *)
    Map.Make
      (struct
        type t = revpath
        let compare = Stdlib.compare
      end) in
  let reads_matrix = Common.prof "Model2.defs_refinements/val_matrix" (fun () ->
    List.map
      (fun grs ->
        assert (grs <> []); (* otherwise, should be parse failure *)
        let dl_best =
          match grs with
          | (_,_,dl)::_ -> dl (* DL of first parse *)
          | [] -> assert false in (* otherwise, should be parse failure *)
        List.mapi
          (fun rank (env,gd,dl) ->
            let u_val =
              List.fold_left
                (fun res (p,role,t0,dl_t0) ->
                  match find_data p gd.data with
                  | Some d -> PMap.add p d res
                  | None ->
                     pp_path p; print_string ": "; pp_data gd.data; print_newline ();
                     assert false)
                PMap.empty u_vars in
            env, gd, u_val, dl -. dl_best, rank)
          grs)
      grss) in
  let reads_fst, reads_others = (* reads for first example, other examples *)
    match reads_matrix with
    | x::l -> x, l
    | [] -> assert false in
  let rec find_dl_rank ?dl_best t p reads : (grid_data * Mdl.bits * int * data) option = (* proper QUICK *)
    match reads with
    | [] -> None
    | (env,gd,u_val,dl,rank)::rem ->
       let dl_best =
         match dl_best with
         | None -> dl
         | Some dl -> dl in
       let d = try PMap.find p u_val
               with _ -> pp_path p; assert false in
       if defs_check ~env t d
       then Some (gd, dl -. dl_best, rank, d) (* recording current_dl - best_dl *)
       else find_dl_rank ~dl_best t p rem
  in
  let module TMap = (* mappings from defining templates *)
    Map.Make
      (struct
        type t = template
        let compare = Stdlib.compare
      end) in
  (* defs from first example *)
  let defs = [] in
  let defs = Common.prof "Model2.defs_refinements/first/patterns" (fun () ->
    let$ defs, (p,role,t0,dl_t0) = defs, u_vars in
    let tmap_score_patt =
      match t0 with
      | `U | `Vec (`U, `U) ->
         let$ res, (env,gd,u_val,dl,rank) = TMap.empty, reads_fst in
         let d = try PMap.find p u_val with _ -> assert false in
         let dl_ctx = dl_ctx_of_data gd.data in
         let dl_d_t0 = dl_data_given_template ~ctx:dl_ctx ~path:p t0 d in
         let$ res, t = res, root_template_of_data ~in_output d in
         if t <> t0 (* a different pattern *)
            && not (TMap.mem t res) (* that was not already seen *)
                   (* && defs_check ~env t d (* and that checks. true by construction *) *)
         then
           let dl_t = dl_template ~env_sig ~ctx:dl_ctx0 ~path:p t in
           let dl_d_t = dl_data_given_template ~ctx:dl_ctx ~path:p t d in
           TMap.add t (dl +. dl_t -. dl_t0 +. dl_d_t -. dl_d_t0, rank) res
         else res
      | _ -> TMap.empty in
    TMap.fold
      (fun t (dl,rank) defs ->
        (dl,p,role,t0,t)::defs)
      tmap_score_patt defs) in
  let defs = Common.prof "Model2.defs_refinements/first/exprs" (fun () ->
    let$ defs, (role_e,t) = defs, defs_expressions ~env_sig in
    let data_fst = Common.prof "Model2.defs_refinements/first/exprs/apply" (fun () ->
      reads_fst
      |> List.filter_map
           (fun (env,gd,u_val,dl,rank) ->
             match defs_check_apply ~env t with
             | None -> None
             | Some te -> Some (te,gd,u_val,dl,rank))) in
    if data_fst = []
    then defs
    else
      let$ defs, (p,role,t0,dl_t0) = defs, u_vars in
      if role_poly_matches role_e role
       (* whether the expression role matches the defined path, and relaxation value *)
      then
        let data_opt =
          data_fst
          |> List.find_map
               (fun (te,gd,u_val,dl,rank) ->
                 let d = try PMap.find p u_val with _ -> assert false in
                 if matches_template te d
                 then Some (gd,dl,rank,d)
                 else None) in
        match data_opt with
        | None -> defs
        | Some (gd,dl,rank,d) ->
           let dl_t = dl_template ~env_sig ~ctx:dl_ctx0 ~path:p t in
           let dl_ctx = dl_ctx_of_data gd.data in
           let dl_d_t0 = dl_data_given_template ~ctx:dl_ctx ~path:p t0 d in
           let dl_d_t = dl_data_given_template ~ctx:dl_ctx ~path:p t d in
           (dl +. dl_t -. dl_t0 +. dl_d_t -. dl_d_t0, p, role, t0, t)::defs
      else defs) in
  (* checking defs w.r.t. other examples *)
  let defs = Common.prof "Model2.defs_refinements/others" (fun () ->
    let$ defs, reads = defs, reads_others in
    defs
    |> List.filter_map
         (fun (dl0,p,role,t0,t) ->
           match find_dl_rank t p reads with
           | None -> None
           | Some (gd1,dl1,rank1,d1) ->
              let dl_ctx = dl_ctx_of_data gd1.data in
              let dl_d_t0 = dl_data_given_template ~ctx:dl_ctx ~path:p t0 d1 in
              let dl_d_t = dl_data_given_template ~ctx:dl_ctx ~path:p t d1 in
              Some (dl0 +. dl1 +. dl_d_t -. dl_d_t0, p, role, t0, t))) in
  (* sorting defs, and returning them as a sequence *)
  defs
  |> List.rev (* to correct for the above List.fold_left's that stack in reverse *)
  |> List.stable_sort (* increasing delta DL *)
       (fun (delta_dl1,_,_,_,_) (delta_dl2,_,_,_,_) ->
         dl_compare delta_dl1 delta_dl2)
  |> Myseq.from_list
             |> Myseq.map (fun (_,p,_,_,t) -> RDef (p,t,false)))
and defs_check ~env (t : template) (d : data) : bool =
  match defs_check_apply ~env t with
  | None -> false
  | Some te -> matches_template te d
and defs_check_apply ~env (t : template) : template option =
  match apply_template ~env t with
  | Result.Ok t1 -> Some t1
  | Result.Error _ -> None
and defs_expressions ~env_sig : (role_poly * template) list =
  (* the [path option] is for the repeat context path, to be used in a For loop *)
  Common.prof "Model2.defs_expressions" (fun () -> (* QUICK *)
  if env_sig = [] then [] (* we are in the input model *)
  else (
  let paths =
    let$ res, (_k,lp) = [], env_sig in (* TODO: make env_sig a flat list *)
    let$ res, p = res, lp in
    ((path_role p :> role_poly), p) :: res in
  let exprs = ref ([] : (role_poly * template) list) in (* stack of expressions *)
  let quota = ref (!max_expressions) in (* bounding nb expressions because of combinatorial number in some tasks *)
  let push e = (* stacking an expression until quota exhausted *)
    exprs := e :: !exprs;
    decr quota;
    if !quota <= 0
    then raise End_of_file
    else () in
  (try
  (* LEVEL 0 *)
  let _ =
    let& (role,p) = paths in
    push (role, `Ref p) in
  let exprs_0 = List.rev !exprs in
  (* LEVEL 1 *)
  let _ =
    let& (role1,e1) = exprs_0 in
    (* unary operators *)
    let _ = (* area(_) *)
      match role1 with
      | `Shape -> push (`Int (`X, `Size `X), `Area e1)
      | _ -> () in
    let _ = (* right(_) *)
      match role1 with
      | `Layer -> push (`Int (`J, `Pos), `Right e1)
      | _ -> () in
    let _ = (* center(_) *)
      match role1 with
      | `Layer -> push (`Int (`J, `Pos), `Center e1)
      | _ -> () in
    let _ = (* bottom(_) *)
      match role1 with
      | `Layer -> push (`Int (`I, `Pos), `Bottom e1)
      | _ -> () in
    let _ = (* middle(_) *)
      match role1 with
      | `Layer -> push (`Int (`I, `Pos), `Middle e1)
      | _ -> () in
    let _ = (* ProjI/J *)
      match role1 with
      | `Vec _ ->
         push (role1, `ProjI e1);
         push (role1, `ProjJ e1)
      | _ -> () in
    let _ = (* ApplySym *)
      match role1 with
      | (`Mask | `Shape | `Layer as role1) ->
         let& sym = [`FlipHeight; `FlipWidth; `FlipDiag1; `FlipDiag2;
                     `Rotate180; `Rotate90; `Rotate270] in
         push (role1, `ApplySym (sym, e1, role1))
      | _ -> () in
    let _ = (* Coloring(_, const) *)
      match role1 with
      | (`Shape | `Layer) ->
         let& c = Grid.all_colors in
         push (role1, `Coloring (e1, `Color c))
      | _ -> () in
    let& (role2,e2) = exprs_0 in
      (* binary operators *)
      let _ = (* corner(_,_) *)
        match role1, role2 with
        | `Vec `Pos, `Vec `Pos when e1 <> e2 ->
           push (role1, `Corner (e1,e2))
        | _ -> () in (* TEST: var/feat *)
      let _ = (* span(_,_) *)
        match role1, role2 with
        | `Int ((`I | `J as ij1), `Pos), `Int ((`I | `J as ij2), `Pos) when ij1=ij2 && e1 < e2 ->
           push (`Int (ij1, `Pos), `Span (e1,e2))
        | `Vec `Pos, `Vec `Pos when e1 < e2 ->
           push (`Vec (`Size `X), `Span (e1,e2))
        | _ -> () in
      let _ = (* min([_;_]) *)
        match role1, role2 with
        | `Int xx1, `Int xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Min [e1;e2])
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Min [e1;e2])
        | _ -> () in
      let _ = (* max([_;_]) *)
        match role1, role2 with
        | `Int xx1, `Int xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Max [e1;e2])
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Max [e1;e2])
        | _ -> () in
      let _ = (* average([_;_]) *)
        match role1, role2 with
        | `Int xx1, `Int xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Average [e1;e2])
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Average [e1;e2])
        | _ -> () in
      let _ = (* translation = pos - pos *)
        match role1, role2 with
        | `Int (ij1, `Pos), `Int (ij2, `Pos) when ij1=ij2 && e1 <> e2 ->
           push (`Int (ij1, `Move), `Minus (e1,e2))
        | `Vec `Pos, `Vec `Pos when e1 <> e2 ->
           push (`Vec `Move, `Minus (e1,e2))
        | _ -> () in
      let _ = (* translationOnto(_,_) *)
        match role1, role2 with
        | `Layer, `Layer when e1 < e2 ->
           push (`Vec `Move, `TranslationOnto (e1,e2));
        | _ -> () in
      let _ = (* translationSym(_,_,_) *)
        match role1, role2 with
        | `Layer, (`Layer|`Grid) when e1 <> e2 ->
           let& sym = [`FlipHeight; `FlipWidth; `FlipDiag1; `FlipDiag2;
                       `Rotate180; `Rotate90; `Rotate270] in
           push (`Vec `Move, `TranslationSym (sym,e1,e2))
        | _ -> () in
      let _ = (* Coloring (_, ref) *)
        match role1, role2 with
        | (`Shape | `Layer), `Color _ -> push (role1, `Coloring (e1,e2))
        | _ -> () in
      () in
  let exprs_1 = List.rev !exprs in
  (* LEVEL 2, not generating Move at this level *)
  let _ = 
    let _ = (* constants *)
      let& k = [0;1;2;3] in
      push (`Int (`X, (if k=0 then `Pos else `X)), `ConstInt k);
      let& l = [0;1;2;3] in
      push (`Vec (if k=0 && l=0 then `Pos else `X), `ConstVec (k,l)) in
    let& (role1,e1) = exprs_1 in
    (* unary operators *)
    let _ = (* IncrInt, DecrInt *)
      match role1 with
      | `Int (_,rvec) when rvec <> `Move ->
         let& k = [1;2;3] in
         push (role1, `IncrInt (e1,k));
         push (role1, `DecrInt (e1,k))
      | _ -> () in
    let _ = (* IncrVec, DecrVec *)
      match role1 with
      | `Vec rvec when rvec <> `Move ->
         let& k = [0;1;2;3] in
         let& l = [0;1;2;3] in
         if k+l > 0 then (
           push (role1, `IncrVec (e1,k,l));
           push (role1, `DecrVec (e1,k,l))
         )
      | _ -> () in
    let _ =
      let& n = [2;3] in
      let _ = (* ScaleUp, ScaleDown *)
        match role1 with
        | `Int (_,rvec) | `Vec rvec when rvec <> `Move ->
           push (role1, `ScaleUp (e1,n));
           push (role1, `ScaleDown (e1,n))
        | `Mask | `Shape ->
           push (role1, `ScaleUp (e1,n))
        | _ -> () in
      () in
    let _ = (* not _ *)
      match role1 with
      | `Mask -> push (`Mask, `LogNot e1)
      | _ -> () in
    let _ = (* 0 + move -> pos *)
      match role1 with
      | `Int (ij, `Move) -> push (`Int (ij, `Pos), `Plus (`ConstInt 0, e1))
      | `Vec `Move -> push (`Vec `Pos, `Plus (`ConstVec (0,0), e1))
      | _ -> () in
    let& (role2,e2) = exprs_1 in
      (* binary operators *)
      let _ = (* _ + _ *)
        match role1, role2 with
        | `Int (_, xx1), `Int (_, (`X | `Size _ | `Move))
          | `Vec xx1, `Vec (`X | `Size _ | `Move)
             when xx1 <> `Move && (if xx1 = `Pos then e1 <> e2 else e1 < e2) ->
           push (role1, `Plus (e1,e2))
        | _ -> () in
      let _ = (* _ - _ *)
        match role1, role2 with (* not generating Moves *)
        | `Int (_, rvec), `Int (_, (`X | `Size _ | `Move)) when rvec <> `Move && e1 <> e2 ->
           push (role1, `Minus (e1,e2))
        | `Vec rvec, `Vec (`X | `Size _ | `Move) when rvec <> `Move && e1 <> e2 ->
           push (role1, `Minus (e1,e2))
        | _ -> () in
      () in
  let exprs_2 = List.rev !exprs in
  (* LEVEL 3 *)
  let _ = 
    let& (role1,e1) = exprs_2 in
    let _ = (* Tiling *)
      match role1 with
      | (`Vec (`X | `Size _) | `Mask | `Shape) ->
         let& k = [1;2;3] in
         let& l = [1;2;3] in
         if k>1 || l>1 then
           push (role1, `Tiling (e1,k,l))
      | _ -> () in
    let _ = (* UnfoldSym *)
      match role1 with
      | `Mask | `Shape | `Layer ->
         let& sym_matrix = [sym_matrix_flipHeightWidth] in
         push (role1, `UnfoldSym (sym_matrix, e1))
      | _ -> () in
    let& (role2,e2) = exprs_2 in
      let _ = (* ScaleTo on masks *)
        match role1, role2 with
        | (`Mask | `Shape), `Vec (`X | `Size _) ->
           push (role1, `ScaleTo (e1,e2))
        | _ -> () in
      let _ = (* ResizeAlikeTo *)
        match role1, role2 with
        | (`Mask | `Shape | `Layer), `Vec (`X | `Size _) ->
           push (role1, `ResizeAlikeTo (e1,e2))
        | _ -> () in
      let _ = (* _ and _ *)
        match role1, role2 with
        | `Mask, `Mask when e1 < e2 -> push (`Mask, `LogAnd (e1,e2))
        | _ -> () in
      let _ = (* _ or _ *)
        match role1, role2 with
        | `Mask, `Mask when e1 < e2 -> push (`Mask, `LogOr (e1,e2))
        | _ -> () in
      let _ = (* _ xor _ *)
        match role1, role2 with
        | `Mask, `Mask when e1 < e2 -> push (`Mask, `LogXOr (e1,e2))
        | _ -> () in
      let _ = (* _ and not _ *)
        match role1, role2 with
        | `Mask, `Mask when e1 <> e2 -> push (`Mask, `LogAndNot (e1,e2))
        | _ -> () in
      () in
  ()
   with End_of_file -> ());
  (*Printf.printf "== %d expressions ==\n" (List.length !exprs);*)
  List.rev !exprs)) (* approximate order in increasing size *)
  
let shape_refinements ~(env_sig : signature) (t : template) : grid_refinement Myseq.t =
  Common.prof "Model2.shape_refinements" (fun () ->
  let rec aux ~(objs : template list) lp = function
    | `Nil ->
       let* obj = Myseq.from_list objs in
       Myseq.return (RObject (`Field (`Layer lp, `Root), obj))
    | `Insert (above,_,below) ->
       Myseq.concat
         [ aux ~objs (`Right lp) below; (* insert below first *)
           aux ~objs (`Left lp) above ]
    | _ -> assert false
  in
  match t with
  | `Background (_,_,layers) ->
     (* TEST let su =
       aux ~objs:[`U] `Root layers in *)
     let sp =
       let objs = [`PosShape (u_vec, `Point (`U))] in
       aux ~objs `Root layers in
     let sr =
       let objs = [`PosShape (u_vec, `Rectangle (u_vec, `U, `U))] in
       aux ~objs `Root layers in
     let ss =
       let ps_shape = signature_of_kind env_sig Shape in
       Myseq.concat
         (List.map
            (fun p_shape ->
              let objs = [`PosShape (u_vec, `Ref p_shape)] in
              aux ~objs `Root layers)
            ps_shape) in
     let so =
       let ps_layer = signature_of_kind env_sig Layer in
       Myseq.concat
         (List.map
            (fun p_layer ->
              let objs = [`Ref p_layer] in
              aux ~objs `Root layers)
            ps_layer) in
     Myseq.concat [so; ss; sr; sp (* TEST; su*)]
  | _ -> assert false)

let grid_refinements ~(env_sig : signature) (t : template) (grss : grid_read list list) : (grid_refinement * template) Myseq.t =
  Myseq.prof "Model2.grid_refinements" (
  Myseq.concat
    [defs_refinements ~env_sig t grss;
     shape_refinements ~env_sig t]
  |> Myseq.filter_map
       (fun r ->
         pp_grid_refinement r; print_newline (); (* TEST *)
         apply_grid_refinement r t))

let dl_grid_model_data (gsr : grids_read) : dl triple (* model, data, model+data *) =
  let dl_data =
    !alpha (* because given training examples are only a sample from a class of grids *)
    *. Mdl.sum gsr.reads
         (function
          | [] -> assert false
          | (_env,_gd,dl)::_ -> dl) in (* using smallest dl *)
  gsr.dl_m, dl_data, gsr.dl_m +. dl_data
		      
let learn_grid_model ~timeout ~beam_width ~refine_degree ~env_sig
      (egrids : (data * Grid.t) list)
    : ((grid_refinement * template) * grids_read * dl) list * bool =
  Grid.reset_memoized_functions ();
  reset_dl_data ();
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RGridInit, init_template)
    ~data:(fun (r,m) ->
      Result.to_option (read_grids ~quota_diff:0 ~env_sig m egrids))
    ~code:(fun (r,m) gsr ->
      let lm, ld, lmd = dl_grid_model_data gsr in
      lmd)
    ~refinements:(fun (r,m) gsr dl ->
      (*pp_grid_model m; print_newline ();*)
      Printf.printf "%.1f\t" dl;
      pp_grid_refinement r; print_newline ();
      (*Printf.printf "DL = %.1f + %.1f = %.1f\n" lm ld lmd;*)
      grid_refinements ~env_sig m gsr.reads)
		     

type refinement =
  | RInit
  | Rinput of grid_refinement
  | Routput of grid_refinement

let xp_refinement (print : Xprint.t) = function
  | RInit -> ()
  | Rinput r -> print#string "IN  "; xp_grid_refinement print r
  | Routput r -> print#string "OUT "; xp_grid_refinement print r
let pp_refinement = Xprint.to_stdout xp_refinement
let string_of_refinement = Xprint.to_string xp_refinement

let apply_refinement (r : refinement) (m : model) : (refinement * model) option =
  Common.prof "Model2.apply_refinement" (fun () ->
  match r with
  | RInit -> None
  | Rinput gr ->
     apply_grid_refinement gr m.input_pattern
     |> Option.map (fun (_,t) -> r, {m with input_pattern = t})
  | Routput gr ->
     apply_grid_refinement gr m.output_template
     |> Option.map (fun (_,t) -> r, {m with output_template = t}))
                 
let model_refinements (last_r : refinement) (m : model) (gsri : grids_read) (gsro : grids_read) : (refinement * model) Myseq.t
  =
  Common.prof "Model2.model_refinements" (fun () ->
  let on_input = true
    (*match last_r with
    | RInit -> true
    | Rinput _ -> true
    | Routput _ -> false*) in
  let on_output = true in
  let envo_sig = signature_of_template m.input_pattern in
  let ref_defis =
    if on_input
    then
      defs_refinements ~env_sig:signature0 m.input_pattern gsri.reads
      |> Myseq.filter_map (fun r -> apply_refinement (Rinput r) m)
    else Myseq.empty in
  let ref_defos =
    if on_output
    then
      defs_refinements ~env_sig:envo_sig m.output_template gsro.reads
      |> Myseq.filter_map (fun r -> apply_refinement (Routput r) m)
    else Myseq.empty in
  let ref_shapis =
    if on_input && grids_read_has_delta gsri
    then
      shape_refinements ~env_sig:signature0 m.input_pattern
      |> Myseq.filter_map
	   (fun r -> apply_refinement (Rinput r) m)
    else Myseq.empty in
  let ref_shapos =
    if on_output && grids_read_has_delta gsro
    then
      shape_refinements ~env_sig:envo_sig m.output_template
      |> Myseq.filter_map
	   (fun r -> apply_refinement (Routput r) m)
    else Myseq.empty in
  Myseq.prof "Model2.model_refinements/seq" (
      Myseq.concat
        [ref_shapis; ref_shapos; ref_defis; ref_defos]
    ))

let dl_model_data (gpsr : grid_pairs_read) : dl triple triple =
  Common.prof "Model2.dl_model_data" (fun () ->
  let lmi = gpsr.dl_mi in
  let lmo = gpsr.dl_mo in
  let ldi, ldo =
    List.fold_left
      (fun (ldi,ldo) ->
        function
        | ((_,_,dli),(_,_,dlo),dl)::_ -> (ldi +. dli, ldo +. dlo)
        | _ -> assert false)
      (0.,0.) gpsr.reads in
  let ldi, ldo = !alpha *. ldi, !alpha *. ldo in
  let lmdi = lmi +. ldi in
  let lmdo = lmo +. ldo in
  (lmi, lmo, lmi +. lmo),
  (ldi, ldo, ldi +. ldo),
  (lmdi, lmdo, lmdi +. lmdo))

let make_norm_dl_model_data () : grid_pairs_read -> dl triple triple =
  let lmdi0 = ref (-1.) in
  let lmdo0 = ref (-1.) in
  fun gpsr ->
  let (lmi,lmo,lm), (ldi,ldo,ld), (lmdi,lmdo,lmd) =
    dl_model_data gpsr in
  let () = (* setting initial DLs *)
    if !lmdi0 < 0.
    then ( lmdi0 := lmdi; lmdo0 := lmdo ) in
  let nlmi, nldi, nlmdi = lmi /. !lmdi0, ldi /. !lmdi0, lmdi /. !lmdi0 in
  let nlmo, nldo, nlmdo = lmo /. !lmdo0, ldo /. !lmdo0, lmdo /. !lmdo0 in
  (nlmi, nlmo, nlmi +. nlmo),
  (nldi, nldo, nldi +. nldo),
  (nlmdi, nlmdo, nlmdi +. nlmdo)
  
let learn_model
      ?(verbose = false)
      ?(grid_viz = false)
      ?(pause = 0.)
      ~timeout
      ~init_model
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : ((refinement * model) * (grid_pairs_read * grids_read * grids_read) * dl) list * bool
  = Common.prof "Model2.learn_model" (fun () ->
  Grid.reset_memoized_functions ();
  let norm_dl_model_data = make_norm_dl_model_data () in
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RInit, init_model)
    ~data:(fun (r,m) ->
      try
        (*if verbose then (
          print_string "\t=> "; pp_refinement r; print_newline ()); *)
        Result.to_option
          (let| gprs = read_grid_pairs m pairs in
           let grsi, grso = split_grid_pairs_read gprs in
           Result.Ok (gprs,grsi,grso))
      with
      | Common.Timeout as exn -> raise exn
      | exn ->
         print_endline "ERROR while parsing examples with new model";
	 print_endline (Printexc.to_string exn);
	 pp_refinement r; print_newline ();
         pp_model m; print_newline ();
	 raise exn)
    ~code:(fun (r,m) (gpsr,gsri,gsro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     norm_dl_model_data gpsr in
           if verbose then (
             Printf.printf "\t?? %.3f\t" lmd;
             pp_refinement r; print_newline ();
(*
	     Printf.printf "\t\tl = %.3f = %.3f + %.3f = (%.3f + %.3f) + (%.3f + %.3f)\n" lmd lm ld lmi lmo ldi ldo;
             print_endline " ===> all reads for first example";
             List.hd gpsr.reads
             |> List.iter
                  (fun ((_,{data=d_i},dl_i), (_, {data=d_o}, dl_o), dl) ->
                    print_endline " --- some read ---";
                    pp_data d_i; print_newline ();
                    pp_data d_o; print_newline ();
                    Printf.printf "\tdl=%.6f\n" dl);
             print_newline ()
              
             print_endline " ===> best read for all examples";
             gpsr.reads
             |> List.iter
                  (fun read ->
                    List.hd read
                    |> (fun ((_,{data=d_i},dl_i), (_, {data=d_o}, dl_o), dl) ->
                     print_endline " --- some read ---";
                     pp_data d_i; print_newline ();
                     pp_data d_o; print_newline ();
                     Printf.printf "\tdl=%.3f\n" dl));
             print_newline ();
  *)
           );
	   flush stdout;
           lmd)
    ~refinements:
    (fun (r,m) (gpsr,gsri,gsro) dl ->
      if verbose then print_newline ();
      Printf.printf "%.3f\t" dl; pp_refinement r; print_newline ();
      if verbose then (
        print_endline " ===> first read for first example";
        List.hd (List.hd gpsr.reads)
        |> (fun ((_,{data=d_i},dl_i), (_, {data=d_o}, dl_o), dl) ->
          print_endline " --- some read ---";
          pp_data d_i; print_newline ();
          pp_data d_o; print_newline ();
          Printf.printf "\tdl=%.1f\n" dl);
        print_newline ());
      if grid_viz then (
        List.iter2
          (fun reads_input reads_pair ->
            match reads_pair with
            | ((_,gdi_knowing_o,_), (_,gdo,_), _)::_ ->
               let gi1 = grid_of_data_failsafe gdi_knowing_o.data in
               let go1 = grid_of_data_failsafe gdo.data in
               let res2 = (* searching for a parse able to generate an output *)
                 let+|+ _, gdi2, _ = Result.Ok reads_input in
                 let| go2 = write_grid ~env:gdi2.data m.output_template in
                 Result.Ok [(gdi2,go2)] in
               (match res2 with
                | Result.Ok ((gdi2,go2)::_) ->
                   let gi2 = grid_of_data_failsafe gdi2.data in
                   Grid.pp_grids [gi1; go1; gi2; go2]
                | Result.Ok [] -> assert false
                | Result.Error exn ->
                   Grid.pp_grids [gi1; go1];
                   print_endline "No output grid could be produced from a parsing of the input grid";
                   print_endline (" => " ^ Printexc.to_string exn));
               print_newline ()
            | _ -> assert false)
          gpsr.input_reads gpsr.reads;
        Unix.sleepf pause);
        (*pp_grids_read "### OUT grids_read ###" gsro;*)
      (*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
      flush stdout;
      let refs = model_refinements r m gsri gsro in
      refs))


