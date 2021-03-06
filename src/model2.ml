
open Task

let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v
   
let alpha = def_param "alpha" 10. string_of_float
let max_nb_parse = def_param "max_nb_parse" 64 string_of_int (* max nb of considered grid parses *)
let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
let max_relaxation_level_parse_layers = def_param "max_relaxation_level_parse_layers" 16 string_of_int (* see parse_layers *)
let max_nb_diff = def_param "max_nb_diff" 3 string_of_int (* max nb of allowed diffs in grid parse *)
let max_nb_grid_reads = def_param "max_nb_grid_reads" 3 string_of_int (* max nb of selected grid reads, passed to the next stage *)
let max_expressions = def_param "max_expressions" 10000 string_of_int (* max nb of considered expressions when generating defs-refinements *)
let max_refinements = def_param "max_refinements" 20 (* TEST 50 *) string_of_int (* max nb of considered refinements *)
let use_repeat = def_param "use_repeat" false string_of_bool (* whether to use the Repeat/For constructs in models *)

exception TODO

(* binders and syntactic sugar *)

let ( %* ) = Myseq.empty
let ( !* ) = Myseq.return
let ( ** ) = Myseq.cons
let ( @* ) = fun seq1 seq2 -> Myseq.concat [seq1; seq2]
        
let ( let| ) res f = Result.bind res f [@@inline]
let ( let* ) seq f = seq |> Myseq.flat_map f [@@inline]
let ( let*? ) seq f = seq |> Myseq.filter_map f [@@inline]
let ( let*! ) seq f = seq |> Myseq.map f [@@inline]

let ( let$ ) (init,l) f =
  l |> List.fold_left (fun res x -> f (res, x)) init [@@inline]
let ( let& ) l f =
  l |> List.iter (fun x -> f x) [@@inline]

let rec result_list_bind (lx : 'a list) (f : 'a -> ('b,'c) Result.t) : ('b list, 'c) Result.t =
  match lx with
  | [] -> Result.Ok []
  | x::lx1 ->
     let| y = f x in
     let| ly1 = result_list_bind lx1 f in
     Result.Ok (y::ly1)
let ( let+| ) = result_list_bind
                   
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

let concat_rev_list_seq (revl : 'a list) (seq : 'a Myseq.t) : 'a Myseq.t =
  (* generates elements in revl in reverse, then elements from seq *)
  (fun () ->
    List.fold_left
      (fun seq x -> Myseq.cons x seq)
      seq revl
      ()) [@@inline]

class ['a] iterator (seq : 'a Myseq.t) =
(* turns a sequence into a stateful iterator *)
object
  (* TODO: record when empty to optimize repeating pops when empty *)
  val mutable s = seq
  method pop : 'a option =
    let open Myseq in
    match s () with
    | Nil -> None
    | Cons (x,next) -> s <- next; Some x
end


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
            
let rec fill_ilist_with_rev_list il l = (* QUICK *)
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
        l, `Insert (left', x, right')

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
  | `Index
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
  | `Index -> Int
  | `Color _ -> Color
  | `Mask -> Mask
  | `Vec _ -> Vec
  | `Shape -> Shape
  | `Object -> Object
  | `Layer -> Layer
  | `Grid -> Grid

type role_poly = (* polymorphic extension of role *)
  [ `Int of [`I | `J | `X] * role_vec_poly
  | `Index
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
    | `Index, `Index -> true
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
  | `Many of bool * 'a list (* many-valued data: ordered, values (objects) *)
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
  | `Arg of int * role option * revpath (* if no role, parent role *)
  | `Item of int option (* pos *) * revpath (* local *) * revpath (* ctx *) ]
let path0 = `Root

let any_item p = `Item (None,`Root,p)
let ith_item i p = `Item (Some i,`Root,p)

let rec (++) p f =
  match p with
  | `Item (i_opt,local,ctx) -> `Item (i_opt, local ++ f, ctx)
  | _ -> `Field (f,p)
       
let rec path_parent : revpath -> revpath option = function
  | `Root -> None
  | `Field (f,p) -> Some p
  | `Arg (i,role_opt,p) -> Some p
  | `Item (i_opt,local,ctx) ->
     ( match path_parent local with
       | None -> Some ctx
       | Some local_parent -> Some (`Item (i_opt,local_parent,ctx)) )
       
let path_split_any_item (path : revpath) : (revpath * revpath) option (* local, ctx *) =
  match path with
  | `Item (None,local,ctx) -> Some (local,ctx)
  | _ -> None
       
let path_ctx (path : revpath) : revpath option =
  match path with
  | `Item (None,local,ctx) -> Some ctx
  | _ -> None

(* reversing a revpath, to get paths starting from the root: used in path_factorize *)
(* must be its own inverse: reverse(reverse(p,`Root),`Root) = p *)
let rec path_reverse (p : revpath) (acc : revpath) : revpath =
  match p with
  | `Root -> acc
  | `Field (`Layer lp, p1) -> path_reverse p1 (`Field (`Layer (ilist_reverse lp `Root), acc))
  | `Field (f,p1) -> path_reverse p1 (`Field (f,acc))
  | `Arg (i,role_opt,p1) -> path_reverse p1 (`Arg (i,role_opt,acc))
  | `Item (i_opt,p1,p2) -> path_reverse p2 (`Item (i_opt, path_reverse p1 `Root, acc))
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
    | `Arg (i1,role1_opt,rq1), `Arg (i2,role2_opt,rq2) ->
       if i1 = i2 && role1_opt = role2_opt
       then aux_path (`Arg (i1,role1_opt,p0)) rq1 rq2
       else p0, path_reverse rp1 `Root, path_reverse rp2 `Root
    | `Item (i1_opt,rq11,rq12), `Item (i2_opt,rq21,rq22) -> (* rq1 > item i1_opt > rq2 *)
       let p01, p11, p21 = aux_path `Root rq11 rq21 in
       if p11 = `Root && p21 = `Root && i1_opt = i2_opt
       then aux_path (`Item (i1_opt,p01,p0)) rq12 rq22
       else `Item (None,p01,p0),
            path_reverse rq21 (`Item (i1_opt, p11, `Root)),
            path_reverse rq22 (`Item (i2_opt, p21, `Root))
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
       
type data = data patt
let data0 = `Background (`Vec (`Int 0, `Int 0), `Color Grid.black, `Nil)

type var =
  [ revpath
  | `Index (* reference to current item index in For-loop *)
  ]

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
  | `Index (* Int *)
  | `Indexing of 'a * 'a (* (Many A, Int) => A *)
  ]
and symmetry = [
  | `Id
  | `FlipHeight | `FlipWidth | `FlipDiag1 | `FlipDiag2
  | `Rotate180 | `Rotate90 | `Rotate270 ]

let sym_matrix_flipHeightWidth = [[`Id; `FlipWidth]; [`FlipHeight; `Rotate180]]
             
type template =
  [ `U
  | `Repeat of template patt
  | `For of revpath * template (* revpath is a many-valued ref *)
  | template patt
  | template expr ] (* TODO: should sub-expressions be restricted to patt and expr ? *)

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
  | `Index -> print#string "index"
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
  | `Arg (i,role_opt,p) -> xp_path print p; print#string "."; print#int i
  | `Item (None,local,ctx) -> xp_path print ctx; print#string "{"; xp_path print local; print#string "}"
  | `Item (Some i,local,ctx) -> xp_path print ctx; print#string "{";  xp_path print local; print#string "}["; print#int i; print#string "]"
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
  | `Many (ordered,l) ->
     Xprint.bracket
       (if ordered then "[", "]" else "{\n\t", " }")
       (Xprint.sep_list ",\n\t" xp)
       print
       l
let pp_patt xp patt = Xprint.to_stdout (xp_patt xp) patt
let pp_patt_dummy patt = pp_patt (fun print _ -> print#string "_") patt

let rec xp_data (print : Xprint.t) : data -> unit = function
  | #patt as patt -> xp_patt xp_data print patt
let pp_data = Xprint.to_stdout xp_data
let string_of_data = Xprint.to_string xp_data

let string_of_index = "$index"
              
let xp_var (print : Xprint.t) : var -> unit = function
  | #revpath as p -> xp_path print p
  | `Index -> print#string string_of_index
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
  | `Index -> print#string string_of_index
  | `Indexing (e1,e2) -> xp print e1; print_string "["; xp print e2; print#string "]"
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
  | `Repeat patt -> print#string "repeat "; xp_patt xp_template print patt
  | `For (p,e1) -> print#string "for {"; xp_path print p; print#string "}: "; xp_template print e1
  | #patt as patt -> xp_patt xp_template print patt
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
  | `Field (f,_) ->
     (match f with
      | (`I | `J) -> Int
      | `Color -> Color
      | `Mask -> Mask
      | (`Pos | `Size) -> Vec
      | `Shape -> Shape
      | `Layer _ -> Layer)
  | `Arg (i,None,p1) -> path_kind p1
  | `Arg (i, Some role, p1) -> kind_of_role role
  | `Item (_,`Root, `Field (`Layer _, _)) -> Object
  | `Item (_,`Root, ctx) -> path_kind ctx
  | `Item (_,local,_) -> path_kind local

let rec path_role ?(ctx = None) (p : revpath) : role =
  match p with
  | `Root ->
     (match ctx with
      | None -> `Grid
      | Some (`Field (`Layer _, _)) -> `Object
      | Some p -> path_role ~ctx:None p) (* TODO: revise role of items of an explicit Many *)
  | `Field ((`I | `J as f), p1) -> `Int (f, path_role_vec ~ctx p1)
  | `Field (`Color, p1) -> `Color (path_role_frame ~ctx p1)
  | `Field (`Mask, _) -> `Mask
  | `Field (`Pos, _) -> `Vec `Pos
  | `Field (`Size, p1) -> `Vec (`Size (path_role_frame ~ctx p1))
  | `Field (`Shape, _) -> `Shape
  | `Field (`Layer _, _) -> `Layer
  | `Arg (i, None, p1) -> path_role ~ctx p1
  | `Arg (i, Some role, p1) -> role
  | `Item (_,local,ctx) -> path_role ~ctx:(Some ctx) local
and path_role_vec ~ctx : revpath -> role_vec = function
  | `Root ->
     (match ctx with
      | None -> assert false
      | Some p -> path_role_vec ~ctx:None p)
  | `Field (`Pos, _) -> `Pos
  | `Field (`Size, p1) -> `Size (path_role_frame ~ctx p1)
  | `Arg (i, None, p1) -> path_role_vec ~ctx p1
  | `Item (_,local,ctx) -> path_role_vec ~ctx:(Some ctx) local
  | p ->
     pp_path p; print_newline ();
     assert false
and path_role_frame ~ctx : revpath -> role_frame = function
  | `Root when ctx = None -> `Grid
  | `Field (`Shape, _) -> `Shape
  | `Arg (i, None, p1) -> path_role_frame ~ctx p1
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

let rec find_field_patt (find : 'a -> 'a option) (f : field) (patt_parent : 'a patt) : 'a option = (* only using head field of p *)
  match f, patt_parent with
  | `I, `Vec (i,j) -> find i
  | `J, `Vec (i,j) -> find j
  | `Color, `Point (color) -> find color
  | `Size, `Rectangle (size,color,mask) -> find size
  | `Color, `Rectangle (size,color,mask) -> find color
  | `Mask, `Rectangle (size,color,mask) -> find mask
  | `Pos, `PosShape (pos,shape) -> find pos
  | `Shape, `PosShape (pos,shape) -> find shape
  | `Size, `Background (size,color,layers) -> find size
  | `Color, `Background (size,color,layers) -> find color
  | `Layer lp, `Background (size,color,layers) ->
     (match find_ilist lp layers with
      | None -> None
      | Some layer -> find layer)
  | _ ->
     pp_field f; print_string ": ";
     pp_patt_dummy patt_parent;
     print_newline ();
     assert false

let rec find_patt (find : 'a -> 'a option) (p : revpath) (patt_parent : 'a patt) : 'a option =
  match p, patt_parent with
  | `Root, _ -> assert false (* there should be no parent *)
  | `Field (f,_), _ -> find_field_patt find f patt_parent
  | `Arg _, _ -> assert false
  | `Item (None,`Root,_), `Many (ordered,items) ->
     let lx_opt = option_list_bind items find in
     (match lx_opt with
      | None -> None
      | Some lx -> Some (`Many (ordered,lx)))
  | `Item (Some i,`Root,_), `Many (_,items_parent) ->
     (match List.nth_opt items_parent i with
      | None -> None
      | Some item -> find item)
  | `Item (_,local,_), _ -> find_patt find local patt_parent
       
let find_data (p : revpath) (d : data) : data option = (* QUICK *)
  let rec aux find p =
    match path_parent p with
    | None -> find d
    | Some parent ->
       aux
         (function
          | #patt as d_parent -> find_patt find p d_parent)
         parent
  in
  aux (fun x -> Some x) p
  
let find_template (p : revpath) (t : template) : template option =
  Common.prof "Model2.find_template" (fun () ->
  let rec aux find p =
    match path_parent p with
    | None -> find t
    | Some parent ->
       aux
         (function
          | `Repeat t1 -> find (t1 :> template) (* assuming p ~ `Item (None,`Root,_) *)
          | `For (_,t1) -> find t1 (* assuming p ~ `Item (None,`Root,_) *)
          | #patt as patt_parent -> find_patt find p patt_parent
          (* `U and #expr are not explorable *)
          | _ -> assert false)
         parent
  in
  aux (fun x -> Some x) p)

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
  | `Many (ordered,items) ->
     let _, acc =
       List.fold_left
         (fun (i,acc) item -> i+1, fold acc (ith_item i p) item patt_ancestry )
         (0,acc) items in
     acc

let rec fold_data (f : 'b -> revpath -> data -> data list (* ancestry *) -> 'b) (acc : 'b) (p : revpath) (d : data) (ancestry : data list) : 'b =
  let acc = f acc p d ancestry in
  fold_patt (fold_data f) acc p d (d::ancestry)
  
let rec fold_template (f : 'b -> revpath option -> revpath -> template -> template list (* ancestry *) -> 'b) (acc : 'b) (for_p : revpath option) (p : revpath) (t : template) (ancestry : template list) : 'b =
  let acc = f acc for_p p t ancestry in
  let t_ancestry = t::ancestry in
  match t with
  | `U -> acc
  | `Repeat patt1 ->
     let p1 = any_item p in
     let acc = f acc for_p p1 (patt1 :> template) t_ancestry in
     fold_patt
       (fun acc p t anc -> fold_template f acc for_p p t anc)
       acc p1 patt1 ((patt1 :> template)::t_ancestry)
  | `For (p_many,t1) ->
     let for_p1 = Some p_many in
     let p1 = any_item p in
     let acc = f acc for_p1 p1 t1 t_ancestry in
     fold_template f acc for_p1 p1 t1 t_ancestry
  | #patt as patt ->
     fold_patt
       (fun acc p t anc -> fold_template f acc for_p p t anc)
       acc p patt t_ancestry
  | #expr -> acc
       
let size_of_data (d : data) : int =
  Common.prof "Model2.size_of_data" (fun () ->
  fold_data (fun res _ _ _ -> res+1) 0 path0 d [])
let size_of_template (t : template) : int =
  fold_template (fun res _ _ _ _ -> res+1) 0 None path0 t []

let signature_of_template (t : template) : signature =
  Common.prof "Model2.signature_of_template" (fun () ->
  let ht = Hashtbl.create 13 in
  let () =
    fold_template
      (fun () for_p p t1 anc1 ->
        let k = path_kind p in
        let ps0 =
          match Hashtbl.find_opt ht k with
          | None -> []
          | Some ps -> ps in
        let ps =
          match p with
          | `Item (None,local,ctx) -> (* if many-valued, add path to first item *)
             let p_fst = `Item (Some 0,local,ctx) in
             let p_snd = `Item (Some 1,local,ctx) in
             let p_trd = `Item (Some 2,local,ctx) in
             p_trd::p_snd::p_fst::p::ps0
          | _ -> p::ps0 in
        Hashtbl.replace ht k ps;
        if k = Vec && t1 = `U then (
          let ps0 =
            match Hashtbl.find_opt ht Int with
            | None -> []
            | Some ps -> ps in
          Hashtbl.replace ht Int (p ++ `I :: p ++ `J :: ps0)
        ))
      () None path0 t [] in
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
  | `Index -> `Int 0
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
  | `Int i ->
     if in_output && i >= 1 && i <= 3
     then [(d :> template)]
     else [] (* position- and size-invariance of inputs *)
  | `Color _ -> [(d :> template)] (* colors can be seen as patterns *)
  | `Mask (`Full true) -> [`Mask `Border; `Mask (`Full false)]
  | `Mask _ -> [(d :> template)] (* masks can be seen as patterns *)
  | `Vec _ -> [`Vec (`U, `U)]
  | `Point _ -> [`Point (`U)]
  | `Rectangle _ -> [`Rectangle (u_vec, `U, `U)]
  | `PosShape _ -> [`PosShape (u_vec, `U)]
  | `Background _ -> assert false (* should not happen *)
  | `Many (ordered,items) ->
     let llt_items = List.map (root_template_of_data ~in_output) items in
     (d :> template) :: List.sort_uniq Stdlib.compare (List.concat llt_items)

let matches_ilist (matches : 'a -> 'b -> bool)
      (il1 : 'a ilist) (il2 : 'b ilist) : bool =
  let rev_l1 = fold_ilist (fun res _ t -> t::res) [] `Root il1 in
  let rev_l2 = fold_ilist (fun res _ d -> d::res) [] `Root il2 in
  List.for_all2 matches rev_l1 rev_l2

let rec matches_template (t : template) (d : data) : bool = (* QUICK *)
  match t, d with
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
  | `Many (ordered1,items1), `Many (ordered2,items2) ->
     ordered1 = ordered2 (* TODO: better handle ordered *)
     && List.for_all2 (fun item1 item2 -> matches_template item1 item2) items1 items2
  | _ -> false
    
(* description lengths *)

type dl = Mdl.bits
let dl0 = 0.

let dl_round dl = Float.round (dl *. 1e9) /. 1e9
        
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
       | `Index -> dl_int_index n
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

  | `Many (ordered,items) ->
     0. (* TODO: encode ordered when length > 1 *)
     +. Mdl.Code.list_plus
          (fun item -> dl ~ctx item ~path:(ith_item 0 path)) (* exact item index does not matter here *)
          items

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
  let rec aux ~ctx ~path d =
    dl_patt_as_template (* NOTE: to align with dl_template on patterns *)
    +. dl_patt aux ~ctx ~path d
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

(* OLD version of dl_path

let path_similarity ~ctx_path v = (* QUICK *)
  (* TODO: revise in depth, make generic, care about expressions mixing various kinds *)
  let rec aux lp1' lp2' =
    match lp1', lp2' with
    | [], [] -> 2.
    | `Root::lp1, _ -> aux lp1 lp2'
    | _, `Root::lp2 -> aux lp1' lp2
    | `Field (`Shape,p1)::lp1, ([] | `Field (`Layer _,_)::_) -> aux (p1::lp1) lp2'
    | ([] | `Field (`Layer _,_)::_), `Field (`Shape,p2)::lp2 -> aux lp1' (p2::lp2)
    | `Field (f1,p1)::lp1, `Field (f2,p2)::lp2 -> aux_field f1 f2 *. aux (p1::lp1) (p2::lp2)
    | `Field (`Layer _, p1)::lp1, [] -> 0.75 *. aux (p1::lp1) []
    | `Field (_,p1)::lp1, [] -> 0.5 *. aux (p1::lp1) []
    | [], `Field (`Layer _, p2)::lp2 -> 0.75 *. aux [] (p2::lp2)
    | [], `Field (_,p2)::lp2 -> 0.5 *. aux [] (p2::lp2)
    | `Arg (_,_,p1)::lp1, _ -> aux (p1::lp1) lp2' (* TODO: should be refined *)
    | `Item (_,q1,ctx1)::lp1, `Item (_,q2,ctx2)::lp2 -> aux (q1::ctx1::lp1) (q2::ctx2::lp2)
    | `Item (_,q1,ctx1)::lp1, _ -> 0.75 *. aux (q1::ctx1::lp1) lp2'
    | _, `Item (_,q2,ctx2)::lp2 -> 0.5 *. aux lp1' (q2::ctx2::lp2)
    | _ ->
       print_string "similarity goal: ";
       pp_path ctx_path; print_string " ~ ";
       pp_path v; print_newline ();
       print_string "failing sub-goal: ";
       pp_path_list lp1'; print_string " ~ ";
       pp_path_list lp2'; print_newline ();
       assert false (* incompatible kinds *)
  and aux_field f1 f2 =
    match f1, f2 with
    | `I, `I -> 1.
    | `I, `J -> 0.5
    | `J, `I -> 0.5
    | `J, `J -> 1.
    | `Color, `Color -> 1.
    | `Mask, `Mask -> 1.
    | `Pos, `Pos -> 1.
    | `Pos, `Size -> 0.5
    | `Size, `Pos -> 0.5
    | `Size, `Size -> 1.
    | `Shape, `Shape -> 1.
    | `Layer l1, `Layer l2 -> aux_ilist l1 l2
    | _ -> 0.1 (* incompatible kind may happen because of expressions mixing diff. kinds *)
(*       print_string (string_of_field f1);
       print_string " ~ ";
       print_string (string_of_field f2);
       print_newline ();
       assert false *)
  and aux_ilist lp1 lp2 =
    if lp1 = lp2 then 1. else 0.8
  in
  aux [ctx_path] [v]
  
let dl_path_among ~(ctx_path : revpath) (vars : revpath list) (x : revpath) : dl =
  Common.prof "Model2.dl_path_among" (fun () ->
  (* DL of identifying x among vars, for use in scope of ctx_path *)
  let total_w, x_w =
    List.fold_left
      (fun (total_w, x_w) v ->
        let sim = path_similarity ~ctx_path v in
        let w = exp sim in
        total_w +. w, (if v=x then w else x_w))
      (0.,0.) vars in
  if x_w = 0. || total_w = 0. (* happens when unify generalizes some path, removing sub-paths *)
  then Stdlib.infinity
  else Mdl.Code.usage (x_w /. total_w))


let dl_path ~(env_sig : signature) ~(ctx_path : revpath) (x : revpath) : dl =
  let k = path_kind x in
  match List.assoc_opt k env_sig with
  | Some vars -> dl_path_among ~ctx_path vars x
  | None -> Stdlib.infinity (* invalid model, TODO: happens when unify generalizes some path, removing sub-paths *)

 OLD version of dl_path *)

(* functions for computing the DL of paths, used as references
   - the coding takes into account the context of use (kind and role)
   - the coding takes into account the similarity between the reference path and the context path
   - the coding is stable, it does not change when the environment signature is extended (unlike the previous solution)
 *)
  
let rec dl_path_length : revpath -> int = function
  | `Root -> 0
  | `Field (`Layer lp, p1) -> 1 + dl_ilist_path_length lp + dl_path_length p1
  | `Field (_, p1) -> 1 + dl_path_length p1
  | `Arg (_,_,p1) -> dl_path_length p1 (* expr args ignored *)
  | `Item (_,p1,p2) -> dl_path_length p1 + dl_path_length p2
and dl_ilist_path_length = function
  | `Root -> 0
  | `Left p1 -> 1 + dl_ilist_path_length p1
  | `Right p1 -> 1 + dl_ilist_path_length p1
          
let rec dl_path_role (role : role) (p : revpath) : dl =
  (* assuming [p] does not contain [`Arg] *)
  (* assuming the length of [p] is known => no need to code for `Root *)
  match role with
  | `Int (rij,rvec) -> dl_path_int rij rvec  p
  | `Index -> raise TODO
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
     Mdl.Code.usage (if ij = rij then 0.75 else 0.25)
     +. dl_path_vec rvec p1
  | _ -> assert false
and dl_path_color = function
  | `Root -> 0.
  | `Field (`Color, p1) -> dl_path_grid_shape p1
  | _ -> assert false
and dl_path_mask = function
  | `Root -> 0.
  | `Field (`Mask, p1) -> dl_path_shape p1
  | _ -> assert false
and dl_path_vec (rvec : role_vec) = function
  | `Root -> 0.
  | `Field ((`Pos|`Size as f), p1) ->
     let dl_choice =
       match rvec, f with
       | `Pos, `Pos 
         | `Size _, `Size -> Mdl.Code.usage 0.9
       | _ -> Mdl.Code.usage 0.1 in
     ( match f with
       | `Pos -> dl_choice +. dl_path_layer p1
       | `Size -> dl_choice +. dl_path_grid_shape p1 )
  | _ -> assert false
and dl_path_shape = function
  | `Root -> 0.
  | `Field (`Shape, p1) -> dl_path_layer p1
  | _ -> assert false
and dl_path_grid_shape = function
  | `Root -> 0.
  | `Field (`Shape, p1) -> dl_path_layer p1
  | _ -> assert false
and dl_path_layer = function
  | `Root -> 0.
  | `Field (`Layer lp, p1) ->
     Mdl.Code.universal_int_star (dl_ilist_path_length lp)
     +. dl_ilist_path lp
     +. dl_path_grid p1
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
              (* `Norm `X; `Diag1 (`X,2); `Diag2 (`X,2);*)
              `Index; `Indexing (`X,`X) ])
    ~bool:(uniform_among [])
    ~color:(uniform_among [])
    ~mask:(uniform_among [
               `ScaleUp (`X,2); `ScaleTo (`X,`X);
               `Tiling (`X,1,1); `ResizeAlikeTo (`X,`X);
               `ApplySym (`FlipHeight, `X, `Mask);
               `UnfoldSym (sym_matrix_flipHeightWidth, `X);
               `LogAnd (`X,`X); `LogOr (`X,`X); `LogXOr (`X,`X);
               `LogAndNot (`X,`X); `LogNot `X;
               `Indexing (`X,`X) ])
    ~vec:(uniform_among [
              `ProjI `X; `ProjJ `X;
              `ConstVec (0,0);
              `Plus (`X,`X); `Minus (`X,`X);
              `IncrVec (`X,1,1); `DecrVec (`X,1,1);
              `ScaleUp (`X,2); `ScaleDown (`X,2);
              `Tiling (`X,1,1);
              `Corner (`X,`X); `Min [`X; `X]; `Max [`X;`X];`Average [`X;`X]; `Span (`X,`X);
              `TranslationOnto (`X,`X);
              `TranslationSym (`FlipHeight,`X,`X);
              `Indexing (`X,`X) ])
    ~shape:(uniform_among [
                `ScaleUp (`X,2); `ScaleTo (`X,`X);
                `Tiling (`X,1,1); `ResizeAlikeTo (`X,`X);
                `ApplySym (`FlipHeight, `X, `Shape);
                `UnfoldSym (sym_matrix_flipHeightWidth, `X);
                `Coloring (`X,`X);
                `Indexing (`X,`X) ])
    ~object_:(uniform_among [
                 `Indexing (`X,`X) ])
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
  | `Index ->
     code_expr
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
  | `Indexing (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some `Index, path)) e2

type code_template = (* dls must correspond to a valid prob distrib *)
  { c_u : dl;
    c_repeat : dl;
    c_for : dl;
    c_patt : dl;
    c_ref : dl;
    c_expr : dl }
let code_template0 =
  { c_u = Mdl.Code.usage 0.1;
    c_repeat = infinity;
    c_for = infinity;
    c_patt = dl_patt_as_template (* Mdl.Code.usage 0.2 *);
    c_ref = Mdl.Code.usage 0.5;
    c_expr = Mdl.Code.usage 0.2 }

let code_template_by_kind : code_template KindMap.t =
  KindMap.make
    ~int:code_template0
    ~bool:code_template0
    ~color:code_template0
    ~mask:code_template0
    ~vec:code_template0
    ~shape:code_template0
    ~object_:code_template0
    ~layer:{ code_template0 with
      c_repeat = Mdl.Code.usage 0.05;
      c_for = Mdl.Code.usage 0.05;
      c_ref = Mdl.Code.usage 0.5;
      c_expr = Mdl.Code.usage 0.1 }
    ~grid:code_template0
    
let dl_template ~(env_sig : signature) ~(ctx : dl_ctx) ?(path = `Root) (t : template) : dl =
  Common.prof "Model2.dl_template" (fun () ->
  let rec aux ~ctx ~path t =
    let k = path_kind path in
    let code = KindMap.find k code_template_by_kind in
    match t with
    | `U ->
       code.c_u
    | `Repeat patt1 ->
       code.c_repeat
       +. dl_patt aux ~ctx ~path:(any_item path) patt1
    | `For (p_many,t1) ->
       code.c_for
       +. dl_path ~env_sig ~ctx_path:path p_many
       +. aux ~ctx ~path:(any_item path) t1
    | #patt as patt ->
       code.c_patt
       +. dl_patt aux ~ctx ~path patt
    | `Ref p ->
       code.c_ref
       +. dl_path ~env_sig ~ctx_path:path p
    | #expr as e ->
       code.c_expr
       +. dl_expr aux ~env_sig ~ctx ~path e
  in
  aux ~ctx ~path t)

    
let dl_data_given_patt
      (dl : ctx:dl_ctx -> ?path:revpath -> 'a -> data -> dl)
      ~ctx ~(path : revpath) (patt : 'a patt) (d : data) : dl =
  match patt, d with
  | `Int _, `Int _ -> 0.
  | `Color _, `Color _ -> 0.
  | `Mask _, `Mask _ -> 0.
  | `Vec (i,j), `Vec (di,dj) ->
     dl ~ctx i di ~path:(path ++ `I)
     +. dl ~ctx j dj ~path:(path ++ `J)
  | `Point (color), `Point (dcolor) ->
     dl ~ctx color dcolor ~path:(path ++ `Color)
  | `Rectangle (size,color,mask), `Rectangle (dsize,dcolor,dmask) ->
     dl ~ctx size dsize ~path:(path ++ `Size)
     +. dl ~ctx color dcolor ~path:(path ++ `Color)
     +. dl ~ctx mask dmask ~path:(path ++ `Mask)
  | `PosShape (pos,shape), `PosShape (dpos,dshape) ->
     dl ~ctx pos dpos ~path:(path ++ `Pos)
     +. dl ~ctx shape dshape ~path:(path ++ `Shape)
  | `Background (size,color,layers), `Background (dsize,dcolor,dlayers) ->
     dl ~ctx size dsize ~path:(path ++ `Size)
     +. dl ~ctx color dcolor ~path:(path ++ `Color)
     +. fold2_ilist
          (fun sum lp shape dshape -> sum +. dl ~ctx shape dshape ~path:(path ++ `Layer lp))
          0. `Root layers dlayers
  | `Many (ordered,items), `Many (dordered,ditems) ->
     assert (ordered = dordered);
     assert (List.length items = List.length ditems);
     List.fold_left2
       (fun res item ditem -> res +. dl ~ctx ~path:(any_item path) item ditem)
       0. items ditems
  | _ -> assert false (* data inconsistent with pattern *)
    
let rec dl_data_given_template ~(ctx : dl_ctx) ?(path = `Root) (t : template) (d : data) : dl = (* cannot be profiled because of indirect recursion *)
  match t, d with
  | `U, _ -> dl_data ~ctx ~path d
  | `Repeat patt1, `Many (false,items) ->
     Mdl.Code.list_plus (fun item ->
         dl_data_given_patt dl_data_given_template ~ctx ~path:(any_item path) patt1 item)
       items
  | `Repeat _, _ -> assert false (* only parses into unordered collections *)
  | `For _, _ -> assert false (* should have been evaluated out *)
  | #patt as patt, _ -> dl_data_given_patt dl_data_given_template ~ctx ~path patt d
  | #expr, _ -> 0. (* will be evaluated out *)
              
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

let dl_delta_path = any_item (`Field (`Layer `Root, `Root)) (* dummy path with kind Shape *)
let dl_delta_shape = `PosShape (`Vec (`Int 0, `Int 0), `Point (`Color Grid.blue)) (* dummy point shape *)
let dl_delta ~(ctx : dl_ctx) (delta : delta) : dl = (* QUICK *)
  if delta = []
  then 0.
  else (* assuming dl_data is constant over point shapes *)
    let n = List.length delta in
    -. 1. (* some normalization to get 0 for empty grid data *)
    +. Mdl.Code.universal_int_star n
    +. float n *. dl_data ~ctx ~path:dl_delta_path dl_delta_shape
  (*
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
exception Out_of_bound of template list * int
exception Negative_integer
let _ =
  Printexc.register_printer
    (function
     | Unbound_var v -> Some ("unbound variable: " ^ string_of_var v)
     | Invalid_expr e -> Some ("invalid expression: " ^ string_of_expr xp_template e)
     | Undefined_result msg -> Some ("undefined expression: " ^ msg)
     | Out_of_bound (items,i) -> Some ("out of bound indexing: " ^ string_of_template (`Many (false,items)) ^ "[" ^ string_of_int i ^ "]")
     | Negative_integer -> Some ("negative integer")
     | _ -> None)
  
type apply_lookup = var -> data option

let lookup_of_env (env : data) : apply_lookup =
  fun v ->
  match v with
  | #revpath as p -> find_data p env
  | `Index -> raise (Unbound_var v)

let apply_patt
      (apply : lookup:apply_lookup -> revpath -> 'a -> 'b)
      ~(lookup : apply_lookup) (p : revpath) : 'a patt -> 'b patt = function
  (* SHOULD NOT use [p] *)
  | (`Bool _ | `Int _ | `Color _ | `Mask _ as d) -> d
  | `Vec (i,j) ->
     `Vec (apply ~lookup (p ++ `I) i,
           apply ~lookup (p ++ `J) j)
  | `Point (color) ->
     `Point (apply ~lookup (p ++ `Color) color)
  | `Rectangle (size,color,mask) ->
     `Rectangle (apply ~lookup (p ++ `Size) size,
                 apply ~lookup (p ++ `Color) color,
                 apply ~lookup (p ++ `Mask) mask)
  | `PosShape (pos,shape) ->
     `PosShape (apply ~lookup (p ++ `Pos) pos,
                apply ~lookup (p ++ `Shape) shape)
  | `Background (size,color,layers) ->
     `Background (apply ~lookup (p ++ `Size) size,
                  apply ~lookup (p ++ `Color) color,
                  map_ilist
                    (fun lp shape -> apply ~lookup (p ++ `Layer lp) shape)
                    `Root layers)
  | `Many (ordered,items) ->
     `Many (ordered,
            List.map
              (fun item -> apply ~lookup (any_item p) item)
              items)

(* let flip_size__f_sym : symmetry -> bool * (Grid.Mask_model.t -> Grid.Mask_model.t) =
  function
  | `Id -> false, (fun mm -> mm)
  | `FlipHeight -> false, Grid.Mask_model.flipHeight
  | `FlipWidth -> false, Grid.Mask_model.flipWidth
  | `FlipDiag1 -> true, Grid.Mask_model.flipDiag1
  | `FlipDiag2 -> true, Grid.Mask_model.flipDiag2
  | `Rotate180 -> false, Grid.Mask_model.rotate180
  | `Rotate90 -> true, Grid.Mask_model.rotate90
  | `Rotate270 -> true, Grid.Mask_model.rotate270 *)

let apply_symmetry ~lookup (sym : symmetry) (role_e1 : role) e d1 = (* : template expr -> template -> template = *)
  (* let flip_size, sym_mask_model = flip_size__f_sym sym in *)
  let sym_pos d = (* symmetry of a point relative to the grid *)
    let p_grid_size = `Field (`Size, `Root) in
    match lookup p_grid_size, d with (* getting the grid size *)
    | Some (`Vec (`Int h, `Int w)), `Vec (`Int i, `Int j) ->
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
    | None, _ -> raise (Unbound_var p_grid_size)
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
       let mm' =
         match sym with
         | `Id -> mm
         | `FlipHeight -> Grid.Mask_model.flipHeight mm
         | `FlipWidth -> Grid.Mask_model.flipWidth mm
         | `FlipDiag1 -> Grid.Mask_model.flipDiag1 mm
         | `FlipDiag2 -> Grid.Mask_model.flipDiag2 mm
         | `Rotate180 -> Grid.Mask_model.rotate180 mm
         | `Rotate90 -> Grid.Mask_model.rotate90 mm
         | `Rotate270 -> Grid.Mask_model.rotate270 mm in
       `Mask mm'
    | _ -> assert false in
  let sym_shape = function
    | `Point col -> `Point col
    | `Rectangle (size, col, mask) ->
       `Rectangle (sym_size size, col, sym_mask mask)
    | _ -> assert false
  in
  match role_e1, d1 with
  | `Vec `Pos, _ -> sym_pos d1
  | `Vec (`Size _), _ -> sym_size d1
  | `Vec `Move, _ -> sym_move d1
  | `Mask, _ -> sym_mask d1
  | `Shape, _ -> sym_shape d1
  | `Layer, `PosShape (pos, shape) -> `PosShape (pos, sym_shape shape) (* NOTE: do not use sym_pos because pos in PosShape must be the top-left corner of the shape, see def of TranslationSym *)
  | _ -> raise (Invalid_expr e)

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
    | `Mask m -> `Mask (mask_matrix m sym_matrix)
    | `Full _ -> `Full false
    | _ -> raise (Undefined_result "Model2.unfold_symmetry: not a custom mask")  
  in
  fun e d ->
  match d with
  | `Mask mm -> `Mask (unfold_mask mm)
  | `Rectangle (size, col, `Mask mm) ->
     `Rectangle (unfold_size size, col, `Mask (unfold_mask mm))
  | `Point _ -> raise (Undefined_result "Model2.unfold_symmetry: point")
  | `PosShape (pos, `Rectangle (size, col, `Mask mm)) ->
     `PosShape (pos, `Rectangle (unfold_size size, col, `Mask (unfold_mask mm)))
  | `PosShape (_, `Point _) -> raise (Undefined_result "Model2.unfold_symmetry: point")
  | _ -> raise (Invalid_expr e)
    
let apply_expr_gen
          (apply : lookup:apply_lookup -> revpath -> 'a -> template)
          ~(lookup : apply_lookup) (p : revpath) (e : 'a expr) : template = (* QUICK *)
  (* SHOULD NOT use [p] *)
  match e with
  | `Ref p ->
     (match lookup (p :> var) with
      | Some d -> (d :> template)
      | None -> raise (Unbound_var (p :> var)))
  | `Index as v ->
     (match lookup (v :> var) with
      | Some d -> (d :> template)
      | None -> raise (Unbound_var (v :> var)))
  | `ConstInt k -> `Int k
  | `ConstVec (k,l) -> `Vec (`Int k, `Int l)
  | `Plus (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Int i1, `Int i2 -> `Int (i1 + i2)
      | `Vec (`Int i1, `Int j1), `Vec (`Int i2, `Int j2) -> `Vec (`Int (i1+i2), `Int (j1+j2))
      | _ -> raise (Invalid_expr e))
  | `Minus (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Int i1, `Int i2 -> `Int (i1-i2)
      | `Vec (`Int i1, `Int j1), `Vec (`Int i2, `Int j2) -> `Vec (`Int (i1-i2), `Int (j1-j2))
      | _ -> raise (Invalid_expr e))
  | `IncrInt (e1,k) ->
     (match apply ~lookup p e1 with
      | `Int i1 -> `Int (i1 + k)
      | _ -> raise (Invalid_expr e))
  | `DecrInt (e1,k) ->
     (match apply ~lookup p e1 with
      | `Int i1 -> `Int (i1 - k)
      | _ -> raise (Invalid_expr e))
  | `IncrVec (e1,k,l) ->
     (match apply ~lookup p e1 with
      | `Vec (`Int i1, `Int j1) -> `Vec (`Int (i1 + k), `Int (j1 + l))
      | _ -> raise (Invalid_expr e))
  | `DecrVec (e1,k,l) ->
     (match apply ~lookup p e1 with
      | `Int i1 -> `Int (i1 - k)
      | `Vec (`Int i1, `Int j1) -> `Vec (`Int (i1 - k), `Int (j1 - l))
      | _ -> raise (Invalid_expr e))
  | `Modulo (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Int i1, `Int i2 -> `Int (i1 mod i2)
      | _ -> raise (Invalid_expr e))
  | `ScaleUp (e1,k) ->
     (match apply ~lookup p e1 with
      | `Int i1 -> `Int (i1 * k)
      | `Vec (`Int i1, `Int j1) -> `Vec (`Int (i1 * k), `Int (j1 * k))
      | `Mask mm -> `Mask (Grid.Mask_model.scale_up k k mm)
      | `Point col ->
         `Rectangle (`Vec (`Int k, `Int k), col, `Mask (`Full false))
      | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm) ->
         `Rectangle (`Vec (`Int (h * k), `Int (w * k)), col, `Mask (Grid.Mask_model.scale_up k k mm))
      | _ -> raise (Invalid_expr e))
  | `ScaleDown (e1,k) ->
     (match apply ~lookup p e1 with
      | `Int i1 ->
         let rem = i1 mod k in
         if rem = 0 || rem = k - 1 (* account for separators *)
         then `Int (i1 / k)
         else raise (Undefined_result "ScaleDown: not an integer")
      | `Vec (`Int i1, `Int j1) ->
         let remi, remj = i1 mod k, j1 mod k in
         if remi = remj && (remi = 0 || remi = k-1) (* account for separators *)
         then `Vec (`Int (i1 / k), `Int (j1 / k))
         else raise (Undefined_result "ScaleDown: not an integer")
      | _ -> raise (Invalid_expr e))
  | `ScaleTo (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Mask mm, `Vec (`Int new_h, `Int new_w) -> `Mask (Grid.Mask_model.scale_to new_h new_w mm)
      | `Point col, `Vec (`Int new_h, `Int new_w) ->
         `Rectangle (`Vec (`Int new_h, `Int new_w), col, `Mask (`Full false))
      | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm), `Vec (`Int new_h, `Int new_w) ->
         `Rectangle (`Vec (`Int new_h, `Int new_w), col, `Mask (Grid.Mask_model.scale_to new_h new_w mm))
      | _ -> raise (Invalid_expr e))
  | `Corner (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Vec (`Int i1, `Int j1), `Vec (`Int i2, `Int j2) ->
         if i1 <> i2 && j1 <> j2
         then `Vec (`Int i1, `Int j2)
         else raise (Undefined_result "Corner: vectors on same row/column")
      | _ -> raise (Invalid_expr e))
  | `Min le1 ->
     le1
     |> List.map (fun e1 -> apply ~lookup p e1)
     |> List.fold_left
          (fun (is_int,is_vec,mini,minj) -> function
            | `Int i -> (true, is_vec, min i mini, minj)
            | `Vec (`Int i, `Int j) -> (is_int, true, min i mini, min j minj)
            | _ -> raise (Invalid_expr e))
          (false, false, max_int, max_int)
     |> (fun (is_int,is_vec,mini,minj) ->
      match is_int, is_vec with
      | true, false -> `Int mini
      | false, true -> `Vec (`Int mini, `Int minj)
      | _ -> assert false)
  | `Max le1 ->
     le1
     |> List.map (fun e1 -> apply ~lookup p e1)
     |> List.fold_left
          (fun (is_int,is_vec,maxi,maxj) -> function
            | `Int i -> (true, is_vec, max i maxi, maxj)
            | `Vec (`Int i, `Int j) -> (is_int, true, max i maxi, max j maxj)
            | _ -> raise (Invalid_expr e))
          (false, false, min_int, min_int)
     |> (fun (is_int,is_vec,maxi,maxj) ->
      match is_int, is_vec with
      | true, false -> `Int maxi
      | false, true -> `Vec (`Int maxi, `Int maxj)
      | _ -> assert false)
  | `Average le1 ->
     le1
     |> List.map (fun e1 -> apply ~lookup p e1)
     |> List.fold_left
          (fun (is_int,is_vec,n,sumi,sumj) -> function
            | `Int i -> (true, is_vec, n+1, sumi+i, sumj)
            | `Vec (`Int i, `Int j) -> (is_int, true, n+1, sumi+i, sumj+j)
            | _ -> raise (Invalid_expr e))
          (false, false, 0, 0, 0)
     |> (fun (is_int,is_vec,n,sumi,sumj) ->
      match is_int, is_vec with
      | true, false ->
         if sumi mod n = 0
         then `Int (sumi / n)
         else raise (Undefined_result "Average: not an integer")
      | false, true ->
         if sumi mod n = 0 && sumj mod n = 0
         then `Vec (`Int (sumi / n), `Int (sumj / n))
         else raise (Undefined_result "Average: not an integer")
      | _ -> assert false) (* empty or ill-typed list *)
  | `Span (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Int i1, `Int i2 ->
         if i1=i2
         then raise (Undefined_result "Span: same int")
         else `Int (abs (i2-i1) + 1)
      | `Vec (`Int i1, `Int j1), `Vec (`Int i2, `Int j2) ->
         if i1=i2 && j1=j2
         then raise (Undefined_result "Span: same vector")
         else `Vec (`Int (abs (i2-i1) + 1), `Int (abs (j2-j1) + 1))
      | _ -> raise (Invalid_expr e))
  | `Norm e1 ->
     (match apply ~lookup p e1 with
      | `Vec (`Int i, `Int j) -> `Int (i+j)
      | _ -> raise (Invalid_expr e))
  | `Diag1 (e1,k) ->
     (match apply ~lookup p e1 with
      | `Vec (`Int i, `Int j) -> `Int ((i+j) mod k)
      | _ -> raise (Invalid_expr e))
  | `Diag2 (e1,k) ->
     (match apply ~lookup p e1 with
      | `Vec (`Int i, `Int j) -> `Int ((i-j) mod k)
      | _ -> raise (Invalid_expr e))
  | `LogAnd (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Mask m1, `Mask m2 ->
         (match m1, m2 with
          | `Full _, _ -> `Mask m2
          | _, `Full _ -> `Mask m1
          | `Mask bm1, `Mask bm2 when Grid.Mask.same_size bm1 bm2 ->
             `Mask (`Mask (Grid.Mask.inter bm1 bm2))
          | _ -> raise (Undefined_result "LogAnd: undefined"))
      | _ -> raise (Invalid_expr e))
  | `LogOr (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Mask m1, `Mask m2 ->
         (match m1, m2 with
          | `Full _, _ -> `Mask m1
          | _, `Full _ -> `Mask m2
          | `Mask bm1, `Mask bm2 when Grid.Mask.same_size bm1 bm2 ->
             `Mask (`Mask (Grid.Mask.union bm1 bm2))
          | _ -> raise (Undefined_result "LogOr: undefined"))
      | _ -> raise (Invalid_expr e))
  | `LogXOr (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Mask m1, `Mask m2 ->
         (match m1, m2 with
          | `Full _, `Mask bm2 -> `Mask (`Mask (Grid.Mask.compl bm2))
          | `Mask bm1, `Full _ -> `Mask (`Mask (Grid.Mask.compl bm1))
          | `Mask bm1, `Mask bm2 when Grid.Mask.same_size bm1 bm2 ->
             `Mask (`Mask (Grid.Mask.diff_sym bm1 bm2))
          | _ -> raise (Undefined_result "LogXOr: undefined"))
      | _ -> raise (Invalid_expr e))
  | `LogAndNot (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Mask m1, `Mask m2 ->
         (match m1, m2 with
          | `Full _, `Mask bm2 -> `Mask (`Mask (Grid.Mask.compl bm2))
          | `Mask bm1, `Mask bm2 when Grid.Mask.same_size bm1 bm2 ->
             `Mask (`Mask (Grid.Mask.diff bm1 bm2))
          | _ -> raise (Undefined_result "LogAndNot: undefined"))
      | _ -> raise (Invalid_expr e))
  | `LogNot e1 ->
     (match apply ~lookup p e1 with
      | `Mask m1 ->
         (match m1 with
          | `Mask bm1 -> `Mask (`Mask (Grid.Mask.compl bm1))
          | _ -> raise (Undefined_result "LogNot: undefined"))
      | _ -> raise (Invalid_expr e))
  | `Area e1 ->
     (match apply ~lookup p e1 with
      | `Point _ -> `Int 1
      | `Rectangle (`Vec (`Int height, `Int width), _, `Mask m) ->
         `Int (Grid.Mask_model.area ~height ~width m)
      | _ -> raise (Invalid_expr e))
  | `Left e1 ->
     (match apply ~lookup p e1 with
      | `PosShape (`Vec (_, `Int j), `Rectangle _) -> `Int j
      | `PosShape _ -> raise (Undefined_result "Left: not a rectangle")
      | _ -> raise (Invalid_expr e))
  | `Right e1 ->
     (match apply ~lookup p e1 with
      | `PosShape (`Vec (_, `Int j), `Rectangle (`Vec (_, `Int w), _, _)) -> `Int (j+w-1)
      | `PosShape _ -> raise (Undefined_result "Right: not a rectangle")
      | _ -> raise (Invalid_expr e))
  | `Center e1 ->
     (match apply ~lookup p e1 with
      | `PosShape (`Vec (_, `Int j), `Rectangle (`Vec (_, `Int w), _, _)) ->
         if w mod 2 = 0
         then raise (Undefined_result "Center: no center, even width")
         else `Int (j + w/2 + 1)
      | `PosShape _ -> raise (Undefined_result "Center: not a rectangle")
      | _ -> raise (Invalid_expr e))
  | `Top e1 ->
     (match apply ~lookup p e1 with
      | `PosShape (`Vec (`Int i, _), `Rectangle _) -> `Int i
      | `PosShape _ -> raise (Undefined_result "Top: not a rectangle")
      | _ -> raise (Invalid_expr e))
  | `Bottom e1 ->
     (match apply ~lookup p e1 with
      | `PosShape (`Vec (`Int i, _), `Rectangle (`Vec (`Int h, _), _, _)) -> `Int (i+h-1)
      | `PosShape _ -> raise (Undefined_result "Bottom: not a rectangle")
      | _ -> raise (Invalid_expr e))
  | `Middle e1 ->
     (match apply ~lookup p e1 with
      | `PosShape (`Vec (`Int i, _), `Rectangle (`Vec (`Int h, _), _, _)) ->
         if h mod 2 = 0
         then raise (Undefined_result "Middle: no middle, even height")
         else `Int (i + h/2 + 1)
      | `PosShape _ -> raise (Undefined_result "Middle: not a rectangle")
      | _ -> raise (Invalid_expr e))
  | `ProjI e1 ->
     (match apply ~lookup p e1 with
      | `Vec (`Int i, _) -> `Vec (`Int i, `Int 0)
      | _ -> raise (Invalid_expr e))
  | `ProjJ e1 ->
     (match apply ~lookup p e1 with
      | `Vec (_, `Int j) -> `Vec (`Int 0, `Int j)
      | _ -> raise (Invalid_expr e))         
  | `TranslationOnto (e1,e2) ->
     let d1 = apply ~lookup p e1 in
     let d2 = apply ~lookup p e2 in
     (match get_pos d1, get_size d1, get_pos d2, get_size d2 with
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
         `Vec (`Int ti, `Int tj)
      | _ -> raise (Invalid_expr e) )
  | `Tiling (e1,k,l) ->
     (match apply ~lookup p e1 with
      | `Vec (`Int h, `Int w) -> `Vec (`Int (h*k), `Int (w*l))
      | `Mask mm -> `Mask (Grid.Mask_model.tile k l mm)
      | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm) -> 
         `Rectangle (`Vec (`Int (h*k), `Int (w*l)), col, `Mask (Grid.Mask_model.tile k l mm))
      | `Point _ -> raise (Undefined_result "Tiling: undefined on points")
      | _ -> raise (Invalid_expr e))
  | `ResizeAlikeTo(e1,e2) ->
     (match apply ~lookup p e2 with
      | `Vec (`Int h, `Int w) ->
         let aux = function
           | `Point col ->
              `Rectangle (`Vec (`Int h, `Int w), col, `Mask (`Full false))
           | `Rectangle (_size, col, `Mask mm) ->
              `Rectangle (`Vec (`Int h, `Int w), col, `Mask (Grid.Mask_model.resize_alike h w mm))
           | _ -> assert false in             
         (match apply ~lookup p e1 with
          | `Mask mm -> `Mask (Grid.Mask_model.resize_alike h w mm)
          | (`Point _ | `Rectangle _ as shape) -> aux shape
          | `PosShape (pos,shape) -> `PosShape (pos, aux shape)
          | _ -> raise (Invalid_expr e))
      | _ -> raise (Invalid_expr e))
  | `ApplySym (sym,e1,role_e1) ->
     apply ~lookup p e1
     |> apply_symmetry ~lookup sym role_e1 e
  | `UnfoldSym (sym_matrix,e1) ->
     apply ~lookup p e1
     |> unfold_symmetry sym_matrix e
  | `TranslationSym (sym,e1,e2) ->
     let d1 = apply ~lookup p e1 in
     let d2 = apply ~lookup p e2 in
     (match get_pos d1, get_size d1, get_pos d2, get_size d2 with
      | Some (mini1,minj1), Some (h1,w1), Some (mini2,minj2), Some (h2,w2) ->
         let ti, tj =
           match sym with
           | `Id -> 0, 0
           | `FlipHeight -> 2 * (mini2-mini1) + (h2-h1), 0
           | `FlipWidth -> 0, 2 * (minj2-minj1) + (w2-w1)
           | `Rotate180 -> 2 * (mini2-mini1) + (h2-h1), 2 * (minj2-minj1) + (w2-w1)
           | `FlipDiag1 ->
              if h2 = w2
              then
                let ti = (mini2 - mini1) - (minj2 - minj1) (* + (h2 - w2) / 2 *) in
                ti, - ti
              else raise (Undefined_result "TranslationSym: FlipDiag1: non-square pivot object")
           | `FlipDiag2 ->
              if h2 = w2 && (h2 - h1 + w2 - w1 mod 2 = 0)
              then
                let ti = (mini2 - mini1) + (minj2 - minj1) + (h2 - h1 + w2 - w1) / 2 in
                ti, - ti
              else raise (Undefined_result "TranslationSym: FlipDiag2: non-square pivot object")
           | `Rotate90 ->
              if h2 = w2
              then
                (mini2 - mini1) - (minj2 - minj1) (* + (h2 - w2) / 2 *),
                (mini2 - mini1) + (minj2 - minj1) + (h2 + w2) / 2 - h1 (* /2 OK because h2=w2 *)
              else raise (Undefined_result "TranslationSym: Rotate90: non-square pivot object")
           | `Rotate270 ->
              if h2 = w2
              then
                (minj2 - minj1) + (mini2 - mini1) + (h2 + w2) / 2 - w1 (* /2 OK because h2=w2 *),
                (minj2 - minj1) - (mini2 - mini1) (* - (h2 - w2) / 2 *)
              else raise (Undefined_result "TranslationSym: Rotate90: non-square pivot object")
         in
         `Vec (`Int ti, `Int tj)
      | _ -> raise (Invalid_expr e) )
  | `Coloring (e1,e2) ->
     (match apply ~lookup p e2 with
      | (`Color c as new_col) ->
         let aux = function
           | `Point _ -> `Point new_col
           | `Rectangle (size, _, mask)  -> `Rectangle (size, new_col, mask)
           | _ -> raise (Invalid_expr e) in
         (match apply ~lookup p e1 with
         | `PosShape (pos, shape) -> `PosShape (pos, aux shape)
         | d -> aux d)
      | _ -> raise (Invalid_expr e))
  | `Indexing (`Ref (`Item (None,local,ctx)), e2) ->
     (match lookup (ctx :> var), apply ~lookup p e2 with
      | Some (`Many (ordered,items)), `Int i ->
         (match List.nth_opt items i with
          | Some item ->
             (match find_data local item with
              | Some d -> (d :> template)
              | None -> raise (Invalid_expr e))
          | None -> raise (Out_of_bound ((items :> template list),i)))
      | _ -> raise (Invalid_expr e))
  | `Indexing (e1,e2) -> (* TODO: optimize for e1 = `Ref (`Item _) *)
     (match apply ~lookup p e1, apply ~lookup p e2 with (* TODO: what should be 'p' for e2? index? *)
      | `Many (ordered,items), `Int i ->
         (match List.nth_opt items i with
          | Some t -> t
          | None -> raise (Out_of_bound (items,i)))
      | _ -> raise (Invalid_expr e))

let apply_expr apply ~(env : data) e =
  apply_expr_gen apply ~lookup:(lookup_of_env env) `Root e
  
let rec apply_template_gen ~(lookup : apply_lookup) (p : revpath) (t : template) : template = (* QUICK *)
  (* SHOULD NOT use [p] *)
  match t with
  | `U -> `U
  | `Repeat patt1 ->
     `Repeat (apply_patt apply_template_gen ~lookup (any_item p) patt1)
  | `For (p_many,e1) ->
     (match lookup (p_many :> var) with
      | Some (`Many (ordered,items)) ->
         let lookup_index i = (* adding a binding for index *)
           function
           | `Index -> Some (`Int i)
(*           | #revpath as p ->
              match path_split_any_item p with
              | Some (local,ctx) when ctx = p_many ->
                 let p_item = `Item (Some i, local, ctx) in (* pointing at the i-th item *)
                 lookup p_item *)
           | p -> lookup p in
         let items_e1 =
           let p1 = any_item p in
           List.mapi
             (fun i _item -> apply_template_gen ~lookup:(lookup_index i) p1 e1) (* TODO: use directly item in lookup functions *)
             items in
         `Many (ordered, items_e1) (* TODO: how to decide on ordered? add to `For construct? *)
      | _ -> raise (Unbound_var (p_many :> var)))
  | #patt as patt -> (apply_patt apply_template_gen ~lookup p patt :> template)
  | #expr as e -> apply_expr_gen apply_template_gen ~lookup p e

let rec apply_template ~(env : data) (t : template) : (template,exn) Result.t =
  try Result.Ok (apply_template_gen ~lookup:(lookup_of_env env) `Root t)
  with (* catching runtime error in expression eval *)
  | (Grid.Undefined_result _ as exn) -> Result.Error exn
  | (Unbound_var _ as exn) -> Result.Error exn
  | (Undefined_result _ as exn) -> Result.Error exn
  | (Out_of_bound _ as exn) -> Result.Error exn
  | (Negative_integer  as exn) -> Result.Error exn
(* DO NOT remove path argument, useful in generate_template (through apply_patt) *)


(* grid generation from data and template *)

let rec generate_template (p : revpath) (t : template) : data = (* QUICK *)
  match t with
  | `U -> default_data_of_path p (* default data *)
  | `Repeat patt1 -> apply_patt
                       (fun ~lookup -> generate_template) ~lookup:(fun _ -> assert false)
                       (any_item p) patt1
  | `For _ -> assert false (* should be eliminated by call to apply_template *)
  | #patt as patt -> apply_patt
                       (fun ~lookup -> generate_template) ~lookup:(fun _ -> assert false)
                       p patt
  | #expr -> assert false (* should be eliminated by call to apply_template *)

  
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
  | `Many (ordered,items) ->
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

type 'b parse = ('b * parse_state) Myseq.t (* parsing result *)
type ('a,'b) x_parse = 'a -> parse_state -> 'b parse (* input -> parse *)
type ('a,'b) p_x_parse = revpath -> ('a,'b) x_parse (* path -> input -> parse *)

let parse_empty : ('a,data) p_x_parse =
  fun p x state -> Myseq.empty

let parse_repeat
      (valid_part : 'a -> parse_state -> bool)
      (parse_item : int -> ('a,data) p_x_parse)
    : ('a list, data list) p_x_parse =
  fun p parts state ->
  let rec aux i p_item parts state =
    (fun () -> (* thumbing is key for efficiency *)
      ( match parts with
        | [] -> Myseq.return ([],state)
        | part0::parts1 ->
           if valid_part part0 state
           then
             Myseq.interleave [ (* interleave instead of concat for fairer exploration *)
                 (* taking data0 *)
                 (let* data0, state0 = parse_item i p_item part0 state in
                  let i1 = i+1 in
                  let p_item1 = ith_item i1 p in
                  let* ldata1, state1 = aux i1 p_item1 parts1 state0 in
                  Myseq.return (data0::ldata1, state1));
                 (* skipping data0 *)
                 aux i p_item parts1 state
               ]
           else aux i p_item parts1 state )
        ())
  in
  let res =
    aux 0 (ith_item 0 p) parts state
    |> Myseq.filter_map
         (fun (items,state) ->
           if items = []
           then None
           else
             let state =
               { state with
                 parts = filter_parts_with_mask ~new_mask:state.mask state.parts } in
             Some (items,state)) in
(*
  print_string "parse_repeat[0] = ";
  (match Myseq.hd_opt res with
   | Some (ld,_) -> pp_data (`Many (false,ld))
   | None -> ());
  print_newline ();
 *)
  res

let parse_many
      (parse_item : int -> 'a -> parse_state -> (data * parse_state) Myseq.t)
      (items : 'a list) (state : parse_state)
    : (data list * parse_state) Myseq.t =
  let rec aux i state = function
    | [] -> Myseq.return ([], state)
    | item::items1 ->
       let* ditem, state = parse_item i item state in
       let* ditems1, state = aux (i+1) state items1 in
       Myseq.return (ditem::ditems1, state)
  in
  aux 0 state items (* TODO: apply slice to the keep only the first parse? *)

let rec parse_template
          ~(parse_u : unit -> ('a,data) p_x_parse)
          ?(parse_repeat : template patt -> ('a, data list) p_x_parse = fun _ -> assert false)
          ~(parse_patt : template patt -> ('a,data) p_x_parse)
          (t : template)
        : ('a,data) p_x_parse =
  match t with
  | `U -> parse_u ()
  | `Repeat patt1 ->
     let parse = parse_repeat patt1 in
     (fun p x state ->
       let* items, state = parse p x state in
       assert (items <> []);
       let data = `Many (false, items) in
       Myseq.return (data,state))
  | `For _ -> assert false
  | #patt as patt -> parse_patt patt
  | #expr -> assert false

let parse_bool t : (bool,data) p_x_parse = (* QUICK *)
  parse_template
    ~parse_u:(fun () ->
      fun p b state -> Myseq.return (`Bool b, state))
    ~parse_patt:(function
      | `Bool b0 ->
         fun p b state ->
         if b=b0 then Myseq.return (`Bool b, state)
         else if state.quota_diff > 0 then
           Myseq.return (`Bool b, add_diff p state)
         else Myseq.empty
      | _ -> parse_empty)
    t

let parse_int t : (int,data) p_x_parse = (* QUICK *)
  parse_template
    ~parse_u:(fun () ->
      fun p i state -> Myseq.return (`Int i, state))
    ~parse_patt:(function
      | `Int i0 ->
         fun p i state ->
         if i=i0 then Myseq.return (`Int i, state)
         else if state.quota_diff > 0 then
           Myseq.return (`Int i, add_diff p state)
         else Myseq.empty
      | _ -> parse_empty)
    t

let parse_color t : (Grid.color,data) p_x_parse = (* QUICK *)
  parse_template
    ~parse_u:(fun () ->
      fun p c state -> Myseq.return (`Color c, state))
    ~parse_patt:(function
      | `Color c0 ->
         fun p c state ->
         if c0 = c then Myseq.return (`Color c0, state)
         else if state.quota_diff > 0 then
           Myseq.return (`Color c, add_diff p state)
         else Myseq.empty
      | _ -> parse_empty)
    t
  
let parse_mask t : (Grid.Mask_model.t list, data) p_x_parse = (* QUICK *)
  parse_template
    ~parse_u:
    (fun () ->
      fun p ms state ->
      let* m = Myseq.from_list ms in
      Myseq.return (`Mask m, state))
    ~parse_patt:(function
      | `Mask m0 ->
         fun p ms state ->
         if List.exists (Grid.Mask_model.subsumes m0) ms then
           Myseq.return (`Mask m0, state)
         else if state.quota_diff > 0 then
           let* m = Myseq.from_list ms in
           Myseq.return (`Mask m, add_diff p state)
         else Myseq.empty
      | _ -> parse_empty)
    t

let parse_vec t : (int * int, data) p_x_parse = (* QUICK *)
  parse_template
    ~parse_u:(fun () ->
      fun p (vi,vj) state ->
      Myseq.return (`Vec (`Int vi, `Int vj), state))
    ~parse_patt:(function
      | `Vec (i,j) ->
         let parse_i = parse_int i in
         let parse_j = parse_int j in
         fun p ->
         let parse_i = parse_i (p ++ `I) in
         let parse_j = parse_j (p ++ `J) in
         fun (vi,vj) state ->
         let* di, state = parse_i vi state in
         let* dj, state = parse_j vj state in
         Myseq.return (`Vec (di,dj), state)
      | _ -> parse_empty)
    t

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
  
let rec parse_shape (t : template) : (unit,data) p_x_parse =
  let parse_point pos color : (Grid.pixel, data) p_x_parse =
    let parse_pos = parse_vec pos in
    let parse_color = parse_color color in
    fun p ->
    let parse_pos = parse_pos (p ++ `Pos) in
    let parse_color = parse_color (p ++ `Shape ++ `Color) in
    fun (i,j,c) state ->
    let* dpos, state = parse_pos (i,j) state in
    let* dcolor, state = parse_color c state in
    Myseq.return (`PosShape (dpos, `Point (dcolor)), state) in
  let parse_rectangle pos size color mask : (Grid.rectangle, data) p_x_parse = (* QUICK *)
    let parse_pos = parse_vec pos in
    let parse_size = parse_vec size in
    let parse_color = parse_color color in
    let parse_mask = parse_mask mask in
    fun p ->
    let parse_pos = parse_pos (p ++ `Pos) in
    let parse_size = parse_size (p ++ `Shape ++ `Size) in
    let parse_color = parse_color (p ++ `Shape ++ `Color) in
    let parse_mask = parse_mask (p ++ `Shape ++ `Mask) in
    fun rect state ->
    let open Grid in
    let* dpos, state = parse_pos (rect.offset_i,rect.offset_j) state in
    let* dsize, state = parse_size (rect.height,rect.width) state in
    let* dcolor, state = parse_color rect.color state in
    let* dmask, state = parse_mask rect.mask_models state in
    Myseq.return (`PosShape (dpos, `Rectangle (dsize,dcolor,dmask)), state)
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
  let parse_single_point pos color : (unit,data) p_x_parse =
    let parse_point = parse_point pos color in
    fun p ->
    let parse_point = parse_point p in
    fun () state ->
    let points = Grid.points state.grid state.mask state.parts in
    Myseq.prof "Model2.parse_single_point/seq" (
    let* point = Myseq.from_list points in
    let* dpoint, state = parse_point point state in
    let* state = Myseq.from_option (state_minus_point state point) in
    Myseq.return (dpoint, state)) in
  let parse_single_rectangle pos size color mask : (unit,data) p_x_parse =    
    let parse_rectangle = parse_rectangle pos size color mask in
    fun p ->
    let parse_rectangle = parse_rectangle p in
    fun () state ->
    let rectangles = Grid.rectangles state.grid state.mask state.parts in
    Myseq.prof "Model2.parse_single_rectangle/seq" (
    let* rect = Myseq.from_list rectangles in
    let* drect, state = parse_rectangle rect state in
    let* state = Myseq.from_option (state_minus_rectangle state rect) in
    Myseq.return (drect, state))
  in
  let parse_repeat_point pos color : (unit,data list) p_x_parse =
    let parse_point = parse_point pos color in
    fun p () state ->
    let points = Grid.points state.grid state.mask state.parts in
    (*print_endline "POINTS";*)
    Myseq.prof "Model2.parse_repeat_point/seq" (
    parse_repeat
      (fun (i,j,c) state -> Grid.Mask.mem i j state.mask)
      (fun i p_item (x,y,c as point) state ->
        let* data, state = parse_point p_item point state in
        let state = { state with
                      mask = Grid.Mask.remove x y state.mask;
                      delta = state.delta } in
        Myseq.return (data, state))
      p points state) in
  let parse_repeat_rectangle pos size color mask : (unit, data list) p_x_parse =
    let parse_rectangle = parse_rectangle pos size color mask in
    fun p () state ->
    let rectangles = Grid.rectangles state.grid state.mask state.parts in
    Myseq.prof "Model2.parse_repeat_rectangle/seq" (
    (*print_endline "RECTANGLES";*)
    parse_repeat
      (fun (rect : Grid.rectangle) state ->
        Grid.Mask.(is_subset rect.new_cover state.mask) (* TEST: disjoint items *)
      (* TODO: true by definition of Grid.rectangles_of_part, revise *)
      (*not Grid.Mask.(is_empty (inter rect.mask state.mask))*)
      )
      (fun i p_item rect state ->
        let* data, state = parse_rectangle p_item rect state in
        let state =
          let new_mask = Grid.Mask.diff state.mask rect.new_cover in
          { state with
            mask = new_mask;
            delta = add_delta_with_mask ~mask:state.mask state.delta rect.delta;
          } in
        Myseq.return (data, state))
      p rectangles state)
  in
  parse_template
    ~parse_u:
    (fun () ->
      fun p () state ->
      Myseq.concat
        [parse_all_points state
         |> Myseq.map
              (fun ((i,j,c), state) ->
                `PosShape (`Vec (`Int i, `Int j), `Point (`Color c)),
                state);
         (let* r, state = parse_all_rectangles state in
          let open Grid in
          let* m = Myseq.from_list r.mask_models in
          Myseq.return
            (`PosShape (`Vec (`Int r.offset_i, `Int r.offset_j),
                        `Rectangle (`Vec (`Int r.height, `Int r.width),
                                    `Color r.color,
                                    `Mask m)),
             state)) ])
    ~parse_repeat:(
      function
      | `PosShape (pos, `Point (color)) ->
         parse_repeat_point pos color
      | `PosShape (pos, `Rectangle (size,color,mask)) ->
         parse_repeat_rectangle pos size color mask
      | _ -> assert false)
    ~parse_patt:(function
      | `PosShape (pos, `Point (color)) ->
         parse_single_point pos color
      | `PosShape (pos, `Rectangle (size,color,mask)) ->
         parse_single_rectangle pos size color mask
      | `Many (ordered,items) ->
         fun p () state ->
         let* ditems, state =
           parse_many
             (fun i item state ->
               parse_shape (item :> template) (ith_item i p) () state)
             items state in
         Myseq.return (`Many (ordered,ditems),state)
      | _ -> assert false)
    t

  
type parse_layer_data (* pld *) =
  { parseur : (unit,data) x_parse; (* parseur for this layer *)
    mutable iterators : (data list * parse_state) iterator list } (* for each choice of parsed data for the above layers, an iterator for each parsed data of the current layer *)
  
let parse_layers layers : (unit, data ilist) p_x_parse =
  if layers = `Nil
  then
    fun p () state -> Myseq.return (`Nil, state)
  else
    fun p -> Common.prof "Model2.parse_layers/prep" (fun () ->
    let n, rev_l_pld = (* nb of layers, list of parse_layer_data, in reverse *)
      fold_ilist
        (fun (n,revl) lp layer ->
          n+1,
          { parseur = parse_shape layer (p ++ `Layer lp);
            iterators = [] }
          ::revl)
        (0,[]) `Root layers in
    (* array of parse_layer_data, index by layer rank, 0 is top-most layer, n-1 is bottom-most *)
    let arr_pld : parse_layer_data array = Array.of_list (List.rev rev_l_pld) in
    fun () state ->
    (* initialization *)
    let pld0 = arr_pld.(0) in
    let iter0 =
      new iterator
        (let* d0, state0 = pld0.parseur () state in
         Myseq.return ([d0], state0)) in
    (*pld0.iterators <- [iter0]; (* unnecessary *) *)
    (* recursion through layers [i] *)
    let rec aux_i all_empty k i : (data list * parse_state) iterator Myseq.t =
      let pld = arr_pld.(i) in
      if i = 0
      then
        Myseq.return iter0
      else
        concat_rev_list_seq
          pld.iterators
          (let*? iter1 = aux_i all_empty k (i-1) in
           match iter1#pop with
           | None -> None
           | Some (rev_ld1,state1) ->
              let new_iter =
                new iterator
                  (let*! d2, state2 = pld.parseur () state1 in
                   d2::rev_ld1, state2) in
              pld.iterators <- new_iter :: pld.iterators;
              all_empty := false;
              Some new_iter) in
    (* generation of layers parsed data, starting at relaxation level *) 
    let rec aux_n k : (data ilist * parse_state) Myseq.t =
      let all_empty = ref true in (* to know if there was any iterator or solution generated for this [k] *)
      Myseq.append
        (let*? iter = aux_i all_empty k (n-1) in
         match iter#pop with (* QUICK *)
         | None -> None
         | Some (rev_ld, state) ->
            let l, dlayers = fill_ilist_with_rev_list layers rev_ld in
            assert (l = []);
            all_empty := false;
            Some (dlayers, state))
        (fun () ->
          if !all_empty (* STOP when nothing more to generate *)
             || k >= !max_relaxation_level_parse_layers (* max relaxation level reached *)
          then Myseq.Nil
          else aux_n (k+1) ())
    in
    Myseq.prof "Model2.parse_layers/seq" (
    aux_n 0))
    
let parse_grid t : (Grid.t, data) p_x_parse = Common.prof "Model2.parse_grid" (fun () ->
  parse_template
    ~parse_u:(fun () -> parse_empty)
    ~parse_patt:
    (function
     | `Background (size,color,layers) ->
        let parse_size = parse_vec size in
        let parse_color = parse_color color in
        let seq_background_colors =
          match color with
          | `Color bc -> (fun g -> Myseq.return bc)
          | `U -> (fun g -> Myseq.from_list (Grid.background_colors g))
          | _ -> assert false in
        let parse_layers = parse_layers layers in
        fun p ->
        let parse_size = parse_size (p ++ `Size) in
        let parse_color = parse_color (p ++ `Color) in
        let parse_layers = parse_layers p in
        fun (g : Grid.t) state -> Myseq.prof "Model2.parse_grid/seq" (
        let* dsize, state = parse_size (g.height,g.width) state in
        let* bc = seq_background_colors g in                              
        let* dcolor, state = parse_color bc state in
        let state = { state with (* ignoring parts belonging to background *)
                      parts = List.filter (fun (p : Grid.part) -> p.color <> bc) state.parts } in
        let* dlayers, state = parse_layers () state in
(*        let bc = (* background color *)
	  match Grid.majority_colors state.mask g with
	  | bc::_ -> bc (* TODO: return sequence of colors *)
	  | [] -> Grid.black in
        let* dcolor, state = parse_color bc state in *)
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
	Myseq.return (data, new_state))
     | _ -> parse_empty)
    t)

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
  let parse_grid = parse_grid t path0 in    
  let state = { quota_diff;
                diff = diff0;
                delta = delta0;
                mask = Grid.Mask.full g.height g.width;
                parts = Grid.segment_by_color g;
                grid = g } in
  let parses =
    let* qdiff = Myseq.range 0 quota_diff in (* for increasing diff quota *)
    let* data, state = parse_grid g {state with quota_diff = qdiff} in (* parse with this quota *)
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
          |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> Stdlib.compare dl1 dl2)
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
  `Background (u_vec, `U, `Nil)
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
      |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> Stdlib.compare dl1 dl2)
      |> limit_dl (fun (_,_,dl) -> dl) in (* bounding by dl_factor *) 
    Result.Ok (reads_input, reads_pair) in
  let input_reads, reads = List.split input_reads_reads in
  Result.Ok {dl_mi; dl_mo; input_reads; reads})
  
(* template transformations *)

let insert_ilist  (f : 'a option -> 'a) (lp : ilist_revpath)(l : 'a ilist) : 'a ilist =
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

let rec insert_patt (f : 'a option -> 'a) (p : revpath) (patt_parent : 'a patt) : 'a patt = (* one-step down insertion only *)
  Common.prof "Model2.insert_patt" (fun () ->
  match p, patt_parent with
  | `Field (`I, _), `Vec (i,j) -> `Vec (f (Some i), j)
  | `Field (`J, _), `Vec (i,j) -> `Vec (i, f (Some j))

  | `Field (`Color, _), `Point (color) -> `Point (f (Some color))

  | `Field (`Size, _), `Rectangle (size,color,mask) -> `Rectangle (f (Some size), color, mask)
  | `Field (`Color, _), `Rectangle (size,color,layers) -> `Rectangle (size, f (Some color), layers)
  | `Field (`Mask, _), `Rectangle (size,color,mask) -> `Rectangle (size, color, f (Some mask))

  | `Field (`Pos, _), `PosShape (pos,shape) -> `PosShape (f (Some pos), shape)
  | `Field (`Shape, _), `PosShape (pos,shape) -> `PosShape (pos, f (Some shape))

  | `Field (`Color, _), `Background (size,color,layers) -> `Background (size, f (Some color), layers)
  | `Field (`Size, _), `Background (size,color,layers) -> `Background (f (Some size), color, layers)
  | `Field (`Layer lp, _), `Background (size,color,layers) ->
     let new_layers = insert_ilist f lp layers in
     `Background (size, color, new_layers)

  | `Item (None,`Root,_), `Many (ordered,items) ->
     let new_items =
       List.map (fun item -> f (Some item)) items in
     `Many (ordered, new_items)
  | `Item (Some i,`Root,_), `Many (ordered,items) ->
     let ar_items = Array.of_list items in
     assert (i >= 0 && i < Array.length ar_items);
     ar_items.(i) <- f (Some ar_items.(i));
     let new_items = Array.to_list ar_items in
     `Many (ordered, new_items)
  | `Item (_,local,_), _ -> insert_patt f local patt_parent

  | _ ->
     pp_path p; print_string ": ";
     pp_patt_dummy patt_parent;
     print_newline ();
     assert false)

(*let insert_expr (f : 'a option -> 'a) (p : revpath) (e_parent : 'a expr) : 'a expr =
  match p, e_parent with
  | `Item (_,local,_), `For (p_many, t1) -> `For (p_many, f (Some t1))
  | _ -> assert false (* other expressions are not insertable *) *)
       
let rec insert_template (f : template option -> template) (p : revpath) (t : template) : template =
  Common.prof "Model2.insert_template" (fun () ->
  match path_parent p with
  | None -> f (Some t)
  | Some parent ->
     insert_template
       (function
        | None -> assert false
        | Some (t_parent : template) ->
           match t_parent with
           | `Repeat t1 ->
              (match f (Some (t1 :> template)) with
               | #patt as new_t1 -> `Repeat new_t1
               | _ -> assert false)
           | `For (p_many,t1) -> `For (p_many, f (Some (t1 :> template)))
           | #patt as patt_parent -> (insert_patt f p patt_parent :> template)
           (* `U and other #expr are not explorable *)
           | _ -> assert false)
       parent t)
                                                                                    
(* model refinements and learning *)

type grid_refinement =
  | RGridInit
  | RDef of revpath * template * revpath option (* many-ctx *) * bool (* partial: only true for some items if many items *)
  | RObject of revpath * template (* object *)
(*  | RRepeat of revpath
  | RSingle of revpath *)

let xp_grid_refinement (print : Xprint.t) = function
  | RGridInit -> ()
  | RDef (p,t,ctx,partial) ->
     print#string "DEF: "; xp_path print p;
     print#string "="; xp_template print t;
     if partial then print#string " (partial)"
  | RObject (path,obj) ->
     print#string "OBJECT at ";
     xp_path print path;
     print#string ": ";
     xp_template print obj
(*  | RRepeat path ->
     print_string "REPEAT at ";
     pp_path path
  | RSingle path ->
     print_string "SINGLE at ";
     pp_path path *)
let pp_grid_refinement = Xprint.to_stdout xp_grid_refinement
let string_of_grid_refinement = Xprint.to_string xp_grid_refinement

exception Refinement_no_change
let apply_grid_refinement (r : grid_refinement) (t : template) : (grid_refinement * template) option (* None if no change *) =
  Common.prof "Model2.apply_grid_refinement" (fun () ->
  try
    let t =
      match r with
      | RGridInit -> raise Refinement_no_change
      | RDef (modified_p,new_t,None, partial) ->
         t
         |> insert_template
              (function
               | Some x when x = new_t -> raise Refinement_no_change
               | _ -> new_t)
              modified_p
      | RDef (modified_p, new_t, Some p_many, partial) ->
         let insert_aux local t1 =
           t1
           |> insert_template
                (function
                 | Some x when x = new_t -> raise Refinement_no_change
                 | _ -> new_t)
                local in
         ( match path_split_any_item modified_p with
           | None -> raise Refinement_no_change
           | Some (local,ctx) ->
              t
              |> insert_template
                   (function
                    | Some (`Repeat patt) ->
                       `For (p_many, insert_aux local (patt :> template))
                    | Some (`For (p_many_0, t1)) when p_many_0 = p_many ->
                       `For (p_many, insert_aux local t1)
                    | _ -> raise Refinement_no_change)
                   ctx )
      | RObject (path,obj) ->
         t
         |> insert_template
              (function
               | Some x when x = obj -> raise Refinement_no_change
               | _ -> obj)
              path
         |> insert_template (fun _ -> `U) (`Field (`Color,`Root)) (* because background color is defined as remaining color after covering shapes *)
(*      | RRepeat path ->
         t
         |> insert_template
              (function
               | Some (`Point _ | `Rectangle _ as patt) -> `Repeat (patt :> template patt)
               | _ -> raise Refinement_no_change)
              path
      | RSingle path ->
         t
         |> insert_template
              (function
               | Some (`Repeat patt) -> (patt :> template)
               | _ -> raise Refinement_no_change)
              path *)
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
         (fun res for_p p t0 anc0 ->
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
                (for_p,p,role,t0,dl0)::res
              else res)
         [] None path0 t []) in
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
                (fun res (for_p,p,role,t0,dl_t0) ->
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
  let rec find_dl_rank ?dl_best t ctx p reads : (grid_data * Mdl.bits * int * data) option = (* proper QUICK *)
    match reads with
    | [] -> None
    | (env,gd,u_val,dl,rank)::rem ->
       let dl_best =
         match dl_best with
         | None -> dl
         | Some dl -> dl in
       let d = try PMap.find p u_val
               with _ -> pp_path p; assert false in
       if defs_check ~env t ctx d
       then Some (gd, dl -. dl_best, rank, d) (* recording current_dl - best_dl *)
       else find_dl_rank ~dl_best t ctx p rem
  in
  let module TMap = (* mappings from defining templates *)
    Map.Make
      (struct
        type t = template * revpath option
        let compare = Stdlib.compare
      end) in
  (* defs from first example *)
  let defs = [] in
  let defs = Common.prof "Model2.defs_refinements/first/patterns" (fun () ->
    let$ defs, (for_p,p,role,t0,dl_t0) = defs, u_vars in
    let tmap_score_patt =
      match t0 with
      | `U ->
         let$ res, (env,gd,u_val,dl,rank) = TMap.empty, reads_fst in
         let d = try PMap.find p u_val with _ -> assert false in
         let dl_ctx = dl_ctx_of_data gd.data in
         let dl_d_t0 = dl_data_given_template ~ctx:dl_ctx ~path:p t0 d in
         let$ res, t = res, root_template_of_data ~in_output d in
         let (t, ctx as t_ctx) =
           match t, for_p with
           | `Many (ordered,items), Some _ ->
              (`Indexing (t, `Index), for_p)
           | _ -> (t,None) in
         if t <> t0 (* a different pattern *)
            && not (TMap.mem t_ctx res) (* that was not already seen *)
                   (* && defs_check ~env t ctx d (* and that checks. true by construction *) *)
         then
           let dl_t = dl_template ~env_sig ~ctx:dl_ctx0 ~path:p t in
           let dl_d_t = dl_data_given_template ~ctx:dl_ctx ~path:p t d in
           TMap.add t_ctx (dl +. dl_t -. dl_t0 +. dl_d_t -. dl_d_t0, rank) res
         else res
      | _ -> TMap.empty in
    TMap.fold
      (fun (t,ctx) (dl,rank) defs ->
        (dl,p,role,t0,t,ctx)::defs)
      tmap_score_patt defs) in
  let defs = Common.prof "Model2.defs_refinements/first/exprs" (fun () ->
    let$ defs, (role_e,t,ctx) = defs, defs_expressions ~env_sig in
    let data_fst = Common.prof "Model2.defs_refinements/first/exprs/apply" (fun () ->
      reads_fst
      |> List.filter_map
           (fun (env,gd,u_val,dl,rank) ->
             match defs_check_apply ~env t ctx with
             | None -> None
             | Some te -> Some (te,gd,u_val,dl,rank))) in
    if data_fst = []
    then defs
    else
      let$ defs, (for_p,p,role,t0,dl_t0) = defs, u_vars in
      if role_poly_matches role_e role
       (* whether the expression role matches the defined path, and relaxation value *)
      then
        let data_opt =
          data_fst
          |> List.find_map
               (fun (te,gd,u_val,dl,rank) ->
                 let d = try PMap.find p u_val with _ -> assert false in
                 if defs_check_match te d
                 then Some (gd,dl,rank,d)
                 else None) in
        match data_opt with
        | None -> defs
        | Some (gd,dl,rank,d) ->
           let dl_t = dl_template ~env_sig ~ctx:dl_ctx0 ~path:p t in
           let dl_ctx = dl_ctx_of_data gd.data in
           let dl_d_t0 = dl_data_given_template ~ctx:dl_ctx ~path:p t0 d in
           let dl_d_t = dl_data_given_template ~ctx:dl_ctx ~path:p t d in
           (dl +. dl_t -. dl_t0 +. dl_d_t -. dl_d_t0, p, role, t0, t, ctx)::defs
      else defs) in
  (* checking defs w.r.t. other examples *)
  let defs = Common.prof "Model2.defs_refinements/others" (fun () ->
    let$ defs, reads = defs, reads_others in
    defs
    |> List.filter_map
         (fun (dl0,p,role,t0,t,ctx) ->
           match find_dl_rank t ctx p reads with
           | None -> None
           | Some (gd1,dl1,rank1,d1) ->
              let dl_ctx = dl_ctx_of_data gd1.data in
              let dl_d_t0 = dl_data_given_template ~ctx:dl_ctx ~path:p t0 d1 in
              let dl_d_t = dl_data_given_template ~ctx:dl_ctx ~path:p t d1 in
              Some (dl0 +. dl1 +. dl_d_t -. dl_d_t0, p, role, t0, t, ctx))) in
  (* sorting defs, and returning them as a sequence *)
  defs
  |> List.rev (* to correct for the above List.fold_left's that stack in reverse *)
  |> List.stable_sort (* increasing delta DL *)
       (fun (delta_dl1,_,_,_,_,_) (delta_dl2,_,_,_,_,_) ->
         Stdlib.compare delta_dl1 delta_dl2)
  |> Myseq.from_list
  |> Myseq.map (fun (_,p,_,_,t,ctx) -> RDef (p,t,ctx,false)))
and defs_check ~env (t : template) (ctx : revpath option) (d : data) : bool =
  match defs_check_apply ~env t ctx with
  | None -> false
  | Some te -> defs_check_match te d
and defs_check_apply ~env (t : template) (ctx : revpath option) : template option =
     (*     print_string "CHECK expr: "; pp_template t; Option.iter (fun p_many -> print_string " at ctx "; pp_path p_many) ctx; print_newline (); *)
  let t =
    match ctx with
    | None -> t
    | Some p_many -> `For (p_many, t) in
  match apply_template ~env t with
  | Result.Ok t1 -> Some t1
  | Result.Error _ -> None
and defs_check_match (t : template) (d : data) : bool = (* QUICK *)
  match t, d with
       (* | `Many (ordered1,items1), `Many (ordered2,items2) ->
          (* TODO: how to handle [ordered] flags *)
          List.sort Stdlib.compare items1 = List.sort Stdlib.compare items2 (* TODO: avoid sorting here *) *)
  | _, `Many (_,items) -> List.for_all (matches_template t) items
  | _ -> matches_template t d
and defs_expressions ~env_sig : (role_poly * template * revpath option) list =
  (* the [path option] is for the repeat context path, to be used in a For loop *)
  Common.prof "Model2.defs_expressions" (fun () ->
  if env_sig = [] then [] (* we are in the input model *)
  else (
  (* TODO: recover rotational vars
  let vars_of_path (p : revpath) : (template * revpath option) Myseq.t =
    match path_ctx p with
    | None -> Myseq.return (`Ref p, None)
    | Some ctx ->
       Myseq.concat [
           Myseq.return (`Indexing (`Ref p, `Index), Some ctx);
           (let* i = Myseq.from_list [0; 1; 2] in
            Myseq.return (`Indexing (`Ref p, `Int i), None)) ] in
  let vars_rotation_of_path (p : revpath) : (template * revpath option) Myseq.t =
    match path_ctx p with
    | None -> Myseq.empty
    | Some ctx ->
       let* k, n = Myseq.from_list [(0,2); (1,2); (0,3); (1,3); (2,3)] in
       Myseq.return (`Indexing (`Ref p, `Modulo (`Plus (`Index, `Int k), `Int n)), Some ctx) in *)
  let paths =
    let$ res, (_k,lp) = [], env_sig in (* TODO: make env_sig a flat list *)
    let$ res, p = res, lp in
    ((path_role p :> role_poly), p) :: res in
  let exprs = ref ([] : (role_poly * template * revpath option) list) in (* stack of expressions *)
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
    match path_ctx p with
    | None -> push (role, `Ref p, None)
    | Some ctx ->
       push (role, `Indexing (`Ref p, `Index), Some ctx);
       let& i = [0; 1; 2] in
       push (role, `Indexing (`Ref p, `Int i), None) in
  let exprs_0 = List.rev !exprs in
  (* LEVEL 1 *)
  let _ =
    let& (role1,e1,ctx1) = exprs_0 in
    (* unary operators *)
    let _ = (* area(_) *)
      match role1 with
      | `Shape -> push (`Int (`X, `Size `X), `Area e1, ctx1)
      | _ -> () in
    let _ = (* right(_) *)
      match role1 with
      | `Layer -> push (`Int (`J, `Pos), `Right e1, ctx1)
      | _ -> () in
    let _ = (* center(_) *)
      match role1 with
      | `Layer -> push (`Int (`J, `Pos), `Center e1, ctx1)
      | _ -> () in
    let _ = (* bottom(_) *)
      match role1 with
      | `Layer -> push (`Int (`I, `Pos), `Bottom e1, ctx1)
      | _ -> () in
    let _ = (* middle(_) *)
      match role1 with
      | `Layer -> push (`Int (`I, `Pos), `Middle e1, ctx1)
      | _ -> () in
    let _ = (* ProjI/J *)
      match role1 with
      | `Vec _ ->
         push (role1, `ProjI e1, ctx1);
         push (role1, `ProjJ e1, ctx1)
      | _ -> () in
    let _ = (* ApplySym *)
      match role1 with
      | (`Mask | `Shape | `Layer as role1) ->
         let& sym = [`FlipHeight; `FlipWidth; `FlipDiag1; `FlipDiag2;
                     `Rotate180; `Rotate90; `Rotate270] in
         push (role1, `ApplySym (sym, e1, role1), ctx1)
      | _ -> () in
    let _ = (* Coloring(_, const) *)
      match role1 with
      | (`Shape | `Layer) ->
         let& c = Grid.all_colors in
         push (role1, `Coloring (e1, `Color c), ctx1)
      | _ -> () in
    let& (role2,e2,ctx2) = exprs_0 in
    if ctx1 = ctx2
    then
      (* binary operators *)
      let _ = (* corner(_,_) *)
        match role1, role2 with
        | `Vec `Pos, `Vec `Pos when e1 <> e2 ->
           push (role1, `Corner (e1,e2), ctx1)
        | _ -> () in (* TEST: var/feat *)
      let _ = (* span(_,_) *)
        match role1, role2 with
        | `Int ((`I | `J as ij1), `Pos), `Int ((`I | `J as ij2), `Pos) when ij1=ij2 && e1 < e2 ->
           push (`Int (ij1, `Pos), `Span (e1,e2), ctx1)
        | `Vec `Pos, `Vec `Pos when e1 < e2 ->
           push (`Vec (`Size `X), `Span (e1,e2), ctx1)
        | _ -> () in
      let _ = (* min([_;_]) *)
        match role1, role2 with
        | `Int xx1, `Int xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Min [e1;e2], ctx1)
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Min [e1;e2], ctx1)
        | _ -> () in
      let _ = (* max([_;_]) *)
        match role1, role2 with
        | `Int xx1, `Int xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Max [e1;e2], ctx1)
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Max [e1;e2], ctx1)
        | _ -> () in
      let _ = (* average([_;_]) *)
        match role1, role2 with
        | `Int xx1, `Int xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Average [e1;e2], ctx1)
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Average [e1;e2], ctx1)
        | _ -> () in
      let _ = (* translation = pos - pos *)
        match role1, role2 with
        | `Int (ij1, `Pos), `Int (ij2, `Pos) when ij1=ij2 && e1 <> e2 ->
           push (`Int (ij1, `Move), `Minus (e1,e2), ctx1)
        | `Vec `Pos, `Vec `Pos when e1 <> e2 ->
           push (`Vec `Move, `Minus (e1,e2), ctx1)
        | _ -> () in
      let _ = (* translationOnto(_,_) *)
        match role1, role2 with
        | `Layer, `Layer when e1 < e2 ->
           push (`Vec `Move, `TranslationOnto (e1,e2), ctx1);
        | _ -> () in
      let _ = (* translationSym(_,_,_) *)
        match role1, role2 with
        | `Layer, (`Layer|`Grid) when e1 <> e2 ->
           let& sym = [`FlipHeight; `FlipWidth; `FlipDiag1; `FlipDiag2;
                       `Rotate180; `Rotate90; `Rotate270] in
           push (`Vec `Move, `TranslationSym (sym,e1,e2), ctx1)
        | _ -> () in
      let _ = (* Coloring (_, ref) *)
        match role1, role2 with
        | (`Shape | `Layer), `Color _ -> push (role1, `Coloring (e1,e2), ctx1)
        | _ -> () in
      () in
  let exprs_1 = List.rev !exprs in
  (* LEVEL 2, not generating Move at this level *)
  let _ = 
    let _ = (* constants *)
      let& k = [0;1;2;3] in
      push (`Int (`X, (if k=0 then `Pos else `X)), `ConstInt k, None);
      let& l = [0;1;2;3] in
      push (`Vec (if k=0 && l=0 then `Pos else `X), `ConstVec (k,l), None) in
    let& (role1,e1,ctx1) = exprs_1 in
    (* unary operators *)
    let _ = (* IncrInt, DecrInt *)
      match role1 with
      | `Int (_,rvec) when rvec <> `Move ->
         let& k = [1;2;3] in
         push (role1, `IncrInt (e1,k), ctx1);
         push (role1, `DecrInt (e1,k), ctx1)
      | _ -> () in
    let _ = (* IncrVec, DecrVec *)
      match role1 with
      | `Vec rvec when rvec <> `Move ->
         let& k = [0;1;2;3] in
         let& l = [0;1;2;3] in
         if k+l > 0 then (
           push (role1, `IncrVec (e1,k,l), ctx1);
           push (role1, `DecrVec (e1,k,l), ctx1)
         )
      | _ -> () in
    let _ =
      let& n = [2;3] in
      let _ = (* ScaleUp, ScaleDown *)
        match role1 with
        | `Int (_,rvec) | `Vec rvec when rvec <> `Move ->
           push (role1, `ScaleUp (e1,n), ctx1);
           push (role1, `ScaleDown (e1,n), ctx1)
        | `Mask | `Shape ->
           push (role1, `ScaleUp (e1,n), ctx1)
        | _ -> () in
      () in
    let _ = (* not _ *)
      match role1 with
      | `Mask -> push (`Mask, `LogNot e1, ctx1)
      | _ -> () in
    let _ = (* 0 + move -> pos *)
      match role1 with
      | `Int (ij, `Move) -> push (`Int (ij, `Pos), `Plus (`ConstInt 0, e1), ctx1)
      | `Vec `Move -> push (`Vec `Pos, `Plus (`ConstVec (0,0), e1), ctx1)
      | _ -> () in
    let& (role2,e2,ctx2) = exprs_1 in
    if ctx1 = ctx2
    then
      (* binary operators *)
      let _ = (* _ + _ *)
        match role1, role2 with
        | `Int (_, xx1), `Int (_, (`X | `Size _ | `Move))
          | `Vec xx1, `Vec (`X | `Size _ | `Move)
             when xx1 <> `Move && (if xx1 = `Pos then e1 <> e2 else e1 < e2) ->
           push (role1, `Plus (e1,e2), ctx1)
        | _ -> () in
      let _ = (* _ - _ *)
        match role1, role2 with (* not generating Moves *)
        | `Int (_, rvec), `Int (_, (`X | `Size _ | `Move)) when rvec <> `Move && e1 <> e2 ->
           push (role1, `Minus (e1,e2), ctx1)
        | `Vec rvec, `Vec (`X | `Size _ | `Move) when rvec <> `Move && e1 <> e2 ->
           push (role1, `Minus (e1,e2), ctx1)
        | _ -> () in
      () in
  let exprs_2 = List.rev !exprs in
  (* LEVEL 3 *)
  let _ = 
    let& (role1,e1,ctx1) = exprs_2 in
    let _ = (* Tiling *)
      match role1 with
      | (`Vec (`X | `Size _) | `Mask | `Shape) ->
         let& k = [1;2;3] in
         let& l = [1;2;3] in
         if k>1 || l>1 then
           push (role1, `Tiling (e1,k,l), ctx1)
      | _ -> () in
    let _ = (* UnfoldSym *)
      match role1 with
      | `Mask | `Shape | `Layer ->
         let& sym_matrix = [sym_matrix_flipHeightWidth] in
         push (role1, `UnfoldSym (sym_matrix, e1), ctx1)
      | _ -> () in
    let& (role2,e2,ctx2) = exprs_2 in
    if ctx1 = ctx2
    then
      let _ = (* ScaleTo on masks *)
        match role1, role2 with
        | (`Mask | `Shape), `Vec (`X | `Size _) ->
           push (role1, `ScaleTo (e1,e2), ctx1)
        | _ -> () in
      let _ = (* ResizeAlikeTo *)
        match role1, role2 with
        | (`Mask | `Shape | `Layer), `Vec (`X | `Size _) ->
           push (role1, `ResizeAlikeTo (e1,e2), ctx1)
        | _ -> () in
      let _ = (* _ and _ *)
        match role1, role2 with
        | `Mask, `Mask when e1 < e2 -> push (`Mask, `LogAnd (e1,e2), ctx1)
        | _ -> () in
      let _ = (* _ or _ *)
        match role1, role2 with
        | `Mask, `Mask when e1 < e2 -> push (`Mask, `LogOr (e1,e2), ctx1)
        | _ -> () in
      let _ = (* _ xor _ *)
        match role1, role2 with
        | `Mask, `Mask when e1 < e2 -> push (`Mask, `LogXOr (e1,e2), ctx1)
        | _ -> () in
      let _ = (* _ and not _ *)
        match role1, role2 with
        | `Mask, `Mask when e1 <> e2 -> push (`Mask, `LogAndNot (e1,e2), ctx1)
        | _ -> () in
      () in
  ()
   with End_of_file -> ());
  (*Printf.printf "== %d expressions ==\n" (List.length !exprs);*)
  List.rev !exprs)) (* approximate order in increasing size *)
  
let shape_refinements ~(env_sig : signature) (t : template) : grid_refinement Myseq.t =
  Common.prof "Model2.shape_refinements" (fun () ->
  (*let aux_repeat p = function
    | `Repeat _ -> Myseq.return (RSingle p)
    | `Point _ | `Rectangle _ -> Myseq.return (RRepeat p)
    | _ -> assert false in *)
  let rec aux ~(objs : template list) lp = function
    | `Nil ->
       let* obj = Myseq.from_list objs in
       Myseq.return (RObject (`Field (`Layer lp, `Root), obj))
    | `Insert (above,_,below) ->
       Myseq.concat
         [ (*aux_repeat [`Layers lp] shape;*) (* TEST *)
           aux ~objs (`Right lp) below; (* insert below first *)
           aux ~objs (`Left lp) above ]
    | _ -> assert false
  in
  match t with
  | `Background (_,_,layers) ->
     let any_point, any_rect, rep_any_point, rep_any_rect =
       if !use_repeat
       then
         fold_ilist
           (fun (ap,ar,rap,rar) lp layer ->
             match layer with
             | `PosShape (_, `Point (`U)) -> (true,ar,rap,rar)
             | `PosShape (_, `Rectangle (_, `U, `U)) -> (ap,true,rap,rar)
             | `Repeat (`PosShape (_, `Point (`U))) -> (ap,ar,true,rar)
             | `Repeat (`PosShape (_, `Rectangle (_, `U, `U))) -> (ap,ar,rap,true)
             | _ -> (ap,ar,rap,rar))
           (false,false,false,false) `Root layers
       else true, true, false, false in
     let sp =
       let objs =
         if rep_any_point
         then []
         else
           let obj = `PosShape (u_vec, `Point (`U)) in
           if any_point
           then [obj]
           else [obj; `Repeat obj] in
       aux ~objs `Root layers in
     let sr =
       let objs =
         if rep_any_rect
         then []
         else
           let obj = `PosShape (u_vec, `Rectangle (u_vec, `U, `U)) in
           if any_rect
           then [obj]
           else [obj; `Repeat obj] in
       aux ~objs `Root layers in
     let ss =
       let ps_shape = signature_of_kind env_sig Shape in
       Myseq.concat
         (List.map
            (fun p_shape ->
              let obj = `PosShape (u_vec, `Ref p_shape) in
              let obj =
                match path_ctx p_shape with
                | None -> obj
                | Some p_many -> `For (p_many, obj) in
              aux ~objs:[obj] `Root layers)
            ps_shape) in
     let so =
       let ps_layer = signature_of_kind env_sig Layer in
       Myseq.concat
         (List.map
            (fun p_layer ->
              let obj = `Ref p_layer in
              let obj =
                match path_ctx p_layer with
                | None -> obj
                | Some p_many -> `For (p_many, obj) in
              aux ~objs:[obj] `Root layers)
            ps_layer) in
     Myseq.concat [so; ss; sr; sp]
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


