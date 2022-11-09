
open Arc_common
open Task

let seq = def_param "seq" false (* TEST *) string_of_bool
let max_seq_length = def_param "max_seq_length" (if !seq then 10 else 1) string_of_int (* max size of collected sequences *)
let max_nb_layers = def_param "max_nb_layers" 8 string_of_int (* max nb of layers in grid models *)
                  
let alpha = def_param "alpha" 10. string_of_float
let max_nb_parse = def_param "max_nb_parse" (if !seq then 256 else 64) string_of_int (* max nb of considered grid parses *)
let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
let max_relaxation_level_parse_layers = def_param "max_relaxation_level_parse_layers" (if !seq then 256 else 16) string_of_int (* see parse_layers *)
let def_match_threshold = def_param "def_match_threshold" (if !seq then 0.6 else 1.) string_of_float (* ratio threshold for considering that a definition is a match *)
let max_nb_diff = def_param "max_nb_diff" 3 string_of_int (* max nb of allowed diffs in grid parse *)
let max_nb_grid_reads = def_param "max_nb_grid_reads" 3 string_of_int (* max nb of selected grid reads, passed to the next stage *)
let max_expressions = def_param "max_expressions" 100000 string_of_int (* max nb of considered expressions when generating defs-refinements *)
let max_refinements = def_param "max_refinements" 20 string_of_int (* max nb of considered refinements *)

(** Part 1: grids *)

(* common data structures *)

type 'a double = 'a * 'a
type 'a triple = 'a * 'a * 'a

type 'a ilist = (* insertable list *)
  [ `Nil
  | `Insert of 'a ilist * 'a * 'a ilist
  | `Append of 'a ilist * 'a ilist ]
type ilist_revpath =
  [ `Root
  | `Left of ilist_revpath
  | `Right of ilist_revpath ]

let rec xp_ilist_path (print : Xprint.t) : ilist_revpath -> unit =
  function
  | `Root -> print#string "0"
  | `Left p -> xp_ilist_path print p; print#string "0"
  | `Right p -> xp_ilist_path print p; print#string "1"

let rec xp_ilist (xp : Xprint.t -> 'a -> unit) (print : Xprint.t) (l : 'a ilist) : unit =
  let rec aux lp = function
    | `Nil -> ()
    | `Insert (left,elt,right) ->
       aux (`Left lp) left;
       print#string "\n  _"; xp_ilist_path print lp; print#string ": "; xp print elt;
       aux (`Right lp) right
    | `Append (left,right) ->
       aux (`Left lp) left;
       aux (`Right lp) right
  in
  aux `Root l

let rec ilist_length : 'a ilist -> int = function
  | `Nil -> 0
  | `Insert (left,elt,right) ->
     ilist_length left + 1 + ilist_length right
  | `Append (left,right) ->
     ilist_length left + ilist_length right

let rec map_ilist (f : ilist_revpath -> 'a -> 'b) (lp : ilist_revpath) (l : 'a ilist) : 'b ilist =
  match l with

  | `Nil -> `Nil
  | `Insert (left,elt,right) ->
     let left = map_ilist f (`Left lp) left in
     let elt = f lp elt in
     let right = map_ilist f (`Right lp) right in
     `Insert (left,elt,right)
  | `Append (left,right) ->
     let left = map_ilist f (`Left lp) left in
     let right = map_ilist f (`Right lp) right in
     `Append (left,right)

let rec map_ilist_result (f : ilist_revpath -> 'a -> ('b,'c) Result.t) (lp : ilist_revpath) (l : 'a ilist) : ('b ilist,'c) Result.t =
  match l with
  | `Nil -> Result.Ok `Nil
  | `Insert (left,elt,right) ->
     let| left = map_ilist_result f (`Left lp) left in
     let| elt = f lp elt in
     let| right = map_ilist_result f (`Right lp) right in
     Result.Ok (`Insert (left,elt,right))
  | `Append (left,right) ->
     let| left = map_ilist_result f (`Left lp) left in
     let| right = map_ilist_result f (`Right lp) right in
     Result.Ok (`Append (left,right))

let rec fold_ilist (f : 'b -> ilist_revpath -> 'a -> 'b) (acc : 'b) (lp : ilist_revpath) (l : 'a ilist) : 'b =
  match l with
  | `Nil -> acc
  | `Insert (left,elt,right) ->
     let acc = fold_ilist f acc (`Left lp) left in
     let acc = f acc lp elt in
     let acc = fold_ilist f acc (`Right lp) right in
     acc
  | `Append (left,right) ->
     let acc = fold_ilist f acc (`Left lp) left in
     let acc = fold_ilist f acc (`Right lp) right in
     acc
       
let rec fold2_ilist (f : 'c -> ilist_revpath -> 'a -> 'b -> 'c result) (acc : 'c) (lp : ilist_revpath) (l1 : 'a ilist) (l2 : 'b ilist) : 'c result =
  match l1, l2 with
  | `Nil, `Nil -> Result.Ok acc
  | `Insert (left1,elt1,right1), `Insert (left2,elt2,right2) ->
     let| acc = fold2_ilist f acc (`Left lp) left1 left2 in
     let| acc = f acc lp elt1 elt2 in
     let| acc = fold2_ilist f acc (`Right lp) right1 right2 in
     Result.Ok acc
  | `Append (left1,right1), `Append (left2,right2) ->
     let| acc = fold2_ilist f acc (`Left lp) left1 left2 in
     let| acc = fold2_ilist f acc (`Right lp) right1 right2 in
     Result.Ok acc
  | _ ->
     Result.Error (Invalid_argument "Model2.fold2_ilist: inconsistent ilists")
            
let rec fill_ilist_with_list il l = (* QUICK *)
  (* replacing elements in il with elements in l, taken in order *)
  (* first element goes leftmost, last element rightmost *)
  match il with
  | `Nil -> l, `Nil
  | `Insert (left,_,right) ->
     let l, left' = fill_ilist_with_list left l in
     ( match l with
     | [] -> assert false
     | x::l ->
        let l, right' = fill_ilist_with_list right l in
        l, `Insert (left', x, right') )
  | `Append (left,right) ->
     let l, left' = fill_ilist_with_list left l in
     let l, right' = fill_ilist_with_list right l in
     l, `Append (left', right')


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

type dim = Item | Sequence (* Item has dim=0, Sequence has dim=1 *)
(* internal repr must be consistent with (max Item Sequence = Sequence) *)

let max_dim_list : dim list -> dim = function
  | [] -> failwith "Model2.max_dim_list: empty list"
  | x::r -> List.fold_left max x r
                
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
  [ `IntCoord of [`I | `J] * role_vec
  | `IntCard (* cardinal, result of a count, in general strictly positive *)
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
  | `IntCoord _ -> Int
  | `IntCard -> Int
  | `Color _ -> Color
  | `Mask -> Mask
  | `Vec _ -> Vec
  | `Shape -> Shape
  | `Object -> Object
  | `Layer -> Layer
  | `Grid -> Grid

let role_can_be_sequence = function
  | `Int (_, `Size `Grid)
    | `Vec (`Size `Grid)
    | `Color `Grid -> false (* grid attributes are singletons *)
  | _ -> true (* layer attributes can be sequences *)
           
type role_poly = (* polymorphic extension of role *)
  [ `IntCoord of [`I | `J | `X] * role_vec_poly
  | `IntCard
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
    | `IntCoord (_,vec_x), `IntCoord (_,vec) -> aux_vec vec_x vec
    | `IntCard, `IntCard -> true
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
  | `Mask of Mask_model.t
  | `Vec of 'a * 'a (* i, j -> vec *)
  | `Point of 'a (* color -> shape *)
  | `Rectangle of 'a * 'a * 'a (* size, color, mask -> shape *)
  | `PosShape of 'a * 'a (* pos, shape -> object *)
  | `Grid of Grid.t
  | `Background of 'a * 'a * 'a * 'a ilist (* grid | size, color, layers (top first) -> grid, the grid component is only used in data to get access to the full grid *)
  ]

type field =
  [ `I
  | `J
  | `Pos
  | `Color
  | `Size
  | `Mask
  | `Shape
  | `Layer of ilist_revpath
  | `Grid ]
type revpath =
  [ `Root
  | `Field of field * revpath
  | `Item of int * revpath
  | `AnyItem of revpath (* for paths at item-level but not about a specific item *)
  | `Arg of int * role option * revpath ] (* if no role, parent role *)
let path0 = `Root

let rec (++) p f = `Field (f,p)
       
let rec path_parent : revpath -> revpath option = function
  | `Root -> None
  | `Field (f,p) -> Some p
  | `Item (i,p) -> Some p
  | `AnyItem p -> Some p
  | `Arg (i,role_opt,p) -> Some p

(* reversing a revpath, to get paths starting from the root: used in path_factorize *)
(* must be its own inverse: reverse(reverse(p,`Root),`Root) = p *)
let rec path_reverse (p : revpath) (acc : revpath) : revpath =
  match p with
  | `Root -> acc
  | `Field (`Layer lp, p1) -> path_reverse p1 (`Field (`Layer (ilist_reverse lp `Root), acc))
  | `Field (f,p1) -> path_reverse p1 (`Field (f,acc))
  | `Item (i,p1) -> path_reverse p1 (`Item (i,acc))
  | `AnyItem p1 -> path_reverse p1 (`AnyItem acc)
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
    | `AnyItem rq1, `AnyItem rq2 ->
       aux_path (`AnyItem p0) rq1 rq2
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
  | `Grid (g : Grid.t) -> Some (g.Grid.height, g.width)
  | `Background (_, `Vec (`Int h, `Int w), _, _) -> Some (h,w)
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
let data0 : data =
  `Background (`Grid (Grid.make 1 1 Grid.black),
               `Vec (`Int 1, `Int 1), `Color Grid.black, `Nil)
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
  | `ScaleUp of 'a * 'a (* on (Int, Vec, Mask, Shape, Grid as T), Card -> T *)
  | `ScaleDown of 'a * 'a (* on (Int, Vec, Mask, Shape, Grid as T), Card -> T *)
  | `ScaleTo of 'a * 'a (* Mask, Grid, Vec -> Mask *)
  | `Crop of 'a * 'a (* Grid, Rectangle -> Grid *)
  | `Strip of 'a (* on Grid *)
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
  | `Stack of 'a list (* on Grids *)
  | `Area of 'a (* on Shape *)
  | `Left of 'a (* on Object *)
  | `Right of 'a (* on Object *)
  | `Center of 'a (* on Object *)
  | `Top of 'a (* on Object *)
  | `Bottom of 'a (* on Object *)
  | `Middle of 'a (* on Object *)
  | `ProjI of 'a (* on Vec *)
  | `ProjJ of 'a (* on Vec *)
  | `MaskOfGrid of 'a (* Grid -> Mask TODO: specify bgcolor *)
  | `GridOfMask of 'a * 'a (* Mask, Color -> Grid *)
  | `TranslationOnto of 'a * 'a (* Obj, Obj -> Vec *)
  | `Tiling of 'a * int * int (* on Vec/Mask/Shape *)
  | `PeriodicFactor of Grid.Transf.periodicity_mode * 'a * 'a (* on Color, Mask/Shape/Object/Grid as T -> T *)
  | `FillResizeAlike of Grid.Transf.periodicity_mode * 'a * 'a * 'a (* on Color, Vec, Mask/Shape/Object/Grid as T -> T *)
  | `Compose of 'a * 'a * 'a (* Color, Mask/Shape/Grid as T, T -> T *)
  | `ApplySym of symmetry * 'a * role (* on Vec, Mask, Shape, Object; role of the argument as computation depends on it *)
  | `UnfoldSym of symmetry list list * 'a (* on Mask, Shape, Object *)
  (* sym list list = matrix to be filled with symmetries of some mask *)
  | `CloseSym of symmetry list * 'a * 'a (* Color, Mask/Shape/Object/Grid as T -> T *)
  (* symmetry list = list of symmetries to chain and stack to force some symmetry, taking the given color as transparent *)
  | `TranslationSym of symmetry * 'a * 'a (* Obj, Obj/Grid -> Vec *)
  | `MajorityColor of 'a (* Grid -> Color *)
  | `ColorCount of 'a (* Grid -> Int *)
  | `Coloring of 'a * 'a (* Shape/Obj, Color -> Shape/Obj *)
  | `SwapColors of 'a * 'a * 'a (* Grid, Color, Color -> Grid *)
  ]
and symmetry = [
  | `Id
  | `FlipHeight | `FlipWidth | `FlipDiag1 | `FlipDiag2
  | `Rotate180 | `Rotate90 | `Rotate270 ]

let all_symmetry = [
    `Id;
    `FlipHeight; `FlipWidth;
    `FlipDiag1; `FlipDiag2;
    `Rotate180; `Rotate90; `Rotate270
  ]
let nb_symmetry = List.length all_symmetry

let all_symmetry_unfold = [
    [[`Id; `FlipWidth]; [`FlipHeight; `Rotate180]];
    [[`Id]; [`FlipHeight]];
    [[`Id; `FlipWidth]];
    [[`Id; `Rotate90]; [`Rotate270; `Rotate180]]
  ] (* TODO: in principle, should add more unfolds following the 10 symmetry groups. See sym_X_unfold in Grid.Transf/Mask_model *)
let nb_symmetry_unfold = List.length all_symmetry_unfold

let all_symmetry_close = List.rev [ (* preferring stronger symmetries. TODO: do through DL *)
    [`FlipHeight];
    [`FlipWidth];
    [`Rotate180];
    [`FlipDiag1];
    [`FlipDiag2];
    [`FlipHeight; `FlipWidth]; (* entails Rotate180 *)
    [`FlipDiag1; `FlipDiag2]; (* entails Rotate180 *)
    [`Rotate90; `Rotate180]; (* entails Rotate270 *)
    [`FlipHeight; `Rotate90; `Rotate180] (* entails FlipWidth, FlipDiag1, FlipDiag2, Rotate270: fullest symmetry *)
  ]
let nb_symmetry_close = List.length all_symmetry_close
                       
type template =
  [ `Any (* an item of sequence items with no constraint (anything) *)
  | template patt (* an item or sequence of items matching the pattern *)
  | template expr (* an expression that evaluates into a template *)
  | `Seq of template list (* a sequence where items match the given template list, items should be ground *)
  | `Cst of template (* all sequence items are the same, and match the item template *)  
  | `Prefix of template * template list (* a sequence whose items match the main template, and whose K first items match the template list, items should be ground *)
  ]
let template0 : template = `Any
let _ = (template0 :> template seq) (* template is an instance of template seq *)

let u_any = `Any
let u_cst = (* `Cst *) u_any (* TEST *)
let u_vec_any : template = `Vec (u_any, u_any)
let u_vec_cst : template = `Vec (u_cst, u_cst)

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

let string_of_dim : dim -> string = function
  | Item -> "I"
  | Sequence -> "S"
                
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
  | `Grid -> print#string "grid"
let pp_field = Xprint.to_stdout xp_field

let rec xp_path (print : Xprint.t) : revpath -> unit = function
  | `Root -> print#string "^"
  | `Field (f,p) -> xp_path print p; print#string "."; xp_field print f
  | `Item (i,p) -> xp_path print p; print#string "["; print#int i; print#string "]"
  | `AnyItem p -> xp_path print p; print#string "[*]"
  | `Arg (i,role_opt,p) -> xp_path print p; print#string "."; print#int i
let pp_path = Xprint.to_stdout xp_path
let string_of_path = Xprint.to_string xp_path

let xp_path_list (print : Xprint.t) lp =
  print#string "(";
  List.iter (fun path -> xp_path print path; print#string " in ") lp;
  print#string ")"
let pp_path_list = Xprint.to_stdout xp_path_list

let xp_mask_model (print : Xprint.t) : Mask_model.t -> unit = function
  | `Full -> print#string "Full"
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
  | `Grid g ->
     Grid.xp_grid print g (* TODO: test and improve *)
  | `Background (_grid,size,color,layers) ->
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
  | `ScaleUp (a,b) -> xp print a; print#string " * "; xp print b
  | `ScaleDown (a,b) -> xp print a; print#string " / "; xp print b
  | `ScaleTo (a,b) -> xp_apply "scaleTo" xp print [a;b]
  | `Crop (a,b) -> xp_apply "crop" xp print [a;b]
  | `Strip a -> xp_apply "strip" xp print [a]
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
  | `Stack la -> xp_apply "stack" xp print la
  | `Area a -> xp_apply "area" xp print [a]
  | `Left a -> xp_apply "left" xp print [a]
  | `Right a -> xp_apply "right" xp print [a]
  | `Center a -> xp_apply "center" xp print [a]
  | `Top a -> xp_apply "top" xp print [a]
  | `Bottom a -> xp_apply "bottom" xp print [a]
  | `Middle a -> xp_apply "middle" xp print [a]
  | `ProjI a -> xp_apply "projI" xp print [a]
  | `ProjJ a -> xp_apply "projJ" xp print [a]
  | `MaskOfGrid a -> xp_apply "mask" xp print [a]
  | `GridOfMask (a,b) -> xp_apply "grid" xp print [a;b]
  | `TranslationOnto (a,b) -> xp_apply "translationOnto" xp print [a;b]
  | `Tiling (a,k,l) -> xp_apply_poly "tiling" print
                         [(fun print -> xp print a);
                          (fun print -> print#int k);
                          (fun print -> print#int l)]
  | `PeriodicFactor (mode,a,b) ->
     xp_apply ("periodicFactor" ^ suffix_periodicity_mode mode) xp print [a;b]
  | `FillResizeAlike (mode,a,b,c) ->
     xp_apply ("fillResizeAlike" ^ suffix_periodicity_mode mode) xp print [a;b;c]
  | `Compose (a,b,c) -> xp_apply "compose" xp print [a;b;c]
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
  | `CloseSym (sym_seq,a,b) ->
     xp_apply_poly "closeSym" print
       [(fun print ->
           List.iter
             (fun sym -> xp_symmetry print sym; print#string "; ")
             sym_seq);
        (fun print -> xp print a);
        (fun print -> xp print b)]
  | `TranslationSym (sym,a,b) -> xp_apply_poly "translationSym" print
                                   [(fun print -> xp_symmetry print sym);
                                    (fun print -> xp print a);
                                    (fun print -> xp print b)]
  | `MajorityColor a -> xp_apply "majorityColor" xp print [a]
  | `ColorCount a -> xp_apply "colorCount" xp print [a]
  | `Coloring (a,b) -> xp_apply "coloring" xp print [a;b]
  | `SwapColors (a,b,c) -> xp_apply "swapColor" xp print [a;b;c]
and xp_symmetry print : symmetry -> unit = function
  | `Id -> print#string "id"
  | `FlipHeight -> print#string "flipHeight"
  | `FlipWidth -> print#string "flipWidth"
  | `FlipDiag1 -> print#string "flipDiag1"
  | `FlipDiag2 -> print#string "flipDiag2"
  | `Rotate180 -> print#string "rotate180"
  | `Rotate90 -> print#string "rotate90"
  | `Rotate270 -> print#string "rotate270"
and suffix_periodicity_mode = function
  | `Total -> "_total"
  | `Strict -> "_strict"
  | `TradeOff -> ""

                  
let string_of_expr xp = Xprint.to_string (xp_expr xp)
                         
                   
let rec xp_template (print : Xprint.t) : template -> unit = function
  | `Any -> print#string "?"
  | #patt as patt -> xp_patt xp_template print patt
  | #expr as e -> xp_expr xp_template print e
  | `Seq items ->
     Xprint.bracket
       ("<", " >")
       (Xprint.sep_list ", " xp_template)
       print
       items
  | `Cst item0 ->
     print#string "<";
     xp_template print item0;
     print#string " == >"
  | `Prefix (main,items) ->
     print#string "<";
     Xprint.sep_list ", " xp_template print items;
     print#string " | ";
     xp_template print main;
     print#string ">"
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

let rec path_dim (p : revpath) : dim =
  match p with
  | `Root -> Item
  | `Field (`Layer _, p1) -> Sequence
  | `Field (f,p1) -> path_dim p1
  | `Item (_,p1) -> Item
  | `AnyItem p1 -> Item
  | `Arg (_, _, p1) -> assert false (* depends on the function/argument, not used *)

let patt_dim (dim : 'a -> dim) (patt : 'a patt) : dim =
  (* using the fact that (max Item Sequence = Sequence), idea of broadcasting *)
  match patt with
  | `Bool _ | `Int _ | `Color _ | `Mask _ -> Item
  | `Vec (i,j) -> max (dim i) (dim j)
  | `Point color -> dim color
  | `Rectangle (size,color,mask) -> max (dim size) (max (dim color) (dim mask))
  | `PosShape (pos,shape) -> max (dim pos) (dim shape)
  | `Grid g -> Item
  | `Background (_grid,size,color,layers) ->
     fold_ilist
       (fun res lp layer -> max res (dim layer))
       (max (dim size) (dim color))
       `Root layers

let expr_dim (dim : 'a -> dim) (e : 'a expr) : dim =
  match e with
  | `Ref p -> path_dim p
  | `ConstInt _ -> Item
  | `ConstVec _ -> Item
  | `Plus (a,b) -> max (dim a) (dim b) (* broadcasting *)
  | `Minus (a,b) -> max (dim a) (dim b)
  | `IncrInt (a,k) -> dim a
  | `DecrInt (a,k) -> dim a
  | `IncrVec (a,k,l) -> dim a
  | `DecrVec (a,k,l) -> dim a
  | `Modulo (a,b) -> max (dim a) (dim b)
  | `ScaleUp (a,b) -> max (dim a) (dim b)
  | `ScaleDown (a,b) -> max (dim a) (dim b)
  | `ScaleTo (a,b) -> max (dim a) (dim b)
  | `Crop (a,b) -> max (dim a) (dim b)
  | `Strip a -> dim a
  | `Corner (a,b) -> max (dim a) (dim b)
  | `Min l -> max_dim_list (List.map dim l)
  | `Max l -> max_dim_list (List.map dim l)
  | `Average l -> max_dim_list (List.map dim l)
  | `Span (a,b) -> max (dim a) (dim b)
  | `Norm a -> dim a
  | `Diag1 (a,k) -> dim a
  | `Diag2 (a,k) -> dim a
  | `LogAnd (a,b) -> max (dim a) (dim b)
  | `LogOr (a,b) -> max (dim a) (dim b)
  | `LogXOr (a,b) -> max (dim a) (dim b)
  | `LogAndNot (a,b) -> max (dim a) (dim b)
  | `LogNot a -> dim a
  | `Stack l -> max_dim_list (List.map dim l)
  | `Area a -> dim a
  | `Left a -> dim a
  | `Right a -> dim a
  | `Center a -> dim a
  | `Top a -> dim a
  | `Bottom a -> dim a
  | `Middle a -> dim a
  | `ProjI a -> dim a
  | `ProjJ a -> dim a
  | `MaskOfGrid a -> dim a
  | `GridOfMask (a,b) -> max (dim a) (dim b)
  | `TranslationOnto (a,b) -> max (dim a) (dim b)
  | `Tiling (a,k,l) -> dim a
  | `PeriodicFactor (_,a,b) -> max (dim a) (dim b)
  | `FillResizeAlike (_,a,b,c) -> max (dim a) (max (dim b) (dim c))
  | `Compose (a,b,c) -> max (dim a) (max (dim b) (dim c))
  | `ApplySym (sym,a,r) -> dim a
  | `UnfoldSym (sym_arr,a) -> dim a
  | `CloseSym (sym_seq,a,b) -> max (dim a) (dim b)
  | `TranslationSym (sym,a,b) -> max (dim a) (dim b)
  | `MajorityColor a -> dim a
  | `ColorCount a -> dim a
  | `Coloring (a,b) -> max (dim a) (dim b)
  | `SwapColors (a,b,c) -> max (dim a) (max (dim b) (dim c))
    
let rec template_dim : template -> dim = function
  | `Any -> Item
  | #patt as patt -> patt_dim template_dim patt
  | #expr as e -> expr_dim template_dim e
  | `Seq items -> Sequence
  | `Cst item0 -> Sequence
  | `Prefix (main,items) -> Sequence
                     
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
      | `Layer _ -> Layer
      | `Grid -> Grid)
  | `Item (_,p1) -> path_kind p1
  | `AnyItem p1 -> path_kind p1
  | `Arg (i,None,p1) -> path_kind p1
  | `Arg (i, Some role, p1) -> kind_of_role role

let rec path_role (p : revpath) : role =
  match p with
  | `Root -> `Grid
  | `Field ((`I | `J as f), p1) -> `IntCoord (f, path_role_vec p1)
  | `Field (`Color, p1) -> `Color (path_role_frame p1)
  | `Field (`Mask, _) -> `Mask
  | `Field (`Pos, _) -> `Vec `Pos
  | `Field (`Size, p1) -> `Vec (`Size (path_role_frame p1))
  | `Field (`Shape, _) -> `Shape
  | `Field (`Layer _, _) -> `Layer
  | `Field (`Grid, _) -> `Grid
  | `Item (_, p1) -> path_role p1
  | `AnyItem p1 -> path_role p1
  | `Arg (i, None, p1) -> path_role p1
  | `Arg (i, Some role, p1) -> role
and path_role_vec : revpath -> role_vec = function
  | `Root -> assert false
  | `Field (`Pos, _) -> `Pos
  | `Field (`Size, p1) -> `Size (path_role_frame p1)
  | `Item (i, p1) -> path_role_vec p1
  | `AnyItem p1 -> path_role_vec p1
  | `Arg (i, None, p1) -> path_role_vec p1
  | p ->
     pp_path p; print_newline ();
     assert false
and path_role_frame : revpath -> role_frame = function
  | `Root -> `Grid
  | `Field (`Shape, _) -> `Shape
  | `Item (i, p1) -> path_role_frame p1
  | `AnyItem p1 -> path_role_frame p1
  | `Arg (i, None, p1) -> path_role_frame p1
  | p -> pp_path p; print_newline (); assert false

let path_is_refinable : revpath -> bool = function
  | `Field (`Grid, `Root) -> false (* the Grid field of Background is not refinable, not really part of the model *)
  | _ -> true
         
let find_ilist (lp : ilist_revpath) (l : 'a ilist) : 'a option = (* QUICK *)
  let rec aux lp =
    match lp with
    | `Root -> l
    | `Left lp1 ->
       ( match aux lp1 with
         | `Nil -> `Nil
         | `Insert (left,_,_) -> left
         | `Append (left,_) -> left)
    | `Right lp1 ->
       ( match aux lp1 with
         | `Nil -> `Nil
         | `Insert (_,_,right) -> right
         | `Append (_,right) -> right)
  in
  match aux lp with
  | `Nil -> None
  | `Insert (_,elt,_) -> Some elt
  | `Append (_,_) -> None
  
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
  | `Size, `Grid g -> Some (`Vec (`Int g.height, `Int g.width))
  | `Grid, `Background (grid,size,color,layers) -> Some grid
  | `Size, `Background (grid,size,color,layers) -> Some size
  | `Color, `Background (grid,size,color,layers) -> Some color
  | `Layer lp, `Background (grid,size,color,layers) -> find_ilist lp layers
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
      | Some (#patt as patt1) ->
         if i = 0
         then Some (patt1 :> data) (* data seen as singleton sequence *)
         else None
      | Some (`Seq items) -> List.nth_opt items i)
  | `AnyItem p1 -> find_data p1 d (* should be a sequence *)
  | `Arg _ -> assert false
  
(* let rec find_template (p : revpath) (t : template) : template option =
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
  | `Arg _ -> assert false) *)


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
  | `Grid g ->
     let acc = fold acc (p ++ `Size) (`Vec (`Int g.height, `Int g.width)) patt_ancestry in
     acc
  | `Background (grid,size,color,layers) ->
     let acc = fold acc (p ++ `Grid) grid patt_ancestry in
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
  | `Any -> acc
  | #patt as patt ->
     fold_patt
       (fun acc p t anc -> fold_template f acc p t anc)
       acc p patt t_ancestry
  | #expr -> acc
  | `Seq items ->
     let _, acc =
       items
       |> List.fold_left
            (fun (i,acc) item ->
              i+1, fold_template f acc (`Item (i,p)) item t_ancestry)
            (0,acc) in
     acc
  | `Cst item0 ->
     fold_template f acc (`Item (0,p)) item0 t_ancestry (* p points both at sequence and item levels *)
  | `Prefix (main,items) ->
     let acc = fold_template f acc (`AnyItem p) main t_ancestry in
     let n, acc =
       let$ (i, acc), item = (0,acc), items in
       i+1, fold_template f acc (`Item (i,p)) item t_ancestry in
     acc
       
let size_of_data (d : data) : int =
  fold_data (fun res _ _ _ -> res+1) 0 path0 d []
let size_of_template (t : template) : int =
  fold_template (fun res _ _ _ -> res+1) 0 path0 t []

let rec template_is_ground : template -> bool = function
  (* returns true if the template evaluates into ground data, hence can be casted as [data]  *)
  | `Any -> false
  | #patt as patt -> patt_is_ground template_is_ground patt
  | #expr -> true (* assuming expressions are only made of functions and refs *)
  | `Seq items -> List.for_all template_is_ground items
  | `Cst item0 -> template_is_ground item0 (* if true, Cst superfluous *)
  | `Prefix (main,items) -> template_is_ground main (* if true, Prefix superfluous *)
and patt_is_ground (is_ground : 'a -> bool) : 'a patt -> bool = function
  | `Bool _ | `Int _ | `Color _ | `Mask _ -> true
  | `Vec (i,j) -> is_ground i && is_ground j
  | `Point c -> is_ground c
  | `Rectangle (s,c,m) -> is_ground s && is_ground c && is_ground m
  | `PosShape (p,sh) -> is_ground p && is_ground sh
  | `Grid g -> true
  | `Background (_g,s,c,layers) ->
     is_ground s && is_ground c
     && fold_ilist (fun res lp layer -> res && is_ground layer) true `Root layers

let signature_of_template (t : template) : signature = (* QUICK *)
  let ht = Hashtbl.create 13 in
  let () =
    fold_template
      (fun () p t1 anc1 ->
        match t1 with
        | `Background _ -> () (* not using full grid descriptions *)
        | `Cst _ (* only item0 matters for constant sequences *)
          | `Prefix _ (* this path is redundant with main *)
          -> ()
        | _ ->
           let k = path_kind p in
           let ps0 =
             match Hashtbl.find_opt ht k with
             | None -> []
             | Some ps -> ps in
           let ps = p::ps0 in
           Hashtbl.replace ht k ps)
      () path0 t [] in
  Hashtbl.fold
    (fun k ps res -> (k, List.rev ps)::res) (* reverse to put them in order *)
    ht []

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
let default_mask = `Mask `Full
let default_shape = `Rectangle (default_shape_size, default_shape_color, default_mask)
let default_object = `PosShape (default_pos, default_shape)
let default_layer = default_object
let default_grid_raw = `Grid (Grid.make 10 10 Grid.black)
let default_grid = `Background (default_grid_raw,
                                default_grid_size, default_grid_color, `Nil)
let default_data_of_path (p : revpath) : data =
  match path_role p with
  | `IntCoord (_, `Pos) -> `Int 0
  | `IntCoord (_, `Size `Grid) -> `Int 10
  | `IntCoord (_, `Size `Shape) -> `Int 2
  | `IntCoord (_, `Move) -> `Int 0
  | `IntCard -> `Int 2
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

let rec root_template_of_data ~(in_output : bool) (d : data) : (template * bool (* partial *)) list = (* QUICK *)
  match d with
  | `Bool _ -> []
  | `Int i as d ->
     if in_output && i >= 1 && i <= 3
     then [(d :> template), false]
     else [] (* position- and size-invariance of inputs *)
  | `Color _ as d -> [(d :> template), false] (* colors can be seen as patterns *)
  | `Mask _ as d -> [(d :> template), false] (* masks can be seen as patterns *)
  | `Vec _ -> [`Vec (u_cst, u_cst), false]
  | `Point _ -> [`Point u_cst, false]
  | `Rectangle _ -> [`Rectangle (u_vec_cst, u_cst, u_cst), false]
  | `PosShape _ -> [`PosShape (u_vec_cst, u_cst), false]
  | `Grid _ -> [] (* no literal grids in model *)
  | `Background _ -> assert false (* single grid constructor in models anyway *)
  | `Seq [] -> []
  | `Seq (item::items) ->
     let n, common_patterns =
       List.fold_left
         (fun (n,ltk) item ->
           let item_patterns = root_template_of_data ~in_output item in
           n+1,
           List.filter_map
             (fun (t,k) ->
               if List.mem_assoc t item_patterns
               then Some (t, k+1)
               else None)
             ltk)
         (1,
          root_template_of_data ~in_output item
          |> List.map (fun (t,_) -> t, 1))
         items in
     common_patterns
     |> List.filter_map (fun (t,k) ->
            if float k /. float n >= !def_match_threshold
            then Some (t, (k<n))
            else None)


(* returning matchers from templates, i.e. functions (data -> bool) *)

type matcher = Matcher of (data -> bool * matcher) [@@unboxed]
(* the returned matcher is for the next sequence items, if any *)

let rec matcher_success : matcher =
  Matcher (fun _ -> true, matcher_success)
let rec matcher_fail : matcher =
  Matcher (fun _ -> false, matcher_fail)
                          
let matcher_collect (Matcher matcher_item : matcher) : matcher =
  let rec aux matcher_item = function
    | [] -> true
    | item::items ->
       let ok1, Matcher next_matcher = matcher_item item in
       let ok2 = aux next_matcher items in
       ok1 && ok2
  in
  Matcher (function
      | `Seq items ->
         assert (items <> []);
         aux matcher_item items, matcher_fail
      | d ->
         let ok, _ = matcher_item d in
         ok, matcher_fail)
  
let matcher_patt (matcher : 'a -> matcher) (patt : 'a patt) : matcher =
  match patt with
  | `Bool b ->
     let rec matcher_bool =
       Matcher (function
           | `Bool db -> db = b, matcher_bool
           | _ -> false, matcher_fail) in
     matcher_bool
  | `Int i ->
     let rec matcher_int =
       Matcher (function
           | `Int di -> di = i, matcher_int
           | _ -> false, matcher_fail) in
     matcher_int
  | `Color c ->
     let rec matcher_color =
       Matcher (function
           | `Color dc -> dc = c, matcher_color
           | _ -> false, matcher_fail) in
     matcher_color
  | `Mask m ->
     let rec matcher_mask =
       Matcher (function
           | `Mask dm -> Mask_model.subsumes m dm, matcher_mask
           | _ -> false, matcher_fail) in
     matcher_mask
  | `Vec (i,j) ->
     let rec matcher_vec (Matcher matcher_i) (Matcher matcher_j) =
       Matcher (function
           | `Vec (di,dj) ->
              let ok1, next_i = matcher_i di in
              let ok2, next_j = matcher_j dj in
              ok1 && ok2, matcher_vec next_i next_j
           | _ -> false, matcher_fail) in
     matcher_vec (matcher i) (matcher j)
  | `Point color ->
     let rec matcher_point (Matcher matcher_color) =
       Matcher (function
           | `Point dcolor ->
              let ok1, next_color = matcher_color dcolor in
              ok1, matcher_point next_color
           | _ -> false, matcher_fail) in
     matcher_point (matcher color)
  | `Rectangle (size,color,mask) ->
     let rec matcher_rectangle (Matcher matcher_size) (Matcher matcher_color) (Matcher matcher_mask) =
       Matcher (function
           | `Rectangle (dsize,dcolor,dmask) ->
              let ok1, next_size = matcher_size dsize in
              let ok2, next_color = matcher_color dcolor in
              let ok3, next_mask = matcher_mask dmask in
              ok1 && ok2 && ok3, matcher_rectangle next_size next_color next_mask
           | _ -> false, matcher_fail) in
     matcher_rectangle (matcher size) (matcher color) (matcher mask)
  | `PosShape (pos,shape) ->
     let rec matcher_ps (Matcher matcher_pos) (Matcher matcher_shape) =
       Matcher (function
           | `PosShape (dpos,dshape) ->
              let ok1, next_pos = matcher_pos dpos in
              let ok2, next_shape = matcher_shape dshape in
              ok1 && ok2, matcher_ps next_pos next_shape
           | _ -> false, matcher_fail) in
     matcher_ps (matcher pos) (matcher shape)
  | `Grid g ->
     let rec matcher_grid =
       Matcher (function
           | `Grid dg | `Background (`Grid dg, _, _, _) -> Grid.same g dg, matcher_grid
           | _ -> false, matcher_fail) in
     matcher_grid
  | `Background (_grid,size,color,layers) ->
     let Matcher matcher_size = matcher size in
     let Matcher matcher_color = matcher color in
     let ilist_matcher_layers =
       map_ilist
         (fun lp layer -> matcher_collect (matcher layer))
         `Root layers in
     Matcher (function
         | `Background (_dgrid,dsize,dcolor,dlayers) ->
            let ok1, _ = matcher_size dsize in
            let ok2, _ = matcher_color dcolor in
            let ok3 =
              match
                fold2_ilist
                  (fun ok_all lp (Matcher matcher_layer) dlayer ->
                    let ok, _ = matcher_layer dlayer in
                    Result.Ok (ok_all && ok))
                  true `Root ilist_matcher_layers dlayers
              with
              | Result.Ok ok3 -> ok3
              | Result.Error _ -> false in (* ilists with different structure *)
            ok1 && ok2 && ok3, matcher_fail
         | _ -> false, matcher_fail)


let rec matcher_template_aux (t : template) : matcher =
  match t with
  | `Any -> matcher_success
  | #patt as patt -> matcher_patt matcher_template_aux patt
  | #expr -> assert false
  | `Seq items ->
     let rec matcher_seq = function
       | [] -> matcher_fail
       | (Matcher matcher_item)::next_matchers ->
          Matcher (fun d ->
              let ok1, _ = matcher_item d in
              ok1, matcher_seq next_matchers) in
     matcher_seq (List.map matcher_template_aux items)
  | `Cst item0 ->
     let Matcher matcher_item0 = matcher_template_aux item0 in
     let rec matcher_cst = function
       | None ->
          Matcher (fun d ->
              let ok1, _next = matcher_item0 d in
              ok1, matcher_cst (Some d))
       | Some d0 as hist ->
          Matcher (fun d ->
              d = d0, matcher_cst hist) in
     matcher_cst None     
  | `Prefix (main,items) ->
     let matcher_main = matcher_template_aux main in
     let rec matcher_prefix = function
       | [] -> matcher_main
       | (Matcher matcher_item)::next_matchers ->
          Matcher (fun d ->
              let ok1, _ = matcher_item d in
              ok1, matcher_prefix next_matchers) in
     matcher_prefix (List.map matcher_template_aux items)

type match_result = Yes of bool (* partial: not all items match *) | No
     
let matches_template (t : template) : data -> match_result =
  let t_dim = template_dim t in
  let Matcher matcher = matcher_collect (matcher_template_aux t) in
  fun d ->
  match t_dim, d with
  | Item, `Seq items ->
     let k, n =
       List.fold_left
         (fun (k,n) item ->
           let ok, _ = matcher item in
           if ok then (k+1,n+1) else (k,n+1))
         (0,0) items in
     if n > 0 && float k /. float n >= !def_match_threshold
     then Yes (k < n)
     else No
  | _ ->
     let ok, _ = matcher d in
     if ok then Yes false else No

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
let dl_mask : Mask_model.t -> dl =
  function
  | `Full -> Mdl.Code.usage 0.5 (* TODO: give equal prob to all specific masks ? *)
  | `Border -> Mdl.Code.usage 0.1
  | `EvenCheckboard
    | `OddCheckboard
    | `PlusCross
    | `TimesCross -> Mdl.Code.usage 0.025
  | `Mask m ->
     let h, w = Grid.dims m in
     let n = h * w in
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
       | `IntCoord (`I, `Pos) -> dl_int_pos ~bound:ctx.box_height n
       | `IntCoord (`J, `Pos) -> dl_int_pos ~bound:ctx.box_width n
       | `IntCoord (`I, `Size _) -> dl_int_size ~bound:ctx.box_height n
       | `IntCoord (`J, `Size _) -> dl_int_size ~bound:ctx.box_width n
       | `IntCoord (_, `Move) -> assert false (* only computation intermediate value *)
       | `IntCard -> assert false (* only computation intermediate value *)
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
     dl ~ctx pos ~path:(path ++ `Pos)
     +. dl ~ctx shape ~path:(path ++ `Shape)

  | `Grid g -> assert false (* so far, not used in models *)
    
  | `Background (_grid,size,color,layers) ->
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
       (if !seq && path_dim path = Sequence then Mdl.Code.universal_int_star 1 else 0.) (* encoding d as singleton sequence *)
       +. dl_patt_as_template (* NOTE: to align with dl_template on patterns *)
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
       let dl =
         try aux ~ctx ~path d
         with exn ->
           print_string "dl_data: assertion failed: ";
           pp_data d; print_string " @ "; pp_path path;
           print_newline ();
           raise exn in
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
  | `AnyItem p1 -> 1 + dl_path_length p1
  | `Arg (_,_,p1) -> dl_path_length p1 (* expr args ignored *)
and dl_ilist_path_length = function
  | `Root -> 0
  | `Left p1 -> 1 + dl_ilist_path_length p1
  | `Right p1 -> 1 + dl_ilist_path_length p1
          
let rec dl_path_role (role : role) (p : revpath) : dl =
  (* assuming [p] does not contain [`Arg] *)
  (* assuming the length of [p] is known => no need to code for `Root *)
  (* TODO: how to cleanly encode choice between Field/Item/AnyItem *)
  match role with
  | `IntCoord (rij,rvec) -> dl_path_int rij rvec  p
  | `IntCard -> dl_path_card p
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
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_int rij rvec p1
  | `AnyItem p1 ->
     dl_path_int rij rvec p1
  | _ -> assert false
and dl_path_card = function
  | `Root -> 0.
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_card p1
  | `AnyItem p1 ->
     dl_path_card p1
  | _ -> assert false
and dl_path_color = function
  | `Root -> 0.
  | `Field (`Color, p1) ->
     dl_path_shape p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_color p1
  | `AnyItem p1 -> dl_path_color p1
  | _ -> assert false
and dl_path_mask = function
  | `Root -> 0.
  | `Field (`Mask, p1) ->
     dl_path_shape p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_mask p1
  | `AnyItem p1 ->
     dl_path_mask p1
  | _ -> assert false
and dl_path_vec (rvec : role_vec) = function
  | `Root -> 0.
  | `Field ((`Pos|`Size as f), p1) ->
     let dl_choice =
       match rvec, f with
       | `Pos, `Pos 
         | `Size _, `Size -> Mdl.Code.usage 0.9
       | _ -> Mdl.Code.usage 0.1 in
     dl_choice
     +. ( match f with
          | `Pos -> dl_path_layer p1
          | `Size -> dl_path_shape p1 )
  | `Item (i, p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_vec rvec p1
  | `AnyItem p1 ->
     dl_path_vec rvec p1
  | _ -> assert false
and dl_path_shape = function
  | `Root -> 0.
  | `Field (`Shape, p1) ->
     dl_path_layer p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_shape p1
  | `AnyItem p1 ->
     dl_path_shape p1
  | _ -> assert false
and dl_path_layer = function
  | `Root -> 0.
  | `Field (`Layer lp, p1) ->
     Mdl.Code.universal_int_star (dl_ilist_path_length lp)
     +. dl_ilist_path lp
     +. dl_path_grid p1
  | `Item (i,p1) ->
     Mdl.Code.universal_int_star i
     +. dl_path_layer p1
  | `AnyItem p1 ->
     dl_path_layer p1
  | _ -> assert false
and dl_path_grid = function
  | `Root -> 0.
  | `Field (`Grid, p1) ->
     dl_path_grid p1
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
              `Area `X; `ColorCount `X;
              (*`Left `X; *) `Right `X; `Center `X;
              (*`Top `X; *) `Bottom `X; `Middle `X;
              `Plus (`X,`X); `Minus (`X,`X); (*`Modulo (`X,`X);*)
              `IncrInt (`X,1); `DecrInt (`X,1);
              `ScaleUp (`X,`X); `ScaleDown (`X,`X);
              `Min [`X;`X]; `Max [`X;`X]; `Average [`X;`X]; `Span (`X,`X);
              (* `Norm `X; `Diag1 (`X,2); `Diag2 (`X,2);*) ])
    ~bool:(uniform_among [])
    ~color:(uniform_among [
                `MajorityColor `X ])
    ~mask:(uniform_among [
               `MaskOfGrid `X;
               `ScaleUp (`X,`X); `ScaleDown (`X,`X); `ScaleTo (`X,`X);
               `Tiling (`X,1,1); `PeriodicFactor (`TradeOff,`X,`X); `FillResizeAlike (`TradeOff,`X,`X,`X); `Compose (`X,`X,`X);
               `ApplySym (`FlipHeight, `X, `Mask);
               `UnfoldSym ([], `X); `CloseSym ([], `X, `X);
               `LogAnd (`X,`X); `LogOr (`X,`X); `LogXOr (`X,`X);
               `LogAndNot (`X,`X); `LogNot `X ])
    ~vec:(uniform_among [
              `ProjI `X; `ProjJ `X;
              `ConstVec (0,0);
              `Plus (`X,`X); `Minus (`X,`X);
              `IncrVec (`X,1,1); `DecrVec (`X,1,1);
              `ScaleUp (`X,`X); `ScaleDown (`X,`X);
              `Tiling (`X,1,1);
              `Corner (`X,`X); `Min [`X; `X]; `Max [`X;`X];`Average [`X;`X]; `Span (`X,`X);
              `TranslationOnto (`X,`X);
              `TranslationSym (`FlipHeight,`X,`X) ])
    ~shape:(uniform_among [
                `ScaleUp (`X,`X); `ScaleDown (`X,`X); `ScaleTo (`X,`X);
                `Tiling (`X,1,1); `PeriodicFactor (`TradeOff,`X,`X);  `FillResizeAlike (`TradeOff,`X,`X,`X); `Compose (`X,`X,`X);
                `ApplySym (`FlipHeight, `X, `Shape);
                `UnfoldSym ([], `X); `CloseSym ([], `X, `X);
                `Coloring (`X,`X) ])
    ~object_:(uniform_among [ ])
    ~layer:(uniform_among [
                `PeriodicFactor (`TradeOff,`X,`X); `FillResizeAlike (`TradeOff,`X,`X,`X);
                `ApplySym (`FlipHeight, `X, `Layer);
                `UnfoldSym ([], `X); `CloseSym ([], `X, `X);
                `Coloring (`X,`X) ])
    ~grid:(uniform_among [
               `GridOfMask (`X,`X);
               `SwapColors (`X,`X,`X);
               `ScaleUp (`X,`X); `ScaleDown (`X,`X); `ScaleTo (`X,`X);
               `Crop (`X,`X); `Strip `X;
               `Tiling (`X,1,1); `PeriodicFactor (`TradeOff,`X,`X); `FillResizeAlike (`TradeOff,`X,`X,`X); `Compose (`X,`X,`X);
               `ApplySym (`FlipHeight, `X, `Mask);
               `UnfoldSym ([], `X); `CloseSym ([], `X, `X) (* `Stack [`X; `X] *) ])
  
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
  | `ScaleUp (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,Some `IntCard,path)) e2
  | `ScaleDown (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,Some `IntCard,path)) e2
  | `ScaleTo (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
     +. dl ~ctx ~path:(`Arg (2,Some (`Vec (`Size `Shape)),path)) e2
  | `Crop (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg(1,None,path)) e1
     +. dl ~ctx ~path:(`Arg(2,Some `Layer,path)) e2
  | `Strip e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1,None,path)) e1
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
  | `Stack le1 ->
     code_expr
     +. Mdl.Code.universal_int_plus (List.length le1)
     +. Mdl.sum le1
          (fun e1 -> dl ~ctx ~path:(`Arg (1,None,path)) e1)
  | `Area e1 ->
     code_expr +. dl ~ctx ~path:(`Arg (1, Some `Shape, path)) e1
  | `Left e1 | `Right e1 | `Center e1
    | `Top e1 | `Bottom e1 | `Middle e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some `Layer, path)) e1
  | `ProjI e1 | `ProjJ e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, None, path)) e1
  | `MaskOfGrid e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some `Grid, path)) e1
  | `GridOfMask (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some `Mask, path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some (`Color `Shape), path)) e2
  | `TranslationOnto (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some `Layer, path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some `Layer, path)) e2
  | `Tiling (e1,k,l) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, None, path)) e1
     +. Mdl.Code.universal_int_plus k
     +. Mdl.Code.universal_int_plus l
  | `PeriodicFactor (mode,e1,e2) ->
     code_expr
     +. dl_periodicity_mode mode
     +. dl ~ctx ~path:(`Arg (1, Some (`Color `Grid), path)) e1
     +. dl ~ctx ~path:(`Arg (2, None, path)) e2
  | `FillResizeAlike (mode,e1,e2,e3) ->
     code_expr
     +. dl_periodicity_mode mode
     +. dl ~ctx ~path:(`Arg (1, Some (`Color `Grid), path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some (`Vec (`Size `Grid)), path)) e2
     +. dl ~ctx ~path:(`Arg (3, None, path)) e3
  | `Compose (e1,e2,e3) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some (`Color `Grid), path)) e1
     +. dl ~ctx ~path:(`Arg (2, None, path)) e2
     +. dl ~ctx ~path:(`Arg (3, None, path)) e3
  | `ApplySym (sym,e1,role_e1) ->
     code_expr (* no need to encode role_e1, deducible from model *)
     +. Mdl.Code.uniform nb_symmetry (* encoding sym *)
     +. dl ~ctx ~path:(`Arg (2, Some role_e1, path)) e1
  | `UnfoldSym (sym_array,e1) ->
     code_expr (* includes encoding of sym list list *)
     +. Mdl.Code.uniform nb_symmetry_unfold (* encoding the choice of the symmetry matrix *)
     +. dl ~ctx ~path:(`Arg (2, None, path)) e1
  | `CloseSym (sym_seq,e1,e2) ->
     code_expr
     +. Mdl.Code.uniform nb_symmetry_close (* encoding sym_seq *)
     +. dl ~ctx ~path:(`Arg (2, Some (`Color `Grid), path)) e1
     +. dl ~ctx ~path:(`Arg (3, None, path)) e2
  | `TranslationSym (sym,e1,e2) ->
     code_expr
     +. Mdl.Code.uniform nb_symmetry (* encoding sym *)
     +. dl ~ctx ~path:(`Arg (2, Some `Layer, path)) e1
     +. dl ~ctx ~path:(`Arg (3, Some `Layer, path)) e2 (* TODO: can be a Grid too *)
  | `MajorityColor e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some `Grid, path)) e1
  | `ColorCount e1 ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, Some `Grid, path)) e1
  | `Coloring (e1,e2) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, None, path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some (`Color `Shape), path)) e2
  | `SwapColors (e1,e2,e3) ->
     code_expr
     +. dl ~ctx ~path:(`Arg (1, None, path)) e1
     +. dl ~ctx ~path:(`Arg (2, Some (`Color `Grid), path)) e2
     +. dl ~ctx ~path:(`Arg (3, Some (`Color `Grid), path)) e3
and dl_periodicity_mode = function
  | `Total -> Mdl.Code.usage 0.25
  | `Strict -> Mdl.Code.usage 0.25
  | `TradeOff -> Mdl.Code.usage 0.5

type code_template = (* dls must correspond to a valid prob distrib *)
  { c_any : dl;
    c_patt : dl;
    c_ref : dl;
    c_expr : dl;
    c_seq : dl;
    c_cst : dl;
    c_prefix : dl }
let code_template0 =
  { c_any = dl_patt_as_template (* Mdl.Code.usage 0.2 *);
    c_patt = dl_patt_as_template (* Mdl.Code.usage 0.2 *);
    c_ref = Mdl.Code.usage 0.3;
    c_expr = Mdl.Code.usage 0.2;
    c_seq = Mdl.Code.usage 0.03;
    c_cst = Mdl.Code.usage 0.04;
    c_prefix = Mdl.Code.usage 0.03 }

let code_template_by_kind : code_template KindMap.t =
  KindMap.make (* TODO: customize sequence constructions *)
    ~int:code_template0
    ~bool:code_template0
    ~color:code_template0
    ~mask:code_template0
    ~vec:code_template0
    ~shape:code_template0
    ~object_:code_template0
    ~layer:code_template0
    ~grid:code_template0

let dl_template ~(env_sig : signature) ~(ctx : dl_ctx) ?(path = `Root) (t : template) : dl = (* QUICK *)
  let rec aux ~ctx ~path t =
    let k = path_kind path in
    let code = KindMap.find k code_template_by_kind in
    match t with
    | `Any ->
       code.c_any
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
       let n, dl_items =
         let$ (i,sum_dl), item = (0,0.), items in
         i+1, aux ~ctx ~path:(`Item (i,path)) item in
       (* TODO: should encode item templates according to main template ! *)
       code.c_seq
       +. Mdl.Code.universal_int_star n (* how many items in sequence *)
       +. dl_items (* coding items as templates *)
    | `Cst item0 ->
       code.c_cst
       +. aux ~ctx ~path:(`Item (0,path)) item0
    | `Prefix (main,items) ->
       let dl_main = aux ~ctx ~path:(`AnyItem path) main in
       let n, dl_items =
         let$ (i,sum_dl), item = (0,0.), items in
         i+1, aux ~ctx ~path:(`Item (i,path)) item in
       (* TODO: should encode item template according to main template ! *)
       code.c_prefix
       +. dl_main (* coding the main template *)
       +. Mdl.Code.universal_int_star n (* how many items in prefix *)
       +. dl_items (* coding prefix items as templates *)
  in
  aux ~ctx ~path t

(* returning encoders from templates/patterns M, i.e. functions computing L(D|M) *)


type encoder = Encoder of (data -> dl * encoder) [@@unboxed] (* the returned encoder is for the next sequence items, if any *)

let rec encoder_zero : encoder =
  Encoder (fun _ -> 0., encoder_zero)
let encoder_fail : encoder =
  Encoder (fun _ -> assert false)
let encoder_lift (dl : 'a -> dl) : encoder =
  Encoder (fun d -> dl d, encoder_fail)
let encoder_pop (Encoder encoder) (d : data) : dl =
  let dl, _ = encoder d in
  dl

let encoder_seq (make_encoder : int -> 'a -> encoder) (items : 'a list) : encoder =
  assert (items <> []);
  let rec aux = function
    | [] -> encoder_fail
    | (Encoder encoder_item)::next_encoders ->
       Encoder (fun d ->
           let dl, _ = encoder_item d in
           dl, aux next_encoders)
  in
  let encoder_items = List.mapi make_encoder items in
  aux encoder_items

let encoder_collect (Encoder encoder_item : encoder) : encoder =
  if !seq
  then
    let rec aux encoder_item n dl = function
      | [] -> n, dl
      | item::items ->
         let dl1, Encoder next_encoder = encoder_item item in
         aux next_encoder (n+1) (dl +. dl1) items
    in
    Encoder (fun d ->
        let n, dl =
          match d with
          | `Seq items ->
             assert (items <> []);
             aux encoder_item 0 0. items
          | d ->
             let dl, _ = encoder_item d in
             1, dl in
        Mdl.Code.universal_int_plus n +. dl, encoder_fail)
  else
    Encoder (fun d ->
        let dl, _ = encoder_item d in
        dl, encoder_fail)
  
let encoder_patt
          (encoder : ctx:dl_ctx -> path:revpath -> 'a -> encoder)
      ~ctx ~(path : revpath) (patt : 'a patt) : encoder =
  match patt with
  | `Bool b -> encoder_zero
  | `Int i -> encoder_zero
  | `Color c -> encoder_zero
  | `Mask m -> encoder_zero
  | `Vec (i,j) ->
     let rec encoder_vec (Encoder encoder_i) (Encoder encoder_j) =
       Encoder (function
           | `Vec (di,dj) ->
              let dl1, next_i = encoder_i di in
              let dl2, next_j = encoder_j dj in
              dl1 +. dl2, encoder_vec next_i next_j
           | _ -> assert false) in
     encoder_vec
       (encoder ~ctx ~path:(path ++ `I) i)
       (encoder ~ctx ~path:(path ++ `J) j)
  | `Point color ->
     let rec encoder_point (Encoder encoder_color) =
       Encoder (function
           | `Point dcolor ->
              let dl1, next_color = encoder_color dcolor in
              dl1, encoder_point next_color
           | _ -> assert false) in
     encoder_point
       (encoder ~ctx ~path:(path ++ `Color) color)
  | `Rectangle (size,color,mask) ->
       let rec encoder_rectangle (Encoder encoder_size) (Encoder encoder_color) (Encoder encoder_mask) =
         Encoder (function
             | `Rectangle (dsize,dcolor,dmask) ->
                let dl1, next_size = encoder_size dsize in
                let dl2, next_color = encoder_color dcolor in
                let dl3, next_mask = encoder_mask dmask in
                dl1 +. dl2 +. dl3, encoder_rectangle next_size next_color next_mask
             | _ -> assert false) in
       encoder_rectangle
         (encoder ~ctx ~path:(path ++ `Size) size)
         (encoder ~ctx ~path:(path ++ `Color) color)
         (encoder ~ctx ~path:(path ++ `Mask) mask)
  | `PosShape (pos,shape) ->
     let rec encoder_posshape (Encoder encoder_pos) (Encoder encoder_shape) =
       Encoder (function
           | `PosShape (dpos,dshape) ->
              let dl1, next_pos = encoder_pos dpos in
              let dl2, next_shape = encoder_shape dshape in
              dl1 +. dl2, encoder_posshape next_pos next_shape
           | _ -> assert false) in
     encoder_posshape
       (encoder ~ctx ~path:(path ++ `Pos) pos)
       (encoder ~ctx ~path:(path ++ `Shape) shape)
  | `Grid g -> assert false (* so far, not used in models *)
  | `Background (_grid,size,color,layers) ->
     let Encoder encoder_size = encoder ~ctx ~path:(path ++ `Size) size in
     let Encoder encoder_color = encoder ~ctx ~path:(path ++ `Color) color in
     let ilist_encoder_layers =
       map_ilist
         (fun lp layer -> encoder_collect (encoder ~ctx ~path:(path ++ `Layer lp) layer))
         `Root layers in
     Encoder (function
         | `Background (_dgrid,dsize,dcolor,dlayers) ->
            let dl1, _ = encoder_size dsize in
            let dl2, _ = encoder_color dcolor in
            let dl3 =
              match
                fold2_ilist
                  (fun sum lp (Encoder encoder_layer) dlayer ->
                    let dl, _ = encoder_layer dlayer in
                    Result.Ok (sum +. dl))
                  0. `Root ilist_encoder_layers dlayers
              with
              | Result.Ok dl3 -> dl3
              | Result.Error _ -> assert false in
            dl1 +. dl2 +. dl3, encoder_fail
         | _ -> assert false)

let rec encoder_any ~ctx ~path =
  Encoder (fun d -> dl_data ~ctx ~path d, encoder_any ~ctx ~path)
(* TODO: compute once [dl_data ~ctx ~path], dl_data should return an encoder too ? *)

let encoder_cst (Encoder encoder_item) =
  Encoder (fun d ->
      let dl, _ = encoder_item d in
      dl, encoder_zero) (* enough to encode the first, all other items are the same *)

let rec encoder_prefix (encoder_main : encoder) (encoders_item : encoder list) : encoder =
  match encoders_item with
  | [] -> encoder_main
  | (Encoder encoder_item)::next_encoders ->
     Encoder (fun d ->
         let dl, _ = encoder_item d in
         dl, encoder_prefix encoder_main next_encoders)
    
let rec encoder_template_aux ~(ctx : dl_ctx) ~(path : revpath) (t : template) : encoder =
  match t with
  | `Any -> encoder_any ~ctx ~path
  | #patt as patt -> encoder_patt encoder_template_aux ~ctx ~path patt
  | #expr -> encoder_zero (* nothing to code *)
  | `Seq items -> encoder_seq (fun i item -> encoder_template_aux ~ctx ~path:(`Item (i,path)) item) items
  | `Cst item0 -> encoder_cst (encoder_template_aux ~ctx ~path:(`Item (0,path)) item0)
  | `Prefix (main,items) ->
     encoder_prefix
       (encoder_template_aux ~ctx ~path main)
       (List.mapi (fun i item -> encoder_template_aux ~ctx ~path:(`Item (i,path)) item) items)
     
let encoder_template ~(ctx : dl_ctx) ?(path : revpath = `Root) (t : template) : data -> dl =
  let encoder = encoder_template_aux ~ctx ~path t in
  let Encoder encoder =
    if !seq && path_dim path = Sequence
    then encoder_collect encoder
    else encoder in
  fun d ->
  let dl, _ = encoder d in
  dl

let dl_parse_rank (rank : int) : dl =
  (* penalty DL for parse rank, starting at 0 *)
  Mdl.Code.universal_int_star rank -. 1.
  
  
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
let dl_delta_shape = `PosShape (`Vec (`Int 0, `Int 0), `Rectangle (`Vec (`Int 1, `Int 1), `Color Grid.blue, `Mask `Full)) (* dummy point shape as rectangle *)
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
  | (`Bool _ | `Int _ | `Color _ | `Mask _ | `Grid _ as d) -> Result.Ok d
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
  | `Background (grid,size,color,layers) ->
     let| rgrid = apply ~lookup (p ++ `Grid) grid in
     let| rsize = apply ~lookup (p ++ `Size) size in
     let| rcolor = apply ~lookup (p ++ `Color) color in
     let| rlayers =
       map_ilist_result
         (fun lp shape -> apply ~lookup (p ++ `Layer lp) shape)
         `Root layers in
     Result.Ok (`Background (rgrid,rsize,rcolor,rlayers))

let mask_model_sym : symmetry -> (Mask_model.t -> Mask_model.t result) = function
  | `Id -> (fun g -> Result.Ok g)
  | `FlipHeight -> Mask_model.flipHeight
  | `FlipWidth -> Mask_model.flipWidth
  | `FlipDiag1 -> Mask_model.flipDiag1
  | `FlipDiag2 -> Mask_model.flipDiag2
  | `Rotate180 -> Mask_model.rotate180
  | `Rotate90 -> Mask_model.rotate90
  | `Rotate270 -> Mask_model.rotate270
                
let grid_sym : symmetry -> (Grid.t -> Grid.t) = function
    | `Id -> Fun.id
    | `FlipHeight -> Grid.Transf.flipHeight
    | `FlipWidth -> Grid.Transf.flipWidth
    | `FlipDiag1 -> Grid.Transf.flipDiag1
    | `FlipDiag2 -> Grid.Transf.flipDiag2
    | `Rotate180 -> Grid.Transf.rotate180
    | `Rotate90 -> Grid.Transf.rotate90
    | `Rotate270 -> Grid.Transf.rotate270
     
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
       let| mm' = mask_model_sym sym mm in
       Result.Ok (`Mask mm')
    | _ -> assert false in
  let sym_shape = function
    | `Point col -> Result.Ok (`Point col)
    | `Rectangle (size, col, mask) ->
       let| mask' = sym_mask mask in
       Result.Ok (`Rectangle (sym_size size, col, mask'))
    | _ -> assert false in
  let sym_grid = function
    | `Grid g ->
       let g' = grid_sym sym g in
       Result.Ok (`Grid g')
    | `Background _ ->
       Result.Error (Undefined_result "ApplySym not defined on Background")
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
  | `Grid, _ -> sym_grid d1
  | _ -> Result.Error (Invalid_expr e)

let unfold_any
      (concatHeight : 'a -> 'a -> 'a result)
      (concatWidth : 'a -> 'a -> 'a result)
      (apply_sym : symmetry -> 'a -> 'a)
      (sym_matrix : symmetry list list)
    : 'a -> 'a result =
  let rec gen_matrix : symmetry list list -> ('a -> 'a result) = function
    | [] -> assert false
    | [row] -> gen_row row
    | row::rows ->
       let g_row = gen_row row in
       let g_rows = gen_matrix rows in
       (fun x ->
         let| xrow = g_row x in
         let| xrows = g_rows x in
         concatHeight xrow xrows)
  and gen_row : symmetry list -> ('a -> 'a result) = function
    | [] -> assert false
    | [sym] -> (fun x -> Result.Ok (apply_sym sym x))
    | sym::syms ->
       let g_syms = gen_row syms in
       (fun x ->
         let xsym = apply_sym sym x in
         let| xsyms = g_syms x in
         concatWidth xsym xsyms) in
  gen_matrix sym_matrix

let unfold_size sym_matrix = function
  | `Vec (`Int h, `Int w) ->
     let| h', w' =
       unfold_any
         (fun (h1,w1) (h2,w2) ->
           if w1=w2 then Result.Ok (h1+h2, w1) else Result.Error (Undefined_result "unfold_size"))
         (fun (h1,w1) (h2,w2) ->
           if h1=h2 then Result.Ok (h1, w1+w2) else Result.Error (Undefined_result "unfold_size"))
         (fun _ (h,w) -> (h,w)) (* for symmetries returning (w,h), h=w must hold *)
         sym_matrix
         (h,w) in
     Result.Ok (`Vec (`Int h', `Int w'))
  | _ -> Result.Error (Undefined_result "Model2.unfold_size: not a size")

let unfold_grid sym_matrix g =
  unfold_any Grid.Transf.concatHeight Grid.Transf.concatWidth grid_sym sym_matrix g
let unfold_grid, reset_unfold_grid =
  Common.memoize2 ~size:101 unfold_grid                     

let unfold_mask_model sym_matrix = function
  | `Mask m ->
     let| m' = unfold_grid sym_matrix m in
     Result.Ok (`Mask m')
  | `Full -> Result.Ok `Full
  | _ -> Result.Error (Undefined_result "Model2.unfold_mask: not a custom mask")
let unfold_mask_model, reset_unfold_mask_model =
  Common.memoize2 ~size:101 unfold_mask_model                   

let unfold_symmetry (sym_matrix : symmetry list list) =
  fun e d ->
  match d with
  | `Mask mm ->
     let| mm = unfold_mask_model sym_matrix mm in
     Result.Ok (`Mask mm)
  | `Rectangle (size, col, `Mask mm) ->
     let| size = unfold_size sym_matrix size in
     let| mm = unfold_mask_model sym_matrix mm in
     Result.Ok (`Rectangle (size, col, `Mask mm))
  | `Point _ -> Result.Error (Undefined_result "Model2.unfold_symmetry: point")
  | `PosShape (pos, `Rectangle (size, col, `Mask mm)) ->
     let| size = unfold_size sym_matrix size in
     let| mm = unfold_mask_model sym_matrix mm in
     Result.Ok (`PosShape (pos, `Rectangle (size, col, `Mask mm)))
  | `PosShape (_, `Point _) -> Result.Error (Undefined_result "Model2.unfold_symmetry: point")
  | `Grid g ->
     let| g' = unfold_grid sym_matrix g in
     Result.Ok (`Grid g')
  | _ -> Result.Error (Invalid_expr e)

       
let close_any
      (stack : 'a list -> 'a result)
      (apply_sym : symmetry -> 'a -> 'a)
      (sym_seq : symmetry list)
    : 'a -> 'a result =
  let rec gen_seq : symmetry list -> ('a -> 'a result) = function
    | [] -> (fun x1 -> Result.Ok x1)
    | sym::syms ->
       let g = gen_seq syms in
       (fun x1 ->
         let y1 = apply_sym sym x1 in
         let| x2 = stack [x1; y1] in
         g x2) in
  gen_seq sym_seq
       
let close_grid sym_seq bgcolor g =
  let| g' = close_any (Grid.Transf.layers ~bgcolor) grid_sym sym_seq g in
  Result.Ok g'
let close_grid, reset_close_grid =
  Common.memoize3 ~size:101 close_grid

let close_mask_model sym_seq = function
  | `Mask m ->
     let| m' = close_grid sym_seq 0 m in
     Result.Ok (`Mask m)
  | `Full -> Result.Ok `Full
  | _ -> Result.Error (Undefined_result "Model2.close_symmetry: not a custom mask")
let close_mask_model, reset_close_mask_model =
  Common.memoize2 ~size:101 close_mask_model                   

let close_symmetry (sym_seq : symmetry list) (bgcolor : Grid.color) =
  fun e d ->
  match d with
  | `Mask mm ->
     let| mm = close_mask_model sym_seq mm in
     Result.Ok (`Mask mm)
  | `Rectangle (size, col, `Mask mm) ->
     let| mm = close_mask_model sym_seq mm in
     Result.Ok (`Rectangle (size, col, `Mask mm))
  | `Point _ -> Result.Error (Undefined_result "Model2.close_symmetry: point")
  | `PosShape (pos, `Rectangle (size, col, `Mask mm)) ->
     let| mm = close_mask_model sym_seq mm in
     Result.Ok (`PosShape (pos, `Rectangle (size, col, `Mask mm)))
  | `PosShape (_, `Point _) -> Result.Error (Undefined_result "Model2.close_symmetry: point")
  | `Grid g ->
     let| g = close_grid sym_seq bgcolor g in
     Result.Ok (`Grid g)
  | _ -> Result.Error (Invalid_expr e)

let reset_memoized_functions_apply () =
  reset_unfold_grid ();
  reset_unfold_mask_model ();
  reset_close_grid ();
  reset_close_mask_model ()

  
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
  | `ScaleUp (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (fun (d1,d2) ->
         match d2 with
         | `Int 0 -> Result.Error (Invalid_argument "ScaleUp: k=0") 
         | `Int k ->
            assert (k > 0);
            (match d1 with
             | `Int i1 -> Result.Ok (`Int (i1 * k))
             | `Vec (`Int i1, `Int j1) -> Result.Ok (`Vec (`Int (i1 * k), `Int (j1 * k)))
             | `Mask mm ->
                let| mm' = Mask_model.scale_up k k mm in
                Result.Ok (`Mask mm')
             | `Point col ->
                Result.Ok (`Rectangle (`Vec (`Int k, `Int k), col, `Mask `Full))
             | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm) ->
                let| mm' = Mask_model.scale_up k k mm in
                Result.Ok (`Rectangle (`Vec (`Int (h * k), `Int (w * k)), col, `Mask mm'))
             | `Grid g ->
                let| g' = Grid.Transf.scale_up k k g in
                Result.Ok (`Grid g')
             | _ -> Result.Error (Invalid_expr e))
         | _ -> Result.Error (Invalid_expr e))
  | `ScaleDown (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (fun (d1,d2) ->
         match d2 with
         | `Int 0 -> Result.Error (Invalid_argument "ScaleDown: k=0") 
         | `Int k ->
            assert (k > 0);
            (match d1 with
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
             | `Mask mm ->
                let| mm' = Mask_model.scale_down k k mm in
                Result.Ok (`Mask mm')
             | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm) ->
                let remh, remw = h mod k, w mod k in
                if remh = remw && (remh = 0 || remh = k-1) (* account for separators *)
                then
                  let| mm' = Mask_model.scale_down k k mm in
                  Result.Ok (`Rectangle (`Vec (`Int (h / k), `Int (w / k)), col, `Mask mm'))
                else Result.Error (Undefined_result "ScaleDown: not an integer size")
             | `Grid g ->
                let| g' = Grid.Transf.scale_down k k g in
                Result.Ok (`Grid g')
             | _ -> Result.Error (Invalid_expr e))
         | _ -> Result.Error (Invalid_expr e))
  | `ScaleTo (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask mm, `Vec (`Int new_h, `Int new_w) ->
           let| mm' = Mask_model.scale_to new_h new_w mm in
           Result.Ok (`Mask mm')
        | `Point col, `Vec (`Int new_h, `Int new_w) ->
           Result.Ok (`Rectangle (`Vec (`Int new_h, `Int new_w), col, `Mask `Full))
        | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm), `Vec (`Int new_h, `Int new_w) ->
           let| mm' = Mask_model.scale_to new_h new_w mm in
           Result.Ok (`Rectangle (`Vec (`Int new_h, `Int new_w), col, `Mask mm'))
        | `Grid g, `Vec (`Int new_h, `Int new_w) ->
           let| g' = Grid.Transf.scale_to new_h new_w g in
           Result.Ok (`Grid g')
        | _ -> Result.Error (Invalid_expr e))
  | `Crop (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Grid g, `PosShape (`Vec (`Int ri, `Int rj), `Rectangle (`Vec (`Int rh, `Int rw), _, `Mask `Border)) -> (* TODO: allow crop on Full rectangles as well ? *)
           let i, j, h, w = ri+1, rj+1, rh-2, rw-2 in (* inside border *)
           let| g' = Grid.Transf.crop g i j h w in
           Result.Ok (`Grid g')
        | _ -> Result.Error (Invalid_expr e))
  | `Strip e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Grid g ->
           let| g'= Grid.Transf.strip g in
           Result.Ok (`Grid g')
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
        | `Mask mm1, `Mask mm2 ->
           let| mm = Mask_model.inter mm1 mm2 in
           Result.Ok (`Mask mm)
        | _ -> Result.Error (Invalid_expr e))
  | `LogOr (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask mm1, `Mask mm2 ->
           let| mm = Mask_model.union mm1 mm2 in
           Result.Ok (`Mask mm)
        | _ -> Result.Error (Invalid_expr e))
  | `LogXOr (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask mm1, `Mask mm2 ->
           let| mm = Mask_model.diff_sym mm1 mm2 in
           Result.Ok (`Mask mm)
        | _ -> Result.Error (Invalid_expr e))
  | `LogAndNot (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask mm1, `Mask mm2 ->
           let| mm = Mask_model.diff mm1 mm2 in
           Result.Ok (`Mask mm)
        | _ -> Result.Error (Invalid_expr e))
  | `LogNot e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Mask mm1 ->
           let| mm = Mask_model.compl mm1 in
           Result.Ok (`Mask mm)
        | _ -> Result.Error (Invalid_expr e))
  | `Stack le1 ->
     let| lres1 = list_map_result (apply ~lookup p) le1 in
     broadcast_list_result lres1
       (fun lt1 ->
         let lg1 = List.map (function `Grid g1 -> g1 | _ -> assert false) lt1 in
         let| g = Grid.Transf.layers lg1 in
         Result.Ok (`Grid g))
  | `Area e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Point _ -> Result.Ok (`Int 1)
        | `Rectangle (`Vec (`Int height, `Int width), _, `Mask m) ->
           Result.Ok (`Int (Mask_model.area ~height ~width m))
        | `Grid g -> Result.Ok (`Int (g.height * g.width))
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
  | `MaskOfGrid e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Grid g -> Result.Ok (`Mask (`Mask (Grid.Mask.from_grid g)))
        | _ -> Result.Error (Invalid_expr e))
  | `GridOfMask (e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Mask (`Mask m), `Color c ->
           Result.Ok (`Grid (Grid.Mask.to_grid m c))
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
           let| mm' = Mask_model.tile k l mm in
           Result.Ok (`Mask mm')
        | `Rectangle (`Vec (`Int h, `Int w), col, `Mask mm) ->
           let| mm' = Mask_model.tile k l mm in
           Result.Ok (`Rectangle (`Vec (`Int (h*k), `Int (w*l)), col, `Mask mm'))
        | `Point _ -> Result.Error (Undefined_result "Tiling: undefined on points")
        | `Grid g ->
           let| g' = Grid.Transf.tile k l g in
           Result.Ok (`Grid g')
        | _ -> Result.Error (Invalid_expr e))
  | `PeriodicFactor (mode,e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (function
        | `Color bgcolor, d2 ->
           let aux_shape = function
             | `Point color ->
                Result.Ok (`Point color)
             | `Rectangle (_size, color, `Mask (`Mask m)) ->
                let| m' = Grid.Transf.periodic_factor mode bgcolor m in
                let h', w' = Grid.dims m' in
                Result.Ok (`Rectangle (`Vec (`Int h', `Int w'), color, `Mask (`Mask m')))
             | _ -> Result.Error (Invalid_expr e) in
           (match d2 with
            | `Mask (`Mask m) ->
               let| m' = Grid.Transf.periodic_factor mode bgcolor m in
               Result.Ok (`Mask (`Mask m'))
            | (`Point _ | `Rectangle _ as shape) -> aux_shape shape
            | `PosShape (pos, shape) ->
               let| shape' = aux_shape shape in
               Result.Ok (`PosShape (pos, shape'))
            | `Grid g ->
               let| g' = Grid.Transf.periodic_factor mode bgcolor g in
               Result.Ok (`Grid g')
            | _ -> Result.Error (Invalid_expr e))
        | _ -> Result.Error (Invalid_expr e))
  | `FillResizeAlike (mode,e1,e2,e3) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     let| res3 = apply ~lookup p e3 in
     broadcast_list_result [res1;res2;res3]
       (function
        | [`Color bgcolor; `Vec (`Int h, `Int w); d3] when h > 0 && w > 0 ->
           let new_size = h, w in
           let aux_shape = function
             | `Point color ->
                Result.Ok (`Rectangle (`Vec (`Int h, `Int w), color, `Mask `Full))
             | `Rectangle (_size, color, `Mask mm) ->
                let| mm' = Mask_model.fill_and_resize_alike mode bgcolor new_size mm in
                Result.Ok (`Rectangle (`Vec (`Int h, `Int w), color, `Mask mm'))
             | _ -> assert false in
           (match d3 with
            | `Mask m ->
               let| m' = Mask_model.fill_and_resize_alike mode bgcolor new_size m in
               Result.Ok (`Mask m')
            | (`Point _ | `Rectangle _ as shape) -> aux_shape shape
            | `PosShape (pos, shape) ->
               let| shape' = aux_shape shape in
               Result.Ok (`PosShape (pos, shape'))
            | `Grid g ->
               let| g' = Grid.Transf.fill_and_resize_alike mode bgcolor new_size g in
               Result.Ok (`Grid g')
            | _ -> Result.Error (Invalid_expr e))
        | _ -> Result.Error (Invalid_expr e))
  | `Compose (e1,e2,e3) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     let| res3 = apply ~lookup p e3 in
     broadcast_list_result [res1;res2;res3]
       (function
        | [`Color c_mask;d1;d2] ->
           let| g1 =
             match d2 with
             | `Mask (`Mask m1) ->
                Result.Ok m1
             | `Rectangle (`Vec (`Int h1, `Int w1), _, `Mask mm1) ->
                let m1 = Mask_model.to_mask ~height:h1 ~width:w1 mm1 in
                Result.Ok m1
             | `Grid g1 ->
                Result.Ok g1
             | _ -> Result.Error (Invalid_expr e) in
           (match d2 with
           | `Mask (`Mask m2) ->
              let| m = Grid.Transf.compose c_mask g1 m2 in
              Result.Ok (`Mask (`Mask m))
           | `Rectangle (`Vec (`Int h2, `Int w2), col2, `Mask mm2) ->
              let m2 = Mask_model.to_mask ~height:h2 ~width:w2 mm2 in
              let| m = Grid.Transf.compose c_mask g1 m2 in (* TODO: reconvert as mask_model *)
              let h, w = Grid.dims m in
              Result.Ok (`Rectangle (`Vec (`Int h, `Int w), col2, `Mask (`Mask m)))
           | `Grid g2 ->
              let| g = Grid.Transf.compose c_mask g1 g2 in
              Result.Ok (`Grid g)
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
  | `CloseSym (sym_matrix,e1,e2) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     broadcast2_result (res1,res2)
       (fun (d1,d2) ->
         match d1 with
         | `Color bgcolor -> close_symmetry sym_matrix bgcolor e d2
         | _ -> Result.Error (Invalid_expr e))
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
  | `MajorityColor e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Grid g ->
           let| c = Grid.majority_color ~except_black:true g in
           Result.Ok (`Color c)
        | _ -> Result.Error (Invalid_expr e))
  | `ColorCount e1 ->
     let| res1 = apply ~lookup p e1 in
     broadcast1_result res1
       (function
        | `Grid g ->
           let n = Grid.color_count g in
           Result.Ok (`Int n)
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
  | `SwapColors (e1,e2,e3) ->
     let| res1 = apply ~lookup p e1 in
     let| res2 = apply ~lookup p e2 in
     let| res3 = apply ~lookup p e3 in
     broadcast_list_result [res1; res2; res3]
       (function
        | [`Grid g; `Color c1; `Color c2] ->
           let| g' = Grid.Transf.swap_colors g c1 c2 in
           Result.Ok (`Grid g')
        | _ -> Result.Error (Invalid_expr e))


let apply_expr apply ~(env : data) e =
  apply_expr_gen apply ~lookup:(lookup_of_env env) `Root e
  
let rec apply_template_gen ~(lookup : apply_lookup) (p : revpath) (t : template) : template result = (* QUICK *)
  (* SHOULD NOT use [p] *)
  match t with
  | `Any -> Result.Ok `Any
  | #patt as patt -> (apply_patt apply_template_gen ~lookup p patt :> template result)
  | #expr as e -> apply_expr_gen apply_template_gen ~lookup p e
  | `Seq items ->
     let| applied_items =
       list_mapi_result
         (fun i item -> apply_template_gen ~lookup (`Item (i,p)) item)
         0 items in
     Result.Ok (`Seq applied_items)
  | `Cst item0 ->
     let| applied_item0 = apply_template_gen ~lookup (`AnyItem p) item0 in
     Result.Ok (`Cst applied_item0)
  | `Prefix (main,items) ->
     let| applied_main = apply_template_gen ~lookup (`AnyItem p) main in
     let| applied_items =
       list_mapi_result
         (fun i item -> apply_template_gen ~lookup (`Item (i,p)) item)
         0 items in
     Result.Ok (`Prefix (applied_main,applied_items))

let apply_template ~(env : data) (t : template) : template result =
  apply_template_gen ~lookup:(lookup_of_env env) `Root t
(* DO NOT remove path argument, useful in generate_template (through apply_patt) *)


(* grid generation from data and template *)

type generator = Generator of (unit -> data * bool (* valid stop *) * generator) [@@unboxed] (* analogous to parseur *)

exception NothingToGenerate
                            
let rec generator_fail : generator =
  Generator (fun () -> raise NothingToGenerate)

let generator_collect (gen_item : generator) : generator =
  let rec aux (Generator gen_item) =
    let d, stop, next_item = gen_item () in
    if stop
    then [d]
    else d :: aux next_item in
  Generator (fun () ->
      let ld =
        try aux gen_item
        with NothingToGenerate -> [] in
      `Seq ld, true, generator_fail)

let generator_patt (gen : revpath -> 'a -> generator) (p : revpath) : 'a patt -> generator = function
  | (`Bool _ | `Int _ | `Color _ | `Mask _ | `Grid _ as d) ->
     let rec gen_d = Generator (fun () -> d, true, gen_d) in
     gen_d
  | `Vec (i,j) ->
     let rec gen_vec (Generator gen_i) (Generator gen_j) =
       Generator (fun () ->
           let di, stop_i, next_i = gen_i () in
           let dj, stop_j, next_j = gen_j () in
           `Vec (di,dj), stop_i && stop_j, gen_vec next_i next_j) in
     gen_vec (gen (p ++ `I) i) (gen (p ++ `J) j)
  | `Point color ->
     let rec gen_point (Generator gen_color) =
       Generator (fun () ->
           let dcolor, stop_color, next_color = gen_color () in
           `Point dcolor, stop_color, gen_point next_color) in
     gen_point (gen (p ++ `Color) color)
  | `Rectangle (size,color,mask) ->
     let rec gen_rectangle (Generator gen_size) (Generator gen_color) (Generator gen_mask) =
       Generator (fun () ->
           let dsize, stop_size, next_size = gen_size () in
           let dcolor, stop_color, next_color = gen_color () in
           let dmask, stop_mask, next_mask = gen_mask () in
           `Rectangle (dsize,dcolor,dmask),
           stop_size && stop_color && stop_mask,
           gen_rectangle next_size next_color next_mask) in
     gen_rectangle (gen (p ++ `Size) size) (gen (p ++ `Color) color) (gen (p ++ `Mask) mask)
  | `PosShape (pos,shape) ->
     let rec gen_ps (Generator gen_pos) (Generator gen_shape) =
       Generator (fun () ->
           let dpos, stop_pos, next_pos = gen_pos () in
           let dshape, stop_shape, next_shape = gen_shape () in
           `PosShape (dpos,dshape), stop_pos && stop_shape, gen_ps next_pos next_shape) in
     gen_ps (gen (p ++ `Pos) pos) (gen (p ++ `Shape) shape)
  | `Background (_grid,size,color,layers) ->
     let Generator gen_size = gen (p ++ `Size) size in
     let Generator gen_color = gen (p ++ `Color) color in
     let ilist_gen_layers =
       map_ilist
         (fun lp layer -> generator_collect (gen (p ++ `Layer lp) layer))
         `Root layers in
     Generator (fun () ->
         let dgrid = `Grid Grid.dummy in (* TODO: the grid should be computed from other components *)
         let dsize, _, _ = gen_size () in
         let dcolor, _, _ = gen_color () in
         let dlayers =
           map_ilist
             (fun lp (Generator gen_layer) ->
               let dlayer, _, _ = gen_layer () in
               dlayer)
             `Root ilist_gen_layers in
         `Background (dgrid,dsize,dcolor,dlayers), true, generator_fail)

let rec generator_template (p : revpath) (t : template) : generator =
  match t with
  | `Any ->
     let rec gen_any =
       Generator (fun () ->
           default_data_of_path p, true, gen_any) in
     gen_any
  | #patt as patt -> generator_patt generator_template p patt
  | #expr -> assert false (* should be eliminated by call to apply_template *)
  | `Seq items ->
     let rec gen_seq = function
       | [] -> generator_fail
       | (Generator gen_item)::next_gens ->
          Generator (fun () ->
              let d, _, _ = gen_item () in
              d, next_gens=[], gen_seq next_gens) in
     gen_seq (List.mapi (fun i item -> generator_template (`Item (i,p)) item) items)
  | `Cst item0 ->
     let Generator gen_item0 = generator_template (`Item (0,p)) item0 in
     let rec gen_cst = function
       | None ->
          Generator (fun () ->
              let d, _, _ = gen_item0 () in
              d, true, gen_cst (Some d))
       | Some d0 as hist ->
          Generator (fun () ->
              d0, true, gen_cst hist) in
     gen_cst None
  | `Prefix (main,items) ->
     let gen_main = generator_template (`AnyItem p) main in
     let rec gen_prefix = function
       | [] -> gen_main
       | (Generator gen_item)::next_gens ->
          Generator (fun () ->
              let d1, _, _ = gen_item () in
              d1, next_gens=[], gen_prefix next_gens) in
     gen_prefix (List.mapi (fun i item -> generator_template (`Item (i,p)) item) items)
     
let generate_template ?(p = `Root) (t : template) : data =
  (* should be named 'ground_template' *)
  let Generator gen = generator_template p t in
  let data, _, _ = gen () in
  data

(* from data to grid *)
  
exception Invalid_data_as_grid of data
let _ = Printexc.register_printer
          (function
           | Invalid_data_as_grid d ->
              Some ("the data does not represent a valid grid specification:\n" ^ string_of_data d)
           | _ -> None)
          
let rec grid_of_data : data -> (Grid.t, exn) Result.t = function
  | `Grid g -> Result.Ok g
  | `Background (_, `Vec (`Int h, `Int w), `Color c, l) when h>0 && w>0 ->
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
  | `Append (above, below) ->
     draw_layers g below;
     draw_layers g above
and draw_layer g = function
  | `PosShape (`Vec (`Int i, `Int j), `Point (`Color c)) ->
     Grid.set_pixel g i j c
  | `PosShape (`Vec (`Int mini, `Int minj), `Rectangle (`Vec (`Int h, `Int w), `Color c, `Mask m)) when h>0 && w>0 ->
     let maxi = mini + h - 1 in
     let maxj = minj + w - 1 in
     for i = mini to maxi do
       for j = minj to maxj do
	 if Mask_model.mem ~height:h ~width:w (i-mini) (j-minj) m
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
  let d = generate_template t' in
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
    bmp: Bitmap.t; (* remaining part of the grid to be explained *)
    parts: Segment.part list; (* remaining parts that can be used *)
    grid: Grid.t; (* the grid to parse *)
  }

let add_diff path state =
  { state with quota_diff = state.quota_diff - 1;
               diff = path::state.diff }

let add_delta_with_bmp ~bmp delta new_delta =
  List.fold_left
    (fun delta (i,j,c as pixel) ->
      if Bitmap.mem i j bmp
      then pixel::delta
      else delta)
    delta new_delta
  
let filter_parts_with_bmp ~new_bmp parts = (* QUICK *)
  List.filter
    (fun p ->
      not (Bitmap.inter_is_empty p.Segment.pixels new_bmp))
    parts

type ('a,'b) parseur = (* input -> state -> results *)
  Parseur of ('a -> parse_state -> ('b * parse_state * bool * ('a,'b) parseur) Myseq.t) [@@unboxed]
(* each result contains: a parsed value, the new state, a valid sequence 'stop' flag, and a parser for the sequence continuation *) 

let parseur_empty : ('a,'b) parseur =
  Parseur (fun x state -> Myseq.empty)
    
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

let parseur_cst (Parseur parse_item : ('a,'b) parseur) : ('a,'b) parseur =
  let rec aux = function
    | None ->
       Parseur (fun x state ->
           let* y, state, stop, _next = parse_item x state in
           if stop
           then Myseq.return (y, state, true, aux (Some y))
           else Myseq.empty)
    | Some y0 as hist ->
       Parseur (fun x state ->
           let* y, state, stop, _next = parse_item x state in
           if stop && y = y0
           then Myseq.return (y, state, true, aux hist)
           else Myseq.empty)
  in
  aux None
  
let parseur_prefix ?(partial = false) (parseur_main : ('a,'b) parseur) (parseur_items : ('a,'b) parseur list) : ('a,'b) parseur =
  let rec aux = function
    | [] -> parseur_main
    | (Parseur parse_item)::l1 ->
       Parseur (fun x state ->
           let* y, state, stop, _next = parse_item x state in (* assuming no sequence nesting *)
           if stop
           then Myseq.return (y, state, (partial || l1=[]), aux l1)
           else Myseq.empty)
  in
  aux parseur_items
let parseur_seq lparseur = parseur_prefix parseur_empty lparseur
  
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

(* let _ = (* UNIT test for parseur_collect *)
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
  let rec parseur_primes l =
    Parseur (fun () state ->
        let* y = Myseq.from_list l in
        let l1 = List.filter (fun y1 -> y1 mod y <> 0 && y mod y1 <> 0) l in
        Myseq.return (y, state, true, parseur_primes l1)) in        
  let Parseur parse_all = parseur_collect (* ~max_depth:1 *) (parseur_primes [1;2;3;4;5]) in
  parse_all () (Obj.magic [|1;2;3|] : parse_state)
  |> Myseq.slice ~limit:10000
  |> Myseq.iter (fun (l,_,_,_) ->
         print_endline (String.concat " " (List.map string_of_int l))) *)


let rec parseur_template
          ~(parseur_any : unit -> ('a,data) parseur)
          ~(parseur_patt : template patt -> ('a,data) parseur)
          (t : template) (p : revpath)
        : ('a,data) parseur =
  match t with
  | `Any -> parseur_any ()
  | #patt as patt -> parseur_patt patt
  | #expr -> assert false
  | `Seq items ->
     parseur_seq
       (List.mapi
          (fun i item -> parseur_template ~parseur_any ~parseur_patt item (`Item (i,p)))
          items)
  | `Cst item0 ->
     parseur_cst (parseur_template ~parseur_any ~parseur_patt item0 (`Item (0,p)))
  | `Prefix (main,items) ->
     parseur_prefix
       (parseur_template ~parseur_any ~parseur_patt main (`AnyItem p))
       (List.mapi
          (fun i item -> parseur_template ~parseur_any ~parseur_patt item (`Item (i,p)))
          items)


let parseur_bool t p : (bool,data) parseur = (* QUICK *)
  parseur_template
    ~parseur_any:(fun () ->
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
    ~parseur_any:(fun () ->
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
    ~parseur_any:(fun () ->
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
  
let parseur_mask t p : (Mask_model.t list, data) parseur = (* QUICK *)
  parseur_template
    ~parseur_any:(fun () ->
      parseur_rec (fun ms state ->
          let* m = Myseq.from_list ms in
          Myseq.return (`Mask m, state)))
    ~parseur_patt:(function
      | `Mask m0 ->
         parseur_rec (fun ms state ->
             if List.exists (Mask_model.subsumes m0) ms then
               Myseq.return (`Mask m0, state)
             else if state.quota_diff > 0 then
               let* m = Myseq.from_list ms in
               Myseq.return (`Mask m, add_diff p state)
             else Myseq.empty)
      | _ -> parseur_empty)
    t p

let parseur_vec t p : (int * int, data) parseur = (* QUICK *)
  parseur_template
    ~parseur_any:(fun () ->
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

let state_minus_shape_gen state occ_color occ_delta occ_new_cover = (* QUICK *)
  let new_bmp = Bitmap.diff state.bmp occ_new_cover in
  if Bitmap.equal new_bmp state.bmp
  then None (* the shape is fully hidden, explains nothing new *)
  else
    let new_state =
      { state with
	bmp = new_bmp;
        delta = add_delta_with_bmp ~bmp:state.bmp state.delta occ_delta;
	parts = filter_parts_with_bmp ~new_bmp state.parts
                |> List.filter (fun (p : Segment.part) -> not (occ_color = p.color && Bitmap.is_subset occ_new_cover p.Segment.pixels))
                               (* that would make occ useless if selecting p later *)
      } in
    Some new_state
let state_minus_point state (i,j,c) =
  let occ_delta = [] in
  let occ_new_cover = Bitmap.singleton state.grid.height state.grid.width i j in
  state_minus_shape_gen state c occ_delta occ_new_cover
let state_minus_rectangle state (rect : Segment.rectangle) =
  state_minus_shape_gen state rect.color rect.delta rect.new_cover  

let parseur_shape t p : (Segment.point list Lazy.t * Segment.rectangle list Lazy.t, data (* object *)) parseur =
  parseur_template
    ~parseur_any:(fun () ->
      parseur_rec (fun (points,rects) state ->
          Myseq.concat
            [(let* rect = Myseq.from_list (Lazy.force rects) in
              let* state = Myseq.from_option (state_minus_rectangle state rect) in
              let open Grid in
              let* m = Myseq.from_list rect.mask_models in
              Myseq.return
                (`PosShape (`Vec (`Int rect.offset_i, `Int rect.offset_j),
                            `Rectangle (`Vec (`Int rect.height, `Int rect.width),
                                        `Color rect.color,
                                        `Mask m)),
                 state));
             (let* (i,j,c as point) = Myseq.from_list (Lazy.force points) in
              let* state = Myseq.from_option (state_minus_point state point) in
              Myseq.return
                (`PosShape (`Vec (`Int i, `Int j), `Point (`Color c)),
                 state))
    ]))
    ~parseur_patt:(function
      | `Point color ->
         parseur_rec1
           (parseur_color color (p ++ `Color))
           (fun parse_color (points,rects) state ->
             let* (i,j,c as point) = Myseq.from_list (Lazy.force points) in
             let* dcolor, state, stop_color, next_color = parse_color c state in
             let* state = Myseq.from_option (state_minus_point state point) in
             Myseq.return (`PosShape (`Vec (`Int i, `Int j), `Point dcolor), state, stop_color, next_color))
      | `Rectangle (size,color,mask) ->
         parseur_rec3
           (parseur_vec size (p ++ `Size))
           (parseur_color color (p ++ `Color))
           (parseur_mask mask (p ++ `Mask))
           (fun parse_size parse_color parse_mask (points,rects) state ->
             let* rect = Myseq.from_list (Lazy.force rects) in
             let open Segment in
             let* dsize, state, stop_size, next_size = parse_size (rect.height,rect.width) state in
             let* dcolor, state, stop_color, next_color = parse_color rect.color state in
             let* dmask, state, stop_mask, next_mask = parse_mask rect.mask_models state in
             let* state = Myseq.from_option (state_minus_rectangle state rect) in
             Myseq.return (`PosShape (`Vec (`Int rect.offset_i, `Int rect.offset_j), `Rectangle (dsize,dcolor,dmask)),
                           state,
                           stop_size, stop_color, stop_mask,
                           next_size, next_color, next_mask))
      | _ -> parseur_empty)
    t p

let parseur_object t p : (unit, data) parseur =
  parseur_template
    ~parseur_any:(fun () ->
      parseur_rec1
        (parseur_shape `Any (p ++ `Shape))
        (fun parse_shape () state ->
          let points = lazy (Segment.points state.grid state.bmp state.parts) in
          let rects = lazy (Segment.rectangles state.grid state.bmp state.parts) in
          let* dobject, state, stop_shape, next_shape = parse_shape (points,rects) state in
          (* no constraint on position *)
          Myseq.return (dobject, state, stop_shape, next_shape)))
    ~parseur_patt:(function
      | `PosShape (pos,shape) ->
         parseur_rec2
           (parseur_vec pos (p ++ `Pos))
           (parseur_shape shape (p ++ `Shape))
           (fun parse_pos parse_shape () state ->
             let points = lazy (Segment.points state.grid state.bmp state.parts) in
             let rects = lazy (Segment.rectangles state.grid state.bmp state.parts) in
             let* dobject, state, stop_shape, next_shape = parse_shape (points,rects) state in
             match dobject with
             | `PosShape (`Vec (`Int i, `Int j), dshape) ->
                let* dpos, state, stop_pos, next_pos = parse_pos (i,j) state in
                Myseq.return (`PosShape (dpos,dshape),
                              state,
                              stop_pos, stop_shape,
                              next_pos, next_shape)
             | _ -> assert false)
      | _ -> parseur_empty)
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
let parseur_layer (t : template) (p : revpath) : (unit,data) parseur =
  let Parseur parse_objects = parseur_collect ~max_depth:!max_seq_length (parseur_object t p) in
  Parseur (fun () state ->
      let* ld, state, _, _ = parse_objects () state in
      match ld with
      | [] -> Myseq.empty (* not allowing empty sequences *)
      | [d] when not !seq -> Myseq.return (d, state, true, parseur_empty)
      | _ -> Myseq.return (`Seq ld, state, true, parseur_empty))
  
let parseur_layers layers p : (unit, data ilist) parseur =
  let rev_layer_parses =
    fold_ilist
      (fun revl lp layer ->
        let Parseur parse = parseur_layer layer (p ++ `Layer lp) in
        let simple_parse =
          fun state ->
          let* d, state, _stop, _next = parse () state in 
          Myseq.return (d,state) in
        simple_parse::revl)
      [] `Root layers in
  let gen = Myseq.product_dependent_fair
              ~max_relaxation_level:(!max_relaxation_level_parse_layers) (* TODO: pb when nesting fair iterations, incompatible with use of parseur_collect *)
              (List.rev rev_layer_parses) in
  Parseur (fun () state ->
  Myseq.prof "Model2.parse_layers/seq" (
      let* ld, state = gen state in
      let l, dlayers = fill_ilist_with_list layers ld in
      assert (l = []);
      Myseq.return (dlayers, state, true, parseur_empty)))
  
let parseur_grid t p : (Grid.t, data) parseur = (* QUICK, runtime in Myseq *)
  parseur_template
    ~parseur_any:(fun () -> parseur_empty)
    ~parseur_patt:
    (function
     | `Grid g0 ->
        parseur_rec (fun g state ->
            if Grid.diff g0 g = None then Myseq.return (`Grid g, state)
            else if state.quota_diff > 0 then
              Myseq.return (`Grid g, add_diff p state)
            else Myseq.empty)
     | `Background (_grid,size,color,layers) ->
        let Parseur parse_size = parseur_vec size (p ++ `Size) in
        let Parseur parse_color = parseur_color color (p ++ `Color) in
        let seq_background_colors =
          match color with
          | `Color bc -> (fun g -> Myseq.return bc)
          | `Any -> (fun g -> Myseq.from_list (Segment.background_colors g))
          | _ -> assert false in
        let parse_bg_color g state =
          let* bc = seq_background_colors g in
          let* dcolor, state, _, _ = parse_color bc state in
          let state = { state with (* ignoring parts belonging to background *)
                        parts = List.filter (fun (p : Segment.part) -> p.color <> bc) state.parts } in
          Myseq.return ((bc,dcolor),state,true,parseur_empty) in          
        let Parseur parse_layers = parseur_layers layers p in
        Parseur (fun (g : Grid.t) state -> Myseq.prof "Model2.parse_grid/seq" (
          let dgrid = `Grid g in
          let* dsize, state, _, _ = parse_size (g.height,g.width) state in
          let* (bc,dcolor), state, _, _ = parse_bg_color g state in
          let* dlayers, state, _, _ = parse_layers () state in
          (*let* ((bc,dcolor),dlayers), state =
            Myseq.pair_dependent_fair (parse_bg_color g) (parse_layers ()) state in*)
          let data = `Background (dgrid,dsize,dcolor,dlayers) in
	  (* adding mask pixels with other color than background to delta *)
          let new_state =
            let new_delta = ref state.delta in
	    Bitmap.iter
	      (fun i j ->
                let c = Grid.get_pixel ~source:"parseur_grid" g i j in
	        if c <> bc then
	          new_delta := (i,j,c)::!new_delta)
	      state.bmp;
            { state with
              delta = (!new_delta);
	      bmp = Bitmap.empty g.height g.width;
	      parts = [] } in
	  Myseq.return (data, new_state, true, parseur_empty)))
     | _ -> parseur_empty)
    t p

(* let _ = (* UNIT test for parseur_grid *)
  let task_name = "b94a9452" in
  let task = Task.from_file ("/local/ferre/data/tasks/ARC/data/training/" ^ task_name ^ ".json") in
  let g = (List.nth task.train 2).input in
  let dl_ctx = { box_height = g.height; box_width = g.width } in
  let state =
    { quota_diff = 0;
      diff = diff0;
      delta = delta0;
      mask = Grid.Mask.full g.height g.width;
      parts = Grid.segment_by_color g;
      grid = g } in
  let t = `Background (u_vec_any, u_any, `Insert (`Nil, u_any, `Nil)) in
  let p = `Root in
  let Parseur parse_grid = parseur_grid t p in
  let encode_grid = encoder_template ~ctx:dl_ctx t in
  parse_grid g state
  (*  |> Myseq.slice ~limit:1000 *)
  |> Myseq.fold_left (fun (best_dl,res) (d, state, stop, next) ->
         assert stop;
         let dl = encode_grid d
                  +. dl_delta ~ctx:dl_ctx state.delta in
         (*Printf.printf "### %.3f\n" dl;
         pp_data d;
         print_newline (); *)
         if dl < 1.5 *. best_dl
         then (min dl best_dl, (d,dl)::res)
         else (best_dl, res))
       (Stdlib.infinity, [])
  |> (fun (best_dl,res) ->
    List.iter (fun (d,dl) ->
        Printf.printf "### %.3f\n" dl;
        pp_data d;
        print_newline ())
      (List.rev res)) *)

  
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
      ?(dl_assuming_grid_known = false)
      ~(quota_diff : int)
      ~(env : data) (t0 : template) (g : Grid.t)
    : (grid_read list, exn) Result.t =
  Common.prof "Model2.read_grid" (fun () ->
  let| t = apply_template ~env t0 in (* reducing expressions *)
  let Parseur parse_grid = parseur_grid t path0 in
  let state = { quota_diff;
                diff = diff0;
                delta = delta0;
                bmp = Bitmap.full g.height g.width;
                parts = Segment.segment_by_color g;
                grid = g } in
  let parses =
    let* qdiff = Myseq.range 0 quota_diff in (* for increasing diff quota *)
    let* data, state, stop, _next = parse_grid g {state with quota_diff = qdiff} in (* parse with this quota *)
    assert stop;
    let* () = Myseq.from_bool (state.quota_diff = 0) in (* check quota fully used to avoid redundancy *)
    let ctx = dl_ctx_of_data data in
    let dl = (* QUICK *)
      let dl_data = encoder_template ~ctx t0 data in
      let dl_diff = dl_diff ~ctx state.diff data in
      let dl_delta = dl_delta ~ctx state.delta in
      (* rounding before sorting to absorb float error accumulation *)
      dl_round (dl_data +. dl_diff +. dl_delta) in
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
      l_parses (* QUICK *)
      |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
      |> (fun l -> Common.sub_list l 0 !max_nb_grid_reads)
      |> limit_dl (fun (_,_,dl) -> dl)
      |> List.mapi (fun rank (env,gd,dl) ->
             let dl_rank = dl_parse_rank rank in
             let dl =
               if dl_assuming_grid_known
               then dl_rank
               else dl +. dl_rank in
             (env, gd, dl)) in
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

let read_grids ?dl_assuming_grid_known ~quota_diff ~env_sig (t : template) (egrids: (data * Grid.t) list) : (grids_read, exn) Result.t =
  let dl_m =
    dl_template
      ~env_sig
      ~ctx:{box_height=Grid.max_size; box_width=Grid.max_size}
      t in
  let| reads =
    list_map_result
      (fun (env,g) -> read_grid ?dl_assuming_grid_known ~quota_diff ~env t g)
      egrids in
  Result.Ok {dl_m; reads}

  
(** TASKS *)
       
(* task models, articulating parsing and evaluation *)

type model = (* input->output models *)    
  { input_pattern : template; (* only consts and unknowns allowed *)
    output_template : template (* complex expressions allowed *)
  }

let init_template =
(*  let u_rect = `PosShape (u_vec_cst, `Rectangle (u_vec_cst, `U, `U)) in
  let u_point = `PosShape (u_vec_cst, `Point (`U)) in *)
  (*let u_layer = `PosShape (u_vec_any, `Any) in*)
  let u_layers = `Nil in
  (*let u_layers = `Insert (`Nil, u_layer, `Nil) in*)
  `Background (u_any, u_vec_any, u_any, u_layers )
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
    read_grid ~dl_assuming_grid_known:true ~quota_diff:(!max_nb_diff) ~env m.input_pattern g in
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
  
let read_grid_pairs ?(pruning = false) ?(env = data0) (m : model) (pairs : Task.pair list) : (grid_pairs_read, exn) Result.t =
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
    pairs
    |> list_map_result
         (fun {input; output} ->
           let| reads_input =
             read_grid ~dl_assuming_grid_known:pruning ~quota_diff:0 ~env m.input_pattern input in (* no diff allowed during training *)
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
           Result.Ok (reads_input, reads_pair)) in
  let input_reads, reads = List.split input_reads_reads in
  Result.Ok {dl_mi; dl_mo; input_reads; reads})
  
(* template transformations *)

let rec change_seq_at_pos (f : 'a -> 'a option) (i : int) (l : 'a list) : 'a list option =
  match l with
  | [] -> failwith "Model2.insert_seq: wrong position"
  | x::r ->
     if i = 0
     then
       match f x with
       | Some y -> Some (y :: r)
       | None -> Some r
     else
       let|? r' = change_seq_at_pos f (i-1) r in
       Some (x :: r')

let change_template_at_path (f : template option -> template option) (p : revpath) (t : template) : template option = (* QUICK *)
  let rec aux revp t =
    match revp, t with
    | _, #expr -> assert false (* not replacing expressions *)
    | `Root, _ -> f (Some t)
    | `Field (f,revp1), (#patt as patt) -> aux_patt f revp1 patt
    | `Item (i,revp1), `Seq items ->
       let|? items' = change_seq_at_pos (aux revp1) i items in
       Some (`Seq items')
    | `Item (0,revp1), `Cst item0 ->
       let|? item0' = aux revp1 item0 in
       Some (`Cst item0')
    | `AnyItem revp1, `Prefix (main,items) ->
       let|? main' = aux revp1 main in
       Some (`Prefix (main', items))
    | `Item (i,revp1), `Prefix (main,items) ->
       let n = List.length items in
       if i = n
       then ( (* new element *)
         assert (revp1 = `Root);
         let|? new_item = f (Some t) in
         Some (`Prefix (main, items @ [new_item]))
       )
       else (
         assert (i >= 0 && i < n);
         let|? items' = change_seq_at_pos (aux revp1) i items in
         Some (`Prefix (main,items'))
       )
       
    | `Item (0,`Root), (`Any | #patt) -> (* tentative *)
       let|? item0 = f (Some t) in
       Some (`Prefix (t, [item0]))

    | _ -> assert false
  and aux_patt field revp1 patt =
    match field, patt with
    | `I, `Vec (i,j) ->
       let|? i' = aux revp1 i in
       Some (`Vec (i',j))
    | `J, `Vec (i,j) ->
       let|? j' = aux revp1 j in
       Some (`Vec (i, j'))

    | `Color, `Point (color) ->
       let|? color' = aux revp1 color in
       Some (`Point color')

    | `Size, `Rectangle (size,color,mask) ->
       let|? size' = aux revp1 size in
       Some (`Rectangle (size', color, mask))
    | `Color, `Rectangle (size,color,layers) ->
       let|? color' = aux revp1 color in
       Some (`Rectangle (size, color', layers))
    | `Mask, `Rectangle (size,color,mask) ->
       let|? mask' = aux revp1 mask in
       Some (`Rectangle (size, color, mask'))

    | `Pos, `PosShape (pos,shape) ->
       let|? pos' = aux revp1 pos in
       Some (`PosShape (pos', shape))
    | `Shape, `PosShape (pos,shape) ->
       let|? shape' = aux revp1 shape in
       Some (`PosShape (pos, shape'))

    | `Grid, `Background (grid,size,color,layers) ->
       let|? grid' = aux revp1 grid in
       Some (`Background (grid', size, color, layers))
    | `Size, `Background (grid,size,color,layers) ->
       let|? size' = aux revp1 size in
       Some (`Background (grid, size', color, layers))
    | `Color, `Background (grid,size,color,layers) ->
       let|? color' = aux revp1 color in
       Some (`Background (grid, size, color', layers))
    | `Layer revlp, `Background (grid,size,color,layers) ->
       let layers' = aux_ilist revlp revp1 layers in
       Some (`Background (grid, size, color, layers'))
       
    | _ ->
       pp_field field; print_string ": ";
       pp_patt_dummy patt;
       print_newline ();
       assert false
  and aux_ilist revlp revp1 ilist =
    match revlp, ilist with
    | `Root, `Nil ->
       assert (revp1 = `Root);
       (match f None with
        | None -> ilist
        | Some elt -> `Insert (`Nil, elt, `Nil))
    | `Root, `Insert (left, elt, right) ->
       (match aux revp1 elt with
        | None -> `Append (left, right)
        | Some elt' -> `Insert (left, elt', right))
    | `Root, `Append (left, right) ->
       assert (revp1 = `Root);
       (match f None with
        | None -> ilist
        | Some elt -> `Insert (left, elt, right))
    | `Left revlp1, `Insert (left, elt, right) ->
       let left' = aux_ilist revlp1 revp1 left in
       `Insert (left', elt, right)
    | `Left revlp1, `Append (left, right) ->
       let left' = aux_ilist revlp1 revp1 left in
       `Append (left', right)
    | `Right revlp1, `Insert (left, elt, right) ->
       let right' = aux_ilist revlp1 revp1 right in
       `Insert (left, elt, right')
    | `Right revlp1, `Append (left, right) ->
       let right' = aux_ilist revlp1 revp1 right in
       `Append (left, right')
    | _ -> assert false
  in
  aux (path_reverse p `Root) t
             
(* model refinements and learning *)

type grid_refinement =
  | RGridInit
  | RAdd of revpath * template
  | RSpe of revpath * template * bool (* partial: only true for some items if many items *)
  | RGen of revpath * template
  | RDel of revpath

let xp_grid_refinement (print : Xprint.t) = function
  | RGridInit -> ()
  | RAdd (p,t) ->
     print#string "ADD ";
     xp_path print p;
     print#string " = ";
     xp_template print t
  | RSpe (p,t,partial) ->
     print#string "SPE "; xp_path print p;
     print#string " = "; xp_template print t;
     if partial then print#string " (partial)"
  | RGen (p,t) ->
     print#string "GEN "; xp_path print p;
     print#string " = "; xp_template print t
  | RDel p ->
     print#string "DEL "; xp_path print p
let pp_grid_refinement = Xprint.to_stdout xp_grid_refinement
let string_of_grid_refinement = Xprint.to_string xp_grid_refinement

exception Refinement_no_change
let apply_grid_refinement (r : grid_refinement) (t : template) : template option (* None if no change or ill-formed change *) = (* QUICK *)
  try
    let|? t' =
      match r with
      | RGridInit -> raise Refinement_no_change
      | RAdd (p,new_t) ->
         change_template_at_path
           (function
            | None -> Some new_t
            | Some _ -> assert false)
           p t
      | RSpe (p,new_t,_) | RGen (p,new_t) ->
         change_template_at_path
           (function
            | None -> assert false
            | Some x ->
               if x = new_t then raise Refinement_no_change
               else Some new_t)
           p t
      | RDel p ->
         change_template_at_path
           (function
            | None -> assert false
            | Some _ -> None)
           p t
    in
(*    print_string "New grid template: ";
    pp_template t;
    print_newline (); *)
    Some t'
  with
  | Refinement_no_change -> None
  | exn ->
     print_endline "ERROR in apply_grid_refinement";
     pp_grid_refinement r; print_newline ();
     pp_template t; print_newline ();
     raise exn (* does not seem to occur any more *)

let rec defs_refinements ~(env_sig : signature) (t : template) (grss : grid_read list list) : grid_refinement Myseq.t =
  Common.prof "Model2.defs_refinements" (fun () ->
  assert (grss <> []);
  let in_output = (env_sig <> []) in    
  let u_vars = (* QUICK *)
    List.rev
      (fold_template
         (fun res p t0 anc0 ->
           match t0 with
           | #expr -> res (* not considering expr re-definition *)
           | _ ->
              if path_is_refinable p
              then
                let role = path_role p in
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
                  match p, find_data p gd.data with
                  | _, Some d -> PMap.add p d res
                  | `Item _, None -> res (* to cope with `Prrefix growing *)
                  | _, None ->
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
  let rec find_dl_rank ?dl_best t p reads : (grid_data * Mdl.bits * int * data * bool (* partial *)) option = (* proper QUICK *)
    match reads with
    | [] -> None
    | (env,gd,u_val,dl,rank)::rem ->
       let dl_best =
         match dl_best with
         | None -> dl
         | Some dl -> dl in
       (match PMap.find_opt p u_val with
        | Some d ->
           (match defs_check ~env t d with
            | Yes partial -> Some (gd, dl -. dl_best, rank, d, partial) (* recording current_dl - best_dl *)
            | No -> find_dl_rank ~dl_best t p rem)
        | _ -> find_dl_rank ~dl_best t p rem)
  in
  let module TMap = (* mappings from defining templates *)
    Map.Make
      (struct
        type t = template
        let compare = Stdlib.compare
      end) in
  let add_template t v tmap =
    if not (TMap.mem t tmap)
    then TMap.add t v tmap
    else tmap in
  (* defs from first example *)
  let defs = [] in
  let defs = Common.prof "Model2.defs_refinements/first/by_path" (fun () ->
    let$ defs, (p,role,t0,dl_t0) = defs, u_vars in (* for each definable path *)
    (* validity conditions, and associated information, for each kind of definition *)
    let valid_Constr =
      match t0 with
      | `Any | `Vec (`Any, `Any) -> true (* including Vec because present initially *)
      | _ -> false in
    let valid_Cst_t =
      match t0 with
      | `Seq (item0::_) | `Prefix (_, item0::_) -> Some (`Cst item0)
      | `Cst _ | `Seq _ | `Prefix _ -> None
      | _ ->
         if not (template_is_ground t0) && path_dim p = Sequence && template_dim t0 = Item
         then Some (`Cst t0)
         else None in
    let valid_PrefixInitExtend_n_maket =
      let valid_role = (* instantiable role in model *)
        match role with
        | `IntCoord _ -> in_output
        | `IntCard -> in_output
        | `Color _ -> true
        | `Mask -> true
        | `Vec _ -> in_output
        | _ -> false in
      if valid_role then
        match t0 with
        | `Prefix (main, items) ->
           Some (List.length items, (fun t_item -> `Prefix (main, items@[t_item])))
        | _ ->
           if not (template_is_ground t0) && path_dim p = Sequence && template_dim t0 = Item
           then Some (0, (fun t_item -> `Prefix (t0, [t_item])))
           else None
      else None in
    let valid_PrefixClose_n_t =
      match t0 with
      | `Prefix (main, items) -> Some (List.length items, `Seq items)
      | _ -> None in
    (* computing a map: defining template -> dl_ctx, dl, d *)
    let tmap =
      let$ tmap, (env,gd,u_val,dl0,rank) = TMap.empty, reads_fst in (* for each read of fst example *)
      let dl_ctx = dl_ctx_of_data gd.data in
      match PMap.find_opt p u_val with (* look up of read data at p *)
      | None -> tmap
      | Some d ->
         let tmap = (* Constr *)
           if valid_Constr then
             let$ tmap, (t, partial) = tmap, root_template_of_data ~in_output d in
             if t <> t0 (* a different pattern *)
             then add_template t (dl_ctx,dl0,d,partial) tmap
             else tmap
           else tmap in
         let tmap = (* Cst *)
           match valid_Cst_t with
           | Some t ->
              (match d with
               | `Seq (d0::ds1) ->
                  let k, n =
                    List.fold_left
                      (fun (k,n) d1 ->
                        if d1 = d0 then (k+1,n+1) else (k,n+1))
                      (1,1) ds1 in
                  if float k /. float n >= !def_match_threshold
                  then
                    let partial = k < n in
                    add_template t (dl_ctx,dl0,d,partial) tmap
                  else tmap
               | _ -> tmap)
           | None -> tmap in
         let tmap = (* Prefix-Init/Extend *)
           match valid_PrefixInitExtend_n_maket with
           | Some (n,make_t) ->
              (match d with
               | `Seq items ->
                  (match List.nth_opt items n with
                   | Some d_item -> (* the nth_item *)
                      let t_item = (d_item :> template) in
                      let t = make_t t_item in
                      add_template t (dl_ctx,dl0,d,false) tmap
                   | None -> tmap)
               | _ -> tmap)
           | None -> tmap in
         let tmap = (* Prefix-Close *)
           match valid_PrefixClose_n_t with
           | Some (n,t) ->
              (match d with
               | `Seq d_items as d when n = List.length d_items ->
                  add_template t (dl_ctx,dl0,d,false) tmap
               | _ -> tmap)
           | None -> tmap in
         tmap in
    (* returning a list of definitions *)
    TMap.fold
      (fun t (dl_ctx,dl0,d,partial) defs ->
        let dl_t = dl_template ~env_sig ~ctx:dl_ctx0 ~path:p t in
        let dl_d_t0 = encoder_template ~ctx:dl_ctx ~path:p t0 d in
        let dl_d_t = encoder_template ~ctx:dl_ctx ~path:p t d in
        let dl = dl0 +. dl_t -. dl_t0 +. dl_d_t -. dl_d_t0 in
        (dl,p,role,t0,t,partial)::defs)
      tmap defs) in
  let defs = Common.prof "Model2.defs_refinements/first/by_expr" (fun () ->
    let$ defs, (role_e,t) = defs, defs_expressions ~env_sig in
    let data_fst = (* QUICK *)
      reads_fst
      |> List.filter_map
           (fun (env,gd,u_val,dl0,rank) ->
             match defs_check_apply ~env t with
             | None -> None
             | Some t_applied -> Some (t_applied,gd,u_val,dl0)) in
    if data_fst = []
    then defs
    else
      let dim_t = template_dim t in
      let$ defs, (p,role,t0,dl_t0) = defs, u_vars in
      let dim_p = path_dim p in
      if (dim_t <= dim_p || not !seq) && role_poly_matches role_e role
        (* whether the expression role matches the defined path, and relaxation value *)
      then
        let dim_t0 = template_dim t0 in
        let valid_PrefixInitExtend_n_maket =
          match dim_t with
          | Item ->
             (match t0 with
              | `Prefix (main, items) ->
                 Some (List.length items, (fun t_item -> `Prefix (main, items@[t_item])))
              | _ ->
                 if not (template_is_ground t0) && dim_p = Sequence && dim_t0 = Item
                 then Some (0, (fun t_item -> `Prefix (t0, [t_item])))
                 else None)
          | Sequence -> None in
        let tmap =
          let$ tmap, (t_applied,gd,u_val,dl0) = TMap.empty, data_fst in
          let dl_ctx = dl_ctx_of_data gd.data in
          match PMap.find_opt p u_val with
          | None -> tmap
          | Some d ->
             let tmap = (* does the expression match the whole data [d] *)
               match matches_template t_applied d with
               | Yes partial -> add_template t (dl_ctx,dl0,d,partial) tmap
               | No -> tmap in
             let tmap = (* does the expression match the next item *)
               match valid_PrefixInitExtend_n_maket, d with
               | Some (n,make_t), `Seq items ->
                  (match List.nth_opt items n with
                   | Some d_item ->
                      (match matches_template t_applied d_item with
                       | Yes partial -> add_template (make_t t) (dl_ctx,dl0,d,partial) tmap
                       | No -> tmap)
                   | None -> tmap)
               | _ -> tmap in
             tmap in
        TMap.fold
          (fun t (dl_ctx,dl0,d,partial) defs ->
            let dl_t = dl_template ~env_sig ~ctx:dl_ctx0 ~path:p t in
            let dl_d_t0 = encoder_template ~ctx:dl_ctx ~path:p t0 d in
            let dl_d_t = encoder_template ~ctx:dl_ctx ~path:p t d in
            let dl = dl0 +. dl_t -. dl_t0 +. dl_d_t -. dl_d_t0 in
            (dl,p,role,t0,t,partial)::defs)
        tmap defs
      else defs) in
  (* checking defs w.r.t. other examples *)
  let _, defs = Common.prof "Model2.defs_refinements/others" (fun () ->
    let$ (i,defs), reads = (2,defs), reads_others in
    i+1,                
    defs
    |> List.filter_map
         (fun (dl,p,role,t0,t,partial) ->
           match find_dl_rank t p reads with
           | None -> None
           | Some (gd1,dl1,rank1,d1,partial1) ->
              let dl_ctx = dl_ctx_of_data gd1.data in
              let dl_d_t0 = encoder_template ~ctx:dl_ctx ~path:p t0 d1 in
              let dl_d_t = encoder_template ~ctx:dl_ctx ~path:p t d1 in
              Some (dl +. dl1 +. dl_d_t -. dl_d_t0, p, role, t0, t, partial || partial1))) in
  (* sorting defs, and returning them as a sequence *)
  defs
  |> List.rev (* to correct for the above List.fold_left's that stack in reverse *)
  |> List.stable_sort (* increasing delta DL *)
       (fun (dl1,_,_,_,_,_) (dl2,_,_,_,_,_) ->
         dl_compare dl1 dl2)
  |> Myseq.from_list
  |> Myseq.map (fun (dl,p,role,t0,t,partial) -> RSpe (p,t,partial)))
and defs_check ~env (t : template) (d : data) : match_result =
  match defs_check_apply ~env t with
  | None -> No
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
    let role = (path_role p :> role_poly) in
    let res = (role,p) :: res in
    if !seq && path_dim p = Sequence
    then (* adding paths to access first sequence elements *)
      let$ res, i = res, [0;1;2] in
      (role, `Item (i,p))::res
    else res in
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
    let _ = (* color constants *)
      let& c = Grid.all_colors in
      push (`Color `X, `Color c) in
    let& (role1,e1) = exprs_0 in
    (* unary operators *)
    let _ = (* area(_) *)
      match role1 with
      | `Shape -> push (`IntCoord (`X, `Size `X), `Area e1)
      | _ -> () in
    let _ = (* right(_) *)
      match role1 with
      | `Layer -> push (`IntCoord (`J, `Pos), `Right e1)
      | _ -> () in
    let _ = (* center(_) *)
      match role1 with
      | `Layer -> push (`IntCoord (`J, `Pos), `Center e1)
      | _ -> () in
    let _ = (* bottom(_) *)
      match role1 with
      | `Layer -> push (`IntCoord (`I, `Pos), `Bottom e1)
      | _ -> () in
    let _ = (* middle(_) *)
      match role1 with
      | `Layer -> push (`IntCoord (`I, `Pos), `Middle e1)
      | _ -> () in
    let _ = (* ProjI/J *)
      match role1 with
      | `Vec _ ->
         push (role1, `ProjI e1);
         push (role1, `ProjJ e1)
      | _ -> () in
    let _ = (* MajorityColor on grids *)
      match role1 with
      | `Grid -> push (`Color `Shape, `MajorityColor e1)
      | _ -> () in
    let _ = (* ColorCount on grids *)
      match role1 with
      | `Grid -> push (`IntCard, `ColorCount e1)
      | _ -> () in
    let _ = (* Strip on grids *)
      match role1 with
      | `Grid -> push (`Grid, `Strip e1)
      | _ -> () in
    let _ = (* PeriodicFactor *)
      match role1 with
      | `Mask | `Shape | `Layer ->
         let& mode = [`TradeOff; `Strict] in
         let bgcolor = 0 in
         push (role1, `PeriodicFactor (mode, `Color bgcolor, e1))
      | `Grid ->
         let& mode = [`TradeOff; `Total; `Strict] in
         let& bgcolor = [`Color Grid.black; `MajorityColor e1] in
         push (role1, `PeriodicFactor (mode,bgcolor,e1))
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
        | `IntCoord ((`I | `J as ij1), `Pos), `IntCoord ((`I | `J as ij2), `Pos) when ij1=ij2 && e1 < e2 ->
           push (`IntCoord (ij1, `Pos), `Span (e1,e2))
        | `Vec `Pos, `Vec `Pos when e1 < e2 ->
           push (`Vec (`Size `X), `Span (e1,e2))
        | _ -> () in
      let _ = (* min([_;_]) *)
        match role1, role2 with
        | `IntCoord xx1, `IntCoord xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Min [e1;e2])
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Min [e1;e2])
        | _ -> () in
      let _ = (* max([_;_]) *)
        match role1, role2 with
        | `IntCoord xx1, `IntCoord xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Max [e1;e2])
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Max [e1;e2])
        | _ -> () in
      let _ = (* average([_;_]) *)
        match role1, role2 with
        | `IntCoord xx1, `IntCoord xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Average [e1;e2])
        | `Vec xx1, `Vec xx2 when xx1 = xx2 && e1 < e2 ->
           push (role1, `Average [e1;e2])
        | _ -> () in
      let _ = (* translation = pos - pos *)
        match role1, role2 with
        | `IntCoord (ij1, `Pos), `IntCoord (ij2, `Pos) when ij1=ij2 && e1 <> e2 ->
           push (`IntCoord (ij1, `Move), `Minus (e1,e2))
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
      let _ = (* Crop on grids *)
        match role1, role2 with
        | `Grid, `Layer -> push (`Grid, `Crop (e1,e2))
        | _ -> () in
      () in
  let exprs_1 = List.rev !exprs in
  (* LEVEL 2, not generating Move at this level *)
  let _ = 
    let _ = (* int and vec constants *)
      let& k = [0;1;2;3] in
      push (`IntCoord (`X, (if k=0 then `Pos else `X)), `ConstInt k);
      let& l = [0;1;2;3] in
      push (`Vec (if k=0 && l=0 then `Pos else `X), `ConstVec (k,l)) in
    let& (role1,e1) = exprs_1 in
    (* unary operators *)
    let _ = (* IncrInt, DecrInt *)
      match role1 with
      | `IntCoord (_,rvec) when rvec <> `Move ->
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
    let _ = (* ScaleUp, ScaleDown by constant *)
      let& n = [2;3] in
      let e2 = `ConstInt n in
      match role1 with
      | `IntCoord (_,rvec) | `Vec rvec when rvec <> `Move ->
         push (role1, `ScaleUp (e1,e2));
         push (role1, `ScaleDown (e1,e2))
      | `Mask | `Shape | `Grid ->
         push (role1, `ScaleUp (e1,e2));
         push (role1, `ScaleDown (e1,e2))
      | _ -> () in
    let _ = (* 0 + move -> pos *)
      match role1 with
      | `IntCoord (ij, `Move) -> push (`IntCoord (ij, `Pos), `Plus (`ConstInt 0, e1))
      | `Vec `Move -> push (`Vec `Pos, `Plus (`ConstVec (0,0), e1))
      | _ -> () in
    let _ = (* ApplySym *)
      match role1 with
      | (`Mask | `Shape | `Layer | `Grid as role1) ->
         let& sym = all_symmetry in
         push (role1, `ApplySym (sym, e1, role1))
      | _ -> () in
    let& (role2,e2) = exprs_1 in
      (* binary operators *)
      let _ = (* ScaleUp, ScaleDown by IntCard-expression*)
        match role1, role2 with
        | (`IntCoord (_,rvec) | `Vec rvec), `IntCard when rvec <> `Move ->
           push (role1, `ScaleUp (e1,e2));
           push (role1, `ScaleDown (e1,e2))
        | (`Mask | `Shape | `Grid), `IntCard ->
           push (role1, `ScaleUp (e1,e2));
           push (role1, `ScaleDown (e1,e2))
        | _ -> () in
      let _ = (* Coloring *)
        match role1, role2 with
        | (`Shape | `Layer), `Color _ ->
           push (role1, `Coloring (e1, e2))
        | _ -> () in
      let _ = (* _ + _ *)
        match role1, role2 with
        | `IntCoord (_, xx1), `IntCoord (_, (`X | `Size _ | `Move))
          | `Vec xx1, `Vec (`X | `Size _ | `Move)
             when xx1 <> `Move && (if xx1 = `Pos then e1 <> e2 else e1 < e2) ->
           push (role1, `Plus (e1,e2)) (* TODO: Abs(e2) if e1:Size and e2:Move *)
        | _ -> () in
      let _ = (* _ - _ *)
        match role1, role2 with (* not generating Moves *)
        | `IntCoord (_, rvec), `IntCoord (_, (`X | `Size _ | `Move)) when rvec <> `Move && e1 <> e2 ->
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
      | (`Vec (`X | `Size _) | `Mask | `Shape | `Grid) ->
         let& k = [1;2;3] in
         let& l = [1;2;3] in
         if k>1 || l>1 then
           push (role1, `Tiling (e1,k,l))
      | _ -> () in
    let _ = (* FillResizeAlike *)
      match role1 with
      | `Vec (`X | `Size _) ->
         let& (role2,e2) = exprs_0 in (* no expression for the mask-grid *)
         (match role2 with
          | `Mask | `Shape | `Layer ->
             let& mode = [`TradeOff; `Strict] in
             let bgcolor = `Color 1 in
             push (role2, `FillResizeAlike (mode,bgcolor,e1,e2))
          | `Grid ->
             let& mode = [`TradeOff; `Total; `Strict] in
             let& bgcolor = [`Color Grid.black; `MajorityColor e2] in
             push (role2, `FillResizeAlike (mode,bgcolor,e1,e2))
          | _ -> ())
      | _ -> () in
    let _ = (* Compose(c,e1,e1) *)
      match role1 with
      | `Mask | `Shape ->
         push (role1, `Compose (`Color 1, e1, e1))
      | `Grid ->
         push (role1, `Compose (`MajorityColor e1, e1, e1));
         let& c = Grid.all_colors in
         push (role1, `Compose (`Color c, e1, e1))
      | _ -> () in
    let _ = (* UnfoldSym *)
      match role1 with
      | `Mask | `Shape | `Layer | `Grid ->
         let& sym_matrix = all_symmetry_unfold in
         push (role1, `UnfoldSym (sym_matrix, e1))
      | _ -> () in
    let _ = (* CloseSym on masks *)
      match role1 with
      | `Mask | `Shape | `Layer ->
         let& sym_seq = all_symmetry_close in
         push (role1, `CloseSym (sym_seq, `Color Grid.black, e1)) (* bgcolor does not matter for masks *)
      | _ -> () in
    let _ = (* SwapColors(_, c1, c2) *)
      match role1 with
      | `Grid ->
         let& c1 = Grid.all_colors in
         push (role1, `SwapColors (e1, `Color c1, `MajorityColor e1));
         let& c2 = Grid.all_colors in
         if c1 > c2 (* symmetric operation *)
         then push (role1, `SwapColors (e1, `Color c1, `Color c2))
         else ()
      | _ -> () in
    let _ = (* not _ *)
      match role1 with
      | `Mask -> push (`Mask, `LogNot e1)
      | _ -> () in
    let& (role2,e2) = exprs_2 in
      let _ = (* ScaleTo on masks *)
        match role1, role2 with
        | (`Mask | `Shape | `Grid), `Vec (`X | `Size _) ->
           push (role1, `ScaleTo (e1,e2))
        | _ -> () in
      let _ = (* CloseSym on grids *)
        match role1, role2 with
        | `Grid, `Color _ ->
           let& sym_seq = all_symmetry_close in
           push (role1, `CloseSym (sym_seq, e2, e1))
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
      let _ = (* Stack *)
        match role1, role2 with
        | `Grid, `Grid when e1 <> e2 -> push (`Grid, `Stack [e1; e2])
        | _ -> () in
      () in
  ()
   with End_of_file -> ());
  (*  Printf.printf "== %d expressions ==\n" (List.length !exprs); flush stdout; *)
  List.rev !exprs)) (* approximate order in increasing size *)
  
let shape_refinements ~(env_sig : signature) (t : template) : grid_refinement Myseq.t =
  Common.prof "Model2.shape_refinements" (fun () ->
  let rec aux ~(objs : template list) lp = function
    | `Nil ->
       let* obj = Myseq.from_list objs in
       Myseq.return (RAdd (`Field (`Layer lp, `Root), obj))
    | `Insert (above,_,below)
      | `Append (above,below) ->
       Myseq.concat
         [ aux ~objs (`Right lp) below; (* insert below first *)
           aux ~objs (`Left lp) above ]
    | _ -> assert false
  in
  match t with
  | `Background (_,_,_,layers) ->
     if ilist_length layers >= !max_nb_layers
     then Myseq.empty
     else (
     (* TEST let su =
       let objs = [`PosShape (u_vec_cst, `Any)] in
       aux ~objs `Root layers in *)
     let sp =
       let objs = [`PosShape (u_vec_cst, `Point (u_cst))] in (* TEST *)
       aux ~objs `Root layers in
     let sr =
       let objs = [`PosShape (u_vec_cst, `Rectangle (u_vec_cst, u_cst, u_cst))] in (* TEST *)
       aux ~objs `Root layers in
     let ss =
       let ps_shape = signature_of_kind env_sig Shape in
       Myseq.concat
         (List.map
            (fun p_shape ->
              let objs = [`PosShape (u_vec_cst, `Ref p_shape)] in
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
     Myseq.concat [so; ss; sr; sp (* TEST ; su *)])
  | #expr -> Myseq.empty
  | _ -> assert false)

let prune_refinements (t : template) : grid_refinement Myseq.t =
  let l =
    fold_template
      (fun res p1 t1 anc1 ->
        match p1, t1 with
        | _, `Color _ -> RGen (p1, `Any) :: res
        | _, `Mask _ -> RGen (p1, `Any) :: res
        | `Field (`Layer _, _), _ -> RDel p1 :: res
        | _ -> res)
      [] `Root t [] in
  Myseq.from_list l
  
let grid_refinements ~(env_sig : signature) (t : template) (grss : grid_read list list) : (grid_refinement * template) Myseq.t =
  Myseq.prof "Model2.grid_refinements" (
  Myseq.concat
    [defs_refinements ~env_sig t grss;
     shape_refinements ~env_sig t]
  |> Myseq.filter_map
       (fun r ->
         pp_grid_refinement r; print_newline (); (* TEST *)
         let|? t' = apply_grid_refinement r t in
         Some (r,t')
    ))

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
  Mask_model.reset_memoized_functions ();
  Segment.reset_memoized_functions ();
  reset_memoized_functions_apply ();
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

let apply_refinement (r : refinement) (m : model) : model option = (* QUICK *)
  match r with
  | RInit -> None
  | Rinput gr ->
     apply_grid_refinement gr m.input_pattern
     |> Option.map (fun t -> {m with input_pattern = t})
  | Routput gr ->
     apply_grid_refinement gr m.output_template
     |> Option.map (fun t -> {m with output_template = t})
                 
let model_refinements_build (last_r : refinement) (m : model) (gsri : grids_read) (gsro : grids_read) : (refinement * model) Myseq.t
  =
  Common.prof "Model2.model_refinements_build" (fun () ->
  let envo_sig = signature_of_template m.input_pattern in
  let ref_defis =
    defs_refinements ~env_sig:signature0 m.input_pattern gsri.reads
    |> Myseq.filter_map (fun gr ->
           let r = Rinput gr in
           let|? m' = apply_refinement r m in
           Some (r,m')) in
  let ref_defos =
    defs_refinements ~env_sig:envo_sig m.output_template gsro.reads
    |> Myseq.filter_map (fun gr ->
           let r = Routput gr in
           let|? m' = apply_refinement r m in
           Some (r,m')) in
  let ref_shapis =
    if grids_read_has_delta gsri
    then
      shape_refinements ~env_sig:signature0 m.input_pattern
      |> Myseq.filter_map (fun gr ->
             let r = Rinput gr in
             let|? m' = apply_refinement r m in
             Some (r,m'))
    else Myseq.empty in
  let ref_shapos =
    if grids_read_has_delta gsro
    then
      shape_refinements ~env_sig:envo_sig m.output_template
      |> Myseq.filter_map (fun gr ->
             let r = Routput gr in
             let|? m' = apply_refinement r m in
             Some (r,m'))
    else Myseq.empty in
  Myseq.concat
    [ref_shapis; ref_shapos; ref_defis; ref_defos])

let model_refinements_prune (m : model) : (refinement * model) Myseq.t =
  Common.prof "Model2.model_refinements_prune" (fun () ->
  prune_refinements m.input_pattern
  |> Myseq.filter_map (fun gr ->
         let r = Rinput gr in
         let|? m' = apply_refinement r m in
         Some (r,m')))
  
let dl_model_data (gpsr : grid_pairs_read) : dl triple triple = (* QUICK *)
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
  (lmdi, lmdo, lmdi +. lmdo)

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
      ?(verbose = 1) (* verbose level *)
      ?(grid_viz = false)
      ?(pause = 0.)
      ~timeout_build ~timeout_prune
      ~init_model
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : (model * grid_pairs_read * bool) double
  = Common.prof "Model2.learn_model" (fun () ->
  Grid.reset_memoized_functions ();
  Mask_model.reset_memoized_functions ();
  Segment.reset_memoized_functions ();
  reset_memoized_functions_apply ();
  let norm_dl_model_data = make_norm_dl_model_data () in
  let data_of_model ~pruning m =
    Result.to_option
      (let| gprs = read_grid_pairs ~pruning m pairs in
       let grsi, grso = split_grid_pairs_read gprs in
       let dl_triples = norm_dl_model_data gprs in
       Result.Ok (gprs,grsi,grso,dl_triples)) in
  let viz_grid_pairs_reads m gpsr =
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
      Unix.sleepf pause)  in
  let lm_build, timed_out_build =
  Mdl.Strategy.beam
    ~timeout:timeout_build
    ~beam_width
    ~refine_degree
    ~m0:(RInit, init_model)
    ~data:(fun (r,m) ->
      try
        if verbose >= 3 then (
          print_string "\t=> "; pp_refinement r; print_newline ());
        data_of_model ~pruning:false m
      with
      | Common.Timeout as exn -> raise exn
      | exn ->
         print_endline "ERROR while parsing examples with new model";
	 print_endline (Printexc.to_string exn);
	 pp_refinement r; print_newline ();
         pp_model m; print_newline ();
	 raise exn)
    ~code:(fun (r,m) (gpsr,gsri,gsro,dl_triples) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) = dl_triples in
           if verbose >= 2 then (
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
    (fun (r,m) (gpsr,gsri,gsro,dl_triples) dl ->
      if verbose >= 2 then print_newline ();
      if verbose >= 1 then (Printf.printf "%.3f\t" dl; pp_refinement r; print_newline ());
      if verbose >= 2 then (
        print_endline " ===> first read for first example";
        List.hd (List.hd gpsr.reads)
        |> (fun ((_,{data=d_i},dl_i), (_, {data=d_o}, dl_o), dl) ->
          print_endline " --- some read ---";
          pp_data d_i; print_newline ();
          pp_data d_o; print_newline ();
          Printf.printf "\tdl=%.1f\n" dl);
        print_newline ());
      viz_grid_pairs_reads m gpsr;
        (*pp_grids_read "### OUT grids_read ###" gsro;*)
      (*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
      flush stdout;
      let refs = model_refinements_build r m gsri gsro in
      refs) in
  match lm_build with
  | [] -> assert false
  | ((_,m_build), (gpsr_build,_,_,_), _)::_ ->
     let lm_prune, timed_out_prune =
       if timeout_prune = 0 (* no pruning *)
       then lm_build, timed_out_build
       else
         Mdl.Strategy.beam
           ~timeout:timeout_prune
           ~beam_width:1
           ~refine_degree
           ~m0:(RInit, m_build)
           ~data:(fun (r,m) ->
             data_of_model ~pruning:true m)
           ~code:(fun (r,m) (gpsr,gsri,gsro,dl_triples) ->
	     let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) = dl_triples in
             if verbose >= 2 then (
               Printf.printf "\t?? %.3f\t" lmd;
               pp_refinement r; print_newline ());
             lmd) (* only parse ranks counted for input grids *)
           ~refinements:(fun (r,m) (gpsr,gsri,gsro,dl_triples) dl ->
             if verbose >= 1 then (Printf.printf "%.3f\t" dl; pp_refinement r; print_newline ());
             viz_grid_pairs_reads m gpsr;
             model_refinements_prune m) in
     match lm_prune with
     | [] -> assert false
     | ((_,m_prune), (gpsr_prune,_,_,_), _)::_ ->
        (m_build, gpsr_build, timed_out_build),
        (m_prune, gpsr_prune, timed_out_prune))

