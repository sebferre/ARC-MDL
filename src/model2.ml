
open Task

let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v
   
let alpha = def_param "alpha" 10. string_of_float
let max_nb_parse = def_param "max_nb_parse" 512 string_of_int (* max nb of considered grid parses *)
let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
let max_nb_shape_parse = def_param "max_nb_shape_parse" 16 string_of_int (* max nb of parses for a shape *)
let max_nb_diff = def_param "max_nb_diff" 3 string_of_int (* max nb of allowed diffs in grid parse *)
let max_nb_grid_reads = def_param "max_nb_grid_reads" 3 string_of_int (* max nb of selected grid reads, passed to the next stage *)

exception TODO

(* binders and syntactic sugar *)
        
let ( let| ) res f = Result.bind res f
let ( let* ) seq f = seq |> Myseq.flat_map f

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

(* type definitions for data, expressions, templates *)
    
type 'a patt =
  [ `Bool of bool
  | `Int of int
  | `Color of Grid.color
  | `Mask of Grid.Mask.t option
  | `Vec of 'a * 'a (* i, j -> vec *)
  | `Point of 'a * 'a (* pos, color -> shape *)
  | `Rectangle of 'a * 'a * 'a * 'a (* pos, size, color, mask -> shape *)
  | `Background of 'a * 'a * 'a ilist (* size, color, layers (top first) -> grid *)
  | `Many of bool * 'a list (* many-valued data: ordered, values (shapes) *)
  ]

type field =
  [ `I
  | `J
  | `Pos
  | `Color
  | `Size
  | `Mask
  | `Layer of ilist_revpath ]
type arg = [
  | `Plus1 | `Plus2
  | `Minus1 | `Minus2
  | `Indexing1 | `Indexing2 ]
type revpath =
  [ `Root
  | `Field of field * revpath
  | `Arg of arg * revpath
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
  | `Arg (a,p) -> Some p
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

type kind =
  [ `Int | `Bool | `Color | `Mask | `Vec | `Shape | `Layer | `Grid ]
let all_kinds =
  [ `Int;  `Bool;  `Color;  `Mask;  `Vec;  `Shape;  `Layer;  `Grid ]

let rec path_kind (p : revpath) : kind =
  match p with
  | `Root -> `Grid
  | `Field (f,_) ->
     (match f with
      | (`I | `J) -> `Int
      | `Color -> `Color
      | `Mask -> `Mask
      | (`Pos | `Size) -> `Vec
      | `Layer _ -> `Layer)
  | `Arg (a,p1) ->
     (match a with
      | (`Plus1 | `Plus2 | `Minus1 | `Minus2) -> `Int
      | `Indexing1 -> path_kind p1
      | `Indexing2 -> `Int)
  | `Item (_,`Root, `Field (`Layer _, _)) -> `Shape
  | `Item (_,`Root, ctx) -> path_kind ctx
  | `Item (_,local,_) -> path_kind local

type role = (* same information as kind + contextual information *)
  [ `Int of [`I | `J] * role_vec
  | `Index
  | `Color of role_frame
  | `Mask
  | `Vec of role_vec
  | `Shape
  | `Layer
  | `Grid ]
and role_vec =
  [ `Pos | `Size of role_frame ]
and role_frame =
  [ `Shape | `Grid ]

let rec path_role ?(ctx = None) (p : revpath) : role =
  match p with
  | `Root ->
     (match ctx with
      | None -> `Grid
      | Some (`Field (`Layer _, _)) -> `Shape
      | Some p -> path_role p)
  | `Field ((`I | `J as f), p1) -> `Int (f, path_role_vec ~ctx p1)
  | `Field (`Color, p1) -> `Color (path_role_frame ~ctx p1)
  | `Field (`Mask, _) -> `Mask
  | `Field (`Pos, _) -> `Vec `Pos
  | `Field (`Size, p1) -> `Vec (`Size (path_role_frame ~ctx p1))
  | `Field (`Layer _, _) -> `Layer
  | `Arg ((`Plus1 | `Plus2 | `Minus1 | `Minus2), p1) -> path_role p1
  | `Arg (`Indexing1,p1) -> path_role p1
  | `Arg (`Indexing2,p2) -> `Index
  | `Item (_,local,ctx) -> path_role ~ctx:(Some ctx) local
and path_role_vec ~ctx : revpath -> role_vec = function
  | `Field (`Pos, _) -> `Pos
  | `Field (`Size, p1) -> `Size (path_role_frame ~ctx p1)
  | _ -> assert false
and path_role_frame ~ctx : revpath -> role_frame = function
  | `Root when ctx = None -> `Grid
  | `Root -> `Shape
  | `Field (`Layer _, _) -> `Shape
  | _ -> assert false
       
type data = data patt
let data0 = `Background (`Vec (`Int 0, `Int 0), `Color Grid.black, `Nil)

type var =
  [ revpath
  | `Index (* reference to current item index in For-loop *)
  ]

type 'a expr =
  [ `Ref of revpath
  | `Index (* Int *)
  | `Plus of 'a * 'a (* (Int, Int) => Int *)
  | `Minus of 'a * 'a (* (Int, Int) => Int *)
  | `Indexing of 'a * 'a (* (Many A, Int) => A *)
  ]
         
type template =
  [ `U
  | `Repeat of template patt
  | `For of revpath * template (* revpath is a many-valued ref *)
  | template patt
  | template expr ] (* TODO: should sub-expressions be restricted to patt and expr ? *)

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

let rec string_of_ilist_path : ilist_revpath -> string =
  function
  | `Root -> "0"
  | `Left p -> string_of_ilist_path p ^ "0"
  | `Right p -> string_of_ilist_path p ^ "1"

let string_of_kind : kind -> string = function
  | `Bool -> "bool"
  | `Int -> "int"
  | `Color -> "color"
  | `Mask -> "mask"
  | `Vec -> "vector"
  | `Shape -> "shape"
  | `Layer -> "layer"
  | `Grid -> "grid"

let rec string_of_role = function
  | `Int (`I, rv) -> "i/" ^ string_of_role_vec rv
  | `Int (`J, rv) -> "j/" ^ string_of_role_vec rv
  | `Index -> "index"
  | `Color rf -> "color/" ^ string_of_role_frame rf
  | `Mask -> "mask"
  | `Vec rv -> "vec/" ^ string_of_role_vec rv
  | `Shape -> "shape"
  | `Layer -> "layer"
  | `Grid -> "grid"
and string_of_role_vec = function
  | `Pos -> "pos"
  | `Size rf -> "size/" ^ string_of_role_frame rf
and string_of_role_frame = function
  | `Shape -> "shape"
  | `Grid -> "grid"
           
let string_of_field : field -> string = function
  | `I -> "i"
  | `J -> "j"
  | `Pos -> "pos"
  | `Color -> "color"
  | `Size -> "size"
  | `Mask -> "mask"
  | `Layer lp -> "layer[" ^ string_of_ilist_path lp ^ "]"

let string_of_arg : arg -> string = function
  | `Plus1 -> "plus1"
  | `Plus2 -> "plus2"
  | `Minus1 -> "minus1"
  | `Minus2 -> "minus2"
  | `Indexing1 -> "indexing1"
  | `Indexing2 -> "indexing2"
               
let rec string_of_path : revpath -> string = function
  | `Root -> ""
  | `Field (f,p) -> string_of_path p ^ "." ^ string_of_field f
  | `Arg (a,p) -> string_of_path p ^ ":" ^ string_of_arg a
  | `Item (None,`Root,ctx) -> string_of_path ctx ^ ".*"
  | `Item (None,local,ctx) -> string_of_path ctx ^ ".*" ^ string_of_path local
  | `Item (Some i,`Root,ctx) -> string_of_path ctx ^ "[" ^ string_of_int i ^ "]"
  | `Item (Some i,local,ctx) -> string_of_path ctx ^ "[" ^ string_of_int i ^ "]" ^ string_of_path local

let pp_path p = print_string (string_of_path p)

let rec string_of_ilist (string : 'a -> string) (l : 'a ilist) : string =
  let rec aux lp = function
    | `Nil -> ""
    | `Insert (left,elt,right) ->
       aux (`Left lp) left
       ^ "\n  [" ^ string_of_ilist_path lp ^ "]: " ^ string elt
       ^ aux (`Right lp) right
  in
  aux `Root l
      
let rec string_of_patt (string : 'a -> string) : 'a patt -> string = function
  | `Bool b -> if b then "true" else "false"
  | `Int i -> string_of_int i
  | `Color c -> Grid.name_of_color c
  | `Mask m_opt ->
     (match m_opt with
      | None -> "full"
      | Some m -> Grid.Mask.to_string m)
  | `Vec (i,j) ->
     "(" ^ string i ^ "," ^ string j ^ ")"
  | `Point (pos,color) ->
     "a point at " ^ string pos
     ^ " with color " ^ string color
  | `Rectangle (pos,size,color,mask) ->
     "a rectangle at " ^ string pos
     ^ " with size " ^ string size
     ^ " and color " ^ string color
     ^ " and mask " ^ string mask
  | `Background (size,color,layers) ->
     "a background with size " ^ string size
     ^ " and color " ^ string color
     ^ " and layers" ^ string_of_ilist string layers
  | `Many (ordered,l) ->
     let contents = String.concat ",\n\t" (List.map string l) in
     if ordered
     then "[ " ^ contents ^ " ]"
     else "{\n\t" ^ contents ^ " }"

let rec string_of_data : data -> string = function
  | #patt as patt -> string_of_patt string_of_data patt

let pp_data d = print_string (string_of_data d)

let string_of_index = "$index"
              
let string_of_var : var -> string = function
  | #revpath as p -> string_of_path p
  | `Index -> string_of_index
  
let rec string_of_expr (string : 'a -> string) : 'a expr -> string = function
  | `Ref p -> string_of_path p
  | `Index -> string_of_index
  | `Plus (a,b) ->
     string a ^ " + " ^ string b
  | `Minus (a,b) ->
     string a ^ " - " ^ string b
  | `Indexing (e1,e2) ->
     string e1 ^ "[" ^ string e2 ^ "]"

(*let pp_expr e = print_string (string_of_expr e)*)

let rec string_of_template : template -> string = function
  | `U -> "?"
  | `Repeat patt -> "repeat " ^ string_of_patt string_of_template patt
  | `For (p,e1) -> "for {" ^ string_of_path p ^ "}: " ^ string_of_template e1
  | #patt as patt -> string_of_patt string_of_template patt
  | #expr as e -> string_of_expr string_of_template e

let pp_template t = print_string (string_of_template t)

let string_of_signature (sg : signature) : string =
  String.concat "\n"
    (List.map
       (fun (k,ps) ->
         string_of_kind k ^ ": "
         ^ String.concat ", "
             (List.map string_of_path ps))
       sg)
                  
let pp_diff diff =
  diff
  |> List.iter (fun p1 -> print_string "  "; pp_path p1)
                  
let pp_delta delta =
  delta
  |> List.sort Stdlib.compare
  |> List.iter (fun (i,j,c) -> Printf.printf " (%d,%d)=" i j; Grid.pp_color c)
    
let pp_grid_data gd =
  print_string "data: "; pp_data gd.data; print_newline ();
  print_string "diff: "; pp_diff gd.diff; print_newline ();
  print_string "delta:"; pp_delta gd.delta; print_newline ()


(* data utilities *)

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

let find_field_patt (find : 'a -> 'a option) (f : field) (patt_parent : 'a patt) : 'a option = (* only using head field of p *)
  match f, patt_parent with
  | `I, `Vec (i,j) -> find i
  | `J,`Vec (i,j) -> find j
  | `Pos, `Point (pos,color) -> find pos
  | `Color, `Point (pos,color) -> find color
  | `Pos, `Rectangle (pos,size,color,mask) -> find pos
  | `Size, `Rectangle (pos,size,color,mask) -> find size
  | `Color, `Rectangle (pos,size,color,mask) -> find color
  | `Mask, `Rectangle (pos,size,color,mask) -> find mask
  | `Size, `Background (size,color,layers) -> find size
  | `Color, `Background (size,color,layers) -> find color
  | `Layer lp, `Background (size,color,layers) ->
     (match find_ilist lp layers with
      | None -> None
      | Some layer -> find layer)
  | _ ->
     print_string (string_of_field f); print_string ": ";
     print_string (string_of_patt (fun _ -> "_") patt_parent);
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
    | None -> Some t
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
       
let fold_patt (fold : 'b -> revpath -> 'a -> 'b) (acc : 'b) (p : revpath) (patt : 'a patt) : 'b =
  match patt with
  | `Bool _ | `Int _ | `Color _ | `Mask _ -> acc
  | `Vec (i,j) ->
     let acc = fold acc (p ++ `I) i in
     let acc = fold acc (p ++ `J) j in
     acc
  | `Point (pos,color) ->
     let acc = fold acc (p ++ `Pos) pos in
     let acc = fold acc (p ++ `Color) color in
     acc
  | `Rectangle (pos,size,color,mask) ->
     let acc = fold acc (p ++ `Pos) pos in
     let acc = fold acc (p ++ `Size) size in
     let acc = fold acc (p ++ `Color) color in
     let acc = fold acc (p ++ `Mask) mask in
     acc
  | `Background (size,color,layers) ->
     let acc = fold acc (p ++ `Size) size in
     let acc = fold acc (p ++ `Color) color in
     let acc =
       fold_ilist
         (fun acc lp shape ->
           fold acc (p ++ `Layer lp) shape)
         acc `Root layers in
     acc
  | `Many (ordered,items) ->
     let _, acc =
       List.fold_left
         (fun (i,acc) item -> i+1, fold acc (ith_item i p) item)
         (0,acc) items in
     acc

let rec fold_data (f : 'b -> revpath -> data -> 'b) (acc : 'b) (p : revpath) (d : data) : 'b =
  let acc = f acc p d in
  fold_patt (fold_data f) acc p d
  
let rec fold_template (f : 'b -> revpath option -> revpath -> template -> 'b) (acc : 'b) (for_p : revpath option) (p : revpath) (t : template) : 'b =
  let acc = f acc for_p p t in
  match t with
  | `U -> acc
  | `Repeat patt1 ->
     let p1 = any_item p in
     let acc = f acc for_p p1 (patt1 :> template) in
     fold_patt (fun acc p t -> fold_template f acc for_p p t) acc p1 patt1
  | `For (p_many,t1) ->
     let for_p1 = Some p_many in
     let p1 = any_item p in
     let acc = f acc for_p1 p1 t1 in
     fold_template f acc for_p1 p1 t1
  | #patt as patt -> fold_patt (fun acc p t -> fold_template f acc for_p p t) acc p patt
  | #expr -> acc
       
let size_of_data (d : data) : int =
  Common.prof "Model2.size_of_data" (fun () ->
  fold_data (fun res _ _ -> res+1) 0 path0 d)
let size_of_template (t : template) : int =
  fold_template (fun res _ _ _ -> res+1) 0 None path0 t

let signature_of_template (t : template) : signature =
  Common.prof "Model2.signature_of_template" (fun () ->
  let ht = Hashtbl.create 13 in
  let () =
    fold_template
      (fun () for_p p t1 ->
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
        if k = `Vec && t1 = `U then (
          let ps0 =
            match Hashtbl.find_opt ht `Int with
            | None -> []
            | Some ps -> ps in
          Hashtbl.replace ht `Int (p ++ `I :: p ++ `J :: ps0)
        ))
      () None path0 t in
  Hashtbl.fold
    (fun k ps res -> (k, List.rev ps)::res) (* reverse to put them in order *)
    ht [])

let signature_of_kind (sg : signature) (k : kind) : revpath list =
  match List.assoc_opt k sg with
  | Some ps -> ps
  | None -> []
  
let rec default_data_of_path (p : revpath) : data =
  match path_role p with
  | `Int (_, `Pos) -> `Int 0
  | `Int (_, `Size `Grid) -> `Int 10
  | `Int (_, `Size `Shape) -> `Int 2
  | `Index -> `Int 0
  | `Color `Grid -> `Color Grid.black
  | `Color `Shape -> `Color Grid.no_color
  | `Mask -> `Mask None
  | `Vec `Pos -> `Vec (`Int 0, `Int 0)
  | `Vec (`Size `Grid) -> `Vec (`Int 10, `Int 10)
  | `Vec (`Size `Shape) -> `Vec (`Int 2, `Int 2)
  | `Shape -> `Rectangle (`Vec (`Int 0, `Int 0), `Vec (`Int 2, `Int 2), `Color Grid.no_color, `Mask None)
  | `Layer -> `Rectangle (`Vec (`Int 0, `Int 0), `Vec (`Int 2, `Int 2), `Color Grid.no_color, `Mask None)
  | `Grid -> `Background (`Vec (`Int 10, `Int 10), `Color Grid.black, `Nil)

let rec root_template_of_data (d : data) : template list = (* QUICK *)
  match d with
  | `Bool _
    | `Int _
    | `Color _
    | `Mask _ -> [(d :> template)]
  | `Vec _ -> [`Vec (`U, `U)]
  | `Point _ -> [`Point (`U, `U)]
  | `Rectangle _ -> [`Rectangle (`U, `U, `U, `U)]
  | `Background _ -> assert false (* should not happen *)
  | `Many (ordered,items) ->
     let llt_items = List.map root_template_of_data items in
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
  | `Mask m1, `Mask m2 when m1 = m2 -> true
  | `Vec (i1,j1), `Vec (i2,j2) ->
     matches_template i1 i2 && matches_template j1 j2
  | `Point (pos1,color1), `Point (pos2,color2) ->
     matches_template pos1 pos2 && matches_template color1 color2
  | `Rectangle (pos1,size1,color1,mask1), `Rectangle (pos2,size2,color2,mask2) ->
     matches_template pos1 pos2 && matches_template size1 size2
     && matches_template color1 color2 && matches_template mask1 mask2
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

let dl_bool : bool -> dl =
  fun b -> 1.
let dl_nat : int -> dl =
  fun i -> Mdl.Code.universal_int_star i
let dl_index ~bound : int -> dl = (* all positions are alike *)
  fun i -> Mdl.Code.uniform bound
let dl_length : int -> dl = (* longer lengths cover more pixels *)
  fun i -> Mdl.Code.universal_int_plus i
let dl_color : Grid.color -> dl =
  fun c -> Mdl.Code.uniform Grid.nb_color
let dl_background_color : Grid.color -> dl =
  function
  | 0 -> Mdl.Code.usage 0.91
  | c ->
     if c > 0 && c < 10 (* 9 colors *)
     then Mdl.Code.usage 0.01
     else invalid_arg "Unexpected color"
let dl_mask : Grid.Mask.t option -> dl =
  function
  | None -> Mdl.Code.usage 0.5
  | Some m ->
     let n = Grid.Mask.height m * Grid.Mask.width m in
     Mdl.Code.usage 0.5 +. float n (* basic bitmap *)
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
  
let dl_ctx_of_data (d : data) : dl_ctx =
  Common.prof "Model2.dl_ctx_of_data" (fun () ->
  (* retrieving grid size: make assumption on paths *)
  let box_height =
    match find_data (`Field (`I, `Field (`Size, `Root))) d with
    | Some (`Int i) -> i
    | _ -> Grid.max_size in
  let box_width =
    match find_data (`Field (`J, `Field (`Size, `Root))) d with
    | Some (`Int j) -> j
    | _ -> Grid.max_size in
  { box_height; box_width })
      

let dl_patt_as_template = Mdl.Code.usage 0.4

let dl_patt
      (dl : ctx:dl_ctx -> path:revpath -> 'a -> dl)
      ~(ctx : dl_ctx) ~(path : revpath) (patt : 'a patt) : dl =
  match patt with
  | `Int n ->
     ( match path_role path with
       | `Int (`I, `Pos) -> dl_index ~bound:ctx.box_height n
       | `Int (`J, `Pos) -> dl_index ~bound:ctx.box_width n
       | `Int (_, `Size _) -> dl_length n
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

  | `Point (pos,color) ->
     ( match path_role path with `Layer -> Mdl.Code.universal_int_plus 1 | _ -> 0.) (* singleton layer *)
     +. Mdl.Code.usage 0.5
     +. dl ~ctx pos ~path:(path ++ `Pos)
     +. dl ~ctx color ~path:(path ++ `Color)
  | `Rectangle (pos,size,color,mask) ->
     ( match path_role path with `Layer -> Mdl.Code.universal_int_plus 1 | _ -> 0.) (* singleton layer *)
     +. Mdl.Code.usage 0.5
     +. dl ~ctx pos ~path:(path ++ `Pos)
     +. dl ~ctx size ~path:(path ++ `Size)
     +. dl ~ctx color ~path:(path ++ `Color)
     +. dl ~ctx mask ~path:(path ++ `Mask)

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
     print_string (string_of_patt (fun _ -> "_") patt);
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

let path_similarity ~ctx_path v = (* QUICK *)
  let rec aux lp1' lp2' =
    match lp1', lp2' with
    | [], [] -> 2.
    | `Root::lp1, _ -> aux lp1 lp2'
    | _, `Root::lp2 -> aux lp1' lp2
    | `Field (f1,p1)::lp1, `Field (f2,p2)::lp2 -> aux_field f1 f2 *. aux (p1::lp1) (p2::lp2)
    | `Field (`Layer _, p1)::lp1, [] -> 0.75 *. aux (p1::lp1) []
    | [], `Field (`Layer _, p2)::lp2 -> 0. *. aux [] (p2::lp2)
    | `Arg (_,p1)::lp1, _ -> aux (p1::lp1) lp2' (* TODO: should be refined *)
    | `Item (_,q1,ctx1)::lp1, `Item (_,q2,ctx2)::lp2 -> aux (q1::ctx1::lp1) (q2::ctx2::lp2)
    | `Item (_,q1,ctx1)::lp1, _ -> 0.75 *. aux (q1::ctx1::lp1) lp2'
    | _, `Item (_,q2,ctx2)::lp2 -> 0.5 *. aux lp1' (q2::ctx2::lp2)
    | _ -> assert false (* incompatible kinds *)
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
    | `Layer l1, `Layer l2 -> aux_ilist l1 l2
    | _ -> assert false
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

(*let dl_var ~env_sig ~ctx_path x =
  let usage_index =
    match path_ctx ctx_path with
    | Some _ -> 0.1 (* Index only available in For-loops *)
    | None -> 0. in
  match x with
  | #revpath as p ->
     Mdl.Code.usage (1. -. usage_index)
     +. dl_path ~env_sig ~ctx_path p
  | `Index -> Mdl.Code.usage usage_index *)

type code_expr = (* dls must correspond to a valid prob distrib *)
  { c_ref : dl;
    c_index : dl;
    c_plus : dl;
    c_minus : dl;
    c_indexing : dl }
let code_expr0 =
  { c_ref = infinity;
    c_index = infinity;
    c_plus = infinity;
    c_minus = infinity;
    c_indexing = infinity }

let code_expr_by_kind : (kind * code_expr) list =
  [ `Int, { 
            c_ref = Mdl.Code.usage 0.2;
            c_index = Mdl.Code.usage 0.2; (* TODO: should it be restricted to some paths? *)
            c_indexing = Mdl.Code.usage 0.2;
            c_plus = Mdl.Code.usage 0.2;
            c_minus = Mdl.Code.usage 0.2 };
    `Bool, { code_expr0 with c_ref = Mdl.Code.usage 1. };
    `Color, { code_expr0 with
              c_ref = Mdl.Code.usage 0.5;
              c_indexing = Mdl.Code.usage 0.5 };
    `Mask, { code_expr0 with
             c_ref = Mdl.Code.usage 0.5;
             c_indexing = Mdl.Code.usage 0.5 };
    `Vec, { code_expr0 with
            c_ref = Mdl.Code.usage 0.5;
            c_indexing = Mdl.Code.usage 0.5 };
    `Shape, { code_expr0 with
              c_ref = Mdl.Code.usage 0.5;
              c_indexing = Mdl.Code.usage 0.5 };
    `Layer, { code_expr0 with
              c_ref = Mdl.Code.usage 1. };
    `Grid, code_expr0 ]
          
let rec dl_expr
          (dl : ctx:dl_ctx -> path:revpath -> 'a -> dl)
          ~(env_sig : signature) ~(ctx : dl_ctx) ~(path : revpath) (e : 'a expr) : dl =
  let k = path_kind path in
  let code = List.assoc k code_expr_by_kind in
  match e with
  | `Ref p ->
     code.c_ref
     +. dl_path ~env_sig ~ctx_path:path p
  | `Index ->
     code.c_index
  | `Plus (e1,e2) ->
     code.c_plus
     +. dl ~ctx ~path:(`Arg (`Plus1,path)) e1
     +. dl ~ctx ~path:(`Arg (`Plus2,path)) e2 (* TODO: better constraint wrt Pos vs Size *)
  | `Minus (e1,e2) ->
     code.c_minus
     +. dl ~ctx ~path:(`Arg (`Minus1,path)) e1
     +. dl ~ctx ~path:(`Arg (`Minus2,path)) e2
  | `Indexing (e1,e2) ->
     code.c_indexing
     +. dl ~ctx ~path:(`Arg (`Indexing1,path)) e1
     +. dl ~ctx ~path:(`Arg (`Indexing2,path)) e2

type code_template = (* dls must correspond to a valid prob distrib *)
  { c_u : dl;
    c_repeat : dl;
    c_for : dl;
    c_patt : dl;
    c_expr : dl }
let code_template0 =
  { c_u = Mdl.Code.usage 0.1;
    c_repeat = infinity;
    c_for = infinity;
    c_patt = dl_patt_as_template (* Mdl.Code.usage 0.4 *);
    c_expr = 0.5 }

let code_template_by_kind : (kind * code_template) list =
  [ `Int, code_template0;
    `Bool, code_template0;
    `Color, code_template0;
    `Mask, code_template0;
    `Vec, code_template0;
    `Shape, code_template0;
    `Layer, { code_template0 with
              c_repeat = Mdl.Code.usage 0.1;
              c_for = Mdl.Code.usage 0.25;
              c_expr = Mdl.Code.usage 0.15 };
    `Grid, code_template0 ]    
    
let dl_template ~(env_sig : signature) ~(ctx : dl_ctx) ?(path = `Root) (t : template) : dl =
  Common.prof "Model2.dl_template" (fun () ->
  let rec aux ~ctx ~path t =
    let k = path_kind path in
    let code = List.assoc k code_template_by_kind in
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
  | `Point (pos,color), `Point (dpos,dcolor) ->
     dl ~ctx pos dpos ~path:(path ++ `Pos)
     +. dl ~ctx color dcolor ~path:(path ++ `Color)
  | `Rectangle (pos,size,color,mask), `Rectangle (dpos,dsize,dcolor,dmask) ->
     dl ~ctx pos dpos ~path:(path ++ `Pos)
     +. dl ~ctx size dsize ~path:(path ++ `Size)
     +. dl ~ctx color dcolor ~path:(path ++ `Color)
     +. dl ~ctx mask dmask ~path:(path ++ `Mask)
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
    
let rec dl_data_given_template ~(ctx : dl_ctx) ?(path = `Root) (t : template) (d : data) : dl = (* QUICK *)
  match t, d with
  | `U, _ -> dl_data ~ctx ~path d
  | `Repeat patt1, `Many (false,items) ->
     Mdl.Code.list_plus (fun item ->
         dl_data_given_patt dl_data_given_template ~ctx ~path:(any_item path) patt1 item)
       items
  | `Repeat _, _ -> assert false (* only parses into unordered collections *)
  | `For _, _ -> assert false (* should have been evaluated out *)
  | #patt as patt, _ -> dl_data_given_patt dl_data_given_template ~ctx ~path patt d
  | #expr, _ -> assert false (* should have been evaluated out *)
           
let dl_diff ~(ctx : dl_ctx) (diff : diff) (data : data) : dl =
  Common.prof "Model2.dl_diff" (fun () ->
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
         diff)
    
let dl_delta ~(ctx : dl_ctx) (delta : delta) : dl =
  Common.prof "Model2.dl_delta" (fun () ->
  -. 1. (* some normalization to get 0 for empty grid data *)
  +. Mdl.Code.list_star
       (fun (i,j,c) ->
         dl_data ~ctx ~path:(any_item (`Field (`Layer `Root, `Root))) (* dummy path with kind Shape *)
           (`Point (`Vec (`Int i, `Int j), `Color c)))
       delta)

(* NOT using optimized DL below for fair comparisons with model points: 
  +. Mdl.Code.comb nb_pixels area (* where they are *)
  +. float nb_pixels *. Mdl.Code.uniform (Grid.nb_color - 1) (* what are their color, different from the color generated by the model *) *)
    
(* evaluation of expression and templates on environment data *)

exception Unbound_var of var
exception Invalid_expr of template expr
exception Out_of_bound of template list * int
let _ =
  Printexc.register_printer
    (function
     | Unbound_var v -> Some ("unbound variable: " ^ string_of_var v)
     | Invalid_expr e -> Some ("invalid expression: " ^ string_of_expr string_of_template e)
     | Out_of_bound (items,i) -> Some ("out of bound indexing: " ^ string_of_template (`Many (false,items)) ^ "[" ^ string_of_int i ^ "]")
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
  | (`Bool _ | `Int _ | `Color _ | `Mask _ as d) -> d
  | `Vec (i,j) ->
     `Vec (apply ~lookup (p ++ `I) i,
           apply ~lookup (p ++ `J) j)
  | `Point (pos,color) ->
     `Point (apply ~lookup (p ++ `Pos) pos,
             apply ~lookup (p ++ `Color) color)
  | `Rectangle (pos,size,color,mask) ->
     `Rectangle (apply ~lookup (p ++ `Pos) pos,
                 apply ~lookup (p ++ `Size) size,
                 apply ~lookup (p ++ `Color) color,
                 apply ~lookup (p ++ `Mask) mask)
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

let apply_expr_gen
          (apply : lookup:apply_lookup -> revpath -> 'a -> template)
          ~(lookup : apply_lookup) (p : revpath) (e : 'a expr) : template = (* QUICK *)
  match e with
  | `Ref p ->
     (match lookup (p :> var) with
      | Some d -> (d :> template)
      | None -> raise (Unbound_var (p :> var)))
  | `Index as v ->
     (match lookup (v :> var) with
      | Some d -> (d :> template)
      | None -> raise (Unbound_var (v :> var)))
  | `Plus (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Int i1, `Int i2 -> `Int (i1 + i2)
      | _ -> raise (Invalid_expr e))
  | `Minus (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Int i1, `Int i2 -> `Int (i1 - i2)
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

let apply_expr apply ~(env : data) p e =
  apply_expr_gen apply ~lookup:(lookup_of_env env) p e
  
let rec apply_template_gen ~(lookup : apply_lookup) (p : revpath) (t : template) : template = (* QUICK *)
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

let rec apply_template ~(env : data) (p : revpath) (t : template) : (template,exn) Result.t =
  Common.prof "Model2.apply_template" (fun () ->
  try Result.Ok (apply_template_gen ~lookup:(lookup_of_env env) p t)
  with (* catching runtime error in expression eval *)
  | (Unbound_var _ as exn) -> Result.Error exn
  | (Out_of_bound _ as exn) -> Result.Error exn)
(* TODO: remove path argument, seems useless *)


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
          
let rec grid_of_data : data -> Grid.t = function
  | `Background (`Vec (`Int h, `Int w), `Color c, l) ->
     let g = Grid.make h w c in
     draw_layers g l;
     g
  | d -> raise (Invalid_data_as_grid d)
and draw_layers g = function
  | `Nil -> ()
  | `Insert (above, layer, below) ->
     draw_layers g below;
     draw_layer g layer;
     draw_layers g above
and draw_layer g = function
  | `Point (`Vec (`Int i, `Int j), `Color c) ->
     Grid.set_pixel g i j c
  | `Rectangle (`Vec (`Int mini, `Int minj), `Vec (`Int h, `Int w), `Color c, `Mask m_opt) ->
     let maxi = mini + h - 1 in
     let maxj = minj + w - 1 in
     for i = mini to maxi do
       for j = minj to maxj do
	 if (match m_opt with
	     | None -> true
	     | Some m -> Grid.Mask.mem (i-mini) (j-minj) m)
	 then Grid.set_pixel g i j c
       done;
     done
  | `Many (ordered,items) ->
     items |> List.rev |> List.iter (draw_layer g)
  | d -> raise (Invalid_data_as_grid d)


let write_grid ~(env : data) ?(delta = delta0) (t : template) : (Grid.t, exn) Result.t = Common.prof "Model2.write_grid" (fun () ->
  let| t' = apply_template ~env `Root t in
  let d = generate_template `Root t' in
  let g = grid_of_data d in
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

let rec parse_ilist
          (parse_elt : 'a -> ilist_revpath -> 'b -> parse_state -> (data * parse_state) Myseq.t)
          (l : 'a ilist) (lp : ilist_revpath) (x : 'b) (state : parse_state)
      : (data ilist * parse_state) Myseq.t =
  match l with
  | `Nil ->
     Myseq.return (`Nil, state)
  | `Insert (left,elt,right) ->
     let* dleft, state = parse_ilist parse_elt left (`Left lp) x state in
     let* delt, state = parse_elt elt lp x state in
     let* dright, state = parse_ilist parse_elt right (`Right lp) x state in
     let dl = `Insert (dleft, delt, dright) in
     Myseq.return (dl, state)

let parse_repeat
      (parse_item : int -> 'a -> parse_state -> (data * 'a * parse_state) Myseq.t)
      (parts : 'a)
      (state : parse_state)
    : (data list * parse_state) Myseq.t =
  let rec aux i parts state =
    let seq_items = parse_item i parts state in
    if Myseq.is_empty seq_items
    then
      if i = 0
      then Myseq.empty (* the final result lists must not be empty *)
      else Myseq.return ([],state)
    else
      let* item0, parts0, state0 = seq_items in
      Myseq.concat [
          (let* items1, state1 = aux (i+1) parts0 state0 in
           Myseq.return (item0::items1, state1));
          
          aux i parts0 state (* ignoring item0 *)
        ]
  in
  let res = aux 0 parts state in
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
          ~(parse_u : (data * parse_state) Myseq.t)
          ?(parse_repeat : template patt -> (data list * parse_state) Myseq.t = fun _ -> assert false)
          ~(parse_patt : template patt -> (data * parse_state) Myseq.t)
          (t : template)
        : (data * parse_state) Myseq.t =
  match t with
  | `U -> parse_u
  | `Repeat patt1 ->
     let* items, state = parse_repeat patt1 in
     assert (items <> []);
     let data = `Many (false, items) in
     Myseq.return (data,state)
  | `For _ -> assert false
  | #patt as patt -> parse_patt patt
  | #expr -> assert false

let parse_bool t p (b : bool) state = (* QUICK *)
  parse_template
    ~parse_u:(Myseq.return (`Bool b, state))
    ~parse_patt:(function
      | `Bool b0 ->
         if b=b0 then Myseq.return (`Bool b, state)
         else if state.quota_diff > 0 then
           Myseq.return (`Bool b, add_diff p state)
         else Myseq.empty
      | _ -> Myseq.empty)
    t

let parse_int t p (i : int) state = (* QUICK *)
  parse_template
    ~parse_u:(Myseq.return (`Int i, state))
    ~parse_patt:(function
      | `Int i0 ->
         if i=i0 then Myseq.return (`Int i, state)
         else if state.quota_diff > 0 then
           Myseq.return (`Int i, add_diff p state)
         else Myseq.empty
      | _ -> Myseq.empty)
    t

let parse_color t p (c : Grid.color) state = (* QUICK *)
  parse_template
    ~parse_u:(Myseq.return (`Color c, state))
    ~parse_patt:(function
      | `Color c0 ->
         if c=c0 then Myseq.return (`Color c, state)
         else if state.quota_diff > 0 then
           Myseq.return (`Color c, add_diff p state)
         else Myseq.empty
      | _ -> Myseq.empty)
    t
  
let parse_mask t p (m : Grid.Mask.t option) state = (* QUICK *)
  parse_template
    ~parse_u:(Myseq.return (`Mask m, state))
    ~parse_patt:(function
      | `Mask m0 ->
         if m=m0 then Myseq.return (`Mask m, state)
         else if state.quota_diff > 0 then
           Myseq.return (`Mask m, add_diff p state)
         else Myseq.empty
      | _ -> Myseq.empty)
    t

let parse_vec t p (vi, vj : int * int) state = (* QUICK *)
  parse_template
    ~parse_u:(Myseq.return (`Vec (`Int vi, `Int vj), state))
    ~parse_patt:(function
      | `Vec (i,j) ->
         let* di, state = parse_int i (p ++ `I) vi state in
         let* dj, state = parse_int j (p ++ `J) vj state in
         Myseq.return (`Vec (di,dj), state)
      | _ -> Myseq.empty)
    t

let state_minus_shape_gen state nb_explained_pixels occ_delta occ_mask =
  (* TODO: optimize *)
  Common.prof "Model2.state_minus_shape_gen" (fun () ->
  let new_mask = Grid.Mask.diff state.mask occ_mask in
  if Grid.Mask.equal new_mask state.mask
  then None (* the shape is fully hidden, explains nothing new *)
  else
    let new_state =
      { state with
        delta = occ_delta @ state.delta;
	mask = new_mask;
	parts =
	  List.filter
	    (fun p ->
	      not (Grid.Mask.is_empty
		     (Grid.Mask.inter p.Grid.pixels new_mask)))
	    state.parts } in
    Some new_state)
let state_minus_point state (i,j,c) =
  let nb_explained_pixels = 1 in
  let occ_delta = [] in
  let occ_mask = Grid.Mask.singleton state.grid.height state.grid.width i j in
  state_minus_shape_gen state nb_explained_pixels occ_delta occ_mask
let state_minus_rectangle state (rect : Grid.rectangle) =
  state_minus_shape_gen state rect.nb_explained_pixels rect.delta rect.mask
  
let rec parse_shape =
  let parse_point pos color p (i,j,c) state =
    let* dpos, state = parse_vec pos (p ++ `Pos) (i,j) state in
    let* dcolor, state = parse_color color (p ++ `Color) c state in
    Myseq.return (`Point (dpos,dcolor), state) in
  let parse_rectangle pos size color mask p rect state =
    let open Grid in
    let* dpos, state = parse_vec pos (p ++ `Pos) (rect.offset_i,rect.offset_j) state in
    let* dsize, state = parse_vec size (p ++ `Size) (rect.height,rect.width) state in
    let* dcolor, state = parse_color color (p ++ `Color) rect.color state in
    let* dmask, state = parse_mask mask (p ++ `Mask) rect.rmask state in
    Myseq.return (`Rectangle (dpos,dsize,dcolor,dmask), state)
  in
  let parse_all_points parts state = Myseq.prof "Model2.parse_all_points" (
    let* point = Myseq.from_list (Grid.points state.grid state.mask parts) in
    let* state = Myseq.from_option (state_minus_point state point) in
    Myseq.return (point, state)) in
  let parse_all_rectangles parts state = Myseq.prof "Model2.parse_all_rectangles" (
    let* rect = Myseq.from_list (Grid.rectangles state.grid state.mask parts) in
    let* state = Myseq.from_option (state_minus_rectangle state rect) in
    Myseq.return (rect, state))
  in
  let parse_single_point pos color p parts state = Myseq.prof "Model2.parse_single_point" (
    let* point, state = parse_all_points parts state in
    parse_point pos color p point state) in
  let parse_single_rectangle pos size color mask p parts state = Myseq.prof "Model2.parse_single_rectangle" (
    let* rect, state = parse_all_rectangles parts state in
    parse_rectangle pos size color mask p rect state)
  in
  let parse_repeat_point pos color p parts state = Myseq.prof "Model2.parse_repeat_point" (
    let points_next =
      Grid.points state.grid state.mask parts
      |> Myseq.from_list
      |> Myseq.introspect in
    (*print_endline "POINTS"; (* TEST *)*)
    parse_repeat
      (fun i points_next state ->
        let p_item = ith_item i p in
        let* point, next_points =
          points_next
          |> Myseq.slice ~offset:0 ~limit:(!max_nb_shape_parse) in
        let* state = Myseq.from_option (state_minus_point state point) in
        let* data, state = parse_point pos color p_item point state in
        let next_points : Grid.point Myseq.t =
          next_points (* only points in remaining mask *)
          |> Myseq.filter (fun (i,j,c) -> Grid.Mask.mem i j state.mask) in
        Myseq.return (data, Myseq.introspect next_points, state))
      points_next state) in
  let parse_repeat_rectangle pos size color mask p parts state = Myseq.prof "Model2.parse_repeat_rectangle" (
    let rectangles_next =
      Grid.rectangles state.grid state.mask parts
      |> Myseq.from_list
      |> Myseq.introspect in
    (*print_endline "RECTANGLES"; (* TEST *)*)
    parse_repeat
      (fun i rectangles_next state ->
        let p_item = ith_item i p in
        let* rect, next_rectangles =
          rectangles_next
          |> Myseq.slice ~offset:0 ~limit:(!max_nb_shape_parse) in
        let* state : parse_state = Myseq.from_option (state_minus_rectangle state rect) in
        let* data, state = parse_rectangle pos size color mask p_item rect state in
        let next_rectangles =
          next_rectangles (* only rectangles included in remaining masks *)
          |> Myseq.filter (fun (r : Grid.rectangle) ->
                 not Grid.Mask.(is_empty (inter r.mask state.mask))) in
        Myseq.return (data, Myseq.introspect next_rectangles, state))
      rectangles_next state)
  in
  fun t p (parts : Grid.part list) state ->
  Myseq.prof "Model2.parse_shape"
  (parse_template
    ~parse_u:
    (Myseq.concat
       [parse_all_points parts state
        |> Myseq.map
             (fun ((i,j,c), state) ->
               `Point (`Vec (`Int i, `Int j), `Color c),
               state);
        parse_all_rectangles parts state
        |> Myseq.map
             (fun (r,state) ->
               let open Grid in
               `Rectangle (`Vec (`Int r.offset_i, `Int r.offset_j),
                           `Vec (`Int r.height, `Int r.width),
                           `Color r.color,
                           `Mask r.rmask),
               state) ])
    ~parse_repeat:(
      function
      | `Point (pos,color) ->
         parse_repeat_point pos color p parts state
      | `Rectangle (pos,size,color,mask) ->
         parse_repeat_rectangle pos size color mask p parts state
      | _ -> assert false)
    ~parse_patt:(function
      | `Point (pos,color) ->
         parse_single_point pos color p parts state
      | `Rectangle (pos,size,color,mask) ->
         parse_single_rectangle pos size color mask p parts state
      | `Many (ordered,items) ->
         let* ditems, state =
           parse_many
             (fun i item state ->
               parse_shape (item :> template) (ith_item i p) state.parts state)
             items state in
         Myseq.return (`Many (ordered,ditems),state)
      | _ -> assert false)
    t
  |> Myseq.slice ~offset:0 ~limit:(!max_nb_shape_parse))
  
let parse_grid t p (g : Grid.t) state =
  Myseq.prof "Model2.parse_grid"
  (parse_template
    ~parse_u:(Myseq.empty)
    ~parse_patt:
    (function
     | `Background (size,color,layers) ->
        let* dlayers, state =
          parse_ilist
            (fun shape lp parts state ->
              parse_shape shape (p ++ `Layer lp) parts state)
            layers `Root state.parts state in
        let* dsize, state = parse_vec size (p ++ `Size) (g.height,g.width) state in
        let bc = (* background color *)
	  match Grid.majority_colors state.mask g with
	  | bc::_ -> bc (* TODO: return sequence of colors *)
	  | [] -> Grid.black in
        let* dcolor, state = parse_color color (p ++ `Color) bc state in
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
	Myseq.return (data, new_state)
     | _ -> Myseq.empty)
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
  let| t = apply_template ~env `Root t in (* reducing expressions *)
  let state = { quota_diff;
                diff = diff0;
                delta = delta0;
                mask = Grid.Mask.full g.height g.width;
                parts = Grid.segment_by_color g;
                grid = g } in
  let parses =
    let* data, state = parse_grid t path0 g state in
    let ctx = dl_ctx_of_data data in
    let dl = Common.prof "Model2.read_grid/first_parses/dl" (fun () ->
      let dl_data = dl_data_given_template ~ctx t data in
      let dl_diff = dl_diff ~ctx state.diff data in
      let dl_delta = dl_delta ~ctx state.delta in
      dl_data +. dl_diff +. dl_delta) in
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
  |> List.for_all
       (fun egdls ->
         egdls
         |> List.exists
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
  `Background (`U, `U, `Nil)
let init_model =
  { input_pattern = init_template;
    output_template = init_template }

let pp_model m =
  print_endline "CONSTRUCT (Mo)";
  pp_template m.output_template; print_newline ();
  print_endline "WHERE (Mi)";
  pp_template m.input_pattern; print_newline ()

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

  | `Field (`Pos, _), `Point (pos,color) -> `Point (f (Some pos), color)
  | `Field (`Color, _), `Point (pos,color) -> `Point (pos, f (Some color))

  | `Field (`Pos, _), `Rectangle (pos,size,color,mask) -> `Rectangle (f (Some pos), size, color, mask)
  | `Field (`Size, _), `Rectangle (pos,size,color,mask) -> `Rectangle (pos, f (Some size), color, mask)
  | `Field (`Color, _), `Rectangle (pos,size,color,layers) -> `Rectangle (pos, size, f (Some color), layers)
  | `Field (`Mask, _), `Rectangle (pos,size,color,mask) -> `Rectangle (pos, size, color, f (Some mask))

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
     print_string (string_of_patt (fun _ -> "_") patt_parent);
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
  | RShape of revpath * template (* shape *)
  | RRepeat of revpath
  | RSingle of revpath

let pp_grid_refinement = function
  | RGridInit -> ()
  | RDef (p,t,ctx,partial) ->
     print_string "DEF: "; pp_path p;
     print_string "="; pp_template t;
     if partial then print_string " (partial)"
  | RShape (path,sh) ->
     print_string "SHAPE at ";
     pp_path path;
     print_string ": ";
     pp_template sh
  | RRepeat path ->
     print_string "REPEAT at ";
     pp_path path
  | RSingle path ->
     print_string "SINGLE at ";
     pp_path path

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
      | RShape (path,shape) ->
         t
         |> insert_template
              (function
               | Some x when x = shape -> raise Refinement_no_change
               | _ -> shape)
              path
         |> insert_template (fun _ -> `U) (`Field (`Color,`Root)) (* because background color is defined as remaining color after covering shapes *)
      | RRepeat path ->
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
              path
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
  Myseq.prof "Model2.defs_refinements" (
  assert (grss <> []);
  let u_vars =
    List.rev
      (fold_template
         (fun res for_p p t0 ->
           let k = path_kind p in
           if k <> `Grid
           then (for_p,p,k,t0)::res
           else res)
         [] None path0 t) in
  let val_matrix =
    List.map
      (fun grs ->
        assert (grs <> []); (* otherwise, should be parse failure *)
        List.mapi
          (fun rank (env,gd,dl) ->
            let u_val =
              List.map (fun (for_p,p,k,t0) ->
                  match find_data p gd.data with
                  | Some d -> p, d
                  | None ->
                     pp_path p; print_string ": "; pp_data gd.data; print_newline ();
                     assert false)
                u_vars in
            env, u_val, dl, rank)
          grs)
      grss in
  let tprio = (* priority among definitions according to def body, expressions first *)
    function
    | `U -> 4
    | `Repeat _ -> 3
    | `For _ -> 2
    | #patt -> 1
    | #expr -> 0 in
  let k_le_map =
    defs_expressions ~env_sig in
  let _, defs =
    List.fold_left (* iteration through examples *)
      (fun (first_ex,defs_yes) reads_ex ->
        let _, defs_yes, _ =
          List.fold_left (* iteration through example read *)
            (fun (first_ex,defs_yes,defs_maybe) (env, u_val, dl, rank) ->
              (* defs_yes: defs valid according to previous examples, and previous reads on current example *)
              (* defs_maybe: defs valid according to previous examples, but not in previous reads on current example, undefined on first example *)
              if first_ex (* on first example *)
              then
                let rev_delta_defs_yes =
                  List.fold_left (* WARNING: preserving order on u_vars *)
                    (fun rev_delta_defs_yes (for_p,p,k,t0) ->
                      let d = try List.assoc p u_val with _ -> assert false in
                      let lt : (template * revpath option) list = [] in (* candidate def bodies *)
                      let lt =
                        match t0 with
                        | `U -> List.fold_right
                                  (fun t lt ->
                                    match t, for_p with
                                    | `Many (ordered,items), Some _ ->
                                       (`Indexing (t, `Index), for_p)::lt
                                    | _ -> (t,None)::lt)
                                  (root_template_of_data d) lt
                        | _ -> lt in (* expressions have priority on patts, which have priority on `U *)
                      let lt =
                        match t0 with
                        | #expr -> lt (* not replacing expressions *)
                        | _ -> (try List.assoc k k_le_map
                                with _ -> assert false) @ lt in 
                      List.fold_left (* WARNING: preserving order on lt *)
                        (fun rev_delta_defs_yes (t,ctx) ->
                          if List.exists (* check if def already found *)
                               (fun (_,_,_,p0,_,t0,ctx0) -> p0=p && t0=t && ctx0=ctx)
                               defs_yes
                             || not (defs_check p k t ctx d env)
                          then rev_delta_defs_yes
                          else (dl, rank, tprio t, p, k, t, ctx) :: rev_delta_defs_yes)
                        rev_delta_defs_yes
                        lt
                    )
                    []
                    u_vars
                in
                let defs_yes = List.rev_append rev_delta_defs_yes defs_yes in
                (first_ex,defs_yes,defs_maybe)
              else
                let rev_new_defs_yes, rev_defs_maybe =
                  List.fold_left (* WARNING: preserving orer on defs_yes and defs_maybe *)
                    (fun (rev_new_defs_yes,rev_defs_maybe) (sum_dl,sum_rank,prio,p,k,t,ctx as def) ->
                      let d = try List.assoc p u_val with _ -> assert false in
                      if defs_check p k t ctx d env
                      then
                        let rev_new_defs_yes = (sum_dl +. dl, sum_rank + rank, prio, p, k, t, ctx) :: rev_new_defs_yes in
                        (rev_new_defs_yes, rev_defs_maybe)
                      else
                        let rev_defs_maybe = def :: rev_defs_maybe in (* kept for remaining reads *)
                        (rev_new_defs_yes, rev_defs_maybe)
                    )
                    ([],[])
                    defs_maybe
                in
                let defs_yes = List.rev_append rev_new_defs_yes defs_yes in
                let defs_maybe = List.rev rev_defs_maybe in
                (first_ex,defs_yes,defs_maybe)
            )
            (first_ex,[],defs_yes) (* defs_yes at previous example is used as defs_maybe for checking *)
            reads_ex in
        false, defs_yes (* the remaining defs_maybe is trashed, they're non-valid defs *)
      )
      (true,[])
      val_matrix in
  defs
  |> List.stable_sort (* increasing DL, increasing rank sum *)
       (fun (sum_dl1,sum_rank1,prio1,_,_,_,_) (sum_dl2,sum_rank2,prio2,_,_,_,_) ->
         Stdlib.compare (sum_dl1,sum_rank1,prio1) (sum_dl2,sum_rank2,prio2))
  |> Myseq.from_list
  |> Myseq.map (fun (_,_,_,p,k,t,ctx) -> RDef (p,t,ctx,false)))
and defs_check (p : revpath) (k : kind) (t : template) (ctx : revpath option) (d : data) env : bool =
  Common.prof "Model2.defs_check" (fun () ->
  match t with
  | `U -> assert false (* should not be used as def *)
  | `Repeat _ -> assert false (* should not be used as def *)
  | `For _ -> assert false
  | #patt ->
     ( match d with
       | `Many (_,items) -> List.for_all (matches_template t) items
       | _ -> matches_template t d )
  | #expr ->
     (*     print_string "CHECK expr: "; pp_template t; Option.iter (fun p_many -> print_string " at ctx "; pp_path p_many) ctx; print_newline (); *)
     let e_opt =
       match ctx, d with
       | None, _ -> Some t
       | Some p_many, `Many _ -> Some (`For (p_many, t))
       | _ -> None in
     ( match e_opt with
       | None -> false
       | Some e ->
          match apply_template ~env p (e :> template), (d :> template) with
          | Result.Ok t1, t2 ->
             (match t1, t2 with
              | `Many (ordered1,items1), `Many (ordered2,items2) ->
                 (* TODO: how to handle [ordered] flags *)
                 List.sort Stdlib.compare items1 = List.sort Stdlib.compare items2 (* TODO: avoid sorting here *)
              | `Many _, _ -> false
              | _, `Many _ -> false
              | t1, t2 -> t1 = t2)
          | Result.Error _, _ -> false ) )
and defs_expressions ~env_sig : (kind * (template * revpath option) list) list =
  (* the [path option] is for the repeat context path, to be used in a For loop *)
  Common.prof "Model2.defs_expressions" (fun () ->
  let int_refs = signature_of_kind env_sig `Int in
  let make_ref (p : revpath) =
    match p with
    | `Item (None,local,ctx) -> `Indexing (`Ref p, `Index), Some ctx
    | _ -> `Ref p, None
  in
  List.map
    (fun k ->
      let le = [] in
      let le =
        List.fold_left
          (fun le p -> make_ref p::le) (* order does not matter *)
          le (signature_of_kind env_sig k) in
      let le =
        match k with
        | `Int ->
           List.fold_left
             (fun le p ->
               let e1, p_ctx = make_ref p in
               Common.fold_for
                 (fun i le ->
	           (`Plus (e1, `Int i), p_ctx)
                   :: (`Minus (e1, `Int i), p_ctx)
                   :: le)
                 1 3 le)
             le int_refs
        | _ -> le in
      let le =
        match k with
        | `Int ->
           List.fold_left (fun le p1 ->
               let e1, p1_ctx = make_ref p1 in
               List.fold_left (fun le p2 ->
                   let e2, p2_ctx = make_ref p2 in
                   if p1_ctx = p2_ctx
                   then
                     let le = (`Plus (e1, e2), p1_ctx) :: le in
                     let le =
                       if p1 = p2 then le (* this means 0 *)
	               else (`Minus (e1, e2), p1_ctx) :: le in
	             le
                   else le) (* not same context *)
                 le int_refs)
             le int_refs
        | _ -> le in
      let le = List.rev le in (* reverse to put in order *)
      k, le)
    all_kinds)


let shape_refinements (t : template) : grid_refinement Myseq.t = (* QUICK *)
  (*let aux_repeat p = function
    | `Repeat _ -> Myseq.return (RSingle p)
    | `Point _ | `Rectangle _ -> Myseq.return (RRepeat p)
    | _ -> assert false in *)
  let rec aux lp = function
    | `Nil ->
       let* shape = Myseq.from_list
                      [`Point (`U, `U);
                       `Rectangle (`U, `U, `U, `U)] in
       Myseq.cons (RShape (`Field (`Layer lp, `Root), `Repeat shape))
         (Myseq.cons (RShape (`Field (`Layer lp, `Root), (shape :> template)))
            Myseq.empty)
    | `Insert (above,shape,below) ->
       Myseq.concat
         [ (*aux_repeat [`Layers lp] shape;*) (* TEST *)
           aux (`Right lp) below; (* insert below first *)
           aux (`Left lp) above ]
    | _ -> assert false
  in
  match t with
  | `Background (_,_,layers) -> aux `Root layers
  | _ -> assert false
       
let grid_refinements ~(env_sig : signature) (t : template) (grss : grid_read list list) : (grid_refinement * template) Myseq.t =
  Myseq.prof "Model2.grid_refinements" (
  Myseq.concat
    [defs_refinements ~env_sig t grss;
     shape_refinements t]
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

let pp_refinement = function
  | RInit -> ()
  | Rinput r -> print_string "IN  "; pp_grid_refinement r
  | Routput r -> print_string "OUT "; pp_grid_refinement r

let apply_refinement (r : refinement) (m : model) : (refinement * model) option =
  match r with
  | RInit -> None
  | Rinput gr ->
     apply_grid_refinement gr m.input_pattern
     |> Option.map (fun (_,t) -> r, {m with input_pattern = t})
  | Routput gr ->
     apply_grid_refinement gr m.output_template
     |> Option.map (fun (_,t) -> r, {m with output_template = t})
                 
let model_refinements (last_r : refinement) (m : model) (gsri : grids_read) (gsro : grids_read) : (refinement * model) Myseq.t
  =
  Myseq.prof "Model2.model_refinements" (
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
      shape_refinements m.input_pattern
      |> Myseq.filter_map
	   (fun r -> apply_refinement (Rinput r) m)
    else Myseq.empty in
  let ref_shapos =
    if on_output && grids_read_has_delta gsro
    then
      shape_refinements m.output_template
      |> Myseq.filter_map
	   (fun r -> apply_refinement (Routput r) m)
    else Myseq.empty in
  Myseq.concat
    [ref_shapos; ref_shapis; ref_defos; ref_defis]
      )

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
        (* print_string "\t=> "; pp_refinement r; print_newline (); *)
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
	     Printf.printf "\t\tl = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;
             print_endline " ===> all reads for first example";
             List.hd gpsr.reads
             |> List.iter
                  (fun ((_,{data=d_i},dl_i), (_, {data=d_o}, dl_o), dl) ->
                    print_endline " --- some read ---";
                    pp_data d_i; print_newline ();
                    pp_data d_o; print_newline ();
                    Printf.printf "\tdl=%.1f\n" dl);
             print_newline ()
 *)
           );
	   flush stdout;
           lmd)
    ~refinements:
    (fun (r,m) (gpsr,gsri,gsro) dl ->
      Printf.printf "%.3f\t" dl; pp_refinement r; print_newline ();
      if grid_viz then (
        List.iter2
          (fun reads_input reads_pair ->
            match reads_input, reads_pair with
            | (_,gdi,_)::_, ((_,gdi_knowing_o,_), (_,gdo,_), _)::_ ->
              Grid.pp_grids
                [grid_of_data gdi_knowing_o.data;
                 grid_of_data gdo.data;
                 grid_of_data gdi.data;
                 Result.get_ok (write_grid ~env:gdi.data m.output_template)];
              print_newline ()
           | _ -> assert false)
          gpsr.input_reads gpsr.reads;
        Unix.sleepf 0.5);
        (*pp_grids_read "### OUT grids_read ###" gsro;*)
      (*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
      flush stdout;
      model_refinements r m gsri gsro))


