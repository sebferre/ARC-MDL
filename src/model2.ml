
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
                   
(** Part 1: grids *)

(* common data structures *)

type 'a triple = 'a * 'a * 'a

type 'a ilist = (* insertable list *)
  [ `Nil
  | `Insert of 'a ilist * 'a * 'a ilist ]
type ilist_field = [`Left | `Right]
type ilist_path = ilist_field list

let (++|) p f = p @ [f]
                
let rec ilist_length : 'a ilist -> int = function
  | `Nil -> 0
  | `Insert (left,elt,right) ->
     ilist_length left + 1 + ilist_length right

let rec map_ilist (f : ilist_path -> 'a -> 'b) (lp : ilist_path) (l : 'a ilist) : 'b ilist =
  match l with
  | `Nil -> `Nil
  | `Insert (left,elt,right) ->
     let left = map_ilist f (lp ++| `Left) left in
     let elt = f lp elt in
     let right = map_ilist f (lp ++| `Right) right in
     `Insert (left,elt,right)

(* type definitions for data, expressions, templates *)
    
type kind =
  [ `Int | `Bool | `Color | `Mask | `Vec | `Shape | `Layer | `Grid ]
let all_kinds =
  [ `Int;  `Bool;  `Color;  `Mask;  `Vec;  `Shape;  `Layer;  `Grid ]
  
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

type field = [ `I | `J | `Pos | `Color | `Size | `Mask | `Layers of ilist_path | `Item of int option (* pos *) * path ]
and path = field list
let path0 = []

let any_item = `Item (None,[])
let ith_item i = `Item (Some i, [])

let rec (++) p f =
  match List.rev p with
  | `Item (i_opt,q)::rev_p1 -> List.rev (`Item (i_opt, q ++ f)::rev_p1)
  | _ -> p @ [f]

let rec path_rev_tail p = (* to access context of path, reading the path from the tail *)
  let rev_p = List.rev p in
  match rev_p with
  | `Item (_,[])::_ -> rev_p
  | `Item (_,q)::_ -> path_rev_tail q @ rev_p (* keep rev_p to avoid confusion between grid color and shape color, for instance *)
  | _ -> rev_p
       
let path_split_any_item (path : path) : (path * path) option (* ctx, local *) =
  match List.rev path with
  | `Item (None,q)::rev_p -> Some (List.rev rev_p, q) (* TODO: optimize to avoid double List.rev *)
  | _ -> None
       
let path_ctx (path : path) : path option =
  match List.rev path with
  | `Item (_,q)::rev_p -> Some (List.rev rev_p) (* TODO: optimize to avoid double List.rev *)
  | _ -> None
       
type data = data patt
let data0 = `Background (`Vec (`Int 0, `Int 0), `Color Grid.black, `Nil)

type 'a expr =
  [ `Var of path
  | `Plus of 'a * 'a
  | `Minus of 'a * 'a
  | `If of 'a * 'a * 'a
  | `For of path * 'a (* path is a many-valued var *)
  ]
         
type template =
  [ `U
  | `Repeat of template patt
  | template patt
  | template expr ]

type signature = (kind * path list) list (* map from kinds to path lists *)
let signature0 = []
  
type diff = path list (* paths to data parts differing from a template *)
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

let string_of_ilist_field : ilist_field -> string = function
  | `Left -> "0"
  | `Right -> "1"
  
let string_of_ilist_path : ilist_path -> string =
  fun lp -> "0" ^ String.concat "" (List.map string_of_ilist_field lp)

let string_of_kind : kind -> string = function
  | `Bool -> "bool"
  | `Int -> "int"
  | `Color -> "color"
  | `Mask -> "mask"
  | `Vec -> "vector"
  | `Shape -> "shape"
  | `Layer -> "layer"
  | `Grid -> "grid"
          
let rec string_of_field : field -> string = function
  | `I -> "i"
  | `J -> "j"
  | `Pos -> "pos"
  | `Color -> "color"
  | `Size -> "size"
  | `Mask -> "mask"
  | `Layers lp -> "layer[" ^ string_of_ilist_path lp ^ "]"
  | `Item (None,p) -> "*." ^ string_of_path p
  | `Item (Some i,p) -> "[" ^ string_of_int i ^ "]." ^ string_of_path p

and string_of_path : path -> string =
  fun p -> String.concat "." (List.map string_of_field p)

let pp_path p = print_string (string_of_path p)

let rec string_of_ilist (string : 'a -> string) (l : 'a ilist) : string =
  let rec aux lp = function
    | `Nil -> ""
    | `Insert (left,elt,right) ->
       aux (lp ++| `Left) left
       ^ "\n  [" ^ string_of_ilist_path lp ^ "]: " ^ string elt
       ^ aux (lp ++| `Right) right
  in
  aux [] l
      
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

let rec string_of_expr (string : 'a -> string) : 'a expr -> string = function
  | `Var p -> "{" ^string_of_path p ^ "}"
  | `Plus (a,b) ->
     string a ^ " + " ^ string b
  | `Minus (a,b) ->
     string a ^ " - " ^ string b
  | `If (cond,e1,e2) ->
     "if " ^ string cond
     ^ " then " ^ string e1
     ^ " else " ^ string e2
  | `For (p,e1) ->
     "for {" ^ string_of_path p ^ "}: " ^ string e1

(*let pp_expr e = print_string (string_of_expr e)*)

let rec string_of_template : template -> string = function
  | `U -> "?"
  | `Repeat patt -> "repeat " ^ string_of_patt string_of_template patt
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

let rec find_ilist (lp : ilist_path) (l : 'a ilist) : 'a option =
  match l with
  | `Nil -> None
  | `Insert (left,elt,right) ->
     match lp with
     | [] -> Some elt
     | `Left::lp1 -> find_ilist lp1 left
     | `Right::lp1 -> find_ilist lp1 right
  
let find_patt (find : path -> 'a -> 'a option)
      (p : path) (patt : 'a patt) : 'a option =
  match p, patt with
  | `I::p, `Vec (i,j) -> find p i
  | `J::p ,`Vec (i,j) -> find p j
  | `Pos::p, `Point (pos,color) -> find p pos
  | `Color::p, `Point (pos,color) -> find p color
  | `Pos::p, `Rectangle (pos,size,color,mask) -> find p pos
  | `Size::p, `Rectangle (pos,size,color,mask) -> find p size
  | `Color::p, `Rectangle (pos,size,color,mask) -> find p color
  | `Mask::p, `Rectangle (pos,size,color,mask) -> find p mask
  | `Size::p, `Background (size,color,layers) -> find p size
  | `Color::p, `Background (size,color,layers) -> find p color
  | `Layers lp::p, `Background (size,color,layers) ->
     Option.bind
       (find_ilist lp layers)
       (fun x -> find p x)
  | `Item (None,q)::[], `Many (ordered,items) -> (* Any must be last *)
     let l_opt =
       List.fold_right
         (fun item l_opt ->
           match l_opt with
           | None -> None 
           | Some l ->
              match find q item with
              | None -> None
              | Some d1 -> Some (d1::l))
         items (Some []) in
     ( match l_opt with
       | None -> None
       | Some l ->
          let l =
            if ordered (* list vs set *)
            then l
            else List.sort Stdlib.compare l in
          Some (`Many (ordered,l)) )
  | `Item (Some i,q)::[], `Many (_,items) ->
     ( match List.nth_opt items i with
       | None -> None
       | Some item -> find q item )
  | _ -> None

let rec find_data (p : path) (d : data) : data option = (* QUICK *)
  match p with
  | [] -> Some d
  | _ -> find_patt find_data p d

let rec find_template (p : path) (t : template) : template option =
  Common.prof "Model2.find_template" (fun () ->
  match p, t with
  | [], _ -> Some t
  | `Item (_,q)::[], `Repeat patt1 -> find_patt find_template q patt1
  | _, (#patt as patt) -> find_patt find_template p patt
  | _ -> None)

let rec fold_ilist (f : 'b -> ilist_path -> 'a -> 'b) (acc : 'b) (lp : ilist_path) (l : 'a ilist) : 'b =
  match l with
  | `Nil -> acc
  | `Insert (left,elt,right) ->
     let acc = fold_ilist f acc (lp ++| `Left) left in
     let acc = f acc lp elt in
     let acc = fold_ilist f acc (lp ++| `Right) right in
     acc
       
let rec fold2_ilist (f : 'c -> ilist_path -> 'a -> 'b -> 'c) (acc : 'c) (lp : ilist_path) (l1 : 'a ilist) (l2 : 'b ilist) : 'c =
  match l1, l2 with
  | `Nil, `Nil -> acc
  | `Insert (left1,elt1,right1), `Insert (left2,elt2,right2) ->
     let acc = fold2_ilist f acc (lp ++| `Left) left1 left2 in
     let acc = f acc lp elt1 elt2 in
     let acc = fold2_ilist f acc (lp ++| `Right) right1 right2 in
     acc
  | _ -> invalid_arg "Model2.fold2_ilist: inconsistent ilists"
       
let fold_patt (fold : 'b -> path -> 'a -> 'b) (acc : 'b) (p : path) (patt : 'a patt) : 'b =
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
           fold acc (p ++ `Layers lp) shape)
         acc [] layers in
     acc
  | `Many (ordered,items) ->
     let _, acc =
       List.fold_left
         (fun (i,acc) item -> i+1, fold acc (p ++ ith_item i) item)
         (0,acc) items in
     acc

let rec fold_data (f : 'b -> path -> data -> 'b) (acc : 'b) (p : path) (d : data) : 'b =
  let acc = f acc p d in
  fold_patt (fold_data f) acc p d

let fold_expr (fold : 'b -> path -> 'a -> 'b) (acc : 'b) (p : path) (e : 'a expr) : 'b =
  match e with
  | `Var _ -> acc
  | `Plus _ | `Minus _ | `If _ -> acc (* not folding through arithmetics and conditional *)
  | `For (p_many,t1) ->
     let acc = fold acc (p ++ any_item) t1 in
     acc
  
let rec fold_template (f : 'b -> path -> template -> 'b) (acc : 'b) (p : path) (t : template) : 'b =
  let acc = f acc p t in
  match t with
  | `U -> acc
  | `Repeat patt1 -> fold_patt (fold_template f) acc (p ++ any_item) patt1
  | #patt as patt -> fold_patt (fold_template f) acc p patt
  | #expr as e -> fold_expr (fold_template f) acc p e
       
let size_of_data (d : data) : int =
  fold_data (fun res _ _ -> res+1) 0 path0 d
let size_of_template (t : template) : int =
  fold_template (fun res _ _ -> res+1) 0 path0 t

let rec path_kind (p : path) : kind =
  match List.rev p with
  | (`I | `J)::_ -> `Int
  | `Color::_ -> `Color
  | `Mask::_ -> `Mask
  | (`Pos | `Size)::_ -> `Vec
  | `Layers _::_ -> `Layer
  | `Item (_,[])::_ -> `Shape
  | `Item (_,q)::_ -> path_kind q
  | [] -> `Grid

let signature_of_template (t : template) : signature =
  let ht = Hashtbl.create 13 in
  let () =
    fold_template
      (fun () p t1 ->
        let k = path_kind p in
        let ps0 =
          match Hashtbl.find_opt ht k with
          | None -> []
          | Some ps -> ps in
        Hashtbl.replace ht k (p::ps0);
        if k = `Vec && t1 = `U then (
          let ps0 =
            match Hashtbl.find_opt ht `Int with
            | None -> []
            | Some ps -> ps in
          Hashtbl.replace ht `Int (p ++ `I :: p ++ `J :: ps0)
        ))
      () path0 t in
  Hashtbl.fold
    (fun k ps res -> (k, List.rev ps)::res) (* reverse to put them in order *)
    ht []

let signature_of_kind (sg : signature) (k : kind) : path list =
  match List.assoc_opt k sg with
  | Some ps -> ps
  | None -> []
  
let rec default_data_of_path (p : path) : data =
  match List.rev p with
  | (`I | `J)::`Pos::_ -> `Int 0
  | (`I | `J)::`Size::[] -> `Int 10
  | (`I | `J)::`Size::_ -> `Int 2
  | `Color::[] -> `Color Grid.black
  | `Color::`Layers _::_ -> `Color Grid.no_color
  | `Mask::_ -> `Mask None
  | `Pos::_ -> `Vec (`Int 0, `Int 0)
  | `Size::[] -> `Vec (`Int 10, `Int 10)
  | `Size::_ -> `Vec (`Int 2, `Int 2)
  | `Layers _::_ -> `Rectangle (`Vec (`Int 0, `Int 0), `Vec (`Int 2, `Int 2), `Color Grid.no_color, `Mask None)
  | `Item (_,q)::_ -> default_data_of_path q
  | [] -> `Background (`Vec (`Int 10, `Int 10), `Color Grid.black, `Nil)
  | _ -> assert false

(*let unify (ld : data list) : template (* without expression *) = (* QUICK *)
  let rec aux t d =
    match t, d with
    | `U, _ -> t

    | `Bool b1, `Bool b2 when b1=b2 -> t
    | `Int i1, `Int i2 when i1=i2 -> t
    | `Color c1, `Color c2 when c1=c2 -> t
    | `Mask m1, `Mask m2 when m1=m2 -> t
                                     
    | `Vec (i1,j1), `Vec (i2,j2) -> `Vec (`U, `U)
      
    | `Point _, `Point _ ->
       `Point (`U, `U)
    | (`Point _ | `Rectangle _), (`Point _ | `Rectangle _) ->
       `Rectangle (`U, `U, `U, `U)
      
    | _ -> `U
  in
  match ld with
  | [] -> assert false
  | d0::ld1 -> List.fold_left aux (d0 :> template) ld1 *)

let rec root_template_of_data (d : data) : template list =
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
     List.sort_uniq Stdlib.compare
       (List.concat (List.map root_template_of_data items))

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
  { env_sig : signature;
    box_height : int;
    box_width : int }
let dl_ctx0 =
  { env_sig = signature0;
    box_height = Grid.max_size;
    box_width = Grid.max_size }
  
let dl_ctx_of_data (d : data) : dl_ctx =
  (* retrieving grid size: make assumption on paths *)
  let box_height =
    match find_data [`Size; `I] d with
    | Some (`Int i) -> i
    | _ -> Grid.max_size in
  let box_width =
    match find_data [`Size; `J] d with
    | Some (`Int j) -> j
    | _ -> Grid.max_size in
  { env_sig = signature0; box_height; box_width }
      

let dl_patt_as_template = Mdl.Code.usage 0.4


let rec dl_patt
      (dl : ctx:dl_ctx -> ?path:path -> 'a -> dl)
      ~(ctx : dl_ctx) ~(path : path) (patt : 'a patt) : dl =
  match path_kind path, patt with
  | `Int, `Int n ->
     ( match path_rev_tail path with
       | `I::`Pos::_ -> dl_index ~bound:ctx.box_height n
       | `J::`Pos::_ -> dl_index ~bound:ctx.box_width n
       | (`I | `J)::`Size::_ -> dl_length n
       | _ -> assert false )

  | `Color, `Color c ->
     ( match path_rev_tail path with
       | `Color::[] -> dl_background_color c
       | _ -> dl_color c )

  | `Mask, `Mask m -> dl_mask m

  | `Vec, `Vec (i,j) -> dl_patt_vec dl ~ctx ~path i j

  | `Layer, `Many (ordered,items) ->
     Mdl.Code.universal_int_plus (List.length items)
     +. 0. (* TODO: encode ordered when length > 1 *)
     +. Mdl.sum items
          (fun item -> dl ~ctx item ~path:(path ++ any_item)) (* exact item index does not matter here *)
  | `Layer, _ -> (* single shape instead of Many *)
     Mdl.Code.universal_int_plus 1 (* singleton *)
     +. dl_patt dl ~ctx patt ~path:(path ++ any_item)

  | `Shape, `Point (pos,color) ->
     Mdl.Code.usage 0.5 +. dl_patt_point dl ~ctx ~path pos color
  | `Shape, `Rectangle (pos,size,color,mask) ->
     Mdl.Code.usage 0.5 +. dl_patt_rectangle dl ~ctx ~path pos size color mask

  | `Grid, `Background (size,color,layers) ->
     let box_height =
       match size with
       | `Vec (`Int i, _) -> i
       | _ -> ctx.box_height in
     let box_width =
       match size with
       | `Vec (_, `Int j) -> j
       | _ -> ctx.box_width in
     let nb_layers = ilist_length layers in
     let ctx_layers = { ctx with box_height; box_width } in
     dl ~ctx size ~path:(path ++ `Size)
     +. dl ~ctx color ~path:(path ++ `Color)
     +. fold_ilist
          (fun sum lp shape ->
            sum +. dl ~ctx:ctx_layers shape ~path:(path ++ `Layers lp))
          (Mdl.Code.universal_int_star nb_layers)
          [] layers
     
  | _ ->
     pp_path path; print_string ": ";
     print_string (string_of_patt (fun _ -> "_") patt);
     print_newline ();
     assert false
and dl_patt_vec dl ~ctx ~path i j =
  dl ~ctx i ~path:(path ++ `I)
  +. dl ~ctx j ~path:(path ++ `J)  
and dl_patt_point dl ~ctx ~path pos color =
  dl ~ctx pos ~path:(path ++ `Pos)
  +. dl ~ctx color ~path:(path ++ `Color)
and dl_patt_rectangle dl ~ctx ~path pos size color mask =
  dl ~ctx pos ~path:(path ++ `Pos)
  +. dl ~ctx size ~path:(path ++ `Size)
  +. dl ~ctx color ~path:(path ++ `Color)
  +. dl ~ctx mask ~path:(path ++ `Mask)
                        

let rec dl_data ~(ctx : dl_ctx) ?(path = []) (d : data) : dl =
  dl_patt_as_template (* NOTE: to align with dl_template on patterns *)
  +. dl_patt dl_data ~ctx ~path d

let path_similarity ~ctx_path v =
  let rec aux rev_p1' rev_p2' =
    match rev_p1', rev_p2' with
    | `I::rev_p1, `I::rev_p2 -> aux rev_p1 rev_p2
    | `I::rev_p1, `J::rev_p2 -> 0.5 *. aux rev_p1 rev_p2
    | `J::rev_p1, `I::rev_p2 -> 0.5 *. aux rev_p1 rev_p2
    | `J::rev_p1, `J::rev_p2 -> aux rev_p1 rev_p2

    | `Color::rev_p1, `Color::rev_p2 -> aux rev_p1 rev_p2

    | `Mask::rev_p1, `Mask::rev_p2 -> aux rev_p1 rev_p2

    | `Pos::rev_p1, `Pos::rev_p2 -> aux rev_p1 rev_p2
    | `Pos::rev_p1, `Size::rev_p2 -> 0.5 *. aux rev_p1 rev_p2
    | `Size::rev_p1, `Size::rev_p2 -> aux rev_p1 rev_p2
    | `Size::rev_p1, `Pos::rev_p2 -> 0.5 *. aux rev_p1 rev_p2

    | [], `Layers _::rev_p2 -> 0.5 *. aux [] rev_p2
    | `Layers lp1::rev_p1, `Layers lp2::rev_p2 -> aux_ilist lp1 lp2 *. aux rev_p1 rev_p2
    | [], [] -> 2.
    | `Layers _::rev_p1, [] -> 0.75 *. aux rev_p1 []

    | `Item (_,q1)::rev_p1, `Item (_,q2)::rev_p2 -> aux (List.rev q1) (List.rev q2) *. aux rev_p1 rev_p2
    | `Item (_,q1)::rev_p1, rev_p2 -> 0.75 *. aux (List.rev_append q1 rev_p1) rev_p2
    | rev_p1, `Item (_,q2)::rev_p2 -> 0.5 *. aux rev_p1 (List.rev_append q2 rev_p2)

    | _ -> assert false (* uncompatible kinds *)
  and aux_ilist lp1 lp2 =
    if lp1 = lp2 then 1. else 0.8
  in
  aux (List.rev ctx_path) (List.rev v)
  
let dl_var ~(ctx_path : path) (vars : path list) (x : path) : dl =
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
  else Mdl.Code.usage (x_w /. total_w)

let dl_path ~(env_sig : signature) ~(ctx_path : path) (x : path) : dl =
  let k = path_kind x in
  match List.assoc_opt k env_sig with
  | Some vars -> dl_var ~ctx_path vars x
  | None -> Stdlib.infinity (* invalid model, TODO: happens when unify generalizes some path, removing sub-paths *)
  
let rec dl_expr
          (dl : ctx:dl_ctx -> ?path:path -> 'a -> dl)
          ~(ctx : dl_ctx) ~(path : path) (e : 'a expr) : dl =
  let k = path_kind path in
  let usage_expr = (* 0. when no expression on kind *)
    match k with
    | `Int -> 0.5
    | `Color -> 0.
    | `Bool -> 0.
    | `Mask -> 0.
    | `Vec -> 0.
    | `Shape -> 0.
    | `Layer -> 0.5
    | `Grid -> 0.
  in
  match e with
  | `Var x ->
     Mdl.Code.usage (1. -. usage_expr)
     +. dl_path ~env_sig:ctx.env_sig ~ctx_path:path x
  | _ ->
     Mdl.Code.usage usage_expr
     +. (match path_kind path, e with
         | `Int, (`Plus (e1,e2) | `Minus (e1,e2)) ->
            let rec make_p2 p = (* 2nd operand prefers a size to a position *)
              match List.rev p with
              | `Item (i_opt,q)::rev_p1 ->
                 let q2 = make_p2 q in
                 List.rev (`Item (i_opt,q2)::rev_p1)
              | _ -> p in
            let p2 = make_p2 path in
            Mdl.Code.usage 0.5 (* choice between Plus and Minus *)
            +. dl ~ctx e1 ~path
            +. dl ~ctx e2 ~path:p2
            
         | `Layer, `For (p_many,e1) ->
            dl_path ~env_sig:ctx.env_sig ~ctx_path:path p_many
            +. dl ~ctx e1 ~path:(path ++ any_item)
         
         | _ -> assert false)

let rec dl_template ~(ctx : dl_ctx) ?(path = []) (t : template) : dl =
  let k = path_kind path in
  let usage_repeat =
    match k with
    | `Int -> 0.
    | `Bool -> 0.
    | `Color -> 0.
    | `Mask -> 0.
    | `Vec -> 0.
    | `Shape -> 0.
    | `Layer -> 0.1
    | `Grid -> 0.
  in
  match t with (* TODO: take path/kind into account, not all combinations are possible *)
  | `U ->
     Mdl.Code.usage 0.1
  | `Repeat patt1 ->
     Mdl.Code.usage usage_repeat
     +. dl_patt dl_template ~ctx ~path:(path ++ any_item) patt1
  | #patt as patt ->
     dl_patt_as_template (* Mdl.Code.usage 0.4 *)
     +. dl_patt dl_template ~ctx ~path patt
  | #expr as e ->
     Mdl.Code.usage (0.5 -. usage_repeat)
     +. dl_expr dl_template ~ctx ~path e

    
let dl_data_given_patt
      (dl : ctx:dl_ctx -> ?path:path -> 'a -> data -> dl)
      ~ctx ~(path : path) (patt : 'a patt) (d : data) : dl =
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
          (fun sum lp shape dshape -> sum +. dl ~ctx shape dshape ~path:(path ++ `Layers lp))
          0. [] layers dlayers
  | `Many (ordered,items), `Many (dordered,ditems) ->
     assert (ordered = dordered);
     assert (List.length items = List.length ditems);
     List.fold_left2
       (fun res item ditem -> res +. dl ~ctx ~path:(path ++ any_item) item ditem)
       0. items ditems
  | _ -> assert false (* data inconsistent with pattern *)
    
let rec dl_data_given_template ~(ctx : dl_ctx) ?(path = []) (t : template) (d : data) : dl =
  match t, d with
  | `U, _ -> dl_data ~ctx ~path d
  | `Repeat patt1, `Many (false,items) ->
     Mdl.Code.universal_int_plus (List.length items)
     +. Mdl.sum items (fun item ->
            dl_data_given_patt dl_data_given_template ~ctx ~path:(path ++ any_item) patt1 item)
  | `Repeat _, _ -> assert false (* only parses into unordered collections *)
  | #patt as patt, _ -> dl_data_given_patt dl_data_given_template ~ctx ~path patt d
  | #expr, _ -> assert false (* should have been evaluated out *)

           
let dl_diff ~(ctx : dl_ctx) (diff : diff) (data : data) : dl =
  let data_size = size_of_data data in
  Mdl.Code.universal_int_star (List.length diff)
  -. 1. (* some normalization to get 0 for empty grid data *)
  +. Mdl.sum diff
       (fun p1 ->
         let d1 =
           match find_data p1 data with
           | Some d1 -> d1
           | None -> assert false in
         Mdl.Code.uniform data_size
         +. dl_data ~ctx d1 ~path:p1)
    
let dl_delta ~(ctx : dl_ctx) (delta : delta) : dl =
  let nb_pixels = List.length delta in
  Mdl.Code.universal_int_star nb_pixels (* number of delta pixels *)
  -. 1. (* some normalization to get 0 for empty grid data *)
  +. Mdl.sum delta
       (fun (i,j,c) ->
         dl_data ~ctx ~path:[`Layers []; any_item] (* dummy path with kind Shape *)
           (`Point (`Vec (`Int i, `Int j), `Color c)))

(* NOT using optimized DL below for fair comparisons with model points: 
  +. Mdl.Code.comb nb_pixels area (* where they are *)
  +. float nb_pixels *. Mdl.Code.uniform (Grid.nb_color - 1) (* what are their color, different from the color generated by the model *) *)
    
(* evaluation of expression and templates on environment data *)

exception Unbound_var of path
exception Invalid_expr of template expr
let _ =
  Printexc.register_printer
    (function
     | Unbound_var p -> Some ("unbound variable: " ^ string_of_path p)
     | Invalid_expr e -> Some ("invalid expression: " ^ string_of_expr string_of_template e)
     | _ -> None)

type apply_lookup = path -> data option
    
let apply_patt
      (apply : lookup:apply_lookup -> path -> 'a -> 'b)
      ~(lookup : apply_lookup) (p : path) : 'a patt -> 'b patt = function
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
                    (fun lp shape -> apply ~lookup (p ++ `Layers lp) shape)
                    [] layers)
  | `Many (ordered,items) ->
     `Many (ordered,
            List.map
              (fun item -> apply ~lookup (p ++ any_item) item)
              items)

let apply_expr_gen
          (apply : lookup:apply_lookup -> path -> 'a -> template)
          ~(lookup : apply_lookup) (p : path) (e : 'a expr) : template = (* QUICK *)
  match e with
  | `Var v ->
     (match lookup v with
      | Some d -> (d :> template)
      | None -> raise (Unbound_var v))
  | `Plus (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Int i1, `Int i2 -> `Int (i1 + i2)
      | _ -> raise (Invalid_expr e))
  | `Minus (e1,e2) ->
     (match apply ~lookup p e1, apply ~lookup p e2 with
      | `Int i1, `Int i2 -> `Int (i1 - i2)
      | _ -> raise (Invalid_expr e))
  | `If (e0,e1,e2) ->
     (match apply ~lookup [] e0 with
      | `Bool b ->
         if b
         then apply ~lookup p e1
         else apply ~lookup p e2
      | _ -> raise (Invalid_expr e))
  | `For (p_many,e1) ->
     (match lookup p_many with
      | Some (`Many (ordered,items)) ->
         let lookup_item i =
           fun p ->
           match path_split_any_item p with
           | Some (ctx,local) when ctx = p_many ->
              let p_item = List.rev (`Item (Some i, local)::List.rev ctx) in (* pointing at the i-th item *)
              lookup p_item
           | _ -> lookup p in
         let items_e1 =
           List.mapi
             (fun i _item -> apply ~lookup:(lookup_item i) (p ++ any_item) e1) (* TODO: use directly item in lookup functions *)
             items in
         `Many (ordered, items_e1) (* TODO: how to decide on ordered? add to `For construct? *)
      | _ -> raise (Unbound_var p_many))

let apply_expr apply ~(env : data) p e =
  apply_expr_gen apply ~lookup:(fun p -> find_data p env) p e
  
let rec apply_template_gen ~(lookup : apply_lookup) (p : path) (t : template) : template =
  Common.prof "Model2.apply_template_gen" (fun () ->
  match t with
  | `U -> `U
  | `Repeat patt1 -> `Repeat (apply_patt apply_template_gen ~lookup (p ++ any_item) patt1)
  | #patt as patt -> (apply_patt apply_template_gen ~lookup p patt :> template)
  | #expr as e -> apply_expr_gen apply_template_gen ~lookup p e)

let rec apply_template ~(env : data) (p : path) (t : template) : template =
  apply_template_gen ~lookup:(fun p -> find_data p env) p t
(* TODO: remove path argument, seems useless *)


(* grid generation from data and template *)

let rec generate_template (p : path) (t : template) : data =
  match t with
  | `U -> default_data_of_path p (* default data *)
  | `Repeat patt1 -> apply_patt
                       (fun ~lookup -> generate_template) ~lookup:(fun _ -> assert false)
                       (p ++ any_item) patt1
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
  try
    let t' = apply_template ~env [] t in
    let d = generate_template [] t' in
    let g = grid_of_data d in
    List.iter
      (fun (i,j,c) -> Grid.set_pixel g i j c)
      delta;
    Result.Ok g
  with exn -> Result.Error exn)       

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
          (parse_elt : 'a -> ilist_path -> 'b -> parse_state -> (data * parse_state) Myseq.t)
          (l : 'a ilist) (lp : ilist_path) (x : 'b) (state : parse_state)
      : (data ilist * parse_state) Myseq.t =
  match l with
  | `Nil ->
     Myseq.return (`Nil, state)
  | `Insert (left,elt,right) ->
     let* dleft, state = parse_ilist parse_elt left (lp ++| `Left) x state in
     let* delt, state = parse_elt elt lp x state in
     let* dright, state = parse_ilist parse_elt right (lp ++| `Right) x state in
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
      let* item, parts, state = seq_items in
      let* items1, state = aux (i+1) parts state in
      Myseq.return (item::items1, state)
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
  | #patt as patt -> parse_patt patt
  | #expr -> assert false

let parse_bool t p (b : bool) state =
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

let parse_int t p (i : int) state =
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

let parse_color t p (c : Grid.color) state =
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
  
let parse_mask t p (m : Grid.Mask.t option) state =
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

let parse_vec t p (vi, vj : int * int) state =
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
    Some new_state
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
  let parse_all_points parts state =
    let* point = Myseq.from_list (Grid.points state.grid state.mask parts) in
    let* state = Myseq.from_option (state_minus_point state point) in
    Myseq.return (point, state) in
  let parse_all_rectangles parts state =
    let* rect = Myseq.from_list (Grid.rectangles state.grid state.mask parts) in
    let* state = Myseq.from_option (state_minus_rectangle state rect) in
    Myseq.return (rect, state)
  in
  let parse_single_point pos color p parts state =
    let* point, state = parse_all_points parts state in
    parse_point pos color p point state in
  let parse_single_rectangle pos size color mask p parts state =
    let* rect, state = parse_all_rectangles parts state in
    parse_rectangle pos size color mask p rect state
  in
  let parse_repeat_point pos color p parts state =
    let points_next =
      Grid.points state.grid state.mask parts
      |> Myseq.from_list
      |> Myseq.introspect in
    (*print_endline "POINTS"; (* TEST *)*)
    parse_repeat
      (fun i points_next state ->
        if i >= !max_nb_shape_parse
        then Myseq.empty
        else
          let p_item = p ++ ith_item i in
          let* point, next_points = points_next in
          let* state = Myseq.from_option (state_minus_point state point) in
          let* data, state = parse_point pos color p_item point state in
          Myseq.return (data, Myseq.introspect next_points, state))
      points_next state in
  let parse_repeat_rectangle pos size color mask p parts state =
    let rectangles =
      Grid.rectangles state.grid state.mask parts
      |> Myseq.from_list
      |> Myseq.introspect in
    (*print_endline "RECTANGLES"; (* TEST *)*)
    parse_repeat
      (fun i rectangles state ->
        if i >= !max_nb_shape_parse
        then Myseq.empty
        else
          let p_item = p ++ ith_item i in
          let* rect, next_rectangles = rectangles in
          let* state = Myseq.from_option (state_minus_rectangle state rect) in
          let* data, state = parse_rectangle pos size color mask p_item rect state in
          Myseq.return (data, Myseq.introspect next_rectangles, state))
      rectangles state
  in
  fun t p (parts : Grid.part list) state ->
  Common.prof "Model2.parse_shape" (fun () ->
  parse_template
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
               parse_shape (item :> template) (p ++ ith_item i) state.parts state)
             items state in
         Myseq.return (`Many (ordered,ditems),state)
      | _ -> assert false)
    t
  |> Myseq.slice ~offset:0 ~limit:(!max_nb_shape_parse))
  
let parse_grid t p (g : Grid.t) state =
  Common.prof "Model2.parse_grid" (fun () ->
  parse_template
    ~parse_u:(Myseq.empty)
    ~parse_patt:
    (function
     | `Background (size,color,layers) ->
        let* dlayers, state =
          parse_ilist
            (fun shape lp parts state ->
              parse_shape shape (p ++ `Layers lp) parts state)
            layers [] state.parts state in
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
  let t = apply_template ~env [] t in (* reducing expression *)
  let state = { quota_diff;
                diff = diff0;
                delta = delta0;
                mask = Grid.Mask.full g.height g.width;
                parts = Grid.segment_by_color g;
                grid = g } in
  let parses =
    let* data, state = parse_grid t path0 g state in
    let ctx = dl_ctx_of_data data in
    let dl_data = dl_data_given_template ~ctx t data in
    let dl_diff = dl_diff ~ctx state.diff data in
    let dl_delta = dl_delta ~ctx state.delta in
    let dl = dl_data +. dl_diff +. dl_delta in
    let gd = {data; diff=state.diff; delta=state.delta} in
    Myseq.return (env, gd, dl) in
  let l_parses =
    parses
    |> Myseq.slice ~offset:0 ~limit:(!max_nb_parse)
    |> Myseq.to_list in
  if l_parses = []
  then Result.Error Parse_failure
  else Result.Ok
         (l_parses
          |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> Stdlib.compare dl1 dl2)
          |> (fun l -> Common.sub_list l 0 !max_nb_grid_reads)
          |> limit_dl (fun (_,_,dl) -> dl)))

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
      ~ctx:{env_sig; box_height=Grid.max_size; box_width=Grid.max_size}
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
      ~ctx:{env_sig=signature0; box_height=Grid.max_size; box_width=Grid.max_size}
      m.input_pattern in    
  let env_sig =
    signature_of_template m.input_pattern in
  let dl_mo =
    dl_template
      ~ctx:{env_sig; box_height=Grid.max_size; box_width=Grid.max_size}
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

let rec insert_ilist (lp : ilist_path) (f : 'a option -> 'a) (l : 'a ilist) : 'a ilist =
  match lp, l with
  | [], `Nil -> `Insert (`Nil, f None, `Nil)
  | [], `Insert (left, elt, right) -> `Insert (left, f (Some elt), right)
  | `Left::lp1, `Insert (left, elt, right) -> `Insert (insert_ilist lp1 f left, elt, right)
  | `Right::lp1, `Insert (left, elt, right) -> `Insert (left, elt, insert_ilist lp1 f right)
  | _ -> assert false

let insert_patt insert (f : 'a option -> 'a) (p : path) (patt : 'a patt) : 'a patt =
  match p, patt with
  | `I::p1, `Vec (i,j) -> `Vec (insert f p1 i, j)
  | `J::p1, `Vec (i,j) -> `Vec (i, insert f p1 j)
  | `Pos::p1, `Point (pos,color) -> `Point (insert f p1 pos, color)
  | `Color::p1, `Point (pos,color) -> `Point (pos, insert f p1 color)
  | `Pos::p1, `Rectangle (pos,size,color,mask) -> `Rectangle (insert f p1 pos, size,color,mask)
  | `Size::p1, `Rectangle (pos,size,color,mask) -> `Rectangle (pos, insert f p1 size, color,mask)
  | `Color::p1, `Rectangle (pos,size,color,mask) -> `Rectangle (pos,size, insert f p1 color, mask)
  | `Mask::p1, `Rectangle (pos,size,color,mask) -> `Rectangle (pos,size,color, insert f p1 mask)
  | `Size::p1, `Background (size,color,layers) -> `Background (insert f p1 size, color,layers)
  | `Color::p1, `Background (size,color,layers) -> `Background (size, insert f p1 color,layers)
  | `Layers lp :: p1, `Background (size,color,layers) ->
     let layers =
       insert_ilist
         lp
         (function
          | None -> if p1=[] then f None else assert false
          | Some shape -> insert f p1 shape)
         layers in
     `Background (size,color,layers)
  | `Item (None,q)::[], `Many (ordered,items) ->
     let new_items =
       List.map
         (fun item -> insert f q item)
         items in
     `Many (ordered, new_items)
  | `Item (Some i,q)::[], `Many (ordered,items) ->
     let ar_items = Array.of_list items in
     assert (i >= 0 && i < Array.length ar_items);
     ar_items.(i) <- insert f q (ar_items.(i) :> template);
     let new_items = Array.to_list ar_items in
     `Many (ordered, new_items)
  | _ ->
     pp_path p; print_string ": ";
     print_string (string_of_patt (fun _ -> "_") patt);
     print_newline ();
     assert false

let insert_expr insert (f : 'a option -> 'a) (p : path) (e : 'a expr) : 'a expr =
  match p, e with
  | _, (`Plus _ | `Minus _) -> assert false
  | `Item (_,q)::[], `For (p_many, t1) -> `For (p_many, insert f q t1)
  | _ -> assert false
       
let rec insert_template (f : template option -> template) (p : path) (t : template) : template =
  match p, t with
  | [], _ -> f (Some t)
  | `Item (_,q)::[], `Repeat patt1 -> `Repeat (insert_patt insert_template f q patt1)
  | _, (#patt as patt) -> (insert_patt insert_template f p patt :> template)
  | _, (#expr as e) -> (insert_expr insert_template f p e :> template)
  | _ -> assert false

                   
(*let rec map_template (f : path -> template -> template) (p : path) (t : template) : template =
  match t with
  | `U -> f p t
  | #expr -> f p t
  | `Bool _ | `Int _ | `Color _ | `Mask _ -> f p t
  | `Vec (i,j) ->
     let i = map_template f (p ++ `I) i in
     let j = map_template f (p ++ `J) j in
     f p (`Vec (i,j))
  | `Point (pos,color) ->
     let pos = map_template f (p ++ `Pos) pos in
     let color = map_template f (p ++ `Color) color in
     f p (`Point (pos,color))
  | `Rectangle (pos,size,color,mask) ->
     let pos = map_template f (p ++ `Pos) pos in
     let size = map_template f (p ++ `Size) size in
     let color = map_template f (p ++ `Color) color in
     let mask = map_template f (p ++ `Mask) mask in
     f p (`Rectangle (pos,size,color,mask))
  | `Background (size,color,layers) ->
     let size = map_template f (p ++ `Size) size in
     let color = map_template f (p ++ `Color) color in
     let layers =
       map_ilist
         (fun lp shape ->
           map_template f (p ++ `Layers lp) shape)
         [] layers in
     f p (`Background (size,color,layers)) *)
                                                                                    
(* model refinements and learning *)

type grid_refinement =
  | RGridInit
  | RDef of path * template * path option (* many-ctx *)
  | RShape of path * template (* shape *)
  | RRepeat of path
  | RSingle of path

let pp_grid_refinement = function
  | RGridInit -> ()
  | RDef (p,t,ctx) ->
     print_string "DEF: "; pp_path p;
     print_string "="; pp_template t
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
      | RDef (modified_p,new_t,None) ->
         t
         |> insert_template
              (function
               | Some x when x = new_t -> raise Refinement_no_change
               | _ -> new_t)
              modified_p
      | RDef (modified_p, new_t, Some p_many) ->
         let insert_aux local t1 =
           t1
           |> insert_template
                (function
                 | Some x when x = new_t -> raise Refinement_no_change
                 | _ -> new_t)
                local in
         ( match path_split_any_item modified_p with
           | None -> raise Refinement_no_change
           | Some (ctx,local) ->
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
         |> insert_template (fun _ -> `U) [`Color] (* because background color is defined as remaining color after covering shapes *)
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
  with Refinement_no_change -> None) (* does not seem to occur any more *)

let rec defs_refinements ~(env_sig : signature) (t : template) (grss : grid_read list list) : grid_refinement Myseq.t =
  Common.prof "Model2.defs_refinements" (fun () ->
  assert (grss <> []);
  let u_vars =
    List.rev
      (fold_template
         (fun res p t0 ->
           let k = path_kind p in
           if k <> `Grid
           then (p,k,t0)::res
           else res)
         [] path0 t) in
  let val_matrix =
    List.map
      (fun grs ->
        assert (grs <> []); (* otherwise, should be parse failure *)
        List.mapi
          (fun rank (env,gd,dl) ->
            let u_val =
              List.map (fun (p,k,t0) ->
                  match find_data p gd.data with
                  | Some d -> p, d
                  | None -> assert false)
                u_vars in
            env, u_val, dl, rank)
          grs)
      grss in

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
                    (fun rev_delta_defs_yes (p,k,t0) ->
                      let d = try List.assoc p u_val with _ -> assert false in
                      let lt = [] in (* candidate def bodies *)
                      let lt =
                        match t0 with
                        | `U -> List.fold_right
                                  (fun t lt -> (t,None)::lt)
                                  (root_template_of_data d) lt
                        | _ -> lt in (* expressions have priority on patts, which have priority on `U *)
                      let lt =
                        match t0 with
                        | #expr -> lt (* already an expression *)
                        | _ -> (try List.assoc k k_le_map with _ -> assert false) @ lt in 
                      List.fold_left (* WARNING: preserving order on lt *)
                        (fun rev_delta_defs_yes (t,ctx) ->
                          if List.exists (* check if def already found *)
                               (fun (_,_,p0,_,t0,ctx0) -> p0=p && t0=t && ctx0=ctx)
                               defs_yes
                             || not (defs_check p k t ctx d env)
                          then rev_delta_defs_yes
                          else (dl, rank, p, k, t, ctx) :: rev_delta_defs_yes)
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
                    (fun (rev_new_defs_yes,rev_defs_maybe) (sum_dl,sum_rank,p,k,t,ctx as def) ->
                      let d = try List.assoc p u_val with _ -> assert false in
                      if defs_check p k t ctx d env
                      then
                        let rev_new_defs_yes = (sum_dl +. dl, sum_rank + rank, p, k, t, ctx) :: rev_new_defs_yes in
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
       (fun (sum_dl1,sum_rank1,_,_,_,_) (sum_dl2,sum_rank2,_,_,_,_) ->
         Stdlib.compare (sum_dl1,sum_rank1) (sum_dl2,sum_rank2))
  |> Myseq.from_list
  |> Myseq.map (fun (_,_,p,k,t,ctx) -> RDef (p,t,ctx)))
and defs_check p k t ctx d env =
  match t with
  | `U -> assert false (* should not be used as def *)
  | `Repeat _ -> assert false (* should not be used as def *)
  | #patt -> List.mem t (root_template_of_data d)
  | #expr ->
     (*     print_string "CHECK expr: "; pp_template t; Option.iter (fun p_many -> print_string " at ctx "; pp_path p_many) ctx; print_newline (); *)
     let e_opt =
       match ctx, d with
       | None, `Many _ -> None (* incompatible expr and data *)
       | None, _ -> Some t
       | Some p_many, `Many _ -> Some (`For (p_many, t))
       | _ -> None in
     ( match e_opt with
       | None -> false
       | Some e ->
          let res =
            apply_template_gen
              ~lookup:(fun v -> find_data v env)
              p e in
          match res, (d :> template) with
          | `Many (ordered1,items1), `Many (ordered2,items2) ->
             (* TODO: how to handle [ordered] flags *)
             List.sort Stdlib.compare items1 = List.sort Stdlib.compare items2 (* TODO: avoid sorting here *)
          | t1, t2 -> t1 = t2 )
(*
  val_matrix
  |> Common.list_product (* TODO: find a way to avoid this combinatorial generation *)
  |> List.map (fun alignment ->
         let sum_dl, sum_rank =
           List.fold_left
             (fun (sum_dl,sum_rank) (_,_,dl,rank) -> (sum_dl +. dl, sum_rank + rank))
             (0.,0) alignment in
         sum_dl, sum_rank, alignment)
  |> List.stable_sort Stdlib.compare (* increasing DL, increasing rank sum *)
  |> Myseq.from_list
  |> Myseq.flat_map (fun (_,_,alignment) ->
         find_defs ~env_sig ~u_vars alignment)
  |> Myseq.unique)

  (* TODO: find appropriate sorting of defs: syntactic criteria, type criteria, DL criteria *)
 *)
(*and find_defs_transpose ~env_vars ~u_vars alignment =
  Common.prof "Model2.find_defs_transpose" (fun () ->
(* assuming env_val and u_val have variables in same order *)
(* true  by construction above *)
  match alignment with
  | [] ->
     List.map (fun p -> p, []) env_vars,
     List.map (fun (p,k) -> p, []) u_vars
  | (env_val,u_val,dl)::l1 ->
     let env_vals1, u_vals1 =
       find_defs_transpose ~env_vars ~u_vars l1 in
     let env_vals =
       List.map2
         (fun (p,v) (p1,lv1) ->
           assert (p = p1);
           (p, v::lv1))
         env_val env_vals1 in
     let u_vals =
       List.map2
         (fun (p,v) (p1,lv1) ->
           assert (p = p1);
           (p, v::lv1))
         u_val u_vals1 in
     env_vals, u_vals) *)
(*and find_defs ~env_sig ~u_vars alignment (* (env_val, u_val, dl, rank) list *) : grid_refinement Myseq.t =
  Common.prof "Model2.find_defs" (fun () ->
  (* find if some gd param unknowns can be replaced
     by some pattern or expression over env vars *)
  (*let map_env_vals, map_u_vals =
    find_defs_transpose ~env_vars ~u_vars alignment in*)
  let* (u,k,t0) = Myseq.from_list u_vars in
  let u_vals =
    List.map
      (fun (_,u_val,_,_) ->
        match List.assoc_opt u u_val with
        | Some d -> d
        | None -> assert false)
      alignment in
  let defs_patt = (* defining u by a common pattern *)
    if t0 = `U
    then (* only for unknowns, uncessary for patts, expressions have priority *)
      let t_vals = unify u_vals in
      if t_vals = `U
      then Myseq.empty
      else Myseq.return (RDef (u, t_vals))
    else Myseq.empty in
  let defs_expr = (* defining u by an expression *)
    match t0 with
    | #expr -> Myseq.empty (* already an expression *)
    | _ -> (* unknowns and patterns *)
       find_u_expr ~env_sig u k u_vals alignment in
  Myseq.concat [defs_expr; defs_patt])
and find_u_expr ~env_sig u_path u_kind u_vals alignment : grid_refinement Myseq.t = (* QUICK *)
  (* TODO: rather use map_env_vals rather than alignment *)
  (* requires to define eval_expr on lists of env *)
  let int_vars = signature_of_kind env_sig `Int in
  let le = [] in
  let le =
    List.fold_left
      (fun le v -> `Var v::le) (* order does not matter *)
      le (signature_of_kind env_sig u_kind) in
  let le =
    match u_kind with
    | `Int ->
       List.fold_left
         (fun le v ->
           Common.fold_for
             (fun i le ->
	       `Plus (`Var v, `Int i)
               :: `Minus (`Var v, `Int i)
               :: le)
             1 3 le)
         le int_vars
    | _ -> le in
  let le =
    match u_kind with
    | `Int ->
       List.fold_left (fun le v1 ->
           List.fold_left (fun le v2 ->
               let le = `Plus (`Var v1, `Var v2) :: le in
               let le =
                 if v1 = v2 then le (* this means 0 *)
	         else `Minus (`Var v1, `Var v2) :: le in
	       le)
             le int_vars)
         le int_vars
    | _ -> le in
  let le = List.rev le in (* reverse to put in order *)
  Myseq.from_list le
  |> Myseq.filter_map (fun e ->
         let e_vals =
           List.map
             (fun (env_val,_,_,_) ->
               apply_template_gen
                 ~lookup:(fun v ->
                   Option.bind
                     (List.assoc_opt (path_kind v) env_val)
                     (fun env_val_k ->
                       List.assoc_opt v env_val_k))
                 u_path e)
             alignment in
         if e_vals = (u_vals :> template list)
         then Some (RDef (u_path, e))
         else None)
 *)
and defs_expressions ~env_sig : (kind * (template * path option) list) list =
  (* the [path option] is for the repeat context path, to be used in a For loop *)
  let int_vars = signature_of_kind env_sig `Int in
  List.map
    (fun k ->
      let le = [] in
      let le =
        List.fold_left
          (fun le v -> (`Var v, path_ctx v)::le) (* order does not matter *)
          le (signature_of_kind env_sig k) in
      let le =
        match k with
        | `Int ->
           List.fold_left
             (fun le v ->
               let v_ctx = path_ctx v in
               Common.fold_for
                 (fun i le ->
	           (`Plus (`Var v, `Int i), v_ctx)
                   :: (`Minus (`Var v, `Int i), v_ctx)
                   :: le)
                 1 3 le)
             le int_vars
        | _ -> le in
      let le =
        match k with
        | `Int ->
           List.fold_left (fun le v1 ->
               let v1_ctx = path_ctx v1 in
               List.fold_left (fun le v2 ->
                   let v2_ctx = path_ctx v2 in
                   if v1_ctx = v2_ctx
                   then
                     let le = (`Plus (`Var v1, `Var v2), v1_ctx) :: le in
                     let le =
                       if v1 = v2 then le (* this means 0 *)
	               else (`Minus (`Var v1, `Var v2), v1_ctx) :: le in
	             le
                   else le) (* not same context *)
                 le int_vars)
             le int_vars
        | _ -> le in
      let le = List.rev le in (* reverse to put in order *)
      k, le)
    all_kinds


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
       Myseq.cons (RShape ([`Layers lp], `Repeat shape))
         (Myseq.cons (RShape ([`Layers lp], (shape :> template)))
            Myseq.empty)
    | `Insert (above,shape,below) ->
       Myseq.concat
         [ (*aux_repeat [`Layers lp] shape;*) (* TEST *)
           aux (lp ++| `Right) below; (* insert below first *)
           aux (lp ++| `Left) above ]
    | _ -> assert false
  in
  match t with
  | `Background (_,_,layers) -> aux [] layers
  | _ -> assert false
       
let grid_refinements ~(env_sig : signature) (t : template) (grss : grid_read list list) : (grid_refinement * template) Myseq.t =
  Myseq.concat
    [defs_refinements ~env_sig t grss;
     shape_refinements t]
  |> Myseq.filter_map
       (fun r -> apply_grid_refinement r t)

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
  = Common.prof "Model2.model_refinements" (fun () ->
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
  let lm = lmi +. lmo in
  let ld = ldi +. ldo in
  (lmi, lmo, lm),
  (ldi, ldo, ld),
  (lmi+.ldi, lmo+.ldo, lm+.ld))
  
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
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RInit, init_model)
    ~data:(fun (r,m) ->
      try
        (*print_string "\t\t=> "; pp_refinement r; print_newline ();*)
        Result.to_option
          (let| gprs = read_grid_pairs m pairs in
           let grsi, grso = split_grid_pairs_read gprs in
           Result.Ok (gprs,grsi,grso))
      with
      | Common.Timeout as exn -> raise exn
      | exn ->
	 print_endline (Printexc.to_string exn);
	 pp_refinement r; print_newline ();
         pp_model m; print_newline ();
	 raise exn)
    ~code:(fun (r,m) (gpsr,gsri,gsro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     dl_model_data gpsr in
           if verbose then (
             Printf.printf "\t?? %.1f\t" lmd;
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
      Printf.printf "%.1f\t" dl; pp_refinement r; print_newline ();
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
