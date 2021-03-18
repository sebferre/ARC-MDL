
open Task

let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v
   
let alpha = def_param "alpha" 10. string_of_float
let max_nb_parse = def_param "max_nb_parse" 3 string_of_int (* max nb of selected grid parses *)
let max_nb_diff = def_param "max_nb_diff" 2 string_of_int (* max nb of allowed diffs in grid parse *)

exception TODO

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
     | Ok ly0, Ok ly1 -> Ok (List.rev_append ly0 ly1)
     | Ok ly0, Error _ -> Ok ly0
     | Error _, Ok ly1 -> Ok ly1
     | Error e1, Error _ -> Error e1
  in
  let| lx = lx_res in
  aux lx
let ( let+|+ ) = result_list_bind_some
                   
(** Part 1: grids *)
        
(* type definitions for data, expressions, templates *)

type 'a triple = 'a * 'a * 'a
                
type kind =
  [ `Int | `Bool | `Color | `Mask | `Vec | `Shape | `Layers | `Grid ]
        
type 'a patt =
  [ `Bool of bool
  | `Int of int
  | `Color of Grid.color
  | `Mask of Grid.Mask.t option
  | `Vec of 'a * 'a (* i, j *)
  | `Point of 'a * 'a (* pos, color *)
  | `Rectangle of 'a * 'a * 'a * 'a (* pos, size, color, mask *)
  | `Background of 'a * 'a * 'a (* size, color, layers (top first) *)
  | `Nil
  | `Insert of 'a * 'a * 'a (* layers above, shape, layers below *)
  ]

type field = [ `I | `J | `Pos | `Color | `Size | `Mask | `Layers | `Above | `Shape | `Below ]
type path = field list
let path0 = []
let (++) p f = p @ [f]
  
type data = data patt
let data0 = `Nil

type expr =
  [ `Var of path
  | expr patt
  | `Plus of expr * expr
  | `Minus of expr * expr
  | `If of expr * expr * expr
  ]
         
type template =
  [ `U
  | template patt
  | `E of expr ]

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

let string_of_field : field -> string = function
  | `I -> "i"
  | `J -> "j"
  | `Pos -> "pos"
  | `Color -> "color"
  | `Size -> "size"
  | `Mask -> "mask"
  | `Layers -> "layers"
  | `Above -> "a"
  | `Shape -> "shape"
  | `Below -> "b"

let string_of_path : path -> string =
  fun p -> String.concat "." (List.map string_of_field p)

let pp_path p = print_string (string_of_path p)

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
  | `Nil -> ""
  | `Insert (above,shape,below) ->
     "[ " ^ string above
     ^ "\n  " ^ string shape
     ^ string below ^ " ]"
  | `Background (size,color,layers) ->
     "a background with size " ^ string size
     ^ " and color " ^ string color
     ^ (if layers = `Nil
        then ""
        else " and layers: " ^ string layers)

let rec string_of_data : data -> string = function
  | #patt as patt -> string_of_patt string_of_data patt

let pp_data d = print_string (string_of_data d)

let rec string_of_expr : expr -> string = function
  | `Var p -> string_of_path p
  | #patt as p -> string_of_patt string_of_expr p
  | `Plus (a,b) ->
     string_of_expr a ^ " + " ^ string_of_expr b
  | `Minus (a,b) ->
     string_of_expr a ^ " - " ^ string_of_expr b
  | `If (cond,e1,e2) ->
     "if " ^ string_of_expr cond
     ^ " then " ^ string_of_expr e1
     ^ " else " ^ string_of_expr e2

let pp_expr e = print_string (string_of_expr e)

let rec string_of_template : template -> string = function
  | `U -> "?"
  | #patt as patt -> string_of_patt string_of_template patt
  | `E e -> "{" ^ string_of_expr e ^ "}"

let pp_template t = print_string (string_of_template t)
                                         
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
  | `Layers::p, `Background (size,color,layers) -> find p layers
  | `Above::p, `Insert (above,shape,below) -> find p above
  | `Shape::p, `Insert (above,shape,below) -> find p shape
  | `Below::p, `Insert (above,shape,below) -> find p below
  | _ -> None

let rec find_data (p : path) (d : data) : data option = (* QUICK *)
  match p with
  | [] -> Some d
  | _ -> find_patt find_data p d

let rec find_template (p : path) (t : template) : template option =
  Common.prof "Model2.find_template" (fun () ->
  match p, t with
  | [], _ -> Some t
  | _, (#patt as patt) -> find_patt find_template p patt
  | _ -> None)

       
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
  | `Nil -> acc
  | `Insert (above,shape,below) ->
     let acc = fold acc (p ++ `Above) above in
     let acc = fold acc (p ++ `Shape) shape in
     let acc = fold acc (p ++ `Below) below in
     acc
  | `Background (size,color,layers) ->
     let acc = fold acc (p ++ `Size) size in
     let acc = fold acc (p ++ `Color) color in
     let acc = fold acc (p ++ `Layers) layers in
     acc

let rec fold_data (f : 'b -> path -> data -> 'b) (acc : 'b) (p : path) (d : data) : 'b =
  let acc = f acc p d in
  fold_patt (fold_data f) acc p d

let rec fold_template (f : 'b -> path -> template -> 'b) (acc : 'b) (p : path) (t : template) : 'b =
  let acc = f acc p t in
  match t with
  | `U -> acc
  | #patt as patt -> fold_patt (fold_template f) acc p patt
  | `E _ -> acc (* not folding through expressions *)
       
let size_of_data (d : data) : int =
  fold_data (fun res _ _ -> res+1) 0 path0 d
let size_of_template (t : template) : int =
  fold_template (fun res _ _ -> res+1) 0 path0 t

let fold_unknowns (f : 'a -> path -> 'a) (init : 'a) (p : path) (t : template) : 'a =
  fold_template
    (fun res p1 ->
      function
      | `U -> f res p1
      | _ -> res)
    init p t

let path_kind (p : path) : kind =
  match List.rev p with
  | (`I | `J)::_ -> `Int
  | `Color::_ -> `Color
  | `Mask::_ -> `Mask
  | (`Pos | `Size)::_ -> `Vec
  | `Layers::_ -> `Layers
  | `Above::_ -> `Layers
  | `Shape::_ -> `Shape
  | `Below::_ -> `Layers
  | [] -> `Grid


let unify (ld : data list) : template (* without expression *) = (* QUICK *)
  let rec aux t d =
    match t, d with
    | `U, _ -> t
    | `E _, _ -> assert false
    | `Bool b1, `Bool b2 when b1=b2 -> t
    | `Int i1, `Int i2 when i1=i2 -> t
    | `Color c1, `Color c2 when c1=c2 -> t
    | `Mask m1, `Mask m2 when m1=m2 -> t
                                     
    | `Vec (i1,j1), `Vec (i2,j2) -> `Vec (`U, `U)
      
    | `Point _, `Point _ ->
       `Point (`U, `U)
    | (`Point _ | `Rectangle _), (`Point _ | `Rectangle _) ->
       `Rectangle (`U, `U, `U, `U)
      
    | `Nil, `Nil -> t
    | `Insert (a1,s1,b1), `Insert (a2,s2,b2) ->
       `Insert (`U,`U,`U)
      
    | `Background (size1,color1,layers1), `Background (size2,color2,layers2) ->
       `Background (`U, `U, `U)

    | _ -> `U
  in
  match ld with
  | [] -> assert false
  | d0::ld1 -> List.fold_left aux (d0 :> template) ld1
        
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

     
type sdl_ctx =
  { env_size : int;
    box_height : int;
    box_width : int }
let sdl_ctx0 =
  { env_size = 0;
    box_height = Grid.max_size;
    box_width = Grid.max_size }
  
let sdl_ctx_of_data (d : data) : sdl_ctx =
  (* retrieving grid size: make assumption on paths *)
  let box_height =
    match find_data [`Size; `I] d with
    | Some (`Int i) -> i
    | _ -> Grid.max_size in
  let box_width =
    match find_data [`Size; `J] d with
    | Some (`Int j) -> j
    | _ -> Grid.max_size in
  { env_size = 0; box_height; box_width }
      

type staged_dl (* sdl *) = Mdl.bits * (ctx:sdl_ctx -> data -> Mdl.bits)
(* template DL, variable data DL (for U bindings) *)
let (+!) dl1 (dl2,f2) = (dl1+.dl2, f2)
let (+?) (dl1,f1) (dl2,f2) = (dl1+.dl2, (fun ~ctx data -> f1 ~ctx data +. f2 ~ctx data))
let staged0 = (fun ~ctx d -> 0.)
let sdl0 = 0., staged0

let dl_patt_as_template = Mdl.Code.usage 0.4
         
let sdl_patt
      (sdl : ctx:sdl_ctx -> 'a -> path -> staged_dl)
      ~(ctx : sdl_ctx) (patt : 'a patt) (p : path) : staged_dl =
  match List.rev p, patt with
  | `I::`Pos::_, `Int i -> dl_index ~bound:ctx.box_height i, staged0
  | `J::`Pos::_, `Int j -> dl_index ~bound:ctx.box_width j, staged0

  | (`I | `J)::`Size::_, `Int i -> dl_length i, staged0

  | `Color::`Shape::_, `Color c -> dl_color c, staged0

  | `Color::_, `Color c -> dl_background_color c, staged0

  | `Mask::_, `Mask m -> dl_mask m, staged0

  | (`Size | `Pos)::_, `Vec (i,j) ->
     sdl ~ctx i (p ++ `I)
     +? sdl ~ctx j (p ++ `J)

  | `Shape::_, `Point (pos,color) ->
     Mdl.Code.usage 0.5
     +! sdl ~ctx pos (p ++ `Pos)
     +? sdl ~ctx color (p ++ `Color)
  | `Shape::_, `Rectangle (pos,size,color,mask) ->
     Mdl.Code.usage 0.5
     +! sdl ~ctx pos (p ++ `Pos)
     +? sdl ~ctx size (p ++ `Size)
     +? sdl ~ctx color (p ++ `Color)
     +? sdl ~ctx mask (p ++ `Mask)

  | (`Layers | `Above | `Below)::_, `Nil ->
     Mdl.Code.usage 0.5, staged0
  | (`Layers | `Above | `Below)::_, `Insert (above,shape,below) ->
     Mdl.Code.usage 0.5
     +! sdl ~ctx above (p ++ `Above)
     +? sdl ~ctx shape (p ++ `Shape)
     +? sdl ~ctx below (p ++ `Below)

  | [], `Background (size,color,layers) ->
     let box_height =
       match size with
       | `Vec (`Int i, _) -> i
       | _ -> ctx.box_height in
     let box_width =
       match size with
       | `Vec (_, `Int j) -> j
       | _ -> ctx.box_width in
     let ctx_layers = { ctx with box_height; box_width } in
     sdl ~ctx size (p ++ `Size)
     +? sdl ~ctx color (p ++ `Color)
     +? sdl ~ctx:ctx_layers layers (p ++ `Layers)
    
  | _ ->
     pp_path p; print_string ": ";
     print_string (string_of_patt (fun _ -> "_") patt);
     print_newline ();
     assert false

let rec sdl_data ~(ctx : sdl_ctx) (d : data) (p : path) : staged_dl =
  dl_patt_as_template (* NOTE: to align with sdl_template on patterns *)
  +! sdl_patt sdl_data ~ctx d p
let dl_data ~ctx (d : data) (p : path) : dl =
  fst (sdl_data ~ctx d p)

let rec sdl_expr ~(ctx : sdl_ctx) (e : expr) (p : path) : staged_dl =
  (* TODO: make it kind-dependent, including coding vars *)
  match e with
  | `Var x ->
     Mdl.Code.usage 0.5
     +. Mdl.Code.uniform ctx.env_size, (* identifying one env var *)
     staged0
  | #patt as patt ->
     Mdl.Code.usage 0.25
     +! sdl_patt sdl_expr ~ctx patt p
  | _ ->
     Mdl.Code.usage 0.25
     +! (match List.rev p, e with
         | (`I | `J)::_, (`Plus (e1,e2) | `Minus (e1,e2)) ->
            Mdl.Code.usage 0.5
            +! sdl_expr ~ctx e1 p
            +? sdl_expr ~ctx e2 p
         | _ -> assert false)

let dl_expr ~ctx e p =
  fst (sdl_expr ~ctx e p)
         
let rec sdl_template ~(ctx : sdl_ctx) (t : template) (p : path) : staged_dl =
  match t with
  | `U ->
     Mdl.Code.usage 0.2,
     (fun ~ctx d ->
       match find_data p d with
       | Some d1 -> dl_data ~ctx d1 p
       | None -> assert false)
  | `E e ->
     Mdl.Code.usage 0.4 +. dl_expr ~ctx e p,
     staged0
  | #patt as patt ->
     dl_patt_as_template
     +! sdl_patt sdl_template ~ctx patt p

let dl_diff ~(ctx : sdl_ctx) (diff : diff) (data : data) : dl =
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
         +. dl_data ~ctx d1 p1)
    
let dl_delta ~(ctx : sdl_ctx) (delta : delta) : dl =
  let nb_pixels = List.length delta in
  Mdl.Code.universal_int_star nb_pixels (* number of delta pixels *)
  -. 1. (* some normalization to get 0 for empty grid data *)
  +. float nb_pixels
     *. (4. *. dl_patt_as_template
         (* NOTE: to align with cost of points in data: Vec (I i, J j), Col c *)
         +. Mdl.Code.uniform ctx.box_height
         +. Mdl.Code.uniform ctx.box_width
         +. Mdl.Code.uniform Grid.nb_color)
(* NOT using optimized DL below for fair comparisons with model points: 
  +. Mdl.Code.comb nb_pixels area (* where they are *)
  +. float nb_pixels *. Mdl.Code.uniform (Grid.nb_color - 1) (* what are their color, different from the color generated by the model *) *)
    
(* evaluation of expression and templates on environment data *)

exception Unbound_Var of path
exception Invalid_expr of expr
let _ =
  Printexc.register_printer
    (function
     | Unbound_Var p -> Some ("unbound variable: " ^ string_of_path p)
     | Invalid_expr e -> Some ("invalid expression: " ^ string_of_expr e)
     | _ -> None)

let eval_patt (eval : 'a -> 'b) : 'a patt -> 'b patt = function
  | (`Bool _ | `Int _ | `Color _ | `Mask _ as d) -> d
  | `Vec (i,j) -> `Vec (eval i, eval j)
  | `Point (pos,color) -> `Point (eval pos, eval color)
  | `Rectangle (pos,size,color,mask) -> `Rectangle (eval pos, eval size, eval color, eval mask)
  | `Nil -> `Nil
  | `Insert (above,shape,below) -> `Insert (eval above, eval shape, eval below)
  | `Background (size,color,layers) -> `Background (eval size, eval color, eval layers)
                       
let rec eval_expr_gen ~(lookup : path -> data option) (e : expr) : data = (* QUICK *)
  match e with
  | `Var p ->
     (match lookup p with
      | Some d -> d
      | None -> raise (Unbound_Var p))
  | #patt as p -> eval_patt (eval_expr_gen ~lookup) p
  | `Plus (e1,e2) ->
     (match eval_expr_gen ~lookup e1, eval_expr_gen ~lookup e2 with
      | `Int i1, `Int i2 -> `Int (i1 + i2)
      | _ -> raise (Invalid_expr e))
  | `Minus (e1,e2) ->
     (match eval_expr_gen ~lookup e1, eval_expr_gen ~lookup e2 with
      | `Int i1, `Int i2 -> `Int (i1 - i2)
      | _ -> raise (Invalid_expr e))
  | `If (e0,e1,e2) ->
     (match eval_expr_gen ~lookup e0 with
      | `Bool b ->
         if b
         then eval_expr_gen ~lookup e1
         else eval_expr_gen ~lookup e2
      | _ -> raise (Invalid_expr e0))

let rec eval_expr ~(env : data) (e : expr) : data =
  eval_expr_gen
    ~lookup:(fun p -> find_data p env)
    e

exception Unbound_U
let _ = Printexc.register_printer
          (function
           | Unbound_U -> Some "unexpected unknown when evaluating template"
           | _ -> None)
                     
let rec eval_template ~(env : data) (t : template) : data =
  Common.prof "Model2.eval_template" (fun () ->
  match t with
  | `U -> raise Unbound_U
  | #patt as p -> eval_patt (eval_template ~env) p
  | `E e -> eval_expr ~env e)


(* grid generation from data and template *)

exception Invalid_data_as_grid
let _ = Printexc.register_printer
          (function
           | Invalid_data_as_grid -> Some "the data does not represent a valid grid specification"
           | _ -> None)
          
let rec grid_of_data : data -> Grid.t = function
  | `Background (`Vec (`Int h, `Int w), `Color c, l) ->
     let g = Grid.make h w c in
     draw_layers g l;
     g
  | _ -> raise Invalid_data_as_grid
and draw_layers g = function
  | `Nil -> ()
  | `Insert (above, shape, below) ->
     draw_layers g below;
     draw_shape g shape;
     draw_layers g above
  | _ -> raise Invalid_data_as_grid
and draw_shape g = function
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
  | _ -> raise Invalid_data_as_grid


let write_grid ~(env : data) ?(delta = delta0) (t : template) : (Grid.t, exn) Result.t = Common.prof "Model2.write_grid" (fun () ->
  try
    let d = eval_template ~env t in
    let g = grid_of_data d in
    List.iter
      (fun (i,j,c) -> Grid.set_pixel g i j c)
      delta;
    Result.Ok g
  with exn -> Result.Error exn)       
       

(* parsing grids with templates *)

type parse_state =
  { nb_diff: int; (* list length of diff *)
    diff: diff; (* paths to data that differ from template patterns *)
    delta: delta; (* pixels that are not explained by the template *)
    mask: Grid.Mask.t; (* remaining part of the grid to be explained *)
    parts: Grid.part list; (* remaining parts that can be used *)
    grid: Grid.t; (* the grid to parse *)
  }

let add_diff path state =
  { state with nb_diff = 1 + state.nb_diff;
               diff = path::state.diff }

let rec parse_template
      ~(parse_u : (data * parse_state) Myseq.t)
      ~(parse_patt : template patt -> (data * parse_state) Myseq.t)
      ~(env : data) (t : template) (p : path) (x : 'a) (state : parse_state)
        : (data * parse_state) Myseq.t =
  match t with
  | `U -> parse_u
  | `E e ->
     let d0 = eval_expr ~env e in
     parse_template ~parse_u ~parse_patt ~env (d0 :> template) p x state
  | #patt as patt -> parse_patt patt

let parse_bool ~env t p (b : bool) state =
  parse_template
    ~parse_u:(Myseq.return (`Bool b, state))
    ~parse_patt:(function
      | `Bool b0 ->
         if b=b0 then Myseq.return (`Bool b, state)
         else if state.nb_diff < !max_nb_diff then
           Myseq.return (`Bool b, add_diff p state)
         else Myseq.empty
      | _ -> Myseq.empty)
    ~env t p b state

let parse_int ~env t p (i : int) state =
  parse_template
    ~parse_u:(Myseq.return (`Int i, state))
    ~parse_patt:(function
      | `Int i0 ->
         if i=i0 then Myseq.return (`Int i, state)
         else if state.nb_diff < !max_nb_diff then
           Myseq.return (`Int i, add_diff p state)
         else Myseq.empty
      | _ -> Myseq.empty)
    ~env t p i state

let parse_color ~env t p (c : Grid.color) state =
  parse_template
    ~parse_u:(Myseq.return (`Color c, state))
    ~parse_patt:(function
      | `Color c0 ->
         if c=c0 then Myseq.return (`Color c, state)
         else if state.nb_diff < !max_nb_diff then
           Myseq.return (`Color c, add_diff p state)
         else Myseq.empty
      | _ -> Myseq.empty)
    ~env t p c state
  
let parse_mask ~env t p (m : Grid.Mask.t option) state =
  parse_template
    ~parse_u:(Myseq.return (`Mask m, state))
    ~parse_patt:(function
      | `Mask m0 ->
         if m=m0 then Myseq.return (`Mask m, state)
         else if state.nb_diff < !max_nb_diff then
           Myseq.return (`Mask m, add_diff p state)
         else Myseq.empty
      | _ -> Myseq.empty)
    ~env t p m state

let parse_vec ~env t p (vi, vj : int * int) state =
  parse_template
    ~parse_u:(Myseq.return (`Vec (`Int vi, `Int vj), state))
    ~parse_patt:(function
      | `Vec (i,j) ->
         let* di, state = parse_int ~env i (p ++ `I) vi state in
         let* dj, state = parse_int ~env j (p ++ `J) vj state in
         Myseq.return (`Vec (di,dj), state)
      | _ -> Myseq.empty)
    ~env t p (vi,vj) state
  
let shape_postprocess (state : parse_state) (seq_shapes : ('a * delta * Grid.Mask.t) Myseq.t) : ('a * parse_state) Myseq.t = (* QUICK *)
  seq_shapes
  |> Myseq.map
       (fun (shape, occ_delta, occ_mask) ->
         let new_mask = Grid.Mask.diff state.mask occ_mask in
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
	 (shape, new_state))
    
let parse_shape =
  let parse_points ~env parts state =
    Grid.points state.grid state.mask parts
    |> Myseq.from_list
    |> Myseq.map (fun (i,j,c) ->
           let occ_delta = [] in
           let occ_mask = Grid.Mask.singleton state.grid.height state.grid.width i j in
           (i,j,c), occ_delta, occ_mask)
    |> shape_postprocess state in
  let parse_rectangles ~env parts state =
    Grid.rectangles state.grid state.mask parts
    |> Myseq.from_list
    |> Myseq.map (fun (rect : Grid.rectangle) ->
           rect, rect.delta, rect.mask)
    |> shape_postprocess state in
  fun ~env t p (parts : Grid.part list) state ->
  Common.prof "Model2.parse_shape" (fun () ->
  parse_template
    ~parse_u:
    (Myseq.concat
       [parse_points ~env parts state
        |> Myseq.map
             (fun ((i,j,c), state) ->
               `Point (`Vec (`Int i, `Int j), `Color c),
               state);
        parse_rectangles ~env parts state
        |> Myseq.map
             (fun (r,state) ->
               let open Grid in
               `Rectangle (`Vec (`Int r.offset_i, `Int r.offset_j),
                           `Vec (`Int r.height, `Int r.width),
                           `Color r.color,
                           `Mask r.rmask),
               state) ])
    ~parse_patt:(function
      | `Point (pos,color) ->
         let* (i,j,c), state = parse_points ~env parts state in
         let* dpos, state = parse_vec ~env pos (p ++ `Pos) (i,j) state in
         let* dcolor, state = parse_color ~env color (p ++ `Color) c state in
         Myseq.return (`Point (dpos,dcolor), state)
      | `Rectangle (pos,size,color,mask) ->
         let* r, state = parse_rectangles ~env parts state in
         let open Grid in
         let* dpos, state = parse_vec ~env pos (p ++ `Pos) (r.offset_i,r.offset_j) state in
         let* dsize, state = parse_vec ~env size (p ++ `Size) (r.height,r.width) state in
         let* dcolor, state = parse_color ~env color (p ++ `Color) r.color state in
         let* dmask, state = parse_mask ~env mask (p ++ `Mask) r.rmask state in
         Myseq.return (`Rectangle (dpos,dsize,dcolor,dmask), state)
      | _ -> Myseq.empty)
    ~env t p parts state)

let rec parse_layers ~env t p (g : Grid.t) state =
  parse_template
    ~parse_u:(Myseq.empty)
    ~parse_patt:
    (function
     | `Nil ->
        Myseq.return (`Nil, state)
     | `Insert (above,shape,below) ->
        let* dabove, state = parse_layers ~env above (p ++ `Above) g state in
        let* dshape, state = parse_shape ~env shape (p ++ `Shape) state.parts state in
        let* dbelow, state = parse_layers ~env below (p ++ `Below) g state in
        let data = `Insert (dabove, dshape, dbelow) in
        Myseq.return (data, state)
     | _ -> Myseq.empty)
    ~env t p g state
  
let parse_grid ~env t p (g : Grid.t) state =
  Common.prof "Model2.parse_grid" (fun () ->
  parse_template
    ~parse_u:(Myseq.empty)
    ~parse_patt:
    (function
     | `Background (size,color,layers) ->
        let* dlayers, state = parse_layers ~env layers (p ++ `Layers) g state in
        let* dsize, state = parse_vec ~env size (p ++ `Size) (g.height,g.width) state in
        let bc = (* background color *)
          (* determining the majority color *)
	  let color_counter = new Common.counter in
	  Grid.Mask.iter
	    (fun i j ->
              if Grid.Mask.mem i j state.mask
              then color_counter#add g.matrix.{i,j})
	    state.mask;
	  (match color_counter#most_frequents with
	   | _, bc::_ -> bc
	   | _ -> Grid.black) in
        let* dcolor, state = parse_color ~env color (p ++ `Color) bc state in
        let data = `Background (dsize,dcolor,dlayers) in
	(* adding mask pixels with other color than background to delta *)
        let new_delta = ref state.delta in
	Grid.Mask.iter
	  (fun i j ->
	    if g.matrix.{i,j} <> bc then
	      new_delta := (i,j, g.matrix.{i,j})::!new_delta)
	  state.mask;
        let new_state =
          { state with
            delta = (!new_delta);
	    mask = Grid.Mask.empty g.height g.width;
	    parts = [] } in
	Myseq.return (data, new_state)
     | _ -> Myseq.empty)
    ~env t p g state)

exception Parse_failure
let _ = Printexc.register_printer
          (function
           | Parse_failure -> Some "the grid could not be parsed with the given template"
           | _ -> None)

type grid_read = data * grid_data * dl

let compare_grid_read =
  fun (_,gd1,dl1) (_,gd2,dl2) ->
  Stdlib.compare (dl1,gd1) (dl2,gd2)
let sort_grid_reads : grid_read list -> grid_read list =
  fun grs ->
  Common.prof "Model2.sort_grid_reads" (fun () ->
  List.sort_uniq compare_grid_read grs)
               
let read_grid
      ~(dl_grid_data : ctx:sdl_ctx -> data -> dl)
      ~(env : data) (t : template) (g : Grid.t)
    : (grid_read list, exn) Result.t =
  Common.prof "Model2.read_grid" (fun () ->
  let state = { nb_diff = 0;
                diff = diff0;
                delta = delta0;
                mask = Grid.Mask.full g.height g.width;
                parts = Grid.segment_by_color g;
                grid = g } in
  let parses =
    let* data, state = parse_grid ~env t path0 g state in
    let ctx = sdl_ctx_of_data data in
    let dl_data = dl_grid_data ~ctx data in
    let dl_diff = dl_diff ~ctx state.diff data in
    let dl_delta = dl_delta ~ctx state.delta in
    let dl = dl_data +. dl_diff +. dl_delta in
    Myseq.return (env, {data; diff=state.diff; delta=state.delta}, dl) in
  let l_sorted_parses =
    parses
    |> Myseq.to_rev_list
    |> sort_grid_reads in
  (*Printf.printf "### %d ###\n" (List.length l_sorted_parses);*)
  if l_sorted_parses = []
  then Result.Error Parse_failure
  else Result.Ok (Common.sub_list l_sorted_parses 0 !max_nb_parse))
							      
(* result of reading a grid: grid data and description lengths *)
type grids_read =
  { dl_m : dl;
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

let dl_template_data (gsr : grids_read) : dl triple (* model, data, model+data *) =
  let dl_data =
    !alpha (* because given training examples are only a sample from a class of grids *)
    *. Mdl.sum gsr.reads
         (function
          | [] -> assert false
          | (_env,_gd,dl)::_ -> dl) in (* using smallest dl *)
  gsr.dl_m, dl_data, gsr.dl_m +. dl_data
		    
let read_grids ~env_size (t : template) (egrids: (data * Grid.t) list) : (grids_read, exn) Result.t =
  let dl_m, dl_grid_data =
    sdl_template
      ~ctx:{env_size; box_height=Grid.max_size; box_width=Grid.max_size}
      t path0 in
  let grss_res =
    let+| env, g = egrids in
    read_grid ~dl_grid_data ~env t g in
  let| reads = grss_res in
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
  let _dl_mi, dl_grid_data =
    sdl_template
      ~ctx:{env_size=0; box_height=Grid.max_size; box_width=Grid.max_size}
      m.input_pattern path0 in    
  let+|+ _, gdi, _ = read_grid ~dl_grid_data ~env m.input_pattern g in
  let| grid = write_grid ~env:gdi.data m.output_template in
  Result.Ok [(gdi, grid)])

let read_grid_pair ~dl_gdi ~dl_gdo ?(env = data0) (m : model) (gi : Grid.t) (go : Grid.t) : (grid_read list * grid_read list, exn) Result.t =
  let| gris = read_grid ~dl_grid_data:dl_gdi ~env m.input_pattern gi in
  let| gros =
    let+|+ _, gdi, _ = Result.Ok gris in
    read_grid ~dl_grid_data:dl_gdo ~env:gdi.data m.output_template go in
  let gros = sort_grid_reads gros in
  (* TODO: should probably sort by dli + dlo rather than dlo only (joint DL) *)
  let gros = Common.sub_list gros 0 !max_nb_parse in
  Result.Ok (gris,gros)

let read_grid_pairs ?(env = data0) (m : model) (pairs : Task.pair list) : (grids_read * grids_read, exn) Result.t =
  Common.prof "Model2.read_grid_pairs" (fun () ->
  (* takes model, input env+grid, output grids *)
  let dl_mi, dl_gdi =
    sdl_template
      ~ctx:{env_size=0; box_height=Grid.max_size; box_width=Grid.max_size}
      m.input_pattern path0 in    
  let env_size =
    size_of_template m.input_pattern in
  let dl_mo, dl_gdo =
    sdl_template
      ~ctx:{env_size; box_height=Grid.max_size; box_width=Grid.max_size}
      m.output_template path0 in
  let| griss =
    let+| {input} = pairs in
    read_grid ~dl_grid_data:dl_gdi ~env m.input_pattern input in
  let| gross =
    let+| gris, {output} = List.combine griss pairs in
    let+|+ _, gdi, _ = Result.Ok gris in
    read_grid ~dl_grid_data:dl_gdo ~env:gdi.data m.output_template output in
  let gross = (* keeping only max_nb_parse best parses to avoid combinatorial *)
    Common.prof "Model2.read_grid_pairs/sort_limit" (fun () ->
    List.map
      (fun gros ->
        let gros = sort_grid_reads gros in
        Common.sub_list gros 0 !max_nb_parse)
      gross) in
  let gsri = {dl_m = dl_mi; reads = griss } in
  let gsro = {dl_m = dl_mo; reads = gross } in
  Result.Ok (gsri,gsro))

  
(* template transformations *)
                                                                    
let rec map_template (f : path -> template -> template) (p : path) (t : template) : template =
  match t with
  | `U -> f p t
  | `E e -> f p t
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
  | `Nil -> f p t
  | `Insert (above,shape,below) ->
     let above = map_template f (p ++ `Above) above in
     let shape = map_template f (p ++ `Shape) shape in
     let below = map_template f (p ++ `Below) below in
     f p (`Insert (above,shape,below))
  | `Background (size,color,layers) ->
     let size = map_template f (p ++ `Size) size in
     let color = map_template f (p ++ `Color) color in
     let layers = map_template f (p ++ `Layers) layers in
     f p (`Background (size,color,layers))
                                                                               
(* model refinements and learning *)

type grid_refinement =
  | RGridInit
  | RDef of path * template
  | RShape of path * template (* shape *)

let pp_grid_refinement = function
  | RGridInit -> ()
  | RDef (p,t) ->
     print_string "DEF: "; pp_path p;
     print_string "="; pp_template t
  | RShape (path,sh) ->
     print_string "SHAPE at ";
     pp_path path;
     print_string ": ";
     pp_template sh

let apply_grid_refinement (r : grid_refinement) (t : template) : template =
  Common.prof "Model2.apply_grid_refinement" (fun () ->
  match r with
  | RGridInit -> t
  | RDef (modified_p,new_t) ->
     map_template
       (fun p1 t1 ->
         if p1 = modified_p (* not only `U *)
         then new_t
         else t1)
       path0 t
  | RShape (path,shape) ->
     map_template
       (fun p1 t1 ->
         let t1 =
           match t1 with
           | `Background (size,_color,layers) ->
              `Background (size,`U,layers) (* because background color is defined as remaining color after covering shapes *)
           | _ -> t1 in
         if p1 = path && t1 = `Nil
         then `Insert (`Nil, shape, `Nil)
         else t1)
       path0 t)

let definable_kinds = [`Int; `Bool; `Color; `Mask; `Vec; `Shape]

let rec defs_refinements ~(env_vars : path list) (t : template) (grss : grid_read list list) : grid_refinement Myseq.t =
  Common.prof "Model2.defs_refinements" (fun () ->
  assert (grss <> []);
  let u_vars =
    fold_template
      (fun res p _ ->
        if List.mem (path_kind p) definable_kinds
        then p::res
        else res)
      [] path0 t in
  let val_matrix =
    List.map
      (fun grs ->
        List.map
          (fun (env,gd,dl) ->
            let env_val =
              List.map (fun p ->
                  match find_data p env with
                  | Some d -> p, d
                  | None -> assert false)
                env_vars in
            let u_val =
              List.map (fun p ->
                  match find_data p gd.data with
                  | Some d -> p, d
                  | None -> assert false)
                u_vars in
            env_val, u_val, dl)
          grs)
      grss in
  val_matrix
  |> List.map Myseq.from_list
  |> Myseq.product
  |> Myseq.fold_left
       (fun defs alignment ->
         Bintree.union defs (find_defs ~env_vars ~u_vars alignment))
       Bintree.empty
  |> Bintree.elements
  |> Myseq.from_list
  (* TODO: find appropriate sorting of defs: syntactic criteria, type criteria, DL criteria *)
  |> Myseq.map (fun (p,t) -> RDef (p,t)))
and find_defs_transpose ~env_vars ~u_vars alignment =
  Common.prof "Model2.find_defs_transpose" (fun () ->
(* assuming env_val and u_val have variables in same order *)
(* true  by construction above *)
  match alignment with
  | [] ->
     List.map (fun p -> p, []) env_vars,
     List.map (fun p -> p, []) u_vars
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
     env_vals, u_vals)
and find_defs ~env_vars ~u_vars alignment (* (env_val, u_val, dl) list *) : (path * template) Bintree.t =
  Common.prof "Model2.find_defs" (fun () ->
  (* find if some gd param unknowns can be replaced
     by some pattern or expression over env vars *)
  (*let map_env_vals, map_u_vals =
    find_defs_transpose ~env_vars ~u_vars alignment in*)
  List.fold_left
    (fun defs u ->
      let u_vals =
        List.map
          (fun (_,u_val,_) -> try List.assoc u u_val with _ -> assert false)
          alignment in
      let defs =
        let t_vals = unify u_vals in
        if t_vals = `U
        then defs
        else Bintree.add (u, t_vals) defs in
      let defs =
        let exprs = find_u_expr ~env_vars (path_kind u) u_vals alignment in
        List.fold_left
          (fun defs e -> Bintree.add (u, `E e) defs)
          defs exprs in
      defs)
    Bintree.empty u_vars)
and find_u_expr ~env_vars k u_vals alignment : expr list =
  Common.prof "Model2.find_u_expr" (fun () ->
  (* TODO: rather use map_env_vals rather than alignment *)
  (* requires to define eval_expr on lists of env *)
  let le = [] in
  let le =
    List.fold_left
      (fun le v ->
        if path_kind v = k
        then `Var v :: le
        else le)
      le env_vars in
  let le =
    match k with
    | `Int ->
       List.fold_left
         (fun le v ->
           if path_kind v = `Int
           then
             Common.fold_for
               (fun i le ->
	         `Plus (`Var v, `Int i)
                 :: `Minus (`Var v, `Int i)
                 :: le)
               1 3 le
           else le)
         le env_vars
    | _ -> le in
  let le =
    match k with
    | `Int ->
       List.fold_left
         (fun le v1 ->
           if path_kind v1 = `Int
           then
             List.fold_left
               (fun le v2 ->
                 if path_kind v2 = `Int
                 then
	           `Minus (`Var v1, `Var v2)
		   :: `Plus (`Var v1, `Var v2)
                   :: le
                 else le)
               le env_vars
           else le)
         le env_vars
    | _ -> le in
  List.filter
    (fun e ->
      let e_vals =
        List.map
          (fun (env_val,_,_) ->
            eval_expr_gen
              ~lookup:(fun v -> List.assoc_opt v env_val)
              e)
          alignment in
      e_vals = u_vals)
    le)

let shape_refinements (t : template) : grid_refinement Myseq.t =
  Common.prof "Model2.shape_refinements" (fun () ->
  let rec aux p = function
    | `Nil ->
       Myseq.cons (RShape (p, `Point (`U, `U)))
         (Myseq.cons (RShape (p, `Rectangle (`U, `U, `U, `U)))
            Myseq.empty)
    | `Insert (above,shape,below) ->
       Myseq.concat
         [aux (p ++ `Above) above;
          aux (p ++ `Below) below]
    | _ -> assert false
  in
  match t with
  | `Background (_,_,layers) -> aux [`Layers] layers
  | _ -> assert false)
                    
let grid_refinements ~(env_vars : path list) (t : template) (grss : grid_read list list) : (grid_refinement * template) Myseq.t =
  Myseq.concat
    [defs_refinements ~env_vars t grss;
     shape_refinements t]
  |> Myseq.map
       (fun r -> r, apply_grid_refinement r t)

let learn_grid_model ~timeout ~beam_width ~refine_degree ~env_vars
      (egrids : (data * Grid.t) list)
    : ((grid_refinement * template) * grids_read * dl) list * bool =
  let env_size = List.length env_vars in
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RGridInit, init_template)
    ~data:(fun (r,m) ->
      Result.to_option (read_grids ~env_size m egrids))
    ~code:(fun (r,m) gsr ->
      let lm, ld, lmd = dl_template_data gsr in
      lmd)
    ~refinements:(fun (r,m) gsr dl ->
      (*pp_grid_model m; print_newline ();*)
      Printf.printf "%.1f\t" dl;
      pp_grid_refinement r; print_newline ();
      (*Printf.printf "DL = %.1f + %.1f = %.1f\n" lm ld lmd;*)
      grid_refinements ~env_vars m gsr.reads)
		     

type refinement =
  | RInit
  | Rinput of grid_refinement
  | Routput of grid_refinement

let pp_refinement = function
  | RInit -> ()
  | Rinput r -> print_string "IN  "; pp_grid_refinement r
  | Routput r -> print_string "OUT "; pp_grid_refinement r

let model_refinements (last_r : refinement) (m : model) (gsri : grids_read) (gsro : grids_read) : (refinement * model) Myseq.t
  = Common.prof "Model2.model_refinements" (fun () ->
  let on_input = true
    (*match last_r with
    | RInit -> true
    | Rinput _ -> true
    | Routput _ -> false*) in
  let on_output = true in
  let envo_vars =
    fold_template
      (fun res p _ -> p::res)
      [] path0 m.input_pattern in
  let ref_defis =
    if on_input
    then
      defs_refinements ~env_vars:[] m.input_pattern gsri.reads
      |> Myseq.map
           (fun r ->
             Rinput r,
             {m with input_pattern = apply_grid_refinement r m.input_pattern})
    else Myseq.empty in
  let ref_defos =
    if on_output
    then
      defs_refinements ~env_vars:envo_vars m.output_template gsro.reads
      |> Myseq.map
           (fun r ->
             Routput r,
             {m with output_template = apply_grid_refinement r m.output_template})
    else Myseq.empty in
  let ref_shapis =
    if on_input && grids_read_has_delta gsri
    then
      shape_refinements m.input_pattern
      |> Myseq.map
	   (fun r ->
	     Rinput r,
             {m with input_pattern = apply_grid_refinement r m.input_pattern})
    else Myseq.empty in
  let ref_shapos =
    if on_output && grids_read_has_delta gsro
    then
      shape_refinements m.output_template
      |> Myseq.map
	   (fun r ->
	     Routput r,
             {m with output_template = apply_grid_refinement r m.output_template})
    else Myseq.empty in
  Myseq.concat
    [ref_defis; ref_shapis; ref_defos; ref_shapos]
      )

let dl_model_data (gsri : grids_read) (gsro : grids_read) : dl triple triple =
  Common.prof "Model2.dl_model_data" (fun () ->
  let lmi, ldi, lmdi = dl_template_data gsri in
  let lmo, ldo, lmdo = dl_template_data gsro in
  (lmi, lmo, lmi+.lmo), (ldi, ldo, ldi+.ldo), (lmdi, lmdo, lmdi+.lmdo))
  
let learn_model
      ?(verbose = true)
      ~timeout
      ~init_model
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : ((refinement * model) * (grids_read * grids_read) * dl) list * bool
  = Common.prof "Model2.learn_model" (fun () ->
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RInit, init_model)
    ~data:(fun (r,m) ->
      try
        (*print_string "\t\t=> "; pp_refinement r; print_newline ();*)
        read_grid_pairs m pairs
        |> Result.to_option
      with
      | Common.Timeout as exn -> raise exn
      | exn ->
	 print_endline (Printexc.to_string exn);
	 pp_model m;
	 raise exn)
    ~code:(fun (r,m) (gsri,gsro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     dl_model_data gsri gsro in
           (*Printf.printf "\t?? %.1f\t" lmd; pp_refinement r; print_newline ();*)
	   (*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
	   flush stdout;
           lmd)
    ~refinements:
    (fun (r,m) (gsri,gsro) dl ->
      if verbose then (
        Printf.printf "%.1f\t" dl; pp_refinement r; print_newline ();
        (*pp_grids_read "### OUT grids_read ###" gsro;*)
	(*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
	flush stdout);
      model_refinements r m gsri gsro))
