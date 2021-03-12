
open Task

let alpha = ref 10.

exception TODO

let ( let* ) seq f = seq |> Myseq.flat_map f

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
  | `Cons of 'a * 'a (* first layer, rest of layers *)
  ]

type field = [ `I | `J | `Pos | `Color | `Size | `Mask | `Layers | `First | `Rest ]
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
  | `First -> "first"
  | `Rest -> "rest"

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
  | `Nil -> "nil"
  | `Cons (shape,layers) ->
     "\n  " ^ string shape ^ " on top of " ^ string layers
  | `Background (size,color,layers) ->
     "a background with size " ^ string size
     ^ " and color " ^ string color
     ^ " and layers: " ^ string layers

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
  | `First::p, `Cons (first,rest) -> find p first
  | `Rest::p, `Cons (first,rest) -> find p rest
  | _ -> None

let rec find_data (p : path) (d : data) : data option =
  Common.prof "Model2.find_data" (fun () ->
  match p with
  | [] -> Some d
  | _ -> find_patt find_data p d)

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
  | `Cons (first,rest) ->
     let acc = fold acc (p ++ `First) first in
     let acc = fold acc (p ++ `Rest) rest in
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
  | `First::_ -> `Shape
  | `Rest::_ -> `Layers
  | [] -> `Grid


let unify (ld : data list) : template (* without expression *) =
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
    | `Cons (first1,rest1), `Cons (first2,rest2) ->
       `Cons (`U, `U)
      
    | `Background (size1,color1,layers1), `Background (size2,color2,layers2) ->
       `Background (`U, `U, `U)

    | _ -> `U
  in
  Common.prof "Model2.unify" (fun () ->
  match ld with
  | [] -> assert false
  | d0::ld1 -> List.fold_left aux (d0 :> template) ld1)
        
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

let sdl_patt
      (sdl : ctx:sdl_ctx -> 'a -> path -> staged_dl)
      ~(ctx : sdl_ctx) (patt : 'a patt) (p : path) : staged_dl =
  match List.rev p, patt with
  | `I::`Pos::_, `Int i -> dl_index ~bound:ctx.box_height i, staged0
  | `J::`Pos::_, `Int j -> dl_index ~bound:ctx.box_width j, staged0

  | (`I | `J)::`Size::_, `Int i -> dl_length i, staged0

  | `Color::`First::_, `Color c -> dl_color c, staged0

  | `Color::_, `Color c -> dl_background_color c, staged0

  | `Mask::_, `Mask m -> dl_mask m, staged0

  | (`Size | `Pos)::_, `Vec (i,j) ->
     sdl ~ctx i (p ++ `I)
     +? sdl ~ctx j (p ++ `J)

  | `First::_, `Point (pos,color) ->
     Mdl.Code.usage 0.5
     +! sdl ~ctx pos (p ++ `Pos)
     +? sdl ~ctx color (p ++ `Color)
  | `First::_, `Rectangle (pos,size,color,mask) ->
     Mdl.Code.usage 0.5
     +! sdl ~ctx pos (p ++ `Pos)
     +? sdl ~ctx size (p ++ `Size)
     +? sdl ~ctx color (p ++ `Color)
     +? sdl ~ctx mask (p ++ `Mask)

  | (`Layers | `Rest)::_, `Nil ->
     Mdl.Code.usage 0.5, staged0
  | (`Layers | `Rest)::_, `Cons (first,rest) ->
     Mdl.Code.usage 0.5
     +! sdl ~ctx first (p ++ `First)
     +? sdl ~ctx rest (p ++ `Rest)

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
  sdl_patt sdl_data ~ctx d p
let dl_data ~ctx (d : data) (p : path) : dl =
  fst (sdl_data ~ctx d p)

let rec sdl_expr ~(ctx : sdl_ctx) (e : expr) (p : path) : staged_dl =
  match e with
  | `Var x ->
     Mdl.Code.usage 0.5 +. Mdl.Code.uniform ctx.env_size, (* identifying one env var *)
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
     Mdl.Code.usage 0.1,
     (fun ~ctx d ->
       match find_data p d with
       | Some d1 -> dl_data ~ctx d1 p
       | None -> assert false)
  | `E e ->
     Mdl.Code.usage 0.3 +. dl_expr ~ctx e p,
     staged0
  | #patt as patt ->
     Mdl.Code.usage 0.6
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
     *. (Mdl.Code.uniform ctx.box_height +. Mdl.Code.uniform ctx.box_width +. Mdl.Code.uniform Grid.nb_color)
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

let eval_patt (eval : 'a -> data) : 'a patt -> data = function
  | (`Bool _ | `Int _ | `Color _ | `Mask _ as d) -> d
  | `Vec (i,j) -> `Vec (eval i, eval j)
  | `Point (pos,color) -> `Point (eval pos, eval color)
  | `Rectangle (pos,size,color,mask) -> `Rectangle (eval pos, eval size, eval color, eval mask)
  | `Nil -> `Nil
  | `Cons (first,rest) -> `Cons (eval first, eval rest)
  | `Background (size,color,layers) -> `Background (eval size, eval color, eval layers)
                       
let rec eval_expr ~(env : data) (e : expr) : data =
  match e with
  | `Var p ->
     (match find_data p env with
      | Some d -> d
      | None -> raise (Unbound_Var p))
  | #patt as p -> eval_patt (eval_expr ~env) p
  | `Plus (e1,e2) ->
     (match eval_expr ~env e1, eval_expr ~env e2 with
      | `Int i1, `Int i2 -> `Int (i1 + i2)
      | _ -> raise (Invalid_expr e))
  | `Minus (e1,e2) ->
     (match eval_expr ~env e1, eval_expr ~env e2 with
      | `Int i1, `Int i2 -> `Int (i1 - i2)
      | _ -> raise (Invalid_expr e))
  | `If (e0,e1,e2) ->
     (match eval_expr ~env e0 with
      | `Bool b ->
         if b
         then eval_expr ~env e1
         else eval_expr ~env e2
      | _ -> raise (Invalid_expr e0))

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
  | `Cons (first, rest) ->
     draw_layers g rest;
     draw_shape g first
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
  { diff: diff; (* paths to data that differ from template patterns *)
    delta: delta; (* pixels that are not explained by the template *)
    mask: Grid.Mask.t; (* remaining part of the grid to be explained *)
    parts: Grid.part list; (* remaining parts that can be used *)
    grid: Grid.t; (* the grid to parse *)
  }

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
         else Myseq.return (`Bool b, {state with diff = p::state.diff})
      | _ -> Myseq.empty)
    ~env t p b state

let parse_int ~env t p (i : int) state =
  parse_template
    ~parse_u:(Myseq.return (`Int i, state))
    ~parse_patt:(function
      | `Int i0 ->
         if i=i0 then Myseq.return (`Int i, state)
         else Myseq.return (`Int i, {state with diff = p::state.diff})
      | _ -> Myseq.empty)
    ~env t p i state

let parse_color ~env t p (c : Grid.color) state =
  parse_template
    ~parse_u:(Myseq.return (`Color c, state))
    ~parse_patt:(function
      | `Color c0 ->
         if c=c0 then Myseq.return (`Color c, state)
         else Myseq.return (`Color c, {state with diff = p::state.diff})
      | _ -> Myseq.empty)
    ~env t p c state
  
let parse_mask ~env t p (m : Grid.Mask.t option) state =
  parse_template
    ~parse_u:(Myseq.return (`Mask m, state))
    ~parse_patt:(function
      | `Mask m0 ->
         if m=m0 then Myseq.return (`Mask m, state)
         else Myseq.return (`Mask m, {state with diff = p::state.diff})
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
  
let shape_postprocess (state : parse_state) (seq_shapes : ('a * delta * Grid.Mask.t) Myseq.t) : ('a * parse_state) Myseq.t =
  seq_shapes
  |> Myseq.map
       (fun (shape, occ_delta, occ_mask) ->
         let new_mask = Grid.Mask.diff state.mask occ_mask in
         let new_state =
           { diff = state.diff;
             delta = occ_delta @ state.delta;
	     mask = new_mask;
	     parts =
	       List.filter
		 (fun p ->
		   not (Grid.Mask.is_empty
			  (Grid.Mask.inter p.Grid.pixels new_mask)))
		 state.parts;
             grid = state.grid } in
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
    ~env t p parts state

let rec parse_layers ~env t p (g : Grid.t) state =
  parse_template
    ~parse_u:(Myseq.empty)
    ~parse_patt:
    (function
     | `Nil ->
        Myseq.return (`Nil, state)
     | `Cons (first,rest) ->
        let* dfirst, state = parse_shape ~env first (p ++ `First) state.parts state in
        let* drest, state = parse_layers ~env rest (p ++ `Rest) g state in
        let data = `Cons (dfirst, drest) in
        Myseq.return (data, state)
     | _ -> Myseq.empty)
    ~env t p g state
  
let parse_grid ~env t p (g : Grid.t) state =
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
          { diff = state.diff;
            delta = (!new_delta);
	    mask = Grid.Mask.empty g.height g.width;
	    parts = [];
            grid = state.grid } in
	Myseq.return (data, new_state)
     | _ -> Myseq.empty)
    ~env t p g state
  
exception Parse_failure
let _ = Printexc.register_printer
          (function
           | Parse_failure -> Some "the grid could not be parsed with the given template"
           | _ -> None)

let read_grid_aux ~(env : data) (t : template) (g : Grid.t)
      ~(dl_grid_data : ctx:sdl_ctx -> data -> dl)
    : (data * grid_data * dl, exn) Result.t = Common.prof "Model2.read_grid_aux" (fun () ->
  let state = { diff = diff0;
                delta = delta0;
                mask = Grid.Mask.full g.height g.width;
                parts = Grid.segment_by_color g;
                grid = g } in
  let _, res =
    parse_grid ~env t path0 g state
    |> Myseq.fold_left
         (fun (dl_min,res) (data,state) ->
           let ctx = sdl_ctx_of_data data in
           let dl_data = dl_grid_data ~ctx data in
           let dl_diff = dl_diff ~ctx state.diff data in
           let dl_delta = dl_delta ~ctx state.delta in
           let dl = dl_data +. dl_diff +. dl_delta in
           (*Printf.printf "grid data: %.1f (%.1f + %.1f + %.1f)\n" dl dl_data dl_diff dl_delta; (* TEST *)
           pp_data data; print_newline (); (* TEST *) *)
           if dl < dl_min
           then dl, Result.Ok (env, {data; diff=state.diff; delta=state.delta}, dl)
           else dl_min, res)
         (infinity, Result.Error Parse_failure) in
  res)

let read_grid ~(env_size : int) ~(env : data) (t : template) (g : Grid.t) : (grid_data, exn) Result.t =
  let ctx =
    { env_size;
      box_height = Grid.max_size;
      box_width = Grid.max_size } in
  let dl_m, dl_grid_data = sdl_template ~ctx t path0 in
  Result.bind
    (read_grid_aux ~env t g ~dl_grid_data)
    (fun (_env,gd,_l) -> Result.Ok gd)
							      
(* result of reading a grid: grid data and description lengths *)
type grids_read =
  { dl_m : dl;
    egdls : (data * grid_data * dl) list; (* env, {data, delta}, dl *)
  }

let grids_read_has_delta (gr : grids_read) : bool =
  List.for_all
    (fun (_env, (gd : grid_data), _dl) -> gd.delta <> [])
    gr.egdls

let dl_template_data (gr : grids_read) : dl triple (* model, data, model+data *) = Common.prof "Model.l_grid_model_data" (fun () ->
  let dl_data =
    !alpha (* because given training examples are only a sample from a class of grids *)
    *. Mdl.sum gr.egdls (fun (env,gd,dl) -> dl) in
  gr.dl_m, dl_data, gr.dl_m +. dl_data)
		    
let read_grids ~env_size (t : template) (egrids: (data * Grid.t) list) : (grids_read, exn) Result.t =
  let ctx =
    { env_size;
      box_height = Grid.max_size;
      box_width = Grid.max_size } in
  let dl_m, dl_grid_data = sdl_template ~ctx t path0 in
  let rec aux = function
    | [] -> Result.Ok []
    | (env,g)::rest ->
       Result.bind
         (read_grid_aux ~env t g ~dl_grid_data)
         (fun egdl ->
           Result.bind
             (aux rest)
             (fun egdls ->
               Result.Ok (egdl::egdls)))
  in
  Result.bind
    (aux egrids)
    (fun egdls -> Result.Ok {dl_m; egdls})

  
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

let apply_model ?(env = data0) (m : model) (g : Grid.t) : (grid_data * Grid.t, exn) Result.t = Common.prof "Model2.apply_model" (fun () ->
  Result.bind
    (read_grid ~env_size:0 ~env m.input_pattern g)
    (fun gdi ->
      Result.bind
        (write_grid ~env:gdi.data m.output_template)
        (fun grid ->
          Result.Ok (gdi, grid))))


let read_grid_pair ~(env : data) (m : model) (gi : Grid.t) (go : Grid.t) : (grid_data * grid_data, exn) Result.t =
  Result.bind
    (read_grid ~env_size:0 ~env m.input_pattern gi)
    (fun gdi ->
      let env_size = size_of_template m.input_pattern in
      Result.bind
        (read_grid ~env_size ~env:gdi.data m.output_template go)
        (fun gdo -> Result.Ok (gdi,gdo)))
     
let read_grid_pairs (m : model) (egis : (data * Grid.t) list) (gos : Grid.t list) : (grids_read * grids_read, exn) Result.t =
  Common.prof "Model2.read_grid_pairs" (fun () ->
  (* takes model, input env+grid, output grids *)
  Result.bind
    (read_grids ~env_size:0 m.input_pattern egis)
    (fun gri ->
      let env_size =
        size_of_template m.input_pattern in
      let egos =
        List.map2
	  (fun (envi,gdi,li) go -> gdi.data, go)
	  gri.egdls gos in
      Result.bind
        (read_grids ~env_size m.output_template egos)
        (fun gro -> Result.Ok (gri, gro))))

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
  | `Cons (first,rest) ->
     let first = map_template f (p ++ `First) first in
     let rest = map_template f (p ++ `Rest) rest in
     f p (`Cons (first,rest))
  | `Background (size,color,layers) ->
     let size = map_template f (p ++ `Size) size in
     let color = map_template f (p ++ `Color) color in
     let layers = map_template f (p ++ `Layers) layers in
     f p (`Background (size,color,layers))
                                                                               
(* model refinements and learning *)

type grid_refinement =
  | RGridInit
  | RDefs of (path * template) list
  | RShape of int (* depth *) * template (* shape *)

let pp_grid_refinement = function
  | RGridInit -> ()
  | RDefs defs ->
     print_string "DEFS:";
     List.iter
       (fun (p,t) ->
         print_string "  "; pp_path p;
         print_string "="; pp_template t)
       defs
  | RShape (depth,sh) ->
     Printf.printf "SHAPE (depth=%d): " depth;
     pp_template sh

let apply_grid_refinement (r : grid_refinement) (t : template) : template =
  Common.prof "Model2.apply_grid_refinement" (fun () ->
  match r with
  | RGridInit -> t
  | RDefs defs ->
     map_template
       (fun p1 t1 ->
         match t1 with
         | `U ->
            (match List.assoc_opt p1 defs with
             | Some t1' -> t1'
             | None -> t1)
         | _ -> t1)
       path0 t
  | RShape (depth, shape) ->
     let p = `Layers :: List.init depth (fun _ -> `Rest) in
     map_template
       (fun p1 t1 ->
         let t1 =
           match t1 with
           | `Background (size,_color,layers) ->
              `Background (size,`U,layers) (* because background color is defined as remaining color after covering shapes *)
           | _ -> t1 in
         if p1 = p
         then `Cons (shape, t1)
         else t1)
       path0 t)
     
let rec find_defs (t : template) (gr : grids_read) : (path * template) list =
  Common.prof "Model2.find_defs" (fun () ->
  (* find if some gd param unknowns can be replaced
     by some expression over env vars *)
  assert (gr.egdls <> []);
  let envs = List.map (fun (env,_,_) -> env) gr.egdls in
  let datas = List.map (fun (_,gd,_) -> gd.data) gr.egdls in
  fold_unknowns
    (fun defs p ->
      (* u-values *)
      let p_ds =
        List.map
          (fun data ->
            match find_data p data with
            | Some d -> d
            | None -> assert false)
          datas in
      let t_p_ds = unify p_ds in
      let e_opt = find_p_expr (path_kind p) p_ds envs in
      match t_p_ds, e_opt with
      | _, Some (`Var _ as e) -> (p, `E e)::defs
      | `U, Some e -> (p, `E e)::defs
      | `U, None -> defs
      | _ -> (p, t_p_ds)::defs)
    [] path0 t)
and find_p_expr : kind -> data list -> data list -> expr option =
  fun k p_ds envs ->
  (* env variables *)
  let env0 = List.hd envs in
  let lv = (* should be only paths from template defining envs *)
    fold_data
      (fun res v d -> v::res)
      [] path0 env0 in
  let seq_v : path Myseq.t = Myseq.from_list lv in
  (* lists of candidate expressions *)
  let le1 : expr Myseq.t =    
    let* v = seq_v in
    if path_kind v = k
    then Myseq.return (`Var v)
    else Myseq.empty in
  let le2 : expr Myseq.t =
    match k with
    | `Int ->
       let* v = seq_v in
       if path_kind v = `Int
       then
	 let* c = Myseq.range 1 3 in
	 Myseq.cons
	   (`Plus (`Var v, `Int c))
	   (Myseq.cons
	      (`Minus (`Var v, `Int c))
	      Myseq.empty)
       else Myseq.empty
    | _ -> Myseq.empty in
  let le3 : expr Myseq.t =
    match k with
    | `Int ->
       let* v1 = seq_v in
       if path_kind v1 = `Int
       then
	 let* v2 = seq_v in
         if path_kind v2 = `Int
         then
	   Myseq.cons
	     (`Minus (`Var v1, `Var v2))
	     (Myseq.cons
		(`Plus (`Var v1, `Var v2))
		Myseq.empty)
         else Myseq.empty
       else Myseq.empty
    | _ -> Myseq.empty in
  let le = Myseq.concat [le1; le2; le3] in
  (* finding the first expression defining 'u' *)
  match
    le
    |> Myseq.find_map
	 (fun e ->
           try
	     let e_ds = List.map (fun env -> eval_expr ~env e) envs in
	     if e_ds = p_ds
	     then Some e
	     else None
           with Unbound_Var _ -> (* variable in env0 not defined through all instances *)
             None) with
  | None -> None
  | Some (e,_next) -> Some e

let defs_refinements (t : template) (gr : grids_read) : grid_refinement Myseq.t =
  match find_defs t gr with
  | [] -> Myseq.empty
  | defs -> Myseq.return (RDefs defs)
                    
let shape_refinements (t : template) : grid_refinement Myseq.t =
  let rec aux depth layers =
  (*Myseq.cons (RShape (depth, `U))*)
    Myseq.cons (RShape (depth, `Point (`U, `U)))
      (Myseq.cons (RShape (depth, `Rectangle (`U, `U, `U, `U)))
         (match layers with
          | `Nil -> Myseq.empty
          | `Cons (_,rest) -> aux (depth+1) rest
          | _ -> assert false))
  in
  match t with
  | `Background (_,_,layers) -> aux 0 layers
  | _ -> assert false
                    
let grid_refinements (t : template) (gr : grids_read) : (grid_refinement * template) Myseq.t =
  Myseq.concat
    [defs_refinements t gr;
     shape_refinements t]
  |> Myseq.map
       (fun r -> r, apply_grid_refinement r t)

let learn_grid_model ~timeout ~beam_width ~refine_degree ~env_size
      (egrids : (data * Grid.t) list)
    : ((grid_refinement * template) * grids_read * dl) list * bool =
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RGridInit, init_template)
    ~data:(fun (r,m) ->
	   Result.to_option (read_grids ~env_size m egrids))
    ~code:(fun (r,m) gr ->
	   (*pp_grid_model m; print_newline ();*)
	   pp_grid_refinement r; print_newline ();
	   let lm, ld, lmd = dl_template_data gr in
	   Printf.printf "DL = %.1f + %.1f = %.1f\n" lm ld lmd;
	   lmd)
    ~refinements:(fun (r,m) gr ->
		  grid_refinements m gr)
		     

type refinement =
  | RInit
  | Rinput of grid_refinement
  | Routput of grid_refinement

let pp_refinement = function
  | RInit -> ()
  | Rinput r -> print_string "IN  "; pp_grid_refinement r
  | Routput r -> print_string "OUT "; pp_grid_refinement r

let model_refinements (last_r : refinement) (m : model) (gri : grids_read) (gro : grids_read) : (refinement * model) Myseq.t
  = Common.prof "Model2.model_refinements" (fun () ->
  let on_input =
    match last_r with
    | RInit -> true
    | Rinput _ -> true
    | Routput _ -> false in
  let ref_defis =
    if on_input
    then
      defs_refinements m.input_pattern gri
      |> Myseq.map
           (fun r ->
             Rinput r,
             {m with input_pattern = apply_grid_refinement r m.input_pattern})
    else Myseq.empty in
  let ref_defos =
    defs_refinements m.output_template gro
    |> Myseq.map
         (fun r ->
           Routput r,
           {m with output_template = apply_grid_refinement r m.output_template}) in
  let ref_shapis =
    if on_input && grids_read_has_delta gri
    then
      shape_refinements m.input_pattern
      |> Myseq.map
	   (fun r ->
	     Rinput r,
             {m with input_pattern = apply_grid_refinement r m.input_pattern})
    else Myseq.empty in
  let ref_shapos =
    if grids_read_has_delta gro
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

let dl_model_data (gri : grids_read) (gro : grids_read) : dl triple triple =
  let lmi, ldi, lmdi = dl_template_data gri in
  let lmo, ldo, lmdo = dl_template_data gro in
  (lmi, lmo, lmi+.lmo), (ldi, ldo, ldi+.ldo), (lmdi, lmdo, lmdi+.lmdo)
  
let learn_model
      ?(verbose = true)
      ~timeout
      ~init_model
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : ((refinement * model) * (grids_read * grids_read) * dl) list * bool
  = Common.prof "Model2.learn_model" (fun () ->
  let gis = List.map (fun {input} -> input) pairs in
  let gos = List.map (fun {output} -> output) pairs in
  let egis = List.map (fun gi -> data0, gi) gis in
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RInit, init_model)
    ~data:(fun (r,m) ->
      try
        read_grid_pairs m egis gos
        |> Result.fold
             ~ok:(fun gri_gro -> Some gri_gro)
             ~error:(fun exn -> None)
      with
      | Common.Timeout as exn -> raise exn
      | exn ->
	 print_endline (Printexc.to_string exn);
	 pp_model m;
	 raise exn)
    ~code:(fun (r,m) (gri,gro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     dl_model_data gri gro in
	   if verbose then (
	     pp_refinement r; print_newline ();
	     Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;
	     flush stdout);
	   lmd)
    ~refinements:
    (fun (r,m) (gri,gro) ->
      model_refinements r m gri gro))
