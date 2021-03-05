
open Task

let alpha = ref 10.

exception TODO

(** Part 1: grids *)
        
(* type definitions for data, expressions, templates *)

type 'a triple = 'a * 'a * 'a
                
type kind =
  [ `Int | `Bool | `Color | `Mask | `Vec | `Shape | `Grid ]
        
type 'a patt =
  [ `Nil
  | `Bool of bool
  | `Int of int
  | `Color of Grid.color
  | `Mask of Grid.Mask.t option
  | `Vec of 'a * 'a (* i, j *)
  | `Point of 'a * 'a (* pos, color *)
  | `Rectangle of 'a * 'a * 'a * 'a (* pos, size, color, mask *)
  | `Background of 'a * 'a (* size, color *)
  | `AddShape of 'a * 'a (* first layer, rest of layers *)
  ]

type field = [ `I | `J | `Pos | `Color | `Size | `Mask | `First | `Rest ]
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
let template0 = `Background (`Vec (`U, `U), `U)

type delta = Grid.pixel list
let delta0 = []

type grid_data =
  { data: data;
    delta: delta }
let grid_data0 =
  { data = data0;
    delta = delta0 }
           

(* stringifiers and pretty-printing *)

let string_of_field : field -> string = function
  | `I -> "i"
  | `J -> "j"
  | `Pos -> "pos"
  | `Color -> "color"
  | `Size -> "size"
  | `Mask -> "mask"
  | `First -> "first"
  | `Rest -> "rest"

let string_of_path : path -> string =
  fun p -> String.concat "." (List.map string_of_field p)

let pp_path p = print_string (string_of_path p)

let rec string_of_patt (string : 'a -> string) : 'a patt -> string = function
  | `Nil -> "nil"
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
  | `Background (size,color) ->
     "a background with size " ^ string size
     ^ " and color " ^ string color
  | `AddShape (shape,grid) ->
     string shape ^ "\non top of " ^ string grid

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
                                         

let pp_delta delta =
  delta
  |> List.sort Stdlib.compare
  |> List.iter (fun (i,j,c) -> Printf.printf " (%d,%d)=" i j; Grid.pp_color c)
    
let pp_grid_data gd =
  print_string "data: "; pp_data gd.data; print_newline ();
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
  | `Size::p, `Background (size,color) -> find p size
  | `Color::p, `Background (size,color) -> find p color
  | `First::p, `AddShape (first,rest) -> find p first
  | `Rest::p, `AddShape (first,rest) -> find p rest
  | _ -> None

let rec find_data (p : path) (d : data) : data option =
  match p with
  | [] -> Some d
  | _ -> find_patt find_data p d

let find_data_unsafe p d =
  match find_data p d with
  | Some d1 -> d1
  | None -> assert false

let rec find_template (p : path) (t : template) : template option =
  match p, t with
  | [], _ -> Some t
  | _, (#patt as patt) -> find_patt find_template p patt
  | _ -> None

       
let fold_patt (fold : 'b -> path -> 'a -> 'b) (acc : 'b) (p : path) (patt : 'a patt) : 'b =
  match patt with
  | `Nil | `Bool _ | `Int _ | `Color _ | `Mask _ -> acc
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
  | `Background (size,color) ->
     let acc = fold acc (p ++ `Size) size in
     let acc = fold acc (p ++ `Color) color in
     acc
  | `AddShape (first,rest) ->
     let acc = fold acc (p ++ `First) first in
     let acc = fold acc (p ++ `Rest) rest in
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

let fold_unknowns (f : 'a -> path -> 'a) (init : 'a) (t : template) : 'a =
  fold_template
    (fun res p ->
      function
      | `U -> f res p
      | _ -> res)
    init path0 t

let path_kind (p : path) : kind =
  match List.rev p with
  | (`I | `J)::_ -> `Int
  | `Color::_ -> `Color
  | `Mask::_ -> `Mask
  | (`Pos | `Size)::_ -> `Vec
  | `First::_ -> `Shape
  | `Rest::_ -> `Grid
  | [] -> `Grid

(* description lengths *)

type dl = Mdl.bits
let dl0 = 0.

let dl_bool : bool -> dl =
  fun b -> 1.
let dl_nat : int -> dl =
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
let dl_mask : Grid.Mask.t option -> dl =
  function
  | None -> Mdl.Code.usage 0.5
  | Some m ->
     let n = Grid.Mask.height m * Grid.Mask.width m in
     Mdl.Code.usage 0.5
     +. Mdl.Code.uniform n
     +. Mdl.Code.comb (Grid.Mask.area m) n

        
type staged_dl (* sdl *) = Mdl.bits * (data -> Mdl.bits)
(* template DL, variable data DL (for U bindings) *)
let (+!) dl1 (dl2,f2) = (dl1+.dl2, f2)
let (+?) (dl1,f1) (dl2,f2) = (dl1+.dl2, (fun data -> f1 data +. f2 data))
let staged0 = (fun d -> 0.)
let sdl0 = 0., staged0

let sdl_patt (sdl : 'a -> path -> staged_dl) ~(env_size : int) (patt : 'a patt) (p : path) : staged_dl =
  match List.rev p, patt with
  | (`I | `J)::(`Size | `Pos)::_, `Int i -> dl_nat i, staged0

  | `Color::_, `Color c -> dl_color c, staged0

  | `Mask::_, `Mask m -> dl_mask m, staged0

  | (`Size | `Pos)::_, `Vec (i,j) ->
     sdl i (p ++ `I)
     +? sdl j (p ++ `J)

  | `First::_, `Point (pos,color) ->
     Mdl.Code.usage 0.5
     +! sdl pos (p ++ `Pos)
     +? sdl color (p ++ `Color)
  | `First::_, `Rectangle (pos,size,color,mask) ->
     Mdl.Code.usage 0.5
     +! sdl pos (p ++ `Pos)
     +? sdl size (p ++ `Size)
     +? sdl color (p ++ `Color)
     +? sdl mask (p ++ `Mask)

  | [], `Nil -> infinity, staged0
  | ([] | `Rest::_), `Background (size,color) ->
     Mdl.Code.usage 0.5
     +! sdl size (p ++ `Size)
     +? sdl color (p ++ `Color)
  | ([] | `Rest::_), `AddShape (first,rest) ->
     Mdl.Code.usage 0.5
     +! sdl first (p ++ `First)
     +? sdl rest (p ++ `Rest)

  | _ ->
     pp_path p; print_string ": ";
     print_string (string_of_patt (fun _ -> "_") patt);
     print_newline ();
     assert false

let rec sdl_data ~(env_size : int) (d : data) (p : path) : staged_dl =
  sdl_patt (sdl_data ~env_size) ~env_size d p
let dl_data (d : data) (p : path) : dl =
  fst (sdl_data ~env_size:0 d p)

let rec sdl_expr ~(env_size : int) (e : expr) (p : path) : staged_dl =
  match e with
  | `Var x ->
     Mdl.Code.usage 0.3 +. Mdl.Code.uniform env_size, (* identifying one env var *)
     staged0
  | #patt as patt ->
     Mdl.Code.usage 0.5
     +! sdl_patt (sdl_expr ~env_size) ~env_size patt p
  | _ ->
     Mdl.Code.usage 0.2
     +! (match List.rev p, e with
         | (`I | `J)::_, (`Plus (e1,e2) | `Minus (e1,e2)) ->
            Mdl.Code.usage 0.5
            +! sdl_expr ~env_size e1 p
            +? sdl_expr ~env_size e2 p
         | _ -> assert false)

let dl_expr ~env_size e p =
  fst (sdl_expr ~env_size e p)
         
let rec sdl_template ~(env_size : int) (t : template) (p : path) : staged_dl =
  match t with
  | `U ->
     Mdl.Code.usage 0.25,
     (fun d ->
       match find_data p d with
       | Some d1 -> dl_data d1 p
       | None -> assert false)
  | `E e ->
     Mdl.Code.usage 0.25 +. dl_expr ~env_size e p,
     staged0
  | #patt as patt ->
     Mdl.Code.usage 0.5
     +! sdl_patt (sdl_template ~env_size) ~env_size patt p

let dl_delta ~(height : int) ~(width : int) (delta : delta) : dl =
  let nb_pixels = List.length delta in
  Mdl.Code.universal_int_star nb_pixels (* number of delta pixels *)
  -. 1. (* some normalization to get 0 for empty grid data *)
  +. float nb_pixels
     *. (Mdl.Code.uniform height +. Mdl.Code.uniform width +. Mdl.Code.uniform Grid.nb_color)
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
  | (`Nil | `Bool _ | `Int _ | `Color _ | `Mask _ as d) -> d
  | `Vec (i,j) -> `Vec (eval i, eval j)
  | `Point (pos,color) -> `Point (eval pos, eval color)
  | `Rectangle (pos,size,color,mask) -> `Rectangle (eval pos, eval size, eval color, eval mask)
  | `Background (size,color) -> `Background (eval size, eval color)
  | `AddShape (first,rest) -> `AddShape (eval first, eval rest)
                       
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
  match t with
  | `U -> raise Unbound_U
  | #patt as p -> eval_patt (eval_template ~env) p
  | `E e -> eval_expr ~env e


(* grid generation from data and template *)

exception Invalid_data_as_grid
let _ = Printexc.register_printer
          (function
           | Invalid_data_as_grid -> Some "the data does not represent a valid grid specification"
           | _ -> None)
          
let rec grid_of_data : data -> Grid.t = function
  | `Background (`Vec (`Int h, `Int w), `Color c) ->
     Grid.make h w c
  | `AddShape (first, rest) ->
     let g = grid_of_data rest in
     draw_shape g first;
     g
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


let write_grid ~(env : data) ?(delta = delta0) (t : template) : (Grid.t, exn) Result.t = Common.prof "Model.write_grid" (fun () ->
  try
    let d = eval_template ~env t in
    let g = grid_of_data d in
    List.iter
      (fun (i,j,c) -> Grid.set_pixel g i j c)
      delta;
    Result.Ok g
  with exn -> Result.Error exn)       
       

(* parsing grids with templates *)

type parse_state = { delta: delta; mask: Grid.Mask.t; parts: Grid.part list }

let parse_const (d : data) (v : data) : data Myseq.t =
  if d = v
  then Myseq.return v
  else Myseq.empty
  
  
let parse_int ~env (t : template) (i : int) : data Myseq.t =
  match t with
  | `U -> Myseq.return (`Int i)
  | `E e -> parse_const (eval_expr ~env e) (`Int i)
  | `Int _ as d -> parse_const d (`Int i)
  | _ -> Myseq.empty
  
let parse_color ~env (t : template) (c : Grid.color) : data Myseq.t =
  match t with
  | `U -> Myseq.return (`Color c)
  | `E e -> parse_const (eval_expr ~env e) (`Color c)
  | `Color _ as d -> parse_const d (`Color c)
  | _ -> Myseq.empty
  
let parse_mask ~env (t : template) (m : Grid.Mask.t option) : data Myseq.t =
  match t with
  | `U -> Myseq.return (`Mask m)
  | `E e -> parse_const (eval_expr ~env e) (`Mask m)
  | `Mask _ as d -> parse_const d (`Mask m)
  | _ -> Myseq.empty
  
let parse_vec ~env (t : template) (vi,vj : int * int) : data Myseq.t =
  let d = `Vec (`Int vi, `Int vj) in
  match t with
  | `U -> Myseq.return d
  | `E e -> parse_const (eval_expr ~env e) d
  | `Vec (i,j) ->
     Myseq.product
       [parse_int ~env i vi;
        parse_int ~env j vj]
     |> Myseq.flat_map
          (function
           | [res_i; res_j] ->
              Myseq.return (`Vec (res_i,res_j))
           | _ -> Myseq.empty)
  | _ -> Myseq.empty

    
let parse_shape_gen g state
      ~(get_occs : Grid.t -> Grid.Mask.t -> Grid.part list -> 'a list)
      ~(matcher : 'a -> (data * delta * Grid.Mask.t) Myseq.t)
    : (data * parse_state) Myseq.t =
  let occs = Common.prof "Model.parse_shape_gen/get_occs" (fun () -> get_occs g state.mask state.parts) in
  Myseq.from_list occs
  |> Myseq.flat_map
       (fun occ ->
	 matcher occ
         |> Myseq.flat_map
	      (fun (res_shape, occ_delta, occ_mask) ->
                let new_mask = Grid.Mask.diff state.mask occ_mask in
                let new_state =
                  { delta = occ_delta @ state.delta;
	            mask = new_mask;
	            parts =
	              List.filter
		        (fun p ->
		          not (Grid.Mask.is_empty
			         (Grid.Mask.inter p.Grid.pixels new_mask)))
		        state.parts } in
	        Myseq.return (res_shape, new_state)))

                
let rec parse_grid ~(env : data) (t : template) (g : Grid.t) (state : parse_state)
        : (data * parse_state) Myseq.t =
  match t with
  | `Background (size,color) ->
     parse_vec ~env size (g.height,g.width)
     |> Myseq.flat_map
          (fun res_size ->
            let bc = (* background color *)
              match color with
              | `Color c -> c
              | `E e ->
                 (match eval_expr ~env e with
                  | `Color c -> c
                  | _ -> assert false)
              | `U ->
                 (* determining the majority color *)
	         let color_counter = new Common.counter in
	         Grid.Mask.iter
	           (fun i j -> color_counter#add g.matrix.{i,j})
	           state.mask;
	         (match color_counter#most_frequents with
	         | _, bc::_ -> bc
	         | _ -> Grid.black)
              | _ -> assert false in
            let res = `Background (res_size, `Color bc) in
	    (* adding mask pixels with other color than background to delta *)
            let new_delta = ref state.delta in
	    Grid.Mask.iter
	      (fun i j ->
	        if g.matrix.{i,j} <> bc then
	          new_delta := (i,j, g.matrix.{i,j})::!new_delta)
	      state.mask;
            let new_state = { delta = (!new_delta);
	                      mask = Grid.Mask.empty g.height g.width;
	                      parts = [] } in
	    Myseq.return (res, new_state))
  | `AddShape (first,rest) ->
     parse_shape ~env first g state
     |> Myseq.flat_map
          (fun (res_first, state) ->
            parse_grid ~env rest g state
            |> Myseq.flat_map
                 (fun (res_rest,state) ->
                   let res = `AddShape (res_first, res_rest) in
                   Myseq.return (res, state)))
  | _ -> Myseq.empty
and parse_shape ~(env : data) (t : template) (g : Grid.t) (state : parse_state) : (data * parse_state) Myseq.t =
  match t with
  | `Point (pos,color) ->
     parse_shape_gen g state
       ~get_occs:Grid.points
       ~matcher:(fun (i,j,c) ->
         Myseq.product
           [parse_vec ~env pos (i,j);
            parse_color ~env color c]
         |> Myseq.flat_map
              (function
               | [res_pos; res_color] ->
                  let res = `Point (res_pos, res_color) in
                  Myseq.return (res, [], Grid.Mask.singleton g.height g.width i j)
               | _ -> assert false))
  | `Rectangle (pos,size,color,mask) ->
     parse_shape_gen g state
       ~get_occs:Grid.rectangles
       ~matcher:(fun r ->
         Myseq.product
           [parse_vec ~env pos (r.offset_i, r.offset_j);
            parse_vec ~env size (r.Grid.height, r.width);
            parse_color ~env color r.color;
            parse_mask ~env mask r.rmask]
         |> Myseq.flat_map
              (function
               | [res_pos; res_size; res_color; res_mask] ->
                  let res = `Rectangle (res_pos, res_size, res_color, res_mask) in
                  Myseq.return (res, r.delta, r.mask)
               | _ -> assert false))
  | _ -> Myseq.empty

exception Parse_failure
let _ = Printexc.register_printer
          (function
           | Parse_failure -> Some "the grid could not be parsed with the given template"
           | _ -> None)

let read_grid_aux ~(env : data) (t : template) (g : Grid.t)
      ~(dl_grid_data : data -> dl)
    : (data * grid_data * dl, exn) Result.t = Common.prof "Model.read_grid_aux" (fun () ->
  let state = { delta = delta0;
                mask = Grid.Mask.full g.height g.width;
                parts = Grid.segment_by_color g } in
  let _, res =
    parse_grid ~env t g state
    |> Myseq.fold_left
         (fun (dl_min,res) (data,state) ->
           let dl_data = dl_grid_data data in
           let dl_delta = dl_delta ~height:g.height ~width:g.width state.delta in
           let dl = dl_data +. dl_delta in
           if dl < dl_min
           then dl, Result.Ok (env, {data; delta=state.delta}, dl)
           else dl_min, res)
         (infinity, Result.Error Parse_failure) in
  res)

let read_grid ~(env : data) (t : template) (g : Grid.t) : (grid_data, exn) Result.t =
  let env_size = size_of_data env in
  let dl_m, dl_grid_data = sdl_template ~env_size t path0 in
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
		    
let read_grids (t : template) (egrids: (data * Grid.t) list) : (grids_read, exn) Result.t =
  let env_size =
    match egrids with
    | (env,_)::_ -> size_of_data env
    | _ -> assert false in
  let dl_m, dl_grid_data = sdl_template ~env_size t path0 in
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
let model0 =
  { input_pattern = template0;
    output_template = template0 }

let pp_model m =
  print_endline "CONSTRUCT (Mo)";
  pp_template m.output_template; print_newline ();
  print_endline "WHERE (Mi)";
  pp_template m.input_pattern; print_newline ()

let apply_model ?(env = data0) (m : model) (g : Grid.t) : (Grid.t, exn) Result.t = Common.prof "Model.apply_model" (fun () ->
  Result.bind
    (read_grid ~env m.input_pattern g)
    (fun gdi ->
      write_grid ~env:gdi.data m.output_template))

(* template transformations *)
                                                                    
let rec map_template (f : path -> template -> template) (p : path) (t : template) : template =
  match t with
  | `U -> f p t
  | `E e -> f p t
  | `Nil | `Bool _ | `Int _ | `Color _ | `Mask _ -> f p t
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
  | `Background (size,color) ->
     let size = map_template f (p ++ `Size) size in
     let color = map_template f (p ++ `Color) color in
     f p (`Background (size,color))
  | `AddShape (first,rest) ->
     let first = map_template f (p ++ `First) first in
     let rest = map_template f (p ++ `Rest) rest in
     f p (`AddShape (first,rest))
                                                                               
(* model refinements and learning *)

type grid_refinement =
  | RGridInit
  | RDefs of (path * expr) list
  | RShape of int (* depth *) * template (* shape *)

let pp_grid_refinement = function
  | RGridInit -> ()
  | RDefs defs ->
     print_string "DEFS:";
     List.iter
       (fun (p,e) ->
         print_string "  "; pp_path p;
         print_string "="; pp_expr e)
       defs
  | RShape (depth,sh) ->
     Printf.printf "SHAPE (depth=%d): " depth;
     pp_template sh

let apply_grid_refinement (r : grid_refinement) (t : template) : template =
  match r with
  | RGridInit -> t
  | RDefs defs ->
     map_template
       (fun p1 t1 ->
         match t1 with
         | `U ->
            (match List.assoc_opt p1 defs with
             | Some e1 -> `E e1
             | None -> t1)
         | _ -> t1)
       path0 t
  | RShape (depth, shape) ->
     let p = List.init depth (fun _ -> `Rest) in
     map_template
       (fun p1 t1 ->
         if p1 = p
         then `AddShape (shape, t1)
         else t1)
       path0 t
     
let rec find_defs (t : template) (gr : grids_read) : (path * expr) list =
  (* find if some gd param unknowns can be replaced
     by some expression over env vars *)
  assert (gr.egdls <> []);
  let envs = List.map (fun (env,_,_) -> env) gr.egdls in
  let datas = List.map (fun (_,gd,_) -> gd.data) gr.egdls in
  let atomic_unknowns =
    fold_template
      (fun res p1 t1 ->
        match List.rev p1, t1 with
        | (`I | `J | `Color | `Mask)::_, `U -> p1::res
        | _ -> res)
      [] path0 t in
  atomic_unknowns
  |> List.fold_left
       (fun defs p ->
         (* u-values *)
         let p_ds =
           List.map
             (fun data -> find_data_unsafe p data)
             datas in
         match p_ds with
         | [] -> assert false
         | d0::ds1 -> (* constants first *)
	    if List.for_all ((=) d0) ds1
	    then (p, (d0 :> expr))::defs
	    else (* then expressions *)
	      match find_defs_p (path_kind p) p_ds envs with
	      | None -> defs
	      | Some e -> (p,e)::defs)
       []
and find_defs_p : kind -> data list -> data list -> expr option =
  fun k p_ds envs ->
  (* env variables *)
  let env0 = List.hd envs in
  let lv =
    fold_data
      (fun res v d -> v::res)
      [] path0 env0 in
  let seq_v : path Myseq.t = Myseq.from_list lv in
  (* lists of candidate expressions *)
  let le1 : expr Myseq.t =    
    seq_v
    |> Myseq.filter_map
         (fun v ->
           if path_kind v = k
           then Some (`Var v)
           else None) in
  let le2 : expr Myseq.t =
    match k with
    | `Int ->
       seq_v
       |> Myseq.flat_map
	    (fun v ->
              if path_kind v = `Int
              then
	        Myseq.range 1 3
	        |> Myseq.flat_map
		     (fun c ->
		       Myseq.cons
		         (`Plus (`Var v, `Int c))
		         (Myseq.cons
			    (`Minus (`Var v, `Int c))
			    Myseq.empty))
              else Myseq.empty)
    | _ -> Myseq.empty in
  let le3 : expr Myseq.t =
    match k with
    | `Int ->
       seq_v
       |> Myseq.flat_map
	    (fun v1 ->
              if path_kind v1 = `Int
              then
	        seq_v
	        |> Myseq.flat_map
		     (fun v2 ->
                       if path_kind v2 = `Int
                       then
		         Myseq.cons
		           (`Minus (`Var v1, `Var v2))
		           (Myseq.cons
			      (`Plus (`Var v1, `Var v2))
			      Myseq.empty)
                       else Myseq.empty)
              else Myseq.empty)
    | _ -> Myseq.empty in
  let le = Myseq.concat [le1; le2; le3] in
  (* finding the first expression defining 'u' *)
  match
    le
    |> Myseq.find_map
	 (fun e ->
	  let e_ds = List.map (fun env -> eval_expr ~env e) envs in
	  if e_ds = p_ds
	  then Some e
	  else None) with
  | None -> None
  | Some (e,_next) -> Some e

let defs_refinements (t : template) (gr : grids_read) : grid_refinement Myseq.t =
  match find_defs t gr with
  | [] -> Myseq.empty
  | defs -> Myseq.return (RDefs defs)
                    
let rec shape_refinements ?(depth = 0) (t : template) : grid_refinement Myseq.t =
  Myseq.cons (RShape (depth, `Point (`Vec (`U, `U), `U)))
    (Myseq.cons (RShape (depth, `Rectangle (`Vec (`U, `U), `Vec (`U, `U), `U, `U)))
       (match t with
        | `Background _ -> Myseq.empty
        | `AddShape (_,rest) ->
           shape_refinements ~depth:(depth+1) rest
        | _ -> assert false))
                    
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
    ~m0:(RGridInit, template0)
    ~data:(fun (r,m) ->
	   Result.to_option (read_grids m egrids))
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
  = Common.prof "Model.model_refinements" (fun () ->
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
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : ((refinement * model) * (grids_read * grids_read) * dl) list * bool
  = Common.prof "Model.learn_model" (fun () ->
  let gis = List.map (fun {input} -> input) pairs in
  let gos = List.map (fun {output} -> output) pairs in
  let egis = List.map (fun gi -> data0, gi) gis in
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RInit, model0)
    ~data:(fun (r,m) ->
	   try
	   match read_grids m.input_pattern egis with
	   | Result.Error _ -> None
	   | Result.Ok gri ->
	      let egos =
		List.map2
		  (fun (envi,gdi,li) go -> gdi.data, go)
		  gri.egdls gos in
	      match read_grids m.output_template egos with
	      | Result.Error _ -> None
	      | Result.Ok gro ->
		 Some (gri, gro)
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

  
let x = 1
          
