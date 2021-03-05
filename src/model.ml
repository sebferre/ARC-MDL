
open Task

let alpha = ref 10.

exception TODO
             
(* data and paths *)

type field = string
           
type path = field list

let path0 = []

let string_of_path p = String.concat "." p
          
let pp_path p = print_string (string_of_path p)

let (++) p f = p @ [f]

type data =
  | Bool of bool
  | Int of int
  | Color of Grid.color
  | Mask of Grid.Mask.t option
  | Struct of (field * data) list

let data0 = Struct []

let as_bool : data -> bool = function
  | Bool b -> b
  | _ -> assert false
let as_int : data -> int = function
  | Int i -> i
  | _ -> assert false
let as_color : data -> Grid.color = function
  | Color c -> c
  | _ -> assert false
let as_mask : data -> Grid.Mask.t option = function
  | Mask m -> m
  | _ -> assert false
let as_fields : data -> (field * data) list = function
  | Struct fields -> fields
  | _ -> assert false

       
let rec pp_data : data -> unit = function
  | Bool b -> print_string (if b then "true" else "false")
  | Int i -> print_int i
  | Color c -> Grid.pp_color c
  | Mask m_opt ->
     (match m_opt with
      | None -> print_string "full"
      | Some m -> Grid.Mask.pp m)
  | Struct fields ->
     print_string "{ ";
     List.iter
       (fun (f,d) ->
         print_string f;
         print_string ": ";
         pp_data d;
         print_string ", ")
       fields;
     print_string "}"

let equal_data (d1 : data) (d2 : data) : bool =
  d1 = d2 (* assuming structure fields are always in same order *)

let rec same_type_data (d1 : data) (d2 : data) : bool =
  match d1, d2 with
  | Bool _, Bool _ -> true
  | Int _, Int _ -> true
  | Color _, Color _ -> true
  | Mask _, Mask _ -> true
  | Struct fields1, Struct fields2 ->
     same_type_fields fields1 fields2
  | _ -> false
and same_type_fields fs1 fs2 =
  match fs1, fs2 with
  | [], [] -> true
  | (f1,d1)::gs1, (f2,d2)::gs2 ->
     f1 = f2 && same_type_data d1 d2 && same_type_fields gs1 gs2
  | _ -> false
  
let rec get_path  (p : path) (d : data) : data option =
  match p, d with
  | [], _ -> Some d
  | f::p1, Struct fields ->
     ( match List.assoc_opt f fields with
       | None -> None
       | Some d1 -> get_path p1 d1 )
  | _ -> None

let fold_data (f : 'a -> path -> data -> 'a) (init : 'a) (p : path) (d : data) : 'a =
  let rec aux res p d =
    match d with
    | Bool _ | Int _ | Color _ | Mask _ -> f res p d
    | Struct fields ->
       List.fold_left
         (fun res (f1,d1) -> aux res (p ++ f1) d1)
         res fields
  in
  aux init p d

let env_size_of_data (d : data) : int =
  fold_data (fun res p _ -> res+1) 0 path0 d
           
       
type expr =
  | Var of path (* assuming some context data *)
  | Const of data
  | Plus of expr * expr
  | Minus of expr * expr
  | If of expr * expr * expr

exception Unbound_Var of path
        
let rec pp_expr : expr -> unit = function
  | Var p -> pp_path p
  | Const d -> pp_data d
  | Plus (e1,e2) -> pp_expr e1; print_string " + "; pp_expr e2
  | Minus (e1,e2) -> pp_expr e1; print_string " - "; pp_expr e2
  | If (cond,e1,e2) ->
     print_string "if "; pp_expr cond;
     print_string " then "; pp_expr e1;
     print_string " else "; pp_expr e2

type def = path * expr
type defs = def list (* for substitutions *)

let pp_defs (defs : defs) : unit =
  List.iter
    (fun (p,e) ->
      print_string "  ";
      pp_path p;
      print_char '=';
      pp_expr e)
    defs

let rec subst_expr (defs : defs) (e : expr) : expr =
  match e with
  | Var p ->
     ( match List.assoc_opt p defs with
       | Some e0 -> e0
       | None -> e )
  | Const _ -> e
  | Plus (e1,e2) ->
     Plus (subst_expr defs e1,
	   subst_expr defs e2)
  | Minus (e1,e2) ->
     Minus (subst_expr defs e1,
	    subst_expr defs e2)
  | If (cond,e1,e2) ->
     If (subst_expr defs cond,
	 subst_expr defs e1,
	 subst_expr defs e2)

let rec eval_expr (env : data) : expr -> data = function
  | Var p ->
     ( match get_path p env  with
       | Some d -> d
       | None -> raise (Unbound_Var p) )
  | Const d -> d
  | Plus (e1,e2) -> Int (as_int (eval_expr env e1) + as_int (eval_expr env e2))
  | Minus (e1,e2) -> Int (as_int (eval_expr env e1) - as_int (eval_expr env e2))
  | If (cond,e1,e2) ->
     if as_bool (eval_expr env cond)
     then eval_expr env e1
     else eval_expr env e2

let eval_var (env : data) (p : path) : data =
  eval_expr env (Var p)
    
type attr =
  | U
  | E of expr

type path_attr = path * attr

exception Unbound_U of path

let pp_attr : attr -> unit = function
  | U -> print_char '?'
  | E e -> pp_expr e

let subst_attr (defs : defs) (p, a : path_attr) : attr =
  match a with
  | U ->
     ( match List.assoc_opt p defs with
       | Some e -> E e
       | None -> U )
  | E e -> a
(* E (subst_expr defs e) (* TODO: is substitution really useful here ? *) *)

let eval_unknown params p =
  match get_path p params with
  | Some d -> d
  | None -> raise (Unbound_U p)

let eval_attr (env : data) (params : data) (p, a : path_attr) : data =
  match a with
  | U -> eval_unknown params p
  | E e -> eval_expr env e

         
(* grid models *)
(* ----------- *)

let p_h = "height"
let p_w = "width"
let p_c = "color"
let p_i = "i"
let p_j = "j"
let p_rm = "mask"
let p_top = "top"
let p_down = "down"

type grid_model =
  | Background of { height: attr; (* one-color background *)
		    width: attr;
		    color: attr }
  | AddShape of shape * grid_model (* shape on top of grid_model *)
and shape =
  | Point of { offset_i: attr;
	       offset_j: attr;
	       color: attr }
  | Rectangle of { height: attr;
		   width: attr;
		   offset_i: attr;
		   offset_j: attr;
		   color: attr;
		   rmask: attr }

let rec pp_grid_model = function
  | Background {height; width; color} ->
     print_string "a background with height=";
     pp_attr height; print_string " x width="; pp_attr width;
     print_string " with color=";
     pp_attr color
  | AddShape (sh,m1) ->
     pp_shape sh;
     print_string "\non top of ";
     pp_grid_model m1
and pp_shape = function
  | Point {offset_i; offset_j; color} ->
     print_string "a point at (i=";
     pp_attr offset_i; print_string ", j="; pp_attr offset_j;
     print_string ") with color="; pp_attr color
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     print_string "a rectangle with height=";
     pp_attr height; print_string " x width="; pp_attr width;
     print_string " at (i="; pp_attr offset_i;
     print_string ", j="; pp_attr offset_j;
     print_string ") with color="; pp_attr color;
     print_string " and mask="; pp_attr rmask

let rec unknowns_grid_model (m : grid_model) : path list =
  unknowns_grid_model_aux [] path0 m
and unknowns_grid_model_aux acc p =
  function
  | Background {height; width; color} ->
     let acc = unknowns_attr acc (p ++ p_h, height) in
     let acc = unknowns_attr acc (p ++ p_w, width) in
     let acc = unknowns_attr acc (p ++ p_c, color) in
     acc
  | AddShape (sh,m1) ->
     let acc = unknowns_shape acc (p ++ p_top) sh in
     let acc =unknowns_grid_model_aux acc (p ++ p_down) m1 in
     acc
and unknowns_shape acc p =
  function
  | Point {offset_i; offset_j; color} ->
     let acc = unknowns_attr acc (p ++ p_i, offset_i) in
     let acc = unknowns_attr acc (p ++ p_j, offset_j) in
     let acc = unknowns_attr acc (p ++ p_c, color) in
     acc
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     let acc = unknowns_attr acc (p ++ p_h, height) in
     let acc = unknowns_attr acc (p ++ p_w, width) in
     let acc = unknowns_attr acc (p ++ p_i, offset_i) in
     let acc = unknowns_attr acc (p ++ p_j, offset_j) in
     let acc = unknowns_attr acc (p ++ p_c, color) in
     let acc = unknowns_attr acc (p ++ p_rm, rmask) in
     acc
and unknowns_attr acc (p,a) =
  match a with
  | U -> p::acc
  | E _ -> acc

     
let rec subst_grid_model (defs: defs) (m : grid_model) : grid_model =
  subst_grid_model_aux defs path0 m
and subst_grid_model_aux defs p : grid_model -> grid_model =
  function
  | Background {height; width; color} ->
     Background {height = subst_attr defs (p ++ p_h, height);
		 width = subst_attr defs (p ++ p_w, width);
		 color = subst_attr defs (p ++ p_c, color)}
  | AddShape (sh,m1) ->
     AddShape (subst_shape defs (p ++ p_top) sh,
	       subst_grid_model_aux defs (p ++ p_down) m1)
and subst_shape (defs : defs) (p : path) : shape -> shape =
  function
  | Point {offset_i; offset_j; color} ->
     Point {offset_i = subst_attr defs (p ++ p_i, offset_i);
	    offset_j = subst_attr defs (p ++ p_j, offset_j);
	    color = subst_attr defs (p ++ p_c, color)}
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     Rectangle {height = subst_attr defs (p ++ p_h, height);
		width = subst_attr defs (p ++ p_w, width);
		offset_i = subst_attr defs (p ++ p_i, offset_i);
		offset_j = subst_attr defs (p ++ p_j, offset_j);
		color = subst_attr defs (p ++ p_c, color);
		rmask = subst_attr defs (p ++ p_rm, rmask)}

type env = data
let env0 = data0
    
type params = data
let params0 = data0
            
type delta = Grid.pixel list
let delta0 = []
			
type grid_data =
  { params: params;
    delta: delta }
let grid_data0 =
  { params = data0;
    delta = delta0 }
    
let pp_params params =
  pp_data params;
  print_newline ()
    
let pp_delta delta =
  delta
  |> List.sort Stdlib.compare
  |> List.iter (fun (i,j,c) -> Printf.printf " (%d,%d)=" i j; Grid.pp_color c);
  print_newline ()
    
let pp_grid_data gd =
  print_string "params: "; pp_params gd.params;
  print_string "delta:"; pp_delta gd.delta

(* input->output models *)    
type model =
  { input_pattern : grid_model; (* only consts and unknowns allowed *)
    output_template : grid_model (* complex expressions allowed *)
  }

let pp_model m =
  print_endline "CONSTRUCT (Mo)";
  pp_grid_model m.output_template; print_newline ();
  print_endline "WHERE (Mi)";
  pp_grid_model m.input_pattern; print_newline ()

					       
(* description lengths *)

type 'a code = 'a -> Mdl.bits

let void_code : 'a code = fun _ -> 0.

let code_bool : bool code = fun _ -> 1.

let code_color : Grid.color code =
  fun _ -> Mdl.Code.uniform Grid.nb_color
let code_background_color : Grid.color code =
  function
  | 0 -> Mdl.Code.usage 0.91
  | c ->
     if c > 0 && c < 10 (* 9 colors *)
     then Mdl.Code.usage 0.01
     else invalid_arg "Unexpected color"

let code_mask : Grid.Mask.t option code =
  function
  | None -> 1.
  | Some m ->
     let n = Grid.Mask.height m * Grid.Mask.width m in
     1. +. Mdl.Code.uniform n +. Mdl.Code.comb (Grid.Mask.area m) n

let rec code_data : data code =
  function
  | Bool b -> code_bool b
  | Int i -> Mdl.Code.universal_int_plus i
  | Color c -> code_color c
  | Mask m_opt -> code_mask m_opt
  | Struct fields -> (* assuming the fields are known and ordered *)
     Mdl.sum fields (fun (f,d) -> code_data d)

    
type staged_code = Mdl.bits (* model proper *) * (env -> params -> Mdl.bits) (* env/params dependent *)

let no_staged_code = fun env params -> 0.
let (+!) l1 (l2,c2) = (l1+.l2, c2)
let (+?) (l1,c1) (l2,c2) = (l1+.l2, (fun env params -> c1 env params +. c2 env params))

let env_size_of_exs exs =
  match exs with
  | (env,_)::_ -> env_size_of_data env
  | _ -> assert false
			     
let rec l_expr : env_size:int -> code:data code -> expr -> Mdl.bits =
  fun ~env_size ~code e ->
  match e with
  | Var p ->
     Mdl.Code.usage 0.3
     +. Mdl.Code.uniform env_size (* identifying one env var *)
  | Const d ->
     Mdl.Code.usage 0.5 +. code d
  | Plus (e1,e2) ->
     Mdl.Code.usage 0.1
     +. l_expr ~env_size ~code e1
     +. l_expr ~env_size ~code e2
  | Minus (e1,e2) ->
     Mdl.Code.usage 0.1
     +. l_expr ~env_size ~code e1
     +. l_expr ~env_size ~code e2
  | _ -> raise TODO

let l_attr_gen : env_size:int -> path_attr -> code:data code -> u_code:(env -> params -> data code) -> staged_code =
  fun ~env_size (p,a) ~code ~u_code ->
  match a with
  | U -> Mdl.Code.usage 0.5, (fun env params -> u_code env params (eval_unknown params p))
  | E e -> Mdl.Code.usage 0.5 +. l_expr ~env_size ~code e, no_staged_code

let l_attr ~env_size pa =
  l_attr_gen ~env_size pa ~code:code_data ~u_code:(fun env params -> code_data)
  
(* coding offset given bound *)
let l_position ~env_size ~(bound : path_attr) (offset: path_attr) : staged_code =
  l_attr_gen ~env_size offset
    ~code:(fun o ->
      match snd bound with
      | E (Const (Int b)) -> Mdl.Code.uniform b
      | _ -> Mdl.Code.universal_int_star (as_int o))
    ~u_code:(fun env params _o ->
      let b = eval_attr env params bound in
      Mdl.Code.uniform (as_int b))
  
(* coding offset and size given bound *)
let l_slice ~env_size ~(bound : path_attr) ~(offset : path_attr) ~(size: path_attr) : staged_code =
  l_position ~env_size ~bound offset
  +? l_attr_gen ~env_size size
       ~code:(fun s ->
	 match snd bound, snd offset with
	 | E (Const (Int b)), E (Const (Int o)) -> Mdl.Code.uniform (b - o)
	 | E (Const (Int b)), _ -> Mdl.Code.uniform b
	 | _, _ -> Mdl.Code.universal_int_plus (as_int s))
       ~u_code:(fun env params _s ->
	 let b = eval_attr env params bound in
	 let o = eval_attr env params offset in
	 Mdl.Code.uniform (as_int b - as_int o))
	    
let l_background_color ~env_size (pa : path_attr) : staged_code =
  l_attr_gen ~env_size pa
    ~code:(fun c -> code_background_color (as_color c))
    ~u_code:(fun env params c -> code_background_color (as_color c))

let rec l_grid_model ~(env_size : int) (* nb of env vars *) (m : grid_model) : Mdl.bits * (env -> grid_data -> Mdl.bits) =
  let (l_m, code_m), (phe, pwe) = l_grid_model_aux ~env_size path0 m in
  let code_grid_data =
    fun env gd ->
    let h, w, nb_pixels = (* QUICK *)
      as_int (eval_attr env gd.params phe),
      as_int (eval_attr env gd.params pwe),
      List.length gd.delta in
    (* encoding parameters according to model *)
    Common.prof "Model.l_grid_model/code_m" (fun () -> code_m env gd.params)
    (* encoding delta pixels *)
    +. Mdl.Code.universal_int_star nb_pixels (* number of delta pixels *)
    -. 1. (* some normalization to get 0 for empty grid data *)
    +. float nb_pixels
       *. (Mdl.Code.uniform h +. Mdl.Code.uniform w +. Mdl.Code.uniform Grid.nb_color) in
(* NOT using optimized DL below for fair comparisons with model points: 
  +. Mdl.Code.comb nb_pixels area (* where they are *)
  +. float nb_pixels *. Mdl.Code.uniform (Grid.nb_color - 1) (* what are their color, different from the color generated by the model *) *)
  l_m, code_grid_data
and l_grid_model_aux ~env_size p =
  function
  | Background {height; width; color} ->
     let phe = p ++ p_h, height in
     let pwe = p ++ p_w, width in
     let l_code =
       Mdl.Code.usage 0.5
       +! l_attr ~env_size phe
       +? l_attr ~env_size pwe
       +? l_background_color ~env_size (p ++ p_c, color) in
     l_code, (phe,pwe)
  | AddShape (sh,m1) ->
     let l_code1, hw1 = l_grid_model_aux ~env_size (p ++ p_down) m1 in
     let l_code =
       Mdl.Code.usage 0.5
       +! l_code1
       +? l_shape ~env_size ~hw:hw1 (p ++ p_top) sh in
     l_code, hw1
and l_shape ~(env_size : int) ~(hw : path_attr * path_attr) (p : path) : shape -> staged_code =
  let h, w = hw in
  function
  | Point {offset_i; offset_j; color} ->
     Mdl.Code.usage 0.5
     +! l_position ~env_size ~bound:h (p ++ p_i, offset_i)
     +? l_position ~env_size ~bound:w (p ++ p_j, offset_j)
     +? l_attr ~env_size (p ++ p_c, color)
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     Mdl.Code.usage 0.5
     +! l_slice ~env_size ~bound:h ~size:(p ++ p_h, height) ~offset:(p ++ p_i, offset_i)
     +? l_slice ~env_size ~bound:w ~size:(p ++ p_w, width) ~offset:(p ++ p_j, offset_j)
     +? l_attr ~env_size (p ++ p_c, color)
     +? l_attr ~env_size (p ++ p_rm, rmask)


(* result of reading a grid: grid data and description lengths *)
type grids_read = { l_m : Mdl.bits;
		    egdls : (env * grid_data * Mdl.bits) list }

let grids_read_has_delta (gr : grids_read) : bool =
  List.for_all
    (fun (env,gd,l) -> gd.delta <> [])
    gr.egdls
		    

type 'a triple = 'a * 'a * 'a
		  
let l_grid_model_data (gr : grids_read) : Mdl.bits triple (* model, data, model+data *) = Common.prof "Model.l_grid_model_data" (fun () ->
  let l_d =
    !alpha (* because given training examples are only a sample from a class of grids *)
    *. Mdl.sum gr.egdls (fun (env,gd,l) -> l) in
  gr.l_m, l_d, gr.l_m +. l_d)
 
let l_model_data (gri : grids_read) (gro : grids_read) : Mdl.bits triple triple =
  let lmi, ldi, lmdi = l_grid_model_data gri in
  let lmo, ldo, lmdo = l_grid_model_data gro in
  (lmi, lmo, lmi+.lmo), (ldi, ldo, ldi+.ldo), (lmdi, lmdo, lmdi+.lmdo)

(* writing grids from models *)
    
let rec apply_grid_model (p : path) (m : grid_model) (env : env) (params : params) : Grid.t =
  match m with
  | Background { height; width; color } ->
     Grid.make
       (as_int @@ eval_attr env params (p ++ p_h, height))
       (as_int @@ eval_attr env params (p ++ p_w, width))
       (as_color @@ eval_attr env params (p ++ p_c, color))
  | AddShape (sh,m1) ->
     let g = apply_grid_model (p ++ p_down) m1 env params in
     apply_shape (p ++ p_top) sh env params g;
     g
and apply_shape (p : path) (sh : shape) env params g : unit =
  match sh with
  | Point {offset_i; offset_j; color} ->
     let i = as_int (eval_attr env params (p ++ p_i, offset_i)) in
     let j = as_int (eval_attr env params (p ++ p_j, offset_j)) in
     let c = as_color (eval_attr env params (p ++ p_c, color)) in
     Grid.set_pixel g i j c
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     let h = as_int (eval_attr env params (p ++ p_h, height)) in
     let w = as_int (eval_attr env params (p ++ p_w, width)) in
     let mini = as_int (eval_attr env params (p ++ p_i, offset_i)) in
     let minj = as_int (eval_attr env params (p ++ p_j, offset_j)) in
     let c = as_color (eval_attr env params (p ++ p_c, color)) in
     let m_opt = as_mask (eval_attr env params (p ++ p_rm, rmask)) in
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
	   
let write_grid (m : grid_model) (env : env) (d : grid_data) : Grid.t = Common.prof "Model.write_grid" (fun () ->
  let g = apply_grid_model path0 m env d.params in
  List.iter
    (fun (i,j,c) -> Grid.set_pixel g i j c)
    d.delta;
  g)


(* reading grids with models *)

let parse_const : data -> data -> params Myseq.t =
  fun d0 d ->
  if equal_data d d0
  then Myseq.return d
  else Myseq.empty
     
let parse_attr : env -> data -> attr -> params Myseq.t =
  fun env d0 a ->
  match a with
  | U -> Myseq.return d0
  | E e ->
     let d = eval_expr env e in
     parse_const d0 d

let rec parse_attr_list : env -> (data * attr) list -> params list Myseq.t =
  fun env lca ->
  match lca with
  | [] -> Myseq.return []
  | (d,a)::lca1 ->
     parse_attr env d a
     |> Myseq.flat_map
          (fun params ->
            parse_attr_list env lca1
            |> Myseq.flat_map
                 (fun l_params ->
                   Myseq.return (params :: l_params)))

let parse_shape_gen
      ~(get_occs : Grid.t -> Grid.Mask.t -> Grid.part list -> 'a list)
      ~(matcher : env -> delta -> Grid.Mask.t -> 'a -> (params * delta * Grid.Mask.t) Myseq.t)
      g env delta mask parts kont
    : (params * delta * Grid.Mask.t * Grid.part list) Myseq.t =
  let occs = Common.prof "Model.parse_shape_gen/get_occs" (fun () -> get_occs g mask parts) in
  Myseq.from_list occs
  |> Myseq.flat_map
       (fun occ ->
	 (matcher env delta mask occ)
         |> Myseq.flat_map
	      (fun (shape_params, occ_delta, occ_mask) ->
	        let new_delta = occ_delta @ delta in
	        let new_mask = Grid.Mask.diff mask occ_mask in
	        let new_parts =
	          List.filter
		    (fun p ->
		      not (Grid.Mask.is_empty
			     (Grid.Mask.inter p.Grid.pixels new_mask)))
		    parts in
	        kont (shape_params, new_delta, new_mask, new_parts)))
  
let rec parse_grid_model
	  (g : Grid.t) (m : grid_model) (env : env)
	  (delta : delta) (mask : Grid.Mask.t) (parts : Grid.part list)
	: (params * delta * Grid.Mask.t * Grid.part list) Myseq.t = Common.prof "Model.parse_grid_model" (fun () ->
  match m with
  | Background {height; width; color} ->
     parse_attr_list env
       [Int g.height, height;
        Int g.width, width]
     |> Myseq.flat_map
          (function
           | [params_height; params_width] ->
	      let new_delta = ref delta in
	      let bc = (* background color *)
	        match color with
	        | E e -> as_color (eval_expr env e)
	        | U ->
	           (* determining the majority color *)
	           let color_counter = new Common.counter in
	           Grid.Mask.iter
	             (fun i j -> color_counter#add g.matrix.{i,j})
	             mask;
	           match color_counter#most_frequents with
	           | _, bc::_ -> bc
	           | _ -> Grid.black in
	      (* adding mask pixels with other color than background to delta *)
	      Grid.Mask.iter
	        (fun i j ->
	          if g.matrix.{i,j} <> bc then
	            new_delta := (i,j, g.matrix.{i,j})::!new_delta)
	        mask;
	      let new_mask = Grid.Mask.empty g.height g.width in
	      let new_parts = [] in
              let params = Struct [p_h, params_height;
                                   p_w, params_width;
                                   p_c, Color bc] in
              let delta = !new_delta in
	      Myseq.return (params, delta, new_mask, new_parts)
           | _ -> assert false)
  | AddShape (sh,m1) ->
     parse_shape
       g sh env delta mask parts
       (fun (params_first,delta,mask,parts) ->
         parse_grid_model g m1 env delta mask parts
         |> Myseq.flat_map
              (fun (params_rest,delta,mask,parts) ->
                let params = Struct [p_top, params_first; p_down, params_rest] in
                Myseq.return (params, delta, (*ld,*) mask, parts))))
and parse_shape g (sh : shape) env delta mask parts kont
  = Common.prof "Model.parse_shape" (fun () ->
  match sh with
  | Point {offset_i; offset_j; color} ->
     parse_shape_gen
       ~get_occs:Grid.points
       ~matcher:
       (fun env delta mask (i,j,c) ->
	 parse_attr_list env
	   [Int i, offset_i;
	    Int j, offset_j;
            Color c, color]
         |> Myseq.flat_map
	      (function
               | [params_i; params_j; params_c] ->
                  let params = Struct [p_i, params_i; p_j, params_j; p_c, params_c] in
	          Myseq.return (params, [], Grid.Mask.singleton g.height g.width i j)
               | _ -> assert false))
       g env delta mask parts kont
		       
  | Rectangle {height; width;
	       offset_i; offset_j;
	       color; rmask} ->
     parse_shape_gen
       ~get_occs:Grid.rectangles
       ~matcher:
       (fun env delta mask r ->
	 parse_attr_list env
	   [Int r.Grid.height, height;
	    Int r.width, width;
	    Int r.offset_i, offset_i;
	    Int r.offset_j, offset_j;
            Color r.color, color;
            Mask r.rmask, rmask]
         |> Myseq.flat_map
              (function
               | [params_h; params_w; params_i; params_j; params_c; params_m] ->
                  let params = Struct [p_h, params_h; p_w, params_w;
                                   p_i, params_i; p_j, params_j;
                                   p_c, params_c; p_rm, params_m] in
	          Myseq.return (params, r.delta, r.mask)
               | _ -> assert false))
       g env delta mask parts kont)
       
let read_grid_aux (env : env) (g : Grid.t) (m : grid_model)
		  ~(code_grid_data : env -> grid_data -> Mdl.bits)
    : (env * grid_data * Mdl.bits) option = Common.prof "Model.read_grid" (fun () ->
  let mask0 = Grid.Mask.full g.height g.width in
  let parts0 = Grid.segment_by_color g in
  let _, res =
    parse_grid_model g m env delta0 mask0 parts0
    |> Myseq.fold_left
         (fun (ld_min,res) (params,delta,mask,parts) ->
           let gd = {params; delta} in
           let ld = code_grid_data env gd in
           if ld < ld_min
           then ld, Some (env,gd,ld)
           else ld_min, res)
         (infinity, None) in
  res)

let read_grid (env : env) (g : Grid.t) (m : grid_model) : grid_data option =
  let env_size = env_size_of_data env in
  let l_m, code_grid_data = l_grid_model ~env_size m in
  Option.bind
    (read_grid_aux env g m ~code_grid_data)
    (fun (_env,gd,_l) -> Some gd)
							      
let read_grids (egrids: (env * Grid.t) list) (m : grid_model) : grids_read option =
  let env_size = env_size_of_exs egrids in
  let l_m, code_grid_data = l_grid_model ~env_size m in
  let rec aux = function
    | [] -> Some []
    | (env,g)::rest ->
       Option.bind
         (read_grid_aux env g m ~code_grid_data)
         (fun egdl ->
           Option.bind
             (aux rest)
             (fun egdls ->
               Some (egdl::egdls)))
  in
  Option.bind
    (aux egrids)
    (fun egdls -> Some {l_m; egdls})

let apply_model (m : model) (env : env) (g : Grid.t) : (Grid.t, string) Result.t = Common.prof "Model.apply_model" (fun () ->
  match read_grid env g m.input_pattern with
  | Some gdi ->
     (try
	 let envo = gdi.params in
	 Result.Ok (write_grid m.output_template envo grid_data0)
       with exn ->
	 Result.Error ("output writing failed: " ^ Printexc.to_string exn))
  | None -> Result.Error ("input reading failed"))

      
(* learning *)

let grid_model0 =
  Background { height = U;
	       width = U;
	       color = U }

let model0 =
  { input_pattern = grid_model0;
    output_template = grid_model0; }

type grid_refinement =
  | RGridInit
  | RDefs of defs
  | RShape of int (* depth *) * shape

let pp_grid_refinement = function
  | RGridInit -> ()
  | RDefs ds ->
     print_string "DEFS:";
     pp_defs ds
  | RShape (depth,sh) ->
     Printf.printf "SHAPE (depth=%d): " depth;
     pp_shape sh

type refinement =
  | RInit
  | Rinput of grid_refinement
  | Routput of grid_refinement

let pp_refinement = function
  | RInit -> ()
  | Rinput r -> print_string "IN  "; pp_grid_refinement r
  | Routput r -> print_string "OUT "; pp_grid_refinement r
	      

let rec find_defs (m : grid_model) (gr : grids_read) : defs =
  (* find if some gd param unknowns can be replaced
     by some expression over env vars *)
  assert (gr.egdls <> []);
  let envs = List.map (fun (e,_,_) -> e) gr.egdls in
  let params = List.map (fun (_,gd,_) -> gd.params) gr.egdls in
  unknowns_grid_model m
  |> List.fold_left
       (fun defs p ->
         (* u-values *)
         let p_ds = List.map (fun param -> eval_unknown param p) params in
         match p_ds with
         | [] -> assert false
         | d0::ds1 -> (* constants first *)
	    if List.for_all (equal_data d0) ds1
	    then (p, Const d0)::defs
	    else (* then expressions *)
	      match find_defs_p p d0 p_ds envs with
	      | None -> defs
	      | Some def -> def::defs)
       []
and find_defs_p : path -> data -> data list -> env list -> def option =
  fun p d0 p_ds envs ->
  (* env variables *)
  let env1 = List.hd envs in
  let lvd =
    fold_data
      (fun res v d -> (v,d)::res)
      [] path0 env1 in
  let seq_vd : (path * data) Myseq.t = Myseq.from_list lvd in
  (* lists of candidate expressions *)
  let le1 : expr Myseq.t =    
    seq_vd
    |> Myseq.filter_map
         (fun (v,d) ->
           if same_type_data d d0
           then Some (Var v)
           else None) in
  let le2 : expr Myseq.t =
    match d0 with
    | Int _ ->
       seq_vd
       |> Myseq.flat_map
	    (fun (v,d) ->
              if same_type_data d d0
              then
	        Myseq.range 1 3
	        |> Myseq.flat_map
		     (fun c ->
		       Myseq.cons
		         (Plus (Var v, Const (Int c)))
		         (Myseq.cons
			    (Minus (Var v, Const (Int c)))
			    Myseq.empty))
              else Myseq.empty)
    | _ -> Myseq.empty in
  let le3 : expr Myseq.t =
    match d0 with
    | Int _ ->
       seq_vd
       |> Myseq.flat_map
	    (fun (v1,d1) ->
              if same_type_data d1 d0
              then
	        seq_vd
	        |> Myseq.flat_map
		     (fun (v2,d2) ->
                       if same_type_data d2 d0
                       then
		         Myseq.cons
		           (Minus (Var v1, Var v2))
		           (Myseq.cons
			      (Plus (Var v1, Var v2))
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
	  let e_ds = List.map (fun env -> eval_expr env e) envs in
	  if List.for_all2 equal_data e_ds p_ds
	  then Some e
	  else None) with
  | None -> None
  | Some (e,_next) -> Some (p,e)
       
let rec insert_a_shape ?(max_depth = 1) ?(depth = 0) m : (grid_refinement * grid_model) Myseq.t =
  let sh1 = Rectangle {height=U; width=U;
	               offset_i=U; offset_j=U;
	               color=U; rmask=U} in
  let sh2 = Point {offset_i=U; offset_j=U; color=U} in
  Myseq.cons
    (RShape (depth,sh1), AddShape (sh1,m))
    (Myseq.cons
       (RShape (depth,sh2), AddShape (sh2,m))
       (if depth >= max_depth
	then Myseq.empty
	else
	  match m with
	  | Background _ -> Myseq.empty
	  | AddShape (sh1,m1) ->
	     insert_a_shape ~max_depth ~depth:(depth + 1) m1
	     |> Myseq.map (fun (r,m1') -> (r, AddShape (sh1,m1')))))
       
let grid_model_refinements (m : grid_model) (gr : grids_read) : (grid_refinement * grid_model) Myseq.t =
  let defs = find_defs m gr in
  if defs = []
  then insert_a_shape m
  else
    Myseq.cons
      (RDefs defs, subst_grid_model defs m)
      (insert_a_shape m)
		     
let learn_grid_model ~timeout ~beam_width ~refine_degree ~env_size
      (egrids : (env * Grid.t) list)
    : ((grid_refinement * grid_model) * grids_read * Mdl.bits) list * bool =
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(let m = grid_model0 in
	 RGridInit, m)
    ~data:(fun (r,m) ->
	   read_grids egrids m)
    ~code:(fun (r,m) gr ->
	   (*pp_grid_model m; print_newline ();*)
	   pp_grid_refinement r; print_newline ();
	   let lm, ld, lmd = l_grid_model_data gr in
	   Printf.printf "DL = %.1f + %.1f = %.1f\n" lm ld lmd;
	   lmd)
    ~refinements:(fun (r,m) gr ->
		  grid_model_refinements m gr)

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
      let defis = find_defs m.input_pattern gri in
      if defis = []
      then Myseq.empty
      else Myseq.return
	     (Rinput (RDefs defis),
	      { input_pattern = subst_grid_model defis m.input_pattern;
		output_template = subst_grid_model defis m.output_template})
    else Myseq.empty in
  let ref_defos =
    let defos = find_defs m.output_template gro in
    if defos = []
    then Myseq.empty
    else Myseq.return
	   (Routput (RDefs defos),
	    {m with
	      output_template = subst_grid_model defos m.output_template}) in
  let ref_shapis =
    if on_input && grids_read_has_delta gri
    then
      insert_a_shape m.input_pattern
      |> Myseq.map
	   (fun (r,mi') ->
	    (Rinput r, {m with input_pattern=mi'}))
    else Myseq.empty in
  let ref_shapos =
    if grids_read_has_delta gro
    then
      insert_a_shape m.output_template
      |> Myseq.map
	   (fun (r,mo') ->
	    (Routput r, {m with output_template=mo'}))
    else Myseq.empty in
  Myseq.concat
    [ref_defis; ref_shapis; ref_defos; ref_shapos] (* v1 *)
      )
	     
let learn_model
      ?(verbose = true)
      ~timeout
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : ((refinement * model) * (grids_read * grids_read) * Mdl.bits) list * bool
  = Common.prof "Model.learn_model" (fun () ->
  let gis = List.map (fun {input} -> input) pairs in
  let gos = List.map (fun {output} -> output) pairs in
  let egis = List.map (fun gi -> env0, gi) gis in
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RInit, model0)
    ~data:(fun (r,m) ->
	   try
	   match read_grids egis m.input_pattern with
	   | None -> None
	   | Some gri ->
	      let egos =
		List.map2
		  (fun (envi,gdi,li) go -> gdi.params, go)
		  gri.egdls gos in
	      match read_grids egos m.output_template with
	      | None -> None
	      | Some gro ->
		 Some (gri, gro)
	   with
	   | Common.Timeout as exn -> raise exn
	   | exn ->
	      print_endline (Printexc.to_string exn);
	      pp_model m;
	      raise exn)
    ~code:(fun (r,m) (gri,gro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     l_model_data gri gro in
	   if verbose then (
	     pp_refinement r; print_newline ();
	     Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;
	     flush stdout);
	   lmd)
    ~refinements:
    (fun (r,m) (gri,gro) ->
     model_refinements r m gri gro))

