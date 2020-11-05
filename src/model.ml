
open Task

let alpha = ref 10.

exception TODO
       
(* vars and expressions *)
       
type 'a var = string

(* variable generator *)
module Genvar =
  struct
    module M = Map.Make
		 (struct
		     type t = string
		     let compare = Stdlib.compare
		   end)
    type t = int M.t
    let empty = M.empty
    let add (prefix : string) (gv : t) : string * t =
      let c =
	match M.find_opt prefix gv with
	| Some c -> c
	| None -> 0 in
      let c1 = c+1 in
      let v = prefix ^ string_of_int c1 in
      v, M.add prefix c1 gv
    let rec add_list (l_prefix : string list) (gv : t) : string list * t =
      match l_prefix with
      | [] -> [], gv
      | prefix::l1 ->
	 let v, gv = add prefix gv in
	 let lv1, gv = add_list l1 gv in
	 v::lv1, gv
  end

type _ kind = (* kinds of values manipulated by models *)
  | Bool : bool kind (* flags *)
  | Int : int kind (* positions, lengths *)
  | Color : Grid.color kind (* colors *)
  | Mask : Grid.Mask.t option kind (* masks *)

let pp_const : type a. a kind -> a -> unit =
  fun k c ->
  match k with
  | Bool -> print_string (if c then "true" else "false")
  | Int -> print_int c
  | Color -> Grid.pp_color c
  | Mask ->
     (match c with
      | None -> print_string "full"
      | Some m -> Grid.Mask.pp m)
	    
let equal_const : type a. a kind -> a -> a -> bool =
  fun k c1 c2 ->
  match k with
  | Bool | Int | Color -> c1 = c2
  | Mask ->
     (match c1, c2 with
      | None, None -> true
      | None, _
      | _, None -> false
      | Some m1, Some m2 -> Grid.Mask.equal m1 m2)

type _ expr =
  | Var: 'a var -> 'a expr
  | Const : 'a -> 'a expr
  | Plus : int expr * int expr -> int expr
  | Minus : int expr * int expr -> int expr
  | If: bool expr * 'a expr * 'a expr -> 'a expr
(* TODO: Mask transformations: rotation, mirror, scaling *)
							    
exception ComplexExpr (* when neither a Var nor a Const *)

let rec pp_expr : type a. a kind -> a expr -> unit =
  fun k e ->
  match e with
  | Var v -> print_string v
  | Const c -> pp_const k c
  | Plus (e1,e2) -> pp_expr k e1; print_string "+"; pp_expr k e2
  | Minus (e1,e2) -> pp_expr k e1; print_string "-"; pp_expr k e2
  | If (cond,e1,e2) ->
     print_string "if "; pp_expr Bool cond;
     print_string " then "; pp_expr k e1;
     print_string " else "; pp_expr k e2

				    
type def = Def : 'a kind * 'a var * 'a expr -> def

let dbool v b = Def (Bool, v, Const b)
let dint v i = Def (Int, v, Const i)
let dcolor v c = Def (Color, v, Const c)
let dmask v m = Def (Mask, v, Const m)

let pp_def : def -> unit =
  function
  | Def (k,v,e) -> print_string v; print_char '='; pp_expr k e
			
let rec get_var_def_opt : type a. a kind -> a var -> def list -> a expr option =
  fun k v defs ->
  match k, defs with
  | _, [] -> None
  | Bool, Def (Bool,v1,e)::_ when v1 = v -> Some e
  | Int, Def (Int,v1,e)::_ when v1 = v -> Some e
  | Color, Def (Color,v1,e)::_ when v1 = v -> Some e
  | Mask, Def (Mask,v1,e)::_ when v1 = v -> Some e
  | _, _::defs1 -> get_var_def_opt (k : a kind) v defs1

let set_var_def : type a. a kind -> a var -> a expr -> def list -> def list =
  fun k v e defs ->
  Def (k,v,e)::defs
			       
let rec subst_expr : type a. def list -> a kind -> a expr -> a expr =
  fun env k e ->
  match e with
  | Var v ->
     ( match get_var_def_opt k v env with
       | Some e0 -> e0
       | None -> e )
  | Const _ -> e
  | Plus (e1,e2) ->
     Plus (subst_expr env k e1,
	   subst_expr env k e2)
  | Minus (e1,e2) ->
     Minus (subst_expr env k e1,
	    subst_expr env k e2)
  | If (cond,e1,e2) ->
     If (subst_expr env Bool cond,
	 subst_expr env k e1,
	 subst_expr env k e2)

	
type env = def list (* variable bindings *)

let env0 = []
	       
let rec eval_expr : type a. def list -> a kind -> a expr -> a (* raises a fatal exception if some var undefined *) =
  fun env k e -> (* QUICK *)
  match e with
  | Var v ->
     ( match get_var_def_opt k v env with
       | Some e -> eval_expr env k e
       | None -> invalid_arg ("Model.eval_expr: undefined var: " ^ v) )
  | Const c -> c
  | Plus (e1,e2) -> eval_expr env k e1 + eval_expr env k e2
  | Minus (e1,e2) -> eval_expr env k e1 - eval_expr env k e2
  | If (cond,e1,e2) ->
     if eval_expr env Bool cond
     then eval_expr env k e1
     else eval_expr env k e2

let eval_var : type a. def list -> a kind -> a var -> a (* raises a fatal exception if some var undefined *) =
  fun env k v -> (* QUICK *)
  match get_var_def_opt k v env with
  | Some e -> eval_expr env k e
  | None -> invalid_arg ("Model.eval_var: undefined var: " ^ v)


(* parameters *)
(* ---------- *)

type _ attr =
  | U : 'a var -> 'a attr (* unknown => to be matched *)
  | E : 'a expr -> 'a attr (* expression => to be evaluated *)

type params = def list (* unknown bindings *)
		      
let pp_attr : type a. a kind -> a attr -> unit =
  fun k p ->
  match p with
  | U u -> print_char '?'; print_string u
  | E e -> pp_expr k e

let subst_attr : type a. params -> a kind -> a attr -> a attr =
  fun params k a ->
  match a with
  | U u ->
     ( match get_var_def_opt k u params with
       | Some e -> E e
       | None -> a )
  | E e -> E (subst_expr params k e)

let eval_attr : type a. env -> params -> a kind -> a attr -> a =
  fun env params k a -> (* QUICK *)
  match a with
  | U u -> eval_var params k u
  | E e -> eval_expr env k e
		   
		   
(* grid models *)
(* ----------- *)
	    
type grid_model =
  | Background of { height: int attr; (* one-color background *)
		    width: int attr;
		    color: Grid.color attr }
  | AddShape of shape * grid_model (* shape on top of grid_model *)
and shape =
  | Point of { offset_i: int attr;
	       offset_j: int attr;
	       color: Grid.color attr }
  | Rectangle of { height: int attr;
		   width: int attr;
		   offset_i: int attr;
		   offset_j: int attr;
		   color: Grid.color attr;
		   rmask: Grid.Mask.t option attr }

let rec pp_grid_model = function
  | Background {height; width; color} ->
     print_string "a background ";
     pp_attr Int height; print_char 'x'; pp_attr Int width;
     print_string " with color ";
     pp_attr Color color
  | AddShape (sh,m1) ->
     pp_shape sh;
     print_string "\non top of ";
     pp_grid_model m1
and pp_shape = function
  | Point {offset_i; offset_j; color} ->
     print_string "a point at (";
     pp_attr Int offset_i; print_char ','; pp_attr Int offset_j;
     print_string ") with color "; pp_attr Color color
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     print_string "a rectangle ";
     pp_attr Int height; print_string " x "; pp_attr Int width;
     print_string " at ("; pp_attr Int offset_i;
     print_string ", "; pp_attr Int offset_j;
     print_string ") with color "; pp_attr Color color;
     print_string " and mask "; pp_attr Mask rmask

let rec subst_grid_model (params : params) : grid_model -> grid_model =
  function
  | Background {height; width; color} ->
     Background {height = subst_attr params Int height;
		 width = subst_attr params Int width;
		 color = subst_attr params Color color}
  | AddShape (sh,m1) ->
     AddShape (subst_shape params sh,
	       subst_grid_model params m1)
and subst_shape (params : params) : shape -> shape =
  function
  | Point {offset_i; offset_j; color} ->
     Point {offset_i = subst_attr params Int offset_i;
	    offset_j = subst_attr params Int offset_j;
	    color = subst_attr params Color color}
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     Rectangle {height = subst_attr params Int height;
		width = subst_attr params Int width;
		offset_i = subst_attr params Int offset_i;
		offset_j = subst_attr params Int offset_j;
		color = subst_attr params Color color;
		rmask = subst_attr params Mask rmask}
			      
type delta = Grid.pixel list
			
type grid_data =
  { params: params;
    delta: delta }

let grid_data0 =
  { params = [];
    delta = [] }
    
let pp_params params =
  params
  |> List.sort Stdlib.compare
  |> List.iter (fun def -> print_char ' '; pp_def def);
  print_newline ()
    
let pp_delta delta =
  delta
  |> List.sort Stdlib.compare
  |> List.iter (fun (i,j,c) -> Printf.printf " (%d,%d)=" i j; Grid.pp_color c);
  print_newline ()
    
let pp_grid_data gd =
  print_string "params:"; pp_params gd.params;
  print_string "delta:"; pp_delta gd.delta

(* input->output models *)    
type model =
  { genvar : Genvar.t;
    input_pattern : grid_model; (* only consts and unknowns allowed *)
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
     1. +. Mdl.Code.uniform (n+1) +. Mdl.Code.comb (Grid.Mask.area m) n

type staged_code = Mdl.bits (* model proper *) * (env -> params -> Mdl.bits) (* env/params dependent *)

let no_staged_code = fun env params -> 0.
let (+!) l1 (l2,c2) = (l1+.l2, c2)
let (+?) (l1,c1) (l2,c2) = (l1+.l2, (fun env params -> c1 env params +. c2 env params))

let env_size_of_exs exs =
  match exs with
  | (env,_)::_ -> List.length env
  | _ -> assert false
			     
let rec l_expr : type a. env_size:int -> a kind -> code:a code -> a expr -> Mdl.bits =
  fun ~env_size k ~code e ->
  match e with
  | Var v ->
     Mdl.Code.usage 0.3
     +. Mdl.Code.uniform env_size (* identifying one env var *)
  | Const c ->
     Mdl.Code.usage 0.5 +. code c
  | Plus (e1,e2) ->
     Mdl.Code.usage 0.1
     +. l_expr ~env_size k ~code e1
     +. l_expr ~env_size k ~code e2
  | Minus (e1,e2) ->
     Mdl.Code.usage 0.1
     +. l_expr ~env_size k ~code e1
     +. l_expr ~env_size k ~code e2
  | _ -> raise TODO

let l_attr : type a. env_size:int -> a kind -> a attr -> code:a code -> u_code:(env -> params -> a code) -> staged_code =
  fun ~env_size k a ~code ~u_code ->
  match a with
  | U u -> Mdl.Code.usage 0.5, (fun env params -> u_code env params (eval_var params k u))
  | E e -> Mdl.Code.usage 0.5 +. l_expr ~env_size k ~code e, no_staged_code

		       
let l_bool ~env_size (bool : bool attr) : staged_code =
  l_attr ~env_size Bool bool ~code:code_bool ~u_code:(fun env params -> code_bool)

let l_int ~env_size (int : int attr) : staged_code =
  l_attr ~env_size Int int ~code:Mdl.Code.universal_int_plus ~u_code:(fun env params -> Mdl.Code.universal_int_plus)
	 
(* coding offset given bound *)
let l_position ~env_size ~(bound : int attr) (offset: int attr) : staged_code =
  l_attr ~env_size Int offset
	 ~code:(fun o ->
		match bound with
		| E (Const b) -> Mdl.Code.uniform b
		| _ -> Mdl.Code.universal_int_star o)
	 ~u_code:(fun env params o ->
		  let b = eval_attr env params Int bound in
		  Mdl.Code.uniform b)
	 					       
(* coding offset and size given bound *)
let l_slice ~env_size ~(bound : int attr) ~(offset : int attr) ~(size: int attr) : staged_code =
  l_position ~env_size ~bound offset
  +? l_attr ~env_size Int size
	    ~code:(fun s ->
		   match bound, offset with
		   | E (Const b), E (Const o) -> Mdl.Code.uniform (b - o)
		   | E (Const b), _ -> Mdl.Code.uniform b
		   | _, _ -> Mdl.Code.universal_int_plus s)
	    ~u_code:(fun env params s ->
		     let b = eval_attr env params Int bound in
		     let o = eval_attr env params Int offset in
		     Mdl.Code.uniform (b - o))
	    
let l_color ~env_size (color : Grid.color attr) : staged_code =
  l_attr ~env_size Color color ~code:code_color ~u_code:(fun env params -> code_color)
let l_background_color ~env_size (color : Grid.color attr) : staged_code =
  l_attr ~env_size Color color ~code:code_background_color ~u_code:(fun env params -> code_background_color)

let l_mask ~env_size (mask : Grid.Mask.t option attr) : staged_code =
  l_attr ~env_size Mask mask ~code:code_mask ~u_code:(fun env params -> code_mask)
		   
let rec l_grid_model ~(env_size : int) (* nb of env vars *) (m : grid_model) : Mdl.bits * (env -> grid_data -> Mdl.bits) =
  let (l_m, code_m), (he, we) = l_grid_model_aux ~env_size m in
  let code_grid_data =
    fun env gd ->
    let h, w, nb_pixels = (* QUICK *)
      eval_attr env gd.params Int he,
      eval_attr env gd.params Int we,
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
and l_grid_model_aux ~env_size =
  function
  | Background {height; width; color} ->
     let l_code =
       Mdl.Code.usage 0.5
       +! l_int ~env_size height
       +? l_int ~env_size width
       +? l_background_color ~env_size color in
     l_code, (height,width)
  | AddShape (sh,m1) ->
     let l_code1, hw1 = l_grid_model_aux ~env_size m1 in
     let l_code =
       Mdl.Code.usage 0.5
       +! l_code1
       +? l_shape ~env_size ~hw:hw1 sh in
     l_code, hw1
and l_shape ~(env_size : int) ~(hw : int attr * int attr) : shape -> staged_code =
  let h, w = hw in
  function
  | Point {offset_i; offset_j; color} ->
     Mdl.Code.usage 0.5
     +! l_position ~env_size ~bound:h offset_i
     +? l_position ~env_size ~bound:w offset_j
     +? l_color ~env_size color
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     Mdl.Code.usage 0.5
     +! l_slice ~env_size ~bound:h ~size:height ~offset:offset_i
     +? l_slice ~env_size ~bound:w ~size:width ~offset:offset_j
     +? l_color ~env_size color
     +? l_mask ~env_size rmask
	   

(* result of reading a grid: grid data and description lengths *)
type grids_read = { l_m : Mdl.bits;
		    egdls : (env * grid_data * Mdl.bits) list }

let grids_read_has_delta (gr : grids_read) : bool =
  List.for_all
    (fun (env,gd,l) -> gd.delta <> [])
    gr.egdls
		    
let align_grids_read_with (gri : grids_read) (l : 'a list) : grids_read =
  { gri with egdls = Common.sub_list gri.egdls 0 (List.length l) }


type 'a triple = 'a * 'a * 'a
		  
let l_grid_model_data (gr : grids_read) : Mdl.bits triple (* model, data, model+data *) = Common.prof "Model.l_grid_model_data" (fun () ->
  let l_d =
    !alpha (* because given training examples are only a sample from a class of grids *)
    *. Mdl.sum gr.egdls (fun (env,gd,l) -> l) in
  gr.l_m, l_d, gr.l_m +. l_d)

let l_model_data (gri : grids_read) (gro : grids_read) : Mdl.bits triple triple =
  (* to remove extra test inputs *)
  let gri = align_grids_read_with gri gro.egdls in
  let lmi, ldi, lmdi = l_grid_model_data gri in
  let lmo, ldo, lmdo = l_grid_model_data gro in
  (lmi, lmo, lmi+.lmo), (ldi, ldo, ldi+.ldo), (lmdi, lmdo, lmdi+.lmdo)

(* writing grids from models *)
    
let rec apply_grid_model (m : grid_model) (env : env) (params : params) : Grid.t =
  match m with
  | Background { height; width; color } ->
     Grid.make
       (eval_attr env params Int height)
       (eval_attr env params Int width)
       (eval_attr env params Color color)
  | AddShape (sh,m1) ->
     let g = apply_grid_model m1 env params in
     apply_shape sh env params g;
     g
and apply_shape (sh : shape) env params g : unit =
  match sh with
  | Point {offset_i; offset_j; color} ->
     let i = eval_attr env params Int offset_i in
     let j = eval_attr env params Int offset_j in
     let c = eval_attr env params Color color in
     Grid.set_pixel g i j c
  | Rectangle {height; width; offset_i; offset_j; color; rmask} ->
     let h = eval_attr env params Int height in
     let w = eval_attr env params Int width in
     let mini = eval_attr env params Int offset_i in
     let minj = eval_attr env params Int offset_j in
     let c = eval_attr env params Color color in
     let m_opt = eval_attr env params Mask rmask in
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
  let g = apply_grid_model m env d.params in
  List.iter
    (fun (i,j,c) -> Grid.set_pixel g i j c)
    d.delta;
  g)


(* reading grids with models *)

type 'a parse_result = ('a, string) Result.t
				    
let parse_const : type a. a kind -> a -> a -> params -> params parse_result =
  fun k c0 c params ->
  if equal_const k c c0
  then Result.Ok params
  else Result.Error "value mismatch"
     
let parse_attr : type a. env -> a kind -> a -> a attr -> params -> params parse_result =
  fun env k c0 a params -> (* QUICK *)
  match a with
  | U u ->
     ( match get_var_def_opt k u params with
       | None -> Result.Ok (set_var_def k u (Const c0) params)
       | Some (Const c) -> parse_const k c0 c params
       | Some _ -> assert false ) (* only constants in params *)
  | E e ->
     let c = eval_expr env k e in
     parse_const k c0 c params

let rec parse_attr_list : type a. env -> a kind -> (a * a attr) list -> params -> params parse_result =
  fun env k lca params ->
  match lca with
  | [] -> Result.Ok params
  | (c,a)::lca1 ->
     Result.bind
       (parse_attr env k c a params)
       (fun params -> parse_attr_list env k lca1 params)

let parse_shape_gen
      ~(get_occs : Grid.t -> Grid.Mask.t -> Grid.part list -> 'a list)
      ~(matcher : env -> grid_data -> Grid.Mask.t -> 'a -> (params * delta * Grid.Mask.t) parse_result)
      g env gd mask parts
      kont
    : (grid_data * Mdl.bits * Grid.Mask.t * Grid.part list) parse_result =
  let occs = Common.prof "Model.parse_shape_gen/get_occs" (fun () -> get_occs g mask parts) in
  let _, res =
    List.fold_left
      (fun (min_l, res) occ ->
       let new_res =
	 Result.bind
	   (matcher env gd mask occ)
	   (fun (new_params, occ_delta, occ_mask) ->
	    let new_gd = { params = new_params;
			   delta = occ_delta @ gd.delta } in
	    let new_mask = Grid.Mask.diff mask occ_mask in
	    let new_parts =
	      List.filter
		(fun p ->
		 not (Grid.Mask.is_empty
			(Grid.Mask.inter p.Grid.pixels new_mask)))
		parts in
	    kont (new_gd, new_mask, new_parts)) in
       match new_res with
       | Result.Ok (new_gd,l,_,_) ->
	  if l < min_l
	  then l, new_res
	  else min_l, res
       | Result.Error _ -> min_l, res)
      (infinity, Result.Error "no matching occurence")
      occs in
  res
       
let rec parse_grid_model
	  ~(code_grid_data : env -> grid_data -> Mdl.bits)
	  (g : Grid.t) (m : grid_model) (env : env)
	  (gd : grid_data) (mask : Grid.Mask.t) (parts : Grid.part list)
	: (grid_data * Mdl.bits * Grid.Mask.t * Grid.part list) parse_result = Common.prof "Model.parse_grid_model" (fun () ->
  match m with
  | Background {height; width; color} ->
     Result.bind
       (parse_attr_list
	  env Int
	  [g.height, height; g.width, width]
	  gd.params)
       (fun new_params ->
	let new_params = ref new_params in
	let new_delta = ref gd.delta in
	let bc = (* background color *)
	  match color with
	  | E e -> eval_expr env Color e
	  | U u ->
	    (* determining the majority color *)
	     let color_counter = new Common.counter in
	     Grid.Mask.iter
	       (fun i j -> color_counter#add g.matrix.{i,j})
	       mask;
	     let bc =
	       match color_counter#most_frequents with
	       | _, bc::_ -> bc
	       | _ -> Grid.black in
	     (* setting the variable *)
	     match parse_attr env Color bc color !new_params with
	     | Result.Ok params -> new_params := params; bc
	     | Result.Error msg -> assert false in
	(* adding mask pixels with other color than background to delta *)
	Grid.Mask.iter
	  (fun i j ->
	   if g.matrix.{i,j} <> bc then
	     new_delta := (i,j, g.matrix.{i,j})::!new_delta)
	  mask;
	let new_mask = Grid.Mask.empty g.height g.width in
	let new_parts = [] in
	let gd = { params = (!new_params); delta = (!new_delta) } in
	let ld = code_grid_data env gd in
	Result.Ok (gd, ld, new_mask, new_parts))
  | AddShape (sh,m1) ->
     parse_shape
       g sh env gd mask parts
       (fun (gd,mask,parts) ->
	parse_grid_model ~code_grid_data g m1 env gd mask parts))
and parse_shape g (sh : shape) env gd mask parts kont = Common.prof "Model.parse_shape" (fun () ->
  match sh with
  | Point {offset_i; offset_j; color} ->
     parse_shape_gen
       ~get_occs:Grid.points
       ~matcher:
       (fun env gd mask (i,j,c) ->
	Result.bind
	  (parse_attr_list
	     env Int
	     [i, offset_i;
	      j, offset_j]
	     gd.params)
	  (fun new_params ->
	   Result.bind
	     (parse_attr env Color c color new_params)
	     (fun new_params ->
	      Result.Ok (new_params, [], Grid.Mask.singleton g.height g.width i j))))
       g env gd mask parts kont
		       
  | Rectangle {height; width;
	       offset_i; offset_j;
	       color; rmask} ->
     parse_shape_gen
       ~get_occs:Grid.rectangles
       ~matcher:
       (fun env gd mask r ->
	Result.bind
	  (parse_attr_list
	     env Int
	     [r.Grid.height, height;
	      r.width, width;
	      r.offset_i, offset_i;
	      r.offset_j, offset_j]
	     gd.params)
	  (fun new_params ->
	   Result.bind
	     (parse_attr env Color r.color color new_params)
	     (fun new_params ->
	      Result.bind
		(parse_attr env Mask r.rmask rmask new_params)
		(fun new_params ->
		 Result.Ok (new_params, r.delta, r.mask)))))
       g env gd mask parts kont)
       
let read_grid_aux (env : env) (g : Grid.t) (m : grid_model)
		  ~(code_grid_data : env -> grid_data -> Mdl.bits)
    : (env * grid_data * Mdl.bits) parse_result = Common.prof "Model.read_grid" (fun () ->
  let gd0 = grid_data0 in
  let mask0 = Grid.Mask.full g.height g.width in
  let parts0 = Grid.segment_by_color g in
  Result.bind
    (parse_grid_model ~code_grid_data g m env gd0 mask0 parts0)
    (fun (gd,ld,mask,parts) -> Result.Ok (env,gd,ld)))

let read_grid (env : env) (g : Grid.t) (m : grid_model) : grid_data parse_result =
  let env_size = List.length env in
  let l_m, code_grid_data = l_grid_model ~env_size m in
  Result.bind
    (read_grid_aux env g m ~code_grid_data)
    (fun (_env,gd,_l) -> Result.Ok gd)
							      
let read_grids (egrids: (env * Grid.t) list) (m : grid_model) : grids_read option =
  let env_size = env_size_of_exs egrids in
  let l_m, code_grid_data = l_grid_model ~env_size m in
  let r_egdls =
    List.map
      (fun (env,g) -> read_grid_aux env g m ~code_grid_data)
      egrids in
  List.fold_right (* checking no errors, and aggregating *)
    (fun r_egdl res ->
     match r_egdl, res with
     | Result.Ok egdl, Some gr -> Some {gr with egdls = egdl::gr.egdls}
     | _ -> None)
    r_egdls (Some {l_m; egdls = []})


let apply_model (m : model) (env : env) (g : Grid.t) : (Grid.t, string) Result.t = Common.prof "Model.apply_model" (fun () ->
  match read_grid env g m.input_pattern with
  | Result.Ok gdi ->
     (try
	 let envo = gdi.params in
	 Result.Ok (write_grid m.output_template envo grid_data0)
       with exn ->
	 Result.Error ("output writing failed: " ^ Printexc.to_string exn))
  | Result.Error msg -> Result.Error ("input reading failed: " ^ msg))

      
(* learning *)

let grid_model0 gv =
  let vH, gv = Genvar.add "H" gv in
  let vW, gv = Genvar.add "W" gv in
  let vC, gv = Genvar.add "C" gv in
  let m =
    Background { height = U vH;
		 width = U vW;
		 color = U vC } in
  gv, m

let model0 =
  let gv = Genvar.empty in
  let gv, mi = grid_model0 gv in
  let gv, mo = grid_model0 gv in
  { genvar = gv;
    input_pattern = mi;
    output_template = mo }

type grid_refinement =
  | RGridInit
  | RDefs of def list
  | RShape of int (* depth *) * shape

let pp_grid_refinement = function
  | RGridInit -> ()
  | RDefs ds ->
     print_string "DEFS:";
     ds |> List.iter (fun d -> print_char ' '; pp_def d)
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
	      

let rec find_defs (gr : grids_read) : def list =
  (* find if some gd param unknowns can be replaced
     by some expression over env vars *)
  assert (gr.egdls <> []);
  let es = List.map (fun (e,_,_) -> e) gr.egdls in
  let gds = List.map (fun (_,gd,_) -> gd) gr.egdls in
  let ps = List.map (fun gd -> gd.params) gds in
  List.fold_left
    (fun defs (Def (k,u,_)) ->
     (* u-values *)
     let u_cs = List.map (fun p -> eval_var p k u) ps in
     match u_cs with
     | [] -> assert false
     | c0::cs1 ->
	if List.for_all (equal_const k c0) cs1
	then Def (k, u, Const c0)::defs
	else
	  match find_defs_u k u u_cs es with
	  | None -> defs
	  | Some d -> d::defs)
    [] (List.hd ps)
and find_defs_u : type a. a kind -> a var -> a list -> env list -> def option =
  fun k u u_cs es ->
  (* env variables *)
  let lv =
    List.fold_left
      (fun lv d ->
       match k, d with
       | Bool, Def (Bool,v,_) -> v::lv
       | Int, Def (Int,v,_) -> v::lv
       | Color, Def (Color,v,_) -> v::lv
       | Mask, Def (Mask,v,_) -> v::lv
       | _ -> lv)
      [] (List.hd es) in
  let seq_v : a var Myseq.t = Myseq.from_list lv in
  (* lists of candidate expressions *)
  let le1 : a expr Myseq.t =    
    seq_v
    |> Myseq.map (fun v -> Var v) in
  let le2 : a expr Myseq.t =
    match k with
    | Int ->
       seq_v
       |> Myseq.flat_map
	    (fun v ->
	     Myseq.range 1 3
	     |> Myseq.flat_map
		  (fun c ->
		   Myseq.cons
		     (Plus (Var v, Const c))
		     (Myseq.cons
			(Minus (Var v, Const c))
			Myseq.empty)))
    | _ -> Myseq.empty in
  let le3 : a expr Myseq.t =
    match k with
    | Int ->
       seq_v
       |> Myseq.flat_map
	    (fun v1 ->
	     seq_v
	     |> Myseq.flat_map
		  (fun v2 ->
		   Myseq.cons
		     (Minus (Var v1, Var v2))
		     (Myseq.cons
			(Plus (Var v1, Var v2))
			Myseq.empty)))
    | _ -> Myseq.empty in
  let le = Myseq.concat [le1; le2; le3] in
  (* finding the first expression defining 'u' *)
  match
    le
    |> Myseq.find_map
	 (fun e ->
	  let e_cs = List.map (fun env -> eval_expr env k e) es in
	  if List.for_all2 (equal_const k) e_cs u_cs
	  then Some e
	  else None) with
  | None -> None
  | Some (e,_next) -> Some (Def (k,u,e))
       
let rec insert_a_shape ?(max_depth = 1) ?(depth = 0) gv m : (grid_refinement * Genvar.t * grid_model) Myseq.t =
  let sh1, gv1 =
    match Genvar.add_list ["H"; "W"; "I"; "J"; "C"; "M"] gv with
    | [vH; vW; vI; vJ; vC; vM], gv ->
       let sh = Rectangle {height=U vH; width=U vW;
			   offset_i=U vI; offset_j=U vJ;
			   color=U vC; rmask=U vM} in
       sh, gv
    | _ -> assert false in
  let sh2, gv2 =
    match Genvar.add_list ["I"; "J"; "C"] gv with
    | [vI; vJ; vC], gv ->
       let sh = Point {offset_i=U vI; offset_j=U vJ; color=U vC} in
       sh, gv
    | _ -> assert false in
  Myseq.cons
    (RShape (depth,sh1), gv1, AddShape (sh1,m))
    (Myseq.cons
       (RShape (depth,sh2), gv2, AddShape (sh2,m))
       (if depth >= max_depth
	then Myseq.empty
	else
	  match m with
	  | Background _ -> Myseq.empty
	  | AddShape (sh1,m1) ->
	     insert_a_shape ~max_depth ~depth:(depth + 1) gv m1
	     |> Myseq.map (fun (r,gv,m1') -> (r, gv, AddShape (sh1,m1')))))
       
let grid_model_refinements (gv : Genvar.t) (m : grid_model) (gr : grids_read) : (grid_refinement * Genvar.t * grid_model) Myseq.t =
  let defs = find_defs gr in
  if defs = []
  then insert_a_shape gv m
  else (
    Myseq.cons
      (RDefs defs, gv, subst_grid_model defs m)
      (insert_a_shape gv m))
		     
let learn_grid_model ~timeout ~beam_width ~refine_degree ~env_size
		     (egrids : (env * Grid.t) list) (gv : Genvar.t)
    : ((grid_refinement * Genvar.t * grid_model) * grids_read * Mdl.bits) list * bool =
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(let gv,m = grid_model0 gv in
	 RGridInit, gv, m)
    ~data:(fun (r,gv,m) ->
	   read_grids egrids m)
    ~code:(fun (r,gv,m) gr ->
	   (*pp_grid_model m; print_newline ();*)
	   pp_grid_refinement r; print_newline ();
	   let lm, ld, lmd = l_grid_model_data gr in
	   Printf.printf "DL = %.1f + %.1f = %.1f\n" lm ld lmd;
	   lmd)
    ~refinements:(fun (r,gv,m) gr ->
		  grid_model_refinements gv m gr)

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
      let defis = find_defs gri in
      if defis = []
      then Myseq.empty
      else Myseq.return
	     (Rinput (RDefs defis),
	      {m with
		input_pattern = subst_grid_model defis m.input_pattern;
		output_template = subst_grid_model defis m.output_template})
    else Myseq.empty in
  let ref_defos =
    let defos = find_defs gro in
    if defos = []
    then Myseq.empty
    else Myseq.return
	   (Routput (RDefs defos),
	    {m with
	      output_template = subst_grid_model defos m.output_template}) in
  let ref_shapis =
    if on_input && grids_read_has_delta gri
    then
      insert_a_shape m.genvar m.input_pattern
      |> Myseq.map
	   (fun (r,gv',mi') ->
	    (Rinput r, {m with genvar=gv'; input_pattern=mi'}))
    else Myseq.empty in
  let ref_shapos =
    if grids_read_has_delta gro
    then
      insert_a_shape m.genvar m.output_template
      |> Myseq.map
	   (fun (r,gv',mo') ->
	    (Routput r, {m with genvar=gv'; output_template=mo'}))
    else Myseq.empty in
  Myseq.concat [ref_defis; ref_shapis;
		(*ref_defos;*) ref_shapos; ref_defos])
	     
let learn_model
      ?(verbose = true)
      ~timeout
      ~beam_width ~refine_degree
      (gis_test : Grid.t list) (* train + test inputs *)
      (gos : Grid.t list) (* only train outputs *)
    : ((refinement * model) * (grids_read * grids_read) * Mdl.bits) list * bool
  = Common.prof "Model.learn_model" (fun () ->
  let egis_test = List.map (fun gi -> env0, gi) gis_test in
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RInit, model0)
    ~data:(fun (r,m) ->
	   try
	   match read_grids egis_test m.input_pattern with
	   | None -> None
	   | Some gri_test ->
	      let gri = align_grids_read_with gri_test gos in
	      let egos =
		List.map2
		  (fun (envi,gdi,li) go -> gdi.params, go)
		  gri.egdls gos in
	      match read_grids egos m.output_template with
	      | None -> None
	      | Some gro ->
		 Some (gri_test, gro)
	   with
	   | Common.Timeout as exn -> raise exn
	   | exn ->
	      print_endline (Printexc.to_string exn);
	      pp_model m;
	      raise exn)
    ~code:(fun (r,m) (gri_test,gro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     l_model_data gri_test gro in
	   if verbose then (
	     pp_refinement r; print_newline ();
	     Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;
	     flush stdout);
	   lmd)
    ~refinements:
    (fun (r,m) (gri_test,gro) ->
     model_refinements r m gri_test gro))
    
(* naive *)
    
type t = unit
let learn (train : Task.pair list) : t = ()
