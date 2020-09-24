
open Task

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
  end

type _ kind = (* kinds of values manipulated by models *)
  | Bool : bool kind (* flags *)
  | Int : int kind (* positions, lengths *)
  | Color : Grid.color kind (* colors *)

type _ expr =
  | Var: 'a var -> 'a expr
  | Const : 'a -> 'a expr
  | Plus: int expr * int expr -> int expr
  | If: bool expr * 'a expr * 'a expr -> 'a expr
							    
exception ComplexExpr (* when neither a Var nor a Const *)

let pp_const : type a. a kind -> a -> unit =
  fun k c ->
  match k with
  | Bool -> print_string (if c then "true" else "false")
  | Int -> print_int c
  | Color -> Grid.pp_color c
	    
let rec pp_expr : type a. a kind -> a expr -> unit =
  fun k e ->
  match e with
  | Var v -> print_string v
  | Const c -> pp_const k c
  | Plus (e1,e2) -> pp_expr k e1; print_char '+'; pp_expr k e2
  | If (cond,e1,e2) ->
     print_string "if "; pp_expr Bool cond;
     print_string " then "; pp_expr k e1;
     print_string " else "; pp_expr k e2

				    
type def = Def : 'a kind * 'a var * 'a expr -> def

let dbool v b = Def (Bool, v, Const b)
let dint v i = Def (Int, v, Const i)
let dcolor v c = Def (Color, v, Const c)

let pp_def : def -> unit =
  function
  | Def (k,v,e) -> print_string v; print_char '='; pp_expr k e
			
exception Unbound_var of string
				
let rec get_var_def : type a. a kind -> a var -> def list -> a expr =
  fun k v defs ->
  match k, defs with
  | _, [] -> raise (Unbound_var v)
  | Bool, Def (Bool,v1,e)::_ when v1 = v -> e
  | Int, Def (Int,v1,e)::_ when v1 = v -> e
  | Color, Def (Color,v1,e)::_ when v1 = v -> e
  | _, _::defs1 -> get_var_def (k : a kind) v defs1
let get_var_def_opt : type a. a kind -> a var -> def list -> a expr option =
  fun k v defs ->
  try Some (get_var_def k v defs)
  with Unbound_var _ -> None

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
  | If (cond,e1,e2) ->
     If (subst_expr env Bool cond,
	 subst_expr env k e1,
	 subst_expr env k e2)

	
type env = def list (* variable bindings *)

let env0 = []
	       
let rec eval_expr : type a. def list -> a kind -> a expr -> a =
  fun env k e ->
  match e with
  | Var v ->
     let e = get_var_def k v env in
     eval_expr env k e
  | Const c -> c
  | Plus (e1,e2) -> eval_expr env k e1 + eval_expr env k e2
  | If (cond,e1,e2) ->
     if eval_expr env Bool cond
     then eval_expr env k e1
     else eval_expr env k e2

let eval_var : type a. def list -> a kind -> a var -> a =
  fun env k v ->
  let e = get_var_def k v env in
  eval_expr env k e


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
  | E _ -> a

let eval_attr : type a. env -> params -> a kind -> a attr -> a =
  fun env params k a ->
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
		   filled: bool attr }

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
  | Rectangle {height; width; offset_i; offset_j; color; filled} ->
     print_string "a rectangle ";
     pp_attr Int height; print_char 'x'; pp_attr Int width;
     print_string " at ("; pp_attr Int offset_i;
     print_string ","; pp_attr Int offset_j;
     print_string ") with color "; pp_attr Color color;
     print_string " and filling "; pp_attr Bool filled

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
  | Rectangle {height; width; offset_i; offset_j; color; filled} ->
     Rectangle {height = subst_attr params Int height;
		width = subst_attr params Int width;
		offset_i = subst_attr params Int offset_i;
		offset_j = subst_attr params Int offset_j;
		color = subst_attr params Color color;
		filled = subst_attr params Bool filled}
			      
	   
type grid_data =
  { params: params;
    delta: Grid.pixel list }

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
  | Rectangle {height; width; offset_i; offset_j; color; filled} ->
     let h = eval_attr env params Int height in
     let w = eval_attr env params Int width in
     let mini = eval_attr env params Int offset_i in
     let minj = eval_attr env params Int offset_j in
     let c = eval_attr env params Color color in
     let f = eval_attr env params Bool filled in
     let maxi = mini + h - 1 in
     let maxj = minj + w - 1 in
     for i = mini to maxi do
       for j = minj to maxj do
	 if f || i=mini || i=maxi || j=minj || j=maxj then
	   Grid.set_pixel g i j c
       done;
     done
	   
let write_grid (m : grid_model) (env : env) (d : grid_data) : Grid.t =
  let g = apply_grid_model m env d.params in
  List.iter
    (fun (i,j,c) -> Grid.set_pixel g i j c)
    d.delta;
  g


(* reading grids with models *)

type 'a parse_result = ('a, string) Result.t
				    
let parse_const : type a. a kind -> a -> a -> params -> params parse_result =
  fun k c0 c params ->
  if c = c0
  then Result.Ok params
  else Result.Error "value mismatch"
     
let parse_attr : type a. env -> a kind -> a -> a attr -> params -> params parse_result =
  fun env k c0 a params ->
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

       
let rec parse_grid_model
	  (g : Grid.t) (m : grid_model) (env : env)
	  (gd : grid_data) (mask : Grid.Mask.t) (parts : Grid.part list)
	: (grid_data * Grid.Mask.t * Grid.part list) parse_result =
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
	  try eval_attr env !new_params Color color
	  with Unbound_var _ ->
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
	let new_mask = Grid.Mask.empty in
	let new_parts = [] in
	Result.Ok ({ params = (!new_params); delta = (!new_delta) }, new_mask, new_parts))
  | AddShape (sh,m1) ->
     parse_shape
       g sh env gd mask parts
       (fun (gd,mask,parts) ->
	parse_grid_model g m1 env gd mask parts)
and parse_shape g (sh : shape) env gd mask parts kont =
  match sh with
  | Point {offset_i; offset_j; color} -> raise TODO
  | Rectangle {height; width;
	       offset_i; offset_j;
	       color; filled} ->
     let lr = Grid.rectangles g mask parts in
     let _, res =
       List.fold_left
	 (fun (min_d, res) r ->
	  let new_res =
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
		    (parse_attr env Bool true filled new_params)
		    (fun new_params ->
		     let new_gd = { params = new_params;
				    delta = r.delta @ gd.delta } in
		     let new_mask = Grid.Mask.diff mask r.mask in
		     let new_parts =
		       List.filter
			 (fun p ->
			  Grid.Mask.is_empty
			    (Grid.Mask.diff p.Grid.pixels mask))
			 parts in
		     kont (new_gd, new_mask, new_parts)))) in
	  match new_res with
	  | Result.Ok (new_gd,_,_) ->
	     let d = List.length new_gd.delta in
	     if d < min_d
	     then d, new_res
	     else min_d, res
	  | Result.Error _ -> min_d, res)
	 (max_int, Result.Error "no matching rectangle")
	 lr in
     res
	      
let read_grid (env : env) (g : Grid.t) (m : grid_model) : (env * grid_data) parse_result =
  let gd0 = { params = []; delta = [] } in
  let mask0 = Grid.Mask.full g.height g.width in
  let parts0 = Grid.segment_by_color g in
  Result.bind
    (parse_grid_model g m env gd0 mask0 parts0)
    (fun (gd,mask,parts) -> Result.Ok (env,gd))

let read_grids (egrids: (env * Grid.t) list) (m : grid_model) : (env * grid_data) list option =
  let r_egds = List.map (fun (env,g) -> read_grid env g m) egrids in
  List.fold_right (* checking no errors, and aggregating *)
    (fun r_egd res ->
     match r_egd, res with
     | Result.Ok egd, Some egds -> Some (egd::egds)
     | _ -> None)
    r_egds (Some [])
    
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


type staged_code = Mdl.bits (* model proper *) * (env -> params -> Mdl.bits) (* env/params dependent *)

let no_staged_code = fun env params -> 0.
let (+!) l1 (l2,c2) = (l1+.l2, c2)
let (+?) (l1,c1) (l2,c2) = (l1+.l2, (fun env params -> c1 env params +. c2 env params))
		      
let l_expr : type a. env_size:int -> a kind -> code:a code -> a expr -> Mdl.bits =
  fun ~env_size k ~code e ->
  match k, e with
  | _, Var v ->
     Mdl.Code.usage 0.5
     +. Mdl.Code.uniform env_size (* identifying one env var *)
  (* the 3 following cases count as one => same usage *)
  | Bool, Const c -> Mdl.Code.usage 0.5 +. code c
  | Int, Const c -> Mdl.Code.usage 0.5 +. code c
  | Color, Const c -> Mdl.Code.usage 0.5 +. code c
  | _ -> raise TODO

let l_attr : type a. env_size:int -> a kind -> a attr -> code:a code -> u_code:(env -> params -> a code) -> staged_code =
  fun ~env_size k a ~code ~u_code ->
  match a with
  | U u -> 1., (fun env params -> u_code env params (eval_var params k u))
  | E e -> 1. +. l_expr ~env_size k ~code e, no_staged_code

		       
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

		   
let rec l_grid_model ~(env_size : int) (* nb of input vars *)
	: grid_model -> staged_code * (int attr * int attr) =
  function
  | Background {height; width; color} ->
     let l_code =
       Mdl.Code.usage 0.5
       +! l_int ~env_size height
       +? l_int ~env_size width
       +? l_background_color ~env_size color in
     l_code, (height,width)
  | AddShape (sh,m1) ->
     let l_code1, hw1 = l_grid_model ~env_size m1 in
     let l_code =
       Mdl.Code.usage 0.5
       +! l_code1
       +? l_shape ~env_size ~hw:hw1 sh in
     l_code, hw1
and l_shape ~(env_size : int) ~(hw : int attr * int attr) : shape -> staged_code =
  let h, w = hw in
  function
  | Point {offset_i; offset_j; color} ->
     Mdl.Code.usage 0.
     +! l_position ~env_size ~bound:h offset_i
     +? l_position ~env_size ~bound:w offset_j
     +? l_color ~env_size color
  | Rectangle {height; width; offset_i; offset_j; color; filled} ->
     Mdl.Code.usage 1.
     +! l_slice ~env_size ~bound:h ~size:height ~offset:offset_i
     +? l_slice ~env_size ~bound:w ~size:width ~offset:offset_j
     +? l_color ~env_size color
     +? l_bool ~env_size filled
	   
	
let l_grid_delta ~(height : int) ~(width : int) (pixels : Grid.pixel list) : Mdl.bits =
  let area = height * width in
  let nb_pixels = List.length pixels in
  Mdl.Code.universal_int_star nb_pixels (* how many delta pixels there are *)
  +. Mdl.Code.comb nb_pixels area (* where they are *)
  +. float nb_pixels *. Mdl.Code.uniform (Grid.nb_color - 1) (* what are their color, different from the color generated by the model *)
    
let l_grid_data ~m ~(code_m : env -> params -> Mdl.bits) ~(hw : int attr * int attr) (env : env) (gd : grid_data) : Mdl.bits =
  let he, we = hw in
  let h = eval_attr env gd.params Int he in
  let w = eval_attr env gd.params Int we in
  code_m env gd.params
  +. l_grid_delta ~height:h ~width:w gd.delta

let l_grid_model_data ~env_size (m : grid_model) (egds : (env * grid_data) list) : Mdl.bits * Mdl.bits * Mdl.bits (* model, data, model+data *) =
  let (l_m, code_m), hw = l_grid_model ~env_size m in
  let l_d = Mdl.sum egds (fun (env,gd) -> l_grid_data ~m ~code_m ~hw env gd) in
  l_m, l_d, l_m +. l_d

let l_model_data (m : model) (egds_out : (env * grid_data) list) : Mdl.bits * Mdl.bits * Mdl.bits =
  let env_size =
    match egds_out with
    | (env,_)::_ -> List.length env
    | _ -> assert false in
  let (l_mi, code_mi), hwi = l_grid_model ~env_size:0 m.input_pattern in
  let (l_mo, code_mo), hwo = l_grid_model ~env_size m.output_template in
  let l_m = l_mi +. l_mo in
  let l_d = Mdl.sum egds_out (fun (env,gdo) -> l_grid_data ~m:m.output_template ~code_m:code_mo ~hw:hwo env gdo) in
  l_m, l_d, l_m +. l_d

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
	
(* computing common definitions among a list of definition lists *)
let inter_defs (ldefs : def list list) : def list =
  match ldefs with
  | [] -> invalid_arg "Model.inter_defs: empty list"
  | defs::l1 ->
     List.fold_left
       (fun defs defs1 ->
	List.filter
	  (function
	    | Def (k,v,e) -> Some e = get_var_def_opt k v defs1)
	  defs)
       defs l1

let rec insert_a_shape ?(max_depth = 2) gv m : (Genvar.t * grid_model) Myseq.t =
  Myseq.cons
    (let vH, gv = Genvar.add "H" gv in
     let vW, gv = Genvar.add "W" gv in
     let vI, gv = Genvar.add "I" gv in
     let vJ, gv = Genvar.add "J" gv in
     let vC, gv = Genvar.add "C" gv in
     let vF, gv = Genvar.add "F" gv in
     let sh = Rectangle {height=U vH; width=U vW;
			 offset_i=U vI; offset_j=U vJ;
			 color=U vC; filled=U vF} in
     gv, AddShape (sh,m))
    (if max_depth = 0
     then Myseq.empty
     else
       match m with
       | Background _ -> Myseq.empty
       | AddShape (sh1,m1) ->
	  insert_a_shape ~max_depth:(max_depth - 1) gv m1
	  |> Myseq.map (fun (gv,m1') -> (gv, AddShape (sh1,m1'))))
       
let grid_model_refinements (gv : Genvar.t) (m : grid_model) (egds : (env * grid_data) list) : (Genvar.t * grid_model) Myseq.t =
  let common_params =
    egds |> List.map (fun (env,gd) -> gd.params) |> inter_defs in
  if common_params = []
  then insert_a_shape gv m
  else (
    Myseq.cons
      (gv, subst_grid_model common_params m)
      (insert_a_shape gv m))
		     
let learn_grid_model ~beam_width ~refine_degree ~env_size
		     (egrids : (env * Grid.t) list) (gv : Genvar.t)
    : ((Genvar.t * grid_model) * (env * grid_data) list * Mdl.bits) list =
  Mdl.Strategy.beam
    ~beam_width
    ~refine_degree
    ~m0:(grid_model0 gv)
    ~data:(fun (gv,m) ->
	   read_grids egrids m)
    ~code:(fun (gv,m) egds ->
	   (*pp_grid_model m; print_newline ();*)
	   let _, _, l = l_grid_model_data ~env_size m egds in
	   Printf.printf "DL = %.1f\n" l;
	   l)
    ~refinements:(fun (gv,m) egds ->
		  grid_model_refinements gv m egds)
		     
(* naive *)
    
type t = unit
let learn (train : Task.pair list) : t = ()
