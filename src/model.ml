
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
  | Plus : int expr * int expr -> int expr
  | Minus : int expr * int expr -> int expr
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
  fun env k e -> Common.prof "Model.eval_expr" (fun () ->
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
     else eval_expr env k e2)

let eval_var : type a. def list -> a kind -> a var -> a (* raises a fatal exception if some var undefined *) =
  fun env k v -> Common.prof "Model.eval_var" (fun () ->
  match get_var_def_opt k v env with
  | Some e -> eval_expr env k e
  | None -> invalid_arg ("Model.eval_var: undefined var: " ^ v))


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
  fun env params k a -> Common.prof "Model.eval_attr" (fun () ->
  match a with
  | U u -> eval_var params k u
  | E e -> eval_expr env k e)
		   
		   
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
     pp_attr Int height; print_string " x "; pp_attr Int width;
     print_string " at ("; pp_attr Int offset_i;
     print_string ", "; pp_attr Int offset_j;
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
  if c = c0
  then Result.Ok params
  else Result.Error "value mismatch"
     
let parse_attr : type a. env -> a kind -> a -> a attr -> params -> params parse_result =
  fun env k c0 a params -> Common.prof "Model.parse_attr" (fun () ->
  match a with
  | U u ->
     ( match get_var_def_opt k u params with
       | None -> Result.Ok (set_var_def k u (Const c0) params)
       | Some (Const c) -> parse_const k c0 c params
       | Some _ -> assert false ) (* only constants in params *)
  | E e ->
     let c = eval_expr env k e in
     parse_const k c0 c params)

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
	: (grid_data * Grid.Mask.t * Grid.part list) parse_result = Common.prof "Model.parse_grid_model" (fun () ->
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
	let new_mask = Grid.Mask.empty in
	let new_parts = [] in
	Result.Ok ({ params = (!new_params); delta = (!new_delta) }, new_mask, new_parts))
  | AddShape (sh,m1) ->
     parse_shape
       g sh env gd mask parts
       (fun (gd,mask,parts) ->
	parse_grid_model g m1 env gd mask parts))
and parse_shape g (sh : shape) env gd mask parts kont = Common.prof "Model.parse_shape" (fun () ->
  match sh with
  | Point {offset_i; offset_j; color} -> raise TODO
  | Rectangle {height; width;
	       offset_i; offset_j;
	       color; filled} ->
     let lr = Grid.rectangles g mask parts in
     let _, res = Common.prof "Model.parse_shape/fold" (fun () -> 
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
	 lr) in
     res)
	      
let read_grid (env : env) (g : Grid.t) (m : grid_model) : (env * grid_data) parse_result = Common.prof "Model.read_grid" (fun () ->
  let gd0 = { params = []; delta = [] } in
  let mask0 = Grid.Mask.full g.height g.width in
  let parts0 = Grid.segment_by_color g in
  Result.bind
    (parse_grid_model g m env gd0 mask0 parts0)
    (fun (gd,mask,parts) -> Result.Ok (env,gd)))

let read_grids (egrids: (env * Grid.t) list) (m : grid_model) : (env * grid_data) list option = Common.prof "Model.read_grids" (fun () ->
  let r_egds = List.map (fun (env,g) -> read_grid env g m) egrids in
  List.fold_right (* checking no errors, and aggregating *)
    (fun r_egd res ->
     match r_egd, res with
     | Result.Ok egd, Some egds -> Some (egd::egds)
     | _ -> None)
    r_egds (Some []))


let apply_model (m : model) (env : env) (g : Grid.t) : (Grid.t, string) Result.t = Common.prof "Model.apply_model" (fun () ->
  match read_grid env g m.input_pattern with
  | Result.Ok (_,gdi) ->
     (try
	 let envo = gdi.params in
	 Result.Ok (write_grid m.output_template envo grid_data0)
       with exn ->
	 Result.Error ("output writing failed: " ^ Printexc.to_string exn))
  | Result.Error msg -> Result.Error ("input reading failed: " ^ msg))

      
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

let env_size_of_egds egds =
  match egds with
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
  Mdl.Code.universal_int_star nb_pixels -. 1. (* how many delta pixels there are, removing invariant 1 bit that codes whethere there is a delta or not  *)
  +. Mdl.Code.comb nb_pixels area (* where they are *)
  +. float nb_pixels *. Mdl.Code.uniform (Grid.nb_color - 1) (* what are their color, different from the color generated by the model *)
    
let l_grid_data ~m ~(code_m : env -> params -> Mdl.bits) ~(hw : int attr * int attr) (env : env) (gd : grid_data) : Mdl.bits =
  let he, we = hw in
  let h = eval_attr env gd.params Int he in
  let w = eval_attr env gd.params Int we in
  code_m env gd.params
  +. l_grid_delta ~height:h ~width:w gd.delta

type 'a triple = 'a * 'a * 'a
		  
let l_grid_model_data (m : grid_model) (egds : (env * grid_data) list) : Mdl.bits triple (* model, data, model+data *) = Common.prof "Model.l_grid_model_data" (fun () ->
  let env_size = env_size_of_egds egds in
  let (l_m, code_m), hw = l_grid_model ~env_size m in
  let l_d =
    10. (* because given training examples are only a sample from a class of grids *)
    *. Mdl.sum egds (fun (env,gd) -> l_grid_data ~m ~code_m ~hw env gd) in
  l_m, l_d, l_m +. l_d)

let l_model_data (m : model) (egdis : (env * grid_data) list) (egdos : (env * grid_data) list) : Mdl.bits triple triple = Common.prof "Model.l_model_data" (fun () ->
  let egdis = Common.sub_list egdis 0 (List.length egdos) in
  (* to remove extra test inputs *)
  let lmi, ldi, lmdi =
    l_grid_model_data m.input_pattern egdis in
  let lmo, ldo, lmdo =
    l_grid_model_data m.output_template egdos in
  (lmi, lmo, lmi+.lmo), (ldi, ldo, ldi+.ldo), (lmdi, lmdo, lmdi+.lmdo))

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
  | RShape of int * shape

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
	      

let rec find_defs (egds : (env * grid_data) list) : def list =
  (* find if some gd param unknowns can be replaced
     by some expression over env vars *)
  assert (egds <> []);
  let es, gds = List.split egds in
  let ps = List.map (fun gd -> gd.params) gds in
  List.fold_left
    (fun defs (Def (k,u,_)) ->
     (* u-values *)
     let u_cs = List.map (fun p -> eval_var p k u) ps in
     match u_cs with
     | [] -> assert false
     | c0::cs1 ->
	if List.for_all (fun c1 -> c1 = c0) cs1
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
	  if e_cs = u_cs
	  then Some e
	  else None) with
  | None -> None
  | Some (e,_next) -> Some (Def (k,u,e))
       
let rec insert_a_shape ?(max_depth = 2) ?(depth = 0) gv m : (grid_refinement * Genvar.t * grid_model) Myseq.t =
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
     RShape (depth,sh), gv, AddShape (sh,m))
    (if depth >= max_depth
     then Myseq.empty
     else
       match m with
       | Background _ -> Myseq.empty
       | AddShape (sh1,m1) ->
	  insert_a_shape ~max_depth ~depth:(depth + 1) gv m1
	  |> Myseq.map (fun (r,gv,m1') -> (r, gv, AddShape (sh1,m1'))))

let grid_model_refinements (gv : Genvar.t) (m : grid_model) (egds : (env * grid_data) list) : (grid_refinement * Genvar.t * grid_model) Myseq.t =
  let defs = find_defs egds in
  if defs = []
  then insert_a_shape gv m
  else (
    Myseq.cons
      (RDefs defs, gv, subst_grid_model defs m)
      (insert_a_shape gv m))
		     
let learn_grid_model ~beam_width ~refine_degree ~env_size
		     (egrids : (env * Grid.t) list) (gv : Genvar.t)
    : ((grid_refinement * Genvar.t * grid_model) * (env * grid_data) list * Mdl.bits) list =
  Mdl.Strategy.beam
    ~beam_width
    ~refine_degree
    ~m0:(let gv,m = grid_model0 gv in
	 RGridInit, gv, m)
    ~data:(fun (r,gv,m) ->
	   read_grids egrids m)
    ~code:(fun (r,gv,m) egds ->
	   (*pp_grid_model m; print_newline ();*)
	   pp_grid_refinement r; print_newline ();
	   let lm, ld, lmd = l_grid_model_data m egds in
	   Printf.printf "DL = %.1f + %.1f = %.1f\n" lm ld lmd;
	   lmd)
    ~refinements:(fun (r,gv,m) egds ->
		  grid_model_refinements gv m egds)

let model_refinements (m : model) (egdis : (env * grid_data) list) (egdos : (env * grid_data) list) : (refinement * model) Myseq.t
  = Common.prof "Model.model_refinements" (fun () ->
  let ref_defis =
    let defis = find_defs egdis in
    if defis = []
    then Myseq.empty
    else Myseq.return
	   (Rinput (RDefs defis),
	    {m with
	      input_pattern = subst_grid_model defis m.input_pattern;
	      output_template = subst_grid_model defis m.output_template}) in
  let ref_defos =
    let defos = find_defs egdos in
    if defos = []
    then Myseq.empty
    else Myseq.return
	   (Routput (RDefs defos),
	    {m with
	      output_template = subst_grid_model defos m.output_template}) in
  let ref_shapis =
    insert_a_shape m.genvar m.input_pattern
    |> Myseq.map
	 (fun (gr,gv',mi') ->
	  (Rinput gr, {m with genvar=gv'; input_pattern=mi'})) in
  let ref_shapos =
    insert_a_shape m.genvar m.output_template
    |> Myseq.map
	 (fun (gr,gv',mo') ->
	  (Routput gr, {m with genvar=gv'; output_template=mo'})) in
  Myseq.concat [ref_defis; ref_shapis;
		ref_defos; ref_shapos])
	     
let learn_model
      ~beam_width ~refine_degree
      (gis_test : Grid.t list) (* train + test inputs *)
      (gos : Grid.t list) (* only train outputs *)
    : ((refinement * model) * ((env * grid_data) list * int * (env * grid_data) list) * Mdl.bits) list
  = Common.prof "Model.learn_model" (fun () ->
  let len_gos = List.length gos in
  let egis_test = List.map (fun gi -> env0, gi) gis_test in
  Mdl.Strategy.beam
    ~beam_width
    ~refine_degree
    ~m0:(RInit, model0)
    ~data:(fun (r,m) ->
	   try
	   match read_grids egis_test m.input_pattern with
	   | None -> None
	   | Some egdis_test ->
	      let egdis = Common.sub_list egdis_test 0 len_gos in
	      let egos =
		List.map2
		  (fun (envi,gdi) go -> gdi.params, go)
		  egdis gos in
	      match read_grids egos m.output_template with
	      | None -> None
	      | Some egdos ->
		 let env_size = env_size_of_egds egdos in
		 Some (egdis_test, env_size, egdos)
	   with exn ->
	     print_endline (Printexc.to_string exn);
	     pp_model m;
	     raise exn)
    ~code:(fun (r,m) (egdis_test,env_size,egdos) ->
	   pp_refinement r; print_newline ();
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     l_model_data m egdis_test egdos in
	   Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;
	   lmd)
    ~refinements:
    (fun (r,m) (egdis_test,env_size,egdos) ->
     model_refinements m egdis_test egdos))
    
(* naive *)
    
type t = unit
let learn (train : Task.pair list) : t = ()
