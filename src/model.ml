
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

type _ expr =
  | Var: 'a var -> 'a expr
  | Const : 'a -> 'a expr
  | Plus: int expr * int expr -> int expr
  | If: bool expr * 'a expr * 'a expr -> 'a expr
							    
type _ kind =
  | Bool : bool kind (* flags *)
  | Int : int kind (* positions, lengths *)
  | Color : Grid.color kind (* colors *)

type (_,_) kind_comp =
  | Same : ('a,'a) kind_comp
  | Diff : ('a,'b) kind_comp

let rec pp_expr : type a. a kind -> a expr -> unit =
  fun k e ->
  match k, e with
  | _, Var v -> print_string v
  | Bool, Const c -> print_string (if c then "true" else "false")
  | Int, Const c -> print_int c
  | Color, Const c -> Grid.pp_color c
  | _, Plus (e1,e2) -> pp_expr k e1; print_char '+'; pp_expr k e2
  | _, If (cond,e1,e2) ->
     print_string "if "; pp_expr Bool cond;
     print_string " then "; pp_expr k e1;
     print_string " else "; pp_expr k e2

exception ComplexExpr (* when neither a Var nor a Const *)
				    
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
			       
let rec eval_expr : type a. a kind -> def list -> a expr -> a =
  fun k params e ->
  match e with
  | Var v ->
     let e = get_var_def k v params in
     eval_expr k params e
  | Const c -> c
  | Plus (e1,e2) -> eval_expr k params e1 + eval_expr k params e2
  | If (cond,e1,e2) ->
     if eval_expr Bool params cond
     then eval_expr k params e1
     else eval_expr k params e2

let eval_var : type a. a kind -> def list -> a var -> a =
  fun k params v ->
  let e = get_var_def k v params in
  eval_expr k params e

let rec subst_expr : type a. def list -> a kind -> a expr -> a expr =
  fun params k e ->
  match e with
  | Var v ->
     ( match get_var_def_opt k v params with
       | Some e0 -> e0
       | None -> e )
  | Const _ -> e
  | Plus (e1,e2) ->
     Plus (subst_expr params k e1,
	   subst_expr params k e2)
  | If (cond,e1,e2) ->
     If (subst_expr params Bool cond,
	 subst_expr params k e1,
	 subst_expr params k e2)

(* grid models *)
(* ----------- *)
	    
type grid_model =
  | Background of { height: int expr; (* one-color background *)
		    width: int expr;
		    color: Grid.color expr }
  | AddShape of shape * grid_model (* shape on top of grid_model *)
and shape =
  | Point of { offset_i: int expr;
	       offset_j: int expr;
	       color: Grid.color expr }
  | Rectangle of { height: int expr;
		   width: int expr;
		   offset_i: int expr;
		   offset_j: int expr;
		   color: Grid.color expr;
		   filled: bool expr }

let rec pp_grid_model = function
  | Background {height; width; color} ->
     print_string "a background ";
     pp_expr Int height; print_char 'x'; pp_expr Int width;
     print_string " with color ";
     pp_expr Color color
  | AddShape (sh,m1) ->
     pp_shape sh;
     print_string "\non top of ";
     pp_grid_model m1
and pp_shape = function
  | Point {offset_i; offset_j; color} ->
     print_string "a point at (";
     pp_expr Int offset_i; print_char ','; pp_expr Int offset_j;
     print_string ") with color "; pp_expr Color color
  | Rectangle {height; width; offset_i; offset_j; color; filled} ->
     print_string "a rectangle ";
     pp_expr Int height; print_char 'x'; pp_expr Int width;
     print_string " at ("; pp_expr Int offset_i;
     print_string ","; pp_expr Int offset_j;
     print_string ") with color "; pp_expr Color color;
     print_string " and filling "; pp_expr Bool filled

let rec subst_grid_model (params : def list) : grid_model -> grid_model =
  function
  | Background {height; width; color} ->
     Background {height = subst_expr params Int height;
		 width = subst_expr params Int width;
		 color = subst_expr params Color color}
  | AddShape (sh,m1) ->
     AddShape (subst_shape params sh,
	       subst_grid_model params m1)
and subst_shape (params : def list) : shape -> shape =
  function
  | Point {offset_i; offset_j; color} ->
     Point {offset_i = subst_expr params Int offset_i;
	    offset_j = subst_expr params Int offset_j;
	    color = subst_expr params Color color}
  | Rectangle {height; width; offset_i; offset_j; color; filled} ->
     Rectangle {height = subst_expr params Int height;
		width = subst_expr params Int width;
		offset_i = subst_expr params Int offset_i;
		offset_j = subst_expr params Int offset_j;
		color = subst_expr params Color color;
		filled = subst_expr params Bool filled}
			      
	   
type grid_data =
  { params: def list;
    delta: Grid.pixel list }

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
    input_pattern : grid_model; (* only consts and vars allowed *)
    output_template : grid_model (* complex expressions allowed *)
  }

let pp_model m =
  print_endline "CONSTRUCT (Mo)";
  pp_grid_model m.output_template; print_newline ();
  print_endline "WHERE (Mi)";
  pp_grid_model m.input_pattern; print_newline ()
				 
(* writing grids from models *)
    
let rec apply_grid_model (m : grid_model) (params : def list) : Grid.t =
  match m with
  | Background { height; width; color } ->
     Grid.make
       (eval_expr Int params height)
       (eval_expr Int params width)
       (eval_expr Color params color)
  | AddShape (sh,m1) ->
     let g = apply_grid_model m1 params in
     apply_shape sh params g;
     g
and apply_shape (sh : shape) params g : unit =
  match sh with
  | Point {offset_i; offset_j; color} ->
     let i = eval_expr Int params offset_i in
     let j = eval_expr Int params offset_j in
     let c = eval_expr Color params color in
     Grid.set_pixel g i j c
  | Rectangle {height; width; offset_i; offset_j; color; filled} ->
     let h = eval_expr Int params height in
     let w = eval_expr Int params width in
     let mini = eval_expr Int params offset_i in
     let minj = eval_expr Int params offset_j in
     let c = eval_expr Color params color in
     let f = eval_expr Bool params filled in
     let maxi = mini + h - 1 in
     let maxj = minj + w - 1 in
     for i = mini to maxi do
       for j = minj to maxj do
	 if f || i=mini || i=maxi || j=minj || j=maxj then
	   Grid.set_pixel g i j c
       done;
     done
	   
let write_grid (m : grid_model) (d : grid_data) : Grid.t =
  let g = apply_grid_model m d.params in
  List.iter
    (fun (i,j,c) -> Grid.set_pixel g i j c)
    d.delta;
  g


(* reading grids with models *)

type 'a parse_result = ('a, string) Result.t
	      
let rec parse_expr : type a. a kind -> a -> a expr -> def list -> def list parse_result =
  fun k c0 e params ->
  match e with
  | Const c ->
     if c0 = c
     then Result.Ok params
     else Result.Error "value mismatch"
  | Var v ->
     ( match get_var_def_opt k v params with
       | Some e1 -> parse_expr k c0 e1 params
       | None -> Result.Ok (set_var_def k v (Const c0) params) )
  | _ -> raise ComplexExpr

let rec parse_expr_list : type a. a kind -> (a * a expr) list -> def list -> def list parse_result =
  fun k lce params ->
  match lce with
  | [] -> Result.Ok params
  | (c,e)::lce1 ->
     Result.bind
       (parse_expr k c e params)
       (fun params -> parse_expr_list k lce1 params)
		     
let rec parse_grid_model (g : Grid.t) (m : grid_model) (gd : grid_data) (mask : Grid.Mask.t) (parts : Grid.part list) : (grid_data * Grid.Mask.t * Grid.part list) parse_result =
  match m with
  | Background {height; width; color} ->
     Result.bind
       (parse_expr_list Int [g.height, height; g.width, width] gd.params)
       (fun new_params ->
	let new_params = ref new_params in
	let new_delta = ref gd.delta in
	let bc = (* background color *)
	  try eval_expr Color !new_params color
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
	    match parse_expr Color bc color !new_params with
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
       g sh gd mask parts
       (fun (gd,mask,parts) ->
	parse_grid_model g m1 gd mask parts)
and parse_shape g (sh : shape) gd mask parts kont =
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
	      (parse_expr_list
		 Int
		 [r.Grid.height, height;
		  r.width, width;
		  r.offset_i, offset_i;
		  r.offset_j, offset_j]
		 gd.params)
	      (fun new_params ->
	       Result.bind
		 (parse_expr Color r.color color new_params)
		 (fun new_params ->
		  Result.bind
		    (parse_expr Bool true filled new_params)
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
	      
let read_grid (g : Grid.t) (m : grid_model) : grid_data parse_result =
  let gd0 = { params = []; delta = [] } in
  let mask0 = Grid.Mask.full g.height g.width in
  let parts0 = Grid.segment_by_color g in
  Result.bind
    (parse_grid_model g m gd0 mask0 parts0)
    (fun (gd,mask,parts) -> Result.Ok gd)

let read_grids (grids: Grid.t list) (m : grid_model) : grid_data list option =
  let r_gds = List.map (fun g -> read_grid g m) grids in
  List.fold_right (* checking no errors, and aggregating *)
    (fun r_gd res ->
     match r_gd, res with
     | Result.Ok gd, Some gds -> Some (gd::gds)
     | _ -> None)
    r_gds (Some [])
    
(* description lengths *)

type 'a code = 'a -> Mdl.bits

let void_code : 'a code = fun _ -> 0.

let l_bool : bool -> Mdl.bits =
  fun _ -> 1.
				     
let l_color : Grid.color -> Mdl.bits =
  fun _ -> Mdl.Code.uniform Grid.nb_color
let l_background_color : Grid.color -> Mdl.bits =
  function
  | 0 -> Mdl.Code.usage 0.91
  | c ->
     if c > 0 && c < 10 (* 9 colors *)
     then Mdl.Code.usage 0.01
     else invalid_arg "Unexpected color"


type 'a staged_code = Mdl.bits (* model proper *) * 'a code (* data dependent *)

let (+!) l1 (l2,c2) = (l1+.l2, c2)
let (+?) (l1,c1) (l2,c2) = (l1+.l2, (fun params -> c1 params +. c2 params))
		      
let l_expr : type a. a kind -> code:a code -> a expr -> def list staged_code =
  fun k ~code e ->
  match k, e with
  | _, Var v ->
     1.,
     (fun params -> code (eval_var k params v))
  | Bool, Const c ->
     1. +. code c,
     void_code
  | Int, Const c ->
     1. +. code c,
     void_code
  | Color, Const c ->
     1. +. code c,
     void_code
  | _ -> raise ComplexExpr

(* coding offset given bound *)
let l_position ~(bound : int expr) (offset : int expr) : def list staged_code =
  (* TODO: is there a better encoding of choice between Var and Const ? *)
  match bound, offset with
  | Const b, Const o ->
     1. +. Mdl.Code.uniform b,
     void_code
  | Var _, Const o ->
     1. +. Mdl.Code.universal_int_star o,
     void_code
  | _, Var _ ->
     1.,
     (fun params ->
      let b = eval_expr Int params bound in
      Mdl.Code.uniform b)
  | _ -> raise ComplexExpr

(* coding offset and size given bound *)
let l_slice ~(bound : int expr) ~(offset : int expr) ~(size: int expr) : def list staged_code =
  match bound, offset, size with
  | Const b, Const o, Const s ->
     2. +. Mdl.Code.uniform (b * (b + 1) / 2), (* number of ways to choose o in [0..b-1], and s in [1..b] *)
     void_code  
  | Const b, Const o, Var _ ->
     2. +. Mdl.Code.uniform b,
     (fun params -> Mdl.Code.uniform (b - o))
  | Const b, Var _, Const s ->
     2. +. Mdl.Code.uniform b,
     (fun params -> Mdl.Code.uniform (b - s + 1))
  | Var _, Const o, Const s ->
     2. +. Mdl.Code.universal_int_star o +. Mdl.Code.universal_int_plus s,
     void_code
  | Var vb, Const o, Var _ ->
     2. +. Mdl.Code.universal_int_star o,
     (fun params ->
      let b = eval_var Int params vb in
      Mdl.Code.uniform (b - o))
  | Var vb, Var _, Const s ->
     2. +. Mdl.Code.universal_int_plus s,
     (fun params ->
      let b = eval_var Int params vb in
      Mdl.Code.uniform (b - s + 1))				       
  | _, Var _, Var _ ->
     2.,
     (fun params ->
      let b = eval_expr Int params bound in
      Mdl.Code.uniform (b * (b + 1) / 2))
  | _ -> raise ComplexExpr
		   
let rec l_grid_model : grid_model -> def list staged_code * (int expr * int expr) =
  function
  | Background {height; width; color} ->
     let l_code =
       Mdl.Code.usage 0.5
       +! l_expr Int height ~code:Mdl.Code.universal_int_plus
       +? l_expr Int width ~code:Mdl.Code.universal_int_plus
       +? l_expr Color color ~code:l_background_color in
     l_code, (height,width)
  | AddShape (sh,m1) ->
     let l_code1, hw1 = l_grid_model m1 in
     let l_code =
       Mdl.Code.usage 0.5
       +! l_code1
       +? l_shape ~hw:hw1 sh in
     l_code, hw1
and l_shape ~(hw : int expr * int expr) : shape -> def list staged_code =
  let h, w = hw in
  function
  | Point {offset_i; offset_j; color} ->
     Mdl.Code.usage 0.
     +! l_position ~bound:h offset_i
     +? l_position ~bound:w offset_j
     +? l_expr Color color ~code:l_color
  | Rectangle {height; width; offset_i; offset_j; color; filled} ->
     Mdl.Code.usage 1.
     +! l_slice ~bound:h ~size:height ~offset:offset_i
     +? l_slice ~bound:w ~size:width ~offset:offset_j
     +? l_expr Color color ~code:l_color
     +? l_expr Bool filled ~code:l_bool
	   
	
let l_grid_delta ~(height : int) ~(width : int) (pixels : Grid.pixel list) : Mdl.bits =
  let area = height * width in
  let nb_pixels = List.length pixels in
  Mdl.Code.universal_int_star nb_pixels (* how many delta pixels there are *)
  +. Mdl.Code.comb nb_pixels area (* where they are *)
  +. float nb_pixels *. Mdl.Code.uniform (Grid.nb_color - 1) (* what are their color, different from the color generated by the model *)
    
let l_grid_data ~m ~(code_m : def list code) ~(hw : int expr * int expr) (gd : grid_data) : Mdl.bits =
  let he, we = hw in
  let h = eval_expr Int gd.params he in
  let w = eval_expr Int gd.params we in
  code_m gd.params
  +. l_grid_delta ~height:h ~width:w gd.delta

let l_grid_model_data (m : grid_model) (gds : grid_data list) : Mdl.bits * Mdl.bits * Mdl.bits (* model, data, model+data *) =
  let (l_m, code_m), hw = l_grid_model m in
  let l_d = Mdl.sum gds (fun gd -> l_grid_data ~m ~code_m ~hw gd) in
  l_m, l_d, l_m +. l_d


(* learning *)

let grid_model0 gv =
  let vH, gv = Genvar.add "H" gv in
  let vW, gv = Genvar.add "W" gv in
  let vC, gv = Genvar.add "C" gv in
  let m =
    Background { height = Var vH;
		 width = Var vW;
		 color = Var vC } in
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
     let sh = Rectangle {height=Var vH; width=Var vW;
			 offset_i=Var vI; offset_j=Var vJ;
			 color=Var vC; filled=Var vF} in
     gv, AddShape (sh,m))
    (if max_depth = 0
     then Myseq.empty
     else
       match m with
       | Background _ -> Myseq.empty
       | AddShape (sh1,m1) ->
	  insert_a_shape ~max_depth:(max_depth - 1) gv m1
	  |> Myseq.map (fun (gv,m1') -> (gv, AddShape (sh1,m1'))))
       
let grid_model_refinements (gv : Genvar.t) (m : grid_model) (gds : grid_data list) : (Genvar.t * grid_model) Myseq.t =
  let common_params =
    gds |> List.map (fun gd -> gd.params) |> inter_defs in
  if common_params = []
  then insert_a_shape gv m
  else (
    Myseq.cons
      (gv, subst_grid_model common_params m)
      (insert_a_shape gv m))
		     
let learn_grid_model ~beam_width ~refine_degree
		     (grids : Grid.t list) (gv : Genvar.t)
    : ((Genvar.t * grid_model) * grid_data list * Mdl.bits) list =
  Mdl.Strategy.beam
    ~beam_width
    ~refine_degree
    ~m0:(grid_model0 gv)
    ~data:(fun (gv,m) ->
	   read_grids grids m)
    ~code:(fun (gv,m) gds ->
	   (*pp_grid_model m; print_newline ();*)
	   let _, _, l = l_grid_model_data m gds in
	   Printf.printf "DL = %.1f\n" l;
	   l)
    ~refinements:(fun (gv,m) gds ->
		  grid_model_refinements gv m gds)
		     
(* naive *)
    
type t = unit
let learn (train : Task.pair list) : t = ()
