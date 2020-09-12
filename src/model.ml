
open Task

exception TODO
       
(* vars and expressions *)
       
type 'a var = string

type _ expr =
  | Var: 'a var -> 'a expr
  | Const : 'a -> 'a expr
  | Plus: int expr * int expr -> int expr
  | If: bool expr * 'a expr * 'a expr -> 'a expr

type _ kind =
  | Bool : bool kind
  | Int : int kind
  | Color : Grid.color kind
					    
type def =
  | DBool of bool var * bool expr (* flags *)
  | DInt of int var * int expr (* positions, sizes, and cardinals *)
  | DColor of Grid.color var * Grid.color expr

let dbool v b = DBool (v, Const b)
let dint v i = DInt (v, Const i)
let dcolor v c = DColor (v, Const c)
				
exception Unbound_var of string
				
let rec get_var_def : type a. a kind -> a var -> def list -> a expr =
  fun k v defs ->
  match k, defs with
  | _, [] -> raise (Unbound_var v)
  | Bool, DBool (v1,e)::_ when v1 = v -> e
  | Int, DInt (v1,e)::_ when v1 = v -> e
  | Color, DColor (v1,e)::_ when v1 = v -> e
  | _, _::defs1 -> get_var_def (k : a kind) v defs1

let set_var_def : type a. a kind -> a var -> a expr -> def list -> def list =
  fun k v e defs ->
  match k with
  | Bool -> DBool (v,e)::defs
  | Int -> DInt (v,e)::defs
  | Color -> DColor (v,e)::defs
			       
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

type grid_data =
  { params: def list;
    delta: Grid.pixel list }

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
     ( try
	 let e1 = get_var_def k v params in
	 parse_expr k c0 e1 params
       with
       | Unbound_var _ ->
	  Result.Ok (set_var_def k v (Const c0) params) )
  | _ -> invalid_arg "Unexpected expression in grid model"

let rec parse_grid_model (g : Grid.t) (m : grid_model) (gd : grid_data) (mask : Grid.Mask.t) : (grid_data * Grid.Mask.t) parse_result =
  match m with
  | Background {height; width; color} ->
     let new_params = ref gd.params in
     let new_delta = ref gd.delta in
     Grid.iter_pixels
       (fun i j c ->
	if Grid.Mask.mem i j mask
	then
	  match parse_expr Color c color !new_params with
	  (* TODO: choose majority color instead of first matching *)
	  | Result.Ok params -> new_params := params
	  | Result.Error _ -> new_delta := (i,j,c)::!new_delta)
       g;
     let new_mask = Grid.Mask.empty in
     Result.Ok ({ params = (!new_params); delta = (!new_delta) }, new_mask)
  | AddShape (sh,m1) ->
     Result.bind
       (parse_shape g sh gd mask)
       (fun (gd,mask) -> parse_grid_model g m1 gd mask)
and parse_shape g (sh : shape) gd mask =
  match sh with
  | Point {offset_i; offset_j; color} -> raise TODO
  | Rectangle {height; width; offset_i; offset_j; color; filled} -> raise TODO
	      
let read_grid (g : Grid.t) (m : grid_model) : grid_data parse_result =
  let mask0 = Grid.Mask.full g.height g.width in
  let gd0 = { params = []; delta = [] } in
  Result.bind
    (parse_grid_model g m gd0 mask0)
    (fun (gd,mask) -> Result.Ok gd)
	      
  
(* input->output models *)
    
type model =
  { input_pattern : grid_model;
    output_template : grid_model
  }


(* naive *)
    
type t = unit
let learn (train : Task.pair list) : t = ()
