
open Task

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
  | Color : color kind
					    
type def =
  | DBool of bool var * bool expr (* flags *)
  | DInt of int var * int expr (* positions, sizes, and cardinals *)
  | DColor of color var * color expr

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
				

(* grids *)
				
type pixel = int * int * color (* x, y, col *)
				
type grid_model =
  | Background of { height: int expr;
		    width: int expr;
		    color: color expr;
		    layers: layer list }
and layer =
  | Single of shape
and shape =
  | Point of { offset_i: int expr;
	       offset_j: int expr;
	       color: color expr }
  | Rectangle of { height: int expr;
		   width: int expr;
		   offset_i: int expr;
		   offset_j: int expr;
		   color: color expr;
		   filled: bool expr }

type grid_data =
  { params: def list;
    delta: pixel list }

let rec apply_grid (m : grid_model) (params : def list) : grid =
  match m with
  | Background { height; width; color; layers } ->
     let height = eval_expr Int params height in
     let width = eval_expr Int params width in
     let matrix = Array.make_matrix height width (eval_expr Color params color) in
     let g : grid = { height; width; matrix } in
     layers |> List.iter (fun l -> apply_layer l params g);
     g
and apply_layer (l : layer) params g : unit =
  match l with
  | Single sh -> apply_shape sh params g
and apply_shape (sh : shape) params g : unit =
  match sh with
  | Point {offset_i; offset_j; color} ->
     let i = eval_expr Int params offset_i in
     let j = eval_expr Int params offset_j in
     let c = eval_expr Color params color in
     g.matrix.(i).(j) <- c
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
	   g.matrix.(i).(j) <- c
       done;
     done
	   

let write_grid (m : grid_model) (d : grid_data) : grid =
  let g = apply_grid m d.params in
  List.iter
    (fun (i,j,c) -> g.matrix.(i).(j) <- c)
    d.delta;
  g

type diff_grid =
  | Grid_size_mismatch of { src_height: int; src_width: int;
			    tgt_height: int; tgt_width: int }
  | Grid_diff_pixels of { height: int; width: int; pixels: pixel list }
							  
let diff_grid (source : grid) (target : grid) : diff_grid option =
  if source.height <> target.height || source.width <> target.width
  then Some (Grid_size_mismatch
	       { src_height = source.height; src_width = source.width;
		 tgt_height = target.height; tgt_width = target.width })
  else
    let height = source.height in
    let width = source.width in
    let res = ref [] in
    for i = 0 to height - 1 do
      for j = 0 to width - 1 do
	let src_c, tgt_c = source.matrix.(i).(j), target.matrix.(i).(j) in
	if src_c <> tgt_c then
	  res := (i,j,tgt_c) :: !res
      done
    done;
    if !res = []
    then None
    else Some (Grid_diff_pixels {height; width; pixels=(!res)})

  
(* input->output models *)
    
type model =
  { input_pattern : grid_model;
    output_template : grid_model
  }


(* naive *)
    
type t = unit
let learn (train : Task.pair list) : t = ()
