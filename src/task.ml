(* types and functions relative to ARC tasks *)
(* as represented in JSON files *)
		     
type pair = { input : Grid.t; output : Grid.t }
type task = { train : pair list; test : pair list }

let rec task_of_json : Yojson.Safe.t -> task = function
  | `Assoc pairs ->
     (match List.assoc_opt "train" pairs, List.assoc_opt "test" pairs with
      | Some (`List trains), Some (`List tests) ->
         { train = List.map pair_of_json trains;
           test = List.map pair_of_json tests }
      | _ -> invalid_arg "Invalid JSON task")
  | _ -> invalid_arg "Invalid JSON task"
and pair_of_json : Yojson.Safe.t -> pair = function
  | `Assoc pairs ->
     (match List.assoc_opt "input" pairs, List.assoc_opt "output" pairs with
      | Some input, Some output ->
         { input = grid_of_json input;
           output = grid_of_json output }
      | _ -> invalid_arg "Invalid JSON pair")
  | _ -> invalid_arg "Invalid JSON pair"
and grid_of_json : Yojson.Safe.t -> Grid.t = function
  | `List (`List row::_ as rows) ->
     let height = List.length rows in
     let width = List.length row in
     let grid = Grid.make height width 0 in
     List.iteri
       (fun i ->
	function
	| `List cells ->
	   List.iteri
	     (fun j ->
	      function
	      | `Int col -> grid#set_pixel i j col
	      | _ -> invalid_arg "Invalid JSON grid color")
	     cells
	| _ -> invalid_arg "Invalid JSON grid row")
       rows;
     grid
  | _ -> invalid_arg "Invalid JSON grid"

let from_file (filename : string) : task =
  let json = Yojson.Safe.from_file filename in
  task_of_json json

let rec pp_task task =
  print_endline "## train pairs";
  pp_pair_list task.train;
  print_endline "## test pairs";
  pp_pair_list task.test
and pp_pair_list pairs =
  List.iteri
    (fun i pair ->
     print_string "# pair "; print_int i; print_newline ();
     pp_pair pair; print_newline ())
    pairs
and pp_pair pair =
  (* TODO: use Grid.pp_grids *)
  Grid.pp_grid pair.input;
  print_endline "===>";
  Grid.pp_grid pair.output
