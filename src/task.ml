(* types and functions relative to ARC tasks *)
(* as represented in JSON files *)
		     
type pair = { input : Grid.t; output : Grid.t }
type task = { train : pair list; test : pair list }

let rec task_of_json : Yojson.Safe.t -> task = function
  | `Assoc ["train", `List trains; "test", `List tests]
  | `Assoc ["test", `List tests; "train", `List trains] ->
     { train = List.map pair_of_json trains;
       test = List.map pair_of_json tests }
  | _ -> invalid_arg "Invalid JSON task"
and pair_of_json : Yojson.Safe.t -> pair = function
  | `Assoc ["input", input; "output", output]
  | `Assoc ["output", output; "input", input] ->
     { input = Grid.of_json input;
       output = Grid.of_json output }
  | _ -> invalid_arg "Invalid JSON pair"

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
