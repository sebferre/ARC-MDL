(* types and functions relative to ARC tasks *)
(* as represented in JSON files *)

type color = int

let black = 0
let blue = 1
let red = 2
let green = 3
let yellow = 4
let grey = 5
let pink = 6
let orange = 7
let cyan = 8
let brown = 9
	       
type grid = { width : int; height : int; matrix : color array array }
type pair = { input : grid; output : grid }
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
     { input = grid_of_json input;
       output = grid_of_json output }
  | _ -> invalid_arg "Invalid JSON pair"
and grid_of_json : Yojson.Safe.t -> grid = function
  | `List (`List row::_ as rows) ->
     let height = List.length rows in
     let width = List.length row in
     let matrix = Array.of_list (List.map row_of_json rows) in
     { height; width; matrix }
  | _ -> invalid_arg "Invalid JSON grid"
and row_of_json : Yojson.Safe.t -> color array = function
  | `List cells ->
     Array.of_list (List.map color_of_json cells)
  | _ -> invalid_arg "Invalid JSON grid row"
and color_of_json : Yojson.Safe.t -> color = function
  | `Int i -> i
  | _ -> invalid_arg "Invalid JSON grid color"

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
  pp_grid pair.input;
  print_endline "===>";
  pp_grid pair.output
and pp_grids grids =
  let max_height =
    List.fold_left (fun res g -> max res g.height) 0 grids in
  for i = 0 to max_height - 1 do
    List.iter
      (fun g ->
       for j = 0 to g.width - 1 do
	 if i < g.height
	 then pp_color g.matrix.(i).(j)
	 else pp_blank ()
       done;
       print_string "   ")
      grids;
    print_newline ()
  done
and pp_grid grid =
  for i = 0 to grid.height - 1 do
    for j = 0 to grid.width - 1 do
      pp_color grid.matrix.(i).(j)
    done;
    print_newline ()
  done
and pp_blank () =
  print_string "  "
and pp_color c =
  let open ANSITerminal in
  let style, str =
    match c with
    | 0 -> [on_black], "  "
    | 1 -> [on_blue], "  "
    | 2 -> [on_red], "  "
    | 3 -> [on_green], "  "
    | 4 -> [on_yellow], "  "
    | 5 -> [black; on_white], "##"
    | 6 -> [on_magenta], "  "
    | 7 -> [red; on_yellow], "##"
    | 8 -> [on_cyan], "  "
    | 9 -> [green; on_red], "##"
    | _ -> invalid_arg "Invalid color code" in
  print_string style str
