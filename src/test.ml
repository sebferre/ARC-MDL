
open Task
open Model

type task_solution =
  { name : string;
    task : task; (* the task description *)
    model : model; (* expected output of learning *)
    train_data : grid_data list; (* expected output of reading train input grids *)
    test_data : grid_data list; (* expected output of reading test input grids *)
  }

let grid_model0 =
  Background { height = Var "H";
	       width = Var "W";
	       color = Const 0 }
let model0 =
  { input_pattern = grid_model0;
    output_template = grid_model0 }
    
let print_grid_mismatch name ~grid ~derived_grid : unit =
  let diff = Grid.diff derived_grid grid in
  match diff with
  | None ->
     Printf.printf "Grid %s: OK\n" name
  | Some (Grid_size_mismatch {src_height; src_width; tgt_height; tgt_width}) ->
     Printf.printf "Grid %s: size mismatch, %dx%d instead of %dx%d\n"
		   name src_height src_width tgt_height tgt_width
  | Some (Grid_diff_pixels {height; width; pixels}) ->
     Printf.printf "Grid %s: %d wrong pixels (generated / expected)\n" name (List.length pixels);
     Grid.pp_grids [derived_grid; grid]
    
let check_write_grid grid_name grid_model grid_data grid =
  let derived_grid = write_grid grid_model grid_data in
  print_grid_mismatch grid_name ~grid ~derived_grid

(* check_read_input_grid *)

let print_grid_data_mismatch grid_name grid ~data ~parsed_data : unit =
  let params = List.sort Stdlib.compare data.params in
  let parsed_params = List.sort Stdlib.compare parsed_data.params in
  let delta = List.sort Stdlib.compare data.delta in
  let parsed_delta = List.sort Stdlib.compare parsed_data.delta in
  if params = parsed_params && delta = parsed_delta
  then Printf.printf "Grid %s: OK\n" grid_name
  else (
    Printf.printf "Grid %s: mismatch\n" grid_name;
    Grid.pp_grid grid;
    print_endline "Expected grid data";
    Model.pp_grid_data data;
    print_endline "Parsed grid data";
    Model.pp_grid_data parsed_data
  )
		      
let check_read_input_grid grid_name grid grid_model grid_data =
  let gd = Model.read_grid grid grid_model in
  print_grid_data_mismatch grid_name grid ~data:grid_data ~parsed_data:gd


(* TODO: check_learn_model *)

let print_dl_grids name grid_model grids : unit =
  let gds = List.map (fun g -> Model.read_grid g grid_model) grids in
  let lm, ld, lmd = Model.l_grid_model_data grid_model gds in
  Printf.printf "DL %s: L = %.1f + %.1f = %.1f\n" name lm ld lmd
  		   
let print_dl name model train : unit =
  let inputs = List.map (fun t -> t.Task.input) train in
  let outputs = List.map (fun t -> t.Task.output) train in
  print_dl_grids "input  with M0" grid_model0 inputs;
  print_dl_grids "input  with Mi" model.input_pattern inputs;
  print_dl_grids "input  with Mo" model.output_template inputs;
  print_dl_grids "output with M0" grid_model0 outputs;
  print_dl_grids "output with Mo" model.output_template outputs;
  print_dl_grids "output with Mi" model.input_pattern outputs
  
			   
(* check task *)
			   
let check_task (tsol : task_solution) : unit =
  Printf.printf "Checking task %s: %d train, %d test\n"
		tsol.name (List.length tsol.task.train) (List.length tsol.task.test);
  (* checking that input reading match input data *)
  pp_model tsol.model;
  print_dl tsol.name tsol.model tsol.task.train;
  let cpt = ref 0 in
  List.iter2
    (fun {input; output} gd ->
     incr cpt;
     (* displaying input grid parts *)
     (*Grid.pp_parts input (Grid.segment_by_color input);*)
     (* displaying description lengths *)
     check_read_input_grid
       (tsol.name ^ "-read--input--" ^ string_of_int !cpt)
       input tsol.model.input_pattern gd;
     (* checking that input pattern and grid data match input grid *)
     check_write_grid
       (tsol.name ^ "-write-input--" ^ string_of_int !cpt)
       tsol.model.input_pattern gd input;
     (* checking that output template and grid data match output grid *)
     check_write_grid
       (tsol.name ^ "-write-output-" ^ string_of_int !cpt)
       tsol.model.output_template gd output)
    (tsol.task.train @ tsol.task.test)
    (tsol.train_data @ tsol.test_data)


(* ============================================================ *)

let file_of_name name =
  "/local/ferre/data/tasks/ARC/data/training/" ^ name ^ ".json"
    
let tsol_ba97ae07 =
  let name = "ba97ae07" in
  { name;
    task = from_file (file_of_name name);
    model =
      { input_pattern =
	  AddShape
	    (Rectangle { height = Var "H2"; width = Var "W2";
			 offset_i = Var "I2"; offset_j = Var "J2";
			 color = Var "C2"; filled = Const true },
	     AddShape
	       (Rectangle { height = Var "H1"; width = Var "W1";
			    offset_i = Var "I1"; offset_j = Var "J1";
			    color = Var "C1"; filled = Const true },
		Background { height = Var "H";
			     width = Var "W";
			     color = Const Grid.black }));
	output_template = (* swapping the two rectangles *)
	  AddShape
	    (Rectangle { height = Var "H1"; width = Var "W1";
			 offset_i = Var "I1"; offset_j = Var "J1";
			 color = Var "C1"; filled = Const true },
	     AddShape
	       (Rectangle { height = Var "H2" (* error *); width = Var "W2";
			    offset_i = Var "I2"; offset_j = Var "J2";
			    color = Var "C2"; filled = Const true },
		Background { height = Var "H";
			     width = Var "W";
			     color = Const 0 }));
      };
    train_data =
      [ { params = [ dint "H" 13; dint "W" 13;
		     dint "H1" 3; dint "W1" 13; dint "I1" 3; dint "J1" 0; dcolor "C1" Grid.green;
		     dint "H2" 13; dint "W2" 2; dint "I2" 0; dint "J2" 3; dcolor "C2" Grid.cyan ];
	  delta = [] };
	{ params = [ dint "H" 7; dint "W" 9;
		     dint "H1" 7; dint "W1" 2; dint "I1" 0; dint "J1" 2; dcolor "C1" Grid.pink;
		     dint "H2" 1; dint "W2" 9; dint "I2" 3; dint "J2" 0; dcolor "C2" Grid.blue ];
	  delta = [] };
	{ params = [ dint "H" 8; dint "W" 7;
		     dint "H1" 8; dint "W1" 1; dint "I1" 0; dint "J1" 2; dcolor "C1" Grid.blue;
		     dint "H2" 1; dint "W2" 7; dint "I2" 3; dint "J2" 0; dcolor "C2" Grid.orange ];
	  delta = [] };
	{ params = [ dint "H" 8; dint "W" 6;
		     dint "H1" 1; dint "W1" 6; dint "I1" 4; dint "J1" 0; dcolor "C1" Grid.red;
		     dint "H2" 8; dint "W2" 1; dint "I2" 0; dint "J2" 1; dcolor "C2" Grid.green ];
	  delta = [] } ];
    test_data =
      [ { params = [ dint "H" 11; dint "W" 6;
		     dint "H1" 2; dint "W1" 6; dint "I1" 2; dint "J1" 0; dcolor "C1" Grid.grey;
		     dint "H2" 11; dint "W2" 2; dint "I2" 0; dint "J2" 2; dcolor "C2" Grid.yellow ];
	  delta = [] } ];
  }
let _ = check_task tsol_ba97ae07
