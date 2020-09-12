
open Task
open Model

type task_solution =
  { name : string;
    task : task; (* the task description *)
    model : model; (* expected output of learning *)
    train_data : grid_data list; (* expected output of reading train input grids *)
    test_data : grid_data list; (* expected output of reading test input grids *)
  }

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

(* TODO: check_read_input_grid *)

(* TODO: check_learn_model *)
		      
let check_task (tsol : task_solution) : unit =
  Printf.printf "Chacking task %s: %d train, %d test\n"
		tsol.name (List.length tsol.task.train) (List.length tsol.task.test);
  let cpt = ref 0 in
  List.iter2
    (fun {input; output} gd ->
     incr cpt;
     let _ =
       let parts = Grid.segment_by_color input in
       Grid.pp_parts input parts in
     (* checking that input pattern and grid data match input grid *)
     check_write_grid (tsol.name ^ "-input--" ^ string_of_int !cpt)
		tsol.model.input_pattern gd input;
     (* checking that output template and grid data match output grid *)
     check_write_grid (tsol.name ^ "-output-" ^ string_of_int !cpt)
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
			     color = Const 0 }));
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
