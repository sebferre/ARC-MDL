
open Task
open Model

type task_solution =
  { name : string;
    task : task; (* the task description *)
    model : model; (* expected output of learning *)
    train_data : grid_data list; (* expected output of reading train input grids *)
    test_data : grid_data list; (* expected output of reading test input grids *)
  }

(* building intermediate results for writing, parsing, and learning *)

type 'a train_test = 'a * 'a
let both f (train,test) = (f train, f test)
    
let tsol_gis tsol : Grid.t list train_test = (* train+test input grids *)
  tsol.task.train |> List.map (fun pair -> pair.input),
  tsol.task.test |> List.map (fun pair -> pair.input)
  
let tsol_gos tsol : Grid.t list train_test = (* train+test output grids *)
  tsol.task.train |> List.map (fun pair -> pair.output),
  tsol.task.test |> List.map (fun pair -> pair.output)

let tsol_egis tsol : (env * Grid.t) list train_test = (* train+test input env-grids *)
  tsol_gis tsol |> both (List.map (fun g -> (env0,g))) (* inputs have empty env *)

let tsol_egos tsol : (env * Grid.t) list train_test = (* train+test output env-grids *)
  let f gdis gos =
    List.map2
      (fun gdi go -> (gdi.params, go)) (* outputs have input params as env *)
      gdis gos in
  let gos_train, gos_test = tsol_gos tsol in
  f tsol.train_data gos_train,
  f tsol.test_data gos_test

let tsol_egdis tsol : (env * grid_data) list train_test = (* train+test input env-gds *)
  tsol.train_data |> List.map (fun gdi -> env0, gdi), (* inputs have empty env *)
  tsol.test_data |> List.map (fun gdi -> env0, gdi)

let tsol_egdos tsol : (env * grid_data) list train_test = (* train+test output env-gds *)
  tsol.train_data |> List.map (fun gdi -> gdi.params, grid_data0), (* perfect output model has empty grid_data *)
  tsol.test_data |> List.map (fun gdi -> gdi.params, grid_data0)


(* == checking functions == *)
		
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
    
let check_write_grid grid_name grid_model grid_env grid_data grid =
  let derived_grid = write_grid grid_model grid_env grid_data in
  print_grid_mismatch grid_name ~grid ~derived_grid



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
		      
let check_read_grid grid_name grid_env grid grid_model grid_data =
  match Model.read_grid grid_env grid grid_model with
  | Result.Ok (_env,gd) ->
     print_grid_data_mismatch grid_name grid ~data:grid_data ~parsed_data:gd
  | Result.Error msg ->
     Printf.printf "Grid %s: could not parse: %s\n" grid_name msg


let print_l_gmd name grid_model egds = (* grid model+data DL *)
  let lm, ld, lmd = Model.l_grid_model_data grid_model egds in
  Printf.printf "DL %s: L = %.1f + %.1f = %.1f\n" name lm ld lmd

let print_l_md model egdis egdos = (* model+data DL *)
  let (lmi,lmo,lm), (ldi,ldo,ld), (lmdi, lmdo, lmd) =
    Model.l_model_data model egdis egdos in
  Printf.printf "DL input  with Mi: L = %.1f + %.1f = %.1f\n" lmi ldi lmdi;
  Printf.printf "DL output with Mo: L = %.1f + %.1f = %.1f\n" lmo ldo lmdo;
  Printf.printf "DL input+output M: L = %.1f + %.1f = %.1f\n" lm ld lmd

let print_l_tsol tsol = (* DLs for train data *)
  let egdis, _ = tsol_egdis tsol in
  let egdos, _ = tsol_egdos tsol in
  print_l_md tsol.model egdis egdos

let print_l_task_model name task model =
  let egis = task.train |> List.map (fun pair -> env0, pair.input) in
  let egdis = read_grids egis model.input_pattern |> Option.get in
  let egos =
    List.map2
      (fun (envi,gdi) pair -> gdi.params, pair.output)
      egdis task.train in
  let egdos = read_grids egos model.output_template |> Option.get in
  print_l_md model egdis egdos
	      
(* monitoring learning *)

let print_learned_model name task : unit =
  let gis_test = (task.train @ task.test) |> List.map (fun {input} -> input) in
  let gos = task.train |> List.map (fun {output} -> output) in
  let lm = Model.learn_model
	     ~beam_width:1 ~refine_degree:1
	     gis_test gos in
  match lm with
  | [] -> assert false
  | ((_,m), (egdis_test,env_size,egdos), l)::_ ->
     print_endline "\n# Learned model:";
     pp_model m;
     print_newline ();
     print_l_md m egdis_test egdos;
	
     print_endline "\n# Input/output grids data (train)";
     let egdis = Common.sub_list egdis_test 0 (List.length egdos) in 
     let egdios = List.combine egdis egdos in
     List.iter2
       (fun ((_envi,gdi),(envo,gdo)) {output} ->
	Model.pp_grid_data gdi;
	Model.pp_grid_data gdo;
	if gdo.params = []
	then check_write_grid "predicted" m.output_template envo grid_data0 output
	else print_endline "Grid predicted: unbound variable";
	print_newline ())
       egdios task.train;
     print_endline "# Checking test instances\n";
     ignore (List.fold_left
	       (fun i {input; output} ->
		(*Grid.pp_grids [input; output];
		let parts = Grid.segment_by_color input in
		Grid.pp_parts input parts;*)
		match apply_model m env0 input with
		| Result.Ok derived ->
		   print_grid_mismatch
		     ("Grid test-" ^ string_of_int i)
		     ~grid:output
		     ~derived_grid:derived;
		   i+1
		| Result.Error msg ->
		   Printf.printf "Grid test-%d: %s\n" i msg;
		   i+1)
	       1 task.test)
			   
(* check task *)
			   
let check_task_solution (tsol : task_solution) : unit =
  print_endline "=====================================\n";
  Printf.printf "Checking task %s: %d train, %d test\n"
		tsol.name (List.length tsol.task.train) (List.length tsol.task.test);
  print_l_task_model tsol.name tsol.task model0;
  print_endline "\n# Expected model:";
  pp_model tsol.model; print_newline ();
  print_l_tsol tsol;
  print_endline "\n# Checking reading and writing input/output grids";
  let cpt = ref 0 in
  List.iter2
    (fun {input; output} gd ->
     incr cpt;
     (* displaying input grid parts *)
     (*Grid.pp_parts input (Grid.segment_by_color input);*)
     (* checking parsing *)
     check_read_grid
       (tsol.name ^ "-read--input--" ^ string_of_int !cpt)
       env0 input tsol.model.input_pattern gd;
     check_read_grid
       (tsol.name ^ "-read--output-" ^ string_of_int !cpt)
       gd.params output tsol.model.output_template grid_data0;
     (* checking writing *)
     check_write_grid
       (tsol.name ^ "-write-input--" ^ string_of_int !cpt)
       tsol.model.input_pattern [] gd input;
     check_write_grid
       (tsol.name ^ "-write-output-" ^ string_of_int !cpt)
       tsol.model.output_template gd.params grid_data0 output)
    (tsol.task.train @ tsol.task.test)
    (tsol.train_data @ tsol.test_data);
  print_endline "\n# Learning a model";
  print_learned_model tsol.name tsol.task
  


let check_task (name : string) (task : Task.task) : unit =
  print_endline "=====================================";
  Printf.printf "Checking task %s: %d train, %d test\n"
		name
		(List.length task.train)
		(List.length task.test);
  print_endline "\n# evaluating model0";
  print_l_task_model name task model0;  
  print_endline "\n# learning a model for train pairs";
  print_learned_model name task;
  print_newline ()
    
(* ============================================================ *)

let file_of_name name =
  "/local/ferre/data/tasks/ARC/data/training/" ^ name ^ ".json"
    
let tsol_ba97ae07 =
  let name = "ba97ae07" in
  { name;
    task = from_file (file_of_name name);
    model =
      { genvar = Genvar.empty;
	input_pattern =
	  AddShape
	    (Rectangle { height = U "H2"; width = U "W2";
			 offset_i = U "I2"; offset_j = U "J2";
			 color = U "C2"; filled = E (Const true) },
	     AddShape
	       (Rectangle { height = U "H1"; width = U "W1";
			    offset_i = U "I1"; offset_j = U "J1";
			    color = U "C1"; filled = E (Const true) },
		Background { height = U "H";
			     width = U "W";
			     color = E (Const Grid.black) }));
	output_template = (* swapping the two rectangles *)
	  AddShape
	    (Rectangle { height = E (Var "H1"); width = E (Var "W1");
			 offset_i = E (Var "I1"); offset_j = E (Var "J1");
			 color = E (Var "C1"); filled = E (Const true) },
	     AddShape
	       (Rectangle { height = E (Var "H2") (* error *); width = E (Var "W2");
			    offset_i = E (Var "I2"); offset_j = E (Var "J2");
			    color = E (Var "C2"); filled = E (Const true) },
		Background { height = E (Var "H");
			     width = E (Var "W");
			     color = E (Const Grid.black) }));
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

let main_solutions () =
  check_task_solution tsol_ba97ae07

let train_dir = "/local/ferre/data/tasks/ARC/data/training/"
let train_names = Array.to_list (Sys.readdir train_dir)

let solved_train_names =
  [ "ba97ae07.json";
    "b94a9452.json";
    "694f12f3.json";
  ]

let maybe_train_names =
  [ "952a094c.json";
    "98cf29f8.json";
  ]
    
let task_of_name name = Task.from_file (train_dir ^ name)
				
let main_tasks names =
  List.iter
    (fun name -> check_task name (task_of_name name))
    names

let _ = main_tasks solved_train_names

