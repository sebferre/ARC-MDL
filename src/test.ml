
open Task
open Model

(* === parameters === *)
       
let training = ref true (* should be set to false on evaluation set *)
let task_timeout = ref 20

(* === hard-coded solution === *)
		       
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
    
let tsol_gri tsol : grids_read train_test = (* train+test input env-gds *)
  let env_size = 0 in
  let l_m, code_gd = l_grid_model ~env_size tsol.model.input_pattern in
  {l_m; egdls = tsol.train_data |> List.map (fun gdi -> env0, gdi, code_gd env0 gdi)}, (* inputs have empty env *)
  {l_m; egdls = tsol.test_data |> List.map (fun gdi -> env0, gdi, code_gd env0 gdi)}

let tsol_gro tsol : grids_read train_test = (* train+test output env-gds *)
  let env_size = List.length (List.hd tsol.train_data).params in
  let l_m, code_gd = l_grid_model ~env_size tsol.model.input_pattern in
  {l_m; egdls = tsol.train_data |> List.map (fun gdi -> gdi.params, grid_data0, code_gd gdi.params grid_data0)}, (* perfect output model has empty grid_data *)
  {l_m; egdls = tsol.test_data |> List.map (fun gdi -> gdi.params, grid_data0, code_gd gdi.params grid_data0)}


(* === printing and checking functions === *)
			     
let print_grid_mismatch name ~grid ~derived_grid : unit =
  let diff = Grid.diff derived_grid grid in
  match diff with
  | None ->
     Printf.printf "%s: SUCCESS\n" name
  | Some (Grid_size_mismatch {src_height; src_width; tgt_height; tgt_width}) ->
     if !training
     then Printf.printf "%s: size mismatch, %dx%d instead of %dx%d\n"
			name src_height src_width tgt_height tgt_width
     else Printf.printf "%s: FAILURE\n" name
  | Some (Grid_diff_pixels {height; width; pixels}) ->
     if !training
     then (
       Printf.printf "%s: %d wrong pixels (generated / expected)\n" name (List.length pixels);
       Grid.pp_grids [derived_grid; grid]
     )
     else Printf.printf "%s: FAILURE\n" name
    
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
  | Result.Ok gd ->
     print_grid_data_mismatch grid_name grid ~data:grid_data ~parsed_data:gd
  | Result.Error msg ->
     Printf.printf "Grid %s: could not parse: %s\n" grid_name msg

let print_l_gmd name gr = (* grid model+data DL *)
  let lm, ld, lmd = Model.l_grid_model_data gr in
  Printf.printf "DL %s: L = %.1f + %.1f = %.1f\n" name lm ld lmd
		   
let print_l_md gri gro = (* model+data DL *)
  let (lmi,lmo,lm), (ldi,ldo,ld), (lmdi, lmdo, lmd) =
    Model.l_model_data gri gro in
  Printf.printf "DL input  with Mi: L = %.1f + %.1f = %.1f\n" lmi ldi lmdi;
  Printf.printf "DL output with Mo: L = %.1f + %.1f = %.1f\n" lmo ldo lmdo;
  Printf.printf "DL input+output M: L = %.1f + %.1f = %.1f\n" lm ld lmd;
  ldo
		   
let print_l_tsol tsol = (* DLs for train data *)
  let gri, _ = tsol_gri tsol in
  let gro, _ = tsol_gro tsol in
  ignore (print_l_md gri gro)

let print_l_task_model name task model =
  let egis = task.train |> List.map (fun pair -> env0, pair.input) in
  let gri = read_grids egis model.input_pattern |> Option.get in
  let egos =
    List.map2
      (fun (_envi,gdi,_li) pair -> gdi.params, pair.output)
      gri.egdls task.train in
  let gro = read_grids egos model.output_template |> Option.get in
  ignore (print_l_md gri gro)

	 
(* === monitoring learning === *)

type measures = (string * [`Tasks|`Bits] * float) list

let print_measures count ms =
  List.iter
    (fun (a,t,v) ->
     match t with
     | `Tasks -> Printf.printf "%s = %.2f tasks (%.2f%%)\n" a v (100. *. v /. float count)
     | `Bits -> Printf.printf "%s = %.1f bits (%.1f bits/task)\n" a v (v /. float count))
    ms;
  print_newline ()
				 
let print_learned_model name task : measures = Common.prof "Test.print_learned_model" (fun () ->
  let lm, timed_out =
    Model.learn_model
      ~verbose:(!training)
      ~timeout:(!task_timeout)
      ~beam_width:1 ~refine_degree:1
      task.train (*gis_test gos*) in
  if timed_out && !training then print_endline "TIMEOUT";
  match lm with
  | [] -> assert false
  | ((_,m), (gri, gro), l)::_ ->
     print_endline "\n# Learned model:";
     pp_model m;
     print_newline ();
     let ldo = print_l_md gri gro in
     
     print_endline "\n# Input/output grids data (train)";
     let egdlios = List.combine gri.egdls gro.egdls in
     let _, nb_ex_train, nb_correct_train =
       List.fold_left2
	 (fun (i,nb_ex, nb_correct)
	      ((_envi,gdi,li),(envo,gdo,lo)) {output} ->
	  let grid_name = "TRAIN " ^ name ^ "/" ^ string_of_int i in
	  if !training then Model.pp_grid_data gdi;
	  if !training then Model.pp_grid_data gdo;
	  if gdo.params = []
	  then check_write_grid grid_name m.output_template envo grid_data0 output
	  else Printf.printf "%s: ERROR (unbound variable)\n" grid_name;
	  print_newline ();
	  i+1,
	  nb_ex+1,
	  nb_correct + (if gdo = grid_data0 then 1 else 0))
	 (1,0,0)
	 egdlios task.train in
     
     print_endline "# Checking test instances\n";
     let _, nb_ex_test, nb_correct_test =
       List.fold_left
	 (fun (i,nb_ex,nb_correct) {input; output} ->
	  (*Grid.pp_grids [input; output];
	    let parts = Grid.segment_by_color input in
	    Grid.pp_parts input parts;*)
	  let score, label =
	    match apply_model m env0 input with
	    | Result.Ok derived ->
	       ( match Grid.diff derived output with
		 | None -> 1, "SUCCESS"
		 | Some _ -> 0, "FAILURE" )
	    | Result.Error msg -> 0, "ERROR" in
	  Printf.printf "TEST %s/%d: %d (%s)\n" name i score label;
	  i+1, nb_ex+1, nb_correct+score)
	 (1,0,0) task.test in
     print_newline ();
     let ms =
       [ "bits-train-error", `Bits, ldo;
	 "acc-train-micro", `Tasks, float nb_correct_train /. float nb_ex_train;
	 "acc-train-macro", `Tasks, (if nb_correct_train = nb_ex_train then 1. else 0.);
	 "acc-test-micro", `Tasks, float nb_correct_test /. float nb_ex_test;
	 "acc-test-macro", `Tasks, (if nb_correct_test = nb_ex_test then 1. else 0.);
       ] in
     print_endline "# Performance measures on task";
     print_measures 1 ms;
     ms)
     
(* check task *)
			   
let check_task_solution (tsol : task_solution) : measures =
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

   
(* === main for task solutions === *)

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
			 color = U "C2"; rmask = U "M2" },
	     AddShape
	       (Rectangle { height = U "H1"; width = U "W1";
			    offset_i = U "I1"; offset_j = U "J1";
			    color = U "C1"; rmask = U "M1" },
		Background { height = U "H";
			     width = U "W";
			     color = E ("C", Const Grid.black) }));
	output_template = (* swapping the two rectangles *)
	  AddShape
	    (Rectangle { height = E ("H1'", Var "H1"); width = E ("W1'", Var "W1");
			 offset_i = E ("I1'", Var "I1"); offset_j = E ("J1'", Var "J1");
			 color = E ("C1'", Var "C1"); rmask = E ("M1'", Var "M1") },
	     AddShape
	       (Rectangle { height = E ("H2'", Var "H2") (* error *); width = E ("W2'", Var "W2");
			    offset_i = E ("I2'", Var "I2"); offset_j = E ("J2'", Var "J2");
			    color = E ("C2'", Var "C2"); rmask = E ("M2'", Var "M2") },
		Background { height = E ("H'", Var "H");
			     width = E ("W'", Var "W");
			     color = E ("C'", Const Grid.black) }));
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

(* === main for tasks without solution === *)
		      
let arc_dir = "/local/ferre/data/tasks/ARC/data/"
let train_dir = arc_dir ^ "training/"
let train_names = Array.to_list (Sys.readdir train_dir)
let eval_dir = arc_dir ^ "evaluation/"
let eval_names = Array.to_list (Sys.readdir eval_dir)

let solved_train_names =
  [ "ba97ae07.json";
    "b94a9452.json";
    "e48d4e1a.json";
  ]

let maybe_train_names =
  [
    "a79310a0.json"; (* pb: rectangle mask, need to generalize from examples *)
    "67a423a3.json"; (* pb: rectangle mask, need to be transpose invariant *)
    "1bfc4729.json"; (* pb: two points, which is which, parse ambiguity, need for collections *)
    "694f12f3.json"; (* pb: need for expression bias *)
    "41e4d17e.json"; (* pb: collection, need for rectangle masks => unfilled square *)
    "952a094c.json"; (* pb: 4 points, which is which, need for nesting? *)
    "98cf29f8.json"; (* pb: parse ambiguity *)
    "d23f8c26.json"; (* pb: need for raster shape + crop *)
    "b9b7f026.json"; (* pb: need for nesting *)
    "d6ad076f.json"; (* pb: parse ambiguity, global rotation, topological relation? *)
    "b548a754.json"; (* pb: global rotation, overfit with cst *)
    "23581191.json"; (* pb: parse ambiguity *)
    "7f4411dc.json"; (* pb: collection, overfit *)
    "05f2a901.json"; (* pb: rectangle mask *)
  ]
    
let task_of_name dir name = Task.from_file (dir ^ name)

class type checker =
  object
    method process_task : string -> task -> unit
    method summarize_tasks : unit
  end
					   
let main_tasks (dir : string) (names : string list) (checker : checker) : unit =
  print_endline "## options";
  Printf.printf "alpha = %.1f\n" !alpha;
  Printf.printf "mode = %s\n" (if !training then "training" else "evaluation");
  Printf.printf "timeout = %d\n" !task_timeout;
  print_newline ();
  let nb_tasks = List.length names in
  let _ =
    List.fold_left
      (fun rank name ->
       let task = task_of_name dir name in
       print_endline "=====================================";
       Printf.printf "[-%d] Checking task %s: %d train, %d test\n"
		     rank name
		     (List.length task.train)
		     (List.length task.test);
       checker#process_task name task;
       rank-1)
      nb_tasks names in
  checker#summarize_tasks;
  Common.prerr_profiling ()

let checker_learning : checker =
  object
    val mutable count = 0
    val mutable sum_ms = []

    method process_task name task =
      print_endline "\n# evaluating model0";
      print_l_task_model name task model0;
      print_endline "\n# learning a model for train pairs";
      let ms = print_learned_model name task in
      count <- count+1;
      sum_ms <-
	if sum_ms = []
	then ms
	else
	  List.map2
	    (fun (a,t,sum_v) (a',t',v) ->
	     assert (a=a' && t=t');
	     (a,t,sum_v+.v))
	    sum_ms ms

    method summarize_tasks =
      Printf.printf "\n## performance measures averaged over %d tasks\n" count;
      print_measures count sum_ms;
      flush stdout
  end

let checker_segmentation : checker =
  object
    method process_task name task =
      List.iteri
	(fun i pair ->
	 Printf.printf "# example %d\n" i; 
	 [pair.input; pair.output]
	 |> List.iter
	      (fun g ->
	       let parts = Grid.segment_by_color g in
	       let full_mask = Grid.(Mask.full g.height g.width) in
	       let points = Grid.points g full_mask parts in
	       let rects = Grid.rectangles g full_mask parts in
	       Grid.pp_parts g parts;
	       Grid.pp_points g points;
	       Grid.pp_rectangles g rects;
	       print_newline ()))
	(task.train @ task.test)
    method summarize_tasks = ()
  end												 
let _ =
  let dir = ref train_dir in
  let names = ref train_names in
  let checker = ref checker_learning in
  Arg.parse
    ["-train", Unit (fun () -> dir := train_dir; training := true; names := train_names), "Use training set of tasks (default)";
     "-eval", Unit (fun () -> dir := eval_dir; training := false; names := eval_names), "Use evaluation set of tasks";
     "-all", Unit (fun () -> ()), "Use all tasks in the 
chosen set (default)";
     "-sample",
        Int (fun n -> Random.self_init (); names := Common.list_sample ~size:n !names),
	"Use the first N tasks in the chosen set";
     "-solved", Unit (fun () -> names := solved_train_names), "Use short list of solved training tasks";
     "-tasks",
     String (fun ids ->
	     let ids = String.split_on_char ',' ids in
	     names :=
	       !names
	       |> List.filter
		    (fun name ->
		     ids
		     |> List.exists
			  (fun id ->
			   String.sub name 0 (String.length id) = id))),
     "Use the tasks specified by their hexadecimal id prefix (comma-separated)";
     "-learn", Unit (fun () -> checker := checker_learning), "Perform learning on chosen tasks (default)";
     "-segment", Unit (fun () -> checker := checker_segmentation), "Show segmentation of grids";
     "-alpha", Set_float Model.alpha, "Multiplication factor over examples in DL computations (default: 10)";
     "-timeout", Set_int task_timeout, "Timeout per task (default: 20s)";
    ]
    (fun str -> ())
    "test [-train|-eval] [-all|-sample N|-solved|-tasks ID,ID,...] [-learn|-segment] [-alpha N] [-timeout N]";	      		       
  main_tasks !dir !names !checker

