
open Task

(* === parameters === *)
       
let training = ref true (* should be set to false on evaluation set *)
let task_timeout = ref 20

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
  let derived_grid = Model.write_grid grid_model grid_env grid_data in
  print_grid_mismatch grid_name ~grid ~derived_grid

let print_grid_data_mismatch grid_name grid ~data ~parsed_data : unit =
  let params = List.sort Stdlib.compare data.Model.params in
  let parsed_params = List.sort Stdlib.compare parsed_data.Model.params in
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

let print_l_task_model name task model =
  let egis = task.train |> List.map (fun pair -> Model.env0, pair.input) in
  let gri = Model.read_grids egis model.Model.input_pattern |> Option.get in
  let egos =
    List.map2
      (fun (_envi,gdi,_li) pair -> gdi.Model.params, pair.output)
      gri.egdls task.train in
  let gro = Model.read_grids egos model.Model.output_template |> Option.get in
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
     Model.pp_model m;
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
	  then check_write_grid grid_name m.output_template envo Model.grid_data0 output
	  else Printf.printf "%s: ERROR (unbound variable)\n" grid_name;
	  print_newline ();
	  i+1,
	  nb_ex+1,
	  nb_correct + (if gdo = Model.grid_data0 then 1 else 0))
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
	    match Model.apply_model m Model.env0 input with
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
     
(* === solved/candidate training tasks === *)
		      
let arc_dir = "/local/ferre/data/tasks/ARC/data/"
let train_dir = arc_dir ^ "training/"
let train_names = Array.to_list (Sys.readdir train_dir)
let eval_dir = arc_dir ^ "evaluation/"
let eval_names = Array.to_list (Sys.readdir eval_dir)

let solved_train_names = (* Version 1.1 *)
  [ "ba97ae07.json";
    "bda2d7a6.json";
    "6f8cd79b.json";
    "e48d4e1a.json";
    "25ff71a9.json";
  ]

let maybe_train_names =
  [
    "1cf80156.json"; (* pb: shape in test instance too sparse (mask size = 11/24) *)
    "496994bd.json"; (* pb: keeping integrity of objects, breaking train invariant *)
    "b94a9452.json"; (* pb: test input breaks train invariant (grid size) *)
    "a79310a0.json"; (* pb: test input breaks train invariant (mask) *)
    "aabf363d.json"; (* pb: test input breaks train invariant (shape size) *)
    "bdad9b1f.json"; (* pb: parsing ambiguity *)
    "e48d4e1a.json"; (* pb: parsing ambiguity, 3 rectangles, should be OK with color and position *)
    "67a423a3.json"; (* pb: rectangle mask, need to be transpose-invariant *)
    "1bfc4729.json"; (* pb: two points, which is which, parse ambiguity, need for collections, should be OK with position *)
    "694f12f3.json"; (* pb: need for expression bias, and ordering by size *)
    "41e4d17e.json"; (* pb: collection, map *)
    "952a094c.json"; (* pb: 4 points, which is which, need for nesting? *)
    "98cf29f8.json"; (* pb: parse ambiguity *)
    "d23f8c26.json"; (* pb: need for raster shape + crop *)
    "b9b7f026.json"; (* pb: need for nesting *)
    "d6ad076f.json"; (* pb: parse ambiguity, global rotation, topological relation? *)
    "b548a754.json"; (* pb: global rotation, overfit with cst *)
    "23581191.json"; (* pb: parse ambiguity *)
    "7f4411dc.json"; (* pb: collection, overfit *)
    "05f2a901.json"; (* pb: rectangle mask *)
    "1fad071e.json"; (* pb: collection, cardinal *)
  ]

  
(* === main === *)
  
let task_of_name dir name = Task.from_file (dir ^ name)

class type checker =
  object
    method process_task : string -> task -> unit
    method summarize_tasks : unit
  end
					   
let main_tasks (dir : string) (names : string list) (checker : checker) : unit =
  print_endline "## options";
  Printf.printf "alpha = %.1f\n" !Model.alpha;
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
      print_l_task_model name task Model.model0;
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
