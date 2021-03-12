
open Task

module Model = Model2

(* === parameters === *)
       
let training = ref true (* should be set to false on evaluation set *)
let task_timeout = ref 20

(* === printing and checking functions === *)

let print_grid_diff ~grid ~derived_grid (diff : Grid.diff) : unit =
  match diff with
  | Grid_size_mismatch {src_height; src_width; tgt_height; tgt_width} ->
     Printf.printf "! size mismatch, %dx%d instead of %dx%d\n"
       src_height src_width tgt_height tgt_width
  | Grid_diff_pixels {height; width; pixels} ->
     Printf.printf "! %d wrong pixels (generated / expected)\n"
       (List.length pixels);
     Grid.pp_grids [derived_grid; grid]
                 
(*
let print_grid_data_mismatch grid_name grid ~data ~parsed_data : unit =
  let params = (*List.sort Stdlib.compare*) data.Model.params in
  let parsed_params = (*List.sort Stdlib.compare*) parsed_data.Model.params in
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
  | Some gd ->
     print_grid_data_mismatch grid_name grid ~data:grid_data ~parsed_data:gd
  | None ->
     Printf.printf "Grid %s: could not parse\n" grid_name
 *)
  
let print_l_gmd name gr = (* grid model+data DL *)
  let lm, ld, lmd = Model.dl_template_data gr in
  Printf.printf "DL %s: L = %.1f + %.1f = %.1f\n" name lm ld lmd
		   
let print_l_md gri gro = (* model+data DL *)
  let (lmi,lmo,lm), (ldi,ldo,ld), (lmdi, lmdo, lmd) =
    Model.dl_model_data gri gro in
  Printf.printf "DL input  with Mi: L = %.1f + %.1f = %.1f\n" lmi ldi lmdi;
  Printf.printf "DL output with Mo: L = %.1f + %.1f = %.1f\n" lmo ldo lmdo;
  Printf.printf "DL input+output M: L = %.1f + %.1f = %.1f\n" lm ld lmd;
  ldo

let print_l_task_model name task model =
  let egis =
    task.train
    |> List.map (fun pair -> Model.data0, pair.input) in
  let gos =
    task.train
    |> List.map (fun pair -> pair.output) in
  Model.read_grid_pairs model egis gos
  |> Result.fold
       ~ok:(fun (gri,gro) -> ignore (print_l_md gri gro))
       ~error:(fun exn -> raise exn)
	 
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
				 
let print_learned_model ~init_model ~refine_degree name task : measures = Common.prof "Test.print_learned_model" (fun () ->
  let lm, timed_out =
    Model.learn_model
      ~verbose:(!training)
      ~timeout:(!task_timeout)
      ~init_model
      ~beam_width:1 ~refine_degree
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
	      ((_envi,gdi,dli),(envo,gdo,dlo)) {output} ->
	  if !training then (Model.pp_grid_data gdi; Printf.printf "   (%.1f bits)\n" dli);
	  if !training then (Model.pp_grid_data gdo; Printf.printf "   (%.1f bits)\n" dlo);
          let score, label =
            Model.write_grid ~env:envo m.output_template
            |> Result.fold
                 ~ok:(fun derived_grid ->
                   match Grid.diff derived_grid output with
                   | None -> 1, "SUCCESS"
                   | Some diff ->
                      if !training then print_grid_diff ~grid:output ~derived_grid diff;
                      0, "FAILURE")
                 ~error:(function
                   | Model.Unbound_U ->
                      if !training then print_endline "! unbound unknown";
                      0, "ERROR"
                   | Model.Unbound_Var p ->
                      if !training then Printf.printf "! unbound variable %s" (Model.string_of_path p);
                      0, Printf.sprintf "ERROR"
                   | exn -> raise exn) in
          Printf.printf "TRAIN %s/%d: %d (%s)\n\n" name i score label;
	  i+1,
	  nb_ex+1,
	  nb_correct + score)
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
	    match Model.apply_model ~env:Model.data0 m input with
	    | Result.Ok (gdi, derived) ->
               if !training then Model.pp_grid_data gdi;
	       ( match Grid.diff derived output with
		 | None -> 1, "SUCCESS"
		 | Some diff ->
                    if !training then print_grid_diff ~grid:output ~derived_grid:derived diff;
                    0, "FAILURE" )
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
let sferre_dir = arc_dir ^ "sferre/"
let sferre_names = Array.to_list (Sys.readdir sferre_dir)

let solved_train_names =
  [ "ba97ae07.json"; (* two rectangles overlapping, below becomes above *)
    "bda2d7a6.json";
    "5582e5ca.json";
    "e9afcf9a.json";
    "6f8cd79b.json";
    "e48d4e1a.json";
    "25ff71a9.json";
    "1cf80156.json"; (* crop on shape *)
    "aabf363d.json";
    "b1948b0a.json";
  ]

let maybe_train_names =
  [
    "f76d97a5.json"; (* pb: good model but wrong test input parse, prefer having a diff, segmentation pb? *)
    "496994bd.json"; (* pb: keeping integrity of objects, breaking train invariant *)
    "b94a9452.json"; (* pb: inner rectangle not compressive, test input breaks train invariant (grid size) *)
    "a79310a0.json"; (* pb: need to consider 2nd best parsing, test input breaks train invariant (mask) *)
    "aabf363d.json"; (* pb: miss point not compressive enough, test input breaks train invariant (shape size) *)
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

let task_model =
  let open Model2 in
  [ "ba97ae07.json",
    {input_pattern =
       `Background (`U, `Color Grid.black,
                    `Cons (`Rectangle (`U,`U,`U,`U),
                           `Cons (`Rectangle (`U,`U,`U,`U),
                                  `Nil)));
     output_template =
       `Background (`E (`Var [`Size]), `E (`Var [`Color]),
                    `Cons (`E (`Var [`Rest; `First]),
                           `Cons (`E (`Var [`First]),
                                  `Nil))) };
    "1cf80156.json",
    {input_pattern =
       `Background (`U, `Color Grid.black,
                    `Cons (`Rectangle (`U,`U,`U,`U),
                           `Nil));
     output_template =
       `Background (`E (`Var [`First; `Size]), `E (`Var [`Rest; `Color]),
                    `Cons (`Rectangle (`Vec (`Int 0, `Int 0), `E (`Var [`First; `Size]), `E (`Var [`First; `Color]), `E (`Var [`First; `Mask])),
                           `Nil))}; 
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

class checker_model ~(get_init_model : string -> Model.model) ~refine_degree : checker =
  object
    val mutable count = 0
    val mutable sum_ms = []

    method process_task name task =
      let init_model =
        try get_init_model name
        with Not_found -> Model.init_model in
      if refine_degree <> 0 then ( (* only for learning, redundant for analyzing *)
        print_endline "\n# evaluating init_model";
        print_l_task_model name task init_model;
        print_endline "\n# learning a model for train pairs"
      );
      let ms = print_learned_model ~init_model ~refine_degree name task in
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

let checker_learning = new checker_model
                         ~get_init_model:(fun _ -> Model.init_model)
                         ~refine_degree:1

let checker_apply = new checker_model
                         ~get_init_model:(fun name -> List.assoc name task_model)
                         ~refine_degree:0
  
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
  let () = Printexc.record_backtrace true in
  let dir = ref train_dir in
  let names = ref train_names in
  let checker = ref checker_learning in
  Arg.parse
    ["-train", Unit (fun () -> dir := train_dir; training := true; names := train_names), "Use training set of tasks (default)";
     "-eval", Unit (fun () -> dir := eval_dir; training := false; names := eval_names), "Use evaluation set of tasks";
     "-sferre", Unit (fun () -> dir := sferre_dir; training := true; names := sferre_names), "Use sferre's set of tasks";
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
                      Filename.check_suffix name ".json"
		      && ids
		         |> List.exists
			      (fun id ->
			        String.sub name 0 (String.length id) = id))),
     "Use the tasks specified by their hexadecimal id prefix (comma-separated)";
     "-learn", Unit (fun () -> checker := checker_learning), "Perform learning on chosen tasks (default)";
     "-apply", Unit (fun () -> checker := checker_apply), "Apply pre-defined models to the chosen tasks (Model.init_model by default)";
     "-segment", Unit (fun () -> checker := checker_segmentation), "Show segmentation of grids";
     "-alpha", Set_float Model.alpha, "Multiplication factor over examples in DL computations (default: 10)";
     "-timeout", Set_int task_timeout, "Timeout per task (default: 20s)";
    ]
    (fun str -> ())
    "test [-train|-eval] [-all|-sample N|-solved|-tasks ID,ID,...] [-learn|-apply|-segment] [-alpha N] [-timeout N]";	      		       
  main_tasks !dir !names !checker
