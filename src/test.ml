
open Task

module Model = Model2

(* === parameters === *)
             
let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v
             
let beam_width = def_param "beam_width" 1 string_of_int
let refine_degree = def_param "refine_degree" 20 string_of_int

(* === command line options === *)
             
let training = ref true (* should be set to false on evaluation set *)
let start_rank = ref max_int
let task_timeout = ref 120
let learning_verbose = ref false
let grid_viz = ref false

(* === printing and checking functions === *)

let print_grid_diff ~grid ~derived_grid (diff : Grid.diff) : unit =
  match diff with
  | Grid_size_mismatch {src_height; src_width; tgt_height; tgt_width} ->
     Printf.printf "! size mismatch, %dx%d instead of %dx%d\n"
       src_height src_width tgt_height tgt_width
  | Grid_diff_pixels {height; width; pixels} ->
     Printf.printf "! %d wrong pixels (generated / expected)\n"
       (List.length pixels);
     if !grid_viz then Grid.pp_grids [derived_grid; grid]
                 
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
  let lm, ld, lmd = Model.dl_grid_model_data gr in
  Printf.printf "DL %s: L = %.1f + %.1f = %.1f\n" name lm ld lmd
		   
let print_l_md gpsr = (* model+data DL *)
  let (lmi,lmo,lm), (ldi,ldo,ld), (lmdi, lmdo, lmd) =
    Model.dl_model_data gpsr (*gsri gsro*) in
  Printf.printf "DL input  with Mi: L = %.1f + %.1f = %.1f\n" lmi ldi lmdi;
  Printf.printf "DL output with Mo: L = %.1f + %.1f = %.1f\n" lmo ldo lmdo;
  Printf.printf "DL input+output M: L = %.1f + %.1f = %.1f\n" lm ld lmd;
  ldo

let print_l_task_model name task model =
  Model.read_grid_pairs model task.train
  |> Result.fold
       ~ok:(fun gpsr -> ignore (print_l_md gpsr))
       ~error:(fun exn -> raise exn)
	 
(* === monitoring learning === *)

type measures = (string * [`Tasks|`Bits|`MRR] * float) list

let print_measures count ms =
  List.iter
    (fun (a,t,v) ->
     match t with
     | `Tasks -> Printf.printf "%s = %.2f tasks (%.2f%%)\n" a v (100. *. v /. float count)
     | `Bits -> Printf.printf "%s = %.1f bits (%.1f bits/task)\n" a v (v /. float count)
     | `MRR -> Printf.printf "%s = %.2f\n" a (v /. float count))
    ms;
  print_newline ()
				 
let print_learned_model ~init_model ~refine_degree name task : measures =
  Common.prof "Test.print_learned_model" (fun () ->
  let lm, timed_out =
    Model.learn_model
      ~verbose:(!training && !learning_verbose)
      ~grid_viz:(!grid_viz)
      ~timeout:(!task_timeout)
      ~init_model
      ~beam_width:(!beam_width) ~refine_degree
      task.train (*gis_test gos*) in
  if timed_out && !training then print_endline "TIMEOUT";
  match lm with
  | [] -> assert false
  | ((_,m), (gpsr, gsri, gsro), l)::_ ->
     print_endline "\n# Learned model:";
     Model.pp_model m;
     print_newline ();
     let ldo = print_l_md gpsr in
     
     print_endline "\n# Input/output grids data (train)";
     let _, nb_ex_train, nb_correct_train, sum_rrank_train =
       List.fold_left2
	 (fun (i,nb_ex, nb_correct, sum_rrank) reads_pair {input; output} ->
           let gri, gro, _ = List.hd reads_pair in
           let envi, gdi, dli = gri in
           let envo, gdo, dlo = gro in
	   if !training then (
             print_endline "Input and ouput best reading:";
             Model.pp_grid_data gdi; Printf.printf "   (%.1f bits)\n" dli;
             Model.pp_grid_data gdo; Printf.printf "   (%.1f bits)\n" dlo;
             if !grid_viz then
               Grid.pp_grids [Model.grid_of_data gdi.data;
                              Model.grid_of_data gdo.data]
           );
           let score, rank, label = (* TODO: use apply model like for test *)
             match Model.apply_model ~env:Model.data0 m input with
             | Result.Ok gdi_derived_s ->
                print_endline "Output writing (up to 3 trials):";
                List.fold_left
                  (fun (score,rank,label) (gdi, derived_grid) ->
                    if score=1 then score, rank, label
                    else (
                      match Grid.diff derived_grid output with
                      | None ->
                         if !training then (
                           Model.pp_grid_data gdi;
                           if !grid_viz then Grid.pp_grids [derived_grid]);
                         1, rank, "SUCCESS"
                      | Some diff ->
                         if !training then (
                           Model.pp_grid_data gdi;
                           print_grid_diff ~grid:output ~derived_grid diff);
                         0, rank+1, "FAILURE"))
                  (0,1,"FAILURE") gdi_derived_s
             | Result.Error err ->
                (match err with
                 | Model.Unbound_U ->
                    if !training then print_endline "! unbound unknown";
                    0, 0, "ERROR"
                 | Model.Unbound_Var p ->
                    if !training then Printf.printf "! unbound variable %s" (Model.string_of_path p);
                    0, 0, Printf.sprintf "ERROR"
                 | exn -> raise exn) in
           Printf.printf "TRAIN %s/%d: %d (%s)\n\n" name i score label;
	   i+1,
	   nb_ex+1,
	   nb_correct + score,
           (if score=1 then sum_rrank +. 1. /. float rank else sum_rrank))
	 (1,0,0,0.)
	 gpsr.reads task.train in
     
     print_endline "# Checking test instances\n";
     let _, nb_ex_test, nb_correct_test, sum_rrank_test =
       List.fold_left
	 (fun (i,nb_ex,nb_correct,sum_rrank) {input; output} ->
	  let score, rank, label =
	    match Model.apply_model ~env:Model.data0 m input with
	    | Result.Ok gdi_derived_s ->
               List.fold_left
                 (fun (score,rank,label) (gdi, derived) ->
                   if score=1 then score, rank, label
                   else (
                     if !training then (
                       print_endline "Input grid:";
                       Model.pp_grid_data gdi;
                       if !grid_viz then Grid.pp_grids [Model2.grid_of_data gdi.data]
                     );
                     print_endline "Output grid:";
	             ( match Grid.diff derived output with
		       | None ->
                          if !training && !grid_viz then (
                            Grid.pp_grids [derived]);
                          1, rank, "SUCCESS"
		       | Some diff ->
                          if !training then (
                            Model.pp_grid_data gdi;
                            print_grid_diff ~grid:output ~derived_grid:derived diff);
                          0, rank+1, "FAILURE" )
                 ))
                 (0,1,"FAILURE") gdi_derived_s
	    | Result.Error msg -> 0, 0, "ERROR" in
	  Printf.printf "TEST %s/%d: %d (%s)\n" name i score label;
	  i+1,
          nb_ex+1,
          nb_correct+score,
          (if score=1 then sum_rrank +. 1. /. float rank else sum_rrank))
	 (1,0,0,0.) task.test in
     print_newline ();
     let ms =
       [ "bits-train-error", `Bits, ldo;
	 "acc-train-micro", `Tasks, float nb_correct_train /. float nb_ex_train;
	 "acc-train-macro", `Tasks, (if nb_correct_train = nb_ex_train then 1. else 0.);
         "acc-train-mrr", `MRR, sum_rrank_train /. float nb_ex_train;
	 "acc-test-micro", `Tasks, float nb_correct_test /. float nb_ex_test;
	 "acc-test-macro", `Tasks, (if nb_correct_test = nb_ex_test then 1. else 0.);
         "acc-test-mrr", `MRR, sum_rrank_test /. float nb_ex_test;
       ] in
     print_endline "# Performance measures on task";
     print_measures 1 ms;
     ms)
     
(* === solved/candidate training tasks === *)
		      
let arc_dir = "/local/ferre/data/tasks/ARC/data/"
let train_dir = arc_dir ^ "training/"
let train_names = List.sort Stdlib.compare (Array.to_list (Sys.readdir train_dir))
let eval_dir = arc_dir ^ "evaluation/"
let eval_names = List.sort Stdlib.compare (Array.to_list (Sys.readdir eval_dir))
let sferre_dir = arc_dir ^ "sferre/"
let sferre_names = List.sort Stdlib.compare (Array.to_list (Sys.readdir sferre_dir))

let solved_train_names = (* 17 tasks, 234s *)
  [ "ba97ae07.json"; (* two rectangles overlapping, below becomes above, runtime=10.2s *)
    "bda2d7a6.json"; (* nested squares, color shift, partial success: rare case seen as noise, pb: sensitive to params, not really understood, runtime=9.2s *)
    "5582e5ca.json"; (* 3x3 grid, keep only majority color, runtime=1.1s *)
    "e9afcf9a.json"; (* two one-color rows, interleaving them, runtime=0.3s *)
    "6f8cd79b.json"; (* black grid => add cyan border, runtime=0.2s *)
    "e48d4e1a.json"; (* colored cross moved according to height of grey rectangle at (0,9), runtime=40.8s *)
    "25ff71a9.json"; (* shape moving 1 pixel down, runtime=0.6s *)
    "1cf80156.json"; (* crop on shape, runtime=1.3s *)
    "aabf363d.json"; (* shape and point => same shape but with point color, runtime=1.4s *)
    "b1948b0a.json"; (* any bitmap, changing background color, runtime=0.7s *)
    "bdad9b1f.json"; (* red and cyan segments, made full lines, yellow point at crossing, runtime=8.3s *)
    "a79310a0.json"; (* cyan shape, moving 1 pixel down, 0.5s *)
    "b94a9452.json"; (* square in square, crop on big square, swap colors, runtime=4.4s *)
    "1bfc4729.json"; (* 2 colored points, expand each in a fixed shape at relative position, runtime=2.7s *)
    "5521c0d9.json"; (* three rectangles moving up by their height, runtime=63.2s. *)
    "ea32f347.json"; (* three grey segments, color them by decreasing length, worked because parses big shapes first. runtime=88.5s *)
    "23581191.json"; (* 2 colored points, determining the position of horizontal and vertical lines, adding red points at different color crossings, runtime=116.1s *)
  ]

let maybe_train_names =
  [
    "9565186b.json"; (* pb: MDL not enough to keep bigest shape (size vs mask), runtime=0.3s *)
    "a1570a43.json"; (* pb: 4 points, which one is top left *)
    "d9fac9be.json"; (* pb: succeeds by chance ? how right point is chosen *)
    "681b3aeb.json"; (* pb: succeeds by chance [2 shapes, paving a 3x3 grid] *)
    "91714a58.json"; (* pb: insists too much on understanding input with many noise points, succeeds on test while failing on 2/3 train pairs, runtime>60 but succeeds earlier, very weak *)
    "928ad970.json"; (* pb: position next to borders on all sides, need more expressions *)
    "f76d97a5.json"; (* pb: good model but wrong test input parse, prefer having a diff, segmentation pb? => add full grid for each color as part *)
    "496994bd.json"; (* pb: moving objects up to some obstacle *)
    "67a423a3.json"; (* pb: rectangle mask, need to be transpose-invariant *)
    "694f12f3.json"; (* pb: need for expression bias, and ordering by size *)
    "41e4d17e.json"; (* pb: collection, map *)
    "952a094c.json"; (* pb: 4 points, which is which, need for nesting? explicit bottom right position of rectangles *)
    "98cf29f8.json"; (* pb: too slow, insufficient expressions on pos/size *)
    "d23f8c26.json"; (* pb: need for raster shape + crop *)
    "b9b7f026.json"; (* pb: need for nesting *)
    "d6ad076f.json"; (* pb: min/max operator, transpose, topological relation? *)
    "b548a754.json"; (* pb: global rotation, overfit with cst *)
    "7f4411dc.json"; (* pb: collection, overfit *)
    "05f2a901.json"; (* pb: rectangle mask *)
    "1fad071e.json"; (* pb: collection, cardinal *)
    "d5d6de2d.json"; (* pb: collection, map *)
    "4522001f.json"; (* pb: invariance par rotation *)
    "28bf18c6.json"; (* pb: size = 2*size1, consider input shape as output refinement *)
    "25d8a9c8.json"; (* pb: collection *)
    "29c11459.json"; (* pb: 1 instance in train, 2 instances in test *)
    "794b24be.json"; (* pb: need switch, [map from nb blue pixels to red fixed shape] *)
    "a68b268e.json"; (* pb: should define mask on sub-grid + full sub-grid shape, [4 shapes with fixed position and color, stacked in some order] *)
  ]

let task_model =
  let open Model2 in
  [ "ba97ae07.json",
    {input_pattern =
       `Background (`U, `Color Grid.black,
                    `Insert (`Nil,
                             `Rectangle (`U,`U,`U,`U),
                             `Insert (`Nil,
                                      `Rectangle (`U,`U,`U,`U),
                                      `Nil)));
     output_template =
       `Background (`E (`Var [`Size]), `E (`Var [`Color]),
                    `Insert (`Nil,
                             `E (`Var [`Layers [`Right]]),
                             `Insert (`Nil,
                                      `E (`Var [`Layers []]),
                                      `Nil))) };
    "1cf80156.json",
    {input_pattern =
       `Background (`U, `Color Grid.black,
                    `Insert (`Nil,
                             `Rectangle (`U,`U,`U,`U),
                             `Nil));
     output_template =
       `Background (`E (`Var [`Layers []; `Size]), `E (`Var [`Layers[`Right]; `Color]),
                    `Insert (`Nil,
                             `Rectangle (`Vec (`Int 0, `Int 0), `E (`Var [`Layers []; `Size]), `E (`Var [`Layers []; `Color]), `E (`Var [`Layers []; `Mask])),
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
        if rank <= !start_rank then (
          let task = task_of_name dir name in
          print_endline "=====================================";
          Printf.printf "[-%d] Checking task %s: %d train, %d test\n"
	    rank name
	    (List.length task.train)
	    (List.length task.test);
          checker#process_task name task
        );
       rank-1)
      nb_tasks names in
  checker#summarize_tasks;
  Common.prerr_profiling ()

class checker_model ~(get_init_model : string -> Model.model) ~refine_degree : checker =
  object
    val mutable count = 0
    val mutable sum_ms = []

    method process_task name task =
      if !grid_viz then (
        print_endline "\n# train grid pairs";
        List.iter
          (fun {input; output} -> Grid.pp_grids [input; output])
          task.train );
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
                         ~refine_degree:(!refine_degree)

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
     "-r", Set_int start_rank, "Start processing with this task rank (only useful for error recovery)";
     "-learn", Unit (fun () -> checker := checker_learning), "Perform learning on chosen tasks (default)";
     "-apply", Unit (fun () -> checker := checker_apply), "Apply pre-defined models to the chosen tasks (Model.init_model by default)";
     "-segment", Unit (fun () -> checker := checker_segmentation), "Show segmentation of grids";
     "-alpha", Set_float Model.alpha, "Multiplication factor over examples in DL computations (default: 10)";
     "-timeout", Set_int task_timeout, "Timeout per task (default: 20s)";
     "-viz", Set grid_viz, "Show train grids, as understood by the model";
     "-v", Set learning_verbose, "Verbose output for the learning phase";
    ]
    (fun str -> ())
    "test [-train|-eval] [-all|-sample N|-solved|-tasks ID,ID,...] [-r N] [-learn|-apply|-segment] [-alpha N] [-timeout N] [-viz] [-v]";
  main_tasks !dir !names !checker
