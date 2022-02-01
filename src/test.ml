
open Task

module Model = Model2

(* === parameters === *)
             
let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v
             
let beam_width = def_param "beam_width" 1 string_of_int
let refine_degree = def_param "refine_degree" 100 (* TEST 20 *) string_of_int

(* === command line options === *)
             
let training = ref true (* should be set to false on evaluation set *)
let start_rank = ref max_int
let task_timeout = ref 30
let verbose = ref false
let grid_viz = ref false
let pause = ref 0.

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

type measures = (string * [`Tasks|`Bits|`MRR|`Seconds] * float) list

let print_measures count ms =
  List.iter
    (fun (a,t,v) ->
     match t with
     | `Tasks -> Printf.printf "%s = %.2f tasks (%.2f%%)\n" a v (100. *. v /. float count)
     | `Bits -> Printf.printf "%s = %.1f bits (%.1f bits/task)\n" a v (v /. float count)
     | `MRR -> Printf.printf "%s = %.2f\n" a (v /. float count)
     | `Seconds -> Printf.printf "%s = %.1f sec (%.1f sec/task)\n" a v (v /. float count))
    ms;
  print_newline ()

let score_learned_model name m (train_test : [`TRAIN of Model.grid_pairs_read |`TEST]) examples : float * float * float (* micro success, macro success, MRR *) =
  let _, nb_ex, nb_correct, sum_rrank =
    List.fold_left
      (fun (i,nb_ex,nb_correct,sum_rrank) {input; output} ->
        Printf.printf "\n## instance %d\n" i;
        if !training then (
          match train_test with
          | `TRAIN gpsr ->
             print_endline "\n> Input and output best reading:";
             let reads_pair = try List.nth gpsr.reads (i-1) with _ -> assert false in
             let gri, gro, _ = try List.hd reads_pair with _ -> assert false in
             let envi, gdi, dli = gri in
             let envo, gdo, dlo = gro in
             Model.pp_grid_data gdi; Printf.printf "   (%.1f bits)\n" dli;
             Model.pp_grid_data gdo; Printf.printf "   (%.1f bits)\n" dlo;
             if !grid_viz then
               Grid.pp_grids [Model.grid_of_data_failsafe gdi.data;
                              Model.grid_of_data_failsafe gdo.data]
          | `TEST -> ()
        );
        if !verbose then (
          print_endline "\n> Best input readings:";
          let input_reads =
            match train_test with
            | `TRAIN gpsr -> Result.Ok (List.nth gpsr.input_reads (i-1))
            | `TEST -> Model.read_grid ~quota_diff:(!Model.max_nb_diff) ~env:Model.data0 m.Model.input_pattern input in
          ( match input_reads with
            | Result.Ok reads ->
               List.iter
                 (fun (_,gdi,dli) ->
                   Model.pp_grid_data gdi;
                   Printf.printf "  (%.1f bits)\n" dli)
                 reads;
               if !grid_viz then (
                 Grid.pp_grids
                   (List.map
                      (fun (_,gdi,_) -> Model.grid_of_data_failsafe gdi.Model.data)
                      reads))
            | Result.Error _ ->
               print_endline "(No input readings)"
          ));
        print_endline "\n> Output prediction from input (up to 3 trials):";
        let score, rank, label, _failed_derived_grids =
	  match Model.apply_model m input with
	  | Result.Ok gdi_derived_s ->
             List.fold_left
               (fun (score,rank,label,failed_derived_grids) (gdi, derived) ->
                 if score=1 then
                   score, rank, label, failed_derived_grids (* already success *)
                 else if rank > 3 then
                   score, rank, label, failed_derived_grids (* at most 3 trials *)
                 else if List.mem derived failed_derived_grids then
                   score, rank, label, failed_derived_grids (* already derived that grid and it failed *)
                 else ( (* score=0 && rank <= 3 && new derived_grid *)
                   Printf.printf ">> Trial %d\n" rank;
                   if !training then (
                     Model.pp_grid_data gdi;
                     if !grid_viz then Grid.pp_grids [Model2.grid_of_data_failsafe gdi.data]
                   );
	           ( match Grid.diff derived output with
		     | None ->
                        if !training then (
                          print_endline "correct output grid";
                          if !grid_viz then Grid.pp_grids [derived]
                        );
                        1, rank, "SUCCESS", failed_derived_grids
		     | Some diff ->
                        if !training then (
                          print_grid_diff ~grid:output ~derived_grid:derived diff);
                        0, rank+1, "FAILURE", derived::failed_derived_grids )
               ))
               (0,1,"FAILURE",[]) gdi_derived_s
	  | Result.Error exn ->
             print_endline (Printexc.to_string exn);
             0, 0, "ERROR", [] in
        let tt = match train_test with `TRAIN _ -> "TRAIN" | `TEST -> "TEST" in
        let str_rank =
          if score = 0
          then "-"
          else
            match rank with
            | 1 -> "1st"
            | 2 -> "2nd"
            | 3 -> "3rd"
            | _ -> assert false in
	Printf.printf "\n%s %s/%d: %d %s (%s)\n" tt name i score str_rank label;
	i+1,
        nb_ex+1,
        nb_correct+score,
        (if score=1 then sum_rrank +. 1. /. float rank else sum_rrank))
      (1,0,0,0.) examples in
  let micro = float nb_correct /. float nb_ex in
  let macro = if nb_correct = nb_ex then 1. else 0. in
  let mrr = sum_rrank /. float nb_ex in
  micro, macro, mrr
  
       
let print_learned_model ~init_model ~refine_degree name task : measures =
  Common.prof "Test.print_learned_model" (fun () ->
  let runtime, res =
    Common.chrono (fun () ->
        Model.learn_model
          ~verbose:(!training && !verbose)
          ~grid_viz:(!grid_viz)
          ~pause:(!pause)
          ~timeout:(!task_timeout)
          ~init_model
          ~beam_width:(!beam_width) ~refine_degree
          task.train)
  in
  match res with
  | Common.Exn exn ->
      print_endline (Printexc.to_string exn);
      Printexc.print_backtrace stdout;
      let ms =
        [ "runtime-learning", `Seconds, runtime;
          "bits-train-error", `Bits, 0.; (* dummy value *)
	  "acc-train-micro", `Tasks, 0.;
	  "acc-train-macro", `Tasks, 0.;
          "acc-train-mrr", `MRR, 0.;
	  "acc-test-micro", `Tasks, 0.;
	  "acc-test-macro", `Tasks, 0.;
          "acc-test-mrr", `MRR, 0.;
        ] in
      ms    
  | Common.Val (lm, timed_out) ->
     if timed_out then print_endline "TIMEOUT";
     match lm with
     | [] -> assert false
     | ((_,m), (gpsr, gsri, gsro), l)::_ ->
        print_endline "\n# Learned model:";
        Model.pp_model m;
        print_newline ();
        let ldo = print_l_md gpsr in
        print_endline "\n# train input/output grids";
        let micro_train, macro_train, mrr_train =
          score_learned_model name m (`TRAIN gpsr) task.train in
        print_endline "\n# Test input/output grids";
        let micro_test, macro_test, mrr_test =
          score_learned_model name m (`TEST) task.test in
        print_endline "\n# Performance measures on task";
        let ms =
          [ "runtime-learning", `Seconds, runtime;
            "bits-train-error", `Bits, ldo;
	    "acc-train-micro", `Tasks, micro_train;
	    "acc-train-macro", `Tasks, macro_train;
            "acc-train-mrr", `MRR, mrr_train;
	    "acc-test-micro", `Tasks, micro_test;
	    "acc-test-macro", `Tasks, macro_test;
            "acc-test-mrr", `MRR, mrr_test;
          ] in
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

let solved_train_names = (* 32 tasks, 147.8s for timeout=30s, max_nb_parses=64, max_refs=100 *)
  [ "08ed6ac7.json"; (* 4 grey bars, colored in size order, runtime=8.4s *)
    "1bfc4729.json"; (* 2 colored points, expand each in a fixed shape at relative position, runtime=2.2s *)
    "1cf80156.json"; (* crop on shape, runtime=0.1s *)
    "1f85a75f.json"; (* crop of a shape among a random cloud of points. runtime about 1s, timeout trying to explain everything, runtime=6.0 *)
    "23581191.json"; (* 2 colored points, determining the position of horizontal and vertical lines, adding red points at different color crossings, runtime=TIMEOUT 28.8  *)
    "25ff71a9.json"; (* shape moving 1 pixel down, runtime=0.2s *)
    "445eab21.json"; (* output a 2x2 grid with color from the larger rectangle. runtime=0.2s *)
    "48d8fb45.json"; (* crop on one shape among several, should choose next to grey point but works by choosing 2nd layer (decreasing size). runtime=1.4s *)
    "5521c0d9.json"; (* three rectangles moving up by their height, runtime=0.7s *)
    "5582e5ca.json"; (* 3x3 grid, keep only majority color, runtime=0.1s *)
    "681b3aeb.json"; (* 2 shapes, paving a 3x3 grid, a bit lucky. runtime=0.8s *)
    "694f12f3.json"; (* NEW 2 yellow rectangles, fill the larger one in red, the smaller in blue *)
    "6f8cd79b.json"; (* black grid => add cyan border, runtime=0.1s *)
    "7e0986d6.json"; (* collection of rectangles + noise points to be removed, runtime=19.3 *)
    "a1570a43.json"; (* red shape moved into 4 green points, runtime=6.9s *)
    "a61ba2ce.json"; (* 4 corners, they join as a small square, runtime=12.2s *)
    "a79310a0.json"; (* cyan shape, moving 1 pixel down, runtime=0.0s *)
    "a87f7484.json"; (* crop on the largest 3x3 shape. runtime=2.4s *)
    "aabf363d.json"; (* shape and point => same shape but with point color, runtime=0.6s *)
    "b1948b0a.json"; (* any bitmap, changing background color, runtime=0.2s *)
    "b230c067.json"; (* NEW three cyan shapes, set the two biggest blue (same shapes), and the smallest red, runtime=14.8s *)
                     (* CAUTION: works because always three shapes, and the different shape always the smaller one *)
    "b94a9452.json"; (* square in square, crop on big square, swap colors, runtime=0.6s *)
    "ba97ae07.json"; (* two rectangles overlapping, below becomes above, runtime=4.4s *)
    "bb43febb.json"; (* NEW two grey rectangles, add red rectangles inside, letting a grey border *)
    "bda2d7a6.json"; (* nested squares, color shift, partial success: rare case seen as noise, pb: sensitive to params, not really understood, runtime=8.7s. With collection: need access to color of item at position (i - 1) mod 3 *)
    (* bda2 pb: difficult to find right parse as collection of stacked full rectangles, prefer to use one color as background, finds bottom rectangle first because bigger *)
    (* bda2 sol: model common masks such border, checkboard, stripes; ?? *)
    "bdad9b1f.json"; (* red and cyan segments, made full lines, yellow point at crossing, runtime=3.5s *)
    "be94b721.json"; (* 3 shapes (at least), selecting the biggest one, runtime=0.5s *)
    "d631b094.json"; (* NEW any colored shape, output 1 x area grid with same color, runtime=0.0s *)
    "e48d4e1a.json"; (* colored cross moved according to height of grey rectangle at (0,9), runtime=10.4s *)
    "e9afcf9a.json"; (* two one-color rows, interleaving them, runtime=0.3s *)
    "ea32f347.json"; (* three grey segments, color them by decreasing length, worked because parses big shapes first. runtime=3.9s *)
    "ea786f4a.json"; (* NEW black point in a colored grid, produce same-size same-color grid with a full-grid x-cross black shape at (0,0), runtime=0.2s *)
  ]

let nogen_train_names = (* tasks that succeeds on examples but fail on test cases *)
  [
    "0962bcdd.json"; (* pb: selects wrong expression for position, need too many diffs
                        todo: use more semantic expressions on pos/size, prune irrelevant constants in input model, consider best mask model w.r.t. hidden pixels (considered as on currently), position at center *) 
    "1caeab9d.json"; (* pb: ignores the side point in shapes (prefers Full like in other examples); with repeat, how to refer to the blue shape?
                        todo: use Repeat to value the common shape, refine ordering of Grid.rectangles, take into account output delta (objective is zero) *)
    "29c11459.json"; (* pb: 1 instance in train, 2 instances in test
                        todo: prevoir un mecanisme de repetition de la transformation *)
    "4522001f.json"; (* pb: invariance par rotation
                        todo: construct where parsing gives a rotation angle (or a transpose bool, or a symmetry?) [ A = Rotation(A, angle) for A in layer, shape ]; consider full grid as is *)
    "496994bd.json"; (* diff OK
                        pb: needs complex expressions (a - b -c) (a - 2*(a+b)) 
                        todo: simply consider application of symmetries to input grid! here vertical symmetry (requires neutral color, black?) *)
    "694f12f3.json"; (* pb: selects wrong expressions
                        todo: like for 0962b, use expressions like size-(2,2) and pos+(1,1); also consider shape filling color (out-of-mask pixels) *)
    "91714a58.json"; (* pb: subtle dl optim parsing two rectangles
                        todo: prune input model, stop condition to avoid explaining big noise *)
    "a61f2674.json"; (* pb: variable nb of bars
                        todo: manage collections *)
    "b230c067.json"; (* pb: too many relaxations needed (constants)
                        todo: should prune input grid model for unnecessary constraints *)
    "bb43febb.json"; (* see 694f *)
    "caa06a1f.json"; (* pb: test has a complex checkboard motif, not examples
                        todo: k-color mask model (k small, 2-3), generalizing filling color ? + mask relaxation  *)
    "d5d6de2d.json"; (* pb: collection, fails to generalize from mapping first item to all items (use relax mechanism), degenerated case where produced rectangle is void (size (0,0))
                        todo: collection (or optional object); void rect OK in output *)
    "d9fac9be.json"; (* pb: no way to choose point with 'other' color or inside 3x3 rect
                        todo: relative position; filling color *)
    "f76d97a5.json"; (* pb: good model but wrong test input parse, prefers having a diff, missing part
                        todo: add full grid for each color as part *)
  ]
  
let maybe_train_names =
  [
    "3bd67248.json"; (* pb: missing diagonals as shapes, maybe add along with Border... *)
    "99b1bc43.json"; (* TODO pb: subgrids, full-grid bitmaps, bitmap logic *)
    "6b9890af.json"; (* TODO pb: need for scale-invariant masks/bitmaps *)
    "ea786f4a.json"; (* pbs: missing black cross as part, chooses checkboard on example 1 *)
    "05269061.json"; (* pb: doesn't know which colors, 6 combinations, only 3 trials. diagonals alternating 3 colors, completion from only one diagonal per color at variable positions. runtime=36.6s *)
    "72ca375d.json"; (* SUCCESS: by trying the different options, not understanding proper *)
    "0b148d64.json"; (* SUCCESS crop on quadrant with different color, wins by relying on parse ordering. runtime=108s *)
    "ddf7fa4f.json"; (* SUCCESS: default ordering of points helps *)
    "de1cd16c.json"; (* pb: need collection, cardinal, and max. pixel with the color of the area with the more points, actually selects the second largest such area. worked once with several trials. runtime=27.6s *)
    "9565186b.json"; (* pb: MDL not enough to keep bigest shape (size vs mask), runtime=0.3s *)
    "928ad970.json"; (* TODO pb: position next to borders on all sides, need more expressions, and also border as special mask, independent of size, and ignoring what is inside *)
    "67a423a3.json"; (* pb: rectangle mask, need to be transpose-invariant *)
    "41e4d17e.json"; (* pb: collection, map *)
    "952a094c.json"; (* pb: 4 points, which is which, need for nesting? explicit bottom right position of rectangles *)
    "98cf29f8.json"; (* pb: too slow, insufficient expressions on pos/size *)
    "d23f8c26.json"; (* pb: need for raster shape + crop *)
    "b9b7f026.json"; (* pb: need for nesting *)
    "d6ad076f.json"; (* pb: min/max operator, transpose, topological relation? *)
    "b548a754.json"; (* pb: global rotation, overfit with cst *)
    "7f4411dc.json"; (* TODO pb: prefers to include adjacent point in rectangle (maximize nb_pixels) *)
    "05f2a901.json"; (* pb: relative position *)
    "1fad071e.json"; (* pb: collection, cardinal *)
    "28bf18c6.json"; (* pb: size = 2*size1, consider input shape as output refinement *)
    "25d8a9c8.json"; (* TODO pb: ouput 2x3 rectangle to be read as two 1x3 rectangles *)
    "794b24be.json"; (* pb: need switch, [map from nb blue pixels to red fixed shape] *)
    "a68b268e.json"; (* pb: should define mask on sub-grid + full sub-grid shape, [4 shapes with fixed position and color, stacked in some order] *)
    "c0f76784.json"; (* pb: tolerate missing element (which could be hidden), including in training examples, i.e. tolerate exception in model refinements *)
    "f8ff0b80.json"; (* TODO pb: output points not compressive enough *) 
    "50cb2852.json"; (* pb: collection *)
    "54d82841.json"; (* pb: collection *)
    "ce22a75a.json"; (* pb: 2nd example is not helping, having two examples like the first one makes it find the right model but it fails to parse the 4 points in test, parses only 3 *)
    "a2fd1cf0.json"; (* TODO pb: collection of two rectangles, order-sensitive? hence failing to find relationship to the two points? *)
    "7468f01a.json"; (* TODO pb: inserts an unspecified rectangle before repeat of unspecified rectangles, maybe problem with sequential parsing top-down *)
    "e26a3af2.json"; (* pb: inserts unspecified rectangle in front of collection of unspecified rectangles. Should work with a single collection of rectangles + collection of points *)
    "e859"; (* collections of small shapes, black, to be colored as a function of area *)
  ]

let task_model =
  let open Model2 in
  [ "ba97ae07.json",
    {input_pattern =
       `Background (`U, `Color Grid.black,
                    `Insert (`Nil,
                             `PosShape (`U, `Rectangle (`U,`U,`U)),
                             `Insert (`Nil,
                                      `PosShape (`U, `Rectangle (`U,`U,`U)),
                                      `Nil)));
     output_template =
       `Background (`Ref (`Field (`Size,`Root)), `Ref (`Field (`Color, `Root)),
                    `Insert (`Nil,
                             `Ref (`Field (`Layer (`Right `Root), `Root)),
                             `Insert (`Nil,
                                      `Ref (`Field (`Layer `Root, `Root)),
                                      `Nil))) };
    "1cf80156.json",
    {input_pattern =
       `Background (`U, `Color Grid.black,
                    `Insert (`Nil,
                             `PosShape (`U, `Rectangle (`U,`U,`U)),
                             `Nil));
     output_template =
       `Background (`Ref (`Field (`Size, `Field (`Layer `Root, `Root))),
                    `Ref (`Field (`Color, `Field (`Layer (`Right `Root), `Root))),
                    `Insert (`Nil,
                             `PosShape (`Vec (`Int 0, `Int 0),
                                        `Ref (`Field (`Shape, `Field (`Layer `Root, `Root)))),
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
     "-pause", Set_float pause, "Time delay (in seconds, default=0.0) at each step during learning, useful in combination with -viz";
     "-v", Set verbose, "Verbose mode";
    ]
    (fun str -> ())
    "test [-train|-eval] [-all|-sample N|-solved|-tasks ID,ID,...] [-r N] [-learn|-apply|-segment] [-alpha N] [-timeout N] [-viz [-pause T]] [-v]";
  main_tasks !dir !names !checker
