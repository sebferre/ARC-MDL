
module MadilArc = Madil.Make(Domain_arc.MyDomain)

(* PARAMS TO BE DEFINED *)
(*let root_path = "/local/ferre/prog/ocaml/arc/arcathon/sandbox/" (* local *)*)
let root_path = "/data/" (* docker *)
let memout = 10000
let timeout_refine = 300 (* 120 *)
let timeout_prune = 30
let timeout_predict = 30
                    
let tasks_path = root_path ^ "evaluation/"
let solution_path = root_path ^ "solution/solution_madil.json"

let load_tasks () (* including trailing / *) : int * (string * MadilArc.task) list =
  let tasks_filenames = Array.to_list (Sys.readdir tasks_path) in
  let name_tasks =
    List.filter_map
      (fun task_filename ->
        match Filename.chop_suffix_opt ~suffix:".json" task_filename with
        | None -> None
        | Some name -> Some (name, MadilArc.task_from_file (tasks_path ^ task_filename)))
      tasks_filenames in
  let tasks_count = List.length name_tasks in
  tasks_count, name_tasks

let process_test_pair env m info id {Task.input; output=_} = (* output not relevant *)
  let nb_preds, preds =
    match Common.do_timeout timeout_predict
            (fun () -> MadilArc.apply ~env m input info) with
    | Some (Result.Ok (predictions : (MadilArc.data * MadilArc.data * Madil_common.dl) list)) ->
       let nb_preds, preds, _ =
         List.fold_left
           (fun (i,preds,seen_outputs) (_gdi,gdo,_dl) ->
             if i < 3 (* at most 3 predictions *)
             then
               let vo = Data.value gdo in
               let output = MadilArc.json_of_value vo in
               if List.mem output seen_outputs
               then i, preds, seen_outputs (* ignore this redundant prediction *)
               else
                 let pred =
                   `Assoc [ "prediction_id", `Int i;
                            "output", output ] in
                 i+1, pred :: preds, output :: seen_outputs
             else i, preds, seen_outputs)
           (0,[],[]) predictions in
       nb_preds, preds
    | _ -> 0, []
  in
  `Assoc [ "output_id", `Int id;
           "number_of_predictions", `Int nb_preds;
           "predictions", `List (List.rev preds) ]
  
let process_task name task =
  let {MadilArc.env; varseq; input_model; output_model; output_generator_info=info} =
    MadilArc.get_init_config name task in
  let init_task_model = MadilArc.make_task_model varseq input_model output_model in
  let res : _ Learning.results =
    MadilArc.learn
      ~memout
      ~timeout_refine
      ~timeout_prune
      ~jump_width:(!MadilArc.jump_width)
      ~refine_degree:(!MadilArc.max_refinements)
      ~env
      ~init_task_model
      task.Task.train in
  let m = res.result_pruning.task_model in
  let _, tests =
    List.fold_left
      (fun (id,tests) pair ->
        try
          let test = process_test_pair env m info id pair in
          id+1, test :: tests
        with _ ->
          id+1, tests) (* recovery from unexpected error, failed some test pair *)
      (0,[]) task.Task.test
  in
  `Assoc [ "task_name", `String name;
           "test", `List tests ]

let print_progress i count =
  if i mod 1 = 0 then
    Printf.printf "%d/%d tasks processed\n" i count
  
let process_tasks count name_tasks =
  let _, sols =
    List.fold_left
      (fun (i,sols) (name,task) ->
        MadilArc.reset_memoization ();
        try
          let sol = process_task name task in
          print_progress i count;
          i+1, sol::sols
        with _ ->
          print_progress i count;
          i+1, sols) (* recovery from unexpected error *)
      (1,[]) name_tasks
  in
  `List sols

let store_solution json_solution =
  let ch_out = open_out solution_path in
  Yojson.Safe.pretty_to_channel ~std:true ch_out json_solution;
  close_out ch_out
(*  Yojson.Safe.to_file solution_path json_solution *)
  
let _ =
  let count, name_tasks = load_tasks () in
  let solution = process_tasks count name_tasks in
  store_solution solution;
  print_endline "Done"
