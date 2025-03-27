
module MadilArc = Madil.Make(Domain_arc.MyDomain)

(* PARAMS TO BE DEFINED *)

(* local *)
(*let tasks_path = "/local/ferre/prog/ocaml/arc/arcprize/arc-agi_test_challenges_sample.json"
let solution_path = "/local/ferre/prog/ocaml/arc/arcprize/submission.json"*)
(* Kaggle *)
let tasks_path = "/kaggle/input/arc-prize-2024/arc-agi_test_challenges.json"
let solution_path = "/kaggle/working/submission.json"

let memout = 5000 (* 10000 *)
(* 12h in total max, so max 432s/task overall *)
let timeout_refine = 180 (* max 300 *)
let timeout_prune = 20 (* 30 *)
let timeout_predict = 20 (* 30 *)

let dummy_grid = Grid.make 1 1 Grid.black
let dummy_json_grid = `List [`List [`Int 0]]

(* copied and modified from madil/task.ml *)
let rec task_of_json =
  let rec aux = function
    | `Assoc fields ->
       let trains =
         match List.assoc_opt "train" fields with
         | Some (`List trains) -> trains
         | _ -> invalid_arg "Invalid JSON task: missing train field" in
       let tests =
         match List.assoc_opt "test" fields with
         | Some (`List tests) -> tests
         | _ -> [] in
       { Task.train = List.map aux_pair trains;
         test = List.map aux_pair tests }
    | _ -> invalid_arg "Invalid JSON task"
  and aux_pair = function
    | `Assoc fields ->
       let input =
         match List.assoc_opt "input" fields with
         | Some i -> MadilArc.value_of_json i
         | None -> invalid_arg "Invalid JSON pair: missing input" in
       let output =
         match List.assoc_opt "output" fields with
         | Some o -> MadilArc.value_of_json o
         | None -> MadilArc.value_of_json dummy_json_grid in (* missing for test instances, dummy value *)
       { input; output }
    | _ -> invalid_arg "Invalid JSON pair"
  in
  aux

let load_tasks () : int * (string * MadilArc.task) list =
  let json = Yojson.Safe.from_file tasks_path in
  let name_tasks =
    match json with
    | `Assoc tasks ->
       List.map
         (fun (name,json) -> name, task_of_json json)
         tasks
    | _ -> failwith "Wrong JSON input format: not an object" in
  let tasks_count = List.length name_tasks in
  tasks_count, name_tasks

let process_test_pair env m info {Task.input; output=_} = (* output not relevant *)
  let nb_preds, preds =
    try
    match Common.do_timeout timeout_predict
            (fun () -> MadilArc.apply ~env m input info) with
    | Some (Result.Ok (predictions : (MadilArc.data * MadilArc.data * Madil_common.dl) list)) ->
       let nb_preds, rev_preds, _ =
         List.fold_left
           (fun (i,preds,seen_outputs) (_gdi,gdo,_dl) ->
             if i < 2 (* at most 2 predictions in ARC Prize *)
             then
               let vo = Data.value gdo in
               let output = MadilArc.json_of_value vo in
               if List.mem output seen_outputs
               then i, preds, seen_outputs (* ignore this redundant prediction *)
               else
                 let pred = output in
                 i+1, pred :: preds, output :: seen_outputs
             else i, preds, seen_outputs)
           (0,[],[]) predictions in
       nb_preds, List.rev rev_preds
    | _ -> 0, []
    with exn ->
      print_string "Failed test: ";
      print_endline (Printexc.to_string exn);
      0, [] (* recovery from error in test case *)
  in
  let pred1, pred2 =
    match preds with
    | [] -> dummy_json_grid, dummy_json_grid
    | [pred] -> pred, pred
    | pred1::pred2::_ -> pred1, pred2 in
  `Assoc [ "attempt_1", pred1;
           "attempt_2", pred2 ]
  
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
      ~search_temperature:(!MadilArc.search_temperature)
      ~env
      ~init_task_model
      task.Task.train in
  let m = res.result_pruning.task_model in
  let tests =
    List.map
      (fun pair ->
        process_test_pair env m info pair)
      task.Task.test
  in
  name, `List tests

let print_progress i count =
  if i mod 1 = 0 then
    Printf.printf "%d/%d tasks processed\n" i count
  
let process_tasks count name_tasks =
  let _, rev_sols =
    List.fold_left
      (fun (i,sols) (name,task) ->
        MadilArc.reset_memoization ();
        try
          let sol = process_task name task in
          print_progress i count;
          i+1, sol::sols
        with exn ->
          print_string "Failed task: ";
          print_endline (Printexc.to_string exn);
          print_progress i count;
          i+1, sols) (* recovery from unexpected error *)
      (1,[]) name_tasks
  in
  `Assoc (List.rev rev_sols)

let store_solution json_solution =
  let ch_out = open_out solution_path in
  Yojson.Safe.pretty_to_channel ~std:true ch_out json_solution;
  close_out ch_out
  
let _ =
  let count, name_tasks = load_tasks () in
  let solution = process_tasks count name_tasks in
  store_solution solution;
  print_endline "Done"
