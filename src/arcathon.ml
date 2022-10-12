
let root_path = "/local/ferre/prog/ocaml/arc/arcathon/sandbox/"
(* let root_path = "/data/" *)
let tasks_path = root_path ^ "evaluation/"
let solution_path = root_path ^ "solution/solution_sferre.json"

let timeout = 30

let load_tasks () (* including trailing / *) : int * (string * Task.task) list =
  let tasks_filenames = Array.to_list (Sys.readdir tasks_path) in
  let tasks_count = List.length tasks_filenames in
  let name_tasks =
    List.map
      (fun task_filename ->
        match Filename.chop_suffix_opt ~suffix:".json" task_filename with
        | None -> assert false
        | Some name -> name, Task.from_file (tasks_path ^ task_filename))
      tasks_filenames in
  tasks_count, name_tasks


let json_of_grid (grid : Grid.t) =
  let open Bigarray in
  let n1, n2 = grid.height, grid.width in
  let rows =
    Common.fold_for_down
      (fun i res ->
        let row =
          Common.fold_for_down
            (fun j row ->
              `Int (Array2.get grid.matrix i j) :: row)
            (n2 - 1) 0 [] in
        `List row :: res)
      (n1 - 1) 0 [] in
  `List rows
  
let process_test_pair m id {Task.input; output=_} = (* output not relevant *)
  let nb_preds, preds =
    match Model2.apply_model m input with
    | Result.Ok predictions ->
       let nb_preds, preds =
         List.fold_left
           (fun (i,preds) (_gdi,predicted_output) ->
             let pred =
               `Assoc [ "prediction_id", `Int i;
                        "output", json_of_grid predicted_output ] in
             i+1, pred :: preds)
           (0,[]) predictions in
       nb_preds, preds
    | Result.Error exn -> 0, []
  in
  `Assoc [ "output_id", `Int id;
           "number_of_predictions", `Int nb_preds;
           "predictions", `List (List.rev preds) ]
  
let process_task name task =
  let runtime, res =
    Common.chrono (fun () ->
        Model2.learn_model
          ~verbose:0
          ~timeout
          ~init_model:Model2.init_model
          ~beam_width:1
          ~refine_degree:(!Model2.max_refinements)
          task.Task.train) in
  let tests =
    match res with
    | Common.Exn exn -> raise exn
    | Common.Val (lm, timed_out) ->
       match lm with
       | [] -> [] (* no leaned model *)
       | ((_,m), _, _)::_ ->
          let _, tests =
            List.fold_left
              (fun (id,tests) pair ->
                try
                  let test = process_test_pair m id pair in
                  id+1, test :: tests
                with _ ->
                  id+1, tests) (* recovery from unexpected error, failed some test pair *)
              (0,[]) task.Task.test in
          tests
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
