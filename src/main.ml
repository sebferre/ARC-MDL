
let train_dir = "/local/ferre/data/tasks/ARC/data/training/"

let train_files = Array.to_list (Sys.readdir train_dir)
		  
let a_few_tasks =
  [ "007bbfb7.json";
    "00d62c1b.json";
    "017c7c7b.json";
    "025d127b.json";
    "045e512c.json";
    "0520fde7.json";
    "05269061.json";
    "05f2a901.json";
    "06df4c85.json";
    "08ed6ac7.json" ]

let pp_task name =
  print_endline name; flush stdout;
  let task = Task.from_file (train_dir ^ name) in
  Task.pp_task task;
  print_newline ()

type named_task = string * Task.task
		
type result =
  { passed : named_task list;
    failed : named_task list;
    nb_passed : int;
    nb_failed : int;
    accuracy : float }
    
let evaluation (tasks : named_task list) : result =
  let passed, failed =
    tasks
    |> List.partition
	 (fun (name,task) ->
	  let m = Model.learn task.Task.train in
	  let ok =
	    task.Task.test
	    |> List.for_all
		 (fun pair ->
		  let outputs = Decision.get_outputs m pair.Task.input in
		  (* TODO: keep 3 first outputs *)
		  outputs
		  |> List.exists
		       (fun output -> output = pair.Task.output)) in
	  print_string (if ok then "+" else "-");
	  ok) in
  let nb_passed = List.length passed in
  let nb_failed = List.length failed in
  let accuracy = float nb_passed /. float (nb_passed + nb_failed) in
  { passed; failed;
    nb_passed; nb_failed;
    accuracy }

let pp_result (res : result) =
  Printf.printf "Accuracy: %.3f (%d/%d)\n" res.accuracy res.nb_passed (res.nb_passed + res.nb_failed)
    

let main () =
  let tasks = List.map (fun name -> name, Task.from_file (train_dir ^ name)) train_files in
  let res = evaluation tasks in
  pp_result res

let _ = main ()
