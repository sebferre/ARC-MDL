
Common.prof_on := false (* required because primitive unix_times not supported by js_of_ocaml *)

exception TODO

let ( let| ) res f = Result.bind res f [@@inline]
        
(* ---- LIS -------- *)
        
type 'a triple = 'a Model2.triple
        
type arc_state =
  { name : string; (* task name *)
    task : Task.task; (* task *)
    norm_dl_model_data : Model2.grid_pairs_read -> Model2.dl triple triple;
    refinement : Model2.refinement; (* previous refinement *)
    model : Model2.model; (* current model *)
    gprs : Model2.grid_pairs_read; (* pair reads *)
    gsri : Model2.grids_read; (* input reads *)
    gsro : Model2.grids_read; (* output reads *)
    dls : Mdl.bits Model2.triple Model2.triple; (* normalized DL components *)
    dl : Mdl.bits; (* global normalized DL *)
  }

type arc_focus = arc_state
               
type arc_extent = arc_state

type task_input = (string * Task.task) Focus.input (* name, data *)
                
type arc_suggestion =
  | InputTask of task_input
  | RefinedState of arc_state

let state_of_model (name : string) (task : Task.task) norm_dl_model_data (r : Model2.refinement) (m : Model2.model) : (arc_state, exn) Result.t =
  let| gprs = Model2.read_grid_pairs m task.Task.train in
  let gsri, gsro = Model2.split_grid_pairs_read gprs in
  let (_, _, (_,_,dl) as dls) = norm_dl_model_data gprs in
  Result.Ok
    { name; task;
      norm_dl_model_data;
      refinement = r;
      model = m;
      gprs; gsri; gsro;
      dls;
      dl }  
               
let grid0 = Grid.make 10 10 Grid.black
let pair0 = {Task.input = grid0; output = grid0}
let task0 = {Task.train = [pair0]; test = [pair0]}
let name0 = "00000000"

let initial_focus (name : string) (task : Task.task) : arc_focus =
  let norm_dl_model_data = Model2.make_norm_dl_model_data () in
  match state_of_model name task norm_dl_model_data Model2.RInit Model2.init_model with
  | Result.Ok s -> s
  | Result.Error exn -> raise exn

        
class arc_place (lis : arc_lis) (focus : arc_focus) =
object
  inherit [arc_lis,arc_focus,arc_extent,arc_suggestion] Lis.place lis focus

  method eval k_extent k_suggestions =
    k_extent focus;
    Jsutils.firebug "Computing suggestions...";
    let suggestions =
      Model2.model_refinements focus.refinement focus.model focus.gsri focus.gsro
      |> Myseq.slice ~offset:0 ~limit:100 (* TODO: make it configurable *)
      |> Myseq.to_rev_list
      |> List.filter_map
           (fun (r,m) ->
             state_of_model focus.name focus.task focus.norm_dl_model_data r m
             |> Result.to_option)
      |> List.sort
           (fun s1 s2 -> Stdlib.compare s1.dl s2.dl)
      |> List.map
           (fun s ->
             `Sugg (RefinedState (s :> arc_state))) in
    let suggestions =
      `Sugg (InputTask (new Focus.input (name0,task0)))
      ::suggestions in
    Jsutils.firebug "Suggestions computed";
    k_suggestions [suggestions]

  method activate = function
    | InputTask i ->
       let name, task = i#get in
       let state = initial_focus name task in
       Some (new arc_place lis state)
    | RefinedState s ->
       Some (new arc_place lis s)

  method abort = ()

  method json = raise TODO

  method results = raise TODO
end

and arc_lis =
object (self)
  inherit [arc_place] Lis.lis

  method initial_place =
    let state0 = initial_focus name0 task0 in
    new arc_place (self :> arc_lis) state0

  method place_of_json json = raise TODO
end

let make_lis (args : (string * string) list) =
  new arc_lis

(* ------- WEBAPP --------- *)
  
type arc_word

type arc_input = [`Task of task_input]
   
let html_dl dl =
  Printf.sprintf "%.3f" dl
   
let xml_of_focus focus =
  [Syntax.Block
     [[Syntax.Kwd (Printf.sprintf "Task %s" focus.name)];
      [Syntax.Kwd ("DL = " ^ html_dl focus.dl)];
      [Syntax.Kwd (Html.pre (Model2.string_of_model focus.model))]]]
                                      
let html_of_word (w : arc_word) : Html.t = assert false

let html_info_of_input (input : arc_input) : Html.input_info =
  match input with
  | `Task input ->
     Html.fileElt_info
       (fun (fname,contents) k ->
         let task_name = Filename.chop_extension (Filename.basename fname) in
         let json = Yojson.Safe.from_string contents in
         let task = Task.task_of_json json in
         input#set (task_name, task);
         k ())
                                         
let html_of_suggestion ~input_dico = function
  | InputTask i ->
     let info = html_info_of_input (`Task i) in
     let key = input_dico#add info in
     Html.html_of_input_info key info ^ " a task"
  | RefinedState s ->
     Jsutils.escapeHTML ("(" ^ html_dl s.dl ^ ")  " ^ Model2.string_of_refinement s.refinement)

let html_of_grid (g : Grid.t) =
  let buf = Buffer.create 1000 in
  Buffer.add_string buf "<table class=\"arc-grid\">";
  for i = 0 to g.height - 1 do
    Buffer.add_string buf "<tr>";
    for j = 0 to g.width - 1 do
      Buffer.add_string buf (Printf.sprintf "<td class=\"arc-cell arc-col%d\"></td>" g.matrix.{i,j})
    done;
    Buffer.add_string buf "</tr>"
  done;
  Buffer.add_string buf "</table>";
  Buffer.contents buf

let html_of_grid_from_data data =
  match Model2.grid_of_data data with
  | Result.Ok g -> html_of_grid g
  | Result.Error exn -> "(undefined grid)"

let html_grid_pair html_i html_o =
  html_i ^ "&nbsp;" ^ html_o
    
type col = ColExample | ColDescr | ColPred
type cell =
  | Example of Grid.t * Grid.t
  | Descr of Model2.grid_read * Model2.grid_read
  | Pred of Model2.grid_data * Grid.t
  | Error of string
                                    
let html_of_cell : cell -> Html.t = function
  | Example (gi,go) ->
     html_grid_pair
       (html_of_grid gi)
       (html_of_grid go)
  | Descr (ri,ro) ->
     let (_, {data=d_i}, dli : Model2.grid_read) = ri in
     let (_, {data=d_o}, dlo : Model2.grid_read) = ro in
     html_grid_pair
       (html_of_grid_from_data d_i)
       (html_of_grid_from_data d_o)
     ^ Printf.sprintf "<br/>DL = %.3f = %.3f + %.3f" (dli +. dlo) dli dlo
     ^ Html.pre ("IN " ^ Model2.string_of_data d_i)
     ^ Html.pre ("OUT " ^ Model2.string_of_data d_o)
  | Pred (gd_i,go) ->
     let d_i = gd_i.Model2.data in
     html_grid_pair
       (html_of_grid_from_data d_i)
       (html_of_grid go)
     ^ Html.pre ("IN " ^ Model2.string_of_data d_i)
  | Error msg -> Jsutils.escapeHTML msg
        
let w_focus : (arc_word, unit, arc_focus) Widget_focus.widget =
  new Widget_focus.widget
    ~id:"lis-focus"
    ~html_of_word

let w_suggestions : arc_suggestion Widget_suggestions.widget =
  new Widget_suggestions.widget
    ~id:"lis-suggestions"
    ~html_of_suggestion

let cols = [ColExample; ColDescr; ColPred]
let w_results : (col, cell) Widget_table.widget =
  new Widget_table.widget
    ~id:"lis-results"
    ~html_of_column:(fun col ->
      let html =
        match col with
        | ColExample -> "Example"
        | ColDescr -> "Description"
        | ColPred -> "Prediction" in
      None, None, None, html)
    ~html_of_cell


let render_place place k =
  let xml = xml_of_focus place#focus in
  w_focus#set_syntax xml;
  place#eval
    (fun ext ->
      let l_bindings =
        List.map2
          (fun pair reads ->
            let ({input=gi; output=go} : Task.pair) = pair in
            let descr =
              match reads with
              | (in_r,out_r,dl)::_ -> Descr (in_r,out_r) (* using only first read *)
              | [] -> Error "No valid description" in
            let pred =
              match Model2.apply_model ext.model gi with
              | Result.Ok ((gdi, go)::_) -> Pred (gdi,go) (* using only first trial *)
              | Result.Ok [] -> Error "No valid prediction"
              | Result.Error exn -> Error (Printexc.to_string exn) in
            [ ColExample, Example (gi,go);
              ColDescr, descr;
              ColPred, pred ])
          ext.task.train ext.gprs.Model2.reads in
      w_results#set_contents cols l_bindings)
    (fun suggestions ->
      w_suggestions#set_suggestions ["col-md-12 col-xs-12"] suggestions;
      let suggestion_handler =
        (fun sugg ->
          match place#activate sugg with
          | Some p -> k ~push_in_history:true p
          | None -> assert false) in
      w_suggestions#on_suggestion_selection suggestion_handler)

let handle_document_keydown ev place k =
  false

let error_message : exn -> string = function
  | exn -> "Unexpected error: " ^ Printexc.to_string exn

let _ =
  Jsutils.firebug "Start...";
  Webapp.start
    ~make_lis
    ~render_place
    ~handle_document_keydown
    ~error_message

