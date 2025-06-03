
open Madil_common
open Arc_common
open Grid

module Intset = Intset.Intmap


(* Shapes *)

let generate_line (len : int) (dir : int * int) (* {-1,0,1}x{-1,0,1} *) : Grid.t result (* mask *) =
  let di, dj = dir in (* TODO: check that value -1, 0 or 1 ? *)
  if len >= 2 && di >= -1 && di <= 1 && dj >= -1 && dj <= 1 then
    let rec aux n i j acc =
      if n = 0
      then i, j, acc
      else
        let i1, j1 = i + di, j + dj in
        aux (n-1) i1 j1 ((i1,j1)::acc)
    in
    let i_last, j_last, lpos = aux (len-1) 0 0 [(0,0)] in
    let h, w = abs i_last + 1, abs j_last + 1 in
    let offset_i, offset_j = min 0 i_last, min 0 j_last in
    let g = Grid.make h w Grid.transparent in
    List.iter
      (fun (i,j) -> Grid.Do.set_pixel g (i - offset_i) (j - offset_j) Grid.black)
      lpos;
    Result.Ok g
  else Result.Error (Failure "invalid line")

let parse_line (g : Grid.t) (* mask *) : (int * (int * int)) option = (* len, dir *) (* QUICK *)
  (* TODO: possibly extend to direction with values higher than 1 *)
  let h, w = Grid.dims g in
  let nb_off = g.color_count.(Grid.transparent) in
  let nb_on = g.color_count.(Grid.black) in
  if not (h * w = nb_off + nb_on) then None (* not a valid mask *)
  else if h = 1 && w > 1 && nb_on = w then Some (w, (0,1))
  else if w = 1 && h > 1 && nb_on = h then Some (h, (1,0))
  else if h = w && nb_on = h then (
    let ok = ref true in
    for k = 0 to h-1 do
      ok := !ok && g.matrix.{k,k} = Grid.black (* diag1 *)
    done;
    if !ok then Some (h, (1,1))
    else (
      ok := true;
      for k = 0 to h-1 do
        ok := !ok && g.matrix.{k, w-k-1} = Grid.black (* diag2 *)
      done;
      if !ok then Some (h, (1,-1))
      else None))
  else None

(*let _ = (* unit test *)
  print_endline "UNIT TEST Grid_patterns.make_line";
  let g = generate_line 3 (0,1) in
  pp Grid.xp_grid g;
  (match parse_line g with
   | None -> print_endline "not a line"
   | Some (len, (di,dj)) -> Printf.printf "len=%d, dir=(%d,%d)\n" len di dj)*)

let parse_skyline (g : Grid.t) (* mask *) : ((int * int) * int list) option = (* dir, lpos *)
  let h, w = Grid.dims g in
  assert (h > 0 && w > 0);
  let aux_vertical () =
    let dir_i = 0 in
    let dir_j = ref 0 in
    let ok = ref true in
    let rev_lpos = ref [] in
    for i = 0 to h-1 do
      if !ok then (
        let state = ref (`Left (Grid.Mask.mem i 0 g)) in
        for j = 1 to w-1 do
          let b = Grid.Mask.mem i j g in
          match !state with
          | `Left b0 ->
             if b0 <> b then
               state := `Right (j, b)
          | `Right (_, b0) ->
             if b0 <> b then
               state := `Wrong
          | `Wrong -> ()      
        done;
        match !state with
        | `Left false -> (* all false *)
           rev_lpos := 0 :: !rev_lpos
        | `Left true -> (* all true *)
           rev_lpos := w :: !rev_lpos
        | `Right (ji, false) -> (* change from true to false at j *)
           if !dir_j = (-1)
           then ok := false (* inconsistent j *)
           else (
             dir_j := 1;
             rev_lpos := ji :: !rev_lpos
         )
        | `Right (ji, true) -> (* change from false to true at j *)
           if !dir_j = 1
           then ok := false (* inconsistent j *)
         else (
             dir_j := (-1);
             rev_lpos := (w - ji) :: !rev_lpos
           )
        | `Wrong ->
           ok := false
      )
    done;
    if !ok && !dir_j <> 0
    then Some ((dir_i, !dir_j), List.rev !rev_lpos)
    else None
  and aux_horizontal () =
    let dir_i = ref 0 in
    let dir_j = 0 in
    let ok = ref true in
    let rev_lpos = ref [] in
    for j = 0 to w-1 do
      if !ok then (
        let state = ref (`Left (Grid.Mask.mem 0 j g)) in
        for i = 1 to h-1 do
          let b = Grid.Mask.mem i j g in
          match !state with
          | `Left b0 ->
             if b0 <> b then
               state := `Right (i, b)
          | `Right (_, b0) ->
             if b0 <> b then
               state := `Wrong
          | `Wrong -> ()      
      done;
        match !state with
        | `Left false -> (* all false *)
           rev_lpos := 0 :: !rev_lpos
        | `Left true -> (* all true *)
           rev_lpos := h :: !rev_lpos
        | `Right (ij, false) -> (* change from true to false at i *)
           if !dir_i = (-1)
           then ok := false (* inconsistent i *)
           else (
             dir_i := 1;
             rev_lpos := ij :: !rev_lpos
         )
        | `Right (ij, true) -> (* change from false to true at i *)
           if !dir_i = 1
           then ok := false (* inconsistent i *)
         else (
             dir_i := (-1);
             rev_lpos := (h - ij) :: !rev_lpos
           )
        | `Wrong ->
           ok := false
      )
    done;
    if !ok && !dir_i <> 0
    then Some ((!dir_i, dir_j), List.rev !rev_lpos)
    else None
  in
  match aux_vertical () with
  | None -> aux_horizontal ()
  | res -> res


(* Coloring *)

let recoloring (g : Grid.t) : (Grid.t * Grid.color array) result = (* QUICK *)
  (* normalize grid coloring, and return a color palette to give original coloring *)
  (* transparent and undefined are left transparent *)
  let nc = Grid.color_count Grid.transparent g in
  let minpos = Array.make Grid.nb_color (max_int,max_int) in    
  let palette = Array.make nc Grid.undefined in (* color in range(0,nc) -> color in g *)
  let rev_palette = Array.make Grid.nb_color Grid.undefined in (* color in g -> color in range(0,nc) *)
  (* defining minpos: color -> min (i,j) s.t. g(i,j)=c *)
  Grid.iter_pixels
    (fun i j c ->
      if Grid.is_true_color c
      then minpos.(c) <- min minpos.(c) (i,j))
    g;
  (* sorting colors by descending frequency, then by ascending min pos in grid *)
  Grid.color_freq_desc g
  |> List.map (fun (n,c) -> let mini, minj = minpos.(c) in (n,mini,minj,c))
  |> List.sort (fun (n1,i1,j1,c1) (n2,i2,j2,c2) -> Stdlib.compare (n2,i1,j1) (n1,i2,j2))
  |> List.iteri (fun rank (n,i,j,c) ->
         palette.(rank) <- c;
         rev_palette.(c) <- rank);
  (* computing the normalized grid *)
  let gres =
    Grid.map_pixels
      (fun c ->
        if Grid.is_true_color c
        then rev_palette.(c)
        else c)
      g in
  Result.Ok (gres, palette)

let recolor (g : Grid.t) (palette : Grid.color array) : Grid.t =
  let nc = Array.length palette in
  Grid.map_pixels
    (fun c ->
      if c >= 0 && c < nc
      then palette.(c)
      else c)
    g

let parse_recoloring (g : Grid.t) (g1 : Grid.t) : (Grid.color,Grid.color) Mymap.t option = (* QUICK *)
  (* is g a recoloring of g1, and how? only for true colors *)
  let h, w = Grid.dims g in
  let h1, w1 = Grid.dims g1 in
  if h=h1 && w=w1
  then
    let m = ref (Mymap.empty : (color,color) Mymap.t) in
    let ok = ref true in
    let i = ref 0 in
    while !ok && !i < h do
      let j = ref 0 in
      while !ok && !j < w do
        let c = g.matrix.{!i,!j} in
        let c1 = g1.matrix.{!i,!j} in
        if Grid.is_true_color c && Grid.is_true_color c1
        then
          (match Mymap.find_opt c1 !m with
          | Some c' ->
             if c' <> c then ok := false (* inconsistency *)
          | None ->
             m := Mymap.add c1 c !m)
        else
          if c <> c1 then ok := false; (* different mask *)
        incr j
      done;
      incr i
    done;
    if !ok
    then Some !m
    else None
  else None (* grid size mismatch *)
  

(* crop *)

let parse_crop (g : Grid.t) (g1 : Grid.t) : (int * int) list = (* QUICK *)
  (* empty result if more than 3 occs *)
  let h, w = Grid.dims g in
  let h1, w1 = Grid.dims g1 in
  if h1 <= h && w1 <= w
  then (
    let res = ref [] in
    for i = 0 to h-h1 do
      for j = 0 to w-w1 do
        if Grid.for_all_pixels
             (fun i1 j1 c1 -> c1 = g.Grid.matrix.{i+i1, j+j1})
             g1
        then res := (i,j)::!res
      done
    done;
    if List.length !res <= 9
    then List.rev !res
    else [])
  else []
  

(* Repeat (a la NumPy) *)

let parse_repeat (g : Grid.t) : (Grid.t * int list * int list) option = (* QUICK *)
  (* returns: the compressed grid, the numbers of repeats on axis i, and the numbers of repeats on axis j *)
  let rec cumsum offset = function
    | [] -> []
    | n::l -> offset :: cumsum (offset+n) l
  in
  let h, w = Grid.dims g in
  let mat = g.matrix in
  let i_repeats =
    let res = ref [] in
    let pos_end = ref (h-1) in
    let same_row = ref true in
    for i = h-2 downto 0 do
      (* comparing row [i] to the following one *)
      for j = 0 to w-1 do
        same_row := !same_row && mat.{i,j} = mat.{i+1,j}
      done;
      (* updating res and pos_end *)
      if not !same_row then (
        res := (!pos_end - i) :: !res;
        pos_end := i
      );
      (* preparing next iteration *)
      same_row := true
    done;
    res := (!pos_end - (-1)) :: !res;
    !res in
  let j_repeats =
    let res = ref [] in
    let pos_end = ref (w-1) in
    let same_col = ref true in
    for j = w-2 downto 0 do
      (* comparing col [j] to the following one *)
      for i = 0 to h-1 do
        same_col := !same_col && mat.{i,j} = mat.{i,j+1}
      done;
      (* updating res and pos_end *)
      if not !same_col then (
        res := (!pos_end - j) :: !res;
        pos_end := j
      );
      (* preparing next iteration *)
      same_col := true
    done;
    res := (!pos_end - (-1)) :: !res;
    !res in
  let h1, w1 = List.length i_repeats, List.length j_repeats in
  if (* h1 <= 5 && w1 <= 5 (* not too many areas *)
     && *) (h1 > 1 || w1 > 1) (* not a single area *)
     && (List.exists (fun n -> n > 1) i_repeats
         || List.exists (fun n -> n > 1) j_repeats) (* no trivial repeat *)
  then
    let g1 = Grid.make h1 w1 Grid.transparent in
    let i_poslist = cumsum 0 i_repeats in
    let j_poslist = cumsum 0 j_repeats in
    List.iteri
      (fun i1 i ->
        List.iteri
          (fun j1 j ->
            Grid.Do.set_pixel g1 i1 j1 mat.{i,j})
          j_poslist)
      i_poslist;
    Some (g1, i_repeats, j_repeats)
  else None

let generate_repeat (g1 : Grid.t) (i_repeats : int list) (j_repeats : int list) : Grid.t result =
  let rec correct_repeats len repeats =
    if len = 0 then []
    else
      match repeats with
      | [] -> 1 :: correct_repeats (len-1) repeats
      | r::l -> r :: correct_repeats (len-1) l in
  let rec ranges offset = function
    | [] -> []
    | n::l -> (offset, offset + n - 1) :: ranges (offset+n) l
  in
  Common.prof "Grid_patterns.generate_repeat" (fun () ->
  let h1, w1 = Grid.dims g1 in
  let i_repeats = correct_repeats h1 i_repeats in    
  let j_repeats = correct_repeats w1 j_repeats in
  if (List.for_all (fun n -> n > 0) i_repeats)
     && (List.for_all (fun n -> n > 0) j_repeats)
  then
    let h = List.fold_left (+) 0 i_repeats in
    let w = List.fold_left (+) 0 j_repeats in
    let g = Grid.make h w Grid.transparent in
    let i_ranges = ranges 0 i_repeats in
    let j_ranges = ranges 0 j_repeats in
    List.iteri
      (fun i1 (start_i,end_i) ->
        List.iteri
          (fun j1 (start_j,end_j) ->
            let c1 = Grid.get_pixel ~source:"Grid_patterns.generate_repeat" g1 i1 j1 in
            for i = start_i to end_i do (* TODO: add function Grid.set_rectangle *)
              for j = start_j to end_j do
                Grid.Do.set_pixel g i j c1
              done
            done)
          j_ranges)
      i_ranges;
    Result.Ok g
  else
    Result.Error (Failure "Invalid Repeat pattern"))

(*let _ = (* unit test of repeats *)
  print_endline "UNIT TEST of parse/generate_repeat";
  let h1, w1 = 1, 4 in
  let i_repeats, j_repeats = [1], [4;1;2;3] in
  let g1 = Grid.init h1 w1 (fun i1 j1 -> (i1 * w1 + j1) mod Grid.nb_color) in
  pp_endline Grid.xp_grid g1;
  let g = generate_repeat g1 i_repeats j_repeats in
  pp_endline Grid.xp_grid g;
  let g1', i_repeats, j_repeats = parse_repeat g in
  pp_endline Grid.xp_grid g1';
  List.iter print_int i_repeats; print_string " / "; List.iter print_int j_repeats; print_newline ();
  print_endline "DONE"*)


(* Segmentation into Objects *)
   
type part = { mini : int; maxi : int;
	      minj : int; maxj : int;
	      nb_pixels : int;
	      pixels : Bitmap.t }

let subgrid_of_part (g : Grid.t) (p : part) : Grid.t = (* QUICK *)
  let h1, w1 = p.maxi - p.mini + 1, p.maxj - p.minj + 1 in
  let g1 = Grid.make h1 w1 Grid.transparent in
  Bitmap.iter
    (fun i j ->
      Grid.Do.set_pixel g1 (i - p.mini) (j - p.minj) g.matrix.{i,j})
    p.pixels;
  g1
let subgrid_of_part, reset_subgrid_of_part =
  Memo.memoize2 ~size:103 subgrid_of_part

let part_of_pixel ~height ~width i j c =
  { mini = i; maxi = i;
    minj = j; maxj = j;
    pixels = Bitmap.singleton height width i j;
    nb_pixels = 1 }

let merge_parts p1 p2 =
  { mini = min p1.mini p2.mini;
    maxi = max p1.maxi p2.maxi;
    minj = min p1.minj p2.minj;
    maxj = max p1.maxj p2.maxj;
    pixels = Bitmap.union p1.pixels p2.pixels;
    nb_pixels = p1.nb_pixels + p2.nb_pixels }

module Objects =
  struct

type segmentation =
  | Connected of connectedness * bool (* one-color *)
  | SameColor
and connectedness =
  | Connect8
  | Connect4
  | Connect2_row
  | Connect2_col

let candidate_segmentations_connected nocolor =
  if nocolor (* on masks *)
  then
    [ Connected (Connect8,false);
      Connected (Connect4,false) ]
  else
    [ Connected (Connect8,true);
      Connected (Connect8,false);
      Connected (Connect4,true);
      Connected (Connect4,false);
      Connected (Connect2_row,true);
      Connected (Connect2_col,true) ]

let rec xp_segmentation ~html print = function
  | Connected (conn,mono) ->
     if mono then print#string "same-color ";
     xp_connectedness ~html print conn
  | SameColor -> print#string "same-color"
and xp_connectedness ~html print = function
  | Connect8 -> print#string "8-connected"
  | Connect4 -> print#string "4-connected"
  | Connect2_row -> print#string "same-row"
  | Connect2_col -> print#string "same-column"

let seg_conn_opt : segmentation -> connectedness option = function
  | Connected (conn, _) -> Some conn
  | SameColor -> None

let connectedness_axes (conn : connectedness) : bool * bool * bool * bool = (* row, col, diag1, diag2 *)
  match conn with
  | Connect8 -> (true, true, true, true)
  | Connect4 -> (true, true, false, false)
  | Connect2_row -> (true, false, false, false)
  | Connect2_col -> (false, true, false, false)

let disconnected_area (conn : connectedness) (g : Grid.t) : int = (* QUICK *)
  let h, w = Grid.dims g in
  let mat = g.matrix in
  let c_row, c_col, c_diag1, c_diag2 = connectedness_axes conn in
  let res = ref 0 in (* nb of disconnected cells, i.e. having only transparent neighbors *)
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let left, right = j-1, j+1 in
      let up, down = i-1, i+1 in
      (* computing ok = transparent and all neighbors are transparent *)
      let ok = ref (mat.{i,j} = Grid.transparent) in
      if !ok && c_row && left >= 0 then ok := mat.{i, left} = Grid.transparent;
      if !ok && c_row && right < w then ok := mat.{i, right} = Grid.transparent;
      if !ok && c_col && up >= 0 then ok := mat.{up, j} = Grid.transparent;
      if !ok && c_col && down < h then ok := mat.{down, j} = Grid.transparent;
      if !ok && c_diag1 && up >= 0 && left >= 0 then ok := mat.{up, left} = Grid.transparent;
      if !ok && c_diag1 && down < h && right < w then ok := mat.{down, right} = Grid.transparent;
      if !ok && c_diag2 && up >= 0 && right < w then ok := mat.{up, right} = Grid.transparent;
      if !ok && c_diag2 && down < h && left >= 0 then ok := mat.{down, left} = Grid.transparent;
      if !ok then incr res
    done
  done;
  !res


type obj = int * int * Grid.t (* object *)
type t = obj list * Grid.t (* noise *)

let segment_gen
      (nmax : int)
      (c_row, c_col, c_diag1, c_diag2 : bool * bool * bool * bool)
      (c_samecolor : bool)
      (g : Grid.t)
    : t = (* objects and noise *)
  Common.prof "Grid.segment" (fun () ->
  let h, w = Grid.dims g in
  let fm : (int * int, part) Find_merge.hashtbl =
    new Find_merge.hashtbl
      ~init_val:{ mini = h; maxi = 0;
                  minj = w; maxj = 0;
                  pixels = Bitmap.empty h w;
                  nb_pixels = 0 }
      ~merge_val:merge_parts
  in
  let mat = g.matrix in
  (* setting initial val of each pixel *)
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let c = mat.{i,j} in
      if c <> Grid.transparent then
        fm#replace (i,j) (part_of_pixel ~height:h ~width:w i j c)
    done
  done;
  (* merging connected pixels *)
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let c = mat.{i,j} in
      (*let pix = (i,j,c) in*)
      if c <> Grid.transparent then (
        (* pixel on the right *)
        let j_right = j+1 in
        if c_row && j_right < w then (
          let c_right = mat.{i,j_right} in
          if c_right <> Grid.transparent && (not c_samecolor || c_right = c) then
	    ignore (fm#merge [(i,j); (i,j_right)]));
        (* pixel down *)
        let i_down = i+1 in
        if c_col && i_down < h then (
          let c_down = mat.{i_down,j} in
          if c_down <> Grid.transparent && (not c_samecolor || c_down = c) then
	    ignore (fm#merge [(i,j); (i_down,j)]));
        (* pixel right and down, diagonally *)
        let i_diag1 = i+1 in
        let j_diag1 = j+1 in
        if c_diag1 && i_diag1 < h && j_diag1 < w then (
          let c_diag1 = mat.{i_diag1,j_diag1} in
          if c_diag1 <> Grid.transparent && (not c_samecolor || c_diag1 = c) then
	    ignore (fm#merge [(i,j); (i_diag1,j_diag1)]));
        (* pixel left and down, diagonally *)
        let i_diag2 = i+1 in
        let j_diag2 = j-1 in
        if c_diag2 && i_diag2 < h && j_diag2 >= 0 then (
          let c_diag2 = mat.{i_diag2,j_diag2} in
          if c_diag2 <> Grid.transparent && (not c_samecolor || c_diag2 = c) then
	    ignore (fm#merge [(i,j); (i_diag2,j_diag2)])))
    done
  done;
  (* collecting parts *)
  let parts =
    fm#fold
      (fun _ part res ->
        let gpart = subgrid_of_part g part in
        let garea = Grid.color_area Grid.transparent gpart in
        (part.mini, part.minj, gpart, garea) :: res)
      [] in
  let sorted_parts =
    List.sort
      (fun (i1,j1,g1,a1) (i2,j2,g2,a2) ->
        Stdlib.compare (a2,i1,j1) (a1,i2,j2)) (* decreasing area first, then increasing i, j *)
      parts in
  let obj_parts, noise_parts = (* considering the smaller objects as noise, when too many objects *)
    let rec aux minsize obj_parts noise_parts =
      let n = List.length obj_parts in
      if n <= nmax
      then obj_parts, noise_parts
      else
        let l1, l2 =
          List.partition
            (fun (i,j,gpart,area) -> area >= minsize) (* TODO: find better, MDL-based? *)
            obj_parts in
        aux (minsize+1) l1 (l2 @ noise_parts) in
    aux 1 sorted_parts [] in
  let objs = List.map (fun (i,j,g,_) -> (i,j,g)) obj_parts in
  let g_noise =
    let g = Grid.make h w Grid.transparent in
    List.iter (fun (i,j,gpart,_) -> Grid.add_grid_at g i j gpart) noise_parts;
    g in
  objs, g_noise)

let segment_connected nmax (conn : connectedness) (samecolor : bool) g =
  segment_gen
    nmax
    (connectedness_axes conn)
    samecolor
    g
let segment_connected, reset_segment_connected =
  Memo.memoize3 ~size:103 segment_connected


(*let _ = (* unit test *)
  print_endline "UNIT TEST Grid_patterns.segment";
  let g = Grid.init 2 10
            (fun i j ->
              match i, j with
              | 0, 1 | 1, 0 | 1, 2 -> Grid.red
              | 0, 5 | 1, 4 | 1, 6 -> Grid.cyan
              | _ -> Grid.transparent) in
  pp Grid.xp_grid g;
  let objs = segment_same_color g in
  List.iter
    (fun (i,j,g1) ->
      pp_endline Grid.xp_grid g1)
    objs*)

let segment_by_color (g : Grid.t) : t = (* objects and noise *)
  Common.prof "Grid_patterns.segment_by_color" (fun () ->
  let h, w = Grid.dims g in
  let mat = g.matrix in
  let color_part =
    Array.make Grid.nb_color (* one potential part per color *)
      { mini = h; maxi = 0;
        minj = w; maxj = 0;
        pixels = Bitmap.empty h w;
        nb_pixels = 0 } in
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let c = mat.{i,j} in
      if Grid.is_true_color c then
        let part = color_part.(c) in
        color_part.(c) <- { mini = min i part.mini;
                            maxi = max i part.maxi;
                            minj = min j part.minj;
                            maxj = max j part.maxj;
                            pixels = Bitmap.add i j part.pixels;
                            nb_pixels = 1 + part.nb_pixels }
    done
  done;
  let parts =
    let res = ref [] in
    Array.iteri
      (fun c part ->
        if part.nb_pixels > 0 then
          let g1 = subgrid_of_part g part in
          let area = part.nb_pixels in
          res := (area, part.mini, part.minj, g1) :: !res)
      color_part;
    !res in
  let sorted_parts =
    List.sort
      (fun (a1,i1,j1,g1) (a2,i2,j2,g2) ->
        Stdlib.compare (a2,i1,j1) (a1,i2,j2)) (* decreasing area first *)
      parts in
  let g_noise = Grid.make h w Grid.transparent in
  List.map (fun (_,i,j,g) -> (i,j,g)) sorted_parts,
  g_noise)

let segment_by_color, reset_segment_by_color =
  Memo.memoize ~size:103 segment_by_color

let parse (nmax : int) seg (g : Grid.t) : t Myseq.t =
  let objs, g_noise =
    match seg with
    | Connected (conn,samecolor) -> segment_connected nmax conn samecolor g
    | SameColor -> segment_by_color g in
  let n = List.length objs in
  if n > 0 && n <= nmax
  then Myseq.return (objs, g_noise)
  else Myseq.empty


type order = Color | AreaMask | Pos

let xp_order ~html print = function
  | Color -> print#string "color"
  | AreaMask -> print#string "area/mask"
  | Pos -> print#string "pos"

let obj_color (i,j,g1) = Grid.majority_color Grid.transparent g1 [@@inline]
let obj_area (i,j,g1) = - (Grid.color_area Grid.transparent g1) [@@inline] (* descending area *)
let obj_mask (i,j,g1) = Grid.Mask.from_grid_background Grid.transparent g1 [@@inline]
let obj_posi (i,j,g1) = let h1, w1 = Grid.dims g1 in float i +. float h1 /. 2. [@@inline]
let obj_posj (i,j,g1) = let h1, w1 = Grid.dims g1 in float j +. float w1 /. 2. [@@inline]
let obj_color_plus obj = obj_color obj, obj_area obj, obj_mask obj
let obj_area_mask_plus obj = obj_area obj, obj_mask obj, obj_color obj

let candidate_orders nmax nocolor =
  if nmax = 1 then [Pos]
  else if nocolor then [AreaMask; Pos]
  else [Color; AreaMask; Pos]

let sort_gen (key : obj -> 'k) (objs : obj list) : obj list =
  let sorted = List.sort Stdlib.compare (List.map (fun obj -> key obj, obj) objs) in
  List.map snd sorted

let sort (order : order) (objs : obj list) : obj list =
  match order with
  | Color -> sort_gen obj_color_plus objs
  | AreaMask -> sort_gen obj_area_mask_plus objs
  | Pos -> List.sort Stdlib.compare objs (* obj = (i,j,g1) *)

let rec single_key (sorted : ('k * obj) list) : bool =
  match sorted with
  | [] -> true
  | [(k,_)] -> true
  | (k1,_)::((k2,_)::_ as r) -> k1 = k2 && single_key r

let rec unique_keys (sorted : ('k * obj) list) : bool =
  match sorted with
  | [] -> true
  | [(k,_)] -> true
  | (k1,_)::((k2,_)::_ as r) -> k1 <> k2 && unique_keys r

  end (* Objects *)

let partition_by_color (g : Grid.t) : (Grid.color * Grid.t (* mask *)) list =
  Common.prof "Grid_patterns.partition_by_color" (fun () ->
  let h, w = Grid.dims g in
  let mat = g.matrix in
  let color_part =
    Array.init Grid.nb_color (* one potential grid per color *)
      (fun c -> (Grid.make h w Grid.zero, ref 0)) in
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let c = mat.{i,j} in
      if Grid.is_true_color c then
        let m, area = color_part.(c) in
        Grid.Do.set_pixel m i j Grid.one;
        incr area
    done
  done;
  let parts =
    let res = ref [] in
    Array.iteri
      (fun c (m,area) ->
        if !area > 0 then
          res := (!area, c, m) :: !res)
      color_part;
    !res in
  let sorted_parts =
    List.sort
      (fun (a1,c1,m1) (a2,c2,m2) ->
        Stdlib.compare (a2,c1) (a1,c2)) (* decreasing area *)
      parts in
  List.map (fun (_,c,m) -> c,m) sorted_parts)

let partition_by_color, reset_partition_by_color =
  Memo.memoize ~size:103 partition_by_color

(*let _ =
  print_endline "UNIT TEST Grid_patterns.partition_by_color";
  let g = Grid.init 6 8 (fun _ _ -> Random.int 4) in
  pp Grid.xp_grid g;
  let lg1s = partition_by_color g in
  List.iter (pp Grid.xp_grid) lg1s*)


(* MOTIFS *)

module Motif =
  struct
    
type t =
  | Scale
  | Periodic of Grid.Transf.axis * Grid.Transf.axis
  | Affine of int * int (* ax + b, same on two axes so far *) 
  (* symmetries *) (* TODO: add symmetry axis/center position *)
  | FlipH | FlipW | FlipHW
  | FlipD1 | FlipD2 | FlipD12
  | Rotate180 | Rotate90
  | FullSym
  | Rings
  (* special motifs *)
  | Corners
  | Border | CrossPlus | CrossTimes | Diamond
  | Star (* CrossPlus+CrossTimes *) (* TODO: other combinations? *)

let xp ~html print = function
  | Scale -> print#string "scale"
  | Periodic (phi,psi) ->
     print#string "periodic["; Grid.Transf.xp_axis print phi;
     print#string ","; Grid.Transf.xp_axis print psi;
     print#string "]"
  | Affine (a,b) ->
     print#string "affine["; print#int a;
     print#string ","; print#int b;
     print#string "]"
  | FlipH -> print#string "flipH"
  | FlipW -> print#string "flipW"
  | FlipHW -> print#string "flipHW"
  | FlipD1 -> print#string "FlipD1"
  | FlipD2 -> print#string "FlipD2"
  | FlipD12 -> print#string "FlipD12"
  | Rotate180 -> print#string "Rotate180"
  | Rotate90 -> print#string "Rotate90"
  | FullSym -> print#string "FullSym"
  | Rings -> print#string "Rings"
  | Corners -> print#string "Corners"
  | Border -> print#string "Border"
  | CrossPlus -> print#string "Cross +"
  | CrossTimes -> print#string "Cross x"
  | Diamond -> print#string "Diamond"
  | Star -> print#string "Star"

let project (mot : t) h w u v : (int -> int -> (int * int) option) =
  (* project coord (i,j) in (h,w) range to (u,v) range, according to motif *)
  let h_1, w_1 = h-1, w-1 in
  match mot with
  | Scale ->
     let k, l = h_1 / u + 1, w_1 / v + 1 in
     (fun i j -> Some (i / k, j / l))
  | Periodic (phi,psi) ->
     let eval_phi = Grid.Transf.eval_axis phi in
     let eval_psi = Grid.Transf.eval_axis psi in
     (fun i j ->
       let a, b = eval_phi i j, eval_psi i j in
       Some (a mod u, b mod v))
  | Affine (a,b) ->
     let proj y = (* positive integer solution to ax + b = y *)
       let z = y - b in
       if z >= 0 && z mod a = 0
       then
         let x = z / a in
         Some x
       else None in
     (fun i j ->
       let@ i' = proj i in
       let@ j' = proj j in
       Some (i',j'))
  | FlipH ->
     (fun i j -> Some (min i (h_1 - i), j))
  | FlipW ->
     (fun i j -> Some (i, min j (w_1 - j)))
  | FlipHW ->
     (fun i j -> Some (min i (h_1 - i), min j (w_1 - j)))
  | FlipD1 ->
     (fun i j ->
       let p, m = i + j, i + (w_1 - j) in
       Some (min p (h_1 + w_1 - p), m))
  | FlipD2 ->
     (fun i j ->
       let p, m = i + j, i + (w_1 - j) in
       Some (p, min m (h_1 + w_1 - m)))
  | FlipD12 ->
     (fun i j ->
       let p, m = i + j, i + (w_1 - j) in
       Some (min p (h_1 + w_1 - p), min m (h_1 + w_1 - m)))
  | Rotate180 ->
     (fun i j -> Some (min (i, j) (h_1 - i, w_1 - j)))
  | Rotate90 ->
     (fun i j ->
       let a, b =
         min (i, j)
           (min (w_1 - j, i)
              (min (h_1 - i, w_1 - j)
                 (j, h_1 - i))) in
       if b >= v
       then Some (w_1 - b, a)
       else Some (a, b))
  | FullSym ->
     (fun i j ->
       let i_min = min i (h_1 - i) in
       let j_min = min j (w_1 - j) in
       Some (min i_min j_min, max i_min j_min))
  | Rings ->
     (fun i j ->
       Some (min (min i (h_1 - i)) (min j (w_1 - j)), 0))
  (* (u,v) = (2,1), shape color at [1,0], bgcolor at [0,0] *)
  | Corners ->
     (fun i j ->
       if (i = 0 || i = h_1) && (j = 0 || j = w_1)
       then Some (1, 0)
       else Some (0, 0))
  | Border ->
     (fun i j ->
       if i = 0 || j = 0 || i = h_1 || j = w_1
       then Some (1, 0)
       else Some (0, 0))
  | CrossPlus ->
     (fun i j ->
       if i = h/2 || i = h_1/2 || j = w/2 || j = w_1/2
       then Some (1, 0)
       else Some (0, 0))
  | CrossTimes ->
     (fun i j ->
       if i = j || i = (w_1-j)
       then Some (1, 0)
       else Some (0, 0))
  | Star ->
     (fun i j ->
       if i = h/2 || i = h_1/2 || j = w/2 || j = w_1/2 (* CrossPlus *)
          || i = j || i = (w_1-j) (* CrossTimes *)
       then Some (1, 0)
       else Some (0, 0))
  | Diamond ->
     assert (h = w);
     (fun i j ->
       let p, m = i + j,  i + (w_1 - j) in
       if p = h_1/2 || p = h_1 + h/2 || m = h_1/2 || m = h_1 + h/2
       then Some (1, 0)
       else Some (0, 0))
             
let all_coredims_of_motif (mot : t) (h : int) (w : int) : Range.t * Range.t * (int * int) list =
  (* range and list of core dimensions (u,v) given a motif and grid dims *)
  match mot with
  | Scale ->
     Range.make_open 1 (* closed 1 h *), (* favoring small cores *)
     Range.make_open 1 (* closed 1 w *),
     Common.fold_for
       (fun u res ->
         if h mod u = 0 (* congruent vertical scale *)
         then
           Common.fold_for
             (fun v res ->
               if w mod v = 0 (* congruent horizontal scale *)
               then
                 if (u = h && v = w) (* not a proper scale *)
                 then res
                 else (u,v)::res
               else res)
             1 w res
         else res)
       1 h []
  | Periodic (phi,psi) ->
     let h', w' = Grid.Transf.bound_axis phi h w, Grid.Transf.bound_axis psi h w in
     let h', w' = min h' Grid.max_size, min w' max_size in (* bounding core size *)
     Range.make_open 1 (* closed 1 h' *),
     Range.make_open 1 (* closed 1 w' *),
     Common.fold_for
       (fun u res ->
         Common.fold_for
           (fun v res ->
             if (u = h' && v = w' && psi <> Zero) (* not a proper periodic *)
             then res
             else (u,v)::res)
           1 w' res)
       1 h' []
  | Affine (a,b) ->
     let u = (h - 1 - b) / a + 1 in
     let v = (w - 1 - b) / a + 1 in
     Range.make_exact u,
     Range.make_exact v,
     [u,v]
  | FlipH ->
     let u, v = (h+1)/2, w in
     Range.make_exact u,
     Range.make_exact v,
     [u,v]
  | FlipW ->
     let u, v = h, (w+1)/2 in
     Range.make_exact u,
     Range.make_exact v,
     [u,v]
  | FlipHW ->
     let u, v = (h+1)/2, (w+1)/2 in
     Range.make_exact u,
     Range.make_exact v,
     [u, v]
  | FlipD1 ->
     let hw' = h+w-1 in (* projected dim, diagonal size *)
     if h = w && hw' <= Grid.max_size
     then
       let u, v = (hw'+1)/2, hw' in (* only half is used *)
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []
  | FlipD2 ->
     let hw' = h+w-1 in
     if h = w && hw' <= Grid.max_size
     then
       let u, v = hw', (hw'+1)/2 in
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []
  | FlipD12 ->
     let hw' = h+w-1 in
     if h = w && hw' <= Grid.max_size
     then
       let u, v = (hw'+1)/2, (hw'+1)/2 in
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []
  | Rotate180 ->
     let u, v = (h+1)/2, w in
     Range.make_exact u,
     Range.make_exact v,
     [u, v]
  | Rotate90 ->
     if h = w
     then
       let u, v = (h+1)/2, (w+1)/2 in
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []
  | FullSym ->
     if h = w
     then
       let u, v = (h+1)/2, (w+1)/2 in (* only half is used, diagonal core *)
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []
  | Rings ->
     let min_hw = min h w in
     if min_hw >= 3
     then
       let u, v = (min_hw+1)/2, 1 in
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []
  | Corners | Border | CrossPlus ->
     if h >= 3 && w >= 3
     then
       let u, v = 2, 1 in
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []            
  | CrossTimes | Diamond | Star ->
     if h = w && h >= 3
     then
       let u, v = 2, 1 in
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []            

let make_core_bi bgcolor color =
  Grid.init 2 1 (fun i j -> if i = 0 then bgcolor else color)

let make_grid (h : int) (w : int) (mot : t) (core : Grid.t) : Grid.t result = (* QUICK *)
  let u, v = Grid.dims core in
  let ru, rv, luv = all_coredims_of_motif mot h w in
  if List.mem (u,v) luv
  then
    let proj = project mot h w u v in
    let g =
      Grid.init h w
        (fun i j ->
          match proj i j with
          | Some (i',j') ->
             assert (i' >= 0 && i' < u && j' >= 0 && j' < v);
             (* pp xp mot; Printf.printf " (%d,%d) -> (%d,%d) [%d,%d]\n" h w u v i' j';
                assert false); *)
             core.Grid.matrix.{i',j'}
          | None -> Grid.transparent) in
    if g.color_count.(Grid.undefined) = 0
    then Result.Ok g
    else Result.Error (Failure "Grid_patterns.Motif.make_grid: undefined cells")
  else Result.Error (Failure "Grid_patterns.Motif.make_grid: incompatible motif and core grid with grid size")
(*let make_grid, reset_make_grid = (* TODO: there is a confusing bug, grids get mixed *)
  Memo.memoize4 ~size:Grid.memoize_size make_grid*)

(* discovering motifs in grids *)
  
let candidates_multi = (* multicolor motifs *)
  let open Grid.Transf in
  [ Scale;
    FlipH; FlipW; FlipHW;
    FlipD1; FlipD2; FlipD12;
    Rotate180; Rotate90;
    FullSym;
    Rings;
    Periodic (I, J);
    Periodic (I, PlusIJ);
    Periodic (PlusIJ, J);
    Periodic (PlusIJ, DiffIJ);
    Periodic (I, Zero);
    Periodic (J, Zero);
    Periodic (PlusIJ, Zero);
    Periodic (DiffIJ, Zero);
    Periodic (MaxIJ, Zero);
    Periodic (MinIJ, Zero);
    Affine (2,0);
    Affine (2,1);
    Affine (3,0);
    Affine (3,1);
    Affine (3,2);
  ]
let nb_affine_params = 3

let candidates_bi = (* bicolor shape-like motifs *)
  let open Grid.Transf in
  [ Border; Corners; CrossPlus; CrossTimes; Diamond; Star ]

let weight : t -> float = function
  | Scale -> 0.3

  (* 0.3 *)
  | FullSym -> 0.3 *. 0.25
  | Rings -> 0.3 *. 0.10

  | FlipHW -> 0.3 *. 0.2
  | FlipH -> 0.3 *. 0.05
  | FlipW -> 0.3 *. 0.05

  | FlipD12 -> 0.3 *. 0.1
  | FlipD1 -> 0.3 *. 0.05
  | FlipD2 -> 0.3 *. 0.05

  | Rotate90 -> 0.3 *. 0.1
  | Rotate180 -> 0.3 *. 0.05

  (* 0.4 *)
  | Periodic (a,b) ->
     0.2 *.
     (match a, b with
      | I, J -> 0.2
      | I, PlusIJ -> 0.04
      | PlusIJ, J -> 0.04
      | PlusIJ, DiffIJ -> 0.04
      | I, Zero -> 0.2
      | J, Zero -> 0.2
      | PlusIJ, Zero -> 0.1
      | DiffIJ, Zero -> 0.1
      | MaxIJ, Zero -> 0.04
      | MinIJ, Zero -> 0.04
      | _ -> assert false)
  | Affine (a,b) ->
     0.2 /. float nb_affine_params
  | Border | Corners | CrossPlus | CrossTimes | Diamond | Star -> 0.1


let from_grid (candidates : t list) (bgcolor : Grid.color) (g : Grid.t) : (t * Range.t * Range.t * Grid.t * Grid.t option * Grid.t) list = (* list of (motif, range_u, range_v, (u,v)-sized core, mask, noise) that [g] agreeds to as pure(motif,core,size(noise)) & mask + noise *)
  (* bgcolor is the color to be ignored *)
  Common.prof "Grid_patterns.Motif.from_grid" (fun () ->
  let h, w = Grid.dims g in
  let area = Grid.color_area Grid.transparent g in
  (* color stats: lists of (color,count) pairs *)
  let add_color c (n,n_def,cstats) =
    let rec aux_cstats = function
      | [] -> [(c,1)]
      | (c0,n0)::cstats1 ->
         if c = c0 then (c0,n0+1)::cstats1
         else
           match aux_cstats cstats1 with
           | (c1,n1)::cstats2 when n1 > n0 ->
              (c1,n1)::(c0,n0)::cstats2
           | cstats1 -> (c0,n0)::cstats1
    in
    if c = bgcolor
    then (n+1, n_def, cstats)
    else (n+1, n_def+1, aux_cstats cstats)
  in
  (* initialization *)
  let motifs =
    List.fold_left
      (fun res mot ->
        let ru, rv, luv = all_coredims_of_motif mot h w in
        let cores =
          List.fold_left
            (fun res2 (u,v) ->
              let proj = project mot h w u v in
              let ncols = Array.make_matrix u v (0,0,[]) in 
              (u,v,proj,ncols)::res2)
            [] luv in
        (mot,ru,rv,cores)::res)
      [] candidates in
  (* color stats through one pass of the grid pixels *)
  List.iter (* much more efficient to iterate on motifs first, then on pixels *)
    (fun (mot,ru,rv,cores) ->
      List.iter
        (fun (u,v,proj,ncols) ->
          Grid.iter_pixels
            (fun i j c ->
              match proj i j with
              | Some (i', j') ->
                 assert (i' >= 0 && i' < u && j' >= 0 && j' < v);
                 (*if not (i' >= 0 && i' < u && j' >= 0 && j' < v) then (
                   pp xp mot; Printf.printf " (%d,%d) [%d,%d]\n" u v i' j';
                   assert false);*)
                 ncols.(i').(j') <- add_color c ncols.(i').(j')
              | None -> ()) (* pixel at (i,j) does not contribute to core *)
            g)
        cores)
    motifs;
  (* collecting results *)
  let res =
    List.fold_left
      (fun res (mot,ru,rv,cores) ->
        let cores_ok =
          List.filter_map
            (fun (u,v,proj,cols) ->
              (* building list of equiv classes with ratio of 1st color to 2nd color, equiv class size, coordinates, and color stats *)
              let l_ijcols = ref [] in
              Array.iteri
                (fun i' row ->
                  Array.iteri
                    (fun j' (n,n_def,cstats) ->
                      if n > 0 then (* non-empty equiv class *)
                        match cstats with
                        | [] ->
                           l_ijcols := (0.,i',j',n,n_def,cstats)::!l_ijcols
                        | [_,n1] ->
                           l_ijcols := (infinity,i',j',n,n_def,cstats)::!l_ijcols
                        | (_,n1)::(_,n2)::_ ->
                           let ratio = float n1 /. float n2 in
                           l_ijcols := (ratio,i',j',n,n_def,cstats)::!l_ijcols)
                    row)
                cols;
              (* sorting equiv classes from most imbalanced to least, then from larger to smaller *)
              let l_ijcols_sorted =
                List.sort (* TODO: is there a better sorting criteria? *)
                  (fun (ratio1,_,_,_,_,_) (ratio2,_,_,_,_,_) -> Stdlib.compare ratio2 ratio1)
                  !l_ijcols in
              (* defining the core, while checking disjunct colors between core and noise *)
              let g_core_opt =
                let g_core = Grid.make u v Grid.undefined in
                let ok, cols_core, cols_noise =
                  List.fold_left
                    (fun (ok,cols_core,cols_noise as res) (_,i',j',n,n_def,cstats) ->
                      if ok
                      then
                        let cstats_core, cstats =
                          List.partition (fun (c1,n1) -> Intset.mem c1 cols_core) cstats in
                        match cstats_core with
                        | [] ->
                           let cstats_noise, cstats =
                             List.partition (fun (c1,n1) -> Intset.mem c1 cols_noise) cstats in
                           (* choosing most frequent color out of noise colors *)
                           (match cstats with
                            | [] -> (* only noise colors: fail *)
                               false, cols_core, cols_noise
                            | (c1,n1)::cstats1 -> (* choosing most frequent color c *)
                               if n1 * 2 >= n_def (* if frequent enough *)
                               then (
                                 Grid.Do.set_pixel g_core i' j' c1;
                                 let cols_core = Intset.add c1 cols_core in
                                 let ok, cols_noise =
                                   List.fold_left
                                     (fun (ok,cols) (c1,n1) ->
                                       if c1 = Grid.transparent
                                       then false, cols
                                       else ok, Intset.add c1 cols)
                                     (ok,cols_noise) cstats1 in
                                 ok, cols_core, cols_noise)
                               else false, cols_core, cols_noise)
                        | [(c1,n1)] ->
                           if n1 * 2 >= n_def (* if frequent enough *)
                           then (
                             Grid.Do.set_pixel g_core i' j' c1;
                             let ok, cols_noise =
                               List.fold_left
                                 (fun (ok,cols) (c1,n1) ->
                                   if c1 = Grid.transparent
                                   then false, cols
                                   else ok, Intset.add c1 cols)
                                 (true,cols_noise) cstats in
                             ok, cols_core, cols_noise)
                           else false, cols_core, cols_noise
                        | _ -> (* two competing core colors: fail *)
                           false, cols_core, cols_noise                        
                      else res)
                    (true, Intset.empty, Intset.empty) l_ijcols_sorted in
                let ok = ok && Intset.cardinal cols_core > 1 in (* otherwise, not really a motif *)
                if ok
                then Some g_core
                else None in
              match g_core_opt with
              | Some g_core ->
                 (* g = pure(core) |> filtered by mask |> plus noise *)
                 (match make_grid h w mot g_core with
                  | Result.Error _ -> None
                  | Result.Ok g_without_noise ->
                     let g_mask_opt =
                       if g.color_count.(bgcolor) = 0
                       then None
                       else Some (Grid.Mask.from_grid_background bgcolor g) in
                     let g_noise =
                       Grid.map2_pixels
                         (fun c1 c2 ->
                           if c1 <> c2 && c1 <> bgcolor then c1 (* noise *)
                           else Grid.transparent)
                         g g_without_noise in
                     let area_core = Grid.color_area Grid.undefined g_core in
                     (* mask area not relevant *)
                     let area_noise = Grid.color_area Grid.transparent g_noise in
                     if 3 * area_noise < area (* noise less than a third of the contents *)
                     then Some (area_core + 3 * area_noise,
                                g_core,g_mask_opt,g_noise)
                     else None)
              | None -> None)
            cores in
        match list_best
                (fun (a1,_,_,_) (a2,_,_,_) -> a1 < a2)
                cores_ok with
        | None -> res
        | Some (area,g_core,g_mask_opt,g_noise) ->
           (area,mot,ru,rv,g_core,g_mask_opt,g_noise)::res)
      [] motifs in
  let res =
    List.sort
      (fun (a1,_,_,_,_,_,_) (a2,_,_,_,_,_,_) -> Stdlib.compare a1 a2)
      res in
  let res =
    List.map (fun (_,mot,ru,rv,core,mask_opt,noise) -> (mot,ru,rv,core,mask_opt,noise)) res in
  res)
let from_grid, reset_from_grid =
  Memo.memoize3 ~size:Grid.memoize_size from_grid

(*let _ = (* TEST *)
  let u, v = 2, 1 in
  let core =
    (* Grid.init u v (fun i' j' -> 3 * i' + j') in *)
    Grid.init 2 10
      (fun i j ->
        if j >= 0 && j < 3 && i+j mod 2 = 1 then Grid.red
        else if j >= 4 && j < 7 && i+j mod 2 = 1 then Grid.cyan
        else Grid.transparent) in
  let h, w, mot =
    let open Grid.Transf in
    (* 3*u, 6*v, Scale *)
    (* 3*u+1, 5*v+1, Periodic (I,J) *)
    (* 3*u+1, 5*v+1, Periodic (PlusIJ,DiffIJ) *)
    (* 2*u, v, FlipH *)
    (* u, 2*v, FlipW *)
    (* 2*u-1, 2*v, FlipHW *)
    (* 2*u, v, Rotate180 *)
    (* 2*u, 2*v, Rotate90 *)
    (* 2*u-1, 2*v-1, FullSym *)
    (* 7, 7, Diamond *)
    6, 10, Periodic (I,J)
  in
  match make_grid h w mot core with
  | Result.Ok g ->
     pp Grid.xp_grid g;
     (*Grid.Do.set_pixel g 1 0 3;
     Grid.Do.set_pixel g 1 5 3;
     pp Grid.xp_grid g;*)
     print_endline "MOTIFS";
     List.iter
       (fun (mot,ru,rv,core,noise) ->
         pp_endline xp mot;
         pp Grid.xp_grid core;
         pp Grid.xp_grid noise;
         print_newline ())
       (from_grid g)
  | Result.Error exn -> raise exn*)
  
  end


module Metagrid = (* grid of grids, separated by sepcolor frontiers *)
  struct

    type t =
      { sepcolor : Grid.color;
        borders : Grid.t; (* 2x2 mask: <<top,bot>,<left,right>> *)
        k : int;
        l : int;
        part_heights : int array;
        part_widths : int array;
        parts : Grid.t array array; (* grid matrix, row-wise *)
      }

    let is_well_formed (mg : t) : bool =
      let k, l = mg.k, mg.l in
      let ok = ref (k > 0 && l > 0) in
      ok := !ok && Grid.dims mg.borders = (2,2) && Grid.Mask.is_well_formed mg.borders;
      ok := !ok && k = Array.length mg.part_heights;
      ok := !ok && l = Array.length mg.part_widths;
      ok := !ok && k = Array.length mg.parts;
      let i = ref 0 in
      let j = ref 0 in
      while !ok && !i < k do
        let hi = mg.part_heights.(!i) in
        let row = mg.parts.(!i) in
        ok := !ok && l = Array.length row;
        while !ok && !j < l do
          let wj = mg.part_widths.(!j) in
          let g1 = row.(!j) in
          let h1, w1 = Grid.dims g1 in
          ok := !ok && h1 = hi && w1 = wj;
          incr j
        done;
        incr i;
        j := 0
      done;
      !ok

    let init_parts (k : int) (l : int) (f : int -> int -> Grid.t) : Grid.t array array =
      Array.init k
        (fun i ->
          Array.init l
            (fun j ->
              f i j))
    
    let generate (mg : t) : Grid.t result =
      let get_offsets start stop n part_lengths =
        let offset_g = Array.make n start in
        let offset_f = Array.make (start + n-1 + stop) start in
        let offset = ref (start + part_lengths.(0)) in
        if start = 1 then offset_f.(0) <- 0;
        for i = 1 to n-1 do
          offset_f.(start + i-1) <- !offset;
          incr offset;
          offset_g.(i) <- !offset;
          offset := !offset + part_lengths.(i)
        done;
        if stop = 1 then offset_f.(start + n-1) <- !offset;
        offset_g, offset_f
      in
      if is_well_formed mg
      then
        let k, l = mg.k, mg.l in
        let top, bot, left, right =
          let b = mg.borders.matrix in
          let offset c = if c = Grid.one then 1 else 0 in
          offset b.{0,0}, offset b.{0,1},
          offset b.{1,0}, offset b.{1,1} in
        let h = Array.fold_left (+) (top + k-1 + bot) mg.part_heights in (* k-1 frontiers *)
        let w = Array.fold_left (+) (left + l-1 + right) mg.part_widths in (* l-1 frontiers *)
        let offset_h_g, offset_h_f = get_offsets top bot k mg.part_heights in
        let offset_w_g, offset_w_f = get_offsets left right l mg.part_widths in
        (* initializing the grid *)
        let g = Grid.make h w Grid.transparent in
        (* drawing horizontal frontiers *)
        Array.iter
          (fun i ->
            for j = 0 to w-1 do
              Grid.Do.set_pixel g i j mg.sepcolor
            done)
          offset_h_f;
        (* drawing vertical frontiers *)
        Array.iter
          (fun j ->
            for i = 0 to h-1 do
              Grid.Do.set_pixel g i j mg.sepcolor
            done)
          offset_w_f;
        (* drawing part grids *)
        Array.iteri
          (fun i offset_i ->
            Array.iteri
              (fun j offset_j ->
                Grid.add_grid_at g offset_i offset_j mg.parts.(i).(j))
              offset_w_g)
          offset_h_g;
        Result.Ok g
      else Result.Error (Failure "Grid_patterns.Metagrid.generate: ill-formed metagrid")
    
    let parse (g : Grid.t) : t list =
      let h, w = Grid.dims g in
      (* looking for horizontal frontiers *)
      let color_h_fs = Array.make (Grid.nb_color + 1) [] in
      for i = h-1 downto 0 do
        let c = g.matrix.{i,0} in
        if c <> Grid.undefined then (
          let j = ref 1 in
          while !j < w && g.matrix.{i,!j} = c do
            incr j
          done;
          if !j >= w then color_h_fs.(c) <- i :: color_h_fs.(c)
        )
      done;
      (* looking for vertical frontiers *)
      let color_w_fs = Array.make (Grid.nb_color + 1) [] in
      for j = w-1 downto 0 do
        let c = g.matrix.{0,j} in
        if c <> Grid.undefined then (
          let i = ref 1 in
          while !i < h && g.matrix.{!i,j} = c do
            incr i
          done;
          if !i >= h then color_w_fs.(c) <- j :: color_w_fs.(c)
        )
      done;
      (* collecting metagrid candidates *)
      let c_fs = ref ([] : (Grid.color * int list * int list) list) in
      Array.iteri
        (fun c h_fs ->
          let w_fs = color_w_fs.(c) in
          match h_fs, w_fs with
          | [], [] -> ()
          | h_fs, w_fs -> c_fs := (c,h_fs,w_fs) :: !c_fs)
        color_h_fs;
      let mgs =
        let sizes_offsets_of_fs right fs =
          let rec aux left = function
            | [] ->
               if left = right (* border frontier *)
               then true, [], []
               else false, (right - left) :: [], left :: []
            | f::fs1 ->
               let stop, sizes1, offsets1 = aux (f+1) fs1 in
               stop, (f - left) :: sizes1, left :: offsets1
          in
          let start, left, fs =
            match fs with
            | 0::fs1 -> true, 1, fs1 (* border frontier *)
            | _ -> false, 0, fs in
          let stop, sizes, offsets = aux left fs in
          start, stop, Array.of_list sizes, Array.of_list offsets
        in
        List.filter_map
          (fun (c,h_fs,w_fs) ->
            let top, bot, part_heights, part_h_offsets = sizes_offsets_of_fs h h_fs in
            let left, right, part_widths, part_w_offsets = sizes_offsets_of_fs w w_fs in
            let k = Array.length part_heights in
            let l = Array.length part_widths in
            if k = 0 || l = 0
               || Array.exists (fun h -> h = 0) part_heights
               || Array.exists (fun w -> w = 0) part_widths
            then None
            else
              let borders =
                let col b = if b then Grid.one else Grid.zero [@@inline] in
                Grid.init 2 2
                  (fun i j ->
                    match i, j with
                    | 0, 0 -> col top
                    | 0, 1 -> col bot
                    | 1, 0 -> col left
                    | 1, 1 -> col right
                    | _ -> assert false) in
              let parts =
                init_parts k l
                  (fun i j ->
                    let g1_res =
                      Grid.Transf.crop g
                        part_h_offsets.(i) part_w_offsets.(j)
                        part_heights.(i) part_widths.(j) in
                    match g1_res with
                    | Result.Ok g1 -> g1
                    | Result.Error _ -> assert false) in
              Some
                { sepcolor = c;
                  borders;
                  k;
                  l;
                  part_heights;
                  part_widths;
                  parts })
          !c_fs in
      let mgs = (* sorting by increasing meta-area *)
        List.sort
          (fun mg1 mg2 -> Stdlib.compare (mg1.k * mg1.l) (mg2.k * mg2.l))
          mgs in
      mgs

(*    let _ = (* unit test *)
      print_endline "UNIT TEST Grid_patterns.Metagrid";
      let mg =
        let sepcolor = Grid.black in
        let part_heights = [|2;1|] in
        let part_widths = [|3;1;2|] in
        let top, bot, left, right = false, false, false, false in
        let borders =
          let col b = if b then Grid.Mask.one else Grid.Mask.zero [@@inline] in
          Grid.init 2 2
            (fun i j ->
              match i, j with
              | 0, 0 -> col top
              | 0, 1 -> col bot
              | 1, 0 -> col left
              | 1, 1 -> col right
              | _ -> assert false) in
        let k = Array.length part_heights in
        let l = Array.length part_widths in
        let parts =
          init_parts k l
            (fun i j ->
              let c = Grid.black + 1 + Random.int 4 in
              Grid.make part_heights.(i) part_widths.(j) c) in
        { sepcolor;
          borders;
          k;
          l;
          part_heights;
          part_widths;
          parts }in
      assert (is_well_formed mg);
      match generate mg with
      | Result.Error exn -> raise exn
      | Result.Ok g ->
         pp Grid.xp_grid g;
         parse g
         |> List.iter
              (fun mg1 ->
                match generate mg1 with
                | Result.Ok g1 ->
                   Printf.printf "Parsing with dims %d x %d" mg1.k mg1.l;
                   pp Grid.xp_grid g1
                | Result.Error exn -> raise exn) *)
    
  end

(* Reset of memoized functions *)
             
let reset_memoized_functions () =
  reset_subgrid_of_part ();
  Objects.reset_segment_connected ();
  Objects.reset_segment_by_color ();
  reset_partition_by_color ();
    (*  Motif.reset_make_grid ();*)
  Motif.reset_from_grid ()

