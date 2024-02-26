
open Madil_common
open Arc_common
open Grid

module Intset = Intset.Intmap


(* Coloring *)

let recoloring (g : Grid.t) : (Grid.t * Grid.color array) result =
  (* normalize grid coloring, and return a color palette to give original coloring *)
  (* transparent and undefined are left transparent *)
  Common.prof "Grid_patterns.recoloring" (fun () ->
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
  Result.Ok (gres, palette))

let recolor (g : Grid.t) (palette : Grid.color array) : Grid.t =
  let nc = Array.length palette in
  Grid.map_pixels
    (fun c ->
      if c >= 0 && c < nc
      then palette.(c)
      else c)
    g

let parse_recoloring (g : Grid.t) (g1 : Grid.t) : (Grid.color,Grid.color) Mymap.t option =
  (* is g a recoloring of g1, and how? only for true colors *)
  Common.prof "Grid_patterns.parse_recoloring" (fun () ->
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
  else None) (* grid size mismatch *)
  

(* crop *)

let parse_crop (g : Grid.t) (g1 : Grid.t) : (int * int) list =
  (* empty result if more than 3 occs *)
  Common.prof "Grid_pattern.parse_crop" (fun () ->
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
  else [])
  

(* Repeat (a la NumPy) *)

let parse_repeat (g : Grid.t) : (Grid.t * int list * int list) option =
  (* returns: the compressed grid, the numbers of repeats on axis i, and the numbers of repeats on axis j *)
  let rec cumsum offset = function
    | [] -> []
    | n::l -> offset :: cumsum (offset+n) l
  in
  Common.prof "Grid_patterns.parse_repeat" (fun () ->
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
  else None)

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

let subgrid_of_part (g : Grid.t) (p : part) : Grid.t =
  Common.prof "Objects.subgrid_of_part" (fun () ->
  let h1, w1 = p.maxi - p.mini + 1, p.maxj - p.minj + 1 in
  let g1 = Grid.make h1 w1 Grid.transparent in
  Bitmap.iter
    (fun i j ->
      Grid.Do.set_pixel g1 (i - p.mini) (j - p.minj) g.matrix.{i,j})
    p.pixels;
  g1)
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
  | Connected
  | OneColor
  | ConnectedOneColor

let candidate_segmentations =
  [ ConnectedOneColor; Connected; OneColor ]
let nb_candidate_segmentations =
  List.length candidate_segmentations

let xp_segmentation ~html print = function
  | Connected -> print#string "connected"
  | OneColor -> print#string "one-color"
  | ConnectedOneColor -> print#string "one-color connected"

type obj = int * int * Grid.t (* object *)
type t = obj list

let segment_gen
      (connected_pixels : Grid.pixel -> Grid.pixel -> bool)
      (g : Grid.t)
    : t = (* position and subgrids of segments *)
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
      let pix = (i,j,c) in
      if c <> Grid.transparent then (
        (* pixel on the right *)
        let j_right = j+1 in
        if j_right < w then (
          let c_right = mat.{i,j_right} in
          if c_right <> Grid.transparent && connected_pixels pix (i,j_right,c_right) then
	    ignore (fm#merge [(i,j); (i,j_right)]));
        (* pixel down *)
        let i_down = i+1 in
        if i_down < h then (
          let c_down = mat.{i_down,j} in
          if c_down <> Grid.transparent && connected_pixels pix (i_down,j,c_down) then
	    ignore (fm#merge [(i,j); (i_down,j)]));
        (* pixel right and down, diagonally *)
        let i_diag1 = i+1 in
        let j_diag1 = j+1 in
        if i_diag1 < h && j_diag1 < w then (
          let c_diag1 = mat.{i_diag1,j_diag1} in
          if c_diag1 <> Grid.transparent && connected_pixels pix (i_diag1,j_diag1,c_diag1) then
	    ignore (fm#merge [(i,j); (i_diag1,j_diag1)]));
        (* pixel left and down, diagonally *)
        let i_diag2 = i+1 in
        let j_diag2 = j-1 in
        if i_diag2 < h && j_diag2 >= 0 then (
          let c_diag2 = mat.{i_diag2,j_diag2} in
          if c_diag2 <> Grid.transparent && connected_pixels pix (i_diag2,j_diag2,c_diag2) then
	    ignore (fm#merge [(i,j); (i_diag2,j_diag2)])))
    done
  done;
  (* collecting parts *)
  let parts =
    fm#fold
      (fun _ part res -> (* TODO: find a way to avoid this trick *)
        let gpart = subgrid_of_part g part in
        let garea = Grid.color_area Grid.transparent gpart in
        (part.mini, part.minj, gpart, garea) :: res)
      [] in
  let sorted_parts =
    List.sort
      (fun (i1,j1,g1,a1) (i2,j2,g2,a2) ->
        Stdlib.compare (a2,i1,j1) (a1,i2,j2)) (* decreasing area first, then increasing i, j *)
      parts in
  List.map (fun (i,j,g,_) -> (i,j,g)) sorted_parts)

let segment = segment_gen (fun _ _ -> true)
let segment, reset_segment = Memo.memoize ~size:103 segment
                    
let segment_same_color = segment_gen (fun (i1,j1,c1) (i2,j2,c2) -> c1 = c2)
let segment_same_color, reset_segment_same_color = Memo.memoize ~size:103 segment_same_color
                    
let segment_same_row_and_color = segment_gen (fun (i1,j1,c1) (i2,j2,c2) -> i1 = i2 && c1 = c2)
let segment_same_row_and_color, reset_segment_same_row_and_color = Memo.memoize ~size:103 segment_same_row_and_color
                    
let segment_same_column_and_color = segment_gen (fun (i1,j1,c1) (i2,j2,c2) -> j1 = j2 && c1 = c2)
let segment_same_column_and_color, reset_segment_same_column_and_color = Memo.memoize ~size:103 segment_same_column_and_color

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

let partition_by_color (g : Grid.t) : t = (* position and subgrids *)
  Common.prof "Grid_patterns.partition_by_color" (fun () ->
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
  List.map (fun (_,i,j,g) -> (i,j,g)) sorted_parts)

let partition_by_color, reset_partition_by_color = Memo.memoize ~size:103 partition_by_color

let parse (g : Grid.t) : (segmentation * t) Myseq.t =
  let* seg = Myseq.from_list candidate_segmentations in
  let objs =
    match seg with
    | Connected -> segment g
    | OneColor -> partition_by_color g
    | ConnectedOneColor -> segment_same_color g in
  Myseq.return (seg, objs)

  end

(* MOTIFS *)

module Motif =
  struct
    
type t =
  | Scale
  | Periodic of Grid.Transf.axis * Grid.Transf.axis
  (* symmetries *) (* TODO: add symmetry axis/center position *)
  | FlipH | FlipW | FlipHW
  | FlipD1 | FlipD2 | FlipD12
  | Rotate180 | Rotate90
  | FullSym
  (* special motifs *)
  | Border | CrossPlus | CrossTimes | Diamond
  | Star (* CrossPlus+CrossTimes *) (* TODO: other combinations? *)

let xp ~html print = function
  | Scale -> print#string "scale"
  | Periodic (phi,psi) ->
     print#string "periodic["; Grid.Transf.xp_axis print phi;
     print#string ","; Grid.Transf.xp_axis print psi;
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
  | Border -> print#string "Border"
  | CrossPlus -> print#string "Cross +"
  | CrossTimes -> print#string "Cross x"
  | Diamond -> print#string "Diamond"
  | Star -> print#string "Star"

let project (mot : t) h w u v : (int -> int -> int * int) =
  (* project coord (i,j) in (h,w) range to (u,v) range, according to motif *)
  let h_1, w_1 = h-1, w-1 in
  match mot with
  | Scale ->
     let k, l = h_1 / u + 1, w_1 / v + 1 in
     (fun i j -> i / k, j / l)
  | Periodic (phi,psi) ->
     let eval_phi = Grid.Transf.eval_axis phi in
     let eval_psi = Grid.Transf.eval_axis psi in
     (fun i j ->
       let a, b = eval_phi i j, eval_psi i j in
       a mod u, b mod v)
  | FlipH ->
     (fun i j -> min i (h_1 - i), j)
  | FlipW ->
     (fun i j -> i, min j (w_1 - j))
  | FlipHW ->
     (fun i j -> min i (h_1 - i), min j (w_1 - j))
  | FlipD1 ->
     (fun i j ->
       let p, m = i + j, i + (w_1 - j) in
       min p (h_1 + w_1 - p), m)
  | FlipD2 ->
     (fun i j ->
       let p, m = i + j, i + (w_1 - j) in
       p, min m (h_1 + w_1 - m))
  | FlipD12 ->
     (fun i j ->
       let p, m = i + j, i + (w_1 - j) in
       min p (h_1 + w_1 - p), min m (h_1 + w_1 - m))
  | Rotate180 ->
     (fun i j -> min (i, j) (h_1 - i, w_1 - j))
  | Rotate90 ->
     (fun i j ->
       let a, b =
         min (i, j)
           (min (w_1 - j, i)
              (min (h_1 - i, w_1 - j)
                 (j, h_1 - i))) in
       if b >= v
       then w_1 - b, a 
       else a, b)
  | FullSym ->
     (fun i j ->
       let i_min = min i (h_1 - i) in
       let j_min = min j (w_1 - j) in
       min i_min j_min, max i_min j_min)
  (* (u,v) = (2,1), shape color at [1,0], bgcolor at [0,0] *)
  | Border ->
     (fun i j ->
       if i = 0 || j = 0 || i = h_1 || j = w_1
       then 1, 0
       else 0, 0)
  | CrossPlus ->
     (fun i j ->
       if i = h/2 || i = h_1/2 || j = w/2 || j = w_1/2
       then 1, 0
       else 0, 0)
  | CrossTimes ->
     (fun i j ->
       if i = j || i = (w_1-j)
       then 1, 0
       else 0, 0)
  | Star ->
     (fun i j ->
       if i = h/2 || i = h_1/2 || j = w/2 || j = w_1/2 (* CrossPlus *)
          || i = j || i = (w_1-j) (* CrossTimes *)
       then 1, 0
       else 0, 0)
  | Diamond ->
     assert (h = w);
     (fun i j ->
       let p, m = i + j,  i + (w_1 - j) in
       if p = h_1/2 || p = h_1 + h/2 || m = h_1/2 || m = h_1 + h/2
       then 1, 0
       else 0, 0)
             
let all_coredims_of_motif (mot : t) (h : int) (w : int) : Range.t * Range.t * (int * int) list =
  (* range and list of core dimensions (u,v) given a motif and grid dims *)
  match mot with
  | Scale ->
     Range.make_closed 1 h,
     Range.make_closed 1 w,
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
     Range.make_closed 1 h',
     Range.make_closed 1 w',
     Common.fold_for
       (fun u res ->
         Common.fold_for
           (fun v res ->
             if (u = h' && v = w' && psi <> Zero) (* not a proper periodic *)
             then res
             else (u,v)::res)
           1 w' res)
       1 h' []
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
     if h = w
     then
       let hw' = h+w-1 in (* projected dim, diagonal size *)
       let u, v = (hw'+1)/2, hw' in (* only half is used *)
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []
  | FlipD2 ->
     if h = w
     then
       let hw' = h+w-1 in
       let u, v = hw', (hw'+1)/2 in
       Range.make_exact u,
       Range.make_exact v,
       [u, v]
     else
       Range.make_open 0, (* dummy *)
       Range.make_open 0, (* dummy *)
       []
  | FlipD12 ->
     if h = w
     then
       let hw' = h+w-1 in
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
  | Border | CrossPlus ->
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
  
let make_grid (h : int) (w : int) (mot : t) (core : Grid.t) : Grid.t result =
  Common.prof "Grid_patterns.Motif.make_grid" (fun () ->
  let u, v = Grid.dims core in
  let ru, rv, luv = all_coredims_of_motif mot h w in
  if List.mem (u,v) luv
  then
    let proj = project mot h w u v in
    let g =
      Grid.init h w
        (fun i j ->
          let i', j' = proj i j in
          assert (i' >= 0 && i' < u && j' >= 0 && j' < v);
            (* pp xp mot; Printf.printf " (%d,%d) -> (%d,%d) [%d,%d]\n" h w u v i' j';
            assert false); *)
          core.Grid.matrix.{i',j'}) in
    Result.Ok g
  else Result.Error (Failure "Grid_patterns.make_grid: incompatible motif and core grid with grid size"))
(*let make_grid, reset_make_grid = (* TODO: there is a confusing bug, grids get mixed *)
  Memo.memoize4 ~size:Grid.memoize_size make_grid*)

(* discovering motifs in grids *)
  
let candidates =
  let open Grid.Transf in
  [ Scale;
    FlipH; FlipW; FlipHW;
    FlipD1; FlipD2; FlipD12;
    Rotate180; Rotate90;
    FullSym;
    Border; CrossPlus; CrossTimes; Diamond; Star;
    Periodic (I, J);
    Periodic (I, PlusIJ);
    Periodic (PlusIJ, J);
    Periodic (PlusIJ, DiffIJ);
    Periodic (I, Zero);
    Periodic (J, Zero);
    Periodic (PlusIJ, Zero);
    Periodic (DiffIJ, Zero);
    Periodic (MaxIJ, Zero);
    Periodic (MinIJ, Zero) ]
let nb_candidates = List.length candidates

let from_grid (g : Grid.t) : (t * Range.t * Range.t * Grid.t * Grid.t) list = (* list of (motif, range_u, range_v, (u,v)-sized core, noise) that [g] agreeds to *)
  Common.prof "Grid_patterns.from_grid" (fun () ->
  let h, w = Grid.dims g in
  (* color stats: lists of (color,count) pairs *)
  let rec add_color c cstats =
    match cstats with
    | [] -> [(c,1)]
    | (c0,n0)::cstats1 ->
       if c = c0 then (c0,n0+1)::cstats1
       else
         match add_color c cstats1 with
         | (c1,n1)::cstats2 when n1 > n0 ->
            (c1,n1)::(c0,n0)::cstats2
         | cstats1 -> (c0,n0)::cstats1
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
              let cols = Array.make_matrix u v [] in 
              (u,v,proj,cols)::res2)
            [] luv in
        (mot,ru,rv,cores)::res)
      [] candidates in
  (* color stats through one pass of the grid pixels *)
  List.iter (* much more efficient to iterate on motifs first, then on pixels *)
    (fun (mot,ru,rv,cores) ->
      List.iter
        (fun (u,v,proj,cols) ->
          Grid.iter_pixels
            (fun i j c ->
              let i', j' = proj i j in
              assert (i' >= 0 && i' < u && j' >= 0 && j' < v);
              (*if not (i' >= 0 && i' < u && j' >= 0 && j' < v) then (
                pp xp mot; Printf.printf " (%d,%d) [%d,%d]\n" u v i' j';
                assert false);*)
              if c <> Grid.undefined then
                cols.(i').(j') <- add_color c cols.(i').(j'))              
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
                    (fun j' cstats ->
                      match cstats with
                      | [] -> () (* empty equiv class *)
                      | [_,n] ->
                         l_ijcols := (infinity,i',j',cstats)::!l_ijcols
                      | (_,n1)::(_,n2)::_ ->
                         let ratio = float n1 /. float n2 in
                         l_ijcols := (ratio,i',j',cstats)::!l_ijcols)
                    row)
                cols;
              (* sorting equiv classes from most imbalanced to least, then from larger to smaller *)
              let l_ijcols_sorted =
                List.sort (* TODO: is there a better sorting criteria? *)
                  (fun (ratio1,_,_,_) (ratio2,_,_,_) -> Stdlib.compare ratio2 ratio1)
                  !l_ijcols in
              (* defining the core, while checking disjunct colors between core and noise *)
              let g_core_opt =
                let g_core = Grid.make u v Grid.undefined in
                let ok, cols_core, cols_noise =
                  List.fold_left
                    (fun (ok,cols_core,cols_noise as res) (_,i',j',cstats) ->
                      if ok
                      then
                        let n = List.fold_left (fun res (c1,n1) -> res + n1) 0 cstats in
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
                               if c1 <> Grid.undefined
                                  && n1 * 2 >= n (* if frequent enough *)
                               then (
                                 Grid.Do.set_pixel g_core i' j' c1;
                                 let cols_core = Intset.add c1 cols_core in
                                 let ok, cols_noise =
                                   List.fold_left
                                     (fun (ok,cols) (c1,n1) ->
                                       if c1 = Grid.undefined || c1 = Grid.transparent
                                       then false, cols
                                       else ok, Intset.add c1 cols)
                                     (ok,cols_noise) cstats1 in
                                 ok, cols_core, cols_noise)
                               else false, cols_core, cols_noise)
                        | [(c1,n1)] ->
                           if n1 * 2 >= n (* if frequent enough *)
                           then (
                             Grid.Do.set_pixel g_core i' j' c1;
                             let ok, cols_noise =
                               List.fold_left
                                 (fun (ok,cols) (c1,n1) ->
                                   if c1 = Grid.undefined || c1 = Grid.transparent
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
                 let g_without_noise =
                   match make_grid h w mot g_core with
                   | Result.Ok g -> g
                   | Result.Error _ -> assert false in
                 let g_noise =
                   Grid.map2_pixels
                     (fun c1 c2 ->
                       if c1 = c2 then Grid.transparent
                       else c1)
                     g g_without_noise in
                 let area_core = Grid.color_area Grid.undefined g_core in
                 let area_noise = Grid.color_area Grid.transparent g_noise in
                 Some (area_core+area_noise,g_core,g_noise)
              | None -> None)
            cores in
        match list_best
                (fun (a1,_,_) (a2,_,_) -> a1 < a2)
                cores_ok with
        | None -> res
        | Some (area,g_core,g_noise) ->
           (area,mot,ru,rv,g_core,g_noise)::res)
      [] motifs in
  let res =
    List.sort
      (fun (a1,_,_,_,_,_) (a2,_,_,_,_,_) -> Stdlib.compare a1 a2)
      res in
  let res =
    List.map (fun (_,mot,ru,rv,core,noise) -> (mot,ru,rv,core,noise)) res in
  res)
let from_grid, reset_from_grid =
  Memo.memoize ~size:Grid.memoize_size from_grid

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
             
(* Reset of memoized functions *)
             
let reset_memoized_functions () =
  reset_subgrid_of_part ();
  Objects.reset_segment ();
  Objects.reset_segment_same_color ();
  Objects.reset_segment_same_row_and_color ();
  Objects.reset_segment_same_column_and_color ();
  Objects.reset_partition_by_color ();
    (*  Motif.reset_make_grid ();*)
  Motif.reset_from_grid ()

