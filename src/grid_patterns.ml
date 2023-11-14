
open Arc_common
open Grid

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
          
let segment_gen
      (connected_pixels : Grid.pixel -> Grid.pixel -> bool)
      (g : Grid.t)
    : (int * int * Grid.t) list = (* position and subgrids of segments *)
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
  (* merging adjacent pixels with same color *)
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

(* MOTIFS *)
                                                                       
type motif =
  | Scale
  | Periodic of Grid.Transf.periodicity
  | FlipH | FlipW | FlipHW
  | FlipD1 | FlipD2 | FlipD12
  | Rotate180 | Rotate90
  | FullSym
(* TODO: add symmetry axis/center position *)

let xp_motif ~html print = function
  | Scale -> print#string "scale"
  | Periodic per -> Grid.Transf.xp_periodicity print per
  | FlipH -> print#string "flipH"
  | FlipW -> print#string "flipW"
  | FlipHW -> print#string "flipHW"
  | FlipD1 -> print#string "FlipD1"
  | FlipD2 -> print#string "FlipD2"
  | FlipD12 -> print#string "FlipD12"
  | Rotate180 -> print#string "Rotate180"
  | Rotate90 -> print#string "Rotate90"
  | FullSym -> print#string "FullSym"

(* Reset of memoized functions *)
             
let reset_memoized_functions () =
  reset_segment ();
  reset_segment_same_color ();
  reset_segment_same_row_and_color ();
  reset_segment_same_column_and_color ()

