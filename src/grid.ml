
open Bigarray

type color = int

type matrix = (int, int8_unsigned_elt, c_layout) Array2.t
	       
type t = { height : int; (* equals Array2.dim1 matrix *)
	   width : int;  (* equals Array2.dim2 matrix *)
	   matrix : matrix }
type pixel = int * int * color (* x, y, col *)

let make height width col =
  let matrix = Array2.create Int8_unsigned C_layout height width in
  Array2.fill matrix col;
  { height; width; matrix }
			   
let set_pixel grid i j c =
  try grid.matrix.{i,j} <- c
  with _ -> () (* pixels out of bound are ignored *)

let iter_pixels f grid =
  let mat = grid.matrix in
  for i = 0 to grid.height - 1 do
    for j = 0 to grid.width - 1 do
      f i j mat.{i,j}
    done
  done

		     
let rec pp_grids grids =
  let max_height =
    List.fold_left (fun res g -> max res g.height) 0 grids in
  for i = 0 to max_height - 1 do
    List.iter
      (fun g ->
       for j = 0 to g.width - 1 do
	 if i < g.height
	 then pp_color g.matrix.{i,j}
	 else pp_blank ()
       done;
       print_string "   ")
      grids;
    print_newline ()
  done
and pp_grid grid =
  for i = 0 to grid.height - 1 do
    for j = 0 to grid.width - 1 do
      pp_color grid.matrix.{i,j}
    done;
    print_newline ()
  done
and pp_blank () =
  print_string "  "
and pp_color c =
  let open ANSITerminal in
  let style, str =
    match c with
    | 0 -> [on_black], "  "
    | 1 -> [on_blue], "  "
    | 2 -> [on_red], "  "
    | 3 -> [on_green], "  "
    | 4 -> [on_yellow], "  "
    | 5 -> [black; on_white], "##"
    | 6 -> [on_magenta], "  "
    | 7 -> [red; on_yellow], "##"
    | 8 -> [on_cyan], "  "
    | 9 -> [green; on_red], "##"
    | _ -> invalid_arg "Invalid color code" in
  print_string style str


(* comparing grids *)
    
type diff =
  | Grid_size_mismatch of { src_height: int; src_width: int;
			    tgt_height: int; tgt_width: int }
  | Grid_diff_pixels of { height: int; width: int; pixels: pixel list }
							  
let diff (source : t) (target : t) : diff option =
  if source.height <> target.height || source.width <> target.width
  then Some (Grid_size_mismatch
	       { src_height = source.height;
		 src_width = source.width;
		 tgt_height = target.height;
		 tgt_width = target.width })
  else
    let height = source.height in
    let width = source.width in
    let res = ref [] in
    for i = 0 to height - 1 do
      for j = 0 to width - 1 do
	let src_c, tgt_c = source.matrix.{i,j}, target.matrix.{i,j} in
	if src_c <> tgt_c then
	  res := (i,j,tgt_c) :: !res
      done
    done;
    if !res = []
    then None
    else Some (Grid_diff_pixels {height; width; pixels=(!res)})

type mask = bool array array (* to identify active pixels in a same-size grid *)

let make_mask height width b =
  Array.make_matrix height width b
