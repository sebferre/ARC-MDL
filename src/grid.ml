
open Bigarray

type color = int

(* conventional colors like in web app *)
let black = 0
let blue = 1
let red = 2
let green = 3
let yellow = 4
let grey = 5
let pink = 6
let orange = 7
let cyan = 8
let brown = 9	       

let no_color = 10
	       
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

(* pretty-printing in terminal *)
    
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
    | 10 -> [on_white], "  "
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

(* grid masks *)

type mask = bool array array (* to identify active pixels in a same-size grid *)

let make_mask height width b =
  Array.make_matrix height width b

(* segmenting grids *)

type part = { mini : int; maxi : int;
	      minj : int; maxj : int;
	      color : color;
	      pixels : (int * int) list }

let part_as_grid (g : t) (p : part) : t =
  let gp = make g.height g.width no_color in
  let col = p.color in
  List.iter
    (fun (i,j) -> set_pixel gp i j col)
    p.pixels;
  gp

let pp_parts (g : t) (ps : part list) : unit =
  List.iter
    (fun p -> Printf.printf "(%d,%d)->(%d,%d) [%d] "
			    p.mini p.minj
			    p.maxi p.maxj
			    (List.length p.pixels))
    ps;
  print_newline ();
  pp_grids (g :: List.map (part_as_grid g) ps)
	      
let segment_by_color (g : t) : part list =
  let h, w = g.height, g.width in
  let fm : (int * int, part) Find_merge.hashtbl =
    new Find_merge.hashtbl
	~init_val:
	{ mini=h-1; maxi=0;
	  minj=w-1; maxj=0;
	  color=0; pixels=[] }
	~merge_val:
	(fun p1 p2 ->
	 assert (p1.color = p2.color);
	 { mini = min p1.mini p2.mini;
	   maxi = max p1.maxi p2.maxi;
	   minj = min p1.minj p2.minj;
	   maxj = max p1.maxj p2.maxj;
	   color = p1.color;
	   pixels = p1.pixels @ p2.pixels })
  in
  let mat = g.matrix in
  (* setting initial val of each pixel *)
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let coord = (i,j) in
      fm#replace coord
		 { mini=i; maxi=i;
		   minj=j; maxj=j;
		   color=mat.{i,j};
		   pixels=[coord] }
    done
  done;
  (* merging adjacent pixels with same color *)
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      (* pixel on the right *)
      if j+1 < w && mat.{i,j} = mat.{i,j+1} then
	ignore (fm#merge [(i,j); (i,j+1)]);
      (* pixel below *)
      if i+1 < h && mat.{i,j} = mat.{i+1,j} then
	ignore (fm#merge [(i,j); (i+1,j)]);
      (* pixel right and down, diagonally *)
      if i+1 < h && j+1 < w && mat.{i,j} = mat.{i+1,j+1} then
	ignore (fm#merge [(i,j); (i+1,j+1)])
    done
  done;
  (* collecting parts *)
  fm#fold
    (fun _ part res -> part::res)
    []
