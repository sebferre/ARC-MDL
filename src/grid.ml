
open Bigarray

module Intset = Intset.Intmap
       
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

let nb_color = 10

let name_of_color : color -> string =
  function
  | 0 -> "black"
  | 1 -> "blue"
  | 2 -> "red"
  | 3 -> "green"
  | 4 -> "yellow"
  | 5 -> "grey"
  | 6 -> "pink"
  | 7 -> "orange"
  | 8 -> "cyan"
  | 9 -> "brown"
  | c -> "col" ^ string_of_int c


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

let iter_pixels f grid = Common.prof "Grid.iter_pixels" (fun () ->
  let mat = grid.matrix in
  for i = 0 to grid.height - 1 do
    for j = 0 to grid.width - 1 do
      f i j mat.{i,j}
    done
  done)

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
    | 0 -> [white; on_black], "0 "
    | 1 -> [white; on_blue], "1 "
    | 2 -> [white; on_red], "2 "
    | 3 -> [white; on_green], "3 "
    | 4 -> [white; on_yellow], "4 "
    | 5 -> [black; on_white], "5#"
    | 6 -> [white; on_magenta], "6 "
    | 7 -> [red; on_yellow], "7#"
    | 8 -> [white; on_cyan], "8 "
    | 9 -> [green; on_red], "9#"
    | 10 -> [on_white], "  "
    | _ -> invalid_arg "Invalid color code" in
  print_string style str


(* comparing grids *)
    
type diff =
  | Grid_size_mismatch of { src_height: int; src_width: int;
			    tgt_height: int; tgt_width: int }
  | Grid_diff_pixels of { height: int; width: int; pixels: pixel list }
							  
let diff (source : t) (target : t) : diff option = Common.prof "Grid.diff" (fun () ->
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
    else Some (Grid_diff_pixels {height; width; pixels=(!res)}))

(* grid masks *)

module Intrel2 = Intrel2.Intmap

module Mask = (* to identify active pixels in a same-size grid *)
  struct
    include Intrel2

    (*let full height width = Common.prof "Grid.Mask.full" (fun () ->
      let res = ref Intrel2.empty in
      for i = 0 to height - 1 do
	for j = 0 to width - 1 do
	  res := Intrel2.add i j !res
	done
      done;
      !res)*)

    let rect mini maxi minj maxj = Common.prof "Grid.Mask.rect" (fun () ->
      let ref_dom = ref Intset.empty in
      for i = mini to maxi do
	ref_dom := Intset.add i !ref_dom
      done;
      let ref_ran = ref Intset.empty in
      for j = minj to maxj do
	ref_ran := Intset.add j !ref_ran
      done;
      Intrel2.prod !ref_dom !ref_ran)

    let full height width =
      rect 0 (height-1) 0 (width-1)
	
  end

(* segmenting grids *)

type part = { mini : int; maxi : int;
	      minj : int; maxj : int;
	      color : color;
	      nb_pixels : int;
	      pixels : Mask.t }

let part_as_grid (g : t) (p : part) : t = Common.prof "Grid.part_as_grid" (fun () ->
  let gp = make g.height g.width no_color in
  let col = p.color in
  Intrel2.iter
    (fun i j -> set_pixel gp i j col)
    p.pixels;
  gp)

let merge_parts (p1 : part) (p2 : part) : part = Common.prof "Grid.merge_parts" (fun () ->
  assert (p1.color = p2.color);
  { mini = min p1.mini p2.mini;
    maxi = max p1.maxi p2.maxi;
    minj = min p1.minj p2.minj;
    maxj = max p1.maxj p2.maxj;
    color = p1.color;
    nb_pixels = p1.nb_pixels + p2.nb_pixels;
    pixels = Mask.union p1.pixels p2.pixels })

let merge_part_list (ps : part list) : part =
  match ps with
  | [] -> invalid_arg "Grid.merge_part_list: empty list"
  | p1::ps1 -> List.fold_left merge_parts p1 ps1
    
let pp_parts (g : t) (ps : part list) : unit =
  List.iter
    (fun p -> Printf.printf "(%d,%d)->(%d,%d) [%d/%d] "
			    p.mini p.minj
			    p.maxi p.maxj
			    p.nb_pixels
			    ((p.maxi-p.mini+1) * (p.maxj-p.minj+1)))
    ps;
  print_newline ();
  pp_grids (g :: List.map (part_as_grid g) ps)
	      
let segment_by_color (g : t) : part list = Common.prof "Grid.segment_by_color" (fun () ->
  let h, w = g.height, g.width in
  let fm : (int * int, part) Find_merge.hashtbl =
    new Find_merge.hashtbl
	~init_val:
	{ mini=h-1; maxi=0;
	  minj=w-1; maxj=0;
	  color=0; nb_pixels=0; pixels=Mask.empty }
	~merge_val:merge_parts
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
		   nb_pixels=1;
		   pixels=Mask.singleton i j }
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
    [])


(* locating shapes *)

type rectangle = { height: int; width: int;
		   offset_i: int; offset_j: int;
		   color: color;
		   mask : Mask.t; (* all pixels covered by the shape, including the delta *)
		   delta : pixel list }

let rectangle_opt_of_part (g : t) (mask : Mask.t) (p : part) : rectangle option = Common.prof "Grid.rectangle_opt_of_part" (fun () ->
  let h, w = p.maxi-p.mini+1, p.maxj-p.minj+1 in
  let area = h * w in
(*  let rect = Mask.rect p.mini p.maxi p.minj p.maxj in
  let r_mask = Intrel2.inter rect mask in
  let valid_area =
    Intrel2.cardinal (Intrel2.inter_r [r_mask; p.pixels])
    + Intrel2.cardinal (Intrel2.diff rect mask) in
  let delta_mask = Intrel2.diff r_mask p.pixels in
  let delta =
    Intrel2.fold
      (fun res i j -> (i, j, g.matrix.{i,j})::res)
      [] delta_mask in *)  
  let valid_area = ref 0 in
  let r_mask = ref p.pixels in
  let delta = ref [] in
  for i = p.mini to p.maxi do
    for j = p.minj to p.maxj do
      if Mask.mem i j mask
      then (
	r_mask := Mask.add i j !r_mask;
	let c = g.matrix.{i,j} in
	if  c = p.color
	then incr valid_area
	else delta := (i,j,c)::!delta )
      else incr valid_area (* out-of-mask pixels are hidden behind another object *)
    done
  done;
  if !valid_area >= 1 * area / 2
  then Some { height = p.maxi-p.mini+1;
	      width = p.maxj-p.minj+1;
	      offset_i = p.mini;
	      offset_j = p.minj;
	      color = p.color;
	      mask = (!r_mask);
	      delta = (!delta) }
  else None)
      
let rectangles (g : t) (mask : Mask.t) (parts : part list) : rectangle list = Common.prof "Grid.rectangles" (fun () ->
  let h_sets =
    (* grouping same-color parts spanning same rows *)
    Common.group_by
      (fun part -> part.mini, part.maxi, part.color)
      parts in
  let v_sets =
    (* grouping same-color parts spanning same cols *)
    Common.group_by
      (fun part -> part.minj, part.maxj, part.color)
      parts in
  let res = [] in
  let res =
    List.fold_left
      (fun res (_, ps) ->
       assert (ps <> []);
       let mp = merge_part_list ps in
       match rectangle_opt_of_part g mask mp with
       | Some r -> r::res
       | None -> res)
      res h_sets in
  let res =
    List.fold_left
      (fun res (_, ps) ->
       match ps with
       | _::_::_ -> (* at least two parts to avoid redundancy with h_sets *)
	  let mp = merge_part_list ps in
	  ( match rectangle_opt_of_part g mask mp with
	    | Some r -> r::res
	    | None -> res )
       | _ -> res)
      res v_sets in
  res)

