
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

let nb_color = 10

let all_colors = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
             
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
	   matrix : matrix; (* [i,j] -> col *)
           color_count : int Array.t; (* col (0..10) -> nb. cells with that color *)
         }
type pixel = int * int * color (* x, y, col *)

let max_size = 30
           
let make height width col =
  let matrix = Array2.create Int8_unsigned C_layout height width in
  let color_count = Array.make (nb_color+1) 0 in 
  Array2.fill matrix col;
  color_count.(col) <- height * width;
  { height; width; matrix; color_count }
			   
let set_pixel grid i j c =
  try
    let c0 = grid.matrix.{i,j} in
    if c <> c0 then (
      grid.matrix.{i,j} <- c;
      (* maintaining color count *)
      grid.color_count.(c0) <- grid.color_count.(c0) - 1;
      grid.color_count.(c) <- grid.color_count.(c) + 1
    )
  with _ -> () (* pixels out of bound are ignored *)

let iter_pixels f grid = Common.prof "Grid.iter_pixels" (fun () ->
  let mat = grid.matrix in
  for i = 0 to grid.height - 1 do
    for j = 0 to grid.width - 1 do
      f i j mat.{i,j}
    done
  done)

let color_partition ~(colors : color list) (grid : t) : int list =
  List.map
    (fun c -> grid.color_count.(c))
    colors

let crop ~(pos : int * int) ~(size : int * int) (grid : t) : t =
  let mini, minj = pos in
  let height, width = size in
  let matrix = Array2.create Int8_unsigned C_layout height width in
  let color_count = Array.make (nb_color+1) 0 in 
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let c = grid.matrix.{mini + i, minj + j} in
      matrix.{i,j} <- c;
      color_count.(c) <- color_count.(c) + 1
    done
  done;
  { height; width; matrix; color_count }

(* pretty-printing in terminal *)

let rec pp_grids grids =
  let grids_per_line = 5 in
  let nb_lines = (List.length grids - 1) / 5 + 1 in
  let max_height =
    List.fold_left (fun res g -> max res g.height) 0 grids in
  for k = 0 to nb_lines - 1 do
    for i = 0 to max_height - 1 do
      List.iteri
	(fun l g ->
	 if l / grids_per_line = k then (
	   for j = 0 to g.width - 1 do
	     if i < g.height
	     then pp_color g.matrix.{i,j}
	     else pp_blank ()
	   done;
	   print_string "   "
	 ))
	grids;
      print_newline ()
    done;
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

module Mask = (* based on Z arithmetics, as compact bitsets *)
  struct
    type t = { height : int;
	       width : int;
	       bits : Z.t; }

    let height m = m.height
    let width m = m.width
    let area m = Z.hamdist Z.zero m.bits

    let same_size m1 m2 =
      m1.height = m2.height && m1.width = m2.width
               
    let to_string m = (*Z.format "%b" m.bits*)
      let bytes = Bytes.create (m.height * m.width + m.height - 1) in
      let pos = ref 0 in
      for i = 0 to m.height - 1 do
        if i > 0 then (
          Bytes.set bytes !pos '|';
          incr pos
        );
        for j = 0 to m.width - 1 do
          if Z.testbit m.bits (i * m.width + j)
          then Bytes.set bytes !pos 'x'
          else Bytes.set bytes !pos '.';
          incr pos
        done
      done;
      Bytes.to_string bytes
    let pp m = print_string (to_string m)
		    
    let empty height width =
      { height; width; bits = Z.zero }
    let full height width =
      { height; width; bits = Z.pred (Z.shift_left Z.one (height * width)) }
    let singleton height width i j =
      { height; width; bits = Z.shift_left Z.one (i * width + j) }
	
    let copy m = m

    let equal m1 m2 =
      m1.height = m2.height
      && m1.width = m2.width
      && Z.equal m1.bits m2.bits
    let is_empty m =
      Z.equal m.bits Z.zero
    let is_subset m1 m2 =
      Z.equal (Z.logand m1.bits (Z.lognot m2.bits)) Z.zero

    let mem i j m =
      i >=0 && i < m.height
      && j >= 0 && j < m.width
      && Z.testbit m.bits (i * m.width + j)

    let add_in_place i j m =
      { m with
	bits = Z.logor m.bits (Z.shift_left Z.one (i * m.width + j)) }
    let remove i j m =
      { m with
	bits = Z.logand m.bits (Z.lognot (Z.shift_left Z.one (i * m.width + j))) }

    let union m1 m2 =
      { m1 with bits = Z.logor m1.bits m2.bits }
    let inter m1 m2 =
      { m1 with bits = Z.logand m1.bits m2.bits }
    let diff m1 m2 =
      { m1 with bits = Z.logand m1.bits (Z.lognot m2.bits) }
    let diff_sym m1 m2 =
      { m1 with bits = Z.logxor m1.bits m2.bits }
    let compl m1 =
      { m1 with bits = Z.lognot m1.bits }

    let iter f m =
      for i = 0 to m.height - 1 do
        let i_w = i * m.width in
	for j = 0 to m.width - 1 do
	  if Z.testbit m.bits (i_w + j) then
	    f i j
	done
      done

    let fold f acc m =
      let res = ref acc in
      for i = 0 to m.height - 1 do
        let i_w = i * m.width in
	for j = 0 to m.width - 1 do
	  if Z.testbit m.bits (i_w + j) then
	    res := f !res i j
	done
      done;
      !res
  end

type mask_model =
  [ `Mask of Mask.t
  | `Full (* all pixels on *)
  | `Border (* width-1 border *)
  | `EvenCheckboard
  | `OddCheckboard
  | `PlusCross
  | `TimesCross
  ]

let mask_model_area ~height ~width = function
  | `Mask bm -> Mask.area bm
  | `Full -> height * width
  | `Border -> 2 * (height + width) - 4
  | `EvenCheckboard -> (height * width + 1) / 2
  | `OddCheckboard -> height * width / 2
  | `PlusCross -> height + width - 1
  | `TimesCross -> height + width - (height mod 2)
  
(* mask-based computations on grids *)

let majority_colors (mask : Mask.t) (g : t) : color list =
  Common.prof "Grid.majority_colors" (fun () ->
  let color_counter = Array.make nb_color 0 in
  Mask.iter
    (fun i j ->
      let c = g.matrix.{i,j} in
      color_counter.(c) <- color_counter.(c) + 1
    )
    mask;
  let max_count = ref 0 in
  let bcs = ref [] in
  for c = 0 to nb_color - 1 do
    let count = color_counter.(c) in
    if count = !max_count then bcs := c::!bcs
    else if count > !max_count then (
      max_count := count;
      bcs := [c])
    else ()
  done;
  !bcs)
let majority_colors, reset_majority_colors =
  let f, reset =
    Common.memoize ~size:103
      (fun (mask,g) ->
        majority_colors mask g) in
  let f = fun mask g -> f (mask,g) in
  f, reset

let mask_model_mem h w i j = (* mask height and width, relative position (i,j) *)
  function
  | `Mask m -> Mask.mem i j m
  | `Full -> true
  | `Border -> i=0 || j=0 || i=h-1 || j=w-1
  | `EvenCheckboard -> (i+j) mod 2 = 0
  | `OddCheckboard -> (i+j) mod 2 = 1
  | `PlusCross -> (i=h/2 || i=(h-1)/2) && (j=w/2 || j=(w-1)/2)
  | `TimesCross -> assert (h=w); i=j || (h-1-i) = j

let models_of_mask (m : Mask.t) : mask_model list =
  let h, w = m.height, m.width in
  let maxi, maxj = h - 1, w - 1 in
  let full = ref true in
  let border = ref true in
  let even_cb = ref true in
  let odd_cb = ref true in
  let plus_cross = ref true in
  let times_cross = ref (maxi = maxj) in
  for i=0 to maxi do
    for j=0 to maxj do (* reuse below Boolean expressions from 'mask_model_mem' *)
      let pixel_on = Mask.mem i j m in
      full := !full && pixel_on = true;
      border := !border && pixel_on = (i=0 || j=0 || i=maxi || j=maxj);
      even_cb := !even_cb && pixel_on = ((i+j) mod 2 = 0);
      odd_cb := !odd_cb && pixel_on = ((i+j) mod 2 = 1);
      plus_cross := !plus_cross && pixel_on = ((i=h/2 || i=maxi/2) && (j=w/2 || j=maxj/2));
      times_cross := !times_cross && pixel_on = (i=j || (maxi-i) = j)
    done
  done;
  let res = [] in
  let res = if !full then `Full::res else res in
  let res = if !border then `Border::res else res in
  let res = if !even_cb then `EvenCheckboard::res else res in
  let res = if !odd_cb then `OddCheckboard::res else res in
  let res = if !plus_cross then `PlusCross::res else res in
  let res = if !times_cross then `TimesCross::res else res in
  let res = if res = [] then [`Mask m] else res in
  res
  
             
(* segmenting grids *)

type part = { mini : int; maxi : int;
	      minj : int; maxj : int;
	      color : color;
	      nb_pixels : int;
	      pixels : Mask.t }

let part_as_grid (g : t) (p : part) : t = Common.prof "Grid.part_as_grid" (fun () ->
  let gp = make g.height g.width no_color in
  let col = p.color in
  Mask.iter
    (fun i j -> set_pixel gp i j col)
    p.pixels;
  gp)

let pp_parts (g : t) (ps : part list) : unit =
  print_endline "PARTS:";
(*  List.iter
    (fun p -> Printf.printf "(%d,%d)->(%d,%d) [%d/%d] "
			    p.mini p.minj
			    p.maxi p.maxj
			    p.nb_pixels
			    ((p.maxi-p.mini+1) * (p.maxj-p.minj+1)))
    ps;
  print_newline ();*)
  pp_grids (g :: List.map (part_as_grid g) ps)

let part_of_pixel ~height ~width i j c =
  { mini = i; maxi = i;
    minj = j; maxj = j;
    color = c;
    pixels = Mask.singleton height width i j;
    nb_pixels = 1 }

let merge_parts_2 p1 p2 =
  assert (p1.color = p2.color);
  { mini = min p1.mini p2.mini;
    maxi = max p1.maxi p2.maxi;
    minj = min p1.minj p2.minj;
    maxj = max p1.maxj p2.maxj;
    color = p1.color;
    pixels = Mask.union p1.pixels p2.pixels;
    nb_pixels = p1.nb_pixels + p2.nb_pixels }
  
let merge_parts (ps : part list) : part = (* QUICK *)
  match ps with
  | [] -> invalid_arg "Grid.merge_parts: empty list"
  | [p1] -> p1
  | p1::ps1 ->
     let pixels = ref (Mask.copy p1.pixels) in
     let mini, maxi, minj, maxj, nb_pixels =
       List.fold_left
	 (fun (mini, maxi, minj, maxj, nb_pixels) p2 ->
	  pixels := Mask.union !pixels p2.pixels;
	  min mini p2.mini, max maxi p2.maxi,
	  min minj p2.minj, max maxj p2.maxj,
	  nb_pixels + p2.nb_pixels)
	 (p1.mini, p1.maxi, p1.minj, p1.maxj, p1.nb_pixels)
	 ps1 in
     { mini; maxi; minj; maxj; color = p1.color;
       nb_pixels; pixels = (!pixels) }

     
module Skyline = (* min-skyline of coordinates *)
  struct
    type t = (int * int) list (* (x,y) list *)
    (* invariant:
       - sorted in increasing x, decreasing y
       - (x,y), (x,y') => y=y'
       - (x,y), (x',y) => x=x'
       - (x,y), (x',y'), x'<=x, y'<=y => x=x', y=y'
       - denotes the set of points
         { (x',y') | some (x,y) in skyline: x'>=x, y'>=y }
     *)
    let print sl =
      List.iter (fun (x,y) -> Printf.printf " (%d,%d)" x y) sl

    let equals sl1 sl2 = sl1=sl2
		
    let empty = []

    let is_empty sl = sl=[]

    let rec mem x y sl =
      match sl with
      | [] -> false
      | (x1,y1)::sl1 -> x >= x1 && (y >= y1 || mem x y sl1)
			   
    let min_x bound sl =
      match sl with (* x of first element *)
      | [] -> bound
      | (x1,_)::_ -> x1
    let rec min_y bound sl =
      match sl with (* y of last element *)
      | [] -> bound
      | [(_,y1)] -> y1
      | _::sl1 -> min_y bound sl1
		  
    let rec add x y sl =
      match sl with
      | [] -> [(x,y)]
      | (x1,y1)::sl1 ->
	 if x >= x1 && y >= y1 then sl (* already in *)
	 else if x < x1 && y > y1 then (x,y)::sl
	 else if x < x1 && y = y1 then (x,y)::sl1
	 else if x <= x1 && y <= y1 then add x y sl1 (* (x1,y1) covered *)
	 else (x1,y1)::add x y sl1

    let rec inter sl1 sl2 =
      match sl1, sl2 with
      | [], _ -> []
      | _, [] -> []
      | (x1,y1)::sl1', (x2,y2)::sl2' -> (* (xi,yi) as pi *)
	 if x1 < x2 && y1 > y2 then (* p1 before p2 in skyline *)
	   match sl1' with
	   | (x1',y1')::sl1'' when x1' < x2 && y1' > y2 ->
	      (* next point in sl1 is also before p2 *)
	      inter sl1' sl2 (* p1 can be skipped *)
	   | _ ->
	      let x, y = max x1 x2, max y1 y2 in
	      (* adding intersection between p1 and p2 *)
	      add x y (inter sl1' sl2)
	 else if x2 < x1 && y2 > y1 then (* p2 before p1 in skyline*)
	   match sl2' with
	   | (x2',y2')::sl2'' when x2' < x1 && y2' > y1 ->
	      (* next point in sl2 is also before p1 *)
	      inter sl1 sl2' (* p2 can be skipped *)
	   | _ ->
	      let x, y = max x1 x2, max y1 y2 in
	      (* adding intersection between p1 and p2 *)
	      add x y (inter sl1 sl2')
	 else (* p1 and p2 are comparable *)
	   if x1=x2 && y1=y2 then (* p1 = p2 *)
	     add x1 y1 (inter sl1' sl2')
	   else if x1 <= x2 && y1 <= y2 then (* p2 in p1 *)
	     add x2 y2 (inter sl1' sl2)
	   else (* p1 in p2 *)
	     add x1 y1 (inter sl1 sl2')

    let rec diff sl1 sl2 =
      match sl1, sl2 with
      | [], _ -> []
      | _, [] -> sl1
      | (x1,y1)::sl1', (x2,y2)::sl2' ->
	 if x2=x1 && y2=y1 then diff sl1' sl2'
	 else if x2 < x1 || y2 > y1 then diff sl1 sl2'
	 else (x1,y1)::diff sl1' sl2 

    let iter = List.iter
  end	 
    
let split_part (part : part) : part list =
  Common.prof "Grid.split_part" (fun () ->
  let mask = part.pixels in
  let h, w = Mask.height mask, Mask.width mask in
  let arr : Skyline.t array array = Array.make_matrix (h+1) (w+1) Skyline.empty in
  let res = ref [] in
  for i = part.mini to part.maxi+1 do
    for j = part.minj to part.maxj+1 do
      if i <= part.maxi && j <= part.maxj
	 && Mask.mem i j mask (* cell belongs to part *)
      then (
	let corners_above =
	  if i = part.mini
	  then Skyline.empty
	  else arr.(i-1).(j) in
	let corners_left =
	  if j = part.minj
	  then Skyline.empty
	  else arr.(i).(j-1) in
	let corners_above =
	  Skyline.add i (Skyline.min_y j corners_left) corners_above in
	let corners_left =
	  Skyline.add (Skyline.min_x i corners_above) j corners_left in
	arr.(i).(j) <- Skyline.inter corners_left corners_above
      );
      let closed_corners =
	if i > part.mini && j > part.minj then
	  Skyline.diff
	    (Skyline.diff
	       arr.(i-1).(j-1)
	       arr.(i-1).(j))
	    arr.(i).(j-1)
	else Skyline.empty in
      Skyline.iter
	(fun (a,b) ->
	  let mini, maxi = a, i-1 in
	  let minj, maxj = b, j-1 in
	  if mini=part.mini && maxi=part.maxi
	     && minj=part.minj && maxj=part.maxj
	  then () (* already known as part *)
	  else (
	    let pixels = ref (Mask.empty h w) in
	    for i = mini to maxi do
	      for j = minj to maxj do
		pixels := Mask.add_in_place i j !pixels
	      done
	    done;
	    let subpart =
	      { mini; maxi;
		minj; maxj;
		color = part.color;
		nb_pixels = (maxi-mini+1) * (maxj-minj+1);
		pixels = (!pixels) } in
	    res := subpart::!res
	))
	closed_corners
    done
  done;
  !res)
  
let segment_by_color (g : t) : part list =
  Common.prof "Grid.segment_by_color" (fun () ->
  let h, w = g.height, g.width in
  let hw = h * w in    
  let fm : (int * int, part) Find_merge.hashtbl =
    new Find_merge.hashtbl
      ~init_val:{ mini = h; maxi = 0;
                  minj = w; maxj = 0;
                  color = no_color;
                  pixels = Mask.empty h w;
                  nb_pixels = 0 }
      ~merge_val:merge_parts_2
  in
  let mat = g.matrix in
  (* setting initial val of each pixel *)
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      fm#replace (i,j) (part_of_pixel ~height:h ~width:w i j mat.{i,j})
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
	ignore (fm#merge [(i,j); (i+1,j+1)]);
      (* pixel left and down, diagonally *)
      if i+1 < h && j-1 >= 0 && mat.{i,j} = mat.{i+1,j-1} then
        ignore (fm#merge [(i,j); (i+1,j-1)])
    done
  done;
  (* collecting parts *)
  fm#fold
    (fun _ part res ->
      if part.mini=0 && part.maxi=h-1
         && part.minj=0 && part.maxj=w-1
         && part.color=black && 2 * part.nb_pixels - 1 > hw
     then res (* ignoring black background *)
     else part :: split_part part @ res)
    [])
let segment_by_color, reset_segment_by_color =
  Common.memoize ~size:203 segment_by_color
  

(* locating shapes *)

type point = pixel

let point_as_grid (g : t) (p : point) : t =
  let gp = make g.height g.width no_color in
  let i, j, c = p in
  set_pixel gp i j c;
  gp

let pp_points g ps =
  print_endline "POINTS:";
  pp_grids (g :: List.map (point_as_grid g) ps)

let points_of_part ?(acc : point list = []) mask (part : part) : point list =
  (* mask gives the part of the grid that remains to be covered *)
  if part.mini = part.maxi && part.minj = part.maxj
     && Mask.mem part.mini part.minj mask
  then (part.mini, part.minj, part.color)::acc
  else
    if part.nb_pixels <= 5
       && (part.maxi - part.mini + 1 <= 3)
       && (part.maxj - part.minj + 1 <= 3)
    then (* splitting small shapes into points *)
      Mask.fold
        (fun acc i j ->
          if Mask.mem i j mask
          then (i, j, part.color)::acc
          else acc)
        acc part.pixels
    else acc

let points (g : t) (mask : Mask.t) (parts : part list) : point list =
  Common.prof "Grid.points" (fun () ->
  parts
  |> List.fold_left
       (fun res part -> points_of_part ~acc:res mask part)
       []
  |> List.sort_uniq (fun (i1,j1,c1 as p1) (i2,j2,c2 as p2) ->
         Stdlib.compare (c1 = black, p1) (c2 = black, p2))) (* black points last *)
let points, reset_points =
  let f, reset =
    Common.memoize ~size:103
      (fun (g,mask,parts) ->
        points g mask parts) in
  let f = fun g mask parts -> f (g,mask,parts) in
  f, reset

type rectangle = { height: int; width: int;
		   offset_i: int; offset_j: int;
		   color: color;
		   mask : Mask.t; (* covered pixels *)
		   mask_models : mask_model list; (* mask models, relative to rectangle box *)
		   delta : pixel list;
                   nb_explained_pixels : int }

let rectangle_as_grid (g : t) (r : rectangle) : t =
  let gr = make g.height g.width no_color in
  let col = r.color in
  for i = r.offset_i to r.offset_i + r.height - 1 do
    for j = r.offset_j to r.offset_j + r.width - 1 do
      if Mask.mem i j r.mask then
	(*match List.find_opt (fun (i',j',c') -> i=i' && j=j') r.delta with
	| Some (_,_,c) -> set_pixel gr i j c
	| None ->*) set_pixel gr i j col
    done
  done;
  gr
		   
let pp_rectangles (g : t) (rs : rectangle list) =
  print_endline "RECTANGLES:";
  pp_grids (g :: List.map (rectangle_as_grid g) rs)

let rectangles_of_part ~(multipart : bool) (g : t) (mask : Mask.t) (p : part) : rectangle list = (* QUICK *)
   let h, w, p_color, p_pixels = p.maxi-p.mini+1, p.maxj-p.minj+1, p.color, p.pixels in
   let _area = h * w in
   let r_mask = ref (Mask.copy p.pixels) in (* rectangle mask *)
   let valid_area = ref 0 in (* area of r_mask *)
   let delta = ref [] in (* inconsistent pixels, relative to mask and p.color *)
   let nb_explained_pixels = ref 0 in (* mask pixels explained by rectangle *)
   for i = p.mini to p.maxi do
     for j = p.minj to p.maxj do
       let c = g.matrix.{i,j} in
       if Mask.mem i j mask (* pixel (i,j) in rectangle box, not yet explained *)
       then
         if c = p_color && Mask.mem i j p_pixels (* this pixel is in part and has the expected color *)
         then (
	   r_mask := Mask.add_in_place i j !r_mask; (* included in rectangle mask *)
           incr valid_area;
           incr nb_explained_pixels ) (* and hence now explained *)
         else delta := (i,j,c)::!delta (* the pixel has yet to be explained *)
       else ( (* pixel in rectangle box but already explained, hidden behind above layers *)
	 r_mask := Mask.add_in_place i j !r_mask; (* we consider it as valid *)
	 incr valid_area )
     done
   done;
   let res = [] in
   let res = (* adding rectangle with specific mask model, without delta *)
     if not multipart && !delta <> [] (* && !valid_area >= 1 * area / 2 *)
     then
       let m =
	 List.fold_left
	   (fun m (i,j,c) ->
	     Mask.remove (i - p.mini) (j - p.minj) m)
	   (Mask.full h w)
	   !delta in
       { height = p.maxi-p.mini+1;
	 width = p.maxj-p.minj+1;
	 offset_i = p.mini;
	 offset_j = p.minj;
	 color = p.color;
	 mask = (!r_mask);
	 mask_models = models_of_mask m;
	 delta = [];
         nb_explained_pixels = (!nb_explained_pixels) } :: res
     else res in
   let res = (* adding full rectangle with delta *)
     if List.length !delta < !valid_area
     then
       let _valid_area = !valid_area + List.length !delta in
       let mask =
         List.fold_left
           (fun mask (i,j,c) -> Mask.add_in_place i j mask)
           (!r_mask) !delta in
       { height = p.maxi-p.mini+1;
	 width = p.maxj-p.minj+1;
	 offset_i = p.mini;
	 offset_j = p.minj;
	 color = p.color;
	 mask;
	 mask_models = [`Full];
	 delta = (!delta);
         nb_explained_pixels = (!nb_explained_pixels) } :: res
     else res in
   res

(*let rectangles_of_part, reset_rectangles_of_part =
  let f, reset =
    Common.memoize ~size:103
      (fun (multipart,g,mask,p) ->
        rectangles_of_part ~multipart g mask p) in
  let f = fun ~multipart g mask p -> f (multipart,g,mask,p) in
  f, reset*)
 
let rectangles (g : t) (mask : Mask.t) (parts : part list) : rectangle list =
  Common.prof "Grid.rectangles" (fun () ->
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
(*  let c_sets =
    (* grouping same-color parts *)
    Common.group_by
      (fun (part : part) -> part.color)
      parts in *)
  let res = [] in
  let res =
    List.fold_left
      (fun res p ->
       let lr = rectangles_of_part ~multipart:false g mask p in
       lr @ res)
      res parts in
  let res =
    List.fold_left
      (fun res (_, ps) ->
       match ps with
       | [] -> assert false
       | [_] -> res
       | _ -> (* at least two parts *)
	  let mp = merge_parts ps in
	  if List.exists (fun p -> Mask.equal p.pixels mp.pixels) ps
	  then res
	  else
	    let lr = rectangles_of_part ~multipart:true g mask mp in
	    lr @ res)
      res h_sets in
  let res =
    List.fold_left
      (fun res (_, ps) ->
       match ps with
       | [] -> assert false
       | [_] -> res
       | _ -> (* at least two parts *)
	  let mp = merge_parts ps in
	  if List.exists (fun p -> Mask.equal p.pixels mp.pixels) ps
	  then res
	  else
	    let lr = rectangles_of_part ~multipart:true g mask mp in
	    lr @ res)
      res v_sets in
(*  let res =
    List.fold_left
      (fun res (_, ps) ->
       match ps with
       | [] -> assert false
       | [_] -> res
       | _ -> (* at least two parts *)
	  let mp = merge_parts ps in
	  if List.exists (fun p -> Mask.equal p.pixels mp.pixels) ps
	  then res
	  else
	    let lr = rectangles_of_part ~multipart:true g mask mp in
	    lr @ res)
      res c_sets in *)
  let res =
    res
    |> List.sort
         (fun rect1 rect2 ->
           Stdlib.compare (* black last, decreasing nb_explained_pixels *)
             (rect1.color = black, - rect1.nb_explained_pixels, rect1)
             (rect2.color = black, - rect2.nb_explained_pixels, rect2))
  in
  res)
let rectangles, reset_rectangles =
  let f, reset =
    Common.memoize ~size:103
      (fun (g,mask,parts) ->
        rectangles g mask parts) in
  let f = fun g mask parts -> f (g,mask,parts) in
  f, reset


let reset_memoized_functions () =
  reset_segment_by_color ();
  reset_majority_colors ();
  reset_points ();
  (*  reset_rectangles_of_part ();*)
  reset_rectangles ()
