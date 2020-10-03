
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

module MaskArray2 = (* to identify active pixels in a same-size grid *)
  struct
    type t = (int, int8_unsigned_elt, c_layout) Array2.t

    let empty height width : t = Common.prof "Grid.Mask.empty" (fun () ->
      let m = Array2.create Int8_unsigned C_layout height width in
      Array2.fill m 0;
      m)

    let singleton height width i j : t = Common.prof "Grid.Mask.singleton" (fun () ->
      let m = Array2.create Int8_unsigned C_layout height width in
      Array2.fill m 0;
      m.{i,j} <- 1;
      m)
	
    let rect height width mini maxi minj maxj : t = Common.prof "Grid.Mask.rect" (fun () ->
      let m = Array2.create Int8_unsigned C_layout height width in
      Array2.fill m 0;
      for i = mini to maxi do
	for j = minj to maxj do
	  m.{i,j} <- 1
	done
      done;
      m)

    let full height width : t = Common.prof "Grid.Mask.full" (fun () ->
      let m = Array2.create Int8_unsigned C_layout height width in
      Array2.fill m 1;
      m)

    let copy m : t = Common.prof "Grid.Mask.copy" (fun () ->
      let m2 = Array2.create Int8_unsigned C_layout
			    (Array2.dim1 m) (Array2.dim2 m) in
      Array2.blit m m2;
      m2)

    let is_empty m : bool = Common.prof "Grid.Mask.is_empty" (fun () ->
      let height = Array2.dim1 m in
      let width = Array2.dim2 m in
      let res = ref 0 in
      for i = 0 to height - 1 do
	for j = 0 to width - 1 do
	  res := !res lor m.{i,j}
	done
      done;
      !res = 0)

    let is_subset m1 m2 : bool = Common.prof "Grid.Mask.is_subset" (fun () ->
      let height = Array2.dim1 m1 in
      let width = Array2.dim2 m2 in
      assert (Array2.dim1 m2 = height);
      assert (Array2.dim2 m2 = width);
      let res = ref 1 in
      for i = 0 to height - 1 do
	for j = 0 to width - 1 do
	  res := !res land (lnot m1.{i,j} lor m2.{i,j})
	done
      done;
      !res = 1)

    let mem i j m =
      m.{i,j} = 1

    let add_in_place i j m =
      m.{i,j} <- 1;
      m
	       
    let union m1 m2 = Common.prof "Grid.Mask.union" (fun () ->
      let height = Array2.dim1 m1 in
      let width = Array2.dim2 m1 in
      assert (Array2.dim1 m2 = height);
      assert (Array2.dim2 m2 = width);
      let m = Array2.create Int8_unsigned C_layout height width in
      for i = 0 to height - 1 do
	for j = 0 to width - 1 do
	  m.{i,j} <- m1.{i,j} lor m2.{i,j}
	done
      done;
      m)
		     
    let union_list ms =
      match ms with
      | [] -> invalid_arg "Grid.Mask.union_list: empty list"
      | m1::ms1 ->
	 let height = Array2.dim1 m1 in
	 let width = Array2.dim2 m1 in
	 let m = Array2.create Int8_unsigned C_layout height width in
	 for i = 0 to height - 1 do
	   for j = 0 to width - 1 do
	     m.{i,j} <-
	       List.fold_left
		 (fun res mk -> res lor mk.{i,j})
		 m1.{i,j} ms1
	   done
	 done;
	 m

    let inter m1 m2 = Common.prof "Grid.Mask.inter" (fun () ->
      let height = Array2.dim1 m1 in
      let width = Array2.dim2 m1 in
      assert (Array2.dim1 m2 = height);
      assert (Array2.dim2 m2 = width);
      let m = Array2.create Int8_unsigned C_layout height width in
      for i = 0 to height - 1 do
	for j = 0 to width - 1 do
	  m.{i,j} <- m1.{i,j} land m2.{i,j}
	done
      done;
      m)
		     
    let diff m1 m2 = Common.prof "Grid.Mask.diff" (fun () ->
      let height = Array2.dim1 m1 in
      let width = Array2.dim2 m1 in
      assert (Array2.dim1 m2 = height);
      assert (Array2.dim2 m2 = width);
      let m = Array2.create Int8_unsigned C_layout height width in
      for i = 0 to height - 1 do
	for j = 0 to width - 1 do
	  m.{i,j} <- m1.{i,j} land (lnot m2.{i,j})
	done
      done;
      m)
		     
    let iter f m =
      let height = Array2.dim1 m in
      let width = Array2.dim2 m in
      for i = 0 to height - 1 do
	for j = 0 to width - 1 do
	  if m.{i,j} = 1 then
	    f i j
	done
      done
	  
  end

module MaskZ =
  struct
    type t = { height : int;
	       width : int;
	       bits : Z.t; }

    let empty height width =
      { height; width; bits = Z.zero }
    let full height width =
      { height; width; bits = Z.pred (Z.shift_left Z.one (height * width)) }
    let singleton height width i j =
      { height; width; bits = Z.shift_left Z.one (i * width + j) }

    let copy m = m

    let is_empty m =
      Z.equal m.bits Z.zero
    let is_subset m1 m2 =
      Z.equal (Z.logand m1.bits (Z.lognot m2.bits)) Z.zero

    let mem i j m =
      Z.testbit m.bits (i * m.width + j)

    let add_in_place i j m =
      { m with
	bits = Z.logor m.bits (Z.shift_left Z.one (i * m.width + j)) }

    let union m1 m2 =
      { m1 with bits = Z.logor m1.bits m2.bits }
    let inter m1 m2 =
      { m1 with bits = Z.logand m1.bits m2.bits }
    let diff m1 m2 =
      { m1 with bits = Z.logand m1.bits (Z.lognot m2.bits) }

    let iter f m =
      for i = 0 to m.height - 1 do
	for j = 0 to m.width - 1 do
	  if Z.testbit m.bits (i * m.width + j) then
	    f i j
	done
      done
  end
    
module Mask = MaskZ
    
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

let merge_parts (p1 : part) (p2 : part) : part = Common.prof "Grid.merge_parts" (fun () ->
  assert (p1.color = p2.color);
  { mini = min p1.mini p2.mini;
    maxi = max p1.maxi p2.maxi;
    minj = min p1.minj p2.minj;
    maxj = max p1.maxj p2.maxj;
    color = p1.color;
    nb_pixels = p1.nb_pixels + p2.nb_pixels;
    pixels = Mask.union p1.pixels p2.pixels })
							     
let merge_part_list (ps : part list) : part = Common.prof "Grid.merge_part_list" (fun () ->
  match ps with
  | [] -> invalid_arg "Grid.merge_part_list: empty list"
  | [p1] -> p1
  | p1::ps1 -> List.fold_left merge_parts p1 ps1)
			      
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

module PixelsMerge =
  struct
    type t =
      | Pixel of int * int
      | Merge of t * t
    let rec fold (f : 'a -> int -> int -> 'a) (acc : 'a) (pm : t) : 'a =
      match pm with
      | Pixel (i,j) -> f acc i j
      | Merge (pm1,pm2) ->
	 let acc1 = fold f acc pm1 in
	 let acc2 = fold f acc1 pm2 in
	 acc2
  end
    
let segment_by_color (g : t) : part list = Common.prof "Grid.segment_by_color" (fun () ->
  let h, w = g.height, g.width in
  let fm : (int * int, color * PixelsMerge.t) Find_merge.hashtbl =
    new Find_merge.hashtbl
	~init_val:(no_color, PixelsMerge.Pixel (-1,-1))
	~merge_val:
	(fun (c1,pm1) (c2,pm2) ->
	 assert (c1 = c2);
	 (c1, PixelsMerge.Merge (pm1,pm2)))
  in
  let mat = g.matrix in
  (* setting initial val of each pixel *)
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let coord = (i,j) in
      fm#replace coord (mat.{i,j}, PixelsMerge.Pixel (i,j))
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
    (fun _ (c,ph) res ->
     let pixels = ref (Mask.empty h w) in
     let mini, maxi, minj, maxj, nb_pixels =
       PixelsMerge.fold
	 (fun (mini, maxi, minj, maxj, nb_pixels) i j ->
	  pixels := Mask.add_in_place i j !pixels;
	  min mini i, max maxi i,
	  min minj j, max maxj j,
	  nb_pixels + 1)
	 (h, -1, w, -1, 0)
	 ph in
     let part =
       { mini; maxi;
	 minj; maxj;
	 color = c;
	 nb_pixels;
	 pixels = (!pixels) } in
     part::res)
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
  let valid_area = ref 0 in
  let r_mask = ref (Mask.copy p.pixels) in
  let delta = ref [] in
  for i = p.mini to p.maxi do
    for j = p.minj to p.maxj do
      if Mask.mem i j mask
      then (
	r_mask := Mask.add_in_place i j !r_mask;
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
  let h_sets = Common.prof "Grid.rectangles/group_by" (fun () ->
    (* grouping same-color parts spanning same rows *)
    Common.group_by
      (fun part -> part.mini, part.maxi, part.color)
      parts) in
  let v_sets = Common.prof "Grid.rectangles/group_by" (fun () ->
    (* grouping same-color parts spanning same cols *)
    Common.group_by
      (fun part -> part.minj, part.maxj, part.color)
      parts) in
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

