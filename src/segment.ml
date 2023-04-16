(* segmenting grids *)

open Arc_common

type part = { mini : int; maxi : int;
	      minj : int; maxj : int;
	      color : Grid.color;
	      nb_pixels : int;
	      pixels : Bitmap.t }

let part_as_grid (g : Grid.t) (p : part) : Grid.t = Common.prof "Grid.part_as_grid" (fun () ->
  let gp = Grid.make g#height g#width Grid.transparent in
  let col = p.color in
  Bitmap.iter
    (fun i j -> Grid.set_pixel gp i j col)
    p.pixels;
  gp)

let pp_parts (g : Grid.t) (ps : part list) : unit =
  print_endline "PARTS:";
(*  List.iter
    (fun p -> Printf.printf "(%d,%d)->(%d,%d) [%d/%d] "
			    p.mini p.minj
			    p.maxi p.maxj
			    p.nb_pixels
			    ((p.maxi-p.mini+1) * (p.maxj-p.minj+1)))
    ps;
  print_newline ();*)
  Grid.pp_grids (g :: List.map (part_as_grid g) ps)

let part_of_pixel ~height ~width i j c =
  { mini = i; maxi = i;
    minj = j; maxj = j;
    color = c;
    pixels = Bitmap.singleton height width i j;
    nb_pixels = 1 }

let merge_parts_2 p1 p2 =
  assert (p1.color = p2.color);
  { mini = min p1.mini p2.mini;
    maxi = max p1.maxi p2.maxi;
    minj = min p1.minj p2.minj;
    maxj = max p1.maxj p2.maxj;
    color = p1.color;
    pixels = Bitmap.union p1.pixels p2.pixels;
    nb_pixels = p1.nb_pixels + p2.nb_pixels }
  
let merge_parts (ps : part list) : part = (* QUICK *)
  match ps with
  | [] -> invalid_arg "Grid.merge_parts: empty list"
  | [p1] -> p1
  | p1::ps1 ->
     let pixels = ref p1.pixels in (* TODO: do not use a ref here *)
     let mini, maxi, minj, maxj, nb_pixels =
       List.fold_left
	 (fun (mini, maxi, minj, maxj, nb_pixels) p2 ->
	  pixels := Bitmap.union !pixels p2.pixels;
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
  let bmp = part.pixels in
  let h, w = Bitmap.height bmp, Bitmap.width bmp in
  let arr : Skyline.t array array = Array.make_matrix (h+1) (w+1) Skyline.empty in
  let res = ref [] in
  for i = part.mini to part.maxi+1 do
    for j = part.minj to part.maxj+1 do
      if i <= part.maxi && j <= part.maxj
	 && Bitmap.mem i j bmp (* cell belongs to part *)
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
	    let pixels = ref (Bitmap.empty h w) in
	    for i = mini to maxi do
	      for j = minj to maxj do
		pixels := Bitmap.add i j !pixels
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
  
let segment_by_color (g : Grid.t) : part list =
  Common.prof "Grid.segment_by_color" (fun () ->
  let h, w = Grid.dims g in
  let hw = h * w in    
  let fm : (int * int, part) Find_merge.hashtbl =
    new Find_merge.hashtbl
      ~init_val:{ mini = h; maxi = 0;
                  minj = w; maxj = 0;
                  color = Grid.transparent;
                  pixels = Bitmap.empty h w;
                  nb_pixels = 0 }
      ~merge_val:merge_parts_2
  in
  let mat = g#matrix in
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
    (fun _ part res -> (* TODO: fin a way to avoid this trick *)
      if part.mini=0 && part.maxi=h-1
         && part.minj=0 && part.maxj=w-1
         && part.color=Grid.black (* NOTE: do not relax this constraint *)
         && 2 * part.nb_pixels - 1 > hw
      then res (* ignoring black background *)
      else part :: split_part part @ res)
    [])
let segment_by_color, reset_segment_by_color =
  Memo.memoize ~size:203 segment_by_color
  

(* locating shapes *)

type rectangle = { height: int; width: int;
		   color: Grid.color;
                   mask : Grid.t;
		   mask_models : Mask_model.t list; (* mask models, relative to rectangle box *)
		   delta : Grid.pixel list;
                   nb_explained_pixels : int }

type pattern =
  [ `None
  | `Point of Grid.color
  | `Rectangle of rectangle ]

type t = { bmp_cover : Bitmap.t;
           pos : int * int;
           shape : Grid.t; (* the subgrid *)
           pattern : pattern }

let segment_as_grid (g : Grid.t) (seg : t) : Grid.t =
  (* to show a segment in the context of its encompassing grid *)
  let offset_i, offset_j = seg.pos in
  let res = Grid.make g#height g#width Grid.transparent in
  Grid.iter_pixels
    (fun delta_i delta_j c ->
      if c = Grid.transparent then ()
      else
        let i, j = offset_i + delta_i, offset_j + delta_j in
        Grid.set_pixel res i j c)
    seg.shape;
  res
  
let pp_segments (label : string) (g : Grid.t) (ss : t list) =
  print_endline label;
  Grid.pp_grids (g :: List.map (segment_as_grid g) ss)		   
  

let background_colors (g : Grid.t) : Grid.color list = (* QUICK, in decreasing frequency order *)
  let area = g#height * g#width in
  let l = ref [] in
  for c = Grid.black to Grid.last_color do
    let n = g#color_count.(c) in
    if n > 0 then l := (c,n)::!l (* keeping only occurring colors *)
  done;
  let l = List.sort (fun (c1,n1) (c2,n2) -> Stdlib.compare (n2,c1) (n1,c2)) !l in
  let l =
    match l with (* keep only significant colors, and at most 2 *)
    | (c1,n1)::(c2,n2)::_ ->
       if n2 >= area / 4
       then [c1; c2]
       else [c1]
    | (c1,_)::_ -> [c1]
    | [] -> [] in
  if List.mem Grid.black l
  then l
  else l @ [Grid.black] (* ensure black is considered as a prefered background color *)

  
let points_of_part ?(acc : Grid.pixel list = []) bmp (part : part) : Grid.pixel list =
  (* bmp gives the part of the grid that remains to be covered *)
  if part.mini = part.maxi && part.minj = part.maxj
     && Bitmap.mem part.mini part.minj bmp
  then (part.mini, part.minj, part.color) :: acc
  else
    if part.nb_pixels <= 5
       && (part.maxi - part.mini + 1 <= 3)
       && (part.maxj - part.minj + 1 <= 3)
    then (* splitting small shapes into points *)
      Bitmap.fold
        (fun acc i j ->
          if Bitmap.mem i j bmp
          then (i, j, part.color) :: acc
          else acc)
        acc part.pixels
    else acc

let points (g : Grid.t) (bmp : Bitmap.t) (parts : part list) : t list =
  Common.prof "Grid.points" (fun () ->
  let height, width = Grid.dims g in
  parts
  |> List.fold_left
       (fun res part -> points_of_part ~acc:res bmp part)
       []
  |> List.sort_uniq (fun p1 p2 ->
         Stdlib.compare p1 p2)
  |> List.map (fun (i,j,c) ->
         { bmp_cover = Bitmap.singleton height width i j;
           pos = (i, j);
           shape = Grid.make 1 1 c;
           pattern = `Point c }))
let points, reset_points =
  Memo.memoize3 ~size:103 points


let shape_of_rectangle bmp offset_i offset_j rect : Grid.t =
  assert ((rect.height, rect.width) = Grid.dims rect.mask);
  let res = Grid.make rect.height rect.width Grid.transparent in
  Grid.Mask.iter
    (fun i j ->
      if Bitmap.mem (offset_i+i) (offset_j+j) bmp
      then Grid.set_pixel res i j rect.color
      else Grid.set_pixel res i j Grid.undefined)
    rect.mask;
  res
  
let rectangles_of_part ~(multipart : bool) (g : Grid.t) (bmp : Bitmap.t) (p : part) : (Bitmap.t * int * int * rectangle) list =
  let height, width = p.maxi-p.mini+1, p.maxj-p.minj+1 in
  let delta = ref [] in
  let nb_delta = ref 0 in
  let nb_explained_pixels = Bitmap.area (Bitmap.inter bmp p.pixels) in 
  for i = p.mini to p.maxi do
    for j = p.minj to p.maxj do
      if Bitmap.mem i j bmp && not (Bitmap.mem i j p.pixels)
      then (
        delta := (i, j, g#matrix.{i,j}) :: !delta;
        incr nb_delta
      )
    done
  done;
  let res = [] in
  let res = (* adding full rectangle with delta *)
    if !nb_delta < height * width - !nb_delta
    then
      let bmp_cover =
        List.fold_left
          (fun bmp (i,j,c) -> Bitmap.add i j bmp)
          p.pixels !delta in
      let elt =
        (bmp_cover, p.mini, p.minj,
         { height; width;
           color = p.color;
           mask = Grid.Mask.full height width;
           mask_models =
             (`Full ::
                if (height = 2 && width >= 2 || width = 2 && height >= 2)
                then [`Border]
                else []);
	   delta = (!delta);
           nb_explained_pixels
        }) in
      elt :: res
    else res in
  let res = (* adding rectangle with specific mask model, without delta *)
    if not multipart && !delta <> [] (* && !valid_area >= 1 * area / 2 *)
    then
      let mask, mask_models =
        Mask_model.from_box_in_bmp ~visible_bmp:bmp
          ~mini:p.mini ~maxi:p.maxi ~minj:p.minj ~maxj:p.maxj
          p.pixels in
      let elt =
        (p.pixels, p.mini, p.minj,
         { height; width;
           color = p.color;
           mask;
           mask_models;
           delta = [];
           nb_explained_pixels
        }) in
      elt :: res
    else res in
  res

let rectangles (g : Grid.t) (bmp : Bitmap.t) (parts : part list) : t list =
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
       let lr = rectangles_of_part ~multipart:false g bmp p in
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
	  if List.exists (fun p -> Bitmap.equal p.pixels mp.pixels) ps
	  then res
	  else
	    let lr = rectangles_of_part ~multipart:true g bmp mp in
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
	  if List.exists (fun p -> Bitmap.equal p.pixels mp.pixels) ps
	  then res
	  else
	    let lr = rectangles_of_part ~multipart:true g bmp mp in
	    lr @ res)
      res v_sets in
(*  let res = (* adding a grid-wide one-color rectangle goes against object-centric modeling, this breaks many solved tasks *)
    List.fold_left
      (fun res (c, ps) ->
        match ps with
        | [] -> assert false
        | p::ps1 ->
           let pixels =
             List.fold_left
               (fun pixels p1 -> Bitmap.union pixels p1.pixels)
               p.pixels ps1 in
           let r : rectangle =
             { height = g.height;
               width = g.width;
               offset_i = 0;
               offset_j = 0;
               color = c;
               bmp_cover = pixels;
               mask_models = Mask_model.from_box_in_bmp ~visible_bmp:bmp
                               ~mini:0 ~maxi:(g.height-1) ~minj:0 ~maxj:(g.width-1)
                               pixels;
               delta = [];
               nb_explained_pixels = (Bitmap.area (Bitmap.inter bmp pixels));
             } in
           if List.mem r res
           then res
           else r :: res)
      res c_sets in *)
(*  let res =
    List.fold_left
      (fun res (_, ps) ->
       match ps with
       | [] -> assert false
       | [_] -> res
       | _ -> (* at least two parts *)
	  let mp = merge_parts ps in
	  if List.exists (fun p -> Bitmap.equal p.pixels mp.pixels) ps
	  then res
	  else
	    let lr = rectangles_of_part ~multipart:true g bmp mp in
	    lr @ res)
      res c_sets in *)
  let res =
    res
    |> List.sort
         (fun (_,i1,j1,rect1) (_,i2,j2,rect2) ->
           Stdlib.compare (* decreasing nb_explained_pixels *)
             (- rect1.nb_explained_pixels, i1, j1, rect1)
             (- rect2.nb_explained_pixels, i2, j2, rect2))
  in
  res
  |> List.map
       (fun (bmp_cover,i,j,rect) ->
         let shape = shape_of_rectangle bmp i j rect in
         { bmp_cover;
           pos = (i,j);
           shape;
           pattern = `Rectangle rect }))
let rectangles, reset_rectangles =
  Memo.memoize3 ~size:103 rectangles


let reset_memoized_functions () =
  reset_segment_by_color ();
  reset_points ();
  reset_rectangles ()
