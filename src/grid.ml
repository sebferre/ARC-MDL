
let memoize_size = 101

open Arc_common
open Bigarray

type color = int

(* conventional colors like in web app *)
let black = 0 (* first color *)
let blue = 1
let red = 2
let green = 3
let yellow = 4
let grey = 5
let pink = 6
let orange = 7
let cyan = 8
let brown = 9	       

(* special colors *)
let transparent = 10 (* for non-covered parts, no color *)
let undefined = 11 (* for use in special algos, hidden parts, any color *)

let nb_color = 10
let last_color = 9

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
  | 10 -> "transparent"
  | 11 -> "undefined"
  | c -> "col" ^ string_of_int c


type matrix = (int, int8_unsigned_elt, c_layout) Array2.t

type pixel = int * int * color (* x, y, col *)

exception Invalid_dim (* height/width are invalid for the operation *)
exception Invalid_coord of string (* .{i,j} invalid, string describes source *)
                         
let max_size = 30

(* NOTE: use constructor functions below, not [new grid] *)
class grid height width =
  let matrix = Array2.create Int8_unsigned C_layout height width in
  let color_count = Array.make (nb_color+2) 0 in
  object
    val height : int = height (* equals Array2.dim1 matrix *)
    val width : int = width  (* equals Array2.dim2 matrix *)
    val color_count : int Array.t = color_count (* col (0..10) -> nb. cells with that color *)
    val matrix : matrix = matrix (* [i,j] -> col *)

    method fill (col : color) : unit =
      Array2.fill matrix col;
      color_count.(col) <- height * width      
                        
    method init (f : int -> int -> color) : unit =
      for i = 0 to height-1 do
        for j = 0 to width-1 do
          let c = f i j in
          matrix.{i,j} <- c;
          color_count.(c) <- color_count.(c) + 1
        done
      done

    method set_pixel i j c = (* use on defined grid *)
      if i < height && j < width && i >= 0 && j >= 0
      then 
        let c0 = matrix.{i,j} in
        if c <> c0 then (
          matrix.{i,j} <- c;
          (* maintaining color count *)
          color_count.(c0) <- color_count.(c0) - 1;
          color_count.(c) <- color_count.(c) + 1
        ) [@@inline]
    (* pixels out of bound are ignored *)

    method map_color (f : color -> color) =
      for i = 0 to height - 1 do
        for j = 0 to width - 1 do
          let c = matrix.{i,j} in
          let c' = f c in
          if c' <> c then (
            matrix.{i,j} <- c';
            (* maintaining color count *)
            color_count.(c) <- color_count.(c) - 1;
            color_count.(c') <- color_count.(c') + 1
          )
        done
      done
                        
    method copy =
      let matrix' = Array2.create Int8_unsigned C_layout height width in
      Array2.blit matrix matrix';
      {< color_count = Array.copy color_count;
         matrix = matrix' >}

    method height = height
    method width = width
    method color_count = color_count
    method matrix = matrix

    method iter_pixels (f : int -> int -> color -> unit) =
      for i = 0 to height - 1 do
        for j = 0 to width - 1 do
          f i j matrix.{i,j}
        done
      done [@@inline]
                  
  end

type t = grid

(* grid constructors *)
       
let make h w c =
  let g = new grid h w in
  g#fill c;
  g

let dummy = make 0 0 0

let init h w f =
  let g = new grid h w in
  g#init f;
  g

let map_pixels (f : color -> color) g =
  let g' = g#copy in (* copying because often not many cells changed *)
  g'#map_color f;
  g' [@@inline]

let map2_pixels (f : color -> color -> color) g1 g2 =
  let h1, w1 = g1#height, g1#width in
  if g2#height <> h1 || g2#width <> w1
  then raise Invalid_dim
  else (
    let g' = new grid h1 w1 in
    g'#init
      (fun i j ->
        let c1 = g1#matrix.{i,j} in
        let c2 = g2#matrix.{i,j} in
        f c1 c2);
    g') [@@inline]


(* accessors and iterators *)
  
let dims (grid : t) : int * int =
  grid#height, grid#width [@@inline]

let get_pixel ?(source = "unknown") grid i j =
  let h, w = dims grid in
  if i < h && j < w && i >= 0 && j >= 0
  then grid#matrix.{i,j}
  else raise (Invalid_coord source) [@@inline]
  
let fold_pixels (f : 'a -> int -> int -> color -> 'a) (acc : 'a) grid : 'a =
  let mat = grid#matrix in
  let res = ref acc in
  for i = 0 to grid#height - 1 do
    for j = 0 to grid#width - 1 do
      let c = mat.{i,j} in
      res := f !res i j c
    done
  done;
  !res [@@inline]

let fold2_pixels (f : 'a -> int -> int -> color -> color -> 'a) (acc : 'a) g1 g2 : 'a =
  if g2#height <> g1#height || g2#width <> g1#width
  then raise Invalid_dim
  else
    let mat1 = g1#matrix in
    let mat2 = g2#matrix in
    let res = ref acc in
    for i = 0 to g1#height - 1 do
      for j = 0 to g1#width - 1 do
        let c1 = mat1.{i,j} in
        let c2 = mat2.{i,j} in
        res := f !res i j c1 c2
      done
    done;
    !res [@@inline]

let for_all_pixels f grid =
  let mat = grid#matrix in
  let h, w = grid#height, grid#width in
  let res = ref true in
  let i = ref 0 in
  let j = ref 0 in
  while !res && !i < h do
    j := 0;
    while !res && !j < w do
      res := !res && f !i !j mat.{!i,!j};
      incr j
    done;
    incr i
  done;
  !res [@@inline]

let for_all2_pixels f grid1 grid2 =
  if grid2#height <> grid1#height || grid2#width <> grid1#width
  then raise Invalid_dim
  else
    let h, w = grid1#height, grid1#width in
    let mat1 = grid1#matrix in
    let mat2 = grid2#matrix in
    let res = ref true in
    let i = ref 0 in
    let j = ref 0 in
    while !res && !i < h do
      j := 0;
      while !res && !j < w do
        res := !res && f !i !j mat1.{!i,!j} mat2.{!i,!j};
        incr j
      done;
      incr i
    done;
    !res [@@inline]

let majority_color (bgcolor : color) (g : t) : color result = (* not counting bgcolor *)
  let res = ref undefined in
  let nb_max = ref 0 in
  for c = black to last_color do
    let nb = g#color_count.(c) in
    if c <> bgcolor && nb > !nb_max then (
      res := c;
      nb_max := nb)
  done;
  if !res = undefined
  then Result.Error (Undefined_result "majority_color: all transparent of bgcolor")
  else Result.Ok !res

let color_count (bgcolor : color) (grid : t) : int = (* not counting bgcolor *)
  let res = ref 0 in
  for c = black to last_color do
    if c <> bgcolor && grid#color_count.(c) > 0
    then incr res
  done;
  !res

let color_area (bgcolor : color) (g : t) : int = (* number of colored cells, excluding bgcolor *)
  let res = ref 0 in
  for c = black to last_color do
    if c <> bgcolor
    then res := !res + g#color_count.(c)
  done;
  !res
  
let color_partition ~(colors : color list) (grid : t) : int list =
  List.map
    (fun c -> grid#color_count.(c))
    colors

let color_freq_desc (grid : t) : (int * color) list =
  let res = ref [] in
  for c = black to last_color do
    let n = grid#color_count.(c) in
    if n > 0 then res := (n,c) :: !res
  done;
  List.sort Stdlib.compare !res

let fill_undefined (g : t) (c : color) : t =
  map_pixels
    (fun c0 ->
      if c0 = undefined
      then c
      else c0)
    g
let fill_undefined, reset_fill_undefined =
  Memo.memoize2 ~size:memoize_size fill_undefined

  
(* pretty-printing in terminal *)

let rec xp_grids (print : Xprint.t) grids =
  let grids_per_line = 5 in
  let nb_lines = (List.length grids - 1) / 5 + 1 in
  let max_height =
    List.fold_left (fun res g -> max res g#height) 0 grids in
  print#string "\n";
  for k = 0 to nb_lines - 1 do
    for i = 0 to max_height - 1 do
      List.iteri
	(fun l g ->
	 if l / grids_per_line = k then (
	   for j = 0 to g#width - 1 do
	     if i < g#height
	     then xp_color print g#matrix.{i,j}
	     else xp_blank print ()
	   done;
	   print#string "   "
	 ))
	grids;
      print#string "\n"
    done;
    print#string "\n"
  done
and xp_grid print grid =
  print#string "\n";
  for i = 0 to grid#height - 1 do
    for j = 0 to grid#width - 1 do
      xp_color print grid#matrix.{i,j}
    done;
    print#string "\n"
  done
and xp_blank print () =
  print#string "  "
and xp_color print c =
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
    | 10 -> [on_white], ". "
    | 11 -> [on_white], "XX"
    | _ -> invalid_arg ("Invalid color code: " ^ string_of_int c) in
  print#style_string style str

let pp_grids grids = Xprint.to_stdout xp_grids grids
let pp_grid grid = Xprint.to_stdout xp_grid grid
let pp_color c = Xprint.to_stdout xp_color c

(* comparing grids *)

let same (g1 : t) (g2 : t) : bool = (g1#matrix = g2#matrix)

let compatible (g1 : t) (g2 : t) : bool = (* QUICK *)
  dims g1 = dims g2
  && for_all2_pixels
       (fun i j c1 c2 ->
         c1 = c2 || c1 = undefined || c2 = undefined)
       g1 g2
                                  
type diff =
  | Grid_size_mismatch of { src_height: int; src_width: int;
			    tgt_height: int; tgt_width: int }
  | Grid_diff_pixels of { height: int; width: int; pixels: pixel list }
							  
let diff (source : t) (target : t) : diff option = (* QUICK *)
  if source#height <> target#height || source#width <> target#width
  then Some (Grid_size_mismatch
	       { src_height = source#height;
		 src_width = source#width;
		 tgt_height = target#height;
		 tgt_width = target#width })
  else
    let height = source#height in
    let width = source#width in
    let res = ref [] in
    for i = 0 to height - 1 do
      for j = 0 to width - 1 do
        let src_c = get_pixel ~source:"diff/1" source i j in
        let tgt_c = get_pixel ~source:"diff/2" target i j in
	if src_c <> tgt_c then
	  res := (i,j,tgt_c) :: !res
      done
    done;
    if !res = []
    then None
    else Some (Grid_diff_pixels {height; width; pixels=(!res)})


(* operations on grids *)

module Transf =
  struct

    (* coloring *)

    let swap_colors g c1 c2 : t result =
      if g#color_count.(c1) = 0
      then Result.Error (Undefined_result "swap_colors: none of the color present")
      else (
        let res =
          map_pixels
            (fun c ->
              if c = c1 then c2
              else if c = c2 then c1
              else c)
            g in
        Result.Ok res)
    let swap_colors, reset_swap_colors =
      Memo.memoize3 ~size:memoize_size swap_colors
    
    (* isometric transformations *)

    let flipHeight g =
      let h, w = dims g in
      init h w
        (fun i j -> get_pixel ~source:"flipHeight" g (h - 1 - i) j)
    let flipHeight, reset_flipHeight =
      Memo.memoize ~size:memoize_size flipHeight
                     
    let flipWidth g =
      let h, w = dims g in
      init h w
        (fun i j -> get_pixel ~source:"flipWidth" g i (w - 1 - j))
    let flipWidth, reset_flipWidth =
      Memo.memoize ~size:memoize_size flipWidth

    let flipDiag1 g =
      let h, w = dims g in
      init w h
        (fun i j -> get_pixel ~source:"flipDiag1" g j i)
    let flipDiag1, reset_flipDiag1 =
      Memo.memoize ~size:memoize_size flipDiag1

    let flipDiag2 g =
      let w', h' = dims g in
      init h' w'
        (fun i j -> get_pixel ~source:"flipDiag2" g (w' - 1 - j) (h' - 1 - i))
    let flipDiag2, reset_flipDiag2 =
      Memo.memoize ~size:memoize_size flipDiag2

    let rotate90 g = (* clockwise *)
      let w', h' = dims g in
      init h' w'
        (fun i j -> get_pixel ~source:"rotate90" g (w' - 1 - j) i)
    let rotate90, reset_rotate90 =
      Memo.memoize ~size:memoize_size rotate90
      
    let rotate180 g = (* clockwise *)
      let h, w = dims g in
      init h w
        (fun i j -> get_pixel ~source:"rotate280" g (h - 1 - i) (w - 1 - j))
    let rotate180, reset_rotate180 =
      Memo.memoize ~size:memoize_size rotate180
      
    let rotate270 g = (* clockwise *)
      let w', h' = dims g in
      init h' w'
        (fun i j -> get_pixel ~source:"rotate270" g j (h' - 1 - i))
    let rotate270, reset_rotate270 =
      Memo.memoize ~size:memoize_size rotate270
      
    (* scaling *)
      
    let scale_up (k : int) (l : int) (g : t) : t result = (* scaling up grid [g] by a factor (k,l) *)
      let h, w = dims g in
      let h', w' = h * k, w * l in
      if h' > max_size || w' > max_size || h' <= 0 || w' <= 0
      then Result.Error (Undefined_result "scale_up: result grid too large or ill-formed")
      else (
        let res = make h' w' black (* or whatever color *) in
        g#iter_pixels
          (fun i j c ->
            for i' = k*i to k*(i+1)-1 do
              for j' = l*j to l*(j+1)-1 do
                res#set_pixel i' j' c
              done
            done);
        Result.Ok res)
    let scale_up, reset_scale_up =
      Memo.memoize3 ~size:memoize_size scale_up

    let scale_down (k : int) (l : int) (g : t) : t result = (* scaling down *)
      let h, w = dims g in
      if k > 0 && l > 0 && h mod k = 0 && w mod l = 0
      then (
        let ok = ref true in
        g#iter_pixels
          (fun i j c ->
            ok := !ok && get_pixel ~source:"scale_down/1" g ((i/k)*k) ((j/l)*l) = c);
        if !ok (* all pixels scaling down to a single pixel have same color *)
        then (
          let res =
            init (h / k) (w / l)
              (fun i j -> get_pixel ~source:"scale_down/2" g (i*k) (j*l)) in
          Result.Ok res )
        else Result.Error (Undefined_result "Grid.Transf.scale_down: grid not regular"))
      else Result.Error (Undefined_result "Grid.Transf.scale_down: dims and factors not congruent")
    let scale_down, reset_scale_down =
      Memo.memoize3 ~size:memoize_size scale_down

    let scale_to (new_h : int) (new_w : int) (g : t) : t result =
      if new_h > 0 && new_w > 0
      then
        let h, w = dims g in
        if new_h >= h && new_w >= w && new_h mod h = 0 && new_w mod w = 0 then
          scale_up (new_h / h) (new_w / w) g
        else if new_h <= h && new_w <= w && h mod new_h = 0 && w mod new_w = 0 then
          scale_down (h / new_h) (w / new_w) g
        else
          match color_freq_desc g with
          | [(n,c)] when n = h * w -> (* full mask model *)
             Result. Ok (make new_h new_w c)
          | _ -> Result.Error (Undefined_result "Grid.Transf.scale_to: invalid scaling factor")
      else
        Result.Error (Undefined_result "Grid.Transf.scale_to: negative scaling vector")
    let scale_to, reset_scale_to =
      Memo.memoize3 ~size:memoize_size scale_to
      
    (* resize and factor *)

    let tile (k : int) (l : int) g : t result = (* k x l tiling of g *)
      let h, w = dims g in
      let h', w' = h * k, w * l in
      if h' > max_size || w' > max_size
      then Result.Error (Undefined_result "tile: result grid too large")
      else (
        let res = make h' w' black (* or whatever color *) in
        g#iter_pixels
          (fun i j c ->
            for u = 0 to k-1 do
              for v = 0 to l-1 do
                res#set_pixel (u*h + i) (v*w + j) c
              done
            done);
        Result.Ok res)
    let tile, reset_tile =
      Memo.memoize3 ~size:memoize_size tile

(* deprecated
    let factor (g : t) : int * int = (* finding the smallest h' x w' repeating factor of m *)
      let rec range a b =
        if a > b then []
        else a :: range (a+1) b
      in
      let h, w = dims g in
      let h_factors = ref (range 1 (h-1)) in
      let w_factors = ref (range 1 (w-1)) in
      for i = 0 to h-1 do
        for j = 0 to w-1 do
          h_factors :=
            !h_factors
            |> List.filter (fun h' ->
                   get_pixel ~source:"factor/1" g i j
                   = get_pixel ~source:"factor/2" g (i mod h') j);
          w_factors :=
            !w_factors
            |> List.filter (fun w' ->
                   get_pixel ~source:"factor/3" g i j
                   = get_pixel g i (j mod w'))
        done
      done;
      let h' = match !h_factors with [] -> h | h'::_ -> h' in
      let w' = match !w_factors with [] -> w | w'::_ -> w' in
      (h', w')
    let factor, reset_factor =
      Memo.memoize ~size:memoize_size factor

    let resize_alike (new_h : int) (new_w : int) (g : t) : t result = (* change size while preserving the repeating pattern *)
      assert (new_h > 0 && new_w > 0);
      let h', w' = factor g in
      if new_h > max_size || new_w > max_size
      then Result.Error (Undefined_result "resize_alike: result grid too large")
      else (
        let res = make new_h new_w black in
        for i' = 0 to h' - 1 do (* for each position in the factor *)
          for j' = 0 to w' - 1 do
            let c = get_pixel ~source:"resize_alike" g i' j' in
            if c <> black then (* when pixel not black *)
              for u = 0 to (new_h - 1) / h' + 1 do
                for v = 0 to (new_w - 1) / w' + 1 do
                  let i, j = u*h' + i', v*w' + j' in
                  if i < new_h && j < new_w then
                    set_pixel res i j c
                done
              done
          done
        done;
        Result.Ok res)
    let resize_alike, reset_resize_alike =
      Memo.memoize3 ~size:memoize_size resize_alike
 *)
      
    type axis = I | J | PlusIJ | DiffIJ | MaxIJ | MinIJ | DivIJ (* avoid TimesIJ whose bound is too high *)
    type period = axis * int (* axis(i,j) mod p: defines p equivalence classes *)
    type periodicity =
      | Period1 of period * (int * int * color) array
      | Period2 of period * period * (int * int * color) array array
    (* array elements are triples k:(nc,n,c) where [c] is the color of
       equivalence class [k], [n] is the number of pixels in that
       equivalence class, and [nc] the number of such pixels
       exhibiting color [c] (the (n-nc) others have background color)
       *)

    let xp_axis (print : Xprint.t) = function
      | I -> print#string "i"
      | J -> print#string "j"
      | PlusIJ -> print#string "i+j"
      | DiffIJ -> print#string "|i-j|"
      | MaxIJ -> print#string "max(i,j)"
      | MinIJ -> print#string "min(i,j)"
      | DivIJ -> print#string "max(i,j)/min(i,j)"
      
    let eval_axis : axis -> (int -> int -> int) = function
      | I -> (fun i j -> i)
      | J -> (fun i j -> j)
      | PlusIJ -> (fun i j -> i+j)
      | DiffIJ -> (fun i j -> max i j - min i j)
      | MaxIJ -> (fun i j -> max i j)
      | MinIJ -> (fun i j -> min i j)
      | DivIJ -> (fun i j -> (max i j + 1) / (min i j + 1))

    let bound_axis : axis -> (int -> int -> int) = function (* max value for axis in hxw grid *)
      | I -> (fun h w -> h)
      | J -> (fun h w -> w)
      | PlusIJ -> (fun h w -> h+w)
      | DiffIJ -> (fun h w -> max h w - 0)
      | MaxIJ -> (fun h w -> max h w)
      | MinIJ -> (fun h w -> min h w)
      | DivIJ -> (fun h w -> (max h w + 1) / 1)

    let xp_period (print : Xprint.t) (axis,p) =
      xp_axis print axis; print#string " mod "; print#int p
    let pp_period = Xprint.to_stdout xp_period

    let periodicity_is_total : periodicity -> bool = function
      | Period1 (_,k_ar) ->
         Array.for_all (fun (nc,n,c) ->
             nc <> 0)
           k_ar
      | Period2 (_,_,k2_ar) ->
         Array.for_all (fun k_ar ->
             Array.for_all (fun (nc,n,c) ->
                 nc <> 0)
               k_ar)
           k2_ar

    let periodicity_is_strict : periodicity -> bool = function
      | Period1 (_,k_ar) ->
         Array.for_all (fun (nc,n,c) ->
             nc = 0 || nc = n)
           k_ar
      | Period2 (_,_,k2_ar) ->
         Array.for_all (fun k_ar ->
             Array.for_all (fun (nc,n,c) ->
                 nc = 0 || nc = n)
               k_ar)
           k2_ar

    let xp_periodicity (print : Xprint.t) (per: periodicity) =
      let total = periodicity_is_total per in
      ( match per with
      | Period1 (period1,k_ar) ->
         xp_period print period1
      | Period2 (period1,period2,_) ->
         xp_period print period1;
         print#string " x ";
         xp_period print period2);
      print#string (if total then " (total)" else " (partial)")
    let pp_periodicity = Xprint.to_stdout xp_periodicity

    let grid_of_periodicity (bgcolor : color) : periodicity -> t = function
      | Period1 ((axis,p),k_ar) ->
         init 1 p
           (fun _i j ->
             let _, _, c_k = k_ar.(j) in
             if c_k = undefined then bgcolor else c_k)
      | Period2 ((axis1,p1),(axis2,p2),k2_ar) ->
         init p1 p2
           (fun i j ->
             let _, _, c_k = k2_ar.(i).(j) in
             if c_k = undefined then bgcolor else c_k)                       
                  
    let all_axis : axis list = [I; J; PlusIJ; DiffIJ; MaxIJ; MinIJ; DivIJ]
    let all_axis_pairs : (axis * axis) list =
      let rec aux = function
        | [] -> []
        | axis1::next ->
           let res = aux next in
           List.fold_left
             (fun res axis2 -> (axis1,axis2)::res)
             res next
      in
      aux all_axis
                  
    let periodicities (bgcolor : color) (g : t) : periodicity list =
      let rec range a b =
        if a > b then []
        else a :: range (a+1) b in
      let product la lb =
        List.fold_left
          (fun res a ->
            List.fold_left
              (fun res b ->
                (a,b)::res)
              res lb)
          [] la
      in
      let h, w = dims g in
      (* initialization *)
      let periods1 = ref(
        List.map
          (fun axis ->
            let f_axis = eval_axis axis in
            let max_axis = bound_axis axis h w in
            let p_map =
              List.map
                (fun p ->
                  let k_ar = Array.make p (0,0,undefined) in
                  (p, k_ar))
                (range 2 max_axis) in
            (axis, f_axis, ref p_map))
          all_axis) in
      let periods2 = ref(
        List.map
          (fun (axis1,axis2) ->
            let f_axis1 = eval_axis axis1 in
            let max_axis1 = bound_axis axis1 h w in
            let f_axis2 = eval_axis axis2 in
            let max_axis2 = bound_axis axis2 h w in
            let p2_map =
              List.map
                (fun (p1,p2) ->
                  let k2_ar = Array.make_matrix p1 p2 (0,0,undefined) in
                  (p1, p2, k2_ar))
                (product (range 2 max_axis1) (range 2 max_axis2)) in
            (axis1, f_axis1, axis2, f_axis2, ref p2_map))
          all_axis_pairs) in
      (* filtering through one pass of the grid pixels *)
      g#iter_pixels
        (fun i j c ->
          periods1 :=
            List.filter
              (fun (axis,f_axis,ref_p_map) ->
                let pre_k = f_axis i j in
                let p_map =
                  List.filter
                    (fun (p,k_ar) ->
                      let k = pre_k mod p in
                      let nc, n, c_k = k_ar.(k) in
                      if c = bgcolor then (k_ar.(k) <- (nc,n+1,c_k); true)
                      else if c_k = undefined then (k_ar.(k) <- (1,n+1,c); true)
                      else if c_k = c then (k_ar.(k) <- (nc+1,n+1,c_k); true)
                      else (* inconsistency *) false)
                    !ref_p_map in
                if p_map = [] (* no period for this axis *)
                then false
                else (ref_p_map := p_map; true))
              !periods1;
          periods2 :=
            List.filter
              (fun (axis1,f_axis1,axis2,f_axis2,ref_p2_map) ->
                let pre_k1 = f_axis1 i j in
                let pre_k2 = f_axis2 i j in
                let p2_map =
                  List.filter
                    (fun (p1,p2,k2_ar) ->
                      let k1 = pre_k1 mod p1 in
                      let k2 = pre_k2 mod p2 in
                      let nc, n, c_k = k2_ar.(k1).(k2) in
                      if c = bgcolor then (k2_ar.(k1).(k2) <- (nc,n+1,c_k); true)
                      else if c_k = undefined then (k2_ar.(k1).(k2) <- (1,n+1,c); true)
                      else if c_k = c then (k2_ar.(k1).(k2) <- (nc+1,n+1,c_k); true)
                      else (* inconsistency *) false)
                    !ref_p2_map in
                if p2_map = [] (* no period for those axis *)
                then false
                else (ref_p2_map := p2_map; true))
              !periods2);
      (* collecting results *)
      let res =
        List.fold_left
          (fun res (axis,f_axis,ref_p_map) ->
            List.fold_left
              (fun res (p,k_ar) ->
                if Array.exists (fun (_,n,_) -> n=0) k_ar (* some equiv class is empty in g *)
                then res
                else
                  let diffs =
                    Array.fold_left
                      (fun res (nc,n,c) ->
                        if c = undefined then res else res + (n-nc))
                      0 k_ar in
                  let cost = p + diffs in
                  (cost, Period1 ((axis,p),k_ar))::res)
              res !ref_p_map)
          [] !periods1 in
      let res =
        List.fold_left
          (fun res (axis1,f_axis1,axis2,f_axis2,ref_p2_map) ->
            List.fold_left
              (fun res (p1,p2,k2_ar) ->
                if Array.exists (fun k_ar ->
                       Array.exists (fun (_,n,_) ->
                           n=0)
                         k_ar)
                     k2_ar (* some equiv class is empty in g *)
                   || Array.for_all (fun k_ar ->
                          Array.for_all2 (fun (_,_,c1) (_,_,c2) -> c1=c2) k2_ar.(0) k_ar (* axis2 is redundant *)
                          || let _, _, c0 = k_ar.(0) in
                             Array.for_all (fun (_,_,c) -> c=c0) k_ar) (* axis1 is redundant *)
                        k2_ar
                then res
                else
                  let diffs =
                    Array.fold_left
                      (fun res k_ar ->
                        Array.fold_left
                          (fun res (nc,n,c) ->
                            if c = undefined then res else res + (n-nc))
                          res k_ar)
                      0 k2_ar in
                  let cost = p1*p2 + diffs in
                  (cost, Period2 ((axis1,p1),(axis2,p2),k2_ar))::res)
              res !ref_p2_map)
          res !periods2 in
      let res = (* sorting by increasing cost *)
        List.sort
          (fun period1 period2 -> Stdlib.compare period1 period2)
          res in
      List.map snd res
    let periodicities, reset_periodicities =
      Memo.memoize2 ~size:memoize_size periodicities

(*    let _ = (* for testing function periodicities on concrete grids *)
      let k_ar = [|1;2;3;4;5|] in
      let g = init 5 5 (fun i j -> k_ar.(max i j mod 5)) in
      print_endline "*** PERIODICITIES ***";
      periodicities 0 g
      |> List.iter (fun per -> pp_periodicity per; print_newline ()) *)
      
    type periodicity_mode = [`Total | `Strict | `TradeOff]

    let find_periodicity (mode : periodicity_mode) (bgcolor : color) (g : t) : periodicity option =
      let periods = periodicities bgcolor g in
      match periods with
      | [] -> None
      | period0::next ->
         match mode with
         | `Total ->
            if periodicity_is_total period0
            then Some period0
            else List.find_opt periodicity_is_total next
         | `Strict ->
            if periodicity_is_strict period0
            then Some period0
            else List.find_opt periodicity_is_strict next
         | `TradeOff ->
            Some period0

    let periodic_factor (mode : periodicity_mode) (bgcolor : color) (g : t) : t result =
      match find_periodicity mode bgcolor g with
      | None -> Result.Error (Undefined_result "Grid.Transf.periodic_factor: no adequate periodicity")
      | Some period -> Result.Ok (grid_of_periodicity bgcolor period)
    let periodic_factor, reset_periodic_factor =
      Memo.memoize3 ~size:memoize_size periodic_factor
           
    let fill_and_resize_with_periodicity (bgcolor : color) (new_h, new_w : int * int) (g : t) (period : periodicity) : t =
      assert (new_h > 0 && new_w > 0);
      match period with
      | Period1 ((axis,p),k_ar) ->
         let f_axis = eval_axis axis in
         init new_h new_w
           (fun i j ->
             let _, _, c_k = k_ar.(f_axis i j mod p) in
             if c_k = undefined then bgcolor else c_k)
      | Period2 ((axis1,p1),(axis2,p2),k2_ar) ->
         let f_axis1 = eval_axis axis1 in
         let f_axis2 = eval_axis axis2 in
         init new_h new_w
           (fun i j ->
             let _, _, c_k = k2_ar.(f_axis1 i j mod p1).(f_axis2 i j mod p2) in
             if c_k = undefined then bgcolor else c_k)

    let fill_and_resize_alike (mode : periodicity_mode) (bgcolor : color) (new_size : int * int) (g : t) : t result =
      match find_periodicity mode bgcolor g with
      | None -> Result.Error (Undefined_result "Grid.Transf.fill_alike: no adequate periodicity found")
      | Some period ->
         let g' = fill_and_resize_with_periodicity bgcolor new_size g period in
         Result.Ok g'
    let fill_and_resize_alike, reset_fill_and_resize_alike =
      Memo.memoize4 ~size:memoize_size fill_and_resize_alike
      
    (* cropping *)

    let crop g offset_i offset_j new_h new_w =
      let h, w = dims g in
      if offset_i >= 0 && offset_i < h
         && offset_j >= 0 && offset_j < w
         && new_h > 0 && new_w > 0
         && offset_i + new_h <= h
         && offset_j + new_w <= w
      then
        let res =
          init new_h new_w
            (fun i j -> get_pixel ~source:"crop" g (offset_i + i) (offset_j + j)) in
        Result.Ok res
      else Result.Error (Undefined_result "Grid.Transf.crop")
    let crop, reset_crop =
      let f, reset =
        Memo.memoize ~size:memoize_size (fun (g,i,j,h,w) -> crop g i j h w) in
      (fun g i j h w -> f (g,i,j,h,w)), reset

    let strip (bgcolor : color) (g : t) (out_bgcolor : color) : t result = (* croping on anything else than bgcolor, the remaining bgcolor is made out_bgcolor *)
      let h, w = dims g in
      let min_i, max_i = ref h, ref (-1) in
      let min_j, max_j = ref w, ref (-1) in
      g#iter_pixels
        (fun i j c ->
          if c <> bgcolor then (
            min_i := min i !min_i;
            max_i := max i !max_i;
            min_j := min j !min_j;
            max_j := max j !max_j));
      if !min_i < 0 (* grid is bgcolor only *)
      then Result.Error (Undefined_result "grid has no contents")
      else
        let| g' = swap_colors g bgcolor out_bgcolor in
        let| g' = crop g' !min_i !min_j (!max_i - !min_i + 1) (!max_j - !min_j + 1) in
        Result.Ok g'
    let strip, reset_strip =
      Memo.memoize3 ~size:memoize_size strip
      
    (* concatenating *)
      
    let concatHeight g1 g2 : t result =
      let h1, w1 = dims g1 in
      let h2, w2 = dims g2 in
      if w1 <> w2 then Result.Error Invalid_dim
      else if h1+h2 > max_size then Result.Error (Undefined_result "concatHeight: result grid too large")
      else (
        let res = make (h1+h2) w1 black (* or whatever color *) in
        g1#iter_pixels
          (fun i1 j1 c1 -> res#set_pixel i1 j1 c1);
        g2#iter_pixels
          (fun i2 j2 c2 -> res#set_pixel (h1+i2) j2 c2);
        Result.Ok res)
    let concatHeight, reset_concatHeight =
      Memo.memoize2 ~size:memoize_size concatHeight
      
    let concatWidth g1 g2 : t result =
      let h1, w1 = dims g1 in
      let h2, w2 = dims g2 in
      if h1 <> h2 then Result.Error Invalid_dim
      else if w1+w2 > max_size then Result.Error (Undefined_result "concatWidth: result grid too large")
      else (
        let res = make h1 (w1+w2) black (* or whatever color *) in
        g1#iter_pixels
          (fun i1 j1 c1 -> res#set_pixel i1 j1 c1);
        g2#iter_pixels
          (fun i2 j2 c2 -> res#set_pixel i2 (w1+j2) c2);
        Result.Ok res)
    let concatWidth, reset_concatWidth =
      Memo.memoize2 ~size:memoize_size concatWidth

    let concatHeightWidth g1 g2 g3 g4 : t result (* top left, top right, bottom left, bottom right *) =
      let| g12 = concatWidth g1 g2 in
      let| g34 = concatWidth g3 g4 in
      concatHeight g12 g34

    (* TODO: selecting halves and quarters *)

    let compose (c1_mask : color) (g1 : t) (g2 : t) : t result = (* repeating g2 for each pixel of g1 that has color bgcolor1 *)
      let h1, w1 = dims g1 in
      let h2, w2 = dims g2 in
      let h, w = h1*h2, w1*w2 in
      if h > max_size || w > max_size
      then Result.Error (Undefined_result "compose: result grid too large")
      else (
        let res = make h w black (* or whatever color *) in
        g1#iter_pixels
          (fun i1 j1 c1 ->
            if c1 = c1_mask then
              g2#iter_pixels
                (fun i2 j2 c2 ->
                  if c2 <> black then
                    res#set_pixel (i1*h2+i2) (j1*w2+j2) c2));
        Result.Ok res)
    let compose, reset_compose =
      Memo.memoize3 ~size:memoize_size compose

    (* symmetrization *)

    let layers (bgcolor : color) gs : t result =
      match gs with
      | [] -> Result.Error (Invalid_argument "Grid.Transf.layers: empty list")
      | g1::gs1 ->
         let h1, w1 = dims g1 in
         if List.for_all (fun gi -> dims gi = (h1,w1)) gs1
         then (
           let res = make h1 w1 bgcolor in
           List.iter
             (fun gi ->
               gi#iter_pixels
                 (fun i j c ->
                   if c <> bgcolor then res#set_pixel i j c))
             gs;
           Result.Ok res)
         else Result.Error Invalid_dim
    let layers, reset_layers =
      Memo.memoize2 ~size:memoize_size layers
      
    let sym_flipHeight_inplace bgcolor g = layers bgcolor [g; flipHeight g]
    let sym_flipWidth_inplace bgcolor g = layers bgcolor [g; flipWidth g]
    let sym_rotate180_inplace bgcolor g = layers bgcolor [g; rotate180 g]
    let sym_flipHeightWidth_inplace bgcolor g = layers bgcolor [g; flipHeight g; flipWidth g; rotate180 g]
    let sym_flipDiag1_inplace bgcolor g =
      if g#height <> g#width
      then Result.Error Invalid_dim
      else layers bgcolor [g; flipDiag1 g]
    let sym_flipDiag2_inplace bgcolor g =
      if g#height <> g#width
      then Result.Error Invalid_dim
      else layers bgcolor [g; flipDiag2 g]
    let sym_flipDiag1Diag2_inplace bgcolor g =
      if g#height <> g#width
      then Result.Error Invalid_dim
      else layers bgcolor [g; flipDiag1 g; flipDiag2 g; rotate180 g]
    let sym_rotate90_inplace bgcolor g =
      if g#height <> g#width
      then Result.Error Invalid_dim
      else layers bgcolor [g; rotate90 g; rotate180 g; rotate270 g]
    let sym_full_inplace bgcolor g =
      if g#height <> g#width
      then Result.Error Invalid_dim
      else (* includes all symmetries *)
        let| g' = layers bgcolor [g; rotate90 g] in
        Result.Ok (flipHeight g')

(* not ready for use
    let sym_flipHeight_unfold g =
      let g'= flipHeight g in
      [ concatHeight g g'; concatHeight g' g ]
    let sym_flipWidth_unfold g =
      let g'= flipWidth g in
      [ concatWidth g g'; concatWidth g' g ]
    let sym_rotate180_unfold g =
      let g' = rotate180 g in
      [ concatHeight g g'; concatHeight g' g;
        concatWidth g g'; concatWidth g' g ]
    let sym_flipHeightWidth_unfold g =
      let g'= flipWidth g in
      let g1 = concatWidth g g' in
      let g2 = concatWidth g' g in
      let g1' = flipHeight g1 in
      let g2' = flipHeight g2 in
      [ concatHeight g1 g1'; concatHeight g1' g1;
        concatHeight g2 g2'; concatHeight g2' g2 ]
    let sym_rotate90_unfold g =
      if g#height <> g#width then raise Invalid_dim;
      let g90 = rotate90 g in
      let g180 = rotate180 g in
      let g270 = rotate270 g in
      [ concatHeightWidth g g90 g270 g180;
        concatHeightWidth g270 g g180 g90;
        concatHeightWidth g180 g270 g90 g;
        concatHeightWidth g90 g180 g g270 ]
 *)

    let reset_memoized_functions () =
      reset_swap_colors ();
      reset_flipHeight ();
      reset_flipWidth ();
      reset_flipDiag1 ();
      reset_flipDiag2 ();
      reset_rotate90 ();
      reset_rotate180 ();
      reset_rotate270 ();
      reset_scale_up ();
      reset_scale_down ();
      reset_scale_to ();
      reset_tile ();
(*      reset_factor ();
      reset_resize_alike (); *)
      reset_periodicities ();
      reset_periodic_factor ();
      reset_fill_and_resize_alike ();
      reset_crop ();
      reset_strip ();
      reset_concatHeight ();
      reset_concatWidth ();
      reset_compose ();
      reset_layers ()
      
  end
  

(* masks: monocolor grids *)

module Mask =
  struct
    (* color coding of Boolean values *)
    let zero = transparent
    let one = black
    let bool (b : bool) : color = if b then one else zero
    
    let area m = m#color_count.(one) [@@inline]

    let empty height width = make height width zero
    let full height width = make height width one
    let singleton height width i j =
      let m = make height width zero in
      m#set_pixel i j one;
      m
    let init height width pred =
      init height width
        (fun i j -> if pred i j then one else zero)
      
    let equal m1 m2 = (m1 = m2) [@@inline]
    let is_empty m =
      for_all_pixels (fun i j c -> c = zero) m
    let is_subset m1 m2 =
      for_all_pixels (fun i j c1 ->
          let c2 = get_pixel ~source:"is_subset" m2 i j in
          c1 = zero ||  c2 = one)
        m1
    let inter_is_empty m1 m2 =
      for_all_pixels (fun i j c1 ->
          let c2 = get_pixel ~source:"inter_is_empty" m2 i j in
          c1 = zero || c2 = zero)
        m1

    let mem i j m =
      i >= 0 && i < m#height
      && j >= 0 && j < m#width
      && get_pixel m i j = one [@@inline]

    let union (m1 : t) (m2 : t) : t =
      map2_pixels (fun c1 c2 -> bool (c1=one || c2=one)) m1 m2
    let union, reset_union =
      Memo.memoize2 ~size:memoize_size union
      
    let inter (m1 : t) (m2 : t) : t =
      map2_pixels (fun c1 c2 -> bool (c1=one && c2=one)) m1 m2
    let inter, reset_inter =
      Memo.memoize2 ~size:memoize_size inter
      
    let diff (m1 : t) (m2 : t) : t =
      map2_pixels (fun c1 c2 -> bool (c1=one && c2=zero)) m1 m2
    let diff, reset_diff =
      Memo.memoize2 ~size:memoize_size diff
      
    let diff_sym (m1 : t) (m2 : t) : t =
      map2_pixels (fun c1 c2 -> bool (c1 <> c2)) m1 m2
    let diff_sym, reset_diff_sym =
      Memo.memoize2 ~size:memoize_size diff_sym
      
    let compl (m : t) : t =
      map_pixels (fun c -> bool (c=zero)) m
    let compl, reset_compl =
      Memo.memoize ~size:memoize_size compl      

    let fold f acc m =
      fold_pixels
        (fun acc i j c -> if c = one then f acc i j else acc)
        acc m

    let iter f m =
      m#iter_pixels
        (fun i j c -> if c = one then f i j)

    let from_bitmap (bmp : Bitmap.t) : t =
      let m = empty (Bitmap.height bmp) (Bitmap.width bmp) in
      Bitmap.iter
        (fun i j -> m#set_pixel i j one)
        bmp;
      m
    let from_bitmap, reset_from_bitmap =
      Memo.memoize ~size:memoize_size from_bitmap
          
    let from_grid_background (bgcolor : color) (g : t) : t =
      (* returns a non-bgcolor mask version of the grid *)
      let h, w = dims g in
      let res = empty h w in
      g#iter_pixels
        (fun i j c ->
          if c <> bgcolor then
            res#set_pixel i j one);
      res
    let from_grid_background, reset_from_grid_background =
      Memo.memoize2 ~size:memoize_size from_grid_background

    let from_grid_color (c_mask : color) (g : t) : t =
      (* returns a non-bgcolor mask version of the grid *)
      let h, w = dims g in
      let res = empty h w in
      g#iter_pixels
        (fun i j c ->
          if c = c_mask then
            res#set_pixel i j one);
      res
    let from_grid_color, reset_from_grid_color =
      Memo.memoize2 ~size:memoize_size from_grid_color

    let to_grid (m : t) (bgcolor : color) (c : color) : t =
      (* return a colored grid version of a mask *)
      let h, w = dims m in
      let res = make h w bgcolor in
      iter
        (fun i j ->
          res#set_pixel i j c)
        m;
      res
    let to_grid, reset_to_grid =
      Memo.memoize3 ~size:memoize_size to_grid

    let to_string m =
      let h, w = dims m in
      let bytes = Bytes.create (h * w + h - 1) in
      let pos = ref 0 in
      for i = 0 to h - 1 do
        if i > 0 then (
          Bytes.set bytes !pos '|';
          incr pos
        );
        for j = 0 to w - 1 do
          if mem i j m
          then Bytes.set bytes !pos 'x'
          else Bytes.set bytes !pos '.';
          incr pos
        done
      done;
      Bytes.to_string bytes
    let pp m = print_string (to_string m)

    let reset_memoized_functions () =
      reset_union ();
      reset_inter ();
      reset_diff ();
      reset_diff_sym ();
      reset_compl ();
      reset_from_bitmap ();
      reset_from_grid_background ();
      reset_from_grid_color ();
      reset_to_grid ()
             
  end

let reset_memoized_functions () =
  reset_fill_undefined ();
  Transf.reset_memoized_functions ();
  Mask.reset_memoized_functions ()
