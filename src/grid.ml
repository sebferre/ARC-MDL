
open Bigarray

exception Undefined_result of string (* for undefined computations *)

type 'a result = ('a,exn) Result.t
                            
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

exception Invalid_dim (* height/width are invalid for the operation *)
           
module type MaskCore =
  sig
    type t
       
    val height : t -> int
    val width : t -> int
    val area : t -> int

    val empty : int -> int -> t
    val full : int -> int -> t
    val singleton : int -> int -> int -> int -> t

    val equal : t -> t -> bool
    val is_empty : t -> bool
    val is_subset : t -> t -> bool

    val mem : int -> int -> t -> bool
    val add : int -> int -> t -> t
    val remove : int -> int -> t -> t
      
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val diff_sym : t -> t -> t
    val compl : t -> t

    val iter : (int -> int -> unit) -> t -> unit
    val fold : ('a -> int -> int -> 'a) -> 'a -> t -> 'a

  end

module MaskCoreBitmap : MaskCore =
  (* SEE mask_core_alt.ml for alternative implementations *)
  (* only MaskCoreZ is a bit more efficient in native code but catastrophic in JS code *)
  struct
    module Bmp = Intmap.Bitmap_base
       
    type t = { height : int;
               width : int;
               bits : (int, int_elt, c_layout) Array1.t }

    let base = Intmap.base
    let array_size height width = (* k bound, l bound for last k *)
      let size = height * width in
      let m = (size + base - 1) / base in
      let n = base - (m * base - size) in
      m, n [@@inline]
    let offsets width i j = (* array index, bit index *)
      let h = i * width + j in
      h / base, h mod base [@@inline]
    let coords width k l = (* i, j *)
      let h = k * base + l in
      h / width, h mod width [@@inline]
           
    let height m = m.height [@@inline]
    let width m = m.width [@@inline]

    let empty height width =
      let m, _n = array_size height width in
      let bits = Array1.create Int C_layout m in
      Array1.fill bits Bmp.empty;
      { height; width; bits }
    let full height width =
      let m, n = array_size height width in
      let bits = Array1.create Int C_layout m in
      Array1.fill bits Bmp.full;
      if n < base then Array1.set bits (m-1) (1 lsl n - 1);
      { height; width; bits }
    let singleton height width i j =
      let m, _n = array_size height width in
      let bits = Array1.create Int C_layout m in
      Array1.fill bits Bmp.empty;
      let k, l = offsets width i j in
      Array1.set bits k (Bmp.singleton l);
      { height; width; bits }
      
    let bits_copy bits =
      let bits' = Array1.create Int C_layout (Array1.dim bits) in
      Array1.blit bits bits';
      bits' [@@inline]
    let bits_for_all f bits =
      let dim = Array1.dim bits in
      let rec aux k =
        k >= dim
        || f k (Array1.get bits k) && aux (k+1)
      in
      aux 0
    let bits_fold f acc bits =
      let dim = Array1.dim bits in
      let rec aux acc k =
        if k >= dim
        then acc
        else
          let acc = f acc k (Array1.get bits k) in
          aux acc (k+1)
      in
      aux acc 0
    let bits_iter f bits =
      let dim = Array1.dim bits in
      for k = 0 to dim-1 do
        f k (Array1.get bits k)
      done
    let bits_map f bits =
      let dim = Array1.dim bits in
      let res = Array1.create Int C_layout dim in
      for k = 0 to dim-1 do
        Array1.set res k (f k (Array1.get bits k))
      done;
      res [@@inline]
    let bits_map2 f bits1 bits2 =
      let dim1 = Array1.dim bits1 in
      let res = Array1.create Int C_layout dim1 in
      for k = 0 to dim1 - 1 do
        Array1.set res k (f (Array1.get bits1 k) (Array1.get bits2 k))
      done;
      res [@@inline]
      
    let equal m1 m2 = (m1 = m2)
    let is_empty m =
      bits_for_all (fun k bmp -> Bmp.is_empty bmp) m.bits
    let is_subset m1 m2 =
      bits_for_all (fun k bmp -> Bmp.subset bmp (Array1.get m2.bits k)) m1.bits

    let mem i j m =
      i >= 0 && i < m.height
      && j >= 0 && j < m.width
      && (let k, l = offsets m.width i j in
          Bmp.mem l (Array1.get m.bits k))
    let add i j m =
      let bits' = bits_copy m.bits in
      let k, l = offsets m.width i j in
      Array1.set bits' k (Bmp.add l (Array1.get m.bits k));
      { m with bits = bits'}
    let remove i j m =
      let bits' = bits_copy m.bits in
      let k, l = offsets m.width i j in
      Array1.set bits' k (Bmp.remove l (Array1.get m.bits k));
      { m with bits = bits'}

    let area m =
      bits_fold
        (fun res k bmp -> res + Bmp.cardinal bmp)
        0 m.bits

    let union m1 m2 =
      { m1 with bits = bits_map2 (fun bmp1 bmp2 -> bmp1 lor bmp2) m1.bits m2.bits }
    let inter m1 m2 =
      { m1 with bits = bits_map2 (fun bmp1 bmp2 -> bmp1 land bmp2) m1.bits m2.bits }
    let diff m1 m2 =
      { m1 with bits = bits_map2 (fun bmp1 bmp2 -> bmp1 land (lnot bmp2)) m1.bits m2.bits }
    let diff_sym m1 m2 =
      { m1 with bits = bits_map2 (fun bmp1 bmp2 -> bmp1 lxor bmp2) m1.bits m2.bits }
    let compl m = diff (full m.height m.width) m

    let fold f acc m =
      let width = m.width in
      bits_fold
        (fun acc k bmp ->
          if bmp = 0
          then acc
          else
            Bmp.fold
              (fun acc l ->
                let i, j = coords width k l in
                f acc i j)
              acc bmp)
        acc m.bits

    let iter f m =
      let width = m.width in
      let dim = Array1.dim m.bits in
      for k = 0 to dim-1 do
        let bmp = Array1.get m.bits k in
        if bmp = 0
        then ()
        else
          Bmp.iter
            (fun l ->
              let i, j = coords width k l in
              f i j)
            bmp
      done
      
  end
  
module Mask =
  struct
    include MaskCoreBitmap
          
    let dims m = height m, width m [@@inline]
    let same_size m1 m2 = (dims m1 = dims m2)

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

    (* isometric transformations *)
      
    let flipHeight m =
      let h, w = dims m in
      let res = ref (empty h w) in
      iter
        (fun i j -> res := add (h - 1 - i) j !res)
        m;
      !res
      
    let flipWidth m =
      let h, w = dims m in
      let res = ref (empty h w) in
      iter
        (fun i j -> res := add i (w - 1 - j) !res)
        m;
      !res

    let flipDiag1 m =
      let h, w = dims m in
      let res = ref (empty w h) in
      iter
        (fun i j -> res := add j i !res)
        m;
      !res

    let flipDiag2 m =
      let h, w = dims m in
      let res = ref (empty w h) in
      iter
        (fun i j -> res := add (w - 1 - j) (h - 1 - i) !res)
        m;
      !res

    let rotate90 m = (* clockwise *)
      let h, w = dims m in
      let res = ref (empty w h) in
      iter
        (fun i j -> res := add j (h - 1 - i) !res)
        m;
      !res
      
    let rotate180 m = (* clockwise *)
      let h, w = dims m in
      let res = ref (empty h w) in
      iter
        (fun i j -> res := add (h - 1 - i) (w - 1 - j) !res)
        m;
      !res
      
    let rotate270 m = (* clockwise *)
      let h, w = dims m in
      let res = ref (empty w h) in
      iter
        (fun i j -> res := add (w - 1 - j) i !res)
        m;
      !res

    (* scaling *)
      
    let scale_up (k : int) (l : int) m = (* scaling up mask [m] by a factor (k,l) *)
      let res = ref (empty (height m * k) (width m * l)) in
      iter
        (fun i j ->
          for i' = k*i to k*(i+1)-1 do
            for j' = l*j to l*(j+1)-1 do
              res := add i' j' !res
            done
          done)
        m;
      !res

    let scale_down (k : int) (l : int) m = (* scaling down *)
      (* each resulting pixel is a OR of the corresponding source pixels *)
      let res = ref (empty (height m / k) (width m / l)) in
      iter
        (fun i j -> res := add (i/k) (j/l) !res)
        m;
      !res

    let scale_to (new_h : int) (new_w : int) (m : t) : t option =
      let h, w = dims m in
      if new_h >= h && new_w >= w && new_h mod h = 0 && new_w mod w = 0 then
        Some (scale_up (new_h / h) (new_w / w) m)
      else if new_h > 0 && new_w > 0 && new_h <= h && new_w <= w && h mod new_h = 0 && w mod new_w = 0 then
        Some (scale_down (h / new_h) (w / new_w) m)
      else None
        
    (* resize and factor *)

    let tile (k : int) (l : int) m = (* k x l tiling of m *)
      let h, w = dims m in
      let h', w' = h * k, w * l in
      let res = ref (empty h' w') in
      iter
        (fun i j ->
          for u = 0 to k-1 do
            for v = 0 to l-1 do
              res := add (u*h + i) (v*w + j) !res
            done
          done)
        m;
      !res

    let factor (m : t) : int * int = (* finding the smallest h' x w' repeating factor of m *)
      let rec range a b =
        if a > b then []
        else a :: range (a+1) b
      in
      let h, w = dims m in
      let h_factors = ref (range 1 (h-1)) in
      let w_factors = ref (range 1 (w-1)) in
      for i = 0 to h-1 do
        for j = 0 to w-1 do
          h_factors := !h_factors |> List.filter (fun h' -> mem i j m = mem (i mod h') j m);
          w_factors := !w_factors |> List.filter (fun w' -> mem i j m = mem i (j mod w') m)
        done
      done;
      let h' = match !h_factors with [] -> h | h'::_ -> h' in
      let w' = match !w_factors with [] -> w | w'::_ -> w' in
      (h', w')

    let resize_alike (m : t) (new_h : int) (new_w : int) : t = (* change size while preserving the repeating pattern *)
      assert (new_h > 0 && new_w > 0);
      let h', w' = factor m in
      let res = ref (empty new_h new_w) in
      for i' = 0 to h' - 1 do (* for each position in the factor *)
        for j' = 0 to w' - 1 do
          if mem i' j' m then (* when pixel on *)
            for u = 0 to (new_h - 1) / h' + 1 do
              for v = 0 to (new_w - 1) / w' + 1 do
                let i, j = u*h' + i', v*w' + j' in
                if i < new_h && j < new_w then
                  res := add i j !res
              done
            done
        done
      done;
      !res
      
    (* cropping and concatenating *)

    let crop m offset_i offset_j new_h new_w =
      let res = ref (empty new_h new_w) in
      for i = 0 to new_h - 1 do
        for j = 0 to new_w - 1 do
          if mem (offset_i + i) (offset_j + j) m then
            res := add i j !res
        done
      done;
      !res      

    let concatHeight m1 m2 =
      if width m1 <> width m2 then raise Invalid_dim;
      let h1, h2 = height m1, height m2 in
      let res = ref (empty (h1+h2) (width m1)) in
      iter
        (fun i1 j1 -> res := add i1 j1 !res)
        m1;
      iter
        (fun i2 j2 -> res := add (h1+i2) j2 !res)
        m2;
      !res
      
    let concatWidth m1 m2 =
      if height m1 <> height m2 then raise Invalid_dim;
      let w1, w2 = width m1, width m2 in
      let res = ref (empty (height m1) (w1+w2)) in
      iter
        (fun i1 j1 -> res := add i1 j1 !res)
        m1;
      iter
        (fun i2 j2 -> res := add i2 (w1+j2) !res)
        m2;
      !res

    let concatHeightWidth m1 m2 m3 m4 (* top left, top right, bottom left, bottom right *) =
      concatHeight
        (concatWidth m1 m2)
        (concatWidth m3 m4)

    (* TODO: selecting halves and quarters *)

    let compose m1 m2 = (* repeating m2 for each 1-bit of m1 *)
      let h1, w1 = dims m1 in
      let h2, w2 = dims m2 in
      let res = ref (empty (h1*h2) (w1*w2)) in
      iter
        (fun i1 j1 ->
          iter
            (fun i2 j2 ->
              res := add (i1*h2+i2) (j1*w2+j2) !res)
            m2)
        m1;
      !res

    (* symmetrization *)
      
    let sym_flipHeight_inplace m = union m (flipHeight m)
    let sym_flipWidth_inplace m = union m (flipWidth m)
    let sym_rotate180_inplace m = union m (rotate180 m)
    let sym_flipHeightWidth_inplace m = union (union m (flipHeight m)) (flipWidth m) (* also includes rotate180(m) *)
    let sym_flipDiag1_inplace m =
      if height m <> width m then raise Invalid_dim;
      union m (flipDiag1 m)
    let sym_flipDiag2_inplace m =
      if height m <> width m then raise Invalid_dim;
      union m (flipDiag2 m)
    let sym_flipDiag1Diag2_inplace m =
      if height m <> width m then raise Invalid_dim;
      union (union m (flipDiag1 m)) (flipDiag2 m)
    let sym_rotate90_inplace m =
      if height m <> width m then raise Invalid_dim;
      union m (rotate90 m)
    let sym_full_inplace m =
      if height m <> width m then raise Invalid_dim;
      union (union m (rotate90 m)) (flipHeight m) (* includes all symmetries *)

    let sym_flipHeight_unfold m =
      let m'= flipHeight m in
      [ concatHeight m m'; concatHeight m' m ]
    let sym_flipWidth_unfold m =
      let m'= flipWidth m in
      [ concatWidth m m'; concatWidth m' m ]
    let sym_rotate180_unfold m =
      let m' = rotate180 m in
      [ concatHeight m m'; concatHeight m' m;
        concatWidth m m'; concatWidth m' m ]
    let sym_flipHeightWidth_unfold m =
      let m'= flipWidth m in
      let m1 = concatWidth m m' in
      let m2 = concatWidth m' m in
      let m1' = flipHeight m1 in
      let m2' = flipHeight m2 in
      [ concatHeight m1 m1'; concatHeight m1' m1;
        concatHeight m2 m2'; concatHeight m2' m2 ]
    let sym_rotate90_unfold m =
      if height m <> width m then raise Invalid_dim;
      let m90 = rotate90 m in
      let m180 = rotate180 m in
      let m270 = rotate270 m in
      [ concatHeightWidth m m90 m270 m180;
        concatHeightWidth m270 m m180 m90;
        concatHeightWidth m180 m270 m90 m;
        concatHeightWidth m90 m180 m m270 ]

  end

module Mask_model =
  struct
    type t =
      [ `Mask of Mask.t
      | `Full of bool (* all pixels on, bool=true if also a collapsed border *)
      | `Border (* width-1 border *)
      | `EvenCheckboard
      | `OddCheckboard
      | `PlusCross
      | `TimesCross
      ]
      
    let subsumes (m0 : t) (m1 : t) : bool =
      match m0, m1 with
      | `Mask m1, `Mask m2 -> Mask.equal m1 m2
      | `Full b0, `Full b1 -> not b0 || b1
      | `Border, `Full true -> true
      | _ -> m0 = m1
  
    let area ~height ~width = function
      | `Mask bm -> Mask.area bm
      | `Full _ -> height * width
      | `Border -> 2 * (height + width) - 4
      | `EvenCheckboard -> (height * width + 1) / 2
      | `OddCheckboard -> height * width / 2
      | `PlusCross -> height + width - 1
      | `TimesCross -> height + width - (height mod 2)
  
    let mem ~height ~width i j = (* mask height and width, relative position (i,j) *)
      function
      | `Mask m -> Mask.mem i j m
      | `Full _ -> true
      | `Border -> i=0 || j=0 || i=height-1 || j=width-1
      | `EvenCheckboard -> (i+j) mod 2 = 0
      | `OddCheckboard -> (i+j) mod 2 = 1
      | `PlusCross -> (i=height/2 || i=(height-1)/2) || (j=width/2 || j=(width-1)/2)
      | `TimesCross -> height=width && (i=j || (height-1-i) = j)

    let to_mask ~height ~width (mm : t) : Mask.t =
      let res = ref (Mask.empty height width) in
      for i = 0 to height - 1 do
        for j = 0 to width - 1 do
          if mem ~height ~width i j mm then
            res := Mask.add i j !res
        done
      done;
      !res
                       
    let from_box_in_mask ?visible_mask ~mini ~maxi ~minj ~maxj (mask : Mask.t) : t list =
      let height, width = maxi-mini+1, maxj-minj+1 in
      let is_visible =
        match visible_mask with
        | None -> (fun absi absj -> true)
        | Some m -> (fun absi absj -> Mask.mem absi absj m)
      in
      let m = ref (Mask.empty height width) in (* mask over the part box *)
      let models = ref [`Full (height=2 && width>=2 || width=2 && height>=2);
                        `Border; `EvenCheckboard; `OddCheckboard; `PlusCross; `TimesCross] in
      for absi = mini to maxi do
        let i = absi - mini in
        for absj = minj to maxj do
          let j = absj - minj in
          let hidden = not (is_visible absi absj) in
          let pixel_on = Mask.mem absi absj mask in
          if pixel_on then m := Mask.add i j !m;
          models :=
            List.filter
              (fun model -> hidden || pixel_on = mem ~height ~width i j model)
              !models;
        done
      done;
      !models @ [`Mask !m] (* still considering as raw mask for allowing some computations such as scaling *)
      
    let from_mask (mask : Mask.t) : t list =
      from_box_in_mask
        ~mini:0 ~maxi:(Mask.height mask - 1)
        ~minj:0 ~maxj:(Mask.width mask - 1)
        mask

    let scale_up k l : t -> t result = function
      | `Mask m -> Result.Ok (`Mask (Mask.scale_up k l m))
      | (`Full false as mm) -> Result.Ok mm
      | mm -> Result.Error (Undefined_result "Grid.Mask_model.scale_up: undefined")

    let scale_to new_h new_w : t -> t result = function
      | `Mask m ->
         ( match Mask.scale_to new_h new_w m with
           | Some m -> Result.Ok (`Mask m)
           | None -> Result.Error (Undefined_result "Grid.Mask.scale_to: wrong new dimension") )
      | (`Full false as mm) -> Result.Ok mm
      | mm -> Result.Error (Undefined_result "Grid.Mask_model.scale_to: undefined")

    let tile k l : t -> t result = function
      | `Mask m -> Result.Ok (`Mask (Mask.tile k l m))
      | `Full false -> Result.Ok (`Full false)
      | _ -> Result.Error (Undefined_result "Grid.Mask_model.tile: undefined")

    let resize_alike new_h new_w : t -> t result = function
      | `Mask m -> Result.Ok (`Mask (Mask.resize_alike m new_h new_w))
      | (`Full false | `OddCheckboard | `EvenCheckboard as mm) -> Result.Ok mm
      | _ -> Result.Error (Undefined_result "Grid.Mask_model.resize_alike: undefined")

    let symmetry (f : Mask.t -> Mask.t) : t -> t result = function
      | `Mask m -> Result.Ok (`Mask (f m))
      | (`Full _ | `Border | `TimesCross | `PlusCross as mm) -> Result.Ok mm
      | _ -> Result.Error (Undefined_result "Grid.Mask_model.symmetry: undefined")
    let flipHeight = symmetry Mask.flipHeight
    let flipWidth = symmetry Mask.flipWidth
    let flipDiag1 = symmetry Mask.flipDiag1
    let flipDiag2 = symmetry Mask.flipDiag2
    let rotate180 = symmetry Mask.rotate180
    let rotate90 = symmetry Mask.rotate90
    let rotate270 = symmetry Mask.rotate270
                   
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
     let pixels = ref p1.pixels in (* TODO: do not use a ref here *)
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
		pixels := Mask.add i j !pixels
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
    (fun _ part res -> (* TODO: fin a way to avoid this trick *)
      if part.mini=0 && part.maxi=h-1
         && part.minj=0 && part.maxj=w-1
         && part.color=black (* NOTE: do not relax this constraint *)
         && 2 * part.nb_pixels - 1 > hw
      then res (* ignoring black background *)
      else part :: split_part part @ res)
    [])
let segment_by_color, reset_segment_by_color =
  Common.memoize ~size:203 segment_by_color
  

(* locating shapes *)

let background_colors (g : t) : color list = (* in decreasing frequency order *)
  Common.prof "Grid.background_colors" (fun () ->
  let area = g.height * g.width in
  let l = ref [] in
  for c = 0 to nb_color - 1 do
    let n = g.color_count.(c) in
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
  if List.mem black l
  then l
  else l @ [black]) (* ensure black is considered as a background color *)

  
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
         Stdlib.compare p1 p2))
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
		   new_cover : Mask.t; (* new covered pixels *)
		   mask_models : Mask_model.t list; (* mask models, relative to rectangle box *)
		   delta : pixel list;
                   nb_explained_pixels : int }

let rectangle_as_grid (g : t) (r : rectangle) : t =
  let gr = make g.height g.width no_color in
  for i = r.offset_i to r.offset_i + r.height - 1 do
    for j = r.offset_j to r.offset_j + r.width - 1 do
      if Mask_model.mem
           ~height:r.height ~width:r.width (i - r.offset_i) (j - r.offset_j)
           (try List.hd r.mask_models with _ -> assert false) then
           (*Mask.mem i j r.mask then*)
	(*match List.find_opt (fun (i',j',c') -> i=i' && j=j') r.delta with
	| Some (_,_,c) -> set_pixel gr i j c
	| None ->*) set_pixel gr i j r.color
    done
  done;
  gr
		   
let pp_rectangles (g : t) (rs : rectangle list) =
  print_endline "RECTANGLES:";
  pp_grids (g :: List.map (rectangle_as_grid g) rs)

  
(*let models_of_mask_part (visible_mask : Mask.t) (p : part) : Mask_model.t list =
  let height, width = p.maxi-p.mini+1, p.maxj-p.minj+1 in
  let m = ref (Mask.empty height width) in (* mask over the part box *)
  let full = ref true in
  let border = ref true in
  let even_cb = ref true in
  let odd_cb = ref true in
  let plus_cross = ref true in
  let times_cross = ref (height = width) in
  for absi = p.mini to p.maxi do
    let i = absi - p.mini in
    for absj = p.minj to p.maxj do (* reuse below Boolean expressions from 'Mask_model.mem' *)
      let j = absj - p.minj in
      let hidden = not (Mask.mem absi absj visible_mask) in
      let pixel_on = Mask.mem absi absj p.pixels in
      if pixel_on then m := Mask.add i j !m;
      full := !full && (hidden || pixel_on = true);
      border := !border && (hidden || pixel_on = (i=0 || j=0 || i=height-1 || j=width-1));
      even_cb := !even_cb && (hidden || pixel_on = ((i+j) mod 2 = 0));
      odd_cb := !odd_cb && (hidden || pixel_on = ((i+j) mod 2 = 1));
      plus_cross := !plus_cross && (hidden || pixel_on = ((i=height/2 || i=(height-1)/2) || (j=width/2 || j=(width-1)/2)));
      times_cross := !times_cross && (hidden || pixel_on = (i=j || (height-1-i) = j))
    done
  done;
  let res = [] in
  let res = if !full then `Full (height=2 && width>=2 || width=2 && height>=2)::res else res in
  let res = if !border then `Border::res else res in
  let res = if !even_cb then `EvenCheckboard::res else res in
  let res = if !odd_cb then `OddCheckboard::res else res in
  let res = if !plus_cross then `PlusCross::res else res in
  let res = if !times_cross then `TimesCross::res else res in
  let res = if res = [] then `Mask !m::res else res in
  res*)
  
let rectangles_of_part ~(multipart : bool) (g : t) (mask : Mask.t) (p : part) : rectangle list =
  let height, width = p.maxi-p.mini+1, p.maxj-p.minj+1 in
  let delta = ref [] in
  let nb_delta = ref 0 in
  let nb_explained_pixels = Mask.area (Mask.inter mask p.pixels) in 
  for i = p.mini to p.maxi do
    for j = p.minj to p.maxj do
      if Mask.mem i j mask && not (Mask.mem i j p.pixels)
      then (
        delta := (i, j, g.matrix.{i,j}) :: !delta;
        incr nb_delta
      )
    done
  done;
  let res = [] in
  let res = (* adding full rectangle with delta *)
    if !nb_delta < height * width - !nb_delta
    then
      let new_cover =
        List.fold_left
          (fun mask (i,j,c) -> Mask.add i j mask)
          p.pixels !delta in
      { height; width;
        offset_i = p.mini; offset_j = p.minj;
        color = p.color;
        new_cover;
        mask_models = [`Full (height = 2 && width >= 2 || width = 2 && height >= 2)];
	delta = (!delta);
        nb_explained_pixels
      } :: res
    else res in
  let res = (* adding rectangle with specific mask model, without delta *)
    if not multipart && !delta <> [] (* && !valid_area >= 1 * area / 2 *)
    then
      { height; width;
        offset_i = p.mini; offset_j = p.minj;
        color = p.color;
        new_cover = p.pixels;
        mask_models = Mask_model.from_box_in_mask ~visible_mask:mask
                        ~mini:p.mini ~maxi:p.maxi ~minj:p.minj ~maxj:p.maxj
                        p.pixels;
        delta = [];
        nb_explained_pixels
      } :: res
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
(*  let res = (* adding a grid-wide one-color rectangle goes against object-centric modeling, this breaks many solved tasks *)
    List.fold_left
      (fun res (c, ps) ->
        match ps with
        | [] -> assert false
        | p::ps1 ->
           let pixels =
             List.fold_left
               (fun pixels p1 -> Mask.union pixels p1.pixels)
               p.pixels ps1 in
           let r : rectangle =
             { height = g.height;
               width = g.width;
               offset_i = 0;
               offset_j = 0;
               color = c;
               new_cover = pixels;
               mask_models = Mask_model.from_box_in_mask ~visible_mask:mask
                               ~mini:0 ~maxi:(g.height-1) ~minj:0 ~maxj:(g.width-1)
                               pixels;
               delta = [];
               nb_explained_pixels = (Mask.area (Mask.inter mask pixels));
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
           Stdlib.compare (* decreasing nb_explained_pixels *)
             (- rect1.nb_explained_pixels, rect1)
             (- rect2.nb_explained_pixels, rect2))
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
  reset_points ();
  (*  reset_rectangles_of_part ();*)
  reset_rectangles ()
