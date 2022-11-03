
open Bigarray

module Xprint = Arc_xprint
   
exception Undefined_result of string (* for undefined computations *)

type 'a result = ('a,exn) Result.t

let ( let| ) res f = Result.bind res f [@@inline]

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
           color_count : int Array.t; (* col (0..10) -> nb. cells with that color *)
	   matrix : matrix; (* [i,j] -> col *)
         }
type pixel = int * int * color (* x, y, col *)

exception Invalid_dim (* height/width are invalid for the operation *)
exception Invalid_coord of string (* .{i,j} invalid, string describes source *)
                         
let max_size = 30
           
let make height width col =
  let matrix = Array2.create Int8_unsigned C_layout height width in
  let color_count = Array.make (nb_color+1) 0 in 
  Array2.fill matrix col;
  color_count.(col) <- height * width;
  { height; width; matrix; color_count }

let copy (g : t) =
  let matrix' = Array2.create Int8_unsigned C_layout g.height g.width in
  Array2.blit g.matrix matrix';
  { g with
    color_count = Array.copy g.color_count;
    matrix = matrix' }
  
let dummy = make 0 0 0

let dims (grid : t) : int * int =
  grid.height, grid.width [@@inline]

let get_pixel ?(source = "unknown") grid i j =
  let h, w = dims grid in
  if i < h && j < w && i >=0 && j >= 0
  then grid.matrix.{i,j}
  else raise (Invalid_coord source) [@@inline]
  
let set_pixel grid i j c =
  let h, w = dims grid in
  if i < h && j < w && i >=0 && j >= 0
  then 
    let c0 = grid.matrix.{i,j} in
    if c <> c0 then (
      grid.matrix.{i,j} <- c;
      (* maintaining color count *)
      grid.color_count.(c0) <- grid.color_count.(c0) - 1;
      grid.color_count.(c) <- grid.color_count.(c) + 1
    )
  else () [@@inline] (* pixels out of bound are ignored *)

let iter_pixels f grid =
  let mat = grid.matrix in
  for i = 0 to grid.height - 1 do
    for j = 0 to grid.width - 1 do
      f i j mat.{i,j}
    done
  done [@@inline]

let majority_color (g : t) : color =
  let res = ref black in
  let nb_max = ref (g.color_count.(black)) in
  for c = 1 to nb_color do
    let nb = g.color_count.(c) in
    if nb > !nb_max then (
      res := c;
      nb_max := nb)
  done;
  !res
  
let color_partition ~(colors : color list) (grid : t) : int list =
  List.map
    (fun c -> grid.color_count.(c))
    colors

(* pretty-printing in terminal *)

let rec xp_grids (print : Xprint.t) grids =
  let grids_per_line = 5 in
  let nb_lines = (List.length grids - 1) / 5 + 1 in
  let max_height =
    List.fold_left (fun res g -> max res g.height) 0 grids in
  print#string "\n";
  for k = 0 to nb_lines - 1 do
    for i = 0 to max_height - 1 do
      List.iteri
	(fun l g ->
	 if l / grids_per_line = k then (
	   for j = 0 to g.width - 1 do
	     if i < g.height
	     then xp_color print g.matrix.{i,j}
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
  for i = 0 to grid.height - 1 do
    for j = 0 to grid.width - 1 do
      xp_color print grid.matrix.{i,j}
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
    | 10 -> [on_white], "  "
    | _ -> invalid_arg "Invalid color code" in
  print#style_string style str

let pp_grids grids = Xprint.to_stdout xp_grids grids
let pp_grid grid = Xprint.to_stdout xp_grid grid
let pp_color c = Xprint.to_stdout xp_color c

(* comparing grids *)

let same (g1 : t) (g2 : t) : bool = (g1 = g2)
  
type diff =
  | Grid_size_mismatch of { src_height: int; src_width: int;
			    tgt_height: int; tgt_width: int }
  | Grid_diff_pixels of { height: int; width: int; pixels: pixel list }
							  
let diff (source : t) (target : t) : diff option = (* QUICK *)
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
        let src_c = get_pixel ~source:"diff/1" source i j in
        let tgt_c = get_pixel ~source:"diff/2" target i j in
	if src_c <> tgt_c then
	  res := (i,j,tgt_c) :: !res
      done
    done;
    if !res = []
    then None
    else Some (Grid_diff_pixels {height; width; pixels=(!res)})

(* grid masks *)

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
    val inter_is_empty : t -> t -> bool

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
    let inter_is_empty m1 m2 =
      bits_for_all (fun k bmp -> bmp land (Array1.get m2.bits k) = 0) m1.bits

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
      let h, w = dims m in
      let h', w' = h * k, w * l in
      if h' > max_size || w' > max_size
      then Result.Error (Undefined_result "Mask.scale_up: result grid too large")
      else (
        let res = ref (empty h' w') in
        iter
          (fun i j ->
            for i' = k*i to k*(i+1)-1 do
              for j' = l*j to l*(j+1)-1 do
                res := add i' j' !res
              done
            done)
          m;
        Result.Ok !res)

    let scale_down (k : int) (l : int) m = (* scaling down *)
      let h, w = dims m in
      if h mod k = 0 && w mod l = 0
      then ( (* each resulting pixel is a OR of the corresponding source pixels *)
        let res = ref (empty (height m / k) (width m / l)) in
        iter
          (fun i j -> res := add (i/k) (j/l) !res)
          m;
        Result.Ok !res)
      else Result.Error (Undefined_result "Mask.scale_down: not congruent")

    let scale_to (new_h : int) (new_w : int) (m : t) : t result =
      let h, w = dims m in
      if new_h >= h && new_w >= w && new_h mod h = 0 && new_w mod w = 0 then
        scale_up (new_h / h) (new_w / w) m
      else if new_h > 0 && new_w > 0 && new_h <= h && new_w <= w && h mod new_h = 0 && w mod new_w = 0 then
        scale_down (h / new_h) (w / new_w) m
      else Result.Error (Undefined_result "Mask.scale_to: not congruent")
        
    (* resize and factor *)

    let tile (k : int) (l : int) m = (* k x l tiling of m *)
      let h, w = dims m in
      let h', w' = h * k, w * l in
      if h' > max_size || w' > max_size
      then Result.Error (Undefined_result "Mask.tile: result grid too large")
      else (
        let res = ref (empty h' w') in
        iter
          (fun i j ->
            for u = 0 to k-1 do
              for v = 0 to l-1 do
                res := add (u*h + i) (v*w + j) !res
              done
            done)
          m;
        Result.Ok !res)

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

    let resize_alike (m : t) (new_h : int) (new_w : int) : t result = (* change size while preserving the repeating pattern *)
      assert (new_h > 0 && new_w > 0);
      if new_h > max_size || new_w > max_size
      then Result.Error (Undefined_result "Mask.resize_alike: result grid too large")
      else (
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
        Result.Ok !res)
      
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

    let concatHeight m1 m2 : t result =
      let h1, w1 = dims m1 in
      let h2, w2 = dims m2 in
      if w1 <> w2 then Result.Error Invalid_dim
      else if h1+h1 > max_size then Result.Error (Undefined_result "Mask.concatHeight: result grid too large")
      else (
        let res = ref (empty (h1+h2) w1) in
        iter
          (fun i1 j1 -> res := add i1 j1 !res)
          m1;
        iter
          (fun i2 j2 -> res := add (h1+i2) j2 !res)
          m2;
        Result.Ok !res)
      
    let concatWidth m1 m2 : t result =
      let h1, w1 = dims m1 in
      let h2, w2 = dims m2 in
      if h1 <> h2 then Result.Error Invalid_dim
      else if w1+w2 > max_size then Result.Error (Undefined_result "Mask.concatWidth: resut grid too large")
      else (
        let res = ref (empty h1 (w1+w2)) in
        iter
          (fun i1 j1 -> res := add i1 j1 !res)
          m1;
        iter
          (fun i2 j2 -> res := add i2 (w1+j2) !res)
          m2;
        Result.Ok !res)

    let concatHeightWidth m1 m2 m3 m4 : t result (* top left, top right, bottom left, bottom right *) =
      let| m12 = concatWidth m1 m2 in
      let| m34 = concatWidth m3 m4 in
      concatHeight m12 m34

    (* TODO: selecting halves and quarters *)

    let compose m1 m2 = (* repeating m2 for each 1-bit of m1 *)
      let h1, w1 = dims m1 in
      let h2, w2 = dims m2 in
      let h, w = h1*h1, w1*w2 in
      if h > max_size || w > max_size
      then Result.Error (Undefined_result "Mask.compose: result grid too large")
      else (
        let res = ref (empty (h1*h2) (w1*w2)) in
        iter
          (fun i1 j1 ->
            iter
              (fun i2 j2 ->
                res := add (i1*h2+i2) (j1*w2+j2) !res)
              m2)
          m1;
        Result.Ok !res)

    (* symmetrization *)
      
    let layers ms : t result =
      match ms with
      | [] -> Result.Error (Invalid_argument "Grid.Mask.layers: empty list")
      | m1::ms1 ->
         let h1, w1 = dims m1 in
         if List.for_all (fun mi -> dims mi = (h1,w1)) ms1
         then
           let m = List.fold_left union m1 ms1 in
           Result.Ok m
         else Result.Error Invalid_dim
         
    let sym_flipHeight_inplace m = layers [m; flipHeight m]
    let sym_flipWidth_inplace m = layers [m; flipWidth m]
    let sym_rotate180_inplace m = layers [m; rotate180 m]
    let sym_flipHeightWidth_inplace m = layers [m; flipHeight m; flipWidth m; rotate180 m]
    let sym_flipDiag1_inplace m =
      if height m <> width m
      then Result.Error Invalid_dim
      else layers [m; flipDiag1 m]
    let sym_flipDiag2_inplace m =
      if height m <> width m
      then Result.Error Invalid_dim
      else layers [m; flipDiag2 m]
    let sym_flipDiag1Diag2_inplace m =
      if height m <> width m
      then Result.Error Invalid_dim
      else layers [m; flipDiag1 m; flipDiag2 m; rotate180 m]
    let sym_rotate90_inplace m =
      if height m <> width m
      then Result.Error Invalid_dim
      else layers [m; rotate90 m; rotate180 m; rotate270 m]
    let sym_full_inplace m =
      if height m <> width m
      then Result.Error Invalid_dim
      else (* includes all symmetries *)
        let| m' = layers [m; rotate90 m] in
        Result.Ok (flipHeight m')

(* not ready for use 
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
 *)
  end

module Mask_model =
  struct
    type t =
      [ `Mask of Mask.t
      | `Full (* all pixels on *)
      | `Border (* width-1 border *)
      | `EvenCheckboard
      | `OddCheckboard
      | `PlusCross
      | `TimesCross
      ]
      
    let subsumes (m0 : t) (m1 : t) : bool =
      match m0, m1 with
      | `Mask m1, `Mask m2 -> Mask.equal m1 m2
      | _ -> m0 = m1
  
    let area ~height ~width = function
      | `Mask bm -> Mask.area bm
      | `Full -> height * width
      | `Border -> 2 * (height + width) - 4
      | `EvenCheckboard -> (height * width + 1) / 2
      | `OddCheckboard -> height * width / 2
      | `PlusCross -> height + width - 1
      | `TimesCross -> height + width - (height mod 2)
  
    let mem ~height ~width i j = (* mask height and width, relative position (i,j) *)
      function
      | `Mask m -> Mask.mem i j m
      | `Full -> true
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
      let models = ref [`Full; `Border; `EvenCheckboard; `OddCheckboard; `PlusCross; `TimesCross] in
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
      | `Mask m ->
         let| m' = Mask.scale_up k l m in
         Result.Ok (`Mask m')
      | (`Full as mm) -> Result.Ok mm
      | mm -> Result.Error (Undefined_result "Grid.Mask_model.scale_up: undefined")
    let scale_up, reset_scale_up =
      Common.memoize3 ~size:101 scale_up

    let scale_down k l : t -> t result = function
      | `Mask m ->
         let| m' = Mask.scale_down k l m in
         Result.Ok (`Mask m')
      | (`Full as mm) -> Result.Ok mm
      | mm -> Result.Error (Undefined_result "Grid.Mask_model.scale_down: undefined")
    let scale_down, reset_scale_down =
      Common.memoize3 ~size:101 scale_down
            
    let scale_to new_h new_w : t -> t result = function
      | `Mask m ->
         let| m' = Mask.scale_to new_h new_w m in
         Result.Ok (`Mask m')
      | (`Full as mm) -> Result.Ok mm
      | mm -> Result.Error (Undefined_result "Grid.Mask_model.scale_to: undefined")
    let scale_to, reset_scale_to =
      Common.memoize3 ~size:101 scale_to

    let tile k l : t -> t result = function
      | `Mask m ->
         let| m' = Mask.tile k l m in
         Result.Ok (`Mask m')
      | `Full -> Result.Ok `Full
      | _ -> Result.Error (Undefined_result "Grid.Mask_model.tile: undefined")
    let tile, reset_tile =
      Common.memoize3 ~size:101 tile

    let resize_alike new_h new_w : t -> t result = function
      | `Mask m ->
         let| m' = Mask.resize_alike m new_h new_w in
         Result.Ok (`Mask m')
      | (`Full | `OddCheckboard | `EvenCheckboard as mm) -> Result.Ok mm
      | _ -> Result.Error (Undefined_result "Grid.Mask_model.resize_alike: undefined")
    let resize_alike, reset_resize_alike =
      Common.memoize3 ~size:101 resize_alike

    let compose (m1 : Mask.t) (mm2 : t) : t result =
      match mm2 with
      | `Mask m2 ->
         let| m = Mask.compose m1 m2 in
         Result.Ok (`Mask m)
      | _ -> Result.Error (Undefined_result "Grid.Mask_model.compose: undefined")
    let compose, reset_compose =
      Common.memoize2 ~size:101 compose
      
    let symmetry (f : Mask.t -> Mask.t) : t -> t result = function
      | `Mask m -> Result.Ok (`Mask (f m))
      | (`Full | `Border | `TimesCross | `PlusCross as mm) -> Result.Ok mm
      | _ -> Result.Error (Undefined_result "Grid.Mask_model.symmetry: undefined")

    let flipHeight = symmetry Mask.flipHeight
    let flipHeight, reset_flipHeight =
      Common.memoize ~size:101 flipHeight

    let flipWidth = symmetry Mask.flipWidth
    let flipWidth, reset_flipWidth =
      Common.memoize ~size:101 flipWidth

    let flipDiag1 = symmetry Mask.flipDiag1
    let flipDiag1, reset_flipDiag1 =
      Common.memoize ~size:101 flipDiag1

    let flipDiag2 = symmetry Mask.flipDiag2
    let flipDiag2, reset_flipDiag2 =
      Common.memoize ~size:101 flipDiag2

    let rotate180 = symmetry Mask.rotate180
    let rotate180, reset_rotate180 =
      Common.memoize ~size:101 rotate180

    let rotate90 = symmetry Mask.rotate90
    let rotate90, reset_rotate90 =
      Common.memoize ~size:101 rotate90

    let rotate270 = symmetry Mask.rotate270
    let rotate270, reset_rotate270 =
      Common.memoize ~size:101 rotate270

    let inter m1 m2 : t result =
      match m1, m2 with
      | `Full, _ -> Result.Ok m2
      | _, `Full -> Result.Ok m1
      | `Mask bm1, `Mask bm2 when Mask.same_size bm1 bm2 ->
         Result.Ok (`Mask (Mask.inter bm1 bm2))
      | _ -> Result.Error (Undefined_result "Mask_model.inter: undefined")
    let inter, reset_inter =
      Common.memoize2 ~size:101 inter

    let union m1 m2 : t result =
      match m1, m2 with
      | `Full, _ -> Result.Ok m1
      | _, `Full -> Result.Ok m2
      | `Mask bm1, `Mask bm2 when Mask.same_size bm1 bm2 ->
         Result.Ok (`Mask (Mask.union bm1 bm2))
      | _ -> Result.Error (Undefined_result "Mask_model.union: undefined")
    let union, reset_union =
      Common.memoize2 ~size:101 union

    let diff_sym (m1 : t) (m2 : t) : t result =
      match m1, m2 with
      | `Full, `Mask bm2 -> Result.Ok (`Mask (Mask.compl bm2))
      | `Mask bm1, `Full -> Result.Ok (`Mask (Mask.compl bm1))
      | `Mask bm1, `Mask bm2 when Mask.same_size bm1 bm2 ->
         Result.Ok (`Mask (Mask.diff_sym bm1 bm2))
      | _ -> Result.Error (Undefined_result "Mask_model.diff_sym: undefined")
    let diff_sym, reset_diff_sym =
      Common.memoize2 ~size:101 diff_sym

    let diff (m1 : t) (m2 : t) : t result =
      match m1, m2 with
      | `Full, `Mask bm2 -> Result.Ok (`Mask (Mask.compl bm2))
      | `Mask bm1, `Mask bm2 when Mask.same_size bm1 bm2 ->
         Result.Ok (`Mask (Mask.diff bm1 bm2))
      | _ -> Result.Error (Undefined_result "Mask_model.diff: undefined")
    let diff, reset_diff =
      Common.memoize2 ~size:101 diff

    let compl (m1 : t) : t result =
      match m1 with
      | `Mask bm1 -> Result.Ok (`Mask (Mask.compl bm1))
      | _ -> Result.Error (Undefined_result "Grid.Mask_model.compl: undefined")
    let compl, reset_compl =
      Common.memoize ~size:101 compl

    let reset_memoized_functions () =
      reset_scale_up ();
      reset_scale_down ();
      reset_scale_to ();
      reset_tile ();
      reset_resize_alike ();
      reset_compose (); 
      reset_flipHeight ();
      reset_flipWidth ();
      reset_flipDiag1 ();
      reset_flipDiag2 ();
      reset_rotate180 ();
      reset_rotate90 ();
      reset_rotate270 ();
      reset_inter ();
      reset_union ();
      reset_diff_sym ();
      reset_diff ();
      reset_compl ()
      
  end

  
(* operations on grids *)

module Transf = (* black considered as neutral color by default *)
  struct

    let mask_of_grid ?(bgcolor = black) (g : t) : Mask.t =
      (* returns a non-bgcolor mask version of the grid *)
      let h, w = dims g in
      let res = ref (Mask.empty h w) in
      iter_pixels
        (fun i j c ->
          if c <> bgcolor then
            res := Mask.add i j !res)
        g;
      !res
    let mask_of_grid, reset_mask_of_grid =
      Common.memoize ~size:101 mask_of_grid

    let grid_of_mask (m : Mask.t) (c : color) : t =
      (* return a colored grid version of a mask *)
      let h, w = Mask.dims m in
      let res = make h w black in
      Mask.iter
        (fun i j ->
          set_pixel res i j c)
        m;
      res
    let grid_of_mask, reset_grid_of_mask =
      Common.memoize2 ~size:101 grid_of_mask
      
    (* coloring *)

    let swap_colors g c1 c2 : t result =
      if g.color_count.(c1) = 0
      then Result.Error (Undefined_result "swap_colors: none of the color present")
      else (
        let h, w = dims g in
        let res = make h w black in
        iter_pixels
          (fun i j c ->
            let c' =
              if c = c1 then c2
              else if c = c2 then c1
              else c in
            set_pixel res i j c')
          g;
        Result.Ok res)
    let swap_colors, reset_swap_colors =
      Common.memoize3 ~size:101 swap_colors
    
    (* isometric transformations *)

    let flipHeight g =
      let h, w = dims g in
      let res = make h w black in
      iter_pixels
        (fun i j c -> set_pixel res (h - 1 - i) j c)
        g;
      res
    let flipHeight, reset_flipHeight =
      Common.memoize ~size:101 flipHeight
                     
    let flipWidth g =
      let h, w = dims g in
      let res = make h w black in
      iter_pixels
        (fun i j c -> set_pixel res i (w - 1 - j) c)
        g;
      res
    let flipWidth, reset_flipWidth =
      Common.memoize ~size:101 flipWidth

    let flipDiag1 g =
      let h, w = dims g in
      let res = make w h black in
      iter_pixels
        (fun i j c -> set_pixel res j i c)
        g;
      res
    let flipDiag1, reset_flipDiag1 =
      Common.memoize ~size:101 flipDiag1

    let flipDiag2 g =
      let h, w = dims g in
      let res = make w h black in
      iter_pixels
        (fun i j c -> set_pixel res (w - 1 - j) (h - 1 - i) c)
        g;
      res
    let flipDiag2, reset_flipDiag2 =
      Common.memoize ~size:101 flipDiag2

    let rotate90 g = (* clockwise *)
      let h, w = dims g in
      let res = make w h black in
      iter_pixels
        (fun i j c -> set_pixel res j (h - 1 - i) c)
        g;
      res
    let rotate90, reset_rotate90 =
      Common.memoize ~size:101 rotate90
      
    let rotate180 g = (* clockwise *)
      let h, w = dims g in
      let res = make h w black in
      iter_pixels
        (fun i j c -> set_pixel res (h - 1 - i) (w - 1 - j) c)
        g;
      res
    let rotate180, reset_rotate180 =
      Common.memoize ~size:101 rotate180
      
    let rotate270 g = (* clockwise *)
      let h, w = dims g in
      let res = make w h black in
      iter_pixels
        (fun i j c -> set_pixel res (w - 1 - j) i c)
        g;
      res
    let rotate270, reset_rotate270 =
      Common.memoize ~size:101 rotate270
      
    (* scaling *)
      
    let scale_up (k : int) (l : int) (g : t) : t result = (* scaling up grid [g] by a factor (k,l) *)
      let h, w = dims g in
      let h', w' = h * k, w * l in
      if h' > max_size || w' > max_size
      then Result.Error (Undefined_result "scale_up: result grid too large")
      else (
        let res = make h' w' black in
        iter_pixels
          (fun i j c ->
            for i' = k*i to k*(i+1)-1 do
              for j' = l*j to l*(j+1)-1 do
                set_pixel res i' j' c
              done
            done)
          g;
        Result.Ok res)
    let scale_up, reset_scale_up =
      Common.memoize3 ~size:101 scale_up

    let scale_down (k : int) (l : int) (g : t) : t result = (* scaling down *)
      let h, w = dims g in
      if h mod k = 0 && w mod l = 0
      then (
        let ok = ref true in
        iter_pixels
          (fun i j c ->
            ok := !ok && get_pixel ~source:"scale_down/1" g ((i/k)*k) ((j/l)*l) = c)
          g;
        if !ok (* all pixels scaling down to a single pixel have same color *)
        then (
          let res = make (h / k) (w / l) black in
          iter_pixels
            (fun i j c -> set_pixel res i j (get_pixel ~source:"scale_down/2" g (i*k) (j*l)))
            res;
          Result.Ok res )
        else Result.Error (Undefined_result "Grid.Transf.scale_down: grid not regular"))
      else Result.Error (Undefined_result "Grid.Transf.scale_down: dims and factors not congruent")
    let scale_down, reset_scale_down =
      Common.memoize3 ~size:101 scale_down

    let scale_to (new_h : int) (new_w : int) (g : t) : t result =
      let h, w = dims g in
      if new_h >= h && new_w >= w && new_h mod h = 0 && new_w mod w = 0 then
        scale_up (new_h / h) (new_w / w) g
      else if new_h > 0 && new_w > 0 && new_h <= h && new_w <= w && h mod new_h = 0 && w mod new_w = 0 then
        scale_down (h / new_h) (w / new_w) g
      else Result.Error (Undefined_result "Grid.Trans.scale_to: invalid scaling vector")
    let scale_to, reset_scale_to =
      Common.memoize3 ~size:101 scale_to
      
    (* resize and factor *)

    let tile (k : int) (l : int) g : t result = (* k x l tiling of g *)
      let h, w = dims g in
      let h', w' = h * k, w * l in
      if h' > max_size || w' > max_size
      then Result.Error (Undefined_result "tile: result grid too large")
      else (
        let res = make h' w' black in
        iter_pixels
          (fun i j c ->
            for u = 0 to k-1 do
              for v = 0 to l-1 do
                set_pixel res (u*h + i) (v*w + j) c
              done
            done)
          g;
        Result.Ok res)
    let tile, reset_tile =
      Common.memoize3 ~size:101 tile

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
      Common.memoize ~size:101 factor

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
      Common.memoize3 ~size:101 resize_alike

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
         not (Array.exists (fun (_,_,c) ->
                  c = no_color)
                k_ar)
      | Period2 (_,_,k2_ar) ->
         not (Array.exists (fun k_ar ->
                  Array.exists (fun (_,_,c) ->
                      c = no_color)
                    k_ar)
                k2_ar)

                  
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
                  let k_ar = Array.make p (0,0,no_color) in
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
                  let k2_ar = Array.make_matrix p1 p2 (0,0,no_color) in
                  (p1, p2, k2_ar))
                (product (range 2 max_axis1) (range 2 max_axis2)) in
            (axis1, f_axis1, axis2, f_axis2, ref p2_map))
          all_axis_pairs) in
      (* filtering through one pass of the grid pixels *)
      iter_pixels
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
                      else if c_k = no_color then (k_ar.(k) <- (1,n+1,c); true)
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
                      else if c_k = no_color then (k2_ar.(k1).(k2) <- (1,n+1,c); true)
                      else if c_k = c then (k2_ar.(k1).(k2) <- (nc+1,n+1,c_k); true)
                      else (* inconsistency *) false)
                    !ref_p2_map in
                if p2_map = [] (* no period for those axis *)
                then false
                else (ref_p2_map := p2_map; true))
              !periods2)
        g;
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
                        if c = no_color then res else res + (n-nc))
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
                            if c = no_color then res else res + (n-nc))
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
      Common.memoize ~size:101 periodicities

    let fill_with_periodicity (bgcolor : color) (g : t) (period : periodicity) : t =
      match period with
      | Period1 ((axis,p),k_ar) ->
         let f_axis = eval_axis axis in
         let g' = copy g in
         iter_pixels
           (fun i j c ->
             if c = bgcolor then ( (* only modifying bgcolor pixels *)
               let _, _, c_k = k_ar.(f_axis i j mod p) in
               if c_k <> no_color then set_pixel g' i j c_k))
           g;
         g'
      | Period2 ((axis1,p1),(axis2,p2),k2_ar) ->
         let f_axis1 = eval_axis axis1 in
         let f_axis2 = eval_axis axis2 in
         let g' = copy g in
         iter_pixels
           (fun i j c ->
             if c = bgcolor then ( (* only modifying bgcolor pixels *)
               let _, _, c_k = k2_ar.(f_axis1 i j mod p1).(f_axis2 i j mod p2) in
               if c_k <> no_color then set_pixel g' i j c_k))
           g;
         g'
      
    let fill_alike (total : bool) (bgcolor : color) (g : t) : t result =
      let periods = periodicities bgcolor g in
      match periods with
      | [] -> Result.Error (Undefined_result "Grid.Transf.fill_alike: no periodicity")
      | period0::next ->
         if total
         then
           if periodicity_is_total period0
           then Result.Error (Undefined_result "Grid.Transf.fill_alike: first period ok")
           else
             match List.find_opt periodicity_is_total next with
             | None -> Result.Error (Undefined_result "Grid.Transf.fill_alike: no total periodicity")
             | Some period -> Result.Ok (fill_with_periodicity bgcolor g period)
         else Result.Ok (fill_with_periodicity bgcolor g period0)
    let fill_alike, reset_fill_alike =
      Common.memoize3 ~size:101 fill_alike
      
    (* cropping *)

    let crop g offset_i offset_j new_h new_w =
      let h, w = dims g in
      if offset_i >= 0 && offset_i < h
         && offset_j >= 0 && offset_j < w
         && new_h > 0 && new_w > 0
         && offset_i + new_h < h
         && offset_j + new_w < w
      then
        let res = make new_h new_w black in
        for i = 0 to new_h - 1 do
          for j = 0 to new_w - 1 do
            let c = get_pixel ~source:"crop" g (offset_i + i) (offset_j + j) in
            if c <> black then
              set_pixel res i j c
          done
        done;
        Result.Ok res
      else Result.Error (Undefined_result "Grid.Transf.crop")
    let crop, reset_crop =
      let f, reset = Common.memoize ~size:101 (fun (g,i,j,h,w) -> crop g i j h w) in
      (fun g i j h w -> f (g,i,j,h,w)), reset

    let strip (g : t) : t result = (* croping on anything else than the majority color, the remaining majority color is made black *)
      let c_strip = majority_color g in
      let h, w = dims g in
      let min_i, max_i = ref h, ref (-1) in
      let min_j, max_j = ref w, ref (-1) in
      iter_pixels
        (fun i j c ->
          if c <> c_strip then (
            min_i := min i !min_i;
            max_i := max i !max_i;
            min_j := min j !min_j;
            max_j := max j !max_j))
        g;
      if !min_i < 0 (* grid is c_strip only *)
      then Result.Error (Undefined_result "monocolor grid")
      else
        let| g' =
          if c_strip <> black
          then swap_colors g c_strip black
          else Result.Ok g in
        let| g' = crop g' !min_i !min_j (!max_i - !min_i + 1) (!max_j - !min_j + 1) in
        Result.Ok g'
    let strip, reset_strip =
      Common.memoize ~size:101 strip
      
    (* concatenating *)
      
    let concatHeight g1 g2 : t result =
      let h1, w1 = dims g1 in
      let h2, w2 = dims g2 in
      if w1 <> w2 then Result.Error Invalid_dim
      else if h1+h2 > max_size then Result.Error (Undefined_result "concatHeight: result grid too large")
      else (
        let res = make (h1+h2) w1 black in
        iter_pixels
          (fun i1 j1 c1 -> set_pixel res i1 j1 c1)
          g1;
        iter_pixels
          (fun i2 j2 c2 -> set_pixel res (h1+i2) j2 c2)
          g2;
        Result.Ok res)
    let concatHeight, reset_concatHeight =
      Common.memoize2 ~size:101 concatHeight
      
    let concatWidth g1 g2 : t result =
      let h1, w1 = dims g1 in
      let h2, w2 = dims g2 in
      if h1 <> h2 then Result.Error Invalid_dim
      else if w1+w2 > max_size then Result.Error (Undefined_result "concatWidth: result grid too large")
      else (
        let res = make h1 (w1+w2) black in
        iter_pixels
          (fun i1 j1 c1 -> set_pixel res i1 j1 c1)
          g1;
        iter_pixels
          (fun i2 j2 c2 -> set_pixel res i2 (w1+j2) c2)
          g2;
        Result.Ok res)
    let concatWidth, reset_concatWidth =
      Common.memoize2 ~size:101 concatWidth

    let concatHeightWidth g1 g2 g3 g4 : t result (* top left, top right, bottom left, bottom right *) =
      let| g12 = concatWidth g1 g2 in
      let| g34 = concatWidth g3 g4 in
      concatHeight g12 g34

    (* TODO: selecting halves and quarters *)

    let compose (m1 : Mask.t) (g2 : t) : t result = (* repeating g2 for each non-black pixel of g1 *)
      let h1, w1 = Mask.dims m1 in
      let h2, w2 = dims g2 in
      let h, w = h1*h2, w1*w2 in
      if h > max_size || w > max_size
      then Result.Error (Undefined_result "compose: result grid too large")
      else (
        let res = make h w black in
        Mask.iter
          (fun i1 j1 ->
            iter_pixels
              (fun i2 j2 c2 ->
                if c2 <> black then
                  set_pixel res (i1*h2+i2) (j1*w2+j2) c2)
              g2)
          m1;
        Result.Ok res)
    let compose, reset_compose =
      Common.memoize2 ~size:101 compose

    (* symmetrization *)

    let layers ?(bgcolor = black) gs : t result =
      match gs with
      | [] -> Result.Error (Invalid_argument "Grid.Transf.layers: empty list")
      | g1::gs1 ->
         let h1, w1 = dims g1 in
         if List.for_all (fun gi -> dims gi = (h1,w1)) gs1
         then (
           let res = make h1 w1 bgcolor in
           List.iter
             (fun gi ->
               iter_pixels
                 (fun i j c ->
                   if c <> bgcolor then set_pixel res i j c)
                 gi)
             gs;
           Result.Ok res)
         else Result.Error Invalid_dim
    let layers, reset_layers =
      let f, reset = Common.memoize ~size:101 (fun (bgcolor,gs) -> layers ~bgcolor gs) in
      (fun ?(bgcolor = black) gs -> f (bgcolor,gs)), reset
      
    let sym_flipHeight_inplace g = layers [g; flipHeight g]
    let sym_flipWidth_inplace g = layers [g; flipWidth g]
    let sym_rotate180_inplace g = layers [g; rotate180 g]
    let sym_flipHeightWidth_inplace g = layers [g; flipHeight g; flipWidth g; rotate180 g]
    let sym_flipDiag1_inplace g =
      if g.height <> g.width
      then Result.Error Invalid_dim
      else layers [g; flipDiag1 g]
    let sym_flipDiag2_inplace g =
      if g.height <> g.width
      then Result.Error Invalid_dim
      else layers [g; flipDiag2 g]
    let sym_flipDiag1Diag2_inplace g =
      if g.height <> g.width
      then Result.Error Invalid_dim
      else layers [g; flipDiag1 g; flipDiag2 g; rotate180 g]
    let sym_rotate90_inplace g =
      if g.height <> g.width
      then Result.Error Invalid_dim
      else layers [g; rotate90 g; rotate180 g; rotate270 g]
    let sym_full_inplace g =
      if g.height <> g.width
      then Result.Error Invalid_dim
      else (* includes all symmetries *)
        let| g' = layers [g; rotate90 g] in
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
      if g.height <> g.width then raise Invalid_dim;
      let g90 = rotate90 g in
      let g180 = rotate180 g in
      let g270 = rotate270 g in
      [ concatHeightWidth g g90 g270 g180;
        concatHeightWidth g270 g g180 g90;
        concatHeightWidth g180 g270 g90 g;
        concatHeightWidth g90 g180 g g270 ]
 *)

    let reset_memoized_functions () =
      reset_mask_of_grid ();
      reset_grid_of_mask ();
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
      reset_factor ();
      reset_resize_alike ();
      reset_periodicities ();
      reset_fill_alike ();
      reset_compose ();
      reset_crop ();
      reset_strip ();
      reset_concatHeight ();
      reset_concatWidth ();
      reset_compose ();
      reset_layers ()
      
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

let background_colors (g : t) : color list = (* QUICK, in decreasing frequency order *)
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
  else l @ [black] (* ensure black is considered as a background color *)

  
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
        mask_models =
          (`Full ::
             if (height = 2 && width >= 2 || width = 2 && height >= 2)
             then [`Border]
             else []);
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
  Transf.reset_memoized_functions ();
  Mask_model.reset_memoized_functions ();
  reset_segment_by_color ();
  reset_points ();
  (*  reset_rectangles_of_part ();*)
  reset_rectangles ()
