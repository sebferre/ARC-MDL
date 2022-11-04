(* grid bitmaps *)

(* module type BITMAP =
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

  end *)

(* SEE mask_core_alt.ml for alternative implementations *)
(* only MaskCoreZ is a bit more efficient in native code but catastrophic in JS code *)

open Bigarray

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
