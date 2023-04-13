
let memoize_size = 101

open Arc_common

type t =
  [ `Full (* all pixels on *)
  | `Border (* width-1 border *)
  | `EvenCheckboard
  | `OddCheckboard
  | `PlusCross
  | `TimesCross
  ]
      
let xp (print : Xprint.t) : t -> unit = function
  | `Full -> print#string "Full"
  | `Border -> print#string "Border"
  | `EvenCheckboard -> print#string "Even Checkboard"
  | `OddCheckboard -> print#string "Odd Checkboard"
  | `PlusCross -> print#string "+-cross"
  | `TimesCross -> print#string "x-cross"

let area ~height ~width = function
  | `Full -> height * width
  | `Border -> 2 * (height + width) - 4
  | `EvenCheckboard -> (height * width + 1) / 2
  | `OddCheckboard -> height * width / 2
  | `PlusCross -> height + width - 1
  | `TimesCross -> height + width - (height mod 2)
  
let mem ~height ~width i j = (* mask height and width, relative position (i,j) *)
  function
  | `Full -> true
  | `Border -> i=0 || j=0 || i=height-1 || j=width-1
  | `EvenCheckboard -> (i+j) mod 2 = 0
  | `OddCheckboard -> (i+j) mod 2 = 1
  | `PlusCross -> (i=height/2 || i=(height-1)/2) || (j=width/2 || j=(width-1)/2)
  | `TimesCross -> height=width && (i=j || (height-1-i) = j)

let matches (m : Grid.t) (mm : t) : bool =
  let h, w = Grid.dims m in
  Grid.for_all_pixels
    (fun i j c -> (c = Grid.Mask.one) = mem ~height:h ~width:w i j mm)
    m
let matches, reset_matches =
  Memo.memoize2
    ~equal:(fun (m,mm) (m',mm') -> m == m' && mm = mm')
    ~size:103 matches
                 
let to_mask ~height ~width (mm : t) : Grid.t =
  let res = Grid.Mask.empty height width in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if mem ~height ~width i j mm then
        Grid.Mask.set res i j
    done
  done;
  res
  
let from_box_in_bmp ?visible_bmp ~mini ~maxi ~minj ~maxj (bmp : Bitmap.t) : Grid.t * t list =
  let height, width = maxi-mini+1, maxj-minj+1 in
  let is_visible =
    match visible_bmp with
    | None -> (fun absi absj -> true)
    | Some m -> (fun absi absj -> Bitmap.mem absi absj m)
  in
  let m = ref (Bitmap.empty height width) in (* bmp over the part box *)
  let models = ref [`Full; `Border; `EvenCheckboard; `OddCheckboard; `PlusCross; `TimesCross] in
  for absi = mini to maxi do
    let i = absi - mini in
    for absj = minj to maxj do
      let j = absj - minj in
      let hidden = not (is_visible absi absj) in
      let pixel_on = Bitmap.mem absi absj bmp in
      if pixel_on then m := Bitmap.add i j !m;
      models :=
        List.filter
          (fun model -> hidden || pixel_on = mem ~height ~width i j model)
          !models;
    done
  done;
  (Grid.Mask.from_bitmap !m,
   !models)
  
let from_bmp (bmp : Bitmap.t) : Grid.t * t list =
  from_box_in_bmp
    ~mini:0 ~maxi:(Bitmap.height bmp - 1)
    ~minj:0 ~maxj:(Bitmap.width bmp - 1)
    bmp
  
let symmetry (f : Grid.t -> Grid.t) : t -> t result = function
  | (`Full | `Border | `TimesCross | `PlusCross as mm) -> Result.Ok mm
  | _ -> Result.Error (Undefined_result "Grid.Mask_model.symmetry: undefined")
       
let flipHeight = symmetry Grid.Transf.flipHeight
let flipHeight, reset_flipHeight =
  Memo.memoize
    ~equal:(==)
    ~size:memoize_size flipHeight
  
let flipWidth = symmetry Grid.Transf.flipWidth
let flipWidth, reset_flipWidth =
  Memo.memoize
    ~equal:(==)
    ~size:memoize_size flipWidth
  
let flipDiag1 = symmetry Grid.Transf.flipDiag1
let flipDiag1, reset_flipDiag1 =
  Memo.memoize
    ~equal:(==)
    ~size:memoize_size flipDiag1
  
let flipDiag2 = symmetry Grid.Transf.flipDiag2
let flipDiag2, reset_flipDiag2 =
  Memo.memoize
    ~equal:(==)
    ~size:memoize_size flipDiag2
  
let rotate180 = symmetry Grid.Transf.rotate180
let rotate180, reset_rotate180 =
  Memo.memoize
    ~equal:(==)
    ~size:memoize_size rotate180
  
let rotate90 = symmetry Grid.Transf.rotate90
let rotate90, reset_rotate90 =
  Memo.memoize
    ~equal:(==)
    ~size:memoize_size rotate90
    
let rotate270 = symmetry Grid.Transf.rotate270
let rotate270, reset_rotate270 =
  Memo.memoize
    ~equal:(==)
    ~size:memoize_size rotate270
  
      
let reset_memoized_functions () =
  reset_matches ();
  reset_flipHeight ();
  reset_flipWidth ();
  reset_flipDiag1 ();
  reset_flipDiag2 ();
  reset_rotate180 ();
  reset_rotate90 ();
  reset_rotate270 ()
