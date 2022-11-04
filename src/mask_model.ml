
open Arc_common

type t =
  [ `Mask of Grid.t (* only monocolor grids *)
  | `Full (* all pixels on *)
  | `Border (* width-1 border *)
  | `EvenCheckboard
  | `OddCheckboard
  | `PlusCross
  | `TimesCross
  ]
      
let subsumes (mm1 : t) (mm2 : t) : bool =
  match mm1, mm2 with
  | `Mask m1, `Mask m2 -> Grid.same m1 m2
  | _ -> mm1 = mm2
       
let area ~height ~width = function
  | `Mask m -> Grid.Mask.area m
  | `Full -> height * width
  | `Border -> 2 * (height + width) - 4
  | `EvenCheckboard -> (height * width + 1) / 2
  | `OddCheckboard -> height * width / 2
  | `PlusCross -> height + width - 1
  | `TimesCross -> height + width - (height mod 2)
  
let mem ~height ~width i j = (* mask height and width, relative position (i,j) *)
  function
  | `Mask m -> Grid.Mask.mem i j m
  | `Full -> true
  | `Border -> i=0 || j=0 || i=height-1 || j=width-1
  | `EvenCheckboard -> (i+j) mod 2 = 0
  | `OddCheckboard -> (i+j) mod 2 = 1
  | `PlusCross -> (i=height/2 || i=(height-1)/2) || (j=width/2 || j=(width-1)/2)
  | `TimesCross -> height=width && (i=j || (height-1-i) = j)
                 
let to_mask ~height ~width (mm : t) : Grid.t =
  let res = Grid.Mask.empty height width in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if mem ~height ~width i j mm then
        Grid.Mask.set res i j
    done
  done;
  res
  
let from_box_in_bmp ?visible_bmp ~mini ~maxi ~minj ~maxj (bmp : Bitmap.t) : t list =
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
  !models @ [`Mask (Grid.Mask.from_bitmap !m)] (* still considering as raw mask for allowing some computations such as scaling *)
  
let from_bmp (bmp : Bitmap.t) : t list =
  from_box_in_bmp
    ~mini:0 ~maxi:(Bitmap.height bmp - 1)
    ~minj:0 ~maxj:(Bitmap.width bmp - 1)
    bmp
  
let scale_up k l : t -> t result = function
  | `Mask m ->
     let| m' = Grid.Transf.scale_up k l m in
     Result.Ok (`Mask m')
  | (`Full as mm) -> Result.Ok mm
  | mm -> Result.Error (Undefined_result "Grid.Mask_model.scale_up: undefined")
let scale_up, reset_scale_up =
  Common.memoize3 ~size:101 scale_up
  
let scale_down k l : t -> t result = function
  | `Mask m ->
     let| m' = Grid.Transf.scale_down k l m in
     Result.Ok (`Mask m')
  | (`Full as mm) -> Result.Ok mm
  | mm -> Result.Error (Undefined_result "Grid.Mask_model.scale_down: undefined")
let scale_down, reset_scale_down =
  Common.memoize3 ~size:101 scale_down
  
let scale_to new_h new_w : t -> t result = function
  | `Mask m ->
     let| m' = Grid.Transf.scale_to new_h new_w m in
     Result.Ok (`Mask m')
  | (`Full as mm) -> Result.Ok mm
  | mm -> Result.Error (Undefined_result "Grid.Mask_model.scale_to: undefined")
let scale_to, reset_scale_to =
  Common.memoize3 ~size:101 scale_to
  
let tile k l : t -> t result = function
  | `Mask m ->
     let| m' = Grid.Transf.tile k l m in
     Result.Ok (`Mask m')
  | `Full -> Result.Ok `Full
  | _ -> Result.Error (Undefined_result "Grid.Mask_model.tile: undefined")
let tile, reset_tile =
  Common.memoize3 ~size:101 tile
  
let resize_alike new_h new_w : t -> t result = function
  | `Mask m ->
     let| m' = Grid.Transf.resize_alike new_h new_w m in
     Result.Ok (`Mask m')
  | (`Full | `OddCheckboard | `EvenCheckboard as mm) -> Result.Ok mm
  | _ -> Result.Error (Undefined_result "Grid.Mask_model.resize_alike: undefined")
let resize_alike, reset_resize_alike =
  Common.memoize3 ~size:101 resize_alike
  
let compose (m1 : Grid.t) (mm2 : t) : t result =
  match mm2 with
  | `Mask m2 ->
     let| m = Grid.Transf.compose m1 m2 in
     Result.Ok (`Mask m)
  | _ -> Result.Error (Undefined_result "Grid.Mask_model.compose: undefined")
let compose, reset_compose =
  Common.memoize2 ~size:101 compose
      
let symmetry (f : Grid.t -> Grid.t) : t -> t result = function
  | `Mask m -> Result.Ok (`Mask (f m))
  | (`Full | `Border | `TimesCross | `PlusCross as mm) -> Result.Ok mm
  | _ -> Result.Error (Undefined_result "Grid.Mask_model.symmetry: undefined")
       
let flipHeight = symmetry Grid.Transf.flipHeight
let flipHeight, reset_flipHeight =
  Common.memoize ~size:101 flipHeight
  
let flipWidth = symmetry Grid.Transf.flipWidth
let flipWidth, reset_flipWidth =
  Common.memoize ~size:101 flipWidth
  
let flipDiag1 = symmetry Grid.Transf.flipDiag1
let flipDiag1, reset_flipDiag1 =
  Common.memoize ~size:101 flipDiag1
  
let flipDiag2 = symmetry Grid.Transf.flipDiag2
let flipDiag2, reset_flipDiag2 =
  Common.memoize ~size:101 flipDiag2
  
let rotate180 = symmetry Grid.Transf.rotate180
let rotate180, reset_rotate180 =
  Common.memoize ~size:101 rotate180
  
let rotate90 = symmetry Grid.Transf.rotate90
let rotate90, reset_rotate90 =
  Common.memoize ~size:101 rotate90
    
let rotate270 = symmetry Grid.Transf.rotate270
let rotate270, reset_rotate270 =
  Common.memoize ~size:101 rotate270
  
let inter mm1 mm2 : t result =
  match mm1, mm2 with
  | `Full, _ -> Result.Ok mm2
  | _, `Full -> Result.Ok mm1
  | `Mask m1, `Mask m2 when Grid.dims m1 = Grid.dims m2 ->
     Result.Ok (`Mask (Grid.Mask.inter m1 m2))
  | _ -> Result.Error (Undefined_result "Mask_model.inter: undefined")
let inter, reset_inter =
  Common.memoize2 ~size:101 inter
  
let union mm1 mm2 : t result =
  match mm1, mm2 with
  | `Full, _ -> Result.Ok mm1
  | _, `Full -> Result.Ok mm2
  | `Mask m1, `Mask m2 when Grid.dims m1 = Grid.dims m2 ->
     Result.Ok (`Mask (Grid.Mask.union m1 m2))
  | _ -> Result.Error (Undefined_result "Mask_model.union: undefined")
let union, reset_union =
  Common.memoize2 ~size:101 union
  
let diff_sym (mm1 : t) (mm2 : t) : t result =
  match mm1, mm2 with
  | `Full, `Mask m2 -> Result.Ok (`Mask (Grid.Mask.compl m2))
  | `Mask m1, `Full -> Result.Ok (`Mask (Grid.Mask.compl m1))
  | `Mask m1, `Mask m2 when Grid.dims m1 = Grid.dims m2 ->
     Result.Ok (`Mask (Grid.Mask.diff_sym m1 m2))
  | _ -> Result.Error (Undefined_result "Mask_model.diff_sym: undefined")
let diff_sym, reset_diff_sym =
  Common.memoize2 ~size:101 diff_sym
  
let diff (mm1 : t) (mm2 : t) : t result =
  match mm1, mm2 with
  | `Full, `Mask m2 -> Result.Ok (`Mask (Grid.Mask.compl m2))
  | `Mask m1, `Mask m2 when Grid.dims m1 = Grid.dims m2 ->
     Result.Ok (`Mask (Grid.Mask.diff m1 m2))
  | _ -> Result.Error (Undefined_result "Mask_model.diff: undefined")
let diff, reset_diff =
  Common.memoize2 ~size:101 diff
  
let compl (mm1 : t) : t result =
  match mm1 with
  | `Mask m1 -> Result.Ok (`Mask (Grid.Mask.compl m1))
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

