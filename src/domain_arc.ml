
open Madil_common
open Arc_common

module GPat = Grid_patterns

let () = Printexc.record_backtrace true
   
module Basic_types (* : Madil.BASIC_TYPES *) =
  struct

    (* generic printers *)

    let xp_bool ~html print b =
      xp_html_elt "span" ~classe:"arc-bool" ~html print
        (fun () -> print#string (if b then "true" else "false"))
                              
    let xp_int ~html print i =
      xp_html_elt "span" ~classe:"arc-int" ~html print
        (fun () -> print#int i)
    
    let xp_vec xp_i xp_j ~html print i j =
      xp_tuple2 xp_i xp_j ~html print (i,j) 

    let xp_obj ~html print i j g =
      print#string "an object "; Grid.xp_grid ~html print g;
      print#string " at position "; xp_vec xp_int xp_int ~html print i j
      
    (* values *)

    type value =
      [ `Null
      | `Bool of bool
      | `Int of int
      | `Vec of int * int
      | `Color of Grid.color
      | `Motif of GPat.Motif.t
      | `Grid of Grid.t
      | `Obj of int * int * Grid.t (* position at (i,j) of the subgrid *)
      | `Map of (value,value) Mymap.t
      | `Seq of value array ]

    let rec xp_value ~html (print : Xprint.t) : value -> unit = function
      | `Null -> print#string "null"
      | `Bool b -> xp_bool ~html print b
      | `Int i -> xp_int ~html print i
      | `Vec (i,j) -> xp_vec xp_int xp_int ~html print i j
      | `Color c -> Grid.xp_color ~html print c
      | `Motif motif -> GPat.Motif.xp ~html print motif
      | `Grid g -> Grid.xp_grid ~html print g
      | `Obj (i,j,g) -> xp_obj ~html print i j g
      | `Map m ->
         print#string "{";
         let _ =
           Mymap.fold
             (fun x y first ->
               if not first then print#string ", ";
               xp_value ~html print x;
               print#string " -> ";
               xp_value ~html print y;
               false)
             m true in
         print#string "}"
      | `Seq vs -> xp_array xp_value ~html print vs

    let value_of_json (* : Yojson.Safe.t -> value *) = function
      | `List (`List row::_ as rows) ->
         let height = List.length rows in
         let width = List.length row in
         let grid = Grid.make height width 0 in
         List.iteri
           (fun i ->
	     function
	     | `List cells ->
	        List.iteri
	          (fun j ->
	            function
	            | `Int col -> Grid.Do.set_pixel grid i j col
	            | _ -> invalid_arg "Invalid JSON grid color")
	          cells
	     | _ -> invalid_arg "Invalid JSON grid row")
           rows;
         `Grid grid
      | _ -> invalid_arg "Invalid JSON grid"

    let json_of_value : value -> Yojson.Safe.t = function
      | `Grid grid ->
         let open Bigarray in
         let n1, n2 = grid.height, grid.width in
         let rows =
           Common.fold_for_down
             (fun i res ->
               let row =
                 Common.fold_for_down
                   (fun j row ->
                     `Int (Array2.get grid.matrix i j) :: row)
                   (n2 - 1) 0 [] in
               `List row :: res)
             (n1 - 1) 0 [] in
         `List rows
      | _ -> invalid_arg "JSON only defined for grid values"
           
    (* model types *)

    type typ =
      | BOOL
      | INT of typ_int
      | VEC of typ_vec
      | COLOR of typ_color
      | MOTIF
      | GRID of typ_grid
      | OBJ of typ_grid
      | MAP of typ * typ
    (* list of values have the type of their elts *)
    and typ_int =
      | CARD
      | COORD of typ_axis * typ_vec
    and typ_axis =
      | I
      | J
    and typ_vec =
      | POS
      | SIZE
      | MOVE
    and typ_color =
      | C_BG of bool (* full *) (* background color *)
      | C_OBJ (* object color *)
    and typ_grid =
      [`Full | `Sprite | `Noise]
      * bool (* no-color, i.e. black and transparent *)
    (* MASK = GRID (`Sprite,true), (`Sprite,false) is the more general case *)

    let typ_bool = BOOL
      
    let nb_typ_axis = 2
    let nb_typ_vec = 3
      
    let rec xp_typ ~html print = function
      | BOOL -> print#string "BOOL"
      | INT ti -> xp_typ_int ~html print ti
      | VEC tv -> xp_typ_vec ~html print tv
      | COLOR tc -> print#string "COLOR"; xp_typ_color ~html print tc
      | MOTIF -> print#string "MOTIF"
      | GRID tg -> xp_typ_grid ~html print tg
      | OBJ tg -> print#string "OBJ "; xp_typ_grid ~html print tg
      | MAP (ta,tb) -> xp_typ ~html print ta; print#string " -> "; xp_typ ~html print tb
    and xp_typ_int ~html print = function
      | CARD -> print#string "CARD"
      | COORD (ij,tv) ->
         xp_typ_vec ~html print tv;
         print#string (match ij with I -> ".I" | J -> ".J")
    and xp_typ_vec ~html print = function
      | POS -> print#string "POS"
      | SIZE -> print#string "SIZE"
      | MOVE -> print#string "MOVE"
    and xp_typ_color ~html print = function
      | C_BG full -> print#string "_BG"; if not full then print#string "_TR"
      | C_OBJ -> print#string "_OBJ"
    and xp_typ_grid ~html print (filling,nocolor) =
      print#string
        (match filling, nocolor with
         | `Full, _ -> "GRID"
         | `Sprite, false -> "SPRITE"
         | `Sprite, true -> "MASK"
         | `Noise, false -> "NOISE"
         | `Noise, true -> "NOISE_MASK")

    (* model vars *)
      
    type var = int
             
    let xp_var ~html print x =
      xp_html_elt "span" ~classe:"model-var" ~html print
        (fun () -> print#string "$"; print#int x)

    (* model constr *)

    type segmentation = [`Default | `SameColor]
      
    type constr =
      | AnyCoord (* COORD *)
      | Vec (* COORD, COORD : VEC *)
      | AnyColor (* COLOR *)
      | AnyMotif (* MOTIF *)
      | AnyGrid (* GRID *)
      | Obj (* POS, SPRITE : OBJ *)
      | AnyMap (* MAP(A,B) *)
      | DomMap of value array (* B+ : MAP(A,B) *) (* fixed set of keys, assumed known from ctx *)
      | Replace (* A, A : MAP(A,A) *)
      | Swap (* A, A : MAP(A,A) *)
      | BgColor (* COLOR, SPRITE : GRID *)
      | IsFull (* SPRITE : GRID *)
      | Crop (* SPRITE ; POS, SIZE : SPRITE *)
      | Objects of segmentation * int (* SIZE, OBJ+ : SPRITE *) (* int is for max seq length *)
      | Monocolor (* COLOR, MASK : SPRITE *)
      (*       | Recoloring (* COLOR+, SPRITE : SPRITE *) *)
      | Recoloring (* SPRITE; MAP(COLOR,COLOR) : SPRITE *)
      | Motif (* MOTIF, SPRITE (core), SPRITE (noise) *)
      | Empty (* SIZE : MASK *)
      | Full (* SIZE : MASK *)
      | Point (* MASK *)

    let xp_any ~html print () =
      xp_html_elt "span" ~classe:"model-any" ~html print
        (fun () -> print#string "?")
    let xp_obj xp_pos xp_sprite ~html print () =
      print#string "at position "; xp_pos ~html print ();
      print#string ": ";
      xp_sprite ~html print ()
    let xp_dommap keys xp_vals ~html print () =
      xp_array ~delims:("〈","〉") xp_value ~html print keys;
      print#string " -> ";
      xp_vals ~html print ()
    let xp_replace xp_a xp_b ~html print () =
      xp_a ~html print ();
      print#string " is replaced by ";
      xp_b ~html print ()
    let xp_swap xp_a xp_b ~html print () =
      xp_a ~html print ();
      print#string " is swapped with ";
      xp_b ~html print ()
    let xp_bgcolor xp_color xp_sprite ~html print () =
      print#string "a grid with background color "; xp_color ~html print ();
      print#string " and with contents"; xp_newline ~html print ();
      xp_sprite ~html print ()
    let xp_isfull xp_sprite ~html print () =
      print#string "a full grid that is";
      xp_newline ~html print ();
      xp_sprite ~html print ()
    let xp_crop xp_sprite xp_pos xp_size ~html print () =
      print#string "the crop of "; xp_sprite ~html print ();
      print#string " at position "; xp_pos ~html print ();
      print#string " with size "; xp_size ~html print ()
      (*print#string "a grid of size "; xp_size ~html print ();
      print#string " that contains at position "; xp_pos ~html print ();
      xp_newline ~html print ();
      xp_sprite ~html print ()*)
    let xp_objects (seg : segmentation) (nmax : int) xp_size xp_objs ~html print () =
      print#string "a grid of size "; xp_size ~html print ();
      print#string " that contains at most "; print#int nmax;
      print#string
        (match seg with
         | `Default -> ""
         | `SameColor -> " same-color");
      print#string " objects like";
      xp_newline ~html print ();
      xp_objs ~html print ()
    let xp_monocolor xp_color xp_mask ~html print () =
      print#string "a grid with only color "; xp_color ~html print ();
      print#string " and with mask"; xp_newline ~html print ();
      xp_mask ~html print ()
(*    let xp_recoloring xp_colors xp_grid ~html print () =
      print#string "recoloring with "; xp_colors ~html print ();
      xp_newline ~html print ();
      xp_grid ~html print () *)
    let xp_recoloring xp_grid xp_map ~html print () =
      print#string "a recoloring of "; xp_grid ~html print ();
      xp_newline ~html print ();
      print#string "where "; xp_map ~html print ()
    let xp_motif xp_mot xp_core xp_noise ~html print () =
      print#string "a grid with motif "; xp_mot ~html print ();
      print#string "  and with core:";
      xp_newline ~html print ();
      xp_core ~html print ();
      print#string "  plus noise:";
      xp_newline ~html print ();
      xp_noise ~html print ()
    let xp_empty xp_size ~html print () =
      print#string "an empty mask of size "; xp_size ~html print ()
    let xp_full xp_size ~html print () =
      print#string "a full mask of size "; xp_size ~html print ()
    let xp_point ~html print () =
      print#string "a point mask"
      
    let xp_pat c xp_args ~html print () =
      match c, xp_args with
      | AnyCoord, [||] -> xp_any ~html print ()
      | Vec, [|xp_i; xp_j|] -> xp_vec xp_i xp_j ~html print () ()
      | AnyColor, [||] -> xp_any ~html print ()
      | AnyMotif, [||] -> xp_any ~html print ()
      | AnyGrid, [||] -> xp_any ~html print ()
      | Obj, [|xp_pos; xp_sprite|] -> xp_obj xp_pos xp_sprite ~html print ()
      | AnyMap, [||] -> xp_any ~html print ()
      | DomMap keys, [|xp_vals|] -> xp_dommap keys xp_vals ~html print ()
      | Replace, [|xp_a; xp_b|] -> xp_replace xp_a xp_b ~html print ()
      | Swap, [|xp_a; xp_b|] -> xp_swap xp_a xp_b ~html print ()
      | BgColor, [|xp_color; xp_sprite|] ->
         xp_bgcolor xp_color xp_sprite ~html print ()
      | IsFull, [|xp_sprite|] ->
         xp_isfull xp_sprite ~html print ()
      | Crop, [|xp_sprite; xp_pos; xp_size|] ->
         xp_crop xp_sprite xp_pos xp_size ~html print ()
      | Objects (seg,nmax), [|xp_size; xp_objs|] ->
         xp_objects seg nmax xp_size xp_objs ~html print ()
      | Monocolor, [|xp_color; xp_mask|] ->
         xp_monocolor xp_color xp_mask ~html print ()
(*      | Recoloring, [|xp_colors; xp_grid|] ->
         xp_recoloring xp_colors xp_grid ~html print () *)
      | Recoloring, [|xp_grid; xp_map|] ->
         xp_recoloring xp_grid xp_map ~html print ()
      | Motif, [|xp_mot; xp_core; xp_noise|] ->
         xp_motif xp_mot xp_core xp_noise ~html print ()
      | Empty, [|xp_size|] ->
         xp_empty xp_size ~html print ()
      | Full, [|xp_size|] ->
         xp_full xp_size ~html print ()
      | Point, [||] ->
         xp_point ~html print ()
      | _ -> assert false

    let xp_field ~html print = function
      | AnyCoord, _ -> assert false
      | Vec, 0 -> print#string "i"
      | Vec, 1 -> print#string "j"
      | Vec, _ -> assert false
      | AnyColor, _ -> assert false
      | AnyMotif, _ -> assert false
      | AnyGrid, _ -> assert false
      | Obj, 0 -> print#string "pos"
      | Obj, 1 -> print#string "sprite"
      | Obj, _ -> assert false
      | AnyMap, _ -> assert false
      | DomMap _, 0 -> print#string "vals"
      | DomMap _, _ -> assert false
      | Replace, 0 -> print#string "a"
      | Replace, 1 -> print#string "b"
      | Replace, _ -> assert false
      | Swap, 0 -> print#string "a"
      | Swap, 1 -> print#string "b"
      | Swap, _ -> assert false
      | BgColor, 0 -> print#string "color"
      | BgColor, 1 -> print#string "sprite"
      | BgColor, _ -> assert false
      | IsFull, _ -> print#string "sprite"
      | Crop, 0 -> print#string "sprite"
      | Crop, 1 -> print#string "pos"
      | Crop, 2 -> print#string "size"
      | Crop, _ -> assert false
      | Objects _, 0 -> print#string "size"
      | Objects _, 1 -> print#string "obj"
      | Objects _, _ -> assert false
      | Monocolor, 0 -> print#string "color"
      | Monocolor, 1 -> print#string "mask"
      | Monocolor, _ -> assert false
(*      | Recoloring, 0 -> print#string "color"
      | Recoloring, 1 -> print#string "sprite"
      | Recoloring, _ -> assert false *)
      | Recoloring, 0 -> print#string "grid"
      | Recoloring, 1 -> print#string "colormap"
      | Recoloring, _ -> assert false
      | Motif, 0 -> print#string "motif"
      | Motif, 1 -> print#string "core"
      | Motif, 2 -> print#string "noise"
      | Motif, _ -> assert false
      | Empty, _ -> print#string "size"
      | Full, _ -> print#string "size"
      | Point, _ -> assert false

    (* data constr *)
                  
    type dconstr = (* make sure data from constant models can be identified as so *)
      | DAnyCoord of int * Range.t (* COORD in some range *)
      | DVec (* COORD, COORD : VEC *)
      | DAnyColor of Grid.color * typ_color (* COLOR *)
      | DAnyMotif of GPat.Motif.t (* MOTIF *)
      | DAnyGrid of Grid.t * typ_grid * Range.t (* height *) * Range.t (* width *) * int (* nb colors *) (* GRID of some type, with some size ranges, and some nb of concrete  colors *)
      | DObj (* SIZE, SPRITE : OBJ *)
      | DAnyMap of (value,value) Mymap.t * typ (* range type *) (* assuming domain known from context *) (* TODO: missing range constraints *)
      | DDomMap of value array (* B+ : MAP(A,B) *)
      | DReplace (* A, A : MAP(A,A) *)
      | DSwap (* A, A : MAP(A,A) *)
      | DBgColor (* COLOR, SPRITE : GRID *)
      | DIsFull (* SPRITE : GRID *)
      | DCrop (* SPRITE, POS, SIZE : SPRITE *)
      | DObjects of segmentation * int (* SIZE, OBJ+ : SPRITE *)
      | DMonocolor (* COLOR, MASK : SPRITE *)
      (*      | DRecoloring (* COLOR+, SPRITE : SPRITE *) *)
      | DRecoloring (* SPRITE; MAP(COLOR,COLOR) : SPRITE *)
      | DMotif (* MOTIF, SPRITE (core), SPRITE (noise) *)
      | DEmpty (* SIZE : MASK *)
      | DFull (* SIZE : MASK *)
      | DPoint (* MASK *)

    let xp_dpat dc xp_args ~html print () =
      match dc, xp_args with (* TODO: consider printing other params for better introspection *)
      | DAnyCoord (ij,_), [||] -> print#int ij
      | DVec, [|xp_i; xp_j|] -> xp_vec xp_i xp_j ~html print () ()
      | DAnyColor (c,_), [||] -> Grid.xp_color ~html print c
      | DAnyMotif motif, [||] -> GPat.Motif.xp ~html print motif
      | DAnyGrid (g,tg,_,_,_), [||] -> Grid.xp_grid ~html print g
      | DObj, [|xp_pos; xp_sprite|] -> xp_obj xp_pos xp_sprite ~html print ()
      | DAnyMap (m,tb), [||] -> xp_value ~html print (`Map m)
      | DDomMap keys, [|xp_vals|] -> xp_dommap keys xp_vals ~html print ()
      | DReplace, [|xp_a; xp_b|] ->
         xp_replace xp_a xp_b ~html print ()
      | DSwap, [|xp_a; xp_b|] ->
         xp_swap xp_a xp_b ~html print ()
      | DBgColor, [|xp_color; xp_sprite|] ->
         xp_bgcolor xp_color xp_sprite ~html print ()
      | DIsFull, [|xp_sprite|] ->
         xp_isfull xp_sprite ~html print ()
      | DCrop, [|xp_sprite; xp_pos; xp_size|] ->
         xp_crop xp_sprite xp_pos xp_size ~html print ()
      | DObjects (seg,nmax), [|xp_size; xp_obj|] ->
         xp_objects seg nmax xp_size xp_obj ~html print ()
      | DMonocolor, [|xp_color; xp_mask|] ->
         xp_monocolor xp_color xp_mask ~html print ()
(*      | DRecoloring, [|xp_color; xp_grid|] ->
         xp_recoloring xp_color xp_grid ~html print () *)
      | DRecoloring, [|xp_grid; xp_map|] ->
         xp_recoloring xp_grid xp_map ~html print ()
      | DMotif, [|xp_mot; xp_core; xp_noise|] ->
         xp_motif xp_mot xp_core xp_noise ~html print ()
      | DEmpty, [|xp_size|] ->
         xp_empty xp_size ~html print ()
      | DFull, [|xp_size|] ->
         xp_full xp_size ~html print ()
      | DPoint, [||] ->
         xp_point ~html print ()
      | _ -> assert false

    (* functions *)
        
    type func =
      [ `Index_1 of int option list (* on any Ndtree *)
      | `Plus_2 (* on Int, Vec *)
      | `Minus_2 (* on Int, Vec *)
      | `Modulo_2 (* on Int *)
      | `ScaleUp_2 (* on (Int, Vec, Mask, Shape, Grid as T), Card -> T *)
      | `ScaleDown_2 (* on (Int, Vec, Mask, Shape, Grid as T), Card -> T *)
      | `ScaleTo_2 (* Mask, Grid, Vec -> Mask *)
      | `Size_1 (* Grid -> Vec *)
      | `Height_1 (* Grid -> Int *)
      | `Width_1 (* Grid -> Int *)
      | `Crop_2 (* Grid, Rectangle -> Grid *)
      | `Strip_1 (* on Grid *)
      | `Corner_2 (* on Vec *)
      | `Min_n (* on Int, Vec *)
      | `Max_n (* on Int, Vec *)
      | `Average_n (* on Int, Vec *)
      | `Span_2 (* on Vec *)
      | `Norm_1 (* Vec -> Int *)
      | `Diag1_1 of int (* Vec -> Int *)
      | `Diag2_1 of int (* Vec -> Int *)
      | `LogAnd_2 (* on Mask *)
      | `LogOr_2 (* on Mask *)
      | `LogXOr_2 (* on Mask *)
      | `LogAndNot_2 (* on Mask *)
      | `LogNot_1 (* on Mask *)
      | `Stack_n (* on Grids *)
      | `Area_1 (* on Shape *)
      | `Left_1 (* on Layer *)
      | `Right_1 (* on Layer *)
      | `Center_1 (* on Layer *)
      | `Top_1 (* on Layer *)
      | `Bottom_1 (* on Layer *)
      | `Middle_1 (* on Layer *)
      | `ProjI_1 (* on Vec *)
      | `ProjJ_1 (* on Vec *)
      | `MaskOfGrid_1 (* Grid -> Mask TODO: specify bgcolor *)
      | `GridOfMask_2 (* Mask, Color -> Grid *)
      | `TranslationOnto_2 (* Obj, Obj -> Vec *)
      | `Tiling_1 of int * int (* on Vec/Mask/Shape *)
      | `PeriodicFactor_2 of Grid.Transf.periodicity_mode (* on Color, Mask/Shape/Layer/Grid as T -> T *)
      | `FillResizeAlike_3 of Grid.Transf.periodicity_mode (* on Color, Vec, Mask/Shape/Layer/Grid as T -> T *)
      | `SelfCompose_2 (* Color, Mask/Shape/Grid as T, T -> T *)
      | `ApplySymVec_1 of symmetry * typ_vec (* on Vec *)
      | `ApplySymGrid_1 of symmetry (* on Mask, Shape, Layer; type of the argument as computation depends on it *)
      | `UnfoldSym_1 of symmetry list list (* on Mask, Shape, Layer *)
      (* sym list list = matrix to be filled with symmetries of some mask *)
      | `CloseSym_2 of symmetry list (* Color, Mask/Shape/Layer/Grid as T -> T *)
      (* symmetry list = list of symmetries to chain and stack to force some symmetry, taking the given color as transparent *)
      | `TranslationSym_2 of symmetry (* viz Grid *) (* Obj, Obj/Grid -> Vec *)
      | `MajorityColor_1 (* Grid -> Color *)
      | `ColorCount_1 (* Grid -> Int *)
      | `Coloring_2 (* Shape/Obj, Color -> Shape/Obj *)
      | `SwapColors_3 (* Grid, Color, Color -> Grid *)
      ]
    and symmetry = [
      | `Id
      | `FlipHeight | `FlipWidth | `FlipDiag1 | `FlipDiag2
      | `Rotate180 | `Rotate90 | `Rotate270 ]

    let all_symmetry = [
        `Id;
        `FlipHeight; `FlipWidth;
        `FlipDiag1; `FlipDiag2;
        `Rotate180; `Rotate90; `Rotate270
      ]
    let nb_symmetry = List.length all_symmetry

    let all_symmetry_unfold = [
        [[`Id; `FlipWidth]; [`FlipHeight; `Rotate180]];
        [[`Id]; [`FlipHeight]];
        [[`Id; `FlipWidth]];
        [[`Id; `Rotate90]; [`Rotate270; `Rotate180]]
      ] (* TODO: in principle, should add more unfolds following the 10 symmetry groups. See sym_X_unfold in Grid.Transf *)
    let nb_symmetry_unfold = List.length all_symmetry_unfold

    let all_symmetry_close =
      List.rev [ (* preferring stronger symmetries. TODO: do through DL *)
          [`FlipHeight];
          [`FlipWidth];
          [`Rotate180];
          [`FlipDiag1];
          [`FlipDiag2];
          [`FlipHeight; `FlipWidth]; (* entails Rotate180 *)
          [`FlipDiag1; `FlipDiag2]; (* entails Rotate180 *)
          [`Rotate90; `Rotate180]; (* entails Rotate270 *)
          [`FlipHeight; `Rotate90; `Rotate180] (* entails FlipWidth, FlipDiag1, FlipDiag2, Rotate270: fullest symmetry *)
        ]
    let nb_symmetry_close = List.length all_symmetry_close

    let rec xp_func : func html_xp =
      fun ~html print f ->
      match f with
      | `Index_1 is ->
         print#string "index";
         xp_list
           (fun ~html print -> function
            | None -> print#string ":"
            | Some i -> print#int i)
           ~html print is
      | `Plus_2 -> print#string "+"
      | `Minus_2 -> print#string "-"
      | `Modulo_2 -> print#string "%"
      | `ScaleUp_2 -> print#string "*"
      | `ScaleDown_2 -> print#string "/"
      | `ScaleTo_2 -> print#string "scaleTo"
      | `Size_1 -> print#string "size"
      | `Height_1 -> print#string "height"
      | `Width_1 -> print#string "width"
      | `Crop_2 -> print#string "crop"
      | `Strip_1 -> print#string "strip"
      | `Corner_2 -> print#string "corner"
      | `Min_n -> print#string "min"
      | `Max_n -> print#string "max"
      | `Average_n -> print#string "average"
      | `Span_2 -> print#string "span"
      | `Norm_1 -> print#string "norm"
      | `Diag1_1 k -> print#string "diag1"
      | `Diag2_1 k -> print#string "diag2"
      | `LogAnd_2 -> print#string "and"
      | `LogOr_2 -> print#string "or"
      | `LogXOr_2 -> print#string "xor"
      | `LogAndNot_2 -> print#string "and_ not"
      | `LogNot_1 -> print#string "not"
      | `Stack_n -> print#string "stack"
      | `Area_1 -> print#string "area"
      | `Left_1 -> print#string "left"
      | `Right_1 -> print#string "right"
      | `Center_1 -> print#string "center"
      | `Top_1 -> print#string "top"
      | `Bottom_1 -> print#string "bottom"
      | `Middle_1 -> print#string "middle"
      | `ProjI_1 -> print#string "projI"
      | `ProjJ_1 -> print#string "projJ"
      | `MaskOfGrid_1 -> print#string "maskOfGrid"
      | `GridOfMask_2 -> print#string "gridOfMask"
      | `TranslationOnto_2 -> print#string "translationOnto"
      | `Tiling_1 (k,l) ->
         print#string "tiling";
         xp_tuple2 ~delims:("[","]") xp_int xp_int ~html print (k,l)
      | `PeriodicFactor_2 mode ->
         print#string ("periodicFactor" ^ suffix_periodicity_mode mode)
      | `FillResizeAlike_3 mode ->
         print#string ("fillResizeAlike" ^ suffix_periodicity_mode mode)
      | `SelfCompose_2 -> print#string "compose"
      | `ApplySymVec_1 (sym,_) ->
         print#string "applySymVec";
         xp_tuple1 ~delims:("[","]") xp_symmetry ~html print sym
      | `ApplySymGrid_1 sym ->
         print#string "applySymGrid";
         xp_tuple1 ~delims:("[","]") xp_symmetry ~html print sym
      | `UnfoldSym_1 sym_matrix ->
         print#string "unfoldSym";
         xp_tuple1 ~delims:("[","]") xp_symmetry_matrix ~html print sym_matrix
      | `CloseSym_2 sym_seq ->
         print#string "closeSym";
         xp_tuple1 ~delims:("[","]") xp_symmetry_seq ~html print sym_seq
      | `TranslationSym_2 sym ->
         print#string "translationSym";
         xp_tuple1 ~delims:("[","]") xp_symmetry ~html print sym
      | `MajorityColor_1 -> print#string "majorityColor"
      | `ColorCount_1 -> print#string "colorCount"
      | `Coloring_2 -> print#string "coloring"
      | `SwapColors_3 -> print#string "swapColor"
    and xp_symmetry : symmetry html_xp =
      fun ~html print sym ->
      match sym with
      | `Id -> print#string "id"
      | `FlipHeight -> print#string "flipHeight"
      | `FlipWidth -> print#string "flipWidth"
      | `FlipDiag1 -> print#string "flipDiag1"
      | `FlipDiag2 -> print#string "flipDiag2"
      | `Rotate180 -> print#string "rotate180"
      | `Rotate90 -> print#string "rotate90"
      | `Rotate270 -> print#string "rotate270"
    and xp_symmetry_matrix : symmetry list list html_xp =
      fun ~html print sym_matrix ->
      xp_list ~delims:("","") ~sep:" "
        (xp_list ~sep:" "
           xp_symmetry)
        ~html print sym_matrix
    and xp_symmetry_seq : symmetry list html_xp =
      fun ~html print sym_seq ->
      xp_list ~delims:("","") ~sep:"; "
        xp_symmetry
        ~html print sym_seq
    and suffix_periodicity_mode = function
      | `Total -> "_total"
      | `Strict -> "_strict"
      | `TradeOff -> ""

    (* ASD *)
              
    let asd (* : asd *) =
      object
        inherit [typ,constr,func] Model.asd
        method is_default_constr = function
          | _ -> false
        method default_and_other_pats = function
          (* synchronize with is_default_constr *)
          | BOOL -> None, [ ]
          | INT CARD -> None, [ ]
          | INT (COORD _) ->
             None,
             [ AnyCoord, [||] ]
          | VEC tv ->
             None,
             [ Vec, [|INT (COORD (I, tv)), 0;
                      INT (COORD (J, tv)), 0|] ]
          | COLOR tc ->
             None,
             [ AnyColor, [||] ]
          | MOTIF ->
             None,
             [ AnyMotif, [||] ]
          | GRID (filling,nocolor) ->
             let full = (filling = `Full) in
             None,
             List.filter_map
               (fun (cond,c_args) ->
                 if cond
                 then Some c_args
                 else None)
               [ true, (AnyGrid, [||]);
                 full, (BgColor, [|COLOR (C_BG full), 0; GRID (`Sprite,nocolor), 0|]);
                 not full, (IsFull, [|GRID (`Full,nocolor), 0|]);
                 true, (Crop, [|GRID (filling,nocolor), 0; VEC POS, 0; VEC SIZE, 0|]);
                 not full, (Objects (`Default,1), [|VEC SIZE, 0; OBJ (`Sprite,nocolor), 1|]);
                 not nocolor, (Monocolor, [|COLOR C_OBJ, 0; GRID (filling,true), 0|]);
                 (* not nocolor, (Recoloring, [|COLOR C_OBJ, 1; GRID (filling,nocolor), 0|]); *)
                 not nocolor, (Recoloring, [|GRID (filling,nocolor), 0; MAP (COLOR C_OBJ, COLOR C_OBJ), 1|]);
                 true, (Motif, [|MOTIF, 0; GRID (filling,nocolor), 0; GRID (`Noise,nocolor), 0|]);
                 not full (*&& nocolor*), (Empty, [|VEC SIZE, 0|]);
                 not full && nocolor, (Full, [|VEC SIZE, 0|]);
                 not full && nocolor, (Point, [||]) ]
          | OBJ tg ->
             None,
             [ Obj, [|VEC POS, 0; GRID tg, 0|] ]
          | MAP (ta,tb) ->
             None,
             List.filter_map
               (fun (cond,c_args) ->
                 if cond
                 then Some c_args
                 else None)
               [ true, (AnyMap, [||]);
                 true, (DomMap [||], [|tb, 1|]);
                 ta=tb, (Replace, [|ta, 0; ta, 0|]);
                 ta=tb, (Swap, [|ta, 0; ta, 0|]) ]
        method funcs k =
          match k with
          | BOOL -> []
          | INT CARD ->
             [ `Index_1 [], [|k|];
               `Plus_2, [|k; k|];
               `Minus_2, [|k; k|];
               `Area_1, [|GRID (`Sprite,false)|];
               `ColorCount_1, [|GRID (`Sprite,false)|]; (* also for `Noise? *)
               `Min_n, [|k; k|];
               `Max_n, [|k; k|];
               `Average_n, [|k; k|];
             ]
          | INT (COORD (axis,tv)) ->
             [ `Index_1 [], [|k|];
               `Height_1, [|GRID (`Sprite,false)|]; (* if axis=I *)
               `Width_1, [|GRID (`Sprite,false)|]; (* if axis=J *)
               `Area_1, [|GRID (`Sprite,false)|];
               `Plus_2, [|k; k|];
               `Minus_2, [|k; k|];
               `ScaleUp_2, [|k; INT CARD|];
               `ScaleDown_2, [|k; INT CARD|];
               `Span_2, [|k; k|]; (* only on same axis POS *)
               `Min_n, [|k; k|];
               `Max_n, [|k; k|];
               `Average_n, [|k; k|];               
             ]
          | VEC tv ->
             [ `Index_1 [], [|k|];
               `Size_1, [|GRID (`Sprite,false)|];
               `Plus_2, [|k; k|];
               `Minus_2, [|k; k|];
               `ScaleUp_2, [|k; INT CARD|];
               `ScaleDown_2, [|k; INT CARD|];
               `ProjI_1, [|k|];
               `ProjJ_1, [|k|];
               `Corner_2, [|k; k|]; (* only on POS *)
               `Span_2, [|k; k|]; (* only on POS *)
               `Min_n, [|k; k|];
               `Max_n, [|k; k|];
               `Average_n, [|k; k|];
               `TranslationOnto_2, [|OBJ (`Sprite,false); OBJ (`Sprite,false)|];
               `TranslationSym_2 `Id, [|OBJ (`Sprite,false); GRID (`Sprite,false)|];
               `ApplySymVec_1 (`Id,tv), [|k|];
               `Tiling_1 (2,2), [|k|];
             ]
          | COLOR tc ->
             [ `Index_1 [], [|k|];
               `MajorityColor_1, [|GRID (`Sprite,false)|]; (* also `Full and `Noise *)
             ]
          | MOTIF ->
             [ `Index_1 [], [|k|]
             ]
          | GRID (filling,nocolor) ->
             let full = (filling = `Full) in
             [ `Index_1 [], [|k|];
               `ScaleUp_2, [|k; INT CARD|];
               `ScaleDown_2, [|k; INT CARD|];
               `ScaleTo_2, [|k; VEC SIZE|];
               (*`Strip_1, [|GRID (false,false)|];*)
               `PeriodicFactor_2 `TradeOff, [|COLOR (C_BG full); k|];
               `Crop_2, [|GRID (`Full,false); OBJ (`Sprite,false)|];
               `ApplySymGrid_1 `Id, [|k|];
               `Coloring_2, [|k; COLOR C_OBJ|];
               `Tiling_1 (2,2), [|k|];
               `FillResizeAlike_3 `TradeOff, [|COLOR (C_BG full); VEC SIZE; k|];
               `SelfCompose_2, [|COLOR C_OBJ; k|];
               `UnfoldSym_1 [], [|k|];
               `CloseSym_2 [], [|COLOR (C_BG full); k|];
               `SwapColors_3, [|k; COLOR C_OBJ; COLOR C_OBJ|];
               `Stack_n, [|k; k|];
               (* on masks *)
               `LogNot_1, [|k|];
               `LogAnd_2, [|k; k|];
               `LogOr_2, [|k; k|];
               `LogAndNot_2, [|k; k|];
               `LogXOr_2, [|k; k|];
             ]
          | OBJ (filling,nocolor) ->
             let full = (filling = `Full) in
             [ `Index_1 [], [|k|];
               `PeriodicFactor_2 `TradeOff, [|COLOR (C_BG full); k|];
               `FillResizeAlike_3 `TradeOff, [|COLOR (C_BG full); VEC SIZE; k|];
               `ApplySymGrid_1 `Id, [|k|];
               `UnfoldSym_1 [], [|k|];
               `CloseSym_2 [], [|COLOR (C_BG full); k|] ]
          | MAP (ta,tb) -> []
        method expr_opt k =
          match k with (* what type can be used to define k *)
          | BOOL -> true, [k]
          | INT CARD -> true, [k]
          | INT (COORD (axis,tv)) ->
             let axis_opp = match axis with I -> J | J -> I in
             true, [k; INT (COORD (axis_opp,tv)); INT CARD]
          | VEC _ -> true, [k]
          | COLOR C_OBJ -> true, [k; COLOR (C_BG true)]
          | COLOR (C_BG true) -> true, [k; COLOR C_OBJ]
          | COLOR (C_BG false) -> true, [k; COLOR (C_BG true); COLOR C_OBJ]
          | MOTIF -> true, [k]
          | GRID (`Sprite,nocolor) -> true, [k; GRID (`Full,nocolor)]
          | GRID _ -> true, [k]
          | OBJ tg -> true, [k]
          | MAP _ -> true, [k]
        method alt_opt = function
          | _ -> false (* LATER *)
      end

    (* model processing *)
      
    type generator_info = unit (* int * int * Grid.color (* container height, width, and color *) *)

    type input =
      [ `Null
      | `IntRange of int * Range.t
      | `Vec of input * input
      | `Color of Grid.color
      | `Motif of GPat.Motif.t
      | `GridDimsCols of Grid.t * Range.t (* height range *) * Range.t (* width range *) * int (* nb cols *)
      (* | `Obj of input (* pos *) * input (* grid *) *)
      | `Objects of int (* height ctx *) * int (* width ctx *) * int (* nb colors *) * (int * int * Grid.t) list (* objects *)
      | `MapDomain of (value,value) Mymap.t * value array (* domain *)
      | `Seq of input list ]

    type encoding = dl
                  
  end

module MyDomain : Madil.DOMAIN =
  struct

    (* boiler plate code *)
    include Basic_types
    include Madil.Defined_types(Basic_types)

    (* parameters *)

    let alpha = def_param "alpha" 10. string_of_float
    let max_nb_parse = def_param "max_nb_parse" 100 string_of_int (* max nb of considered doc parses *)
    let max_nb_reads = def_param "max_nb_doc_reads" 3 string_of_int (* max nb of selected doc reads, passed to the next stage *)
    let max_nb_writes = def_param "max_nb_doc_writes" 3 string_of_int (* max nb of selected output writes *)
    let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
    let max_refinements = def_param "max_refinements" 100 string_of_int (* max nb of considered refinements *)
    let jump_width = def_param "jump_width" 3 string_of_int (* max nb of explored pattern refinements at some model path during learning (refining phase). min=1 *)
    
    (* constructors and accessors *)
                        
    let get_pos : value -> (int * int) option =
      function
      | `Grid _ -> Some (0,0)
      | `Obj (i, j, _) -> Some (i,j)
      | _ -> None
          
    let get_size : value -> (int * int) option =
      function
      | `Grid g -> Some (Grid.dims g)
      | `Obj (_, _, shape) -> Some (Grid.dims shape)
      | _ -> None

    let make_anycoord axis tv : model = Model.make_pat (INT (COORD (axis,tv))) AnyCoord [||]
    let make_vec tv mi mj : model = Model.make_pat (VEC tv) Vec [|mi;mj|]
    let make_anycolor tc : model = Model.make_pat (COLOR tc) AnyColor [||]
    let make_anymotif : model = Model.make_pat MOTIF AnyMotif [||]
    let make_anygrid tg : model = Model.make_pat (GRID tg) AnyGrid [||]
    let make_obj tg mpos mg1 : model = Model.make_pat (OBJ tg) Obj [|mpos;mg1|]
    let make_anymap ta tb : model = Model.make_pat (MAP (ta,tb)) AnyMap [||]
    let make_dommap ta tb keys mvals : model = Model.make_pat (MAP (ta,tb)) (DomMap keys) [|mvals|]
    let make_replace ta tb ma mb : model = Model.make_pat (MAP (ta,tb)) Replace [|ma; mb|]
    let make_swap ta tb ma mb : model = Model.make_pat (MAP (ta,tb)) Swap [|ma; mb|]
    let make_bgcolor mcol mg1 : model = Model.make_pat (GRID (`Full,false)) BgColor [|mcol; mg1|]
    let make_isfull mg1 : model = Model.make_pat (GRID (`Sprite,false)) IsFull [|mg1|]
    let make_crop tg mg1 mpos msize : model = Model.make_pat (GRID tg) Crop [|mg1; mpos; msize|]
    let make_objects seg nmax msize mobj : model = Model.make_pat (GRID (`Sprite,false)) (Objects (seg,nmax)) [|msize; mobj|]
    let make_monocolor mcol mmask : model = Model.make_pat (GRID (`Sprite,false)) Monocolor [|mcol; mmask|]
    (*let make_recoloring mcol mgrid : model = Model.make_pat (GRID (`Sprite,false)) Recoloring [|mcol; mgrid|]*)
    let make_recoloring tg mgrid mmap : model = Model.make_pat (GRID tg) Recoloring [|mgrid; mmap|]
    let make_motif tg mmotif mcore mnoise : model = Model.make_pat (GRID tg) Motif [|mmotif; mcore; mnoise|]
    let make_empty msize : model = Model.make_pat (GRID (`Sprite,false)) Empty [|msize|]
    let make_full msize : model = Model.make_pat (GRID (`Sprite,true)) Full [|msize|]
    let make_point : model = Model.make_pat (GRID (`Sprite,true)) Point [||]
      
    let get_int (d : data) : int =
      match Data.value d with
      | `Int ij -> ij
      | _ -> assert false
    let get_vec (d : data) : int * int =
      match Data.value d with
      | `Vec (i,j) -> i, j
      | _ -> assert false
    let get_color (d : data) : Grid.color =
      match Data.value d with
      | `Color c -> c
      | _ -> assert false
    let get_motif (d : data) : GPat.Motif.t =
      match Data.value d with
      | `Motif mot -> mot
      | _ -> assert false
    let get_grid (d : data) : Grid.t =
      match Data.value d with
      | `Grid g -> g
      | _ -> assert false
      
    let make_danycoord ij r : data =
      Data.make_dpat (`Int ij) (DAnyCoord (ij,r)) [||]
    let make_dvec di dj : data =
      let i, j = get_int di, get_int dj in
      Data.make_dpat (`Vec (i,j)) DVec [|di;dj|]
    let make_danycolor c tc : data =
      Data.make_dpat (`Color c) (DAnyColor (c,tc)) [||]
    let make_danymotif m : data =
      Data.make_dpat (`Motif m) (DAnyMotif m) [||]
    let make_danygrid g tg rh rw nc : data =
      Data.make_dpat (`Grid g) (DAnyGrid (g,tg,rh,rw,nc)) [||]
    let make_dobj dpos dg1 : data =
      let i, j = get_vec dpos in
      let g1 = get_grid dg1 in
      Data.make_dpat (`Obj (i,j,g1)) DObj [|dpos;dg1|]
    let make_danymap tb m : data =
      Data.make_dpat (`Map m) (DAnyMap (m,tb)) [||]
    let make_ddommap (keys : value array) dvals : data =
      let m =
        match Data.value dvals with
        | `Seq vals ->
           assert (Array.length keys = Array.length vals);
           let res = ref (Mymap.empty : (value,value) Mymap.t) in
           Array.iter2
             (fun k v -> res := Mymap.add k v !res)
             keys vals;
           !res
        | _ -> assert false in
      Data.make_dpat (`Map m) (DDomMap keys) [|dvals|]
    let make_dreplace (dom : value array) da db : data =
      let a, b = Data.value da, Data.value db in
      let m =
        Array.fold_right
          (fun k res ->
            let v = if k = a then b else k in
            Mymap.add k v res)
          dom Mymap.empty in
      Data.make_dpat (`Map m) DReplace [|da; db|]
    let make_dswap (dom : value array) da db : data =
      let a, b = Data.value da, Data.value db in
      let m =
        Array.fold_right
          (fun k res ->
            let v = if k = a then b
                    else if k = b then a
                    else k in
            Mymap.add k v res)
          dom Mymap.empty in
      Data.make_dpat (`Map m) DSwap [|da; db|]
    let make_dbgcolor dcol dspr : data =
      let g =
        match Data.value dcol, Data.value dspr with
        | `Color bc, `Grid g1 -> Grid.fill_transparent g1 bc
        | _ -> assert false in
      Data.make_dpat (`Grid g) DBgColor [|dcol;dspr|]
    let make_disfull dspr : data =
      Data.make_dpat (Data.value dspr) DIsFull [|dspr|]
    let make_dcrop dg dpos dsize : data result =
      let| g1 =
        match Data.value dg, Data.value dpos, Data.value dsize with
        | `Grid g, `Vec (i,j), `Vec (h1,w1) ->
           Grid.Transf.crop g i j h1 w1
        | _ -> assert false in
      Result.Ok (Data.make_dpat (`Grid g1) DCrop [|dg; dpos; dsize|])
    let make_dobjects seg nmax dsize dobjs : data =
      let g =
        match Data.value dsize, Data.value dobjs with
        | `Vec (h,w), `Seq objs ->
           let g = Grid.make h w Grid.transparent in
           Array.iter
             (function
              | `Obj (i,j,g1) -> Grid.add_grid_at g i j g1
              | _ -> assert false)
             objs;
           g
        | _ -> assert false in
      Data.make_dpat (`Grid g) (DObjects (seg,nmax)) [|dsize; dobjs|]
    let make_dmonocolor dcol dmask : data =
      let g_res =
        match Data.value dcol, Data.value dmask with
        | `Color c, `Grid g1 ->
           Grid.Transf.swap_colors g1 Grid.Mask.one c
        | _ -> assert false in
      match g_res with
      | Result.Ok g ->
         Data.make_dpat (`Grid g) DMonocolor [|dcol; dmask|]
      | _ ->
         let c = get_color dcol in
         let mask = get_grid dmask in
         pp xp_value (`Color c);
         pp xp_value (`Grid mask);
         assert false
    (* let make_drecoloring dcols dgrid : data =
      let g =
        match Data.value dcols, Data.value dgrid with
        | `Seq cols, `Grid g1 ->
           let palette =
             Array.map
               (function
                | `Color c -> c
                | _ -> assert false)
               cols in
           Grid_patterns.recolor g1 palette
        | _ ->
           pp_endline xp_data dcols;
           pp_endline xp_data dgrid;
           assert false in
      Data.make_dpat (`Grid g) DRecoloring [|dcols; dgrid|] *)
    let make_drecoloring dgrid dmap : data =
      let g =
        match Data.value dgrid, Data.value dmap with
        | `Grid g1, `Map mcol ->
           Grid.map_pixels
             (fun c1 ->
               if Grid.is_true_color c1
               then
                 match Mymap.find_opt (`Color c1) mcol with
                 | Some (`Color c) -> c
                 | Some _ -> assert false
                 | None -> c1
               else c1)
             g1
        | _ -> assert false in
      Data.make_dpat (`Grid g) DRecoloring [|dgrid; dmap|]
    let make_dmotif dmot dcore dnoise : data result =
      let mot = get_motif dmot in
      let g_core = get_grid dcore in
      let g_noise = get_grid dnoise in
      let h, w = Grid.dims g_noise in
      let| g = GPat.Motif.make_grid h w mot g_core in
      Grid.add_grid_at g 0 0 g_noise;
      Result.Ok (Data.make_dpat (`Grid g) DMotif [|dmot; dcore; dnoise|])
    let make_dempty dsize : data =
      let g =
        match Data.value dsize with
        | `Vec (h,w) -> Grid.Mask.empty h w
        | _ -> assert false in
      Data.make_dpat (`Grid g) DEmpty [|dsize|]
    let make_dfull dsize : data =
      let g =
        match Data.value dsize with
        | `Vec (h,w) -> Grid.Mask.full h w
        | _ -> assert false in
      Data.make_dpat (`Grid g) DFull [|dsize|]
    let make_dpoint : data =
      let g = Grid.Mask.full 1 1 in
      Data.make_dpat (`Grid g) DPoint [||]
      
    (* evaluation *)

    let bool_of_value : value -> bool result = function
      | `Bool b -> Result.Ok b
      | _ -> Result.Error (Failure "model evaluation: expected Boolean value")

    let value_of_bool b = `Bool b
      
    let value_of_seq (vs : value array) : value = `Seq vs

    exception Invalid_expr of string
            
    module Funct =
      struct

        let grid_sym : symmetry -> (Grid.t -> Grid.t) = function
          | `Id -> Fun.id
          | `FlipHeight -> Grid.Transf.flipHeight
          | `FlipWidth -> Grid.Transf.flipWidth
          | `FlipDiag1 -> Grid.Transf.flipDiag1
          | `FlipDiag2 -> Grid.Transf.flipDiag2
          | `Rotate180 -> Grid.Transf.rotate180
          | `Rotate90 -> Grid.Transf.rotate90
          | `Rotate270 -> Grid.Transf.rotate270

        let apply_symmetry_vec (sym : symmetry) (tv : typ_vec) e (d1 : value) : value result =
(*  let sym_pos d = (* symmetry of a point relative to the grid *)
    let p_grid_size = `Field (`Size, `Root) in
    match lookup p_grid_size, d with (* getting the grid size *)
    | Result.Ok (`Vec (`Int h, `Int w)), `Vec (`Int i, `Int j) ->
       let i', j' =
         match sym with
         | `Id -> i, j
         | `FlipHeight -> h-1-i, j
         | `FlipWidth -> i, w-1-j
         | `FlipDiag1 -> j, i
         | `FlipDiag2 -> w-1-j, h-1-i
         | `Rotate180 -> h-1-i, w-1-j
         | `Rotate90 -> j, h-1-i
         | `Rotate270 -> w-1-j, i in
       `Vec (`Int i', `Int j')
    | _ -> assert false in *)
          let sym_size = function
            | `Vec (h, w) ->
               let h', w' =
                 match sym with
                 | `Id | `FlipHeight | `FlipWidth | `Rotate180 -> h, w
                 | `FlipDiag1 | `FlipDiag2 | `Rotate90 | `Rotate270 -> w, h in
               `Vec (h', w')
            | _ -> assert false in
          let sym_move = function (* symmetry relative to position (0,0) *)
            | `Vec (i, j) ->
               let i', j' =
                 match sym with
                 | `Id -> i, j
                 | `FlipHeight -> -i, j
                 | `FlipWidth -> i, -j
                 | `FlipDiag1 -> j, i
                 | `FlipDiag2 -> -j, -i
                 | `Rotate180 -> -i, -j
                 | `Rotate90 -> j, -i
                 | `Rotate270 -> -j, i in
               `Vec (i', j')
            | _ -> assert false
          in
          match tv, d1 with
  (*  | POS, _ -> Result.Ok (sym_pos d1) *)
          | SIZE, _ -> Result.Ok (sym_size d1)
          | MOVE, _ -> Result.Ok (sym_move d1)
          | _ -> Result.Error (Invalid_expr e)
        
        let apply_symmetry_grid (sym : symmetry) e (d1 : value) : value result =
          match d1 with
          | `Obj (i, j, g1) ->
             let g1' = grid_sym sym g1 in
             Result.Ok (`Obj (i, j, g1')) (* NOTE: do not use sym_pos because pos in PosShape must be the top-left corner of the shape, see def of TranslationSym *)
          | `Grid g ->
             let g' = grid_sym sym g in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e)

        let unfold_any
              (concatHeight : 'a -> 'a -> 'a result)
              (concatWidth : 'a -> 'a -> 'a result)
              (apply_sym : symmetry -> 'a -> 'a)
              (sym_matrix : symmetry list list)
            : 'a -> 'a result =
          let rec gen_matrix : symmetry list list -> ('a -> 'a result) = function
            | [] -> assert false
            | [row] -> gen_row row
            | row::rows ->
               let g_row = gen_row row in
               let g_rows = gen_matrix rows in
               (fun x ->
                 let| xrow = g_row x in
                 let| xrows = g_rows x in
                 concatHeight xrow xrows)
          and gen_row : symmetry list -> ('a -> 'a result) = function
            | [] -> assert false
            | [sym] -> (fun x -> Result.Ok (apply_sym sym x))
            | sym::syms ->
               let g_syms = gen_row syms in
               (fun x ->
                 let xsym = apply_sym sym x in
                 let| xsyms = g_syms x in
                 concatWidth xsym xsyms) in
          gen_matrix sym_matrix

        let unfold_grid sym_matrix g =
          unfold_any Grid.Transf.concatHeight Grid.Transf.concatWidth grid_sym sym_matrix g
        let unfold_grid, reset_unfold_grid =
          Memo.memoize2 ~size:101 unfold_grid
          
        let rec unfold_symmetry (sym_matrix : symmetry list list) : _ -> value -> value result =
          fun e d ->
          match d with
          | `Grid g ->
             let| g' = unfold_grid sym_matrix g in
             Result.Ok (`Grid g')
          | `Obj (i, j, g1) ->
             let| g1 = unfold_grid sym_matrix g1 in
             Result.Ok (`Obj (i, j, g1))
          | _ -> Result.Error (Invalid_expr e)

        let close_any
              (stack : 'a list -> 'a result)
              (apply_sym : symmetry -> 'a -> 'a)
              (sym_seq : symmetry list)
            : 'a -> 'a result =
          let rec gen_seq : symmetry list -> ('a -> 'a result) = function
            | [] -> (fun x1 -> Result.Ok x1)
            | sym::syms ->
               let g = gen_seq syms in
               (fun x1 ->
                 let y1 = apply_sym sym x1 in
                 let| x2 = stack [x1; y1] in
                 g x2) in
          gen_seq sym_seq
       
        let close_grid sym_seq bgcolor g =
          let| g' = close_any (Grid.Transf.layers bgcolor) grid_sym sym_seq g in
          Result.Ok g'
        let close_grid, reset_close_grid =
          Memo.memoize3 ~size:101 close_grid

        let rec close_symmetry (sym_seq : symmetry list) (bgcolor : Grid.color) =
          fun e d ->
          match d with
          | `Grid g ->
             let| g' = close_grid sym_seq bgcolor g in
             Result.Ok (`Grid g')
          | `Obj (i, j, g1) ->
             let| g1 = close_grid sym_seq bgcolor g1 in
             Result.Ok (`Obj (i, j, g1))
          | _ -> Result.Error (Invalid_expr e)

        let reset_memoized_functions_apply () =
          reset_unfold_grid ();
          reset_close_grid ()
  
      end

    let compile_scalar_func : func -> (value array -> value result) =
      let e = "" in
      function
      | `Index_1 _ -> assert false (* not a scalar function *)
      | `Plus_2 ->
         (function
          | [| `Int i1; `Int i2|] -> Result.Ok (`Int (i1 + i2))
          | [| `Vec (i1,j1); `Vec (i2,j2)|] -> Result.Ok (`Vec (i1+i2, j1+j2))
          | _ -> Result.Error (Invalid_expr e))
      | `Minus_2 ->
         (function
          | [| `Int i1; `Int i2|] -> Result.Ok (`Int (i1-i2))
          | [| `Vec (i1, j1); `Vec (i2, j2)|] -> Result.Ok (`Vec (i1-i2, j1-j2))
          | _ -> Result.Error (Invalid_expr e))
      | `Modulo_2 ->
         (function
          | [| `Int i1; `Int i2|] -> Result.Ok (`Int (i1 mod i2))
          | _ -> Result.Error (Invalid_expr e))
      | `ScaleUp_2 ->
         (function
          | [| d1; `Int 0|] -> Result.Error (Invalid_argument "ScaleUp: k=0") 
          | [| d1; `Int k|] ->
             assert (k > 0);
             ( match d1 with
               | `Int i -> Result.Ok (`Int (i * k))
               | `Vec (i,j) -> Result.Ok (`Vec (i * k, j * k))
               | `Grid g ->
                  let| g' = Grid.Transf.scale_up k k g in
                  Result.Ok (`Grid g')
               | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `ScaleDown_2 ->
         (function
          | [| d1; `Int 0|] -> Result.Error (Invalid_argument "ScaleDown: k=0") 
          | [| d1; `Int k|] ->
             assert (k > 0);
             (match d1 with
              | `Int i1 ->
                 let rem = i1 mod k in
                 if rem = 0 || rem = k - 1 (* account for separators *)
                 then Result.Ok (`Int (i1 / k))
                 else Result.Error (Undefined_result "ScaleDown: not an integer")
              | `Vec (i1, j1) ->
                 let remi, remj = i1 mod k, j1 mod k in
                  if remi = remj && (remi = 0 || remi = k-1) (* account for separators *)
                  then Result.Ok (`Vec (i1 / k, j1 / k))
                  else Result.Error (Undefined_result "ScaleDown: not an integer")
              | `Grid g ->
                 let| g' = Grid.Transf.scale_down k k g in
                 Result.Ok (`Grid g')
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `ScaleTo_2 ->
         (function
          | [| `Grid g; (`Vec (new_h, new_w))|] ->
             let| g' = Grid.Transf.scale_to new_h new_w g in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))
      | `Size_1 ->
         (function
          | [|`Grid g|] ->
             let h, w = Grid.dims g in
             Result.Ok (`Vec (h, w))
          | _ -> Result.Error (Invalid_expr e))
      | `Height_1 ->
         (function
          | [|`Grid g|] ->
             let h, _ = Grid.dims g in
             Result.Ok (`Int h)
          | _ -> Result.Error (Invalid_expr e))
      | `Width_1 ->
         (function
          | [|`Grid g|] ->
             let _, w = Grid.dims g in
             Result.Ok (`Int w)
          | _ -> Result.Error (Invalid_expr e))
      | `Crop_2 ->
         (function
          | [| `Grid g; `Obj (ri, rj, shape)|] ->
             let| c = Grid.majority_color Grid.transparent shape in
             if Mask_model.matches (Grid.Mask.from_grid_color c shape) `Border (* TODO: allow crop on Full rectangles as well ? *)
             then
               let rh, rw = Grid.dims shape in
               let i, j, h, w = ri+1, rj+1, rh-2, rw-2 in (* inside border *)
               let| g' = Grid.Transf.crop g i j h w in
               Result.Ok (`Grid g')
             else Result.Error (Invalid_expr e)
          | _ -> Result.Error (Invalid_expr e))
      | `Strip_1 ->
         (function
          | [| `Grid g|] ->
             let| bgcolor = Grid.majority_color Grid.transparent g in
             let| _, _, g'= Grid.Transf.strip bgcolor g Grid.black in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))
      | `Corner_2 ->
         (function
          | [| `Vec (i1, j1); `Vec (i2, j2)|] ->
             if i1 <> i2 && j1 <> j2
             then Result.Ok (`Vec (i1, j2))
             else Result.Error (Undefined_result "Corner: vectors on same row/column")
          | _ -> Result.Error (Invalid_expr e))
      | `Min_n ->
         (fun ds ->
           let| is_int, is_vec, mini, minj =
             ds
             |> Array.fold_left
                  (fun res t ->
                    let| is_int,is_vec,mini,minj = res in
                    match t with
                    | `Int i -> Result.Ok (true, is_vec, min i mini, minj)
                    | `Vec (i, j) -> Result.Ok (is_int, true, min i mini, min j minj)
                    | _ -> Result.Error (Invalid_expr e))
                  (Result.Ok (false, false, max_int, max_int)) in
           (match is_int, is_vec with
            | true, false -> Result.Ok (`Int mini)
            | false, true -> Result.Ok (`Vec (mini, minj))
            | _ -> assert false))
      | `Max_n ->
         (fun ds ->
           let| is_int,is_vec,maxi,maxj =
             ds
             |> Array.fold_left
                  (fun res t ->
                    let| is_int,is_vec,maxi,maxj = res in
                    match t with
                    | `Int i -> Result.Ok (true, is_vec, max i maxi, maxj)
                    | `Vec (i, j) -> Result.Ok (is_int, true, max i maxi, max j maxj)
                    | _ -> Result.Error (Invalid_expr e))
                  (Result.Ok (false, false, min_int, min_int)) in
           (match is_int, is_vec with
            | true, false -> Result.Ok (`Int maxi)
            | false, true -> Result.Ok (`Vec (maxi, maxj))
            | _ -> assert false))
      | `Average_n ->
         (fun ds ->
           let| is_int,is_vec,n,sumi,sumj =
             ds
             |> Array.fold_left
                  (fun res t ->
                    let| is_int,is_vec,n,sumi,sumj = res in
                    match t with
                    | `Int i -> Result.Ok (true, is_vec, n+1, sumi+i, sumj)
                    | `Vec (i, j) -> Result.Ok (is_int, true, n+1, sumi+i, sumj+j)
                    | _ -> Result.Error (Invalid_expr e))
                  (Result.Ok (false, false, 0, 0, 0)) in
           (match is_int, is_vec with
            | true, false ->
               if sumi mod n = 0
               then Result.Ok (`Int (sumi / n))
               else Result.Error (Undefined_result "Average: not an integer")
            | false, true ->
               if sumi mod n = 0 && sumj mod n = 0
               then Result.Ok (`Vec (sumi / n, sumj / n))
               else Result.Error (Undefined_result "Average: not an integer")
            | _ -> assert false)) (* empty or ill-typed list *)
      | `Span_2 ->
         (function
          | [| `Int i1; `Int i2|] ->
             if i1=i2
             then Result.Error (Undefined_result "Span: same int")
             else Result.Ok (`Int (abs (i2-i1) + 1))
          | [| `Vec (i1, j1); `Vec (i2, j2)|] ->
             if i1=i2 && j1=j2
             then Result.Error (Undefined_result "Span: same vector")
             else Result.Ok (`Vec (abs (i2-i1) + 1, abs (j2-j1) + 1))
          | _ -> Result.Error (Invalid_expr e))
      | `Norm_1 ->
         (function
          | [| `Vec (i, j)|] -> Result.Ok (`Int (i+j))
          | _ -> Result.Error (Invalid_expr e))
      | `Diag1_1 k ->
         (function
          | [| `Vec (i, j)|] -> Result.Ok (`Int ((i+j) mod k))
          | _ -> Result.Error (Invalid_expr e))
      | `Diag2_1 k ->
         (function
          | [| `Vec (i, j)|] -> Result.Ok (`Int ((i-j) mod k))
          | _ -> Result.Error (Invalid_expr e))
      | `LogAnd_2 ->
         (function
          | [| `Grid m1; `Grid m2|] when Grid.dims m1 = Grid.dims m2 -> (* TODO: generalize Mask logics to grids transfs *)
             let m = Grid.Mask.inter m1 m2 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `LogOr_2 ->
         (function
          | [| `Grid m1; `Grid m2|] when Grid.dims m1 = Grid.dims m2 ->
             let m = Grid.Mask.union m1 m2 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `LogXOr_2 ->
         (function
          | [|`Grid m1; `Grid m2|] when Grid.dims m1 = Grid.dims m2 ->
             let m = Grid.Mask.diff_sym m1 m2 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `LogAndNot_2 ->
         (function
          | [| `Grid m1; `Grid m2|] when Grid.dims m1 = Grid.dims m2 ->
             let m = Grid.Mask.diff m1 m2 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `LogNot_1 ->
         (function
          | [| `Grid m1|] ->
             let m = Grid.Mask.compl m1 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `Stack_n ->
         (fun ds ->
           let lg1 = Array.map (function `Grid g1 -> g1 | _ -> assert false) ds in
           let| g = Grid.Transf.layers Grid.transparent (Array.to_list lg1) in
           Result.Ok (`Grid g))
      | `Area_1 ->
         (function
          | [| `Grid g|] ->
             Result.Ok (`Int (Grid.color_area Grid.transparent g))
          | _ -> Result.Error (Invalid_expr e))
      | `Left_1 ->
         (function
          | [| `Obj (_, j, _)|] -> Result.Ok (`Int j)
          | _ -> Result.Error (Invalid_expr e))
      | `Right_1 ->
         (function
          | [| `Obj (_, j, shape)|] ->
             let h, w = Grid.dims shape in
             Result.Ok (`Int (j+w-1))
          | _ -> Result.Error (Invalid_expr e))
      | `Center_1 ->
         (function
          | [| `Obj (_, j, shape)|] ->
             let h, w = Grid.dims shape in
             if w mod 2 = 0
             then Result.Error (Undefined_result "Center: no center, even width")
             else Result.Ok (`Int (j + w/2 + 1))
          | _ -> Result.Error (Invalid_expr e))
      | `Top_1 ->
         (function
          | [| `Obj (i, _, _)|] -> Result.Ok (`Int i)
          | _ -> Result.Error (Invalid_expr e))
      | `Bottom_1 ->
         (function
          | [| `Obj (i, _, shape)|] ->
             let h, w = Grid.dims shape in
             Result.Ok (`Int (i+h-1))
          | _ -> Result.Error (Invalid_expr e))
      | `Middle_1 ->
         (function
          | [| `Obj (i, _, shape)|] ->
             let h, w = Grid.dims shape in
             if h mod 2 = 0
             then Result.Error (Undefined_result "Middle: no middle, even height")
             else Result.Ok (`Int (i + h/2 + 1))
          | _ -> Result.Error (Invalid_expr e))
      | `ProjI_1 ->
         (function
          | [| `Vec (i, _)|] -> Result.Ok (`Vec (i, 0))
          | _ -> Result.Error (Invalid_expr e))
      | `ProjJ_1 ->
         (function
          | [| `Vec (_, j)|] -> Result.Ok (`Vec (0, j))
          | _ -> Result.Error (Invalid_expr e))
      | `MaskOfGrid_1 ->
         (function
          | [| `Grid g|] -> Result.Ok (`Grid (Grid.Mask.from_grid_background Grid.black g)) (* TODO: improve *)
          | _ -> Result.Error (Invalid_expr e))
      | `GridOfMask_2 ->
         (function
          | [| `Grid m; `Color c|] ->
             Result.Ok (`Grid (Grid.Mask.to_grid m Grid.black c)) (* TODO: improve *)
          | _ -> Result.Error (Invalid_expr e))
      | `TranslationOnto_2 ->
         (function
          | [| `Obj (mini1,minj1,g1); `Obj (mini2,minj2,g2)|] ->
             let h1, w1 = Grid.dims g1 in
             let h2, w2 = Grid.dims g2 in
             let maxi1, maxj1 = mini1 + h1 - 1, minj1 + w1 - 1 in
             let maxi2, maxj2 = mini2 + h2 - 1, minj2 + w2 - 1 in
             let ti =
               if maxi1 < mini2 then mini2 - maxi1 - 1
               else if maxi2 < mini1 then - (mini1 - maxi2 - 1)
               else 0 in
             let tj =
               if maxj1 < minj2 then minj2 - maxj1 - 1
               else if maxj2 < minj1 then - (minj1 - maxj2 - 1)
               else 0 in
             Result.Ok (`Vec (ti, tj))
          | _ -> Result.Error (Invalid_expr e))
      | `Tiling_1 (k,l) ->
         (function
          | [| `Vec (h, w)|] -> Result.Ok (`Vec (h*k, w*l))
          | [| `Grid g|] ->
             let| g' = Grid.Transf.tile k l g in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))
      | `PeriodicFactor_2 mode ->
         (function
          | [| `Color bgcolor; d2|] ->
             (match d2 with
              | `Grid g ->
                 let| g' = Grid.Transf.periodic_factor mode bgcolor g in
                 Result.Ok (`Grid g')
              | `Obj (i, j, shape) ->
                 let| shape' = Grid.Transf.periodic_factor mode bgcolor shape in
                 Result.Ok (`Obj (i, j, shape'))
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `FillResizeAlike_3 mode ->
         (function
          | [| `Color bgcolor; `Vec (h, w); d3|] when h > 0 && w > 0 ->
             let new_size = h, w in
             (match d3 with
              | `Grid g ->
                 let| g' = Grid.Transf.fill_and_resize_alike mode bgcolor new_size g in
                 Result.Ok (`Grid g')
              | `Obj (i, j, shape) ->
                 let| shape' = Grid.Transf.fill_and_resize_alike mode bgcolor new_size shape in
                 Result.Ok (`Obj (i, j, shape'))
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `SelfCompose_2 ->
         (function
          | [| `Color c_mask; `Grid g1|] ->
             let| g = Grid.Transf.compose c_mask g1 g1 in
             Result.Ok (`Grid g)
          | _ -> Result.Error (Invalid_expr e))
      | `ApplySymVec_1 (sym,tv) ->
         (function
          | [|d1|] -> Funct.apply_symmetry_vec sym tv e d1
          | _ -> Result.Error (Invalid_expr e))
      | `ApplySymGrid_1 sym ->
         (function
          | [|d1|] -> Funct.apply_symmetry_grid sym e d1
          | _ -> Result.Error (Invalid_expr e))
      | `UnfoldSym_1 sym_matrix ->
         (function
          | [|d1|] -> Funct.unfold_symmetry sym_matrix e d1
          | _ -> Result.Error (Invalid_expr e))
      | `CloseSym_2 sym_matrix ->
         (function
          | [| `Color bgcolor; d2|] -> Funct.close_symmetry sym_matrix bgcolor e d2
          | _ -> Result.Error (Invalid_expr e))
      | `TranslationSym_2 sym ->
         (function
          | [|d1;d2|] ->
         (match get_pos d1, get_size d1, get_pos d2, get_size d2 with
          | Some (mini1,minj1), Some (h1,w1), Some (mini2,minj2), Some (h2,w2) ->
             let| ti, tj =
               match sym with
               | `Id -> Result.Ok (0, 0)
               | `FlipHeight -> Result.Ok (2 * (mini2-mini1) + (h2-h1), 0)
               | `FlipWidth -> Result.Ok (0, 2 * (minj2-minj1) + (w2-w1))
               | `Rotate180 -> Result.Ok (2 * (mini2-mini1) + (h2-h1), 2 * (minj2-minj1) + (w2-w1))
               | `FlipDiag1 ->
                  if h2 = w2
                  then
                    let ti = (mini2 - mini1) - (minj2 - minj1) (* + (h2 - w2) / 2 *) in
                    Result.Ok (ti, - ti)
                  else Result.Error (Undefined_result "TranslationSym: FlipDiag1: non-square pivot object")
               | `FlipDiag2 ->
                  if h2 = w2 && (h2 - h1 + w2 - w1 mod 2 = 0)
                  then
                    let ti = (mini2 - mini1) + (minj2 - minj1) + (h2 - h1 + w2 - w1) / 2 in
                    Result.Ok (ti, - ti)
                  else Result.Error (Undefined_result "TranslationSym: FlipDiag2: non-square pivot object")
               | `Rotate90 ->
                  if h2 = w2
                  then
                    Result.Ok
                      ((mini2 - mini1) - (minj2 - minj1) (* + (h2 - w2) / 2 *),
                       (mini2 - mini1) + (minj2 - minj1) + (h2 + w2) / 2 - h1) (* /2 OK because h2=w2 *)
                  else Result.Error (Undefined_result "TranslationSym: Rotate90: non-square pivot object")
               | `Rotate270 ->
                  if h2 = w2
                  then
                    Result.Ok
                      ((minj2 - minj1) + (mini2 - mini1) + (h2 + w2) / 2 - w1 (* /2 OK because h2=w2 *),
                       (minj2 - minj1) - (mini2 - mini1)) (* - (h2 - w2) / 2 *)
                  else Result.Error (Undefined_result "TranslationSym: Rotate90: non-square pivot object")
             in
             Result.Ok (`Vec (ti, tj))
          | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `MajorityColor_1 ->
         (function
          | [| `Grid g|] ->
             let| c = Grid.majority_color Grid.black g in
             Result.Ok (`Color c)
          | _ -> Result.Error (Invalid_expr e))
      | `ColorCount_1 ->
         (function
          | [| `Grid g|] ->
             let n = Grid.color_count Grid.black g in
             Result.Ok (`Int n)
          | _ -> Result.Error (Invalid_expr e))
      | `Coloring_2 ->
         (function
          | [| d1; `Color c|] ->
             (match d1 with
              | `Grid g ->
                 let m = Grid.Mask.from_grid_background Grid.transparent g in (* collapsing all colors *)
                 let g' = Grid.Mask.to_grid m Grid.transparent c in (* mask to shape with color c *)
                 Result.Ok (`Grid g')
              (* | `Obj (i, j, shape) ->
                 let| shape =  shape in
                 Result.Ok (`PosShape (pos, shape)) *)
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `SwapColors_3 ->
         (function
          | [| `Grid g; `Color c1; `Color c2|] ->
             let| g' = Grid.Transf.swap_colors g c1 c2 in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))

    let eval_func (f : func) (args_tree : value Ndtree.t array) : value Ndtree.t result = (* QUICK *)
      let e = "" in
      match f, args_tree with
      | `Index_1 is, [|v1|] ->
         (match Ndtree.index v1 is with
          | Some v -> Result.Ok v
          | None -> Result.Error (Invalid_expr e))
      | _ ->
         let scalar_f = compile_scalar_func f in
         Ndtree.broadcast_result scalar_f args_tree

    let eval_unbound_var x = Result.Ok (Ndtree.scalar (Some `Null))
    let eval_arg () = Result.Error (Failure "eval: unexpected Arg")

    (* model-based generation *)
      
(*    let generator_pat t c gen_args =
      match t, c, gen_args with
      | INT (COORD (axis,tv)), AnyCoord, [||] ->
         (fun (h,w,c) ->
           let bound = match axis with I -> h | J -> w in
           let ij, range =
             match tv with
             | SIZE -> min (max 1 (bound / 3)) bound, Range.make_closed 1 bound
             | POS | MOVE -> 0, Range.make_closed 0 (bound-1) in
           Myseq.return (make_danycoord ij range))
      | _, Vec, [|gen_i; gen_j|] ->
         (fun (h,w,c) ->
           let* di = gen_i (h,w,c) in
           let* dj = gen_j (h,w,c) in
           Myseq.return (make_dvec di dj))
      | COLOR tc, AnyColor, [||] ->
         (fun (h,w,c) -> Myseq.return (make_danycolor (min (c+1) Grid.last_color) tc))
      | MOTIF, AnyMotif, [||] ->
         (fun (h,w,c) -> Myseq.return (make_danymotif GPat.Motif.FlipHW))
      | GRID ((filling,nocolor) as tg), AnyGrid, [||] ->
         (fun (h,w,c) ->
           let c1 = if nocolor then Grid.Mask.one else min (c+1) Grid.last_color in
           let h1, w1 = min (max 1 (h/3)) h, min (max 1 (w/3)) w in
           let g1 = Grid.make h1 w1 c1 in (* TODO: specialize according to filling *) 
           Myseq.return (make_danygrid g1 tg (Range.make_closed 1 h) (Range.make_closed 1 w)))
      | _, Obj, [|gen_pos; gen_g1|] ->
         (fun (h,w,c) ->
           let* dg1 = gen_g1 (h,w,c) in
           let g1 = get_grid dg1 in
           let h1, w1 = Grid.dims g1 in
           let* dpos = gen_pos (h-h1, w-w1, c) in
           Myseq.return (make_dobj dpos dg1))
      | _, BgColor, [|gen_col; gen_g1|] ->
         (fun (h,w,c) ->
           let* dcol = gen_col (h,w,c) in
           let c = get_color dcol in
           let* dg1 = gen_g1 (h,w,c) in
           Myseq.return (make_dbgcolor dcol dg1))
      | _, IsFull, [|gen_g1|] ->
         (fun (h,w,c) ->
           let* dg1 = gen_g1 (h,w,c) in
           Myseq.return (make_disfull dg1))
      | _, Crop, [|gen_size; gen_pos; gen_g1|] ->
         (fun (h0,w0,c0) ->
           let* dsize = gen_size (h0,w0,c0) in
           let h, w = get_vec dsize in
           let* dpos = gen_pos (h,w,c0) in
           let* dg1 = gen_g1 (h,w,c0) in
           Myseq.return (make_dcrop dsize dpos dg1))
      | _, Objects seg, [|gen_size; gen_objs|] ->
         (fun (h0,w0,c0) ->
           let* dsize = gen_size (h0,w0,c0) in
           let h, w = get_vec dsize in
           let* dobjs = gen_objs (h,w,c0) in (* TODO: take seg into account... *)
           Myseq.return (make_dobjects seg dsize dobjs))
      | _, Monocolor, [|gen_col; gen_mask|] ->
         (fun (h,w,c) ->
           let* dcol = gen_col (h,w,c) in
           let* dmask = gen_mask (h,w,c) in
           Myseq.return (make_dmonocolor dcol dmask))
      | _, Motif, [|gen_mot; gen_core; gen_noise|] ->
         (fun (h0,w0,c0) ->
           let* dmot = gen_mot (h0,w0,c0) in
           let* dcore = gen_core (h0,w0,c0) in
           let* dnoise = gen_noise (h0,w0,c0) in
           Myseq.from_result (make_dmotif dmot dcore dnoise))
      | _, Empty, [|gen_size|] ->
         (fun (h,w,c) ->
           let* dsize = gen_size (h,w,c) in
           Myseq.return (make_dempty dsize))
      | _, Full, [|gen_size|] ->
         (fun (h,w,c) ->
           let* dsize = gen_size (h,w,c) in
           Myseq.return (make_dfull dsize))
      | _, Point, [||] ->
         (fun (h,w,c) ->
           Myseq.return (make_dpoint))
      | _ -> assert false *)

    let default_grid (filling, nocolor) (h, w) =
      match filling, nocolor with
      | `Full, _ -> Grid.make h w Grid.black
      | `Sprite, false -> Grid.make h w Grid.blue
      | `Sprite, true -> Grid.make h w Grid.Mask.one
      | `Noise, _ -> Grid.make h w Grid.transparent
    let default_grid, reset_default_grid =
      Memo.memoize2 ~size:103 default_grid

    let generator_pat t c gen_args = (* systematic version *)
      match t, c, gen_args with
      | INT (COORD (axis,tv)), AnyCoord, [||] ->
         let a, b =
           match tv with
           | SIZE -> 1, Grid.max_size
           | POS | MOVE -> 0, Grid.max_size-1 in
         let range = Range.make_closed a b in
         (fun info ->
           let* ij = Myseq.range a b in
           Myseq.return (make_danycoord ij range))
      | _, Vec, [|gen_i; gen_j|] ->
         (fun info ->
           let* lij = Myseq.product_fair [gen_i info; gen_j info] in
           match lij with
           | [di; dj] -> Myseq.return (make_dvec di dj)
           | _ -> assert false)
      | COLOR tc, AnyColor, [||] ->
         let a, b =
           match tc with
           | C_BG true | C_OBJ -> Grid.black, Grid.last_color
           | C_BG false -> Grid.black, Grid.transparent in
         (fun info ->
           let* c = Myseq.range a b in
           Myseq.return (make_danycolor c tc))
      | MOTIF, AnyMotif, [||] ->
         (fun info ->
           let* mot = Myseq.from_list GPat.Motif.candidates in
           Myseq.return (make_danymotif mot))
      | GRID tg, AnyGrid, [||] ->
         let range = Range.make_closed 1 Grid.max_size in
         let nc = Grid.nb_color in
         (fun info ->
           let* lhw = Myseq.product_fair [Myseq.range 1 8; Myseq.range 1 8] in
           match lhw with
           | [h; w] ->
              let g = default_grid tg (h,w) in 
              Myseq.return (make_danygrid g tg range range nc)
           | _ -> assert false)
      | _, Obj, [|gen_pos; gen_g1|] ->
         (fun info ->
           let* lposg1 = Myseq.product_fair [gen_pos info; gen_g1 info] in
           match lposg1 with
           | [dpos; dg1] -> Myseq.return (make_dobj dpos dg1)
           | _ -> assert false)
      | MAP (ta,tb), AnyMap, [||] ->
         (fun info ->
           Myseq.return (make_danymap tb Mymap.empty)) (* empty map = identity map *)
      | _, DomMap keys, [|gen_vals|] ->
         (fun info ->
           let* dvals = gen_vals info in
           match Data.value dvals with
           | `Seq vals when Array.length vals = Array.length keys ->
              Myseq.return (make_ddommap keys dvals)
           | _ -> Myseq.empty)
      | _, Replace, [|gen_a; gen_b|] ->
         (fun info ->
           let* lab = Myseq.product_fair [gen_a info; gen_b info] in
           match lab with
           | [da; db] ->
              let a, b = Data.value da, Data.value db in
              let dom = [|a; b|] in (* get the true domain through info *)
              Myseq.return (make_dreplace dom da db)
           | _ -> assert false)
      | _, Swap, [|gen_a; gen_b|] ->
         (fun info ->
           let* lab = Myseq.product_fair [gen_a info; gen_b info] in
           match lab with
           | [da; db] ->
              let a, b = Data.value da, Data.value db in
              let dom = [|a; b|] in (* get the true domain through info *)
              Myseq.return (make_dswap dom da db)
           | _ -> assert false)
      | _, BgColor, [|gen_col; gen_g1|] ->
         (fun info ->
           let* lcg1 = Myseq.product_fair [gen_col info; gen_g1 info] in
           match lcg1 with
           | [dcol; dg1] -> Myseq.return (make_dbgcolor dcol dg1)
           | _ -> assert false)
      | _, IsFull, [|gen_g1|] ->
         (fun info ->
           let* dg1 = gen_g1 info in
           Myseq.return (make_disfull dg1))
      | _, Crop, [|gen_g; gen_pos; gen_size|] ->
         (fun info ->
           let* l = Myseq.product_fair [gen_g info; gen_pos info; gen_size info] in
           match l with
           | [dg; dpos; dsize] ->
              let* dg1 = Myseq.from_result (make_dcrop dg dpos dsize) in
              Myseq.return dg1 
           | _ -> assert false)
      | _, Objects (seg,nmax), [|gen_size; gen_objs|] ->
         (fun info ->
           let* l = Myseq.product_fair [gen_size info; gen_objs info] in
           match l with
           | [dsize; dobjs] ->
              (match Data.value dobjs with
               | `Seq vs1 when Array.length vs1 <= nmax -> 
                  Myseq.return (make_dobjects seg nmax dsize dobjs)
               | _ -> Myseq.empty)
           | _ -> assert false)
      | _, Monocolor, [|gen_col; gen_mask|] ->
         (fun info ->
           let* l = Myseq.product_fair [gen_col info; gen_mask info] in
           match l with
           | [dcol; dmask] -> Myseq.return (make_dmonocolor dcol dmask)
           | _ -> assert false)
      (* | _, Recoloring, [|gen_cols; gen_grid|] ->
         (fun info ->
           let* l = Myseq.product_fair [gen_cols info; gen_grid info] in
           match l with
           | [dcols; dgrid] -> Myseq.return (make_drecoloring dcols dgrid)
           | _ -> assert false) *)
      | _, Recoloring, [|gen_grid; gen_map|] ->
         (fun info ->
           let* l = Myseq.product_fair [gen_grid info; gen_map info] in
           match l with
           | [dgrid; dmap] -> Myseq.return (make_drecoloring dgrid dmap)
           | _ -> assert false)
      | _, Motif, [|gen_mot; gen_core; gen_noise|] ->
         (fun info ->
           let* l = Myseq.product_fair [gen_mot info; gen_core info; gen_noise info] in
           match l with
           | [dmot; dcore; dnoise] -> Myseq.from_result (make_dmotif dmot dcore dnoise)
           | _ -> assert false)
      | _, Empty, [|gen_size|] ->
         (fun info ->
           let* dsize = gen_size info in
           Myseq.return (make_dempty dsize))
      | _, Full, [|gen_size|] ->
         (fun info ->
           let* dsize = gen_size info in
           Myseq.return (make_dfull dsize))
      | _, Point, [||] ->
         (fun info ->
           Myseq.return (make_dpoint))
      | _ -> assert false

    (* model-based parsing *)
           
    let input_of_value (t : typ) (v : value) : input =
      match t, v with
      | _, `Null -> `Null
      | INT (COORD (axis,tv)), `Int i ->
         let range =
           match tv with
           | SIZE -> Range.make_closed 1 Grid.max_size
           | POS | MOVE -> Range.make_closed 0 Grid.max_size in
         `IntRange (i, range)
      | VEC tv, `Vec (i,j) ->
         let range =
           match tv with
           | SIZE -> Range.make_closed 1 Grid.max_size
           | POS | MOVE -> Range.make_closed 0 Grid.max_size in
         `Vec (`IntRange (i,range), `IntRange (j,range)) 
      | COLOR tc, `Color c -> `Color c
      | MOTIF, `Motif mot -> `Motif mot
      | GRID (filling,nocolor), `Grid g -> `GridDimsCols (g, Range.make_open 1, Range.make_open 1, Grid.nb_color)
      | OBJ (filling,nocolor), `Obj obj -> `Objects (Grid.max_size, Grid.max_size, Grid.nb_color, [obj])
      | MAP _, `Map m ->
         let domain = mymap_keys m in
         `MapDomain (m, domain)
      | _ -> assert false

    let parseur_value v input =
      let rec aux v input =
        match v, input with
        | `Int i0, `IntRange (i,_) -> i = i0, `Null
        | `Vec (i,j), `Vec (in_i,in_j) ->
           let ok_i, _ = aux (`Int i) in_i in
           let ok_j, _ = aux (`Int j) in_j in
           ok_i && ok_j, `Null
        | `Color c0, `Color c -> c = c0 && Grid.is_true_color c, `Null
        | `Color c0, `Seq (`Color c::lc) -> c = c0 && Grid.is_true_color c, `Seq lc
        | `Motif mot0, `Motif mot -> mot = mot0, `Null
        | `Grid g0, `GridDimsCols (g,_,_,_) -> g = g0, `Null
        | `Obj obj0, `Objects(h,w,nc,objs) ->
           if List.mem obj0 objs
           then true, `Objects (h, w, nc, List.filter ((<>) obj0) objs)
           else false, input
        | `Map m0, `MapDomain (m,dom) -> m0 = m, `Null
           
        | _, `Null -> true, `Null
        | _ -> false, input in
      let ok, input = aux v input in
      if ok
      then Myseq.return (Data.make_dexpr v, input)
      else Myseq.empty
        
    let parseur_pat t c parse_args =
      match t, c, parse_args with
      | _, AnyCoord, [||] ->
         (function
          | `IntRange (ij,range) -> Myseq.return (make_danycoord ij range, `Null)
          | _ -> assert false)
      | _, Vec, [|parse_i; parse_j|] ->
         (function
          | `Vec (in_i, in_j) ->
             let* di, _ = parse_i in_i in
             let* dj, _ = parse_j in_j in
             Myseq.return (make_dvec di dj, `Null)
          | _ -> assert false)
      | COLOR tc, AnyColor, [||] ->
         (function
          | `Color c ->
             if Grid.is_true_color c
             then Myseq.return (make_danycolor c tc, `Null)
             else Myseq.empty
          | `Seq (`Color c::lc) -> (* TODO: generalize over other types *)
             if Grid.is_true_color c
             then Myseq.return (make_danycolor c tc, `Seq lc)
             else Myseq.empty
          | `Seq [] -> Myseq.empty
          | _ -> assert false)
      | MOTIF, AnyMotif, [||] ->
         (function
          | `Motif mot -> Myseq.return (make_danymotif mot, `Null)
          | _ -> assert false)
      | GRID tg, AnyGrid, [||] ->
         (function
          | `GridDimsCols (g,rh,rw,nc) ->
             Myseq.return (make_danygrid g tg rh rw nc, `Null)
          | _ -> assert false)
      | _, Obj, [|parse_pos; parse_g1|] ->
         (function
          | `Objects (h, w, nc, objs) ->
             Myseq.bind_interleave_at_most 3
               (Myseq.from_list objs)
               (fun (i,j,g1 as obj) ->
                 let other_objs = List.filter ((<>) obj) objs in
                 let* dg1, _ = parse_g1 (`GridDimsCols (g1,
                                                    Range.make_closed 1 (h-i),
                                                    Range.make_closed 1 (w-j),
                                                    nc)) in
                 let* dpos, _ = parse_pos (`Vec (`IntRange (i, Range.make_closed 0 (h-1)),
                                                 `IntRange (j, Range.make_closed 0 (w-1)))) in
                 Myseq.return (make_dobj dpos dg1, `Objects (h,w,nc,other_objs)))
          | _ -> assert false)
      | MAP (ta,tb), AnyMap, [||] ->
         (function
          | `MapDomain (m,dom) ->
             Myseq.return (make_danymap tb m, `Null)
          | _ -> assert false)
      | MAP (ta,tb), DomMap keys, [|parse_vals|] ->
         (function
          | `MapDomain (m,dom) ->
             let pairs = Mymap.bindings m in
             let m_keys = Array.of_list (List.map fst pairs) in
             if m_keys = keys
             then
               let vals = List.map snd pairs in
               let* dvals, input = parse_vals (`Seq (List.map (input_of_value tb) vals)) in
               if input = `Seq []
               then Myseq.return (make_ddommap keys dvals, `Null)
               else Myseq.empty
             else Myseq.empty
          | _ -> assert false)
      | MAP (ta,tb), Replace, [|parse_a; parse_b|] when ta=tb ->
         (function
          | `MapDomain (m,dom) ->
             let m_diff = Mymap.filter (fun a b -> a <> b) m in
             (match Mymap.bindings m_diff with
              | [a, b] ->
                 let* da, _ = parse_a (input_of_value ta a) in
                 let* db, _ = parse_b (input_of_value ta b) in
                 Myseq.return (make_dreplace dom da db, `Null)
              | _ -> Myseq.empty)
          | _ -> assert false)
      | MAP (ta,tb), Swap, [|parse_a; parse_b|] when ta=tb ->
         (function
          | `MapDomain (m,dom) ->
             let m_diff = Mymap.filter (fun a b -> a <> b) m in
             (match Mymap.bindings m_diff with
              | [a, b; c, d] when a=d && b=c ->
                 let* da, _ = parse_a (input_of_value ta a) in
                 let* db, _ = parse_b (input_of_value ta b) in
                 Myseq.return (make_dswap dom da db, `Null)
              | _ -> Myseq.empty)
          | _ -> assert false)
      | _, BgColor, [|parse_col; parse_g1|] ->
         (function
          | `GridDimsCols (g,rh,rw,nc) ->
             if Grid.is_full g
             then
               let* bc = Myseq.from_list (Segment.background_colors g) in
               let* dcol, _ = parse_col (`Color bc) in
               let* g1 = Myseq.from_result (Grid.Transf.swap_colors g bc Grid.transparent) in
               let nc1 = if g.Grid.color_count.(bc) > 0 then nc-1 else nc in
               let* dg1, _ = parse_g1 (`GridDimsCols (g1,rh,rw,nc1)) in
               Myseq.return (make_dbgcolor dcol dg1, `Null)
             else Myseq.empty
          | _ -> assert false)
      | _, IsFull, [|parse_g1|] ->
         (fun input ->
           let* dg1, _ = parse_g1 input in
           let g1 = get_grid dg1 in
           if Grid.is_full g1
           then Myseq.return (make_disfull dg1, `Null)
           else Myseq.empty)
      | _, Crop, [|parse_g; parse_pos; parse_size|] ->
         (function
          | `GridDimsCols (g1,rh1,rw1,nc1) ->
             let h1, w1 = Grid.dims g1 in
             let* dsize, _ =
               parse_size (`Vec (`IntRange (h1, rh1),
                                 `IntRange (w1, rw1))) in
             let* dg, _ = parse_g `Null in (* expression *)
             let g =
               match Data.value dg with
               | `Grid g -> g
               | v -> pp_endline xp_data dg; assert false in
             let h, w = Grid.dims g in
             let* i, j = Myseq.from_list (Grid_patterns.parse_crop g g1) in
             let* dpos, _ =
               parse_pos (`Vec (`IntRange (i, Range.make_closed 0 (h-h1)),
                                `IntRange (j, Range.make_closed 0 (w-w1)))) in
             let* dg1 = Myseq.from_result (make_dcrop dg dpos dsize) in
             Myseq.return (dg1, `Null)
(*             
             let h, w = Grid.dims g in
             let* dsize, _ = parse_size (`Vec (`IntRange (h, rh),
                                               `IntRange (w, rw))) in
             (match Grid_patterns.segment g with
              | [(i,j,g1)] ->
                 let* dg1, _ = parse_g1 (`GridDimsCols (g1,
                                                        Range.make_closed 1 h,
                                                        Range.make_closed 1 w,
                                                        nc)) in
                 let h1, w1 = Grid.dims (get_grid dg1) in
                 let* dpos, _ = parse_pos (`Vec (`IntRange (i, Range.make_closed 0 (h-h1)),
                                                 `IntRange (j, Range.make_closed 0 (w-w1)))) in
                 Myseq.return (make_dcrop dsize dpos dg1, `Null)
              | _ -> Myseq.empty)
 *)
          | _ -> assert false)
      | _, Objects (seg,nmax), [|parse_size; parse_objs|] ->
         (function
          | `GridDimsCols (g,rh,rw,nc) ->
             if Grid.is_full g then Myseq.empty
             else              
               let h, w = Grid.dims g in
               let* dsize, _ = parse_size (`Vec (`IntRange (h, rh),
                                                 `IntRange (w, rw))) in
               let objs =
                 match seg with
                 | `Default -> Grid_patterns.segment g
                 | `SameColor -> Grid_patterns.segment_same_color g in
               let* () = Myseq.from_bool (List.length objs <= nmax) in
               let* dobjs, input = parse_objs (`Objects (h,w,nc,objs)) in
               (match input with
               | `Objects (_,_,_,[]) -> Myseq.return (make_dobjects seg nmax dsize dobjs, `Null)
               | _ -> Myseq.empty) (* all objects must be used *)
          | _ -> assert false)
      | _, Monocolor, [|parse_col; parse_mask|] ->
         (function
          | `GridDimsCols (g,rh,rw,nc) ->
             if Grid.color_count Grid.transparent g = 1
             then
               let* c = Myseq.from_result (Grid.majority_color Grid.transparent g) in
               let* dcol, _ = parse_col (`Color c) in
               let* mask = Myseq.from_result (Grid.Transf.swap_colors g c Grid.Mask.one) in
               let* dmask, _ = parse_mask (`GridDimsCols (mask,rh,rw,1)) in
               Myseq.return (make_dmonocolor dcol dmask, `Null)
             else Myseq.empty
          | _ -> assert false)
      (* | _, Recoloring, [|parse_cols; parse_grid|] ->
         (function
          | `GridDimsCols (g,rh,rw,nc) ->
             let* g1, palette = Myseq.from_result (Grid_patterns.recoloring g) in
             let nc1 = Array.length palette in
             let* () = Myseq.from_bool (nc1 >= 2 && g1 <> g) in
             let* dcols, input = parse_cols (`Seq (Array.to_list palette)) in
             (match input with
              | `Seq [] ->
                 let* dgrid, _ = parse_grid (`GridDimsCols (g1,rh,rw,nc1)) in
                 Myseq.return (make_drecoloring dcols dgrid, `Null)
              | _ -> Myseq.empty)
          | _ -> assert false) *)
      | _, Recoloring, [|parse_grid; parse_map|] ->
         (function
          | `GridDimsCols (g,rh,rw,nc) ->
             let* dg1, _ = parse_grid `Null in (* expression expected *)
             let g1 = get_grid dg1 in
             (match Grid_patterns.parse_recoloring g g1 with
              | Some mcol ->
                 let m =
                   Mymap.fold
                     (fun c1 c2 res ->
                       Mymap.add (`Color c1) (`Color c2) res)
                     mcol (Mymap.empty : (value,value) Mymap.t) in
                 let dom = mymap_keys m in
                 let* dmap, _ = parse_map (`MapDomain (m,dom)) in
                 Myseq.return (make_drecoloring dg1 dmap, `Null)
              | None -> Myseq.empty)
          | _ -> assert false)
      | _, Motif, [|parse_mot; parse_core; parse_noise|] ->
         (function
          | `GridDimsCols (g,rh,rw,nc) ->
             let* mot, ru, rv, g_core, g_noise = Myseq.from_list (GPat.Motif.from_grid g) in
             let* dmot, _ = parse_mot (`Motif mot) in
             let* dcore, _ = parse_core (`GridDimsCols (g_core,ru,rv,nc)) in
             let* dnoise, _ = parse_noise (`GridDimsCols (g_noise,rh,rw,nc)) in
             let* data = Myseq.from_result (make_dmotif dmot dcore dnoise) in
             Myseq.return (data, `Null)
          | _ -> assert false)            
      | _, (Empty | Full as c), [|parse_size|] ->
         let pred_maked h w = function
           | Empty -> (fun i j c -> c = Grid.Mask.zero), make_dempty
           | Full -> (fun i j c -> c = Grid.Mask.one), make_dfull
           | _ -> assert false
         in
         (function
          | `GridDimsCols (mask,rh,rw,nc) -> (* nc = 1 *)
             let h, w = Grid.dims mask in
             let pred, maked = pred_maked h w c in
             if Grid.for_all_pixels pred mask
             then
               let* dsize, _ = parse_size (`Vec (`IntRange (h, rh),
                                                 `IntRange (w, rw))) in
               Myseq.return (maked dsize, `Null)
             else Myseq.empty
          | _ -> assert false)
      | _, Point, [||] ->
         (function
          | `GridDimsCols (mask,rh,rw,nc) -> (* nc=1 *)
             let h, w = Grid.dims mask in
             if h=1 && w=1 && Grid.Mask.mem 0 0 mask
             then Myseq.return (make_dpoint, `Null)
             else Myseq.empty
          | _ -> assert false)
      | _ -> assert false

    (* description length *)

    let dl_color (c : Grid.color) (tc : typ_color) : dl =
      (* Mdl.Code.uniform Grid.nb_color *)
      match tc with
      | C_OBJ ->
         if c = 0 then Mdl.Code.usage 0.091
         else (* 0.909 for other colors in total *)
           if c > 0 && c < 10 (* 9 colors *)
           then Mdl.Code.usage 0.101
           else invalid_arg ("dl_shape_color: Unexpected color: " ^ Grid.name_of_color c)
      | C_BG full ->
         let bgcolor, nbcolor =
           if full
           then Grid.black, Grid.nb_color - 1
           else Grid.transparent, Grid.nb_color in
         if c = bgcolor then Mdl.Code.usage 0.910
         else (* 0.090 for other colors in total *)
           if c >= Grid.black && c <= Grid.last_color (* nbcolor *)
           then Mdl.Code.usage (0.090 /. float nbcolor)
           else invalid_arg ("dl_background_color: Unexpected color: " ^ Grid.name_of_color c)

    let dl_motif (m : GPat.Motif.t) : dl =
      Mdl.Code.uniform GPat.Motif.nb_candidates
         
    let dl_grid g (filling,nocolor) rh rw nc : dl = (* too efficient a coding for being useful? *)
      (* nc is nb of colors, not including transparent or undefined, nocolor implies nc=1 *)
      let h, w = Grid.dims g in
      let area = h * w in
      let in_mask = area - g.color_count.(Grid.transparent) in
      let dl_color =
        if nc = 0
        then (assert (in_mask = 0); 0.)
        else Mdl.Code.uniform nc in
      Range.dl h rh +. Range.dl w rw
      +. (match filling with
          | `Full -> float area *. dl_color
          | `Sprite -> float area (* sprite mask positions *)
                       +. (if nocolor then 0. else float in_mask *. dl_color) (* sprite colors *)
          | `Noise -> Mdl.Code.universal_int_star in_mask (* noise area *)
                      +. Mdl.Code.comb in_mask area (* noise position *)
                      +. (if nocolor then 0. else float in_mask *. dl_color)) (* noise colors *)
           
    let dl_map dl_a dl_b m =
      (* TODO: should get constraint info about values, e.g. range for integers *)
      Mymap.fold
        (fun a b res -> dl_a a +. dl_b b +. res)
        m 0.

    let rec dl_value t v =
      match t, v with
      | _, `Null -> assert false
      | BOOL, `Bool b -> 1.
      | INT CARD, `Int i ->
         Mdl.Code.universal_int_star i
      | INT (COORD (axis,tv)), `Int ij ->
         assert (ij >= 0 && ij <= Grid.max_size);
         Range.dl ij (Range.make_closed 0 Grid.max_size)
      | VEC tv, `Vec (i,j) ->
         dl_value (INT (COORD (I,tv))) (`Int i)
         +. dl_value (INT (COORD (J,tv))) (`Int j)
      | COLOR tc, `Color c -> dl_color c tc
      | MOTIF, `Motif m -> dl_motif m
      | GRID tg, `Grid g ->
         let rmax = Range.make_closed 1 Grid.max_size in
         dl_grid g tg rmax rmax Grid.nb_color
      | _, `Obj (i,j,g) ->
         dl_value (INT (COORD (I, POS))) (`Int i)
         +. dl_value (INT (COORD (J, POS))) (`Int j)
         +. dl_value (GRID (`Sprite,false)) (`Grid g)
      | MAP (ta,tb), `Map m ->
         Mdl.Code.universal_int_star (Mymap.cardinal m)
         +. dl_map (dl_value ta) (dl_value tb) m
      | _, `Seq _ -> assert false
      | _ -> assert false

           
    let encoding_dpat dc encs =
      match dc, encs with
      | DAnyCoord (ij,range), [||] -> Range.dl ij range
      | DVec, [|enc_i; enc_j|] ->  enc_i +. enc_j
      | DAnyColor (c,tc), [||] -> dl_color c tc
      | DAnyMotif m, [||] -> dl_motif m
      | DAnyGrid (g,tg,rh,rw,nc), [||] -> dl_grid g tg rh rw nc
      | DObj, [|enc_pos; enc_g1|] -> enc_pos +. enc_g1
      | DAnyMap (m,tb), [||] -> dl_map (fun _ -> 0.) (dl_value tb) m (* assuming keys known from ctx *)
      | DDomMap keys, [|enc_vals|] -> enc_vals (* keys encoded in model *)
      | DReplace, [|enc_a; enc_b|] -> enc_a +. enc_b
      | DSwap, [|enc_a; enc_b|] -> enc_a +. enc_b
      | DBgColor, [|enc_col; enc_g1|] -> enc_col +. enc_g1
      | DIsFull, [|enc_g1|] -> enc_g1
      | DCrop, [|enc_g; enc_pos; enc_size|] -> enc_g +. enc_pos +. enc_size
      | DObjects (seg,nmax), [|enc_size; enc_objs|] -> enc_size +. enc_objs (* TODO: take seg into account for encoding objects *)
      | DMonocolor, [|enc_col; enc_mask|] -> enc_col +. enc_mask
      (* | DRecoloring, [|enc_cols; enc_grid|] -> enc_cols +. enc_grid *)
      | DRecoloring, [|enc_grid; enc_map|] -> enc_grid +. enc_map
      | DMotif, [|enc_motif; enc_core; enc_noise|] -> enc_motif +. enc_core +. enc_noise
      | DEmpty, [|enc_size|] -> enc_size
      | DFull, [|enc_size|] -> enc_size
      | DPoint, [||] -> 0.
      | _ -> assert false
    let encoding_alt dl_choice enc = dl_choice +. enc
    let encoding_seq dl_length encs = dl_length +. Array.fold_left (+.) 0. encs
    let encoding_expr_value v = 0.
    let dl_of_encoding enc = enc
           
    let dl_var ~nb_env_vars t p =
      let k = max 1 nb_env_vars in (* to avoid 0, happens in pruning mode *)
      Mdl.Code.uniform k

    let dl_constr_params t c =
      match t, c with
      | _, AnyCoord -> 0.
      | _, Vec -> 0.
      | _, AnyColor -> 0.
      | _, AnyMotif -> 0.
      | _, AnyGrid -> 0.
      | _, Obj -> 0.
      | _, AnyMap -> 0.
      | MAP (ta,tb), DomMap keys ->
         assert (keys <> [||]);
         Mdl.Code.universal_int_plus (Array.length keys)
         +. Array.fold_left
              (fun res a -> dl_value ta a)
              0. keys
      | _, DomMap _ -> assert false
      | _, Replace -> 0.
      | _, Swap -> 0.
      | _, BgColor -> 0.
      | _, IsFull -> 0.
      | _, Crop -> 0.
      | _, Objects (seg,nmax) ->
         Mdl.Code.usage
           (match seg with
            | `Default -> 0.5
            | `SameColor -> 0.5)
         +. Mdl.Code.universal_int_plus nmax
      | _, Monocolor -> 0.
      | _, Recoloring -> 0.
      | _, Motif -> 0.
      | _, Empty -> 0.
      | _, Full -> 0.
      | _, Point -> 0.

           
    let dl_periodicity_mode : Grid.Transf.periodicity_mode -> dl = function
      | `Total -> Mdl.Code.usage 0.25
      | `Strict -> Mdl.Code.usage 0.25
      | `TradeOff -> Mdl.Code.usage 0.5
           
    let dl_func_params (t : typ) : func -> dl = function
      | `Index_1 is ->
         assert (is <> []);
         Mdl.Code.universal_int_plus (List.length is)
         +. Mdl.sum is
              (function
               | None -> Mdl.Code.usage 0.25
               | Some i -> Mdl.Code.usage 0.75
                           +. (if i >= 0
                               then Mdl.Code.usage 0.75 +. Mdl.Code.universal_int_star i
                               else Mdl.Code.usage 0.25 +. Mdl.Code.universal_int_plus (-i)))
      | `Plus_2 -> 0.
      | `Minus_2 -> 0.
      | `Modulo_2 -> 0.
      | `ScaleUp_2 -> 0.
      | `ScaleDown_2 -> 0.
      | `ScaleTo_2 -> 0.
      | `Size_1 -> 0.
      | `Height_1 -> 0.
      | `Width_1 -> 0.
      | `Crop_2 -> 0.
      | `Strip_1 -> 0.
      | `Corner_2 -> 0.
      | `Min_n -> 0.
      | `Max_n -> 0.
      | `Average_n -> 0.
      | `Span_2 -> 0.
      | `Norm_1 -> 0.
      | `Diag1_1 k -> Mdl.Code.universal_int_star k
      | `Diag2_1 k -> Mdl.Code.universal_int_star k
      | `LogAnd_2 | `LogOr_2 | `LogXOr_2 | `LogAndNot_2 | `LogNot_1 -> 0.
      | `Stack_n -> 0.
      | `Area_1 -> 0.
      | `Left_1 | `Right_1 | `Center_1 | `Top_1 | `Bottom_1 | `Middle_1 -> 0.
      | `ProjI_1 | `ProjJ_1 -> 0.
      | `MaskOfGrid_1 | `GridOfMask_2 -> 0.
      | `TranslationOnto_2 -> 0.
      | `Tiling_1 (k,l) -> Mdl.Code.universal_int_plus k +. Mdl.Code.universal_int_plus l
      | `PeriodicFactor_2 p -> dl_periodicity_mode p
      | `FillResizeAlike_3 p -> dl_periodicity_mode p
      | `SelfCompose_2 -> 0.
      | `ApplySymVec_1 (sym,tv) -> Mdl.Code.uniform nb_symmetry +. Mdl.Code.uniform nb_typ_vec
      | `ApplySymGrid_1 sym -> Mdl.Code.uniform nb_symmetry
      | `UnfoldSym_1 symar -> Mdl.Code.uniform nb_symmetry_unfold
      | `CloseSym_2 symar -> Mdl.Code.uniform nb_symmetry_unfold
      | `TranslationSym_2 sym -> Mdl.Code.uniform nb_symmetry
      | `MajorityColor_1 -> 0.
      | `ColorCount_1 -> 0.
      | `Coloring_2 -> 0.
      | `SwapColors_3 -> 0.

    (* expression index *)

    let make_index (bindings : bindings) : expr_index =
      (*pp xp_bindings bindings;*)
      (*let test level index = (* testing expr index[i]($21) in task a157, $21 is seq of pos of input objects *)
        match Mymap.find_opt 21 bindings with
        | None -> ()
        | Some (t,v_tree) ->
           print_endline level;
           assert (t = VEC POS);
           assert (Ndtree.ndim v_tree = 1);
           assert (Ndtree.length v_tree = Some 5);
           [1;2;-2;-1]
           |> List.iter
                (fun i ->
                  print_int i;
                  match Ndtree.index v_tree [Some i] with
                  | None -> assert false
                  | Some vi ->
                     match Mymap.find_opt (t,vi) index with
                     | None -> assert false
                     | Some es ->
                        let ei = Expr.Apply (t, `Index_1 [Some i], [|Expr.Ref (t,21)|]) in
                        match Myseq.find_map
                                (fun e -> if e = ei then Some e else None)
                                (Expr.Exprset.to_seq es) with
                        | Some _ -> ()
                        | None -> assert false
                        (*if Expr.Exprset.mem ei es
                        then ()
                        else assert false*));
           print_newline ()
      in*)                          
      let bgcolors full =
        Grid.black :: if full then [] else [Grid.transparent] in
      let index = Expr.Index.empty in
      let index = Expr.index_add_bindings index bindings in
      let index = (* LEVEL 0 - ndtree indexes *)
        Expr.index_apply_functions
          ~eval_func
          index 1
          (fun (t_args, v_args_tree) ->
            match t_args, v_args_tree with
            | [|t1|], [|v1|] ->
               let ndim = Ndtree.ndim v1 in
               let res = [] in
               let res =
                 if ndim >= 1
                 then
                   let$ res, i = res, [0; 1; 2; -2; -1] in
                   (t1, `Index_1 [Some i], `Default)::res
                 else res in
               let res =
                 if ndim >= 2
                 then
                   let$ res, j = res, [0; 1; 2; -2; -1] in
                   (t1, `Index_1 [None; Some j], `Default)::res
                 else res in
               res
            | _ -> []) in
      (*pp (xp_expr_index ~on_typ:(function VEC POS -> true | _ -> false)) index;*)
      (*test "TEST LEVEL 0" index;*)
      let index = (* LEVEL 1 *)
        Expr.index_apply_functions
          ~eval_func
          index 2 (* TEST *)
          (fun (t_args, v_args_tree) ->
            let res = [] in
            let res = (* Size_1, Height, Width, Area_1 *)
              match t_args with
              | [|GRID (filling,nocolor)|] ->
                 (VEC SIZE, `Size_1, `Default)
                 ::(INT (COORD (I, SIZE)), `Height_1, `Default)
                 ::(INT (COORD (J, SIZE)), `Width_1, `Default)
                 ::(INT CARD, `Area_1, `Default)
                 ::(INT (COORD (I, SIZE)), `Area_1, `Default)
                 ::(INT (COORD (J, SIZE)), `Area_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Right, Center, Bottom, Middle *)
              match t_args with
              | [|OBJ tg|] ->
                 (INT (COORD (J,POS)), `Right_1, `Default)
                 ::(INT (COORD (J,POS)), `Center_1, `Default)
                 ::(INT (COORD (I,POS)), `Bottom_1, `Default)
                 ::(INT (COORD (I,POS)), `Middle_1, `Default)
                 ::res
              | _ -> res in
            let res = (* ProjI/J_1 *)
              match t_args with
              | [|VEC tv|] -> (VEC tv, `ProjI_1, `Default)::(VEC tv, `ProjJ_1, `Default)::res
              | _ -> res in
            let res = (* MajorityColor_1 *)
              match t_args with
              | [|GRID (filling,false)|] ->
                 let full = (filling = `Full) in
                 let$ res, tc = res, [C_BG full; C_OBJ] in
                 (COLOR tc, `MajorityColor_1, `Default)::res
              | _ -> res in
            let res = (* ColorCount_1 *)
              match t_args with
              | [|GRID (filling,false)|] -> (INT CARD, `ColorCount_1, `Default)::res
              | _ -> res in
            (*let res = (* Strip_1: covered by pattern Crop *)
              match t_args with
              | [|GRID (filling,nocolor)|] -> (GRID (false,nocolor), `Strip_1)::res
              | _ -> res in *)
            (* TODO: PeriodicFactor_2, as pattern *)
            let res = (* Corner_2 *)
              match t_args with
              | [|VEC POS; VEC POS|] -> (VEC POS, `Corner_2, `Default)::res
              | _ -> res in
            let res = (* Span_2 *)
              match t_args with
              | [|INT (COORD (axis1,POS)); INT (COORD (axis2,POS))|] when axis1=axis2 ->
                 (INT (COORD (axis1,POS)), `Span_2, `Default)::res
              | [|VEC POS; VEC POS|] -> (VEC POS, `Span_2, `Default)::res
              | _ -> res in
            (*let res = (* Min, Max, Average *) TODO: as vectorized op
              match t_args with
              | [|INT (COORD (axis1,tv1)) as t1; t2|] when t2=t1 ->
                 (t1, `Min_n)::(t1, `Max_n)::(t1, `Average_n)::res
              | [|VEC tv1 as t1; t2|] when t2=t1 ->
                 (t1, `Min_n)::(t1, `Max_n)::(t1, `Average_n)::res
              | _ -> res in *)
            let res = (* translation = pos - pos *)
              match t_args with
              | [|INT (COORD (axis1,POS)); INT (COORD (axis2,POS))|] when axis1=axis2 ->
                 (INT (COORD (axis1,MOVE)), `Minus_2, `Default)::res
              | [|VEC POS; VEC POS|] -> (VEC POS, `Minus_2, `Default)::res
              | _ -> res in
            let res = (* TranslationOnto *)
              match t_args with
              | [|OBJ _; OBJ _|] -> (VEC MOVE, `TranslationOnto_2, `Default)::res
              | _ -> res in
            let res = (* TranslationSym *)
              match t_args with
              | [|OBJ _; (OBJ _ | GRID _)|] ->
                 let$ res, sym =
                   res,
                   [`FlipHeight; `FlipWidth; `FlipDiag1; `FlipDiag2;
                    `Rotate180; `Rotate90; `Rotate270] in
                 (VEC MOVE, `TranslationSym_2 sym, `Default)::res
              | _ -> res in
            (*let res = (* Crop *)
              match t_args with
              | [|GRID tg1; OBJ _|] -> (GRID tg1, `Crop_2)::res
              | _ -> res in*)
            res) in
      (*pp (xp_expr_index ~on_typ:(function VEC POS -> true | _ -> false)) index;*)
      (*test "TEST LEVEL 1" index;*)
      let index = (* LEVEL 2 *)
        Expr.index_apply_functions
          ~eval_func
          index 2 (* TEST *)
          (fun (t_args,v_args_tree) ->
            let res = [] in
            let res = (* ScaleUp, ScaleDown *)
              match t_args with
              | [|(INT _ | VEC _ | GRID _ as t1)|] ->
                 let$ res, k = res, [2;3] in
                 let args_spec = `Custom [|`Pos 0; `Val (INT CARD, `Int k)|] in
                 (t1, `ScaleUp_2, args_spec)::(t1, `ScaleDown_2, args_spec)::res
              | _ -> res in
            (* MOVE as POS ? *)
            let res = (* ApplySymGrid *)
              match t_args with
              | [|GRID _ as t1|] ->
                 let$ res, sym = res, all_symmetry in
                 (t1, `ApplySymGrid_1 sym, `Default)::res
              | _ -> res in
            let res = (* Coloring *)
              match t_args with
              | [|GRID _ as t1; COLOR _|] -> (t1, `Coloring_2, `Default)::res
              | _ -> res in
            let res = (* Plus *)
              match t_args with
              | [|INT (COORD (_,tv1)) as t1|] when tv1 <> MOVE ->
                 let$ res, i2 = res, [1;2;3] in
                 let args_spec = `Custom [|`Pos 0; `Val (t1, `Int i2)|] in
                 (t1, `Plus_2, args_spec)::res
              | [|INT (COORD (_,tv1)) as t1; INT (COORD (_,(SIZE|MOVE)))|] when tv1 <> MOVE ->
                 (t1, `Plus_2, `Default)::res
              | [|VEC tv1 as t1|] when tv1 <> MOVE ->
                 let$ res, i2 = res, [0;1;2;3] in
                 let$ res, j2 = res, (if i2=0 then [1;2;3] else [0;1;2;3]) in
                 let args_spec = `Custom [|`Pos 0; `Val (t1, `Vec (i2,j2))|] in
                 (t1, `Plus_2, args_spec)::res
              | [|VEC tv1 as t1; VEC (SIZE|MOVE)|] when tv1 <> MOVE ->
                 (t1, `Plus_2, `Default)::res
              | _ -> res in
            let res = (* Minus *)
              match t_args with
              | [|INT (COORD (_,tv1)) as t1|] when tv1 <> MOVE ->
                 let$ res, i2 = res, [1;2;3] in
                 let args_spec = `Custom [|`Pos 0; `Val (t1, `Int i2)|] in
                 (t1, `Minus_2, args_spec)::res
              | [|INT (COORD (_,tv1)) as t1; INT (COORD (_, (SIZE|MOVE)))|] when tv1 <> MOVE ->
                 (t1, `Minus_2, `Default)::res
              | [|VEC tv1 as t1|] when tv1 <> MOVE ->
                 let$ res, i2 = res, [0;1;2;3] in
                 let$ res, j2 = res, (if i2=0 then [1;2;3] else [0;1;2;3]) in
                 let args_spec = `Custom [|`Pos 0; `Val (t1, `Vec (i2,j2))|] in
                 (t1, `Minus_2, args_spec)::res
              | [|VEC tv1 as t1; VEC (SIZE|MOVE)|] when tv1 <> MOVE ->
                 (t1, `Minus_2, `Default)::res
              | _ -> res in
          res) in
      (*test "TEST LEVEL 2" index;*)
      let index = (* LEVEL 3 *)
        Expr.index_apply_functions
          ~eval_func
          index 1 (* TEST *)
          (fun (t_args,v_args_tree) ->
            let res = [] in
            (*let res = (* Tiling *)
              match t_args with
              | [|(VEC SIZE | GRID _ as t1)|] ->
                 let$ res, k = res, [1;2;3] in
                 let$ res, l = res, [1;2;3] in
                 if k>1 || l>1
                 then (t1, `Tiling_1 (k,l), `Default)::res
                 else res
              | _ -> res in*)
            (*let res = (* FillResizeAlike *)
              match t_args with
              | [|VEC SIZE; GRID ((`Full|`Sprite as filling),_) as t3|] ->
                 let full = (filling = `Full) in
                 let$ res, bgcolor = res, bgcolors full in
                 let args_spec = `Custom [|`Val (COLOR (C_BG full), `Color bgcolor); `Pos 0; `Pos 1|] in
                 let$ res, mode =
                   res, (if full
                         then [`TradeOff; `Total; `Strict]
                         else [`TradeOff; `Strict]) in
                 (t3, `FillResizeAlike_3 mode, args_spec)::res
              | _ -> res in*)
            let res = (* SelfCompose *)
              match t_args with
              | [|GRID _ as t2|] ->
                 let$ res, color = res, Grid.all_colors in
                 let args_spec = `Custom [|`Val (COLOR C_OBJ, `Color color); `Pos 0|] in
                 (t2, `SelfCompose_2, args_spec)::res
              | _ -> res in
            (*let res = (* UnfoldSym *)
              match t_args with
              | [|GRID _ as t1|] ->
                 let$ res, sym_matrix = res, all_symmetry_unfold in
                 (t1, `UnfoldSym_1 sym_matrix, `Default)::res
              | _ -> res in*)
            let res = (* CloseSym *)
              match t_args with
              | [|GRID (filling,_) as t2|] ->
                 let full = (filling = `Full) in
                 let$ res, bgcolor = res, bgcolors full in
                 let args_spec = `Custom [|`Val (COLOR (C_BG full), `Color bgcolor); `Pos 0|] in
                 let$ res, sym_seq = res, all_symmetry_close in
                 (t2, `CloseSym_2 sym_seq, args_spec)::res
              | _ -> res in
            (*let res = (* SwapColors *)
              match t_args with
              | [|GRID (_,_) as t1; COLOR C_OBJ; COLOR C_OBJ|] -> (t1, `SwapColors_3)::res
              | _ -> res in*)
            let res = (* LogNot *)
              match t_args with
              | [|GRID (`Sprite,true) as t1|] -> (t1, `LogNot_1, `Default)::res
              | _ -> res in
            let res = (* And, Or, XOr, AndNOt *)
              match t_args with
              | [|GRID (`Sprite,true) as t1; t2|] when t2=t1 ->
                 let$ res, f = res, [`LogAnd_2; `LogOr_2; `LogXOr_2; `LogAndNot_2] in
                 (t1,f, `Default)::res
              | _ -> res in
            (*let res = (* ScaleTo *)
              match t_args with
              | [|GRID _ as t1; VEC SIZE|] -> (t1, `ScaleTo_2, `Default)::res
              | _ -> res in*)
            (* Stack *)
            res) in
      (* pp (xp_expr_index ~on_typ:(function VEC POS -> true | _ -> false)) index; *)
      index
    let make_index, reset_make_index =
      Memo.memoize ~size:103 make_index

    (* refining *)

    let refinements_pat ~env_vars (t : typ) (c : constr) (args : model array) (varseq : varseq) (data : data) : (model * varseq) list =
      match t, c with
      | INT (COORD (axis,tv)), AnyCoord -> []
      | COLOR tc, AnyColor -> []
      | MOTIF, AnyMotif -> []
      | MAP (ta,tb), AnyMap ->
         let refs : (model * varseq) list = [] in
         let refs = (* DomMap *)
           match tb, Data.value data with
           | COLOR tc, `Map m -> (* TODO: generalize to other types *)
              let keys = mymap_keys m in
              let xvals, varseq = Refining.new_var varseq in
              let mvals, varseq = (* explicit sequence of same length as keys *)
                Array.fold_right
                  (fun _ (mvals, varseq) ->
                    let xcol, varseq = Refining.new_var varseq in
                    let mcol = Model.make_def xcol (make_anycolor tc) in
                    let mvals = Model.make_cons mcol mvals in (* TODO: wrong if encompassing loops, extend Cons/Nil with loop var *)
                    mvals, varseq)
                  keys (Model.make_nil tb, varseq) in
              (make_dommap ta tb keys
                 (Model.make_loop (Range.make_closed 1 Grid.nb_color)
                    (Model.make_def xvals mvals)),
               varseq)
              :: refs
           | _ -> refs in
         let refs = (* Replace *)
           if ta = tb then
             let xa, varseq = Refining.new_var varseq in
             let xb, varseq = Refining.new_var varseq in
             (make_replace ta tb
                (Model.make_def xa (make_anycolor C_OBJ))
                (Model.make_def xb (make_anycolor C_OBJ)),
              varseq)
             :: refs
           else refs in
         let refs = (* Swap *)
           if ta = tb then
             let xa, varseq = Refining.new_var varseq in
             let xb, varseq = Refining.new_var varseq in
             (make_swap ta tb
                (Model.make_def xa (make_anycolor C_OBJ))
                (Model.make_def xb (make_anycolor C_OBJ)),
              varseq)
             :: refs
           else refs in
         refs
      | GRID (filling,nocolor as tg), AnyGrid ->
         let refs : (model * varseq) list = [] in
         let refs = (* BgColor *)
           if filling = `Full then
             let xcol, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             (make_bgcolor
                (Model.make_def xcol (make_anycolor (C_BG true)))
                (Model.make_def xg1 (make_anygrid (`Sprite,nocolor))),
              varseq)
             :: refs
           else refs in
         let refs = (* IsFull *)
           if filling = `Sprite then
             let xgrid1, varseq = Refining.new_var varseq in
             (make_isfull
                (Model.make_def xgrid1 (make_anygrid (`Full,nocolor))),
              varseq)
             :: refs
           else refs in
         let refs = (* Crop *)
           let xpos, varseq = Refining.new_var varseq in
           let xpos_i, varseq = Refining.new_var varseq in
           let xpos_j, varseq = Refining.new_var varseq in
           let xsize, varseq = Refining.new_var varseq in
           let xsize_i, varseq = Refining.new_var varseq in
           let xsize_j, varseq = Refining.new_var varseq in
           let cropable_vars =
             Mymap.fold
               (fun x t res ->
                 match t with
                 | GRID (`Full,_) -> x::res
                 | _ -> res)
               env_vars [] in
           let$ refs, gvar = refs, cropable_vars in
           (make_crop tg
              (Model.make_expr (GRID tg) (Expr.Ref (GRID tg, gvar)))
              (Model.make_def xpos
                 (make_vec POS
                    (Model.make_def xpos_i (make_anycoord I POS))
                    (Model.make_def xpos_j (make_anycoord J POS))))
                (Model.make_def xsize
                   (make_vec SIZE
                      (Model.make_def xsize_i (make_anycoord I SIZE))
                      (Model.make_def xsize_j (make_anycoord J SIZE)))),
            varseq)
           :: refs in
         let refs = (* Objects *)
           if filling <> `Full then
             let xsize, varseq = Refining.new_var varseq in
             let xsize_i, varseq = Refining.new_var varseq in
             let xsize_j, varseq = Refining.new_var varseq in
             let xobj, varseq = Refining.new_var varseq in
             let xpos, varseq = Refining.new_var varseq in
             let xpos_i, varseq = Refining.new_var varseq in
             let xpos_j, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             let$ refs, seg = refs, [`Default; `SameColor] in
             let$ refs, nmax = refs, [1;9] in
             let m_g1, varseq =
               match seg with
               | `Default ->
                  Model.make_def xg1 (make_anygrid (`Sprite,nocolor)),
                  varseq
               | `SameColor ->
                  let xcol, varseq = Refining.new_var varseq in
                  let xm1, varseq = Refining.new_var varseq in
                  Model.make_def xg1
                    (make_monocolor
                       (Model.make_def xcol (make_anycolor C_OBJ))
                       (Model.make_def xm1 (make_anygrid (`Sprite,true)))),
                  varseq in
             (make_objects seg nmax
                (Model.make_def xsize
                   (make_vec SIZE
                      (Model.make_def xsize_i (make_anycoord I SIZE))
                      (Model.make_def xsize_j (make_anycoord J SIZE))))
                (Model.make_loop (Range.make_closed 1 nmax)
                   (Model.make_def xobj
                      (make_obj (`Sprite,nocolor)
                         (Model.make_def xpos
                            (make_vec POS
                               (Model.make_def xpos_i (make_anycoord I POS))
                               (Model.make_def xpos_j (make_anycoord J POS))))
                         m_g1))),
              varseq)
             :: refs
           else refs in
         let refs = (* Monocolor *)
           if not nocolor then
             let xcol, varseq = Refining.new_var varseq in
             let xmask, varseq = Refining.new_var varseq in
             (make_monocolor
                (Model.make_def xcol (make_anycolor C_OBJ))
                (Model.make_def xmask (make_anygrid (filling,true))),
              varseq)
             :: refs
           else refs in
         (*let refs = (* Recoloring *)
           if not nocolor then
             let xcol, varseq = Refining.new_var varseq in
             let xgrid, varseq = Refining.new_var varseq in
             (make_recoloring
                (Model.make_loop (Range.make_closed 1 Grid.nb_color)
                   (Model.make_def xcol (make_anycolor C_OBJ)))
                (Model.make_def xgrid (make_anygrid (filling,false))),
              varseq)
             :: refs
           else refs in*)
         let refs = (* Recoloring *)
           if not nocolor then
             let xmap, varseq = Refining.new_var varseq in
             let xg1s =
               Mymap.fold
                 (fun x t res ->
                   match t with
                   | GRID (_,false) -> x::res
                   | _ -> res)
                 env_vars [] in
             let eg1s =
               match Grid_patterns.recoloring (get_grid data) with
               | Result.Ok (g1,_) -> [Expr.Const (t, `Grid g1)]
               | _ -> [] in
             let eg1s =
               List.fold_left
                 (fun res xg1 ->
                   let rg1 = Expr.Ref (t, xg1) in
                   rg1
                   (* :: Expr.Apply (t, `Index_1 [Some 0], [|rg1|])
                   :: Expr.Apply (t, `Index_1 [Some (-1)], [|rg1|]) *) (* need to know var dim *)
                   :: res)
                 eg1s xg1s in
             let$ refs, eg1 = refs, eg1s in
             (make_recoloring tg
                (Model.make_expr (GRID tg) eg1)
                (Model.make_def xmap (make_anymap (COLOR C_OBJ) (COLOR C_OBJ))),
              varseq)
             :: refs
           else refs in
         let refs = (* Motif *)
           let xmot, varseq = Refining.new_var varseq in
           let xcore, varseq = Refining.new_var varseq in
           let xnoise, varseq = Refining.new_var varseq in
           (make_motif (filling,nocolor)
             (Model.make_def xmot (make_anymotif))
             (Model.make_def xcore (make_anygrid (filling,nocolor)))
             (Model.make_def xnoise (make_anygrid (`Noise,nocolor))),
            varseq)
           :: refs in
         let refs = (* Masks *)
           let msize, varseq_msize =
             let xsize, varseq = Refining.new_var varseq in
             let xsize_i, varseq = Refining.new_var varseq in
             let xsize_j, varseq = Refining.new_var varseq in
             Model.make_def xsize
               (make_vec SIZE
                  (Model.make_def xsize_i (make_anycoord I SIZE))
                  (Model.make_def xsize_j (make_anycoord J SIZE))),
             varseq in
           (make_empty msize, varseq_msize)
           :: (if nocolor then
                 (make_full msize, varseq_msize)
                 :: (make_point, varseq)
                 :: refs
               else refs) in
         refs
      | _ -> []
    let refinements_postprocessing t c args =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    let prunings_value t v varseq =
      match t, v with
      | INT (COORD (axis,tv)), `Int i ->
         [ make_anycoord axis tv, varseq ]
      | VEC tv, `Vec (i,j) ->
         let x, varseq = Refining.new_var varseq in
         let y, varseq = Refining.new_var varseq in
         [ make_vec tv
             (Model.make_def x (make_anycoord I tv))
             (Model.make_def y (make_anycoord J tv)), varseq ]
      | COLOR tc, `Color c ->
         [ make_anycolor tc, varseq ]
      | MOTIF, `Motif m ->
         [ make_anymotif, varseq ]
      | GRID tg, `Grid g ->
         [ make_anygrid tg, varseq ]
      | OBJ tg, `Obj (i,j,g1) ->
         let xpos, varseq = Refining.new_var varseq in
         let xi, varseq = Refining.new_var varseq in
         let xj, varseq = Refining.new_var varseq in
         let xg1, varseq = Refining.new_var varseq in
         [ make_obj tg
             (Model.make_def xpos
                (make_vec POS
                   (Model.make_def xi (make_anycoord I POS))
                   (Model.make_def xj (make_anycoord J POS))))
             (Model.make_def xg1 (make_anygrid tg)),
           varseq ]
      | MAP (ta,tb), `Map m ->
         [ make_anymap ta tb, varseq ]
      | _ -> assert false
    let prunings_pat ~env_vars t c args varseq data =
      match t, c with
      | GRID _, AnyGrid -> []
      | GRID tg, _ -> [ make_anygrid tg, varseq ]
      | MAP _, AnyMap -> []
      | MAP (ta,tb), _ -> [ make_anymap ta tb, varseq ]
      | _ -> []
    let prunings_postprocessing t c args =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    (* initialization *)
      
    let varseq0 : varseq = Myseq.range 1 max_int

    let get_init_config name task =
      let open Task_model in
      let env = make_danycolor Grid.black (C_BG true) in (* dummy *)
      let varseq = varseq0 in
      let xi, varseq = Refining.new_var varseq in
      let xo, varseq = Refining.new_var varseq in
      let input_model = Model.make_def xi (make_anygrid (`Full,false)) in
      let output_model = Model.make_def xo (make_anygrid (`Full,false)) in
      let output_generator_info = () (* Grid.max_size, Grid.max_size, -1 *) in
      { env;
        varseq;
        input_model;
        output_model;
        output_generator_info }

    let log_reading r m ~status =
      (*print_endline "READING";
      pp_endline xp_refinement r;
      pp_endline xp_task_model m;*)
      ()
    let log_refining r m prs dl =
      Printf.printf "REF  %.3f  " dl;
      pp_endline xp_refinement r;
      (*pp_endline xp_task_model m;*)
      ()

    let default_name_task =
      let open Task in
      let make_i h w i j h1 w1 c1 =
        let g = Grid.make h w Grid.black in
        let g1 = Grid.make h1 w1 c1 in
        Grid.add_grid_at g i j g1;
        g in
      let make_o h1 w1 c2 =
        Grid.make h1 w1 c2 in
      let make_io h w i j h1 w1 c1 c2 =
        make_i h w i j h1 w1 c1,
        make_o h1 w1 c2
      in
      let i1, o1 = make_io 8 8 2 4 3 3 Grid.blue Grid.red in
      let i2, o2 = make_io 9 11 4 3 4 3 Grid.blue Grid.red in
      let i3, o3 = make_io 13 7 6 0 2 5 Grid.blue Grid.red in
      "default",
      { train = [ {input = `Grid i1;
                   output = `Grid o1};
                  {input = `Grid i2;
                   output = `Grid o2}];
        test = [ {input = `Grid i3;
                  output = `Grid o3} ] }

    let reset_memoization () =
      Grid.reset_memoized_functions ();
      Grid_patterns.reset_memoized_functions ();
      Segment.reset_memoized_functions ();
      Funct.reset_memoized_functions_apply ();
      reset_default_grid ();
      reset_make_index ()
  end

module MyMadil = Madil.Make(MyDomain)
