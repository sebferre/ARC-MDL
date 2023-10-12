
open Madil_common
open Arc_common

module Basic_types (* : Madil.BASIC_TYPES *) =
  struct

    (* generic printers *)

    let xp_bool ~html print b =
      print#string (if b then "true" else "false")
                              
    let xp_int ~html print i = print#int i
    
    let xp_vec xp_i xp_j ~html print i j =
      xp_tuple2 xp_i xp_j ~html print (i,j) 

    let xp_color ~html print c =
      xp_html_elt "span" ~classe:("arc-col" ^ string_of_int c) ~html print
        (fun () ->
          print#string (Grid.name_of_color c))

    let xp_grid ~html (print : Xprint.t) g =
      if html
      then
        let h, w = Grid.dims g in
        xp_html_elt "table" ~classe:"arc-grid" ~html print
          (fun () ->
            for i = 0 to h - 1 do
              xp_html_elt "tr" ~html print
                (fun () ->
                  for j = 0 to w - 1 do
                    let classe = "arc-cell arc-col" ^ string_of_int g.matrix.{i,j} in
                    xp_html_elt "td" ~classe ~html print
                      (fun () -> ())
                  done)
            done)
      else
        Grid.xp_grid print g 
      
    (* values *)

    type value =
      [ `Null
      | `Bool of bool
      | `Int of int
      | `Vec of int * int
      | `Color of Grid.color
      | `Grid of Grid.t ]
    (* TODO: missing object span: pos+size *)

    let rec xp_value ~html (print : Xprint.t) : value -> unit = function
      | `Null -> print#string "null"
      | `Bool b -> xp_bool ~html print b
      | `Int i -> xp_int ~html print i
      | `Vec (i,j) -> xp_vec xp_int xp_int ~html print i j
      | `Color c -> xp_color ~html print c
      | `Grid g -> xp_grid ~html print g

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

    (* model types *)

    type typ =
      | BOOL
      | INT of typ_int
      | VEC of typ_vec
      | COLOR
      | GRID of typ_grid
    and typ_int =
      | CARD
      | COORD of t_axis * typ_vec
    and t_axis =
      | I
      | J
    and typ_vec =
      | POS
      | SIZE
      | MOVE
    and typ_grid =
      bool (* full = no transparent cell *)
      * bool (* no-color, i.e. black and transparent *)
    (* MASK = GRID (false,true), (false,false) is the more general case *)

    let typ_bool = BOOL
      
    let nb_axis = 2
    let nb_typ_vec = 3
      
    let rec xp_typ ~html print = function
      | BOOL -> print#string "BOOL"
      | INT ti -> xp_typ_int ~html print ti
      | VEC tv -> xp_typ_vec ~html print tv
      | COLOR -> print#string "COLOR"
      | GRID tg -> xp_typ_grid ~html print tg
    and xp_typ_int ~html print = function
      | CARD -> print#string "CARD"
      | COORD (ij,tv) ->
         xp_typ_vec ~html print tv;
         print#string (match ij with I -> ".I" | J -> ".J")
    and xp_typ_vec ~html print = function
      | POS -> print#string "POS"
      | SIZE -> print#string "SIZE"
      | MOVE -> print#string "MOVE"
    and xp_typ_grid ~html print (full,nocolor) =
      print#string (if full then "GRID"
                    else if nocolor then "MASK"
                    else "SPRITE")

    (* model vars *)
      
    type var = int
             
    let xp_var ~html print x =
      xp_html_elt "span" ~classe:"model-var" ~html print
        (fun () -> print#string "$"; print#int x)

    (* model constr *)
      
    type constr =
      | AnyCoord (* COORD *)
      | Coord of int (* COORD *)
      | Vec (* COORD, COORD : VEC *)
      | AnyColor (* COLOR *)
      | Color of Grid.color (* COLOR *)
      | AnyGrid (* GRID *)
      | Grid of Grid.t (* GRID *)
      | BgColor (* COLOR, SPRITE : GRID *)
      | IsFull (* SPRITE : GRID *)
      | Crop (* SIZE, POS, SPRITE : SPRITE *)
      | Monocolor (* COLOR, MASK : SPRITE *)
      | Empty (* SIZE : MASK *)
      | Full (* SIZE : MASK *)
      | Border (* SIZE : MASK *)
      | Point (* MASK *)

    let xp_bgcolor xp_color xp_sprite ~html print () =
      print#string "a grid with background color "; xp_color ~html print ();
      print#string " and with contents"; xp_newline ~html print ();
      xp_sprite ~html print ()
    let xp_isfull xp_sprite ~html print () =
      print#string "a full grid that is";
      xp_newline ~html print ();
      xp_sprite ~html print ()
    let xp_crop xp_size xp_pos xp_sprite ~html print () =
      print#string "a grid of size "; xp_size ~html print ();
      print#string " that contains at position "; xp_pos ~html print ();
      xp_newline ~html print ();
      xp_sprite ~html print ()
    let xp_monocolor xp_color xp_mask ~html print () =
      print#string "a grid with only color "; xp_color ~html print ();
      print#string " and with mask"; xp_newline ~html print ();
      xp_mask ~html print ()
    let xp_empty xp_size ~html print () =
      print#string "an empty mask of size "; xp_size ~html print ()
    let xp_full xp_size ~html print () =
      print#string "a full mask of size "; xp_size ~html print ()
    let xp_border xp_size ~html print () =
      print#string "a border mask of size "; xp_size ~html print ()
    let xp_point ~html print () =
      print#string "a point mask"
      
    let xp_pat c xp_args ~html print () =
      match c, xp_args with
      | AnyCoord, [||] -> print#string "?"
      | Coord ij, [||] -> print#int ij
      | Vec, [|xp_i; xp_j|] -> xp_vec xp_i xp_j ~html print () ()
      | AnyColor, [||] -> print#string "?color"
      | Color c, [||] -> xp_color ~html print c
      | AnyGrid, [||] -> print#string "?grid"
      | Grid g, [||] -> xp_grid ~html print g
      | BgColor, [|xp_color; xp_sprite|] ->
         xp_bgcolor xp_color xp_sprite ~html print ()
      | IsFull, [|xp_sprite|] ->
         xp_isfull xp_sprite ~html print ()
      | Crop, [|xp_size; xp_pos; xp_sprite|] ->
         xp_crop xp_size xp_pos xp_sprite ~html print ()
      | Monocolor, [|xp_color; xp_mask|] ->
         xp_monocolor xp_color xp_mask ~html print ()
      | Empty, [|xp_size|] ->
         xp_empty xp_size ~html print ()
      | Full, [|xp_size|] ->
         xp_full xp_size ~html print ()
      | Border, [|xp_size|] ->
         xp_border xp_size ~html print ()
      | Point, [||] ->
         xp_point ~html print ()
      | _ -> assert false

    let xp_field ~html print = function
      | AnyCoord, _ -> assert false
      | Coord _, _ -> assert false
      | Vec, 0 -> print#string "i"
      | Vec, 1 -> print#string "j"
      | Vec, _ -> assert false
      | AnyColor, _ -> assert false
      | Color _, _ -> assert false
      | AnyGrid, _ -> assert false
      | Grid _, _ -> assert false
      | BgColor, 0 -> print#string "color"
      | BgColor, 1 -> print#string "sprite"
      | BgColor, _ -> assert false
      | IsFull, _ -> print#string "sprite"
      | Crop, 0 -> print#string "size"
      | Crop, 1 -> print#string "pos"
      | Crop, 2 -> print#string "sprite"
      | Crop, _ -> assert false
      | Monocolor, 0 -> print#string "color"
      | Monocolor, 1 -> print#string "mask"
      | Monocolor, _ -> assert false
      | Empty, _ -> print#string "size"
      | Full, _ -> print#string "size"
      | Border, _ -> print#string "size"
      | Point, _ -> assert false

    (* data constr *)
                  
    type dconstr = (* make sure data from constant models can be identified as so *)
      | DAnyCoord of int * Range.t (* COORD in some range *)
      | DCoord of int (* COORD const *)
      | DVec (* COORD, COORD : VEC *)
      | DAnyColor of Grid.color (* COLOR *)
      | DColor of Grid.color (* COLOR *)
      | DAnyGrid of Grid.t * typ_grid * Range.t (* height *) * Range.t (* width *) (* GRID of some type and with some size ranges *)
      | DGrid of Grid.t (* GRID const *)
      | DBgColor (* COLOR, SPRITE : GRID *)
      | DIsFull (* SPRITE : GRID *)
      | DCrop (* SIZE, POS, SPRITE : SPRITE *)
      | DMonocolor (* COLOR, MASK : SPRITE *)
      | DEmpty (* SIZE : MASK *)
      | DFull (* SIZE : MASK *)
      | DBorder (* SIZE : MASK *)
      | DPoint (* MASK *)

    let xp_dpat dc xp_args ~html print () =
      match dc, xp_args with
      | DAnyCoord (ij,_), [||] -> print#int ij
      | DCoord ij, [||] -> print#int ij
      | DVec, [|xp_i; xp_j|] -> xp_vec xp_i xp_j ~html print () ()
      | DAnyColor c, [||] -> xp_color ~html print c
      | DColor c, [||] -> xp_color ~html print c
      | DAnyGrid (g,_,_,_), [||] -> xp_grid ~html print g
      | DGrid g, [||] -> xp_grid ~html print g
      | DBgColor, [|xp_color; xp_sprite|] ->
         xp_bgcolor xp_color xp_sprite ~html print ()
      | DIsFull, [|xp_sprite|] ->
         xp_isfull xp_sprite ~html print ()
      | DCrop, [|xp_size; xp_pos; xp_sprite|] ->
         xp_crop xp_size xp_pos xp_sprite ~html print ()
      | DMonocolor, [|xp_color; xp_mask|] ->
         xp_monocolor xp_color xp_mask ~html print ()
      | DEmpty, [|xp_size|] ->
         xp_empty xp_size ~html print ()
      | DFull, [|xp_size|] ->
         xp_full xp_size ~html print ()
      | DBorder, [|xp_size|] ->
         xp_border xp_size ~html print ()
      | DPoint, [||] ->
         xp_point ~html print ()
      | _ -> assert false

    (* functions *)
        
    type func =
      [ `ConstInt_0 of int (* Int *)
      | `ConstVec_0 of int * int (* Vec *)
      | `ConstColor_0 of Grid.color (* Color *)
      | `Plus_2 (* on Int, Vec *)
      | `Minus_2 (* on Int, Vec *)
      | `IncrInt_1 of int (* on Int *)
      | `DecrInt_1 of int (* in Int *)
      | `IncrVec_1 of int * int (* on Vec *)
      | `DecrVec_1 of int * int (* in Vec *)
      | `Modulo_2 (* on Int *)
      | `ScaleUp_2 (* on (Int, Vec, Mask, Shape, Grid as T), Card -> T *)
      | `ScaleDown_2 (* on (Int, Vec, Mask, Shape, Grid as T), Card -> T *)
      | `ScaleTo_2 (* Mask, Grid, Vec -> Mask *)
      | `Size_1 (* Grid -> Vec *)
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
      | `TranslationSym_2 of symmetry * bool (* viz Grid *) (* Obj, Obj/Grid -> Vec *)
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
      | `ConstInt_0 k -> print#string "'"; xp_int ~html print k
      | `ConstVec_0 (k,l) -> print#string "'"; xp_vec xp_int xp_int ~html print k l
      | `ConstColor_0 c -> print#string "'"; xp_color ~html print c
      | `Plus_2 -> print#string "+"
      | `Minus_2 -> print#string "-"
      | `IncrInt_1 k -> print#string "+"; xp_int ~html print k
      | `DecrInt_1 k -> print#string "-"; xp_int ~html print k
      | `IncrVec_1 (k,l) -> print#string "+"; xp_vec xp_int xp_int ~html print k l
      | `DecrVec_1 (k,l) -> print#string "-"; xp_vec xp_int xp_int ~html print k l
      | `Modulo_2 -> print#string "%"
      | `ScaleUp_2 -> print#string "*"
      | `ScaleDown_2 -> print#string "/"
      | `ScaleTo_2 -> print#string "scaleTo"
      | `Size_1 -> print#string "size"
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
      | `TranslationSym_2 (sym, on_grid) ->
         print#string "translationSym";
         xp_tuple2 ~delims:("[","]") xp_symmetry xp_on_grid ~html print (sym,on_grid)
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
    and xp_on_grid ~html print on_grid =
      print#string (if on_grid then "on-grid" else "on-layer")
    and suffix_periodicity_mode = function
      | `Total -> "_total"
      | `Strict -> "_strict"
      | `TradeOff -> ""

    (* ASD *)
              
    let asd (* : asd *) =
      object
        inherit [typ,constr,func] Model.asd
        method is_default_constr = function
          | AnyCoord | AnyColor | AnyGrid -> true
          | _ -> false
        method default_and_other_pats = function
          | BOOL -> None, [ ]
          | INT CARD -> None, [ ]
          | INT (COORD _) ->
             Some AnyCoord,
             [ Coord 0, [||] ]
          | VEC tv ->
             None,
             [ Vec, [|INT (COORD (I, tv));
                      INT (COORD (J, tv))|] ]
          | COLOR ->
             Some AnyColor,
             [ Color Grid.black, [||] ]
          | GRID (full,nocolor) ->
             Some AnyGrid,
             List.filter_map
               (fun (cond,c_args) ->
                 if cond
                 then Some c_args
                 else None)
               [ true, (Grid Grid.dummy, [||]);
                 full, (BgColor, [|COLOR; GRID (false,nocolor)|]);
                 not full, (IsFull, [|GRID (true,nocolor)|]);
                 not full, (Crop, [|VEC SIZE; VEC POS; GRID (false,nocolor)|]);
                 not nocolor, (Monocolor, [|COLOR; GRID (full,true)|]);
                 not full && nocolor, (Empty, [|VEC SIZE|]);
                 not full && nocolor, (Full, [|VEC SIZE|]);
                 not full && nocolor, (Border, [|VEC SIZE|]);
                 not full && nocolor, (Point, [||]) ]
        method funcs k =
          match k with
          | BOOL -> []
          | INT CARD ->
             [ `ConstInt_0 0, [||];
               `Plus_2, [|k; k|];
               `Minus_2, [|k; k|];
               `IncrInt_1 1, [|k|];
               `DecrInt_1 1, [|k|];
               `Area_1, [|GRID (false,false)|];
               `ColorCount_1, [|GRID (false,false)|];
               `Min_n, [|k; k|];
               `Max_n, [|k; k|];
               `Average_n, [|k; k|];
             ]
          | INT (COORD (axis,tv)) ->
             [ `ConstInt_0 0, [||];
               `Plus_2, [|k; k|];
               `Minus_2, [|k; k|];
               `IncrInt_1 1, [|k|];
               `DecrInt_1 1, [|k|];
               `ScaleUp_2, [|k; INT CARD|];
               `ScaleDown_2, [|k; INT CARD|];
               `Span_2, [|k; k|]; (* only on same axis POS *)
               `Min_n, [|k; k|];
               `Max_n, [|k; k|];
               `Average_n, [|k; k|];               
             ]
          | VEC tv ->
             [ `ConstVec_0 (0,0), [||];
               `Plus_2, [|k; k|];
               `Minus_2, [|k; k|];
               `IncrVec_1 (1,1), [|k|];
               `DecrVec_1 (1,1), [|k|];
               `ScaleUp_2, [|k; INT CARD|];
               `ScaleDown_2, [|k; INT CARD|];
               `ProjI_1, [|k|];
               `ProjJ_1, [|k|];
               `Corner_2, [|k; k|]; (* only on POS *)
               `Span_2, [|k; k|]; (* only on POS *)
               `Min_n, [|k; k|];
               `Max_n, [|k; k|];
               `Average_n, [|k; k|];
               (* `TranslationOnto_2, [|OBJ; OBJ|]; *)
               (* `TranslationSym_2, [|OBJ; GRID|]; *)
               `ApplySymVec_1 (`Id,tv), [|k|];
               `Tiling_1 (2,2), [|k|];
             ]
          | COLOR ->
             [ `ConstColor_0 Grid.black, [||];
               `MajorityColor_1, [|GRID (false,false)|];
             ]
          | GRID (full,nocolor) ->
             [ `ScaleUp_2, [|k; INT CARD|];
               `ScaleDown_2, [|k; INT CARD|];
               `ScaleTo_2, [|k; VEC SIZE|];
               `Strip_1, [|GRID (false,false)|];
               `PeriodicFactor_2 `TradeOff, [|COLOR; k|];
               (* `Crop_2, [|GRID; OBJ|]; *)
               `ApplySymGrid_1 `Id, [|k|];
               `Coloring_2, [|k; COLOR|];
               `Tiling_1 (2,2), [|k|];
               `FillResizeAlike_3 `TradeOff, [|COLOR; VEC SIZE; k|];
               `SelfCompose_2, [|COLOR; k|];
               `UnfoldSym_1 [], [|k|];
               `CloseSym_2 [], [|COLOR; k|];
               `SwapColors_3, [|k; COLOR; COLOR|];
               `Stack_n, [|k; k|];
               (* on masks *)
               `LogNot_1, [|k|];
               `LogAnd_2, [|k; k|];
               `LogOr_2, [|k; k|];
               `LogAndNot_2, [|k; k|];
               `LogXOr_2, [|k; k|];
             ]
        method expr_opt = function
          | k -> Some k
        method alt_opt = function
          | _ -> false (* LATER *)
      end

    (* model processing *)
      
    type generator_info = int * int (* container height and width *)
                        
    type input =
      [ `Null
      | `IntRange of int * Range.t
      | `Vec of input * input
      | `Color of Grid.color
      | `GridDims of Grid.t * int (* height ctx *) * int (* width ctx *) ]

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
    
    (* constructors and accessors *)
                        
    let make_anycoord axis tv : model = Model.make_pat (INT (COORD (axis,tv))) AnyCoord [||]
    let make_coord axis tv i : model = Model.make_pat (INT (COORD (axis,tv))) (Coord i) [||]
    let make_vec tv mi mj : model = Model.make_pat (VEC tv) Vec [|mi;mj|]
    let make_anycolor : model = Model.make_pat COLOR AnyColor [||]
    let make_color c : model = Model.make_pat COLOR (Color c) [||]
    let make_anygrid tg : model = Model.make_pat (GRID tg) AnyGrid [||]
    let make_grid tg g : model = Model.make_pat (GRID tg) (Grid g) [||]
    let make_bgcolor mcol mg1 : model = Model.make_pat (GRID (true,false)) BgColor [|mcol; mg1|]
    let make_isfull mg1 : model = Model.make_pat (GRID (false,false)) IsFull [|mg1|]
    let make_crop msize mpos mg1 : model = Model.make_pat (GRID (false,false)) Crop [|msize; mpos; mg1|]
    let make_monocolor mcol mmask : model = Model.make_pat (GRID (false,false)) Monocolor [|mcol; mmask|]
    let make_empty msize : model = Model.make_pat (GRID (false,true)) Empty [|msize|]
    let make_full msize : model = Model.make_pat (GRID (false,true)) Full [|msize|]
    let make_border msize : model = Model.make_pat (GRID (false,true)) Border [|msize|] (* could be said full grid? *)
    let make_point : model = Model.make_pat (GRID (false,true)) Point [||]
      
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
    let get_grid (d : data) : Grid.t =
      match Data.value d with
      | `Grid g -> g
      | _ -> assert false
      
    let make_danycoord ij r : data =
      Data.make_dpat (`Int ij) (DAnyCoord (ij,r)) [||]
    let make_dcoord ij : data =
      Data.make_dpat (`Int ij) (DCoord ij) [||]
    let make_dvec di dj : data =
      let i, j =
        match Data.value di, Data.value dj with
        | `Int i, `Int j -> i, j
        | _ -> assert false in
      Data.make_dpat (`Vec (i,j)) DVec [|di;dj|]
    let make_danycolor c : data =
      Data.make_dpat (`Color c) (DAnyColor c) [||]
    let make_dcolor c : data =
      Data.make_dpat (`Color c) (DColor c) [||]
    let make_danygrid g tg rh rw : data =
      Data.make_dpat (`Grid g) (DAnyGrid (g,tg,rh,rw)) [||]
    let make_dgrid g : data =
      Data.make_dpat (`Grid g) (DGrid g) [||]
    let make_dbgcolor dcol dspr : data =
      let g =
        match Data.value dcol, Data.value dspr with
        | `Color bc, `Grid g1 -> Grid.fill_transparent g1 bc
        | _ -> assert false in
      Data.make_dpat (`Grid g) DBgColor [|dcol;dspr|]
    let make_disfull dspr : data =
      Data.make_dpat (Data.value dspr) DIsFull [|dspr|]
    let make_dcrop dsize dpos dstr : data =
      let g =
        match Data.value dsize, Data.value dpos, Data.value dstr with
        | `Vec (h,w), `Vec (i,j), `Grid g1 ->
           let g = Grid.make h w Grid.transparent in
           Grid.add_grid_at g i j g1;
           g
        | _ -> assert false in
      Data.make_dpat (`Grid g) DCrop [|dsize; dpos; dstr|]
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
    let make_dborder dsize : data =
      let g =
        match Data.value dsize with
        | `Vec (h,w) -> Grid.Mask.init h w (fun i j -> i = 0 || j = 0 || i = h-1 || j = w-1)
        | _ -> assert false in
      Data.make_dpat (`Grid g) DBorder [|dsize|]
    let make_dpoint : data =
      let g = Grid.Mask.full 1 1 in
      Data.make_dpat (`Grid g) DPoint [||]
      
    (* evaluation *)
                        
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
          (* | `Layer, `PosShape (pos, shape) ->
             let| shape' = sym_grid shape in
             Result.Ok (`PosShape (pos, shape')) (* NOTE: do not use sym_pos because pos in PosShape must be the top-left corner of the shape, see def of TranslationSym *) *)
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
          (* | `PosShape (pos, shape) ->
             let| shape = unfold_symmetry sym_matrix e shape in
             Result.Ok (`PosShape (pos, shape)) *)
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
          (* | `PosShape (pos, shape) ->
             let| shape = close_symmetry sym_seq bgcolor e shape in
             Result.Ok (`PosShape (pos, shape)) *)
          | _ -> Result.Error (Invalid_expr e)

        let reset_memoized_functions_apply () =
          reset_unfold_grid ();
          reset_close_grid ()
  
      end

    let eval_func (f : func) (args : value array) : value result =
      let e = "" in
      match f, args with
      | `ConstInt_0 k, [||] -> Result.Ok (`Int k)
      | `ConstVec_0 (k,l), [||] -> Result.Ok (`Vec (k,l))
      | `ConstColor_0 c, [||] -> Result.Ok (`Color c)
      | `Plus_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Int i1, `Int i2 -> Result.Ok (`Int (i1 + i2))
          | `Vec (i1,j1), `Vec (i2,j2) -> Result.Ok (`Vec (i1+i2, j1+j2))
          | _ -> Result.Error (Invalid_expr e))
      | `Minus_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Int i1, `Int i2 -> Result.Ok (`Int (i1-i2))
          | `Vec (i1, j1), `Vec (i2, j2) -> Result.Ok (`Vec (i1-i2, j1-j2))
          | _ -> Result.Error (Invalid_expr e))
      | `IncrInt_1 k, [|d1|] ->
         (match d1 with
          | `Int i1 -> Result.Ok (`Int (i1 + k))
          | _ -> Result.Error (Invalid_expr e))
      | `DecrInt_1 k, [|d1|] ->
         (match d1 with
          | `Int i1 -> Result.Ok (`Int (i1 - k))
          | _ -> Result.Error (Invalid_expr e))
      | `IncrVec_1 (k,l), [|d1|] ->
         (match d1 with
          | `Vec (i1, j1) -> Result.Ok (`Vec (i1 + k, j1 + l))
          | _ -> Result.Error (Invalid_expr e))
      | `DecrVec_1 (k,l), [|d1|] ->
         (match d1 with
          | `Int i1 -> Result.Ok (`Int (i1 - k))
          | `Vec (i1, j1) -> Result.Ok (`Vec (i1 - k, j1 - l))
          | _ -> Result.Error (Invalid_expr e))
      | `Modulo_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Int i1, `Int i2 -> Result.Ok (`Int (i1 mod i2))
          | _ -> Result.Error (Invalid_expr e))
      | `ScaleUp_2, [|d1;d2|] ->
         ( match d2 with
           | `Int 0 -> Result.Error (Invalid_argument "ScaleUp: k=0") 
           | `Int k ->
              assert (k > 0);
              ( match d1 with
                | `Int i -> Result.Ok (`Int (i * k))
                | `Vec (i,j) -> Result.Ok (`Vec (i * k, j * k))
                | `Grid g ->
                   let| g' = Grid.Transf.scale_up k k g in
                   Result.Ok (`Grid g')
                | _ -> Result.Error (Invalid_expr e))
           | _ -> Result.Error (Invalid_expr e))
      | `ScaleDown_2, [|d1;d2|] ->
         (match d2 with
          | `Int 0 -> Result.Error (Invalid_argument "ScaleDown: k=0") 
          | `Int k ->
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
      | `ScaleTo_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Grid g, (`Vec (new_h, new_w)) ->
             let| g' = Grid.Transf.scale_to new_h new_w g in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))
      | `Size_1, [|d1|] ->
         (match d1 with
          | `Grid g ->
             let h, w = Grid.dims g in
             Result.Ok (`Vec (h, w))
          | _ -> Result.Error (Invalid_expr e))
(*      | `Crop_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Grid g, `PosShape (`Vec (ri, rj), `Grid shape) ->
             let| c = Grid.majority_color Grid.transparent shape in
             if Mask_model.matches (Grid.Mask.from_grid_color c shape) `Border (* TODO: allow crop on Full rectangles as well ? *)
             then
               let rh, rw = Grid.dims shape in
               let i, j, h, w = ri+1, rj+1, rh-2, rw-2 in (* inside border *)
               let| g' = Grid.Transf.crop g i j h w in
               Result.Ok (`Grid g')
             else Result.Error (Invalid_expr e)
          | _ -> Result.Error (Invalid_expr e)) *)
      | `Strip_1, [|d1|] ->
         (match d1 with
          | `Grid g ->
             let| bgcolor = Grid.majority_color Grid.transparent g in
             let| _, _, g'= Grid.Transf.strip bgcolor g Grid.black in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))
      | `Corner_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Vec (i1, j1), `Vec (i2, j2) ->
             if i1 <> i2 && j1 <> j2
             then Result.Ok (`Vec (i1, j2))
             else Result.Error (Undefined_result "Corner: vectors on same row/column")
          | _ -> Result.Error (Invalid_expr e))
      | `Min_n, ds ->
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
          | _ -> assert false)
      | `Max_n, ds ->
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
          | _ -> assert false)
      | `Average_n, ds ->
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
          | _ -> assert false) (* empty or ill-typed list *)
      | `Span_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Int i1, `Int i2 ->
             if i1=i2
             then Result.Error (Undefined_result "Span: same int")
             else Result.Ok (`Int (abs (i2-i1) + 1))
          | `Vec (i1, j1), `Vec (i2, j2) ->
             if i1=i2 && j1=j2
             then Result.Error (Undefined_result "Span: same vector")
             else Result.Ok (`Vec (abs (i2-i1) + 1, abs (j2-j1) + 1))
          | _ -> Result.Error (Invalid_expr e))
      | `Norm_1, [|d1|] ->
         (match d1 with
          | `Vec (i, j) -> Result.Ok (`Int (i+j))
          | _ -> Result.Error (Invalid_expr e))
      | `Diag1_1 k, [|d1|] ->
         (match d1 with
          | `Vec (i, j) -> Result.Ok (`Int ((i+j) mod k))
          | _ -> Result.Error (Invalid_expr e))
      | `Diag2_1 k, [|d1|] ->
         (match d1 with
          | `Vec (i, j) -> Result.Ok (`Int ((i-j) mod k))
          | _ -> Result.Error (Invalid_expr e))
      | `LogAnd_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Grid m1, `Grid m2 when Grid.dims m1 = Grid.dims m2 -> (* TODO: generalize Mask logics to grids transfs *)
             let m = Grid.Mask.inter m1 m2 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `LogOr_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Grid m1, `Grid m2 when Grid.dims m1 = Grid.dims m2 ->
             let m = Grid.Mask.union m1 m2 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `LogXOr_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Grid m1, `Grid m2 when Grid.dims m1 = Grid.dims m2 ->
             let m = Grid.Mask.diff_sym m1 m2 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `LogAndNot_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Grid m1, `Grid m2 when Grid.dims m1 = Grid.dims m2 ->
             let m = Grid.Mask.diff m1 m2 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `LogNot_1, [|d1|] ->
         (match d1 with
          | `Grid m1 ->
             let m = Grid.Mask.compl m1 in
             Result.Ok (`Grid m)
          | _ -> Result.Error (Invalid_expr e))
      | `Stack_n, ds ->
         let lg1 = Array.map (function `Grid g1 -> g1 | _ -> assert false) ds in
         let| g = Grid.Transf.layers Grid.transparent (Array.to_list lg1) in
         Result.Ok (`Grid g)
      | `Area_1, [|d1|] ->
         (match d1 with
          | `Grid g ->
             Result.Ok (`Int (Grid.color_area Grid.transparent g))
          | _ -> Result.Error (Invalid_expr e))
        (* needs OBJ
      | `Left_1, [|d1|] ->
         (match d1 with
          | `PosShape (`Vec (_, `Int j), _) -> Result.Ok (`Int j)
          | _ -> Result.Error (Invalid_expr e))
      | `Right_1, [|d1|] ->
         (match d1 with
          | `PosShape (`Vec (_, `Int j), `Grid (shape,_)) ->
             let h, w = Grid.dims shape in
             Result.Ok (`Int (j+w-1))
          | _ -> Result.Error (Invalid_expr e))
      | `Center_1, [|d1|] ->
         (match d1 with
          | `PosShape (`Vec (_, `Int j), `Grid (shape,_)) ->
             let h, w = Grid.dims shape in
             if w mod 2 = 0
             then Result.Error (Undefined_result "Center: no center, even width")
             else Result.Ok (`Int (j + w/2 + 1))
          | `PosShape _ -> Result.Error (Undefined_result "Center: not a rectangle")
          | _ -> Result.Error (Invalid_expr e))
      | `Top_1, [|d1|] ->
         (match d1 with
          | `PosShape (`Vec (i, _), _) -> Result.Ok (`Int i)
          | _ -> Result.Error (Invalid_expr e))
      | `Bottom_1, [|d1|] ->
         (match d1 with
          | `PosShape (`Vec (i, _), `Grid (shape,_)) ->
             let h, w = Grid.dims shape in
             Result.Ok (`Int (i+h-1))
          | `PosShape _ -> Result.Error (Undefined_result "Bottom: not a rectangle")
          | _ -> Result.Error (Invalid_expr e))
      | `Middle_1, [|d1|] ->
         (match d1 with
          | `PosShape (`Vec (i, _), `Grid (shape,_)) ->
             let h, w = Grid.dims shape in
             if h mod 2 = 0
             then Result.Error (Undefined_result "Middle: no middle, even height")
             else Result.Ok (`Int (i + h/2 + 1))
          | _ -> Result.Error (Invalid_expr e))
         *)
      | `ProjI_1, [|d1|] ->
         (match d1 with
          | `Vec (i, _) -> Result.Ok (`Vec (i, 0))
          | _ -> Result.Error (Invalid_expr e))
      | `ProjJ_1, [|d1|] ->
         (match d1 with
          | `Vec (_, j) -> Result.Ok (`Vec (0, j))
          | _ -> Result.Error (Invalid_expr e))
      | `MaskOfGrid_1, [|d1|] ->
         (match d1 with
          | `Grid g -> Result.Ok (`Grid (Grid.Mask.from_grid_background Grid.black g)) (* TODO: improve *)
          | _ -> Result.Error (Invalid_expr e))
      | `GridOfMask_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Grid m, `Color c ->
             Result.Ok (`Grid (Grid.Mask.to_grid m Grid.black c)) (* TODO: improve *)
          | _ -> Result.Error (Invalid_expr e))
    (* needs OBJ
      | `TranslationOnto_2, [|d1;d2|] ->
         (match get_pos d1, get_size d1, get_pos d2, get_size d2 with
          | Some (mini1,minj1), Some (h1,w1), Some (mini2,minj2), Some (h2,w2) ->
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
     *)
      | `Tiling_1 (k,l), [|d1|] ->
         (match d1 with
          | `Vec (h, w) -> Result.Ok (`Vec (h*k, w*l))
          | `Grid g ->
             let| g' = Grid.Transf.tile k l g in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))
      | `PeriodicFactor_2 mode, [|d1;d2|] ->
         (match d1 with
          | `Color bgcolor ->
             (match d2 with
              | `Grid g ->
                 let| g' = Grid.Transf.periodic_factor mode bgcolor g in
                 Result.Ok (`Grid g')
              (* | `PosShape (pos, shape) ->
                 let| shape' = aux shape in
                 Result.Ok (`PosShape (pos, shape')) *)
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `FillResizeAlike_3 mode, [|d1;d2;d3|] ->
         (match d1, d2 with
          | `Color bgcolor, `Vec (h, w) when h > 0 && w > 0 ->
             let new_size = h, w in
             (match d3 with
              | `Grid g ->
                 let| g' = Grid.Transf.fill_and_resize_alike mode bgcolor new_size g in
                 Result.Ok (`Grid g')
               (* | `PosShape (pos, shape) ->
                  let| shape' = aux shape in
                  Result.Ok (`PosShape (pos, shape')) *)
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `SelfCompose_2, [|d1;d2|] ->
         (match d1, d2 with
          | `Color c_mask,  `Grid g1 ->
             let| g = Grid.Transf.compose c_mask g1 g1 in
             Result.Ok (`Grid g)
          | _ -> Result.Error (Invalid_expr e))
      | `ApplySymVec_1 (sym,tv), [|d1|] ->
         Funct.apply_symmetry_vec sym tv e d1
      | `ApplySymGrid_1 sym, [|d1|] ->
         Funct.apply_symmetry_grid sym e d1
      | `UnfoldSym_1 sym_matrix, [|d1|] ->
         Funct.unfold_symmetry sym_matrix e d1
      | `CloseSym_2 sym_matrix, [|d1;d2|] ->
         (match d1 with
          | `Color bgcolor -> Funct.close_symmetry sym_matrix bgcolor e d2
          | _ -> Result.Error (Invalid_expr e))
      (* needs OBJ
      | `TranslationSym_2 (sym,on_grid), [|d1;d2|] ->
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
         *)
      | `MajorityColor_1, [|d1|] ->
         (match d1 with
          | `Grid g ->
             let| c = Grid.majority_color Grid.black g in
             Result.Ok (`Color c)
          | _ -> Result.Error (Invalid_expr e))
      | `ColorCount_1, [|d1|] ->
         (match d1 with
          | `Grid g ->
             let n = Grid.color_count Grid.black g in
             Result.Ok (`Int n)
          | _ -> Result.Error (Invalid_expr e))
      | `Coloring_2, [|d1;d2|] ->
         (match d2 with
          | `Color c ->
             (match d1 with
              | `Grid g ->
                 let m = Grid.Mask.from_grid_background Grid.transparent g in (* collapsing all colors *)
                 let g' = Grid.Mask.to_grid m Grid.transparent c in (* mask to shape with color c *)
                 Result.Ok (`Grid g')
               (* | `PosShape (pos, shape) ->
                  let| shape = aux shape in
                  Result.Ok (`PosShape (pos, shape)) *)
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `SwapColors_3, [|d1;d2;d3|] ->
         (match d1, d2, d3 with
          | `Grid g, `Color c1, `Color c2 ->
             let| g' = Grid.Transf.swap_colors g c1 c2 in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))
      | _ -> assert false (* invalid argument number for some function *)

    let eval_unbound_var x = Result.Ok `Null
    let eval_arg () = Result.Error (Failure "eval: unexpected Arg")

    let rec data_of_value t v =
      let| dc, dargs =
        match t, v with
        | INT (COORD (axis,tv)), `Int i ->
           Result.Ok (DCoord i, [||])
        | VEC tv, `Vec (i,j) ->
           let| di = data_of_value (INT (COORD (I,tv))) (`Int i) in
           let| dj = data_of_value (INT (COORD (J,tv))) (`Int j) in
           Result.Ok (DVec, [|di; dj|])
        | COLOR, `Color c ->
           Result.Ok (DColor c, [||])
        | GRID tg, `Grid g ->
           Result.Ok (DGrid g, [||])
        | _ -> Result.Error (Failure "data_of_value: unexpected value") in
      Result.Ok (Data.D (v, DPat (dc, dargs)))
    let rec model_of_value t v =
      let| c, args =
        match t, v with
        | INT (COORD (axis,tv)), `Int i ->
           Result.Ok (Coord i, [||])
        | VEC tv, `Vec (i,j) ->
           let| mi = model_of_value (INT (COORD (I,tv))) (`Int i) in
           let| mj = model_of_value (INT (COORD (J,tv))) (`Int j) in
           Result.Ok (Vec, [|mi; mj|])
        | COLOR, `Color c ->
           Result.Ok (Color c, [||])
        | GRID tg, `Grid g ->
           Result.Ok (Grid g, [||])
        | _ -> Result.Error (Failure "data_of_value: unexpected value") in
      Result.Ok (Model.Pat (t, c, args))
    let bool_of_value : value -> bool result = function
      | `Bool b -> Result.Ok b
      | _ -> Result.Error (Failure "model evaluation: expected Boolean value")
    let dseq_value (ld : data list) : value =
      assert false (* LATER: `List (List.map Data.value ld) *)

    (* model-based generation *)
      
    let generator_pat t c gen_args =
      match t, c, gen_args with
      | INT (COORD (axis,tv)), AnyCoord, [||] ->
         (fun (h,w) ->
           let bound = match axis with I -> h | J -> w in
           let ij, range =
             match tv with
             | SIZE -> min 2 bound, Range.make_closed 1 bound
             | POS | MOVE -> 0, Range.make_closed 0 (bound-1) in
           Myseq.return (make_danycoord ij range))
      | _, Coord ij, [||] ->
         (fun (h,w) -> Myseq.return (make_dcoord ij))
      | _, Vec, [|gen_i; gen_j|] ->
         (fun (h,w) ->
           let* di = gen_i (h,w) in
           let* dj = gen_j (h,w) in
           Myseq.return (make_dvec di dj))
      | _, AnyColor, [||] ->
         (fun (h,w) -> Myseq.return (make_danycolor Grid.blue))
      | _, Color c, [||] ->
         (fun (h,w) -> Myseq.return (make_dcolor c))
      | GRID ((full,nocolor) as tg), AnyGrid, [||] ->
         (fun (h,w) ->
           let c = if nocolor then Grid.Mask.one else Grid.blue in
           let g = Grid.make 2 2 c in
           Myseq.return (make_danygrid g tg (Range.make_closed 1 h) (Range.make_closed 1 w)))
      | GRID tg, Grid g, [||] ->
         (fun (h,w) ->
           Myseq.return (make_dgrid g))
      | _, BgColor, [|gen_col; gen_g1|] ->
         (fun (h,w) ->
           let* dcol = gen_col (h,w) in
           let* dg1 = gen_g1 (h,w) in
           Myseq.return (make_dbgcolor dcol dg1))
      | _, IsFull, [|gen_g1|] ->
         (fun (h,w) ->
           let* dg1 = gen_g1 (h,w) in
           Myseq.return (make_disfull dg1))
      | _, Crop, [|gen_size; gen_pos; gen_g1|] ->
         (fun (h0,w0) ->
           let* dsize = gen_size (h0,w0) in
           let h, w = get_vec dsize in
           let* dpos = gen_pos (h,w) in
           let* dg1 = gen_g1 (h,w) in
           Myseq.return (make_dcrop dsize dpos dg1))
      | _, Monocolor, [|gen_col; gen_mask|] ->
         (fun (h,w) ->
           let* dcol = gen_col (h,w) in
           let* dmask = gen_mask (h,w) in
           Myseq.return (make_dmonocolor dcol dmask))
      | _, Empty, [|gen_size|] ->
         (fun (h,w) ->
           let* dsize = gen_size (h,w) in
           Myseq.return (make_dempty dsize))
      | _, Full, [|gen_size|] ->
         (fun (h,w) ->
           let* dsize = gen_size (h,w) in
           Myseq.return (make_dfull dsize))
      | _, Border, [|gen_size|] ->
         (fun (h,w) ->
           let* dsize = gen_size (h,w) in
           Myseq.return (make_dborder dsize))
      | _, Point, [||] ->
         (fun (h,w) ->
           Myseq.return (make_dpoint))
      | _ -> assert false

    (* model-based parsing *)
           
    let input_of_value : value -> input = function
      | `Grid g -> `GridDims (g, Grid.max_size, Grid.max_size)
      | _ -> assert false

    let parseur_pat t c parse_args =
      match t, c, parse_args with
      | _, AnyCoord, [||] ->
         (function
          | `IntRange (ij,range) -> Myseq.return (make_danycoord ij range, `Null)
          | _ -> assert false)
      | _, Coord ij0, [||] ->
         (function
          | `IntRange (ij,range) ->
             if ij = ij0
             then Myseq.return (make_dcoord ij, `Null)
             else Myseq.empty
          | _ -> assert false)
      | _, Vec, [|parse_i; parse_j|] ->
         (function
          | `Vec (in_i, in_j) ->
             let* di, _ = parse_i in_i in
             let* dj, _ = parse_j in_j in
             Myseq.return (make_dvec di dj, `Null)
          | _ -> assert false)
      | _, AnyColor, [||] ->
         (function
          | `Color c -> Myseq.return (make_danycolor c, `Null)
          | _ -> assert false)
      | _, Color c0, [||] ->
         (function
          | `Color c ->
             if c = c0
             then Myseq.return (make_dcolor c, `Null)
             else Myseq.empty
          | _ -> assert false)
      | GRID tg, AnyGrid, [||] ->
         (function
          | `GridDims (g,h0,w0) ->
             Myseq.return (make_danygrid g tg (Range.make_closed 1 h0) (Range.make_closed 1 w0), `Null)
          | _ -> assert false)
      | GRID tg, Grid g0, [||] ->
         (function
          | `GridDims (g,h0,w0) ->
             if Grid.same g g0
             then Myseq.return (make_dgrid g, `Null)
             else Myseq.empty
          | _ -> assert false)
      | _, BgColor, [|parse_col; parse_g1|] ->
         (function
          | `GridDims (g,h0,w0) ->
             let* bc = Myseq.from_list (Segment.background_colors g) in
             let* dcol, _ = parse_col (`Color bc) in
             let* g1 = Myseq.from_result (Grid.Transf.swap_colors g bc Grid.transparent) in
             let* dg1, _ = parse_g1 (`GridDims (g1,h0,w0)) in
             Myseq.return (make_dbgcolor dcol dg1, `Null)
          | _ -> assert false)
      | _, IsFull, [|parse_g1|] ->
         (fun input ->
           let* dg1, _ = parse_g1 input in
           let g1 = get_grid dg1 in
           if Grid.is_full g1
           then Myseq.return (make_disfull dg1, `Null)
           else Myseq.empty)
      | _, Crop, [|parse_size; parse_pos; parse_g1|] ->
         (function
          | `GridDims (g,h0,w0) ->
             let h, w = Grid.dims g in
             let* dsize, _ = parse_size (`Vec (`IntRange (h, Range.make_closed 1 h0),
                                               `IntRange (w, Range.make_closed 1 w0))) in
             let* i, j, g1 =
               Myseq.from_result
                 (Grid.Transf.strip Grid.transparent g Grid.transparent) in
             let* dpos, _ = parse_pos (`Vec (`IntRange (i, Range.make_closed 0 (h-1)),
                                             `IntRange (j, Range.make_closed 0 (w-1)))) in
             let* dg1, _ = parse_g1 (`GridDims (g1, h-i, w-j)) in
             Myseq.return (make_dcrop dsize dpos dg1, `Null)
          | _ -> assert false)
      | _, Monocolor, [|parse_col; parse_mask|] ->
         (function
          | `GridDims (g,h0,w0) ->
             if Grid.color_count Grid.transparent g = 1
             then
               let* c = Myseq.from_result (Grid.majority_color Grid.transparent g) in
               let* dcol, _ = parse_col (`Color c) in
               let* mask = Myseq.from_result (Grid.Transf.swap_colors g c Grid.Mask.one) in
               let* dmask, _ = parse_mask (`GridDims (mask,h0,w0)) in
               Myseq.return (make_dmonocolor dcol dmask, `Null)
             else Myseq.empty
          | _ -> assert false)
      | _, (Empty | Full | Border as c), [|parse_size|] ->
         let pred_maked h w = function
           | Empty -> (fun i j c -> c <> Grid.Mask.one), make_dempty
           | Full -> (fun i j c -> c = Grid.Mask.one), make_dfull
           | Border -> (fun i j c -> (c = Grid.Mask.one) = (i=0 || j=0 || i=h-1 || j=w-1)), make_dborder
           | _ -> assert false
         in
         (function
          | `GridDims (mask,h0,w0) ->
             let h, w = Grid.dims mask in
             let pred, maked = pred_maked h w c in
             if Grid.for_all_pixels pred mask
             then
               let* dsize, _ = parse_size (`Vec (`IntRange (h, Range.make_closed 1 h0),
                                                 `IntRange (w, Range.make_closed 1 w0))) in
               Myseq.return (maked dsize, `Null)
             else Myseq.empty
          | _ -> assert false)
      | _, Point, [||] ->
         (function
          | `GridDims (mask,h0,w0) ->
             let h, w = Grid.dims mask in
             if h=1 && w=1 && Grid.Mask.mem 0 0 mask
             then Myseq.return (make_dpoint, `Null)
             else Myseq.empty
          | _ -> assert false)
      | _ -> assert false

    (* description length *)

    let dl_grid g (full,nocolor) rh rw : dl = (* too efficient a coding for being useful? *)
      let h, w = Grid.dims g in
      let area = h * w in
      let in_mask = area - g.color_count.(Grid.transparent) in
      let dl_color = Mdl.Code.uniform Grid.nb_color in
      Range.dl h rh +. Range.dl w rw
      +. (if full then float area *. dl_color
          else if nocolor then float area (* bitmap *)
          else float area +. float in_mask *. dl_color)
           
    let encoding_dpat dc encs =
      match dc, encs with
      | DAnyCoord (ij,range), [||] -> Range.dl ij range
      | DCoord ij, [||] -> 0.
      | DVec, [|enc_i; enc_j|] ->  enc_i +. enc_j
      | DAnyColor c, [||] -> Mdl.Code.uniform Grid.nb_color
      | DColor c, [||] -> 0.
      | DAnyGrid (g,tg,rh,rw), [||] -> dl_grid g tg rh rw
      | DGrid g, [||] -> 0.
      | DBgColor, [|enc_col; enc_g1|] -> enc_col +. enc_g1
      | DIsFull, [|enc_g1|] -> enc_g1
      | DCrop, [|enc_size; enc_pos; enc_g1|] -> enc_size +. enc_pos +. enc_g1
      | DMonocolor, [|enc_col; enc_mask|] -> enc_col +. enc_mask
      | DEmpty, [|enc_size|] -> enc_size
      | DFull, [|enc_size|] -> enc_size
      | DBorder, [|enc_size|] -> enc_size
      | DPoint, [||] -> 0.
      | _ -> assert false
    let encoding_alt dl_choice enc = dl_choice +. enc
    let encoding_seq encs = assert false (* LATER *)
    let dl_of_encoding enc = enc
           
    let dl_constr_params t c =
      match t, c with
      | _, AnyCoord -> 0.
      | INT (COORD (axis,tv)), Coord ij ->
         Range.dl ij (Range.make_closed 0 Grid.max_size)
      | _, Vec -> 0.
      | _, AnyColor -> 0.
      | _, Color c -> Mdl.Code.uniform Grid.nb_color
      | _, AnyGrid -> 0.
      | GRID tg, Grid g ->
         let rmax = Range.make_closed 1 Grid.max_size in
         dl_grid g tg rmax rmax
      | _, BgColor -> 0.
      | _, IsFull -> 0.
      | _, Crop -> 0.
      | _, Monocolor -> 0.
      | _, Empty -> 0.
      | _, Full -> 0.
      | _, Border -> 0.
      | _, Point -> 0.
      | _ -> assert false


    let dl_periodicity_mode : Grid.Transf.periodicity_mode -> dl = function
      | `Total -> Mdl.Code.usage 0.25
      | `Strict -> Mdl.Code.usage 0.25
      | `TradeOff -> Mdl.Code.usage 0.5
           
    let dl_func_params : func -> dl = function
      | `ConstInt_0 i -> Mdl.Code.universal_int_star i (* TODO: depends on typ_vec *)
      | `ConstVec_0 (i,j) -> Mdl.Code.universal_int_star i +. Mdl.Code.universal_int_star j (* TODO: depdends on typ_vec *)
      | `ConstColor_0 c -> Mdl.Code.uniform Grid.nb_color
      | `Plus_2 -> 0.
      | `Minus_2 -> 0.
      | `IncrInt_1 k -> Mdl.Code.universal_int_star k
      | `DecrInt_1 k -> Mdl.Code.universal_int_star k
      | `IncrVec_1 (k,l) -> Mdl.Code.universal_int_star k +. Mdl.Code.universal_int_star l
      | `DecrVec_1 (k,l) -> Mdl.Code.universal_int_star k +. Mdl.Code.universal_int_star l
      | `Modulo_2 -> 0.
      | `ScaleUp_2 -> 0.
      | `ScaleDown_2 -> 0.
      | `ScaleTo_2 -> 0.
      | `Size_1 -> 0.
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
      | `TranslationSym_2 (sym,b) -> Mdl.Code.uniform nb_symmetry +. 1.
      | `MajorityColor_1 -> 0.
      | `ColorCount_1 -> 0.
      | `Coloring_2 -> 0.
      | `SwapColors_3 -> 0.

    let dl_var ~nb_env_vars p =
      let k = max 1 nb_env_vars in (* to avoid 0, happens in pruning mode *)
      Mdl.Code.uniform k

    (* expression index *)

    let make_index (bindings : bindings) : expr_index =
      let index = Expr.Index.empty in
      let index = Expr.index_add_bindings index bindings in
      let index = (* LEVEL 1 *)
        Expr.index_apply_functions
          ~eval_func
          index 2
          (fun (t_args, v_args) ->
            let res = (* consts *)
              match t_args with
              | [| |] ->
                 let res = (* const colors *)
                   let$ res, c = [], Grid.all_colors in
                   (COLOR, `ConstColor_0 c)::res in
                 let res = (* const ints and vecs *)
                   let$ res, k = res, [0;1;2;3] in
                   let f = `ConstInt_0 k in
                   let res =
                     let$ res, ti =
                       res,
                       (if k = 0
                        then [CARD;
                              COORD (I,POS); COORD (J,POS)]
                        else [CARD;
                              COORD (I,POS); COORD (J,POS);
                              COORD (I,SIZE); COORD (J,SIZE);
                              COORD (I,MOVE); COORD (J,MOVE)]) in
                     (INT ti, f)::res in
                   let$ res, l = res, [0;1;2;3] in
                   let f = `ConstVec_0 (k,l) in
                   let$ res, tv =
                     res, (if k=0 && l=0
                           then [POS]
                           else [POS; SIZE; MOVE]) in
                   (VEC tv, f)::res in
                 res
              | _ -> [] in
            let res = (* Size_1, Area_1 *)
              match t_args with
              | [|GRID (full,nocolor)|] -> (VEC SIZE, `Size_1)::(INT CARD, `Area_1)::res
              | _ -> res in
            let res = (* ProjI/J_1 *)
              match t_args with
              | [|VEC tv|] -> (VEC tv, `ProjI_1)::(VEC tv, `ProjJ_1)::res
              | _ -> res in
            let res = (* MajorityColor_1 *)
              match t_args with
              | [|GRID (full,false)|] -> (COLOR, `MajorityColor_1)::res
              | _ -> res in
            let res = (* ColorCount_1 *)
              match t_args with
              | [|GRID (full,false)|] -> (INT CARD, `ColorCount_1)::res
              | _ -> res in
            let res = (* Strip_1 *)
              match t_args with
              | [|GRID (full,nocolor)|] -> (GRID (false,nocolor), `Strip_1)::res
              | _ -> res in
            (* TODO: PeriodicFactor_2, well as pattern *)
            let res = (* Corner_2 *)
              match t_args with
              | [|VEC POS; VEC POS|] -> (VEC POS, `Corner_2)::res
              | _ -> res in
            let res = (* Span_2 *)
              match t_args with
              | [|INT (COORD (axis1,POS)); INT (COORD (axis2,POS))|] when axis1=axis2 ->
                 (INT (COORD (axis1,POS)), `Span_2)::res
              | [|VEC POS; VEC POS|] -> (VEC POS, `Span_2)::res
              | _ -> res in
            let res = (* Min, Max, Average *)
              match t_args with
              | [|INT (COORD (axis1,tv1)) as t1; t2|] when t2=t1 ->
                 (t1, `Min_n)::(t1, `Max_n)::(t1, `Average_n)::res
              | [|VEC tv1 as t1; t2|] when t2=t1 ->
                 (t1, `Min_n)::(t1, `Max_n)::(t1, `Average_n)::res
              | _ -> res in
            let res = (* translation = pos - pos *)
              match t_args with
              | [|INT (COORD (axis1,POS)); INT (COORD (axis2,POS))|] when axis1=axis2 ->
                 (INT (COORD (axis1,MOVE)), `Minus_2)::res
              | [|VEC POS; VEC POS|] -> (VEC POS, `Minus_2)::res
              | _ -> res in
            (* TranslationOnto, TranslationSym, Crop on objects *)
            res) in
      let index = (* LEVEL 2 *)
        Expr.index_apply_functions
          ~eval_func
          index 2
          (fun (t_args,v_args) ->
            let res = [] in
            let res = (* IncrInt, DecrInt *)
              match t_args with
              | [|INT (COORD (axis,tv)) as t|] when tv <> MOVE ->
                 let$ res, k = res, [1;2;3] in
                 (t, `IncrInt_1 k) :: (t, `DecrInt_1 k) :: res
              | _ -> res in
            let res = (* IncrVec, DecrVec *)
              match t_args with
              | [|VEC tv as t|] when tv <> MOVE ->
                 let$ res, k = res, [0;1;2;3] in
                 let$ res, l = res, [0;1;2;3] in
                 (t, `IncrVec_1 (k,l)) :: (t, `DecrVec_1 (k,l)) :: res
              | _ -> res in
            let res = (* ScaleUp, ScaleDown *)
              match t_args, v_args with
              | [|(INT _ | VEC _ | GRID _ as t1); INT CARD|], [|_; `Int k|] when k>=2 && k<=3 ->
                 (* TODO: pb, constants not in previous level. Move to LEVEL 3? *)
                 (t1, `ScaleUp_2)::(t1, `ScaleDown_2)::res
              | _ -> res in
            (* MOVE as POS ? *)
            let res = (* ApplySymGrid *)
              match t_args with
              | [|GRID _ as t1|] ->
                 List.fold_left
                   (fun res sym -> (t1, `ApplySymGrid_1 sym)::res)
                   res all_symmetry
              | _ -> res in
            let res = (* Coloring *)
              match t_args with
              | [|GRID _ as t1; COLOR|] -> (t1, `Coloring_2)::res
              | _ -> res in
            let res = (* Plus *)
              match t_args with
              | [|INT (COORD (_,tv1)) as t1; INT (COORD (_,(SIZE|MOVE)))|] when tv1 <> MOVE ->
                 (t1, `Plus_2)::res
              | [|VEC tv1 as t1; VEC (SIZE|MOVE)|] when tv1 <> MOVE ->
                 (t1, `Plus_2)::res
              | _ -> res in
            let res = (* Minus *)
              match t_args with
              | [|INT (COORD (_,tv1)) as t1; INT (COORD (_, (SIZE|MOVE)))|] when tv1 <> MOVE ->
                 (t1, `Minus_2)::res
              | [|VEC tv1 as t1; VEC (SIZE|MOVE)|] when tv1 <> MOVE ->
                 (t1, `Minus_2)::res
              | _ -> res in
            res) in
      let index = (* LEVEL 3 *)
        Expr.index_apply_functions
          ~eval_func
          index 2
          (fun (t_args,v_args) ->
            let res = [] in
            let res = (* Tiling *)
              match t_args with
              | [|(VEC SIZE | GRID _ as t1)|] ->
                 let$ res, k = res, [1;2;3] in
                 let$ res, l = res, [1;2;3] in
                 if k>1 || l>1
                 then (t1, `Tiling_1 (k,l))::res
                 else res
              | _ -> res in
            let res = (* FillResizeAlike *)
              match t_args with
              | [|COLOR; VEC SIZE; GRID (full,_) as t3|] ->
                 let$ res, mode =
                   res, (if full
                         then [`TradeOff; `Total; `Strict]
                         else [`TradeOff; `Strict]) in
                 (t3, `FillResizeAlike_3 mode)::res
              | _ -> res in
            let res = (* SelfCompose *)
              match t_args with
              | [|COLOR; GRID _ as t2|] -> (t2, `SelfCompose_2)::res
              | _ -> res in
            let res = (* UnfoldSym *)
              match t_args with
              | [|GRID _ as t1|] ->
                 let$ res, sym_matrix = res, all_symmetry_unfold in
                 (t1, `UnfoldSym_1 sym_matrix)::res
              | _ -> res in
            let res = (* CloseSym *)
              match t_args with
              | [|COLOR; GRID (full,_) as t2|] when not full ->
                 let$ res, sym_seq = res, all_symmetry_close in
                 (t2, `CloseSym_2 sym_seq)::res
              | _ -> res in
            let res = (* SwapColors *)
              match t_args with
              | [|GRID (_,_) as t1; COLOR; COLOR|] -> (t1, `SwapColors_3)::res
              | _ -> res in
            let res = (* LogNot *)
              match t_args with
              | [|GRID (false,true) as t1|] -> (t1, `LogNot_1)::res
              | _ -> res in
            let res = (* And, Or, XOr, AndNOt *)
              match t_args with
              | [|GRID (false,true) as t1; t2|] when t2=t1 ->
                 let$ res, f = res, [`LogAnd_2; `LogOr_2; `LogXOr_2; `LogAndNot_2] in
                 (t1,f)::res
              | _ -> res in
            let res = (* ScaleTo *)
              match t_args with
              | [|GRID _ as t1; VEC SIZE|] -> (t1, `ScaleTo_2)::res
              | _ -> res in
            (* Stack *)
            res) in
      index

    (* refining *)

    let value_of_bool b = `Bool b

    let refinements_pat (t : typ) (c : constr) (args : model array) (varseq : varseq) : data -> (model * varseq * input) list =
      match t, c with
      | INT (COORD (axis,tv)), AnyCoord ->
         (fun data ->
           let ij = get_int data in
           let range =
             match tv with
             | SIZE -> Range.make_closed 1 Grid.max_size
             | POS | MOVE -> Range.make_closed 0 Grid.max_size in
           [(make_coord axis tv ij, varseq, `IntRange (ij,range))])
      | COLOR, AnyColor ->
         (fun data ->
           let c = get_color data in
           [(make_color c, varseq, `Color c)])
      | GRID (full,nocolor), AnyGrid ->
         (fun data ->
           let g = get_grid data in
           let h, w = Grid.dims g in
           let input = `GridDims (g,h,w) in
           let refs : (model * varseq * input) list = [] in
           let refs = (* BgColor *)
             if full then
               let xcol, varseq = Refining.new_var varseq in
               let xg1, varseq = Refining.new_var varseq in
               (make_bgcolor
                  (Model.make_def xcol (make_anycolor))
                  (Model.make_def xg1 (make_anygrid (false,nocolor))),
                varseq, input)
               :: refs
             else refs in
           let refs = (* IsFull *)
             if not full then
               (make_isfull (make_anygrid (true,nocolor)),
                varseq, input)
               :: refs
             else refs in
           let refs = (* Crop *)
             if not full then
               let xsize, varseq = Refining.new_var varseq in
               let xsize_i, varseq = Refining.new_var varseq in
               let xsize_j, varseq = Refining.new_var varseq in
               let xpos, varseq = Refining.new_var varseq in
               let xpos_i, varseq = Refining.new_var varseq in
               let xpos_j, varseq = Refining.new_var varseq in
               let xg1, varseq = Refining.new_var varseq in
               (make_crop
                  (Model.make_def xsize
                     (make_vec SIZE
                        (Model.make_def xsize_i (make_anycoord I SIZE))
                        (Model.make_def xsize_j (make_anycoord J SIZE))))
                  (Model.make_def xpos
                     (make_vec POS
                        (Model.make_def xpos_i (make_anycoord I POS))
                        (Model.make_def xpos_j (make_anycoord J POS))))
                  (Model.make_def xg1 (make_anygrid (false,nocolor))),
                varseq, input)
               :: refs
             else refs in
           let refs = (* Monocolor *)
             if not nocolor then
               let xcol, varseq = Refining.new_var varseq in
               let xmask, varseq = Refining.new_var varseq in
               (make_monocolor
                  (Model.make_def xcol (make_anycolor))
                  (Model.make_def xmask (make_anygrid (false,true))),
                varseq, input)
               :: refs
             else refs in
           let refs = (* Masks *)
             if nocolor then
               let msize, varseq_msize =
                 let xsize, varseq = Refining.new_var varseq in
                 let xsize_i, varseq = Refining.new_var varseq in
                 let xsize_j, varseq = Refining.new_var varseq in
                 Model.make_def xsize
                   (make_vec SIZE
                      (Model.make_def xsize_i (make_anycoord I SIZE))
                      (Model.make_def xsize_j (make_anycoord J SIZE))),
                 varseq in
               (make_empty msize, varseq_msize, input)
               :: (make_full msize, varseq_msize, input)
               :: (make_border msize, varseq_msize, input)
               :: (make_point, varseq, input)
               :: refs
             else refs in
           refs)
      (* TODO: refine Full and Border as Point is size=1,1 ? *)
      | _ ->
         (fun data -> [])
    let refinements_postprocessing t c args =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    let prunings_pat t c args varseq =
      match t, c with
      | INT (COORD (axis,tv)), Coord ij ->
         (fun data ->
           [make_anycoord axis tv, varseq, `IntRange (ij, Range.make_exact ij)])
      | COLOR, Color c ->
         (fun data ->
           [make_anycolor, varseq, `Color c])
      | _, AnyGrid ->
         (fun data -> [])
      | GRID tg, _ ->
         (fun data ->
           let g = get_grid data in
           let h, w = Grid.dims g in
           [make_anygrid tg, varseq, `GridDims (g,h,w)])
      | _ ->
         (fun data -> [])
    let prunings_postprocessing t c args =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    (* initialization *)
      
    let varseq0 : varseq = Myseq.range 1 max_int

    let get_init_task_model name task =
      let open Task_model in
      let env0 = make_dcolor Grid.black in (* dummy *)
      let init_task_model =
        let xi, varseq_i = Refining.new_var varseq0 in
        let xo, varseq_o = Refining.new_var varseq0 in
        { input_model = Model.make_def xi (make_anygrid (true,false));
          input_varseq = varseq_i;
          nb_env_vars = 0;
          output_model = Model.make_def xo (make_anygrid (true,false));
          output_varseq = varseq_o } in
      let info_o = (Grid.max_size, Grid.max_size) in
      env0, init_task_model, info_o

    let log_reading r m ~status =
      (*print_endline "READING";
        pp_endline xp_refinement r;*)
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
      Segment.reset_memoized_functions ();
      Funct.reset_memoized_functions_apply ()
  
  end

module MyMadil = Madil.Make(MyDomain)
