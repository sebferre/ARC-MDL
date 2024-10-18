
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

    (* model types *)

    type typ =
      { kind : typ_kind;
        ndim : int } (* to account for Ndseq *)
    and typ_kind =
      | BOOL
      | INT of typ_int
      | VEC of typ_vec
      | COLOR of typ_color
      | SEG (* object segmentation *)
      | MOTIF of typ_motif
      | GRID of typ_grid
      | OBJ of typ_grid
      | MAP of typ_kind * typ_kind
    (* list of values have the type of their elts *)
    and typ_int =
      | CARD
      | INDEX
      | COORD of typ_axis * typ_vec
    and typ_axis =
      | I
      | J
    and typ_vec =
      | POS
      | SIZE
      | MOVE (* TODO: consider moving into typ_int *)
    and typ_color =
      | C_BG of bool (* full *) (* background color *)
      | C_OBJ (* object color *)
    and typ_motif =
      | MULTI
      | BI
    and typ_grid =
      [`Full | `Sprite | `Noise]
      * bool (* no-color, i.e. black and transparent *)
    (* MASK = GRID (`Sprite,true), (`Sprite,false) is the more general case *)

    let scalar kind = {kind; ndim = 0} [@@inline]
    
    let typ_bool = scalar BOOL (* for conditions, TODO: higher ndim? *)
    let typ_index = {kind = INT INDEX; ndim = 1}

    let nb_typ_axis = 2
    let nb_typ_vec = 3

    let axis_transpose = function
      | I -> J
      | J -> I
      
    let rec xp_typ ~html print t =
      if t.ndim > 0 then print#string "< ";
      xp_typ_kind ~html print t.kind;
      if t.ndim > 0 then (print#string " >^"; print#int t.ndim)
    and xp_typ_kind ~html print = function
      | BOOL -> print#string "BOOL"
      | INT ti -> xp_typ_int ~html print ti
      | VEC tv -> xp_typ_vec ~html print tv
      | COLOR tc -> print#string "COLOR"; xp_typ_color ~html print tc
      | SEG -> print#string "SEG"
      | MOTIF tm -> print#string "MOTIF"; xp_typ_motif ~html print tm
      | GRID tg -> xp_typ_grid ~html print tg
      | OBJ tg -> print#string "OBJ "; xp_typ_grid ~html print tg
      | MAP (ta,tb) -> xp_typ_kind ~html print ta; print#string " -> "; xp_typ_kind ~html print tb
    and xp_typ_int ~html print = function
      | CARD -> print#string "CARD"
      | INDEX -> print#string "INDEX"
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
    and xp_typ_motif ~html print = function
      | MULTI -> print#string "_MULTI"
      | BI -> print#string "_BI"
    and xp_typ_grid ~html print (filling,nocolor) =
      print#string
        (match filling, nocolor with
         | `Full, _ -> "GRID"
         | `Sprite, false -> "SPRITE"
         | `Sprite, true -> "MASK"
         | `Noise, false -> "NOISE"
         | `Noise, true -> "NOISE_MASK")

    (* values *)

    type value =
      [ `Null
      | `Bool of bool
      | `Int of int
      | `IntRange of int * Range.t (* INT of some range *)
      | `Vec of int * int
      | `VecRange of int * int * Range.t * Range.t (* TODO: make it more modular *)
      | `Color of Grid.color
      | `ColorTyp of Grid.color * typ_color (* COLOR of some type *)
      | `Seg of GPat.Objects.segmentation
      | `Motif of GPat.Motif.t
      | `MotifTyp of GPat.Motif.t * typ_motif (* MOTIF of some type *)
      | `Grid of Grid.t
      | `GridRange of Grid.t * typ_grid * Range.t (* height *) * Range.t (* width *) * int (* nb colors *) (* GRID of some type, with some size ranges, and some nb of concrete  colors *)
      | `Obj of value * value (* position at (i,j) of the subgrid *)
      | `Map of (value,value) Mymap.t
      | `MapTyp of (value,value) Mymap.t * typ_kind (* domain type *) * typ_kind (* range type *) (* assuming domain known from context *) (* TODO: missing range constraints *)
      | value Ndseq.seq ]

    let rec xp_value ~html (print : Xprint.t) : value -> unit = function
      | `Null -> print#string "null"
      | `Bool b -> xp_bool ~html print b
      | `Int i | `IntRange (i,_) -> xp_int ~html print i
      | `Vec (i,j) | `VecRange (i,j,_,_) -> xp_vec xp_int xp_int ~html print i j
      | `Color c | `ColorTyp (c,_) -> Grid.xp_color ~html print c
      | `Seg seg -> GPat.Objects.xp_segmentation ~html print seg
      | `Motif motif | `MotifTyp (motif,_) -> GPat.Motif.xp ~html print motif
      | `Grid g | `GridRange (g,_,_,_,_) -> Grid.xp_grid ~html print g
      | `Obj (pos,g1) ->
         print#string "an object "; xp_value ~html print g1;
         print#string " at position "; xp_value ~html print pos
      | `Map m | `MapTyp (m,_,_) ->
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
      | #Ndseq.seq as v -> Ndseq.xp_seq xp_value ~html print v

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
           
    (* model vars *)
      
    type var = int
             
    let xp_var ~html print x =
      xp_html_elt "span" ~classe:"model-var" ~html print
        (fun () -> print#string "$"; print#int x)

    (* model constr *)

    type segmentation = [`Connected | `ConnectedSameColor | `SameColor]
    type direction = [`H | `V]
      
    type constr =
      | Vec (* COORD, COORD : VEC *)
      | Obj (* POS, SPRITE : OBJ *)
      | DomMap of value list (* B+ : MAP(A,B) *) (* fixed set of keys, assumed known from ctx *)
      | Replace (* A, A : MAP(A,A) *)
      | Swap (* A, A : MAP(A,A) *)
      | BgColor (* COLOR, SPRITE : GRID *)
      | IsFull (* SPRITE : GRID *)
      | Crop (* SPRITE expr ; POS, SIZE : SPRITE *)
      | Objects of int (* nmax *) * [`Connected|`SameColor] (* mode *) (* SIZE, SEG, CARD, OBJ+, derived OBJ (merge) : SPRITE *) (* int is for max seq length, mode constrains SEG *)
      | ColorPartition (* SIZE, SPRITE+ : SPRITE *)
      | Monocolor (* COLOR, MASK : SPRITE *)
      | Recoloring (* SPRITE expr; MAP(COLOR,COLOR) : SPRITE *)
      | MotifMulti of bool (* partial *) (* MOTIF MULTI, SPRITE (core), derived SPRITE (pure), MASK? (mask), SPRITE (noise) *)
      | MotifBi of bool (* partial *) (* MOTIF BI, COLOR (bg), COLOR (obj), derived SPRITE (pure), MASK? (mask), SPRITE (noise) *)
      | Metagrid (* COLOR, MASK, VEC SIZE, SIZE+, SIZE+, GRID++ : GRID *)
      | Repeat (* SPRITE, INT+, INT+ : SPRITE *)
      | Empty (* SIZE : MASK *)
      | Full (* SIZE : MASK *)
      | Point (* MASK *)
      | Line (* len:INT SIZE, dir:VEC MOVE : MASK *)
      | ColorSeq of direction (* INT SIZE, COLOR+ : GRID *)
      | ColorMat (* VEC SIZE, COLOR++ : GRID *)
      | SeqCons of int (* depth *) (* head:X^k-1, tail:X^k : X^k *)
      | SeqRepeat of int (* depth *) (* X^(k-1) : X^k *)
      | SeqRange (* start:INT, step:INT : INT+ *) (* TODO: add depth arg *)
      | SeqIndex (* seq:X^n expr ; index:INT^1 : X^(n-k) *)
      | SeqIndexOf of typ_kind (* X. seq:X^n expr ; value:X : INDEX^1 *)

    let xp_any t ~html print () =
      xp_html_elt "span" ~classe:"model-any" ~html print
        (fun () -> print#string "?")
    
    let xp_pat c xp_args ~html print () =
      match c, xp_args with
      | Vec, [|xp_i; xp_j|] ->
         xp_vec xp_i xp_j ~html print () ()
      | Obj, [|xp_pos; xp_sprite|] ->
         print#string "at position "; xp_pos ~html print ();
         print#string ": ";
         xp_sprite ~html print ()
      | DomMap keys, [|xp_vals|] ->
         xp_list ~delims:("〈","〉") xp_value ~html print keys;
         print#string " -> ";
         xp_vals ~html print ()
      | Replace, [|xp_a; xp_b|] ->
         xp_a ~html print ();
         print#string " is replaced by ";
         xp_b ~html print ()
      | Swap, [|xp_a; xp_b|] ->
         xp_a ~html print ();
         print#string " is swapped with ";
         xp_b ~html print ()
      | BgColor, [|xp_color; xp_sprite|] ->
         print#string "a grid with background color "; xp_color ~html print ();
         print#string " and with contents"; xp_newline ~html print ();
         xp_sprite ~html print ()
      | IsFull, [|xp_sprite|] ->
         print#string "a full grid that is";
         xp_newline ~html print ();
         xp_sprite ~html print ()
      | Crop, [|xp_sprite; xp_pos; xp_size|] ->
         print#string "the crop of "; xp_sprite ~html print ();
         print#string " at position "; xp_pos ~html print ();
         print#string " with size "; xp_size ~html print ()
         (*print#string "a grid of size "; xp_size ~html print ();
           print#string " that contains at position "; xp_pos ~html print ();
           xp_newline ~html print ();
           xp_sprite ~html print ()*)
      | Objects (nmax,_mode), [|xp_size; xp_seg; xp_card; xp_objs; xp_merger|] ->
         print#string "a grid of size "; xp_size ~html print ();
         print#string " that contains "; xp_card ~html print ();
         print#string " <= "; print#int nmax;
         print#string " "; xp_seg ~html print ();
         print#string " objects like";
         xp_newline ~html print ();
         xp_objs ~html print ();
         print#string " forming the constellation object: ";
         xp_merger ~html print ()
      | ColorPartition, [|xp_size; xp_grids|] ->
         print#string "a grid of size "; xp_size ~html print ();
         print#string " that is composed of colored layers";
         xp_newline ~html print ();
         xp_grids ~html print ()
      | Monocolor, [|xp_color; xp_mask|] ->
         print#string "a grid with only color "; xp_color ~html print ();
         print#string " and with mask"; xp_newline ~html print ();
         xp_mask ~html print ()
         (* let xp_recoloring xp_colors xp_grid ~html print () =
            print#string "recoloring with "; xp_colors ~html print ();
            xp_newline ~html print ();
            xp_grid ~html print () *)
      | Recoloring, [|xp_grid; xp_map|] ->
         print#string "a recoloring of "; xp_grid ~html print ();
         xp_newline ~html print ();
         print#string "where "; xp_map ~html print ()
      | MotifMulti partial, [|xp_mot; xp_core; xp_pure; xp_mask_opt; xp_noise|] ->
         print#string (if partial then "a grid with partial motif " else "a grid with motif ");
         xp_mot ~html print ();
         print#string "  and with core:";
         xp_newline ~html print ();
         xp_core ~html print ();
         print#string "  that equals the pure grid: ";
         xp_pure ~html print ();
         if partial then (
           print#string "  filtered by the mask: ";
           xp_mask_opt ~html print ()
         );
         print#string "  plus the noise:";
         xp_newline ~html print ();
         xp_noise ~html print ()
      | MotifBi partial, [|xp_mot; xp_bgcolor; xp_color; xp_pure; xp_mask_opt; xp_noise|] ->
         print#string (if partial then "a grid with partial bicolor motif " else "a grid with bicolor motif ");
         xp_mot ~html print ();
         print#string "  and with bgcolor:"; xp_bgcolor ~html print ();
         print#string ", and with color:"; xp_color ~html print ();
         xp_newline ~html print ();
         print#string "  that equals the pure grid: ";
         xp_pure ~html print ();
         if partial then (
           print#string "  filtered by the mask: ";
           xp_mask_opt ~html print ()
         );
         print#string "  plus the noise:";
         xp_newline ~html print ();
         xp_noise ~html print ()
      | Metagrid, [|xp_sepcolor; xp_borders; xp_dims; xp_heights; xp_widths; xp_gridss|] ->
         print#string "a metagrid with dims ";
         xp_dims ~html print ();
         print#string " and sep-color ";
         xp_sepcolor ~html print ();
         print#string " and borders ";
         xp_borders ~html print ();
         xp_newline ~html print ();
         print#string "  with subgrid heights: ";
         xp_heights ~html print ();
         xp_newline ~html print ();
         print#string "  with subgrid widths: ";
         xp_widths ~html print ();
         xp_newline ~html print ();
         print#string "  with subgrids: ";
         xp_gridss ~html print ()      
      | Repeat, [|xp_grid; xp_nis; xp_njs|] ->
         print#string "a repeat pattern on rows "; xp_nis ~html print ();
         print#string " and on columns "; xp_njs ~html print ();
         print#string " of grid: "; xp_grid ~html print ()
      | Empty, [|xp_size|] ->
         print#string "an empty mask of size "; xp_size ~html print ()
      | Full, [|xp_size|] ->
         print#string "a full mask of size "; xp_size ~html print ()
      | Point, [||] ->
         print#string "a point mask"
      | Line, [|xp_len; xp_dir|] ->
         print#string "a line of length "; xp_len ~html print ();
         print#string " and direction "; xp_dir ~html print ()
      | ColorSeq dir, [|xp_size; xp_colors|] ->
         print#string "a ";
         print#string (match dir with `H -> "horizontal" | `V -> "vertical");
         print#string " 1D grid with size "; xp_size ~html print ();
         print#string " and colors: ";
         xp_colors ~html print ()
      | ColorMat, [|xp_size; xp_colorss|] ->
         print#string "a 2D grid with size "; xp_size ~html print ();
         print#string " and colors: ";
         xp_colorss ~html print ()
      | SeqCons depth, [|xp_hd; xp_tl|] ->
         print#string ("Cons[" ^ string_of_int depth ^ "]");
         xp_tuple2 xp_hd xp_tl ~html print ((),())
      | SeqRepeat depth, [|xp_e|] ->
         print#string ("Repeat[" ^ string_of_int depth ^ "](");
         xp_e ~html print (); print#string ")"
      | SeqRange, [|xp_start; xp_step|] ->
         print#string "Range";
         xp_tuple2 xp_start xp_step ~html print ((),())
      | SeqIndex, [|xp_seq; xp_index|] ->
         print#string "Index";
         xp_tuple2 xp_seq xp_index ~html print ((),())
      | SeqIndexOf tvalue, [|xp_seq; xp_value|] ->
         print#string "IndexOf";
         xp_tuple2 xp_seq xp_value ~html print ((),())
      | _ -> assert false

    let xp_field ~html print = function
      | Vec, 0 -> print#string "i"
      | Vec, 1 -> print#string "j"
      | Vec, _ -> assert false
      | Obj, 0 -> print#string "pos"
      | Obj, 1 -> print#string "sprite"
      | Obj, _ -> assert false
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
      | Objects _, 1 -> print#string "seg"
      | Objects _, 2 -> print#string "card"
      | Objects _, 3 -> print#string "obj"
      | Objects _, 4 -> print#string "merger"
      | Objects _, _ -> assert false
      | ColorPartition, 0 -> print#string "size"
      | ColorPartition, 1 -> print#string "layer"
      | ColorPartition, _ -> assert false
      | Monocolor, 0 -> print#string "color"
      | Monocolor, 1 -> print#string "mask"
      | Monocolor, _ -> assert false
      | Recoloring, 0 -> print#string "grid"
      | Recoloring, 1 -> print#string "colormap"
      | Recoloring, _ -> assert false
      | MotifMulti _, 0 -> print#string "motif"
      | MotifMulti _, 1 -> print#string "core"
      | MotifMulti _, 2 -> print#string "pure"
      | MotifMulti _, 3 -> print#string "mask"
      | MotifMulti _, 4 -> print#string "noise"
      | MotifMulti _, _ -> assert false
      | MotifBi _, 0 -> print#string "motif"
      | MotifBi _, 1 -> print#string "bgcolor"
      | MotifBi _, 2 -> print#string "color"
      | MotifBi _, 3 -> print#string "pure"
      | MotifBi _, 4 -> print#string "mask"
      | MotifBi _, 5 -> print#string "noise"
      | MotifBi _, _ -> assert false
      | Metagrid, 0 -> print#string "sepcolor"
      | Metagrid, 1 -> print#string "borders"
      | Metagrid, 2 -> print#string "dims"
      | Metagrid, 3 -> print#string "heights"
      | Metagrid, 4 -> print#string "widths"
      | Metagrid, 5 -> print#string "gridss"
      | Metagrid, _ -> assert false
      | Repeat, 0 -> print#string "grid"
      | Repeat, 1 -> print#string "rows"
      | Repeat, 2 -> print#string "cols"
      | Repeat, _ -> assert false
      | Empty, _ -> print#string "size"
      | Full, _ -> print#string "size"
      | Point, _ -> assert false
      | Line, 0 -> print#string "length"
      | Line, 1 -> print#string "direction"
      | Line, _ -> assert false
      | ColorSeq _, 0 -> print#string "size"
      | ColorSeq _, 1 -> print#string "colors"
      | ColorSeq _, _ -> assert false
      | ColorMat, 0 -> print#string "size"
      | ColorMat, 1 -> print#string "colors"
      | ColorMat, _ -> assert false
      | SeqCons _, 0 -> print#string "head"
      | SeqCons _, 1 -> print#string "tail"
      | SeqCons _, _ -> assert false
      | SeqRepeat _, 0 -> print#string "elt"
      | SeqRepeat _, _ -> assert false
      | SeqRange, 0 -> print#string "start"
      | SeqRange, 1 -> print#string "step"
      | SeqRange, _ -> assert false
      | SeqIndex, 0 -> print#string "seq"
      | SeqIndex, 1 -> print#string "index"
      | SeqIndex, _ -> assert false
      | SeqIndexOf _, 0 -> print#string "seq"
      | SeqIndexOf _, 1 -> print#string "value"
      | SeqIndexOf _, _ -> assert false
    
    (* functions *)

    type symmetry =
      [ `Id
      | `FlipHeight | `FlipWidth | `FlipDiag1 | `FlipDiag2
      | `Rotate180 | `Rotate90 | `Rotate270
      ]

    type func_itemwise =
      [ `Plus_2 (* on Int, Vec *)
      | `Minus_2 (* on Int, Vec *)
      | `Modulo_2 (* on Int *)
      | `ScaleUp_2 (* on (Int, Vec, Mask, Shape, Grid as T), Card -> T *)
      | `ScaleDown_2 (* on (Int, Vec, Mask, Shape, Grid as T), Card -> T *)
      | `ScaleTo_2 (* Mask, Grid, Vec -> Mask *)
      | `I_1 (* Vec -> Coord *)
      | `J_1 (* Vec -> Coord *)
      | `IJTranspose_1 (* I <-> J *)
      | `Direction_1 (* Int/Vec -> Int/Vec *)
      | `Abs_1 (* Int/Vec -> Int/Vec *)
      | `AsTVec_1 of typ_vec (* Int/Vec -> tv *)
      | `Pos_1 (* Obj -> Pos *)
      | `Grid_1 (* Obj -> Grid *)
      | `Size_1 (* Grid -> Vec *)
      | `Crop_2 (* Grid, Rectangle -> Grid *)
      | `Strip_1 (* on Grid *)
      | `Corner_2 (* on Vec *)
      | `Average_n (* on Int, Vec *)
      | `Span_2 (* on Vec *)
      | `Norm_1 (* Vec -> Int *)
      | `Diag1_1 of int (* Vec -> Int *)
      | `Diag2_1 of int (* Vec -> Int *)
      | `LogNot_1 (* on Mask *)
      | `Stack_n (* on Grids *)
      | `Area_1 (* on Shape *)
      | `Left_1 (* on Obj *)
      | `Right_1 (* on Obj *)
      | `Center_1 (* on Obj *)
      | `Top_1 (* on Obj *)
      | `Bottom_1 (* on Obj *)
      | `Middle_1 (* on Obj *)
      | `MiddleCenter_1 (* on Obj *)
      | `ProjI_1 (* on Vec *)
      | `ProjJ_1 (* on Vec *)
      | `MaskOfGrid_1 (* Sprite -> Mask *)
      | `GridOfMask_2 (* Mask, Color -> Grid *)
      | `Tiling_1 of int * int (* on Vec/Mask/Shape *)
      | `Unrepeat_1 (* Grid -> Grid *)
      | `PeriodicFactor_2 of Grid.Transf.periodicity_mode (* on Color, Mask/Shape/Layer/Grid as T -> T *)
      | `FillResizeAlike_3 of Grid.Transf.periodicity_mode (* on Color, Vec, Mask/Shape/Layer/Grid as T -> T *)
      | `SelfCompose_3 (* Color bg, Color cmask, Mask/Shape/Grid as T, T -> T *)
      | `ApplySymVec_1 of symmetry * typ_vec (* on Vec *)
      | `ApplySymGrid_1 of symmetry (* on Mask, Shape, Layer; type of the argument as computation depends on it *)
      | `UnfoldSym_1 of symmetry list list (* on Mask, Shape, Layer *)
      (* sym list list = matrix to be filled with symmetries of some mask *)
      | `CloseSym_2 of symmetry list (* Color, Mask/Shape/Layer/Grid as T -> T *)
      (* symmetry list = list of symmetries to chain and stack to force some symmetry, taking the given color as transparent *)
      | `TranslationSym_2 of symmetry (* viz Grid *) (* Obj, Obj/Grid -> Vec *)
      | `MajorityColor_1 (* Grid -> Color *)
      | `MinorityColor_1 (* Grid -> Color *)
      | `ColorCount_1 (* Grid -> Int *)
      | `Coloring_2 (* Shape/Obj, Color -> Shape/Obj *)
      | `SwapColors_3 (* Grid, Color, Color -> Grid *)
      ]

    type func =
      [ `Cast_1 of typ_kind * typ_kind (* k -> k' cast *)
      | `Index_1 of int option list (* X^k -> X^0..k *)
      | `Tail_1 (* X^k -> X^k *)
      | `Reverse_1 (* X^k -> X^k *)
      | `Rotate_1 of int (* shift *) (* X^k -> X^k *)
      | `Transpose_1 (* X^k -> X^k *)
      | `Flatten_1 of bool (* by rows vs cols *) * bool (* like snake *) (* X^k -> X^k-1 *)
      | `Cardinal_1 (* X^k -> Int *)
      | `Sum_1 (* Int^k -> Int *)
      | `Min_1 (* Int^k -> Int *)
      | `Max_1 (* Int^k -> Int *)
      | `ArgMin_1 (* Int^k -> Index^1 *)
      | `ArgMax_1 (* Int^k -> Index^1 *)
      | `LogAnd_1 (* Mask^k -> Mask *)
      | `LogOr_1 (* Mask^k -> Mask *)
      | `LogXOr_1 (* Mask^k -> Mask *)
      | `Halves_1 of direction (* Grid^k -> Grid^(k+1) *)
      | `TranslatedOnto_1 (* Obj^k -> Pos^(k+1) *)
      | func_itemwise
      ]

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
      | `Cast_1 (k,k') ->
         print#string "cast["; xp_typ_kind ~html print k;
         print#string " > "; xp_typ_kind ~html print k';
         print#string "]"
      | `Index_1 is ->
         print#string "index";
         xp_list
           (fun ~html print -> function
            | None -> print#string ":"
            | Some i -> print#int i)
           ~html print is
      | `Tail_1 -> print#string "tail"
      | `Reverse_1 -> print#string "reverse"
      | `Rotate_1 shift -> print#string "rotate["; print#int shift; print#string "]"
      | `Transpose_1 -> print#string "transpose"
      | `Flatten_1 (rows,snake) ->
         print#string "flatten";
         print#string (if rows then "_by_rows" else "_by_cols");
         if snake then print#string "_like_snake"
      | `Cardinal_1 -> print#string "cardinal"
      | `Plus_2 -> print#string "+"
      | `Minus_2 -> print#string "-"
      | `Modulo_2 -> print#string "%"
      | `ScaleUp_2 -> print#string "*"
      | `ScaleDown_2 -> print#string "/"
      | `ScaleTo_2 -> print#string "scaleTo"
      | `I_1 -> print#string "i"
      | `J_1 -> print#string "j"
      | `IJTranspose_1 -> print#string "ij_transpose"
      | `Direction_1 -> print#string "direction"
      | `Abs_1 -> print#string "abs"
      | `AsTVec_1 tv -> print#string "as"; xp_typ_vec ~html print tv
      | `Pos_1 -> print#string "pos"
      | `Grid_1 -> print#string "grid"
      | `Size_1 -> print#string "size"
      | `Crop_2 -> print#string "crop"
      | `Strip_1 -> print#string "strip"
      | `Corner_2 -> print#string "corner"
      | `Sum_1 -> print#string "sum"
      | `Min_1 -> print#string "min"
      | `Max_1 -> print#string "max"
      | `ArgMin_1 -> print#string "argmin"
      | `ArgMax_1 -> print#string "argmax"
      | `Average_n -> print#string "average"
      | `Span_2 -> print#string "span"
      | `Norm_1 -> print#string "norm"
      | `Diag1_1 k -> print#string "diag1"
      | `Diag2_1 k -> print#string "diag2"
      | `LogAnd_1 -> print#string "and"
      | `LogOr_1 -> print#string "or"
      | `LogXOr_1 -> print#string "xor"
      | `LogNot_1 -> print#string "not"
      | `Stack_n -> print#string "stack"
      | `Area_1 -> print#string "area"
      | `Left_1 -> print#string "left"
      | `Right_1 -> print#string "right"
      | `Center_1 -> print#string "center"
      | `Top_1 -> print#string "top"
      | `Bottom_1 -> print#string "bottom"
      | `Middle_1 -> print#string "middle"
      | `MiddleCenter_1 -> print#string "middle_center"
      | `Halves_1 dir -> print#string "halves"; print#string (match dir with `H -> "H" | `V -> "V")
      | `ProjI_1 -> print#string "projI"
      | `ProjJ_1 -> print#string "projJ"
      | `MaskOfGrid_1 -> print#string "maskOfGrid"
      | `GridOfMask_2 -> print#string "gridOfMask"
      | `TranslatedOnto_1 -> print#string "translatedOnto"
      | `Tiling_1 (k,l) ->
         print#string "tiling";
         xp_tuple2 ~delims:("[","]") xp_int xp_int ~html print (k,l)
      | `Unrepeat_1 -> print#string "unrepeat"
      | `PeriodicFactor_2 mode ->
         print#string ("periodicFactor" ^ suffix_periodicity_mode mode)
      | `FillResizeAlike_3 mode ->
         print#string ("fillResizeAlike" ^ suffix_periodicity_mode mode)
      | `SelfCompose_3 -> print#string "compose"
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
      | `MinorityColor_1 -> print#string "minorityColor"
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
        inherit [typ,typ,constr,func] Model.asd
        method abstract t = {t with ndim = 0} (* ignoring ndim to avoid infinite recursion *)
        method pats t (* abstract *) =
          (* synchronize with is_default_constr *)
          assert (t.ndim = 0);
          let res =
            [ SeqCons 0, [|t; t|];
              SeqRepeat 0, [|t|];
              SeqIndex, [|t; scalar (INT INDEX)|] ] in
          match t.kind with
          | BOOL -> res
          | INT ti ->
             let res =
               (SeqRange, [|t; {t with kind = INT (COORD (I, MOVE))} |]) :: res in
             (* let res =
               match ti with
               | INDEX ->
                  let$ res, kind = res, [BOOL] in (* TODO: should be polymorphic, for every type *)
                  (SeqIndexOf kind, [|scalar kind; scalar kind|]) :: res
               | _ -> res in *)
             res
          | VEC tv ->
             (Vec, [| {t with kind = INT (COORD (I, tv))};
                      {t with kind = INT (COORD (J, tv))} |])
             :: res
          | COLOR tc -> res
          | SEG -> res
          | MOTIF tm -> res
          | GRID (filling,nocolor) ->
             let full = (filling = `Full) in
             List.fold_left
               (fun res (cond,c_args) ->
                 if cond
                 then c_args::res
                 else res)
               res
               [ full, (BgColor,
                        [| {t with kind = COLOR (C_BG full)};
                           {t with kind = GRID (`Sprite,nocolor)} |]);
                 not full, (IsFull, [| {t with kind = GRID (`Full,nocolor)} |]);
                 true, (Crop,
                        [| {t with kind = GRID (filling,nocolor)};
                           {t with kind = VEC POS};
                           {t with kind = VEC SIZE} |]);
                 not full, (Objects (1,`Connected),
                            [| {t with kind = VEC SIZE};
                               {t with kind = SEG};
                               {t with kind = INT CARD};
                               {t with kind = OBJ (`Sprite,nocolor)};
                            (* derived merger, not counting *) |]);
                 (*not nocolor, (ColorPartition, [|VEC SIZE; GRID (`Sprite,false)|]);*)
                 not nocolor, (Monocolor,
                               [| {t with kind = COLOR C_OBJ};
                                  {t with kind = GRID (filling,true)} |]);
                 not nocolor, (Recoloring,
                               [| {t with kind = GRID (filling,nocolor)};
                                  {t with kind = MAP (COLOR C_OBJ, COLOR C_OBJ)} |]);
                 true, (MotifMulti false,
                        [| {t with kind = MOTIF MULTI};
                           {t with kind = GRID ((if filling = `Noise then `Sprite else filling), nocolor)};
                          (* derived pure, not counting *)
                           {t with kind = GRID (`Sprite,true)}; (* TODO: encode optional *)
                           {t with kind = GRID (`Noise,nocolor)} |]);
                 (*true, (Repeat, [|GRID (filling,nocolor);
                                  INT (COORD (I, SIZE));
                                  INT (COORD (J, SIZE))|]);*)
                 true, (MotifBi false,
                        [| {t with kind = MOTIF BI};
                           {t with kind = COLOR (C_BG full)};
                           {t with kind = COLOR C_OBJ};
                           (* derived pure, not counting *)
                           {t with kind = GRID (`Sprite,true)}; (* TODO: encode optional *)
                           {t with kind = GRID (`Noise,nocolor)} |]);
                 (*true, (Repeat, [|GRID (filling,nocolor);
                                  INT (COORD (I, SIZE));
                                  INT (COORD (J, SIZE))|]);*)
                 true, (Metagrid,
                        [| {t with kind = COLOR (C_BG full)};
                           {t with kind = GRID (`Sprite,true)};
                           {t with kind = VEC SIZE};
                           {t with kind = INT (COORD (I,SIZE))};
                           {t with kind = INT (COORD (J,SIZE))};
                           {t with kind = GRID (filling,nocolor)} |]);
                 not full (*&& nocolor*), (Empty, [| {t with kind = VEC SIZE} |]);
                 not full && nocolor, (Full, [| {t with kind = VEC SIZE} |]);
                 not full && nocolor, (Point, [||]);
                 not full && nocolor, (Line, [| {t with kind = INT (COORD (I, SIZE))};
                                                {t with kind = VEC MOVE} |]);
                 full && not nocolor, (ColorSeq `H,
                                       [| {t with kind = INT (COORD (I,SIZE))};
                                          {t with kind = COLOR C_OBJ} |]);
                 full && not nocolor, (ColorMat,
                                       [| {t with kind = VEC SIZE};
                                          {t with kind = COLOR C_OBJ} |]) ]
          | OBJ tg ->
             (Obj, [| {t with kind = VEC POS};
                      {t with kind = GRID tg} |])
             :: res
          | MAP (ka,kb) ->
             List.fold_left
               (fun res (cond,c_args) ->
                 if cond
                 then c_args::res
                 else res)
               res
               [ true, (DomMap [], [| {t with kind = kb} |]);
                 ka=kb, (Replace, [| {t with kind = ka};
                                     {t with kind = ka} |]);
                 ka=kb, (Swap, [| {t with kind = ka};
                                  {t with kind = ka} |]) ]
        method funcs t (* abstract *) =
          assert (t.ndim = 0);
          let res =
            [ `Cast_1 (t.kind,t.kind), [|t|];
              `Index_1 [], [|t|];
              `Flatten_1 (true,false), [|t|];
              `Tail_1, [|t|];
              `Reverse_1, [|t|];
              `Rotate_1 1, [|t|];
              `Transpose_1, [|t|] ] in
          match t.kind with
          | BOOL -> res
          | INT CARD ->
             (`Cardinal_1, [| {t with kind = OBJ (`Sprite,false)} |]) (* TODO: generalize to other kinds, and other ndims, param and result *)
             ::(`Sum_1, [|t|])
             ::(`Min_1, [|t|])
             ::(`Max_1, [|t|])
             ::(`Plus_2, [|t; t|])
             ::(`Minus_2, [|t; t|])
             ::(`Area_1, [| {t with kind = GRID (`Sprite,false)} |])
             ::(`ColorCount_1, [| {t with kind = GRID (`Sprite,false)} |]) (* also for `Noise? *)
             ::(`Average_n, [|t; t|])
             ::res
          | INT INDEX ->
             (`Sum_1, [|t|])
             ::(`Min_1, [|t|])
             ::(`Max_1, [|t|])
             ::(`ArgMin_1, [| {t with kind = INT CARD} |]) (* TODO: should be any INT, except maybe INDEX *)
             ::(`ArgMax_1, [| {t with kind = INT CARD} |]) (* TODO: should be any INT, except maybe INDEX *)
             ::res
          | INT (COORD (axis,tv)) ->
             (`Sum_1, [|t|])
             ::(`Min_1, [|t|])
             ::(`Max_1, [|t|])
             ::(`I_1, [| {t with kind = VEC tv} |])
             ::(`J_1, [| {t with kind = VEC tv} |])
             ::(`Left_1, [| {t with kind = OBJ (`Sprite,false) } |])
             ::(`Right_1, [| {t with kind = OBJ (`Sprite,false) } |])
             ::(`Center_1, [| {t with kind = OBJ (`Sprite,false) } |])
             ::(`Top_1, [| {t with kind = OBJ (`Sprite,false) } |])
             ::(`Bottom_1, [| {t with kind = OBJ (`Sprite,false) } |])
             ::(`Middle_1, [| {t with kind = OBJ (`Sprite,false) } |])
             ::(`IJTranspose_1, [| {t with kind = INT (COORD (axis_transpose axis, tv))} |])
             ::(`Direction_1, [|t|])
             ::(`Abs_1, [|t|])
             ::(`AsTVec_1 tv, [| {t with kind = INT (COORD (axis, tv))} |]) (* should be any other tv *)
             ::(`Area_1, [| {t with kind = GRID (`Sprite,false)} |])
             ::(`Plus_2, [|t; t|])
             ::(`Minus_2, [|t; t|])
             ::(`ScaleUp_2, [|t; {t with kind = INT CARD} |])
             ::(`ScaleDown_2, [|t; {t with kind = INT CARD} |])
             ::(`Span_2, [|t; t|]) (* only on same axis POS *)
             ::(`Average_n, [|t; t|])
             ::res
          | VEC tv ->
             (`Pos_1, [| {t with kind = OBJ (`Sprite,false)} |])
             ::(`MiddleCenter_1, [| {t with kind = OBJ (`Sprite,false) } |])
             ::(`Size_1, [| {t with kind = GRID (`Sprite,false)} |])
             ::(`Plus_2, [|t; t|])
             ::(`Minus_2, [|t; t|])
             ::(`ScaleUp_2, [|t; {t with kind = INT CARD} |])
             ::(`ScaleDown_2, [|t; {t with kind = INT CARD} |])
             ::(`ProjI_1, [|t|])
             ::(`ProjJ_1, [|t|])
             ::(`IJTranspose_1, [|t|])
             ::(`Direction_1, [|t|])
             ::(`Abs_1, [|t|])
             ::(`AsTVec_1 tv, [| {t with kind = VEC tv} |]) (* should be any other tv *)
             ::(`Corner_2, [|t; t|]) (* only on POS *)
             ::(`Span_2, [|t; t|]) (* only on POS *)
             ::(`Average_n, [|t; t|])
             ::(`TranslatedOnto_1, [| {t with kind = OBJ (`Sprite,false)} |])
             ::(`TranslationSym_2 `Id, [| {t with kind = OBJ (`Sprite,false)};
                                          {t with kind = GRID (`Sprite,false)} |])
             (* ::(`ApplySymVec_1 (`Id,tv), [|t|]) *)
             ::(`Tiling_1 (2,2), [|t|])
             ::res
          | COLOR tc ->
             (`MajorityColor_1, [| {t with kind = GRID (`Sprite,false)}|]) (* also `Full and `Noise *)
             ::(`MinorityColor_1, [| {t with kind = GRID (`Sprite,false)} |]) (* also `Full and `Noise *)
             ::res
          | SEG -> res
          | MOTIF tm -> res
          | GRID (filling,nocolor) ->
             let full = (filling = `Full) in
             (`Grid_1, [| {t with kind = OBJ (filling,nocolor)} |])
             ::(`Halves_1 `H, [|t|])
             ::(`MaskOfGrid_1, [| {t with kind = OBJ (`Sprite,false)} |])
             ::(`ScaleUp_2, [|t; {t with kind = INT CARD} |])
             ::(`ScaleDown_2, [|t; {t with kind = INT CARD} |])
             ::(`ScaleTo_2, [|t; {t with kind = VEC SIZE} |])
               (*::(`Strip_1, [|GRID (false,false)|])*)
             (* ::(`PeriodicFactor_2 `TradeOff, [| {t with kind = COLOR (C_BG full)}; t|]) *)
             (* ::(`Crop_2, [| {t with kind = GRID (`Full,false)};
                            {t with kind = OBJ (`Sprite,false)} |]) *)
             ::(`ApplySymGrid_1 `Id, [|t|])
             ::(`Coloring_2, [|t; {t with kind = COLOR C_OBJ} |])
             ::(`Unrepeat_1, [|t|])
             (* ::(`FillResizeAlike_3 `TradeOff, [| {t with kind = COLOR (C_BG full)};
                                                 {t with kind = VEC SIZE};
                                                 t |]) *)
             ::(`SelfCompose_3, [| {t with kind = COLOR (C_BG full)};
                                   {t with kind = COLOR C_OBJ};
                                   t |])
             (* ::(`UnfoldSym_1 [], [|t|]) *)
             ::(`CloseSym_2 [], [| {t with kind = COLOR (C_BG full)}; t|])
             (* ::(`SwapColors_3, [|t; {t with kind = COLOR C_OBJ}; {t with kind = COLOR C_OBJ} |]) *)
             (* ::(`Stack_n, [|t; t|]) *)
             (* on masks *)
             ::(`LogNot_1, [|t|])
             ::(`LogAnd_1, [|t|])
             ::(`LogOr_1, [|t|])
             ::(`LogXOr_1, [|t|])
             ::res
          | OBJ (filling,nocolor) ->
             (*let full = (filling = `Full) in*)
             (* (`PeriodicFactor_2 `TradeOff, [| {t with kind = COLOR (C_BG full)}; t |]) *)
             (* ::(`FillResizeAlike_3 `TradeOff, [| {t with kind = COLOR (C_BG full)};
                                                 {t with kind = VEC SIZE};
                                                 t |]) *)
             (* ::(`ApplySymGrid_1 `Id, [|t|]) *)
             (* ::(`UnfoldSym_1 [], [|t|]) *)
             (* ::(`CloseSym_2 [], [| {t with kind = COLOR (C_BG full)}; t |]) *)
             res
          | MAP (ka,kb) -> res
        
        method expr_opt t = true
        method alt_opt t = false (* LATER *)
      end

    (* model processing *)
      
    type generator_info =
      [ `Null
      | `Int of int * int (* interval *)
      | `Vec of generator_info (* i *) * generator_info (* j *)
      | `Color of Grid.color list (* interval *)
      | `Seg of GPat.Objects.segmentation list
      | `Motif of GPat.Motif.t list
      | `Grid of (int * int) (* height *) * (int * int) (* width *) * Grid.color list (* color *)
      | `Obj of generator_info (* pos *) * generator_info (* grid *)
      | `Map of generator_info (* src *) * generator_info (* dst *)
      | generator_info Ndseq.seq ]

    type input =
      [ `Null
      | `IntRange of int * Range.t
      | `Vec of input * input
      | `Color of Grid.color (* TODO: add range: nb colors *)
      | `Seg of GPat.Objects.segmentation (* TODO: add range: nb segs *)
      | `Motif of GPat.Motif.t (* TODO: add range: nb motifs *)
      | `GridDimsCols of Grid.t * Range.t (* height range *) * Range.t (* width range *) * int (* nb cols *)
      | `Obj of input (* pos *) * input (* grid *)
      | `MapDomain of (value,value) Mymap.t * value list (* domain *)
      | input Ndseq.seq ]

    let rec xp_input : input html_xp =
      fun ~html print ->
      function
      | `Null ->
         print#string "Null"
      | `IntRange (i,r) ->
         print#string "IntRange"
      | `Vec (in_i,in_j) ->
         print#string "Vec"
      | `Color c ->
         print#string "Color"
      | `Seg seg ->
         print#string "Seg"
      | `Motif mot ->
         print#string "Motif"
      | `GridDimsCols (g,rh,rw,nc) ->
         print#string "GridDimsCols"
      | `Obj (in_pos,in_grid) ->
         print#string "Obj"
      | `MapDomain (m,dom) ->
         print#string "MapDomain"
      | #Ndseq.seq as x -> Ndseq.xp_seq xp_input ~html print x
    
    type encoding = dl
                  
  end

module MyDomain : Madil.DOMAIN =
  struct

    (* boiler plate code *)
    include Basic_types
    include Madil.Defined_types(Basic_types)

    (* parameters *)

    let alpha = def_param "alpha" 100. string_of_float
    let max_nb_parse = def_param "max_nb_parse" 100 string_of_int (* max nb of considered doc parses *)
    let max_nb_reads = def_param "max_nb_doc_reads" 3 string_of_int (* max nb of selected doc reads, passed to the next stage *)
    let max_nb_writes = def_param "max_nb_doc_writes" 3 string_of_int (* max nb of selected output writes *)
    let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
    let max_expr_size = def_param "max_expr_size" 9 string_of_int (* max size of candidate expressions *)
    let max_expr_refinements_per_read = def_param "max_expr_refinements_per_read" 1000 string_of_int (* max nb of considered expr refinements per grid read *)
    let max_expr_refinements_per_var = def_param "max_expr_refinements_per_var" 10 string_of_int (* max nb of considered expr refinements per model var *)
    let max_refinements = def_param "max_refinements" 100 string_of_int (* max nb of considered refinements *)
    let jump_width = def_param "jump_width" 3 string_of_int (* max nb of explored pattern refinements at some model path during learning (refining phase). min=1 *)
    let search_temperature = def_param "search_temperature" 1. string_of_float (* DEPRECATED by MCTS approach - to control choice of model to jump to and refine, based on softmax: base-2 log, values between -2. and 0. *)

    let max_interleave_parse_obj = def_param "max_interleave_parse_obj" 3 string_of_int

    let _ = Random.init 1976
    
    (* constructors and accessors *)
                        
    let get_pos : value -> (int * int) option =
      function
      | `Grid _ -> Some (0,0)
      | `Obj (`Vec (i, j), _) -> Some (i,j)
      | _ -> None
          
    let get_size : value -> (int * int) option =
      function
      | `Grid g -> Some (Grid.dims g)
      | `Obj (_, `Grid g1) -> Some (Grid.dims g1)
      | _ -> None

(* REM    let make_anyint ti : model = Model.make_any (INT ti)
    let make_anycard : model = make_anyint CARD
    let make_anycoord axis tv : model = make_anyint (COORD (axis,tv))
    let make_anyvec tv : model = Model.make_any (VEC tv)
    let make_anycolor tc : model = Model.make_any (COLOR tc)
    let make_anyseg : model = Model.make_any SEG
    let make_anymotif tmot : model = Model.make_any (MOTIF tmot)
    let make_anygrid tg : model = Model.make_any (GRID tg)
    let make_anyobj tg : model = Model.make_any (OBJ tg)
    let make_anymap ta tb : model = Model.make_any (MAP (ta,tb)) *)
    
(* REM    let make_vec tv mi mj : model = Model.make_pat (VEC tv) Vec [|mi;mj|]
    let make_obj tg mpos mg1 : model = Model.make_pat (OBJ tg) Obj [|mpos;mg1|]
    let make_dommap ta tb keys mvals : model = Model.make_pat (MAP (ta,tb)) (DomMap keys) [|mvals|]
    let make_replace ta tb ma mb : model = Model.make_pat (MAP (ta,tb)) Replace [|ma; mb|]
    let make_swap ta tb ma mb : model = Model.make_pat (MAP (ta,tb)) Swap [|ma; mb|]
    let make_bgcolor mcol mg1 : model = Model.make_pat (GRID (`Full,false)) BgColor [|mcol; mg1|]
    let make_isfull mg1 : model = Model.make_pat (GRID (`Sprite,false)) IsFull [|mg1|]
    let make_crop tg mg1 mpos msize : model = Model.make_pat (GRID tg) Crop [|mg1; mpos; msize|]
    let make_objects nmax mode msize mseg mcard mobjs mmerger : model = Model.make_pat (GRID (`Sprite,false)) (Objects (nmax,mode)) [|msize; mseg; mcard; mobjs; mmerger|]
    let make_colorpartition filling msize mgrids : model = Model.make_pat (GRID (filling,false)) ColorPartition [|msize; mgrids|]
    let make_monocolor mcol mmask : model = Model.make_pat (GRID (`Sprite,false)) Monocolor [|mcol; mmask|]
    let make_recoloring tg mgrid mmap : model = Model.make_pat (GRID tg) Recoloring [|mgrid; mmap|]
    let make_motifmulti tg partial mmotif mcore mpure mmask_opt mnoise : model = Model.make_pat (GRID tg) (MotifMulti partial) [|mmotif; mcore; mpure; mmask_opt; mnoise|]
    let make_motifbi tg partial mmotif mbgcolor mcolor mpure mmask_opt mnoise : model = Model.make_pat (GRID tg) (MotifBi partial) [|mmotif; mbgcolor; mcolor; mpure; mmask_opt; mnoise|]
    let make_metagrid tg msepcolor mborders mdims mheights mwidths mgridss : model = Model.make_pat (GRID tg) Metagrid [|msepcolor; mborders; mdims; mheights; mwidths; mgridss|]
    let make_repeat tg mgrid mnis mnjs : model = Model.make_pat (GRID tg) Repeat [|mgrid; mnis; mnjs|]
    let make_empty msize : model = Model.make_pat (GRID (`Sprite,false)) Empty [|msize|]
    let make_full msize : model = Model.make_pat (GRID (`Sprite,true)) Full [|msize|]
    let make_point : model = Model.make_pat (GRID (`Sprite,true)) Point [||]
    let make_line mlen mdir : model = Model.make_pat (GRID (`Sprite,true)) Line [|mlen; mdir|]
    let make_colorseq dir msize mcolors : model = Model.make_pat (GRID (`Full,false)) (ColorSeq dir) [|msize; mcolors|]
    let make_colormat msize mcolorss : model = Model.make_pat (GRID (`Full,false)) ColorMat [|msize; mcolorss|]
    let make_seqcons t depth mhd mtl = Model.make_pat t (SeqCons depth) [|mhd;mtl|]
    let make_seqrepeat t depth me = Model.make_pat t (SeqRepeat depth) [|me|]
    let make_seqrange t mstart mstep = Model.make_pat t SeqRange [|mstart; mstep|]
    let make_seqindex t mseq mindex = Model.make_pat t SeqIndex [|mseq; mindex|]
    let make_seqindexof tvalue mseq mindex = Model.make_pat (INT INDEX) (SeqIndexOf tvalue) [|mseq; mindex|] *)

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
    let get_seg (d : data) : GPat.Objects.segmentation =
      match Data.value d with
      | `Seg seg -> seg
      | _ -> assert false
    let get_motif (d : data) : GPat.Motif.t =
      match Data.value d with
      | `Motif mot -> mot
      | _ -> assert false
    let get_grid (d : data) : Grid.t =
      match Data.value d with
      | `Grid g -> g
      | _ -> assert false


    let make_objects_v_dmerger dsize dseg dcard dobjs : value * data =
      let vsize = Data.value dsize in
      let depth = Ndseq.depth vsize in
      let v, merger =
        Ndseq.map_tup ~depth (0,0)
          (function
           | `Vec (h, w), `Seg seg, `Int card, seq_objs ->
              let objs =
                match Ndseq.as_seq seq_objs with
                | Some (_,objs) ->
                   List.map
                     (function
                      | `Obj (`Vec (i,j), `Grid g1) -> (i,j,g1)
                      | _ -> assert false)
                     objs
                | None -> assert false in
              assert (card = List.length objs);
              let i0, j0, h0, w0 =
                if card = 0
                then 0, 0, 1, 1 (* dummy merger pos/size *)
                else
                  let i0, j0, ih0, jw0 =
                    List.fold_left
                      (fun (i0,j0,ih0,jw0) (i,j,g1) ->
                        let h1, w1 = Grid.dims g1 in
                        min i0 i, min j0 j,
                        max ih0 (i+h1), max jw0 (j+w1))
                      (h, w, 0, 0) objs in
                  assert (i0 < ih0 && j0 < jw0);
                  i0, j0, ih0 - i0, jw0 - j0 in
              let g = Grid.make h w Grid.transparent in
              let g0 = Grid.make h0 w0 Grid.transparent in (* merger object *)
              List.iter
                (fun (i,j,g1) ->
                  Grid.add_grid_at g i j g1;
                  Grid.add_grid_at g0 (i-i0) (j-j0) g1)
                objs;
              (`Grid g, `Obj (`Vec (i0,j0), `Grid g0))
           | _ -> assert false)
          (vsize, Data.value dseg, Data.value dcard, Data.value dobjs) in
      v, Data.make_dexpr merger

    let make_motif_dpure dmot vcore dnoise : data Myseq.t =
      let vmot = Data.value dmot in
      let depth = Ndseq.depth vmot in
      let* vpure =
        Ndseq.map_tup_myseq ~name:"make_motif_dpure" ~depth 0
          (function
           | `Motif mot, `Grid g_core, `Grid g_noise ->
              let h, w = Grid.dims g_noise in
              let* g_pure = Myseq.from_result (GPat.Motif.make_grid h w mot g_core) in
              Myseq.return (`Grid g_pure)
           | _ -> assert false)
          (vmot, vcore, Data.value dnoise) in
      Myseq.return (Data.make_dexpr vpure)
    let make_motif_multi_dpure dmot dcore dnoise =
      make_motif_dpure dmot (Data.value dcore) dnoise
    let make_motif_bi_dpure dmot dbgcolor dcolor dnoise =
      let vbgcolor, vcolor = Data.value dbgcolor, Data.value dcolor in
      let depth = Ndseq.depth vbgcolor in
      assert (Ndseq.depth vcolor = depth);
      let vcore =
        Ndseq.map_tup ~depth 0
          (function
           | `Color bgcolor, `Color color ->
              `Grid (GPat.Motif.make_core_bi bgcolor color)
           | _ -> assert false)
          (vbgcolor, vcolor) in
      assert (Ndseq.depth vcore = depth);
      make_motif_dpure dmot vcore dnoise
    
    (* evaluation *)

    let bool_of_value : value -> bool result = function
      | `Bool b -> Result.Ok b
      | _ -> Result.Error (Failure "model evaluation: expected Boolean value")

    let value_of_bool b = `Bool b
      
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
            | `Vec (h,w) ->
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
          | `Obj (`Vec (i, j), `Grid g1) ->
             let g1' = grid_sym sym g1 in
             Result.Ok (`Obj (`Vec (i, j), `Grid g1')) (* NOTE: do not use sym_pos because pos in PosShape must be the top-left corner of the shape, see def of TranslationSym *)
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
          | `Obj (`Vec (i, j), `Grid g1) ->
             let| g1 = unfold_grid sym_matrix g1 in
             Result.Ok (`Obj (`Vec (i, j), `Grid g1))
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
          | `Obj (`Vec (i, j), `Grid g1) ->
             let| g1 = close_grid sym_seq bgcolor g1 in
             Result.Ok (`Obj (`Vec (i, j), `Grid g1))
          | _ -> Result.Error (Invalid_expr e)

        let reset_memoized_functions_apply () =
          reset_unfold_grid ();
          reset_close_grid ()
  
      end

    let eval_func_itemwise : func_itemwise -> (value array -> value result) =
      let e = "" in
      function
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
      | `I_1 ->
         (function
          | [| `Vec (i,j)|] -> Result.Ok (`Int i)
          | _ -> Result.Error (Invalid_expr e))
      | `J_1 ->
         (function
          | [| `Vec (i,j)|] -> Result.Ok (`Int j)
          | _ -> Result.Error (Invalid_expr e))
      | `IJTranspose_1 ->
         (function
          | [| `Int ij|] -> Result.Ok (`Int ij)
          | [| `Vec (i,j)|] -> Result.Ok (`Vec (j,i))
          | _ -> Result.Error (Invalid_expr e))
      | `Direction_1 ->
         let dir ij = if ij = 0 then 0 else ij / abs ij [@@inline] in
         (function
          | [| `Int ij|] -> Result.Ok (`Int (dir ij))
          | [| `Vec (i,j)|] -> Result.Ok (`Vec (dir i, dir j))
          | _ -> Result.Error (Invalid_expr e))
      | `Abs_1 ->
         (function
          | [| `Int ij|] -> Result.Ok (`Int (abs ij))
          | [| `Vec (i,j)|] -> Result.Ok (`Vec (abs i, abs j))
          | _ -> Result.Error (Invalid_expr e))
      | `AsTVec_1 POS ->
         (function
          | [| `Int ij|] when ij >= 0 -> Result.Ok (`Int ij)
          | [| `Vec (i,j)|] when i >= 0 && j >= 0 -> Result.Ok (`Vec (i,j))
          | _ -> Result.Error (Invalid_expr e))
      | `AsTVec_1 SIZE ->
         (function
          | [| `Int ij|] when ij >= 1 -> Result.Ok (`Int ij)
          | [| `Vec (i,j)|] when i >= 1 && j >= 1 -> Result.Ok (`Vec (i,j))
          | _ -> Result.Error (Invalid_expr e))
      | `AsTVec_1 MOVE ->
         (function
          | [| `Int ij|] -> Result.Ok (`Int ij)
          | [| `Vec (i,j)|] -> Result.Ok (`Vec (i,j))
          | _ -> Result.Error (Invalid_expr e))
      | `Pos_1 ->
         (function
          | [| `Obj (pos, _)|] -> Result.Ok pos
          | _ -> Result.Error (Invalid_expr e))
      | `Grid_1 ->
         (function
          | [| `Obj (pos,g1)|] -> Result.Ok g1
          | _ -> Result.Error (Invalid_expr e))    
      | `Size_1 ->
         (function
          | [|`Grid g|] ->
             let h, w = Grid.dims g in
             Result.Ok (`Vec (h, w))
          | _ -> Result.Error (Invalid_expr e))
      | `Crop_2 ->
         (function
          | [| `Grid g; `Obj (`Vec (ri, rj), `Grid shape)|] ->
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
          | [| `Vec (i, j)|] -> Result.Ok (`Int (abs i + abs j))
          | _ -> Result.Error (Invalid_expr e))
      | `Diag1_1 k ->
         (function
          | [| `Vec (i, j)|] -> Result.Ok (`Int ((i+j) mod k))
          | _ -> Result.Error (Invalid_expr e))
      | `Diag2_1 k ->
         (function
          | [| `Vec (i, j)|] -> Result.Ok (`Int ((i-j) mod k))
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
          | [| `Obj (`Vec (_, j), _)|] -> Result.Ok (`Int j)
          | _ -> Result.Error (Invalid_expr e))
      | `Right_1 ->
         (function
          | [| `Obj (`Vec (_, j), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             Result.Ok (`Int (j+w-1))
          | _ -> Result.Error (Invalid_expr e))
      | `Center_1 ->
         (function
          | [| `Obj (`Vec (_, j), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             if w mod 2 = 0
             then Result.Error (Undefined_result "Center: no center, even width")
             else Result.Ok (`Int (j + w/2))
          | _ -> Result.Error (Invalid_expr e))
      | `Top_1 ->
         (function
          | [| `Obj (`Vec (i, _), _)|] -> Result.Ok (`Int i)
          | _ -> Result.Error (Invalid_expr e))
      | `Bottom_1 ->
         (function
          | [| `Obj (`Vec (i, _), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             Result.Ok (`Int (i+h-1))
          | _ -> Result.Error (Invalid_expr e))
      | `Middle_1 ->
         (function
          | [| `Obj (`Vec (i, _), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             if h mod 2 = 0
             then Result.Error (Undefined_result "Middle: no middle, even height")
             else Result.Ok (`Int (i + h/2))
          | _ -> Result.Error (Invalid_expr e))
      | `MiddleCenter_1 ->
         (function
          | [| `Obj (`Vec (i, j), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             if h mod 2 = 0 || w mod 2 = 0
             then Result.Error (Undefined_result "MiddleCenter: no middle or no center, even height or width")
             else Result.Ok (`Vec (i + h/2, j + w/2))
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
          | [| `Grid g|] -> Result.Ok (`Grid (Grid.Mask.from_grid_background Grid.transparent g))
          | _ -> Result.Error (Invalid_expr e))
      | `GridOfMask_2 ->
         (function
          | [| `Grid m; `Color c|] ->
             Result.Ok (`Grid (Grid.Mask.to_grid m Grid.black c)) (* TODO: improve *)
          | _ -> Result.Error (Invalid_expr e))
      | `Tiling_1 (k,l) ->
         (function
          | [| `Vec (h, w)|] -> Result.Ok (`Vec (h*k, w*l))
          | [| `Grid g|] ->
             let| g' = Grid.Transf.tile k l g in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))
      | `Unrepeat_1 ->
         (function
          | [| `Grid g|] ->
             (match Grid_patterns.parse_repeat g with
              | Some (g1,_,_) -> Result.Ok (`Grid g1)
              | None -> Result.Error (Invalid_expr e))             
          | _ -> Result.Error (Invalid_expr e))
      | `PeriodicFactor_2 mode ->
         (function
          | [| `Color bgcolor; d2|] ->
             (match d2 with
              | `Grid g ->
                 let| g' = Grid.Transf.periodic_factor mode bgcolor g in
                 Result.Ok (`Grid g')
              | `Obj (pos, `Grid shape) ->
                 let| shape' = Grid.Transf.periodic_factor mode bgcolor shape in
                 Result.Ok (`Obj (pos, `Grid shape'))
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
              | `Obj (pos, `Grid shape) ->
                 let| shape' = Grid.Transf.fill_and_resize_alike mode bgcolor new_size shape in
                 Result.Ok (`Obj (pos, `Grid shape'))
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `SelfCompose_3 ->
         (function
          | [| `Color bgcolor; `Color c_mask; `Grid g1|] ->
             let| g = Grid.Transf.compose bgcolor c_mask g1 g1 in
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
      | `MinorityColor_1 ->
         (function
          | [| `Grid g|] ->
             let| c = Grid.minority_color Grid.black g in
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
              | _ -> Result.Error (Invalid_expr e))
          | _ -> Result.Error (Invalid_expr e))
      | `SwapColors_3 ->
         (function
          | [| `Grid g; `Color c1; `Color c2|] ->
             let| g' = Grid.Transf.swap_colors g c1 c2 in
             Result.Ok (`Grid g')
          | _ -> Result.Error (Invalid_expr e))

    let eval_aggreg (name : string) (init : value -> 'a option) (g_item : 'a * value -> 'a option) (v1 : value) : 'a result =
      (* v1 is usually a sequence *)
      let acc_opt =
        Ndseq.fold_left
          (fun res v ->
            match res with
            | None -> init v
            | Some acc -> g_item (acc, v))
          None v1 in
      match acc_opt with
      | Some acc -> Result.Ok acc
      | None -> Result.Error (Undefined_result (name ^ ": no values"))

    let eval_arg_best (name : string) (proj : value -> 'a option) (better : 'a -> 'a -> bool) (v1 : value) : value result (* index *) =
      let res =
        Ndseq.foldi_left
          (fun res revpath v ->
            match res, proj v with
            | _, None -> None
            | None, Some x -> Some (revpath, x)
            | Some (best_revpath, best), Some x ->
               if better x best
               then Some (revpath, x)
               else res)
          None v1 in
      match res with
      | Some (best_revpath, _best) ->
         Result.Ok (Ndseq.seq 0 (List.rev_map (fun i -> `Int i) best_revpath))
      | None -> Result.Error (Undefined_result (name ^ ": no values"))

    let rec eval_func (f : func) : value array -> value result = (* QUICK *)
      match f with
      | `Cast_1 (k,k') ->
         (function
          | [|v1|] -> Result.Ok v1
          | _ -> assert false)         
      | `Index_1 is ->
         (function
          | [|v1|] ->
             Option.to_result
               ~none:(Undefined_result "index: undefined")
               (Ndseq.index_list v1 is)
          | _ -> assert false)
      | `Tail_1 ->
         (function
          | [|v1|] ->
             Option.to_result
               ~none:(Undefined_result "tail: undefined on the empty sequence")
               (Ndseq.tail ~depth:0 v1)
          | _ -> assert false)
      | `Reverse_1 ->
         (function
          | [|v1|] ->
             if Ndseq.depth v1 >= 1
             then
               Result.Ok (Ndseq.map ~depth:0 0
                            (Ndseq.seq_of_seq List.rev)
                            v1)
             else Result.Error (Undefined_result "reverse: not defined on scalars")
          | _ -> assert false)
      | `Rotate_1 shift ->
         (function
          | [|v1|] ->
             if Ndseq.depth v1 >= 1
             then
               Result.Ok (Ndseq.map ~depth:0 0
                            (Ndseq.seq_of_seq
                               (fun l -> list_rotate l shift))
                            v1)
             else Result.Error (Undefined_result "rotate: not defined on scalars")
          | _ -> assert false)
      | `Transpose_1 ->
         (function
          | [|v1|] ->
             Option.to_result
               ~none:(Undefined_result "transpose: rows have different lengths")
               (Ndseq.transpose v1)
          | _ -> assert false)
      | `Flatten_1 (rows,snake) ->
         (function
          | [|v1|] ->
             Option.to_result
               ~none:(Undefined_result "flatten: less than 2 dims")
               (if rows
                then Ndseq.flatten_by_rows ~snake v1
                else Ndseq.flatten_by_cols ~snake v1)
          | _ -> assert false)
      | `Cardinal_1 ->
         (function
          | [|v1|] ->
             if Ndseq.depth v1 >= 1
             then
               Result.Ok (Ndseq.map ~depth:0 (- Ndseq.depth v1)
                            (Ndseq.item_of_seq
                               (fun l -> `Int (List.length l)))
                            v1)
             else Result.Error (Undefined_result "cardinal: not a sequence")
          | _ -> assert false)
      | `Sum_1 ->
         (function
          | [|v1|] ->
             let| sum =
               eval_aggreg "sum"
                 (function `Int i -> Some i | _ -> None)
                 (function (sum, `Int i) -> Some (sum + i) | _ -> None)
                 v1 in
             Result.Ok (`Int sum)
          | _ -> assert false)
      | `Min_1 ->
         (function
          | [|v1|] ->
             let| m =
               eval_aggreg "min"
                 (function `Int i -> Some i | _ -> None)
                 (function (m, `Int i) -> Some (min m i) | _ -> None)
                 v1 in
             Result.Ok (`Int m)
          | _ -> assert false)
      | `Max_1 ->
         (function
          | [|v1|] ->
             let| m =
               eval_aggreg "max"
                 (function `Int i -> Some i | _ -> None)
                 (function (m, `Int i) -> Some (max m i) | _ -> None)
                 v1 in
             Result.Ok (`Int m)
          | _ -> assert false)
      | `ArgMin_1 ->
         (function
          | [|v1|] -> (* returns first index if multiple *)
             eval_arg_best "argmin"
               (function `Int i -> Some i | _ -> None)
               (fun i best -> i < best)
               v1
          | _ -> assert false)
      | `ArgMax_1 ->
         (function
          | [|v1|] -> (* returns first index if multiple *)
             eval_arg_best "argmax"
               (function `Int i -> Some i | _ -> None)
               (fun i best -> i > best)
               v1
          | _ -> assert false)
      | `LogAnd_1 ->
         (function
          | [|v1|] ->
             let| m =
               eval_aggreg "and"
                 (function `Grid m -> Some m | _ -> None)
                 (function
                  | (m1, `Grid m2) when Grid.dims m1 = Grid.dims m2 ->
                     Some (Grid.Mask.inter m1 m2)
                  | _ -> None)
                 v1 in
             Result.Ok (`Grid m)
          | _ -> assert false)
      | `LogOr_1 ->
         (function
          | [|v1|] ->
             let| m =
               eval_aggreg "or"
                 (function `Grid m -> Some m | _ -> None)
                 (function
                  | (m1, `Grid m2) when Grid.dims m1 = Grid.dims m2 ->
                     Some (Grid.Mask.union m1 m2)
                  | _ -> None)
                 v1 in
             Result.Ok (`Grid m)
          | _ -> assert false)
      | `LogXOr_1 ->
         (function
          | [|v1|] ->
             let| m =
               eval_aggreg "xor"
                 (function `Grid m -> Some m | _ -> None)
                 (function
                  | (m1, `Grid m2) when Grid.dims m1 = Grid.dims m2 ->
                     Some (Grid.Mask.diff_sym m1 m2)
                  | _ -> None)
                 v1 in
             Result.Ok (`Grid m)
          | _ -> assert false)
      | `Halves_1 dir ->
         (function
          | [|v1|] ->
             Ndseq.map_result 1
               (function
                | `Grid g ->
                   let h, w = Grid.dims g in
                   let| g1, g2 =
                     match dir with
                     | `H ->
                        let w' = w / 2 in
                        let| g1 = Grid.Transf.crop g 0 0 h w' in
                        let| g2 = Grid.Transf.crop g 0 (w-w') h w' in
                        Result.Ok (g1,g2)
                     | `V ->
                        let h' = h / 2 in
                        let| g1 = Grid.Transf.crop g 0 0 h' w in
                        let| g2 = Grid.Transf.crop g (h - h') 0 h' w in
                        Result.Ok (g1,g2) in
                   Result.Ok (Ndseq.seq 0 [`Grid g1; `Grid g2])
                | _ -> Result.Error (Undefined_result "halvesX: not a grid"))
               v1
          | _ -> assert false)
      | `TranslatedOnto_1 ->
         (function
          | [|v1|] ->
             let ndim = Ndseq.depth v1 in
             if ndim > 0
             then
               Result.Ok
               (Ndseq.map ~depth:(ndim - 1) 1 (* adding a dimension *)
                 (fun seq_objs ->
                   match Ndseq.as_seq seq_objs with
                   | Some (d, objs) ->
                      assert (d = 0);
                      Ndseq.seq 1
                        (List.map
                           (fun obj1 ->
                             Ndseq.seq 0
                               (List.map
                                  (fun obj2 ->
                                    match obj1, obj2 with
                                    | `Obj (`Vec (mini1,minj1), `Grid g1),
                                      `Obj (`Vec (mini2,minj2), `Grid g2) ->
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
                                       `Vec (mini1 + ti, minj1 + tj)
                                    | _ -> assert false)
                                  objs))
                           objs)
                   | None -> assert false)
                 v1)
             else Result.Error (Undefined_result "translatedOnto_1: not a sequence")
          | _ -> assert false)
    
      | #func_itemwise as f ->
         let f_item = eval_func_itemwise f in
         (fun args -> Ndseq.broadcast_result f_item args)

    let eval_unbound_var x = Result.Error (Failure ("eval: unbound var $" ^ string_of_int x)) (* Result.Ok `Null *)
    let eval_arg () = Result.Error (Failure "eval: unexpected Arg")

    (* model-based generation *)
      
(*    let default_grid (filling, nocolor) (h, w) =
      match filling, nocolor with
      | `Full, _ -> Grid.make h w Grid.black
      | `Sprite, false -> Grid.make h w Grid.blue
      | `Sprite, true -> Grid.make h w Grid.Mask.one
      | `Noise, _ -> Grid.make h w Grid.transparent
    let default_grid, reset_default_grid =
      Memo.memoize2 ~size:103 default_grid *)

    let generator_value v info =
      let* v', info =
        if info = `Null (* expression-only argument *)
        then Myseq.return (v, `Null)
        else
          Ndseq.match_myseq 0
            (fun v info -> Myseq.return (v, info))
            v info in
      (* Warning: v' may be different from v because of broadcasting in Ndseq.match_myseq *)
      Myseq.return (Data.make_dexpr v', info)

    let rec generator_any t info =
      let depth = Ndseq.depth info in
      let rec aux kind info =
            match kind, info with
            | _, `Null ->
               Myseq.return (`Null, `Null)
            | BOOL, _ -> assert false
            | INT _, `Int (a,b) ->
               let range = Range.make_closed a b in
               let* n = Myseq.range a b in
               Myseq.return (`Int n, `IntRange (n,range))
            | VEC tv, `Vec (`Int (i1,i2), `Int (j1,j2)) ->
               let ri = Range.make_closed i1 i2 in
               let rj = Range.make_closed j1 j2 in
               let* i = Myseq.range i1 i2 in
               let* j = Myseq.range j1 j2 in
               Myseq.return (`Vec (i,j), `VecRange (i,j,ri,rj))
            | VEC _, `Vec _ -> assert false
            | COLOR tc, `Color lc ->
               let* c = Myseq.from_list lc in
               Myseq.return (`Color c, `ColorTyp (c,tc))
            | SEG, `Seg lseg ->
               let* seg = Myseq.from_list lseg in
               Myseq.return (`Seg seg, `Seg seg)
            | MOTIF tmot, `Motif lmot ->
               let* mot = Myseq.from_list lmot in
               Myseq.return (`Motif mot, `MotifTyp (mot,tmot))
            | GRID tg, `Grid ((minh,maxh),(minw,maxw),lc) ->
               let range_h = Range.make_closed minh maxh in
               let range_w = Range.make_closed minw maxw in
               let nc = List.length lc in
               let* lhwc = Myseq.product_fair
                             [Myseq.range minh maxh;
                              Myseq.range minw maxw;
                              Myseq.from_list lc] in
               (match lhwc with
                | [h; w; c] ->
                   let g = Grid.make h w c in
                   Myseq.return (`Grid g, `GridRange (g,tg,range_h,range_w,nc))
                | _ -> assert false)
            | OBJ tg, `Obj (info_pos,info_g1) ->
               let* vpos, vpos_r = aux (VEC POS) info_pos in
               let* vg1, vg1_r = aux (GRID tg) info_g1 in
               Myseq.return (`Obj (vpos,vg1), `Obj (vpos_r, vg1_r))
            | MAP (ka,kb), `Map (info_a, info_b) ->
               let m = Mymap.empty in
               Myseq.return (`Map m, `MapTyp (m,ka,kb)) (* empty map = identity map *)
            | _ -> assert false
      in
      let* v, vr =
        Ndseq.map_tup_myseq ~depth (0,0)
          (fun info -> aux t.kind info)
          (tup1 info) in
      let info = Ndseq.const `Null info in
      Myseq.return (Data.make_dany v vr, info)
    
    let rec generator_pat t c gen_args info =
      let depth = Ndseq.depth info in
      match t.kind, c, gen_args with
      | _, Vec, [|gen_i; gen_j|] ->
         let info_i, info_j =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Vec (i,j) -> i, j
              | _ -> assert false)
             (tup1 info) in
         let* lij = Myseq.product_fair [gen_i info_i;
                                        gen_j info_j] in
         (match lij with
          | [di, _; dj, _] ->
             let v : value =
               Ndseq.map_tup ~depth 0
                 (function
                  | `Int i, `Int j -> `Vec (i,j)
                  | _ -> assert false)
                 (Data.value di, Data.value dj) in
             Myseq.return (Data.make_dpat v c [|di; dj|], info)
          | _ -> assert false)
    
      | OBJ _, Obj, [|gen_pos; gen_g1|] ->
         let info_pos, info_g1 =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Obj (info_pos, info_g1) -> info_pos, info_g1
              | _ -> assert false)
             (tup1 info) in
         let* lposg1 = Myseq.product_fair [gen_pos info_pos;
                                           gen_g1 info_g1] in
         (match lposg1 with
          | [dpos, _; dg1, _] ->
             let v : value =
               Ndseq.map_tup ~depth 0
                 (fun (vpos,vg1) -> `Obj (vpos,vg1))
                 (Data.value dpos, Data.value dg1) in
             Myseq.return (Data.make_dpat v c [|dpos; dg1|], info)
          | _ -> assert false)
    
      | MAP (ka,kb), DomMap keys, [|gen_vals|] ->
         let k = List.length keys in
         let info_vals =
           Ndseq.map ~depth (+1)
             (Ndseq.seq_of_item
                (function
                 | `Map (info_a,info_b) -> List.init k (fun _ -> info_b)
                 | _ -> assert false))
             info in
         let* dvals, _ = gen_vals info_vals in
         let v : value =
           Ndseq.map ~depth (-1)
             (Ndseq.item_of_seq
                (fun vals ->
                  assert (List.length vals = k);
                  let m = mymap_of_list (List.combine keys vals) in
                  `Map m))
             (Data.value dvals) in
         Myseq.return (Data.make_dpat v c [|dvals|], info)
    
      | MAP (ka,kb), Replace, [|gen_a; gen_b|] ->
         let info_a, info_b =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Map (info_a,info_b) -> info_a, info_b
              | _ -> assert false)
             (tup1 info) in
         let* lab = Myseq.product_fair [gen_a info_a;
                                        gen_b info_b] in
         (match lab with
          | [da, _; db, _] ->
             let v : value =
               Ndseq.map_tup ~depth 0
                 (fun (va, vb) -> `Map (mymap_of_list [va, vb; vb, vb]))
                 (Data.value da, Data.value db) in
             Myseq.return (Data.make_dpat v c [|da; db|], info)
          | _ -> assert false)
    
      | MAP (ka,kb), Swap, [|gen_a; gen_b|] ->
         let info_a, info_b =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Map (info_a,info_b) -> info_a,info_b
              | _ -> assert false)
             (tup1 info) in
         let* lab = Myseq.product_fair [gen_a info_a;
                                        gen_b info_b] in
         (match lab with
          | [da, _; db, _] ->
             let v : value =
               Ndseq.map_tup ~depth 0
                 (fun (va, vb) -> `Map (mymap_of_list [va, vb; vb, va]))
                 (Data.value da, Data.value db) in
             Myseq.return (Data.make_dpat v c [|da; db|], info)
          | _ -> assert false)
    
      | GRID _, BgColor, [|gen_col; gen_g1|] ->
         let info_col =
           Ndseq.map ~depth 0
             (function
              | `Grid (rh,rw,lc) -> `Color lc
              | _ -> assert false)
             info in
         let* dbc, _ = gen_col info_col in
         let info_g1 =
           Ndseq.map_tup ~depth 0
             (function
              | `Color bc, `Grid (rh,rw,lc) ->
                 let lc1 = List.filter ((<>) bc) lc in
                 `Grid (rh,rw,lc1)
              | _ -> assert false)
             (Data.value dbc, info) in
         let* dg1, _ = gen_g1 info_g1 in
         let v : value =
           Ndseq.map_tup ~depth 0
             (function
              | `Color bc, `Grid g1 -> `Grid (Grid.fill_transparent g1 bc)
              | _ -> assert false)
             (Data.value dbc, Data.value dg1) in
         Myseq.return (Data.make_dpat v c [|dbc; dg1|], info)
    
      | GRID _, IsFull, [|gen_g1|] ->
         let* dg1, _ = gen_g1 info in
         let v = Data.value dg1 in
         Myseq.return (Data.make_dpat v c [|dg1|], info)
    
      | GRID _, Crop, [|gen_g; gen_pos; gen_size|] ->
         let* dg, _ = gen_g `Null in (* a fixed value *)
         let info_pos, info_size =
           Ndseq.map_tup ~depth (0,0)
             (function
              | _, `Grid g ->
                 let h, w = Grid.dims g in
                 `Vec (`Int (0,0), `Int (0,0)),
                 `Vec (`Int (1, h), `Int (1, w))
              | _ -> assert false)
             (info, Data.value dg) in
         let* l = Myseq.product_fair [gen_pos info_pos;
                                      gen_size info_size] in
         (match l with
          | [dpos, _; dsize, _] ->
             let* v =
               Ndseq.map_tup_myseq ~name:"gen/Crop" ~depth 0
                 (function
                  | `Grid g, `Vec (i,j), `Vec (h1,w1) ->
                     let* g  = Myseq.from_result (Grid.Transf.crop g i j h1 w1) in
                     Myseq.return (`Grid g)
                  | _ -> assert false)
                 (Data.value dg, Data.value dpos, Data.value dsize) in
             Myseq.return (Data.make_dpat v c [|dg; dpos; dsize|], info)
          | _ -> assert false)
    
      | GRID _, Objects (nmax,mode), [|gen_size; gen_seg; gen_card; gen_objs; _gen_merger|] ->
         let info_seg, info_card =
           Ndseq.map_tup ~depth (0,0)
             (fun _ ->
               `Seg [ match mode with
                      | `Connected -> GPat.Objects.(Connected (Connect8,false))
                      | `SameColor -> GPat.Objects.SameColor ],
               `Int (1,nmax))
             (tup1 info) in
         let* l = Myseq.product_fair
                    [gen_seg info_seg;
                     gen_card info_card] in
         (match l with
          | [dseg, _; dcard, _] ->
             let vcard = Data.value dcard in
             (* let* () = Myseq.from_bool
                         (Ndseq.for_all
                            (function
                             | `Int card -> card <= nmax
                             | _ -> assert false)
                            vcard) in *)
             let info_objs =
               Ndseq.map_tup ~depth 1
                 (function
                  | `Int card, `Grid ((minh,maxh),(minw,maxw),lc) ->
                     let info_obj = `Obj (`Vec (`Int (0,0), `Int (0,0)), `Grid ((1,3),(1,3),lc)) in
                     Ndseq.seq 0 (List.init card (fun _ -> info_obj))
                  | _ -> assert false)
                 (vcard, info) in
             let* dobjs, _ = gen_objs info_objs in
             let info_size =
               Ndseq.map_tup ~depth 0
                 (fun (seq_objs, info) ->
                   match Ndseq.as_seq seq_objs, info with
                   | Some (0, objs), `Grid ((minh,maxh),(minw,maxw),lc) ->
                      let minh, minw =
                        List.fold_left
                          (fun (minh,minw) obj ->
                            match obj with
                            | `Obj (`Vec (i,j), `Grid g1) ->
                               let h1, w1 = Grid.dims g1 in
                               max minh (i+h1), max minw (j+w1)
                            | _ -> assert false)
                          (minh,minw) objs in
                      let maxh, maxw = max maxh minh, max maxw minw in
                      `Vec (`Int (minh,maxh), `Int (minw,maxw))
                   | _ -> assert false)
                 (Data.value dobjs, info) in
             let* dsize, _ = gen_size info_size in
             let v, dmerger = make_objects_v_dmerger dsize dseg dcard dobjs in
             Myseq.return (Data.make_dpat v c [|dsize; dseg; dcard; dobjs; dmerger|], info)
          | _ -> assert false)
    
(* TODO      | _, ColorPartition, [|gen_size; gen_grids|], _ ->
         let* l = Myseq.product_fair [gen_size info; gen_grids info] in
         (match l with
          | [dsize, _; dgrids, _] ->
             let* data = Myseq.from_result (make_dcolorpartition dsize dgrids) in
             Myseq.return (data, `Null)
          | _ -> assert false) *)
    
      | GRID _, Monocolor, [|gen_col; gen_mask|] ->
         let info_col, info_mask =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Grid (rh,rw,lc) -> `Color lc, `Grid (rh,rw,[Grid.Mask.one])
              | _ -> assert false)
             (tup1 info) in
         let* l = Myseq.product_fair [gen_col info_col;
                                      gen_mask info_mask] in
         (match l with
          | [dcol, _; dmask, _] ->
             let* v =
               Ndseq.map_tup_myseq ~name:"gen/Monocolor" ~depth 0
                 (function
                  | `Color c, `Grid g1 ->
                     let* g = Myseq.from_result (Grid.Transf.swap_colors g1 Grid.Mask.one c) in
                     Myseq.return (`Grid g)
                  | _ -> assert false)
                 (Data.value dcol, Data.value dmask) in
             Myseq.return (Data.make_dpat v c [|dcol; dmask|], info)
          | _ -> assert false)
    
      | GRID _, Recoloring, [|gen_grid; gen_map|] ->
         let info_map =
           Ndseq.map ~depth 0
             (function
              | `Grid (rh,rw,lc) -> `Map (`Color lc, `Color lc)
              | _ -> assert false)
             info in
         let* l = Myseq.product_fair [gen_grid `Null;
                                      gen_map info_map] in
         (match l with
          | [dgrid, _; dmap, _] ->
             let v =
               Ndseq.map_tup ~depth 0
                 (function
                  | `Grid g1, `Map mcol ->
                     let g =
                       Grid.map_pixels
                         (fun c1 ->
                           if Grid.is_true_color c1
                           then
                             match Mymap.find_opt (`Color c1) mcol with
                             | Some (`Color c) -> c
                             | Some _ -> assert false
                             | None -> c1
                           else c1)
                         g1 in
                     `Grid g
                  | _ -> assert false)
                 (Data.value dgrid, Data.value dmap) in
             Myseq.return (Data.make_dpat v c [|dgrid; dmap|], info)
          | _ -> assert false)
    
      | GRID _, MotifMulti partial, [|gen_mot; gen_core; _gen_pure; gen_mask_opt; gen_noise|] ->
         let info_mot, info_noise =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Grid ((minh,maxh),(minw,maxw),lc) ->
                 `Motif GPat.Motif.candidates_multi,
                 `Grid ((minh,maxh),(minw,maxw), [Grid.transparent])
              | _ -> assert false)
             (tup1 info) in
         let* l = Myseq.product_fair [gen_mot info_mot;
                                      gen_noise info_noise] in
         (match l with
          | [dmot, _; dnoise, _] ->
             let* info_mask_opt, info_core =
               Ndseq.map_tup_myseq ~name:"gen/MotifMulti/info_res" ~depth (0,0)
                 (function
                  | `Grid ((minh,maxh),(minw,maxw),lc), `Motif mot, `Grid gnoise ->
                     let h, w = Grid.dims gnoise in
                     let _, _, luv = GPat.Motif.all_coredims_of_motif mot h w in
                     let* u, v = Myseq.from_list luv in
                     Myseq.return
                       ((if partial then `Grid ((h,h),(w,w),[Grid.Mask.one]) else `Null),
                        `Grid ((u,u),(v,v),lc))
                  | _ -> assert false)
                 (info, Data.value dmot, Data.value dnoise) in
             let* dcore, _ = gen_core info_core in
             let* dmask_opt, _ = gen_mask_opt info_mask_opt in
             let* v, pure =
               Ndseq.map_tup_myseq ~name:"gen/Motif" ~depth (0,0)
                 (function
                  | `Motif mot, `Grid g_core, vmask_opt, `Grid g_noise ->
                     let mask_opt =
                       match partial, vmask_opt with
                       | true, `Grid mask -> Some mask
                       | _ -> None in
                     let h, w = Grid.dims g_noise in
                     let* g_pure = Myseq.from_result (GPat.Motif.make_grid h w mot g_core) in
                     let* g =
                       match mask_opt with
                       | None -> Myseq.return (Grid.Do.copy g_pure)
                       | Some m ->
                          let bgcolor = if partial then Grid.transparent else assert false in
                          Myseq.from_result (Grid.Mask.crop bgcolor m g_pure) in
                     Grid.add_grid_at g 0 0 g_noise;
                     Myseq.return (`Grid g, `Grid g_pure)
                  | _ -> assert false)
                 (Data.value dmot, Data.value dcore, Data.value dmask_opt, Data.value dnoise) in
             let dpure = Data.make_dexpr pure in (* computed data *)
             Myseq.return (Data.make_dpat v c [|dmot; dcore; dpure; dmask_opt; dnoise|], info)
          | _ -> assert false)

      | GRID (filling,nocolor), MotifBi partial, [|gen_mot; gen_bgcolor; gen_color; _gen_pure; gen_mask_opt; gen_noise|] ->
         let* info_mot, info_noise =
           Ndseq.map_tup_myseq ~depth (0,0)
             (function
              | `Grid ((minh,maxh),(minw,maxw),lc) ->
                 if maxh >= 3 && maxw >= 3 (* bicolor motifs have size at least 3x3 *)
                 then
                   Myseq.return
                     (`Motif GPat.Motif.candidates_bi,
                      `Grid ((max 3 minh, maxh),(max 3 minw, maxw), [Grid.transparent]))
                 else Myseq.empty
              | _ -> assert false)
             (tup1 info) in
         let* l = Myseq.product_fair [gen_mot info_mot;
                                      gen_noise info_noise] in
         (match l with
          | [dmot, _; dnoise, _] ->
             let* info_mask_opt, info_bgcolor =
               Ndseq.map_tup_myseq ~name:"gen/Motif/info_res" ~depth (0,0)
                 (function
                  | `Grid ((minh,maxh),(minw,maxw),lc), `Motif mot, `Grid gnoise ->
                     let h, w = Grid.dims gnoise in
                     let lbgcolor =
                       if filling = `Full
                       then lc
                       else Grid.transparent :: lc in
                     Myseq.return
                       ((if partial then `Grid ((h,h),(w,w),[Grid.Mask.one]) else `Null),
                        `Color lbgcolor)
                  | _ -> assert false)
                 (info, Data.value dmot, Data.value dnoise) in
             let* dbgcolor, _ = gen_bgcolor info_bgcolor in
             let info_color =
               Ndseq.map_tup ~depth 0
                 (function
                  | `Grid (_,_,lc), `Color bgcolor -> `Color (list_remove bgcolor lc)
                  | _ -> assert false)
                 (info, Data.value dbgcolor) in
             let* dcolor, _ = gen_color info_color in
             let* dmask_opt, _ = gen_mask_opt info_mask_opt in
             let* v, pure =
               Ndseq.map_tup_myseq ~name:"gen/Motif" ~depth (0,0)
                 (function
                  | `Motif mot, `Color bgcolor, `Color color, vmask_opt, `Grid g_noise ->
                     let mask_opt =
                       match partial, vmask_opt with
                       | true, `Grid mask -> Some mask
                       | _ -> None in
                     let h, w = Grid.dims g_noise in
                     let g_core = GPat.Motif.make_core_bi bgcolor color in
                     let* g_pure = Myseq.from_result (GPat.Motif.make_grid h w mot g_core) in
                     let* g =
                       match mask_opt with
                       | None -> Myseq.return (Grid.Do.copy g_pure)
                       | Some m ->
                          let g_bgcolor = if partial then Grid.transparent else assert false in
                          Myseq.from_result (Grid.Mask.crop g_bgcolor m g_pure) in
                     Grid.add_grid_at g 0 0 g_noise;
                     Myseq.return (`Grid g, `Grid g_pure)
                  | _ -> assert false)
                 (Data.value dmot, Data.value dbgcolor, Data.value dcolor, Data.value dmask_opt, Data.value dnoise) in
             let dpure = Data.make_dexpr pure in (* computed data *)
             Myseq.return (Data.make_dpat v c [|dmot; dbgcolor; dcolor; dpure; dmask_opt; dnoise|], info)
          | _ -> assert false)

      | GRID _, Metagrid, [|gen_sepcolor; gen_borders; gen_dims; gen_heights; gen_widths; gen_gridss|] ->
         let info_sepcolor, info_borders, info_dims =
           Ndseq.map_tup ~depth (0,0,0)
             (function
              | `Grid (rh,rw,lc) ->
                 `Color lc,
                 `Grid ((2,2), (2,2), [Grid.Mask.one]),
                 `Vec (`Int (1,3), `Int (1,3))                 
              | _ -> assert false)
             (tup1 info) in
         let* l = Myseq.product_fair [gen_sepcolor info_sepcolor;
                                      gen_borders info_borders;
                                      gen_dims info_dims] in
         (match l with
          | [dsepcolor, _; dborders, _; ddims, _] ->
             let info_heights, info_widths =
               Ndseq.map_tup ~depth (1,1)
                 (function
                  | `Vec (k,l) ->
                     Ndseq.seq 0 (List.init k (fun _ -> `Int (1,10))),
                     Ndseq.seq 0 (List.init l (fun _ -> `Int (1,10)))
                  | _ -> assert false)
                 (tup1 (Data.value ddims)) in
             let* l1 = Myseq.product_fair
                         [gen_heights info_heights;
                          gen_widths info_widths] in
             (match l1 with
              | [dheights, _; dwidths, _] ->
                 let info_gridss =
                   Ndseq.map_tup ~name:"gen/Metagrid/vx" ~depth 2
                     (function
                      | `Color lc, `Color sepcolor, `Vec (k,l), vheights, vwidths ->
                         let heights =
                           match Ndseq.as_seq vheights with
                           | Some (_,l) -> List.map (function `Int i -> i | _ -> assert false) l
                           | None -> assert false in
                         let widths =
                           match Ndseq.as_seq vwidths with
                           | Some (_,l) -> List.map (function `Int j -> j | _ -> assert false) l
                           | None -> assert false in
                         let lc1 = List.filter ((<>) sepcolor) lc in
                         let lc1 = if lc1 = [] then Grid.all_colors else lc1 in
                         Ndseq.seq 1
                           (List.init k (fun i ->
                                Ndseq.seq 0
                                  (List.init l (fun j ->
                                       let h1 = try List.nth heights i with _ -> assert false in
                                       let w1 = try List.nth widths j with _ -> assert false in
                                       `Grid ((h1,h1),(w1,w1),lc1)))))
                      | _ -> assert false)
                     (info_sepcolor, Data.value dsepcolor, Data.value ddims, Data.value dheights, Data.value dwidths) in
                 let* dgridss, _ = gen_gridss info_gridss in
                 let* v : value =
                   Ndseq.map_tup_myseq ~name:"gen/Metagrid/v" ~depth 0
                     (function
                      | `Color sepcolor, `Grid borders, `Vec (k,l), vheights, vwidths, vgridss ->
                         let part_heights =
                           match Ndseq.as_seq vheights with
                           | Some (_,l) -> Array.of_list (List.map (function `Int i -> i | _ -> assert false) l)
                           | None -> assert false in
                         let part_widths =
                           match Ndseq.as_seq vwidths with
                           | Some (_,l) -> Array.of_list (List.map (function `Int j -> j | _ -> assert false) l)
                           | None -> assert false in
                         let parts =
                           match Ndseq.as_seq vgridss with
                           | Some (_,l) ->
                              Array.of_list
                                (List.map
                                   (fun row ->
                                     match Ndseq.as_seq row with
                                     | Some (_,l2) -> Array.of_list (List.map (function `Grid g -> g | _ -> assert false) l2)
                                     | None -> assert false)
                                   l)
                           | None -> assert false in
                         let mg =
                           { GPat.Metagrid.sepcolor;
                             borders;
                             k;
                             l;
                             part_heights;
                             part_widths;
                             parts } in
                         let* g = Myseq.from_result (GPat.Metagrid.generate mg) in
                         Myseq.return (`Grid g)
                      | _ -> assert false)
                     (Data.value dsepcolor, Data.value dborders, Data.value ddims,
                      Data.value dheights, Data.value dwidths, Data.value dgridss) in
                 Myseq.return (Data.make_dpat v c [|dsepcolor; dborders; ddims; dheights; dwidths; dgridss|], info)
              | _ -> assert false)
          | _ -> assert false)
    
(* TODO      | _, Repeat, [|gen_grid; gen_nis; gen_njs|], _ ->
         let* l = Myseq.product_fair [gen_grid info; gen_nis info; gen_njs info] in
         (match l with
          | [dgrid, _; dnis, _; dnjs, _] ->
             let* data = Myseq.from_result (make_drepeat dgrid dnis dnjs) in
             Myseq.return (data, `Null)
         | _ -> assert false) *)
    
      | GRID _, Empty, [|gen_size|] ->
         let info_size =
           Ndseq.map ~depth 0
             (function
              | `Grid ((minh,maxh),(minw,maxw),_) -> `Vec (`Int (minh,maxh), `Int (minw,maxw))
              | _ -> assert false)
             info in
         let* dsize, _ = gen_size info_size in
         let v =
           Ndseq.map ~depth 0
             (function
              | `Vec (h,w) -> `Grid (Grid.Mask.empty h w)
              | _ -> assert false)
             (Data.value dsize) in
         Myseq.return (Data.make_dpat v c [|dsize|], info)
      | GRID _, Full, [|gen_size|] ->
         let info_size =
           Ndseq.map ~depth 0
             (function
              | `Grid ((minh,maxh),(minw,maxw),_) -> `Vec (`Int (minh,maxh), `Int (minw,maxw))
              | _ -> assert false)
             info in
         let* dsize, _ = gen_size info_size in
         let v =
           Ndseq.map ~depth 0
             (function
              | `Vec (h,w) -> `Grid (Grid.Mask.full h w)
              | _ -> assert false)
             (Data.value dsize) in
         Myseq.return (Data.make_dpat v c [|dsize|], info)
      | GRID _, Point, [||] ->
         let v =
           Ndseq.map ~depth 0
             (fun _ -> `Grid (Grid.Mask.full 1 1))
             info in
         Myseq.return (Data.make_dpat v c [||], info)
      | GRID _, Line, [|gen_len; gen_dir|] ->
         let info_len, info_dir =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Grid ((minh,maxh),(minw,maxw),_) ->
                 `Int (min minh minw, max maxh maxw),
                 `Vec (`Int (0,1), `Int (-1,1))
              | _ -> assert false)
             (tup1 info) in
         let* dlen, _ = gen_len info_len in
         let* ddir, _ = gen_dir info_dir in (* TODO: avoid (0,0) *)
         let* v =
           Ndseq.map_tup_myseq ~depth 0
             (function
              | `Int len, `Vec dir ->
                 let* g = Myseq.from_result (GPat.generate_line len dir) in
                 Myseq.return (`Grid g)
              | _ -> assert false)
             (Data.value dlen, Data.value ddir) in
         Myseq.return (Data.make_dpat v c [|dlen; ddir|], info)
    
      | GRID _, ColorSeq dir, [|gen_size; gen_colors|] ->
         let info_size =
           Ndseq.map ~depth 0
             (function
              | `Grid ((minh,maxh),(minw,maxw),lc) ->
                 (match dir with
                  | `H -> `Int (max 2 minw,maxw)
                  | `V -> `Int (max 2 minh,maxh))
              | _ -> assert false)
             info in
         let* dsize, _ = gen_size info_size in
         let info_colors =
           Ndseq.map_tup ~depth 1
             (function
              | `Grid ((minh,maxh),(minw,maxw),lc), `Int k ->
                 Ndseq.seq 0 (List.init k (fun _ -> `Color lc))
              | _ -> assert false)
             (info,Data.value dsize) in
         let* dcolors, _ = gen_colors info_colors in
         let v =
           Ndseq.map_tup ~depth 0
             (fun (vsize,vcolors) ->
               match vsize, Ndseq.as_seq vcolors with
               | `Int size, Some (0,lcolors) ->
                  let n = List.length lcolors in
                  assert (n = size);
                  let colors = List.map (function `Color c -> c | _ -> assert false) lcolors in
                  let g =
                    match dir with
                    | `H -> Grid.init 1 n (fun i j -> try List.nth colors j with _ -> assert false)
                    | `V -> Grid.init n 1 (fun i j -> try List.nth colors i with _ -> assert false) in
                  `Grid g
               | _ -> assert false)
             (Data.value dsize, Data.value dcolors) in
         Myseq.return (Data.make_dpat v c [|dsize; dcolors|], info)
    
      | GRID _, ColorMat, [|gen_size; gen_colorss|] ->
         let info_size =
           Ndseq.map ~depth 0
             (function
              | `Grid ((minh,maxh),(minw,maxw),lc) ->
                 `Vec (`Int (max 2 minh, min 3 maxh),
                       `Int (max 2 minw, min 3 maxw))
              | _ -> assert false)
             info in
         let* dsize, _ = gen_size info_size in
         let info_colorss =
           Ndseq.map_tup ~depth 2
             (function
              | `Grid ((minh,maxh),(minw,maxw),lc), `Vec (k,l) ->
                 Ndseq.seq 1
                   (List.init k (fun _ ->
                        Ndseq.seq 0
                          (List.init l (fun _ ->
                               `Color lc))))
              | _ -> assert false)
             (info, Data.value dsize) in                                   
         let* dcolorss, _ = gen_colorss info_colorss in
         let v =
           Ndseq.map_tup ~depth 0
             (fun (vsize, vcolorss) ->
               match vsize, Ndseq.as_seq vcolorss with
               | `Vec (size_h,size_w), Some (1,lcolorss) ->
                  let h = List.length lcolorss in
                  assert (h = size_h);
                  let ll =
                    List.map
                      (fun vcolors ->
                        match Ndseq.as_seq vcolors with
                        | Some (0,lcolors) -> lcolors
                        | _ -> assert false)
                      lcolorss in
                  let w =
                    List.fold_left
                      (fun res lcolors -> min res (List.length lcolors))
                      max_int ll in
                  assert (w = size_w);
                  let g =
                    Grid.init h w
                      (fun i j ->
                        let vc = try List.nth (List.nth ll i) j with _ -> assert false in
                        match vc with
                        | `Color c -> c
                        | _ -> assert false) in
                  `Grid g
               | _ -> assert false)
             (Data.value dsize, Data.value dcolorss) in
         Myseq.return (Data.make_dpat v c [|dsize; dcolorss|], info)
    
      | _, SeqCons depth, [|gen_hd; gen_tl|] ->
         let* xhd = Myseq.from_option (Ndseq.head ~depth info) in
         let* xtl = Myseq.from_option (Ndseq.tail ~depth info) in
         let* dhd, _ = gen_hd xhd in
         let* dtl, _ = gen_tl xtl in
         let v : value =
           Ndseq.cons ~depth (Data.value dhd) (Data.value dtl) in
         Myseq.return (Data.make_dpat v c [|dhd;dtl|], info)
    
      | _, SeqRepeat dep, [|gen_e|] ->
         let* xe =
           Ndseq.map_myseq ~depth:dep (-1)
             (Ndseq.item_of_seq
                (function
                 | [] -> Myseq.empty
                 | x::_ -> Myseq.return x))
             info in
         let* de, _ = gen_e xe in
         let v : value =
           Ndseq.map_tup ~depth:dep (depth - dep) 
             (fun (info,ve) ->
               match Ndseq.as_seq info with
               | Some (d,linfos) ->
                  assert (d = Ndseq.depth ve);
                  Ndseq.seq d (List.map (fun _ -> ve) linfos)
               | _ -> assert false)
             (info, Data.value de) in
         Myseq.return (Data.make_dpat v c [|de|], info)           

      | _, SeqRange, [|gen_start; gen_step|] ->
         let* xstart, xstep =
           Ndseq.map_tup_myseq ~depth:(depth-1) (0,0)
             (fun info ->
               match Ndseq.as_seq info with
               | Some (_,l) ->
                  (match l with
                   | `Int (a1,b1)::`Int (a2,b2)::_ ->
                      Myseq.return (`Int (a1,b1),
                                    `Int (a2-b1, b2-a1))
                   | _ -> Myseq.empty)
               | _ -> assert false)
             (tup1 info) in
         let* dstart, _ = gen_start xstart in
         let* dstep, _ = gen_step xstep in
         let v : value =
           Ndseq.map_tup ~depth:(depth-1) 1
             (fun (info,vstart,vstep) ->
               match Ndseq.as_seq info, vstart, vstep with
               | Some (_,l), `Int start, `Int step ->
                  let n = List.length l in
                  Ndseq.seq 0 (List.init n (fun i -> `Int (start + i * step)))
               | _ -> assert false)
             (info, Data.value dstart, Data.value dstep) in
         Myseq.return (Data.make_dpat v c [|dstart; dstep|], info)

      | _, SeqIndex, [|gen_seq; gen_index|] ->
         let* dseq, _ = gen_seq `Null in (* a fixed value *)
         let vseq = Data.value dseq in
         let depth_seq = Ndseq.depth vseq in
         let info_index =
           Ndseq.seq 0 (List.init (depth_seq - depth) (fun _ -> `Int (0,2))) in (* default index *)
         let* dindex, _ = gen_index info_index in
         let index =
           match Ndseq.as_seq (Data.value dindex) with
           | Some (0, l) ->
              List.map
                (function
                 | `Int i -> Some i
                 | _ -> assert false)
                l
           | _ -> assert false in
         let* v : value =
           match Ndseq.index_list vseq index with
           | Some v -> Myseq.return v
           | None -> Myseq.empty (* index undefined *) in
         Myseq.return (Data.make_dpat v c [|dseq; dindex|], info)

      | INT INDEX, SeqIndexOf tvalue, [|gen_seq; gen_value|] ->
         let* dseq, _ = gen_seq `Null in (* a fixed value *)
         let vseq = Data.value dseq in
         let depth_seq = Ndseq.depth vseq in
         let info_value = `Null in (* what else? *)
         let* dvalue, _ = gen_value info_value in
         let value = Data.value dvalue in
         let depth_value = Ndseq.depth value in
         let* v =
           let rec aux rev_path depseq vseq = (* iterating over substructures, searching v *)
             if depseq = depth_value
             then
               if vseq = value
               then
                 let vindex = Ndseq.seq 0 (List.rev rev_path) in
                 Myseq.return vindex
               else Myseq.empty
             else
               match Ndseq.as_seq vseq with
               | Some (d, l) ->
                  let n = List.length l in
                  let* i, vi = Myseq.zip (Myseq.range 0 (n-1)) (Myseq.from_list l) in
                  aux (`Int i :: rev_path) d vi
               | None -> assert false
           in
           aux [] depth_seq vseq in
         Myseq.return (Data.make_dpat v c [|dseq; dvalue|], info)

      | _ ->
         pp_endline xp_typ t;
         pp_endline (xp_pat c (Array.map (fun _ -> fun ~html print _ -> print#string "_") gen_args)) ();
         (if info = `Null then print_endline "info = NULL");
         assert false
    
    (* model-based parsing *)
           
    let input_of_value (t : typ) (v : value) : input =
      assert (Ndseq.depth v = t.ndim);
      Ndseq.map 0
        (fun v ->
          match t.kind, v with
          | _, `Null -> `Null
          | INT CARD, `Int i -> `IntRange (i, Range.make_open 0)
          | INT INDEX, `Int i -> `IntRange (i, Range.make_open 0)
          | INT (COORD (axis,tv)), `Int i ->
             let range =
               match tv with
               | SIZE -> Range.make_closed 1 Grid.max_size
               | POS -> Range.make_closed 0 Grid.max_size
               | MOVE -> Range.make_closed (- Grid.max_size) Grid.max_size in
             `IntRange (i, range)
          | VEC tv, `Vec (i,j) ->
             let range =
               match tv with
               | SIZE -> Range.make_closed 1 Grid.max_size
               | POS -> Range.make_closed 0 Grid.max_size
               | MOVE -> Range.make_closed (- Grid.max_size) Grid.max_size in
             `Vec (`IntRange (i,range), `IntRange (j,range)) 
          | COLOR tc, `Color c -> `Color c
          | SEG, `Seg seg -> `Seg seg
          | MOTIF tmot, `Motif mot -> `Motif mot
          | GRID (filling,nocolor), `Grid g ->
             let rh = Range.make_open 1 in
             let rw = Range.make_open 1 in
             let nc = if nocolor then 1 else Grid.nb_color in
             `GridDimsCols (g, rh, rw, nc)
          | OBJ (filling,nocolor), `Obj (`Vec (i,j), `Grid g) ->
             let rh = Range.make_open 1 in
             let rw = Range.make_open 1 in
             let nc = if nocolor then 1 else Grid.nb_color in
             `Obj (`Vec (`IntRange (i, Range.make_closed 0 Grid.max_size),
                         `IntRange (j, Range.make_closed 0 Grid.max_size)),
                   `GridDimsCols (g, rh, rw, nc))
          | MAP _, `Map m ->
             let domain = mymap_keys m in
             `MapDomain (m, domain)
          | _ -> assert false)
        v

    let value_of_input t input : value =
      Ndseq.map 0
        (fun input ->
          match input with
          | `Null -> `Null
          | `IntRange (i,_) -> `Int i
          | `Vec (`IntRange (i,_), `IntRange (j,_)) -> `Vec (i,j)
          | `Color c -> `Color c
          | `Seg seg -> `Seg seg
          | `Motif mot -> `Motif mot
          | `GridDimsCols (g,rh,rw,nc) -> `Grid g
          | `Obj (`Vec (`IntRange (i,_), `IntRange (j,_)), `GridDimsCols (g1,_,_,_)) -> `Obj (`Vec (i,j), `Grid g1)
          | `MapDomain (m,dom) -> `Map m
          | _ -> assert false)
        input

    let parseur_value v input =
      let* v', input =
        if input = `Null (* for expression-only arguments *)
        then Myseq.return (v, `Null)
        else
          Ndseq.match_myseq 0
            (fun v input ->
              match v, input with
              | `Null, `Null ->
                 Myseq.return (v, `Null)
              | `Int i0, `IntRange (i,_) ->
                 if i = i0
                 then Myseq.return (v, `Null)
                 else Myseq.empty
              | `Vec (i0,j0), `Vec (`IntRange (i,_), `IntRange (j,_)) ->
                 if i = i0 && j = j0
                 then Myseq.return (v, `Null)
                 else Myseq.empty
              | `Color c0, `Color c ->
                 if c = c0
                 then Myseq.return (v, `Null)
                 else Myseq.empty
              | `Seg seg0, `Seg seg ->
                 if seg = seg0
                 then Myseq.return (v, `Null)
                 else Myseq.empty
              | `Motif mot0, `Motif mot ->
                 if mot = mot0
                 then Myseq.return (v, `Null)
                 else Myseq.empty
              | `Grid g0, `GridDimsCols (g,_,_,_) ->
                 if g = g0
                 then Myseq.return (v, `Null)
                 else Myseq.empty
              | `Obj (`Vec (i0,j0), `Grid g0),
                `Obj (`Vec (`IntRange (i,_), `IntRange (j,_)), `GridDimsCols (g, _, _, _)) ->
                 if i = i0 && j = j0 && g = g0
                 then Myseq.return (v, `Null)
                 else Myseq.empty
              | `Map m0, `MapDomain (m,dom) ->
                 if m0 = m
                 then Myseq.return (v, `Null)
                 else Myseq.empty
              | _, `Null ->
                 Myseq.return (v, `Null) (* to handle expr args *)
              | _ -> Myseq.empty)
            v input in
      Myseq.return (Data.make_dexpr v', input)

    let parseur_any t input =
      let depth = Ndseq.depth input in
      let* v, vr =
        Ndseq.map_tup_myseq ~name:"parse/any" ~depth (0,0)
          (fun input ->
            match t.kind, input with
            | _, `Null -> Myseq.empty (* useful to avoid pruning of constant expression-only arguments TODO: this is dirty *)
            | INT _, `IntRange (ij,range) ->
               Myseq.return (`Int ij, `IntRange (ij,range))
            | VEC tv, `Vec (`IntRange (i,ri), `IntRange (j,rj)) ->
               Myseq.return (`Vec (i,j), `VecRange (i,j,ri,rj))
            | COLOR tc, `Color c ->
               Myseq.return (`Color c, `ColorTyp (c,tc))
            | SEG, `Seg seg ->
               Myseq.return (`Seg seg, `Seg seg)
            | MOTIF tmot, `Motif mot ->
               Myseq.return (`Motif mot, `MotifTyp (mot,tmot))
            | GRID tg, `GridDimsCols (g,rh,rw,nc) ->
               Myseq.return (`Grid g, `GridRange (g,tg,rh,rw,nc))
            | OBJ tg, `Obj (`Vec (`IntRange (i,ri), `IntRange (j,rj)),
                            `GridDimsCols (g,rh,rw,nc)) ->
               Myseq.return (`Obj (`Vec (i,j), `Grid g),
                             `Obj (`VecRange (i,j,ri,rj), `GridRange (g,tg,rh,rw,nc)))
            | MAP (ka,kb), `MapDomain (m,dom) ->
               Myseq.return (`Map m, `MapTyp (m,ka,kb))
            | _ ->
               print_string "PARSE FAILURE in parseur_any:";
               pp xp_typ t;
               assert false)
          (tup1 input) in
      let input = Ndseq.const `Null input in
      Myseq.return (Data.make_dany v vr, input)
    
    let parseur_pat t c parse_args input =
      let depth = Ndseq.depth input in
      match t.kind, c, parse_args with
      (*      | _, _, _, `Null -> Myseq.empty (* useful to avoid pruning of constant expression-only arguments *) *)

      | _, Vec, [|parse_i; parse_j|] ->
         let v = value_of_input t input in
         let in_i, in_j =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Vec (in_i, in_j) -> in_i, in_j
              | _ -> assert false)
             (tup1 input) in
         let* di, _ = parse_i in_i in
         let* dj, _ = parse_j in_j in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|di; dj|], input)

      | _, Obj, [|parse_pos; parse_g1|] ->
         let v = value_of_input t input in
         let in_pos, in_g1 =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Obj (in_pos, in_g1) -> in_pos, in_g1
              | _ -> assert false)
             (tup1 input) in
         let* dpos, _ = parse_pos in_pos in
         let* dg1, _ = parse_g1 in_g1 in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dpos; dg1|], input)

      | MAP (ka,kb), DomMap keys, [|parse_vals|] ->
         let tb = scalar kb in (* only atomic values in maps *)
         let v = value_of_input t input in
         let* in_vals =
           Ndseq.map_myseq ~depth 1
             (function
              | `MapDomain (m,dom) ->
                 let pairs = Mymap.bindings m in
                 let m_keys = List.map fst pairs in
                 if m_keys = keys
                 then
                   let vals = List.map snd pairs in
                   Myseq.return (Ndseq.seq 0 (List.map (input_of_value tb) vals)) (* TODO: replace 0 by values-dependent expr *)
                 else Myseq.empty
              | _ -> assert false)
             input in
         let* dvals, _ = parse_vals in_vals in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dvals|], input)
    
      | MAP (ka,kb), Replace, [|parse_a; parse_b|] when ka=kb ->
         let ta = scalar ka in
         let v = value_of_input t input in
         let* in_a, in_b =
           Ndseq.map_tup_myseq ~name:"parse/Repalce/in_a_b" ~depth (0,0)
             (function
              | `MapDomain (m,dom) ->
                 let m_diff = Mymap.filter (fun a b -> a <> b) m in
                 (match Mymap.bindings m_diff with
                  | [a, b] ->
                     Myseq.return (input_of_value ta a,
                                   input_of_value ta b)
                  | _ -> Myseq.empty)
              | _ -> assert false)
             (tup1 input) in
         let* da, _ = parse_a in_a in
         let* db, _ = parse_b in_b in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|da; db|], input)
    
      | MAP (ka,kb), Swap, [|parse_a; parse_b|] when ka=kb ->
         let ta = scalar ka in
         let v = value_of_input t input in
         let* in_a, in_b =
           Ndseq.map_tup_myseq ~name:"parse/Swap/in_a_b" ~depth (0,0)
             (function
              | `MapDomain (m,dom) ->
                 let m_diff = Mymap.filter (fun a b -> a <> b) m in
                 (match Mymap.bindings m_diff with
                  | [a, b; c, d] when a=d && b=c -> Myseq.return (input_of_value ta a,
                                                                  input_of_value ta b)
                  | _ -> Myseq.empty)
              | _ -> assert false)
             (tup1 input) in
         let* da, _ = parse_a in_a in
         let* db, _ = parse_b in_b in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|da; db|], input)
    
      | _, BgColor, [|parse_col; parse_g1|] ->
         let v = value_of_input t input in
         let* in_col, in_g1 =
           Ndseq.map_tup_myseq ~name:"parse/BgColor/in_col_g1" ~depth (0,0)
             (function
              | `GridDimsCols (g,rh,rw,nc) ->
                 if Grid.is_full g
                 then
                   let* bc = Myseq.from_list (Segment.background_colors g) in
                   let* g1 = Myseq.from_result (Grid.Transf.swap_colors g bc Grid.transparent) in
                   let nc1 = if nc > 1 && g.Grid.color_count.(bc) > 0 then nc-1 else nc in
                   Myseq.return (`Color bc,
                                 `GridDimsCols (g1,rh,rw,nc1))
                 else Myseq.empty
              | _ -> assert false)
             (tup1 input) in
         let* dcol, _ = parse_col in_col in
         let* dg1, _ = parse_g1 in_g1 in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dcol; dg1|], input)

      | _, IsFull, [|parse_g1|] ->
         let* dg1, _ = parse_g1 input in
         let* v =
           Ndseq.map_myseq ~depth 0
             (function
              | `Grid g1 ->
                 if Grid.is_full g1
                 then Myseq.return (`Grid g1)
                 else Myseq.empty
              | _ -> assert false)
             (Data.value dg1) in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dg1|], input)
    
      | _, Crop, [|parse_g; parse_pos; parse_size|] ->
         let v = value_of_input t input in
         let depth = Ndseq.depth v in
         let* dg, _ = parse_g `Null in (* expression *)
         let vg = Data.value dg in
         let* () = Myseq.from_bool (Ndseq.depth vg = depth) in
         let in_size =
           Ndseq.map ~depth 0
             (function
              | `GridDimsCols (g1,rh1,rw1,nc1) ->
                 let h1, w1 = Grid.dims g1 in
                 `Vec (`IntRange (h1, rh1),
                       `IntRange (w1, rw1))
              | _ -> assert false)
             input in
         let* dsize, _ = parse_size in_size in
         let* in_pos =
           try
             Ndseq.map_tup_myseq ~name:"parse/Crop/in_pos" ~depth 0
               (function
                | `Grid g1, `Grid g, `Vec (h1,w1) ->
                   let h1, w1 = Grid.dims g1 in
                   let h, w = Grid.dims g in
                   let* i, j = Myseq.from_list (Grid_patterns.parse_crop g g1) in
                   Myseq.return
                     (`Vec (`IntRange (i, Range.make_closed 0 (h-h1)),
                            `IntRange (j, Range.make_closed 0 (w-w1))))
                | _, `Null, _ -> Myseq.empty (* failed computation for source grid *)
                | _ -> assert false)
               (v, vg, Data.value dsize)
           with Invalid_argument _ -> Myseq.empty in (* dg may have an inconsistent structure *)
         let* dpos, _ = parse_pos in_pos in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dg; dpos; dsize|], input)
    
      | _, Objects (nmax,mode), [|parse_size; parse_seg; parse_card; parse_objs; _parse_merger|] ->
         let v = value_of_input t input in
         let in_size =
           Ndseq.map ~depth 0
             (function
              | `GridDimsCols (g,rh,rw,nc) ->
                 let h, w = Grid.dims g in
                 `Vec (`IntRange (h, rh),
                       `IntRange (w, rw))
              | _ -> assert false)
             input in
         let* dsize, _ = parse_size in_size in
         let* in_seg =
           let* seg = (* common choice for all sequence items *)
             Myseq.from_list
               (match mode with
                | `Connected -> GPat.Objects.candidate_segmentations_connected
                | `SameColor -> [GPat.Objects.SameColor]) in
           Myseq.return
             (Ndseq.map ~depth 0
                (function
                 | `GridDimsCols (g,rh,rw,nc) -> `Seg seg
                 | _ -> assert false)
                input) in
         let* dseg, _ = parse_seg in_seg in
         let* in_card, in_objs =
           Ndseq.map_tup_myseq ~name:"parse/Objects/in_objs" ~depth (0,1)
             (function
              | `GridDimsCols (g,rh,rw,nc), `Seg seg, `Vec (h,w) ->
                 let nc1 = nc in
                 (* PB: not robust segmentation choice, and makes monocolor non-compresive
                   match seg with
                   | GPat.Objects.Connected (_,true) | GPat.Objects.SameColor -> 1
                   | _ -> nc in *)
                 let* objs = GPat.Objects.parse seg g in
                 let card = List.length objs in
                 let* () = Myseq.from_bool (card <= nmax) in
                 let* objs = (* permutations of first three objects *)
                   match objs with
                   | [] -> Myseq.return objs
                   | [o1] -> Myseq.return objs
                   | [o1;o2] -> Myseq.cons objs (Myseq.return [o2;o1])
                   | o1::o2::o3::os ->
                      Myseq.cons objs
                        (Myseq.cons (o1::o3::o2::os)
                           (Myseq.cons (o2::o1::o3::os)
                              (Myseq.cons (o2::o3::o1::os)
                                 (Myseq.cons (o3::o2::o1::os)
                                    (Myseq.return (o3::o1::o2::os)))))) in
                 Myseq.return
                   (`IntRange (card, Range.make_closed 0 nmax),
                    Ndseq.seq 0
                      (List.map
                         (fun (i,j,g1) ->
                           `Obj (`Vec (`IntRange (i, Range.make_closed 0 (h-1)),
                                       `IntRange (j, Range.make_closed 0 (w-1))),
                                 `GridDimsCols (g1,
                                                Range.make_closed 1 (h-i),
                                                Range.make_closed 1 (w-j),
                                                nc1)))
                         objs))
              | _ -> assert false)
             (input, Data.value dseg, Data.value dsize) in
         let* dcard, _ = parse_card in_card in
         let* dobjs, _ = parse_objs in_objs in
         let _v, dmerger = make_objects_v_dmerger dsize dseg dcard dobjs in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dsize; dseg; dcard; dobjs; dmerger|], input)
    
(*      | _, ColorPartition, [|parse_size; parse_grids|], `GridDimsCols (g,rh,rw,nc) ->
         let h, w = Grid.dims g in
         let rh1 = Range.make_exact h in
         let rw1 = Range.make_exact w in
         let* dsize, _ = parse_size
                           (`Vec (`IntRange (h, rh),
                                  `IntRange (w, rw))) in
         let lg1s = Grid_patterns.partition_by_color g in
         let* () = Myseq.from_bool (lg1s <> []) in
         let g1s =
           List.map
             (fun g1 -> `GridDimsCols (g1,rh1,rw1,nc)) (* h/w known, keeping nc>1 for supporting Monocolor *)
             lg1s in
         let* dgrids, _ = parse_grids (`Seq g1s) in
         let* data = Myseq.from_result (make_dcolorpartition dsize dgrids) in
         Myseq.return (data, `Null) *)
    
      | _, Monocolor, [|parse_col; parse_mask|] ->
         let v = value_of_input t input in
         let* in_col, in_mask =
           Ndseq.map_tup_myseq ~name:"parse/Monocolor/in_col_mask" ~depth (0,0)
             (function
              | `GridDimsCols (g,rh,rw,nc) ->
                 if Grid.color_count Grid.transparent g = 1
                 then
                   let* c = Myseq.from_result (Grid.majority_color Grid.transparent g) in
                   let* mask = Myseq.from_result (Grid.Transf.swap_colors g c Grid.Mask.one) in
                   Myseq.return (`Color c, `GridDimsCols (mask,rh,rw,1))
                 else Myseq.empty
              | _ -> assert false)
             (tup1 input) in
         let* dcol, _ = parse_col in_col in
         let* dmask, _ = parse_mask in_mask in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dcol; dmask|], input)

      | _, Recoloring, [|parse_grid; parse_map|] ->
         let v = value_of_input t input in
         let depth = Ndseq.depth v in
         let* dg1, _ = parse_grid `Null in (* expression expected *)
         let vg1 = Data.value dg1 in
         let* () = Myseq.from_bool (Ndseq.depth vg1 = depth) in
         let* in_map =
           try
             Ndseq.map_tup_myseq ~depth 0
               (function
                | `GridDimsCols (g,rh,rw,nc), `Grid g1 ->
                   (match Grid_patterns.parse_recoloring g g1 with
                    | Some mcol ->
                       let m =
                         Mymap.fold
                           (fun c1 c2 res ->
                             Mymap.add (`Color c1) (`Color c2) res)
                           mcol (Mymap.empty : (value,value) Mymap.t) in
                       let dom = mymap_keys m in
                       Myseq.return (`MapDomain (m,dom))
                    | None -> Myseq.empty)
                | input, vg1 ->
                   pp_endline xp_input input;
                   pp_endline xp_value vg1;
                   assert false)
               (input, vg1)
           with Invalid_argument _ -> Myseq.empty in (* dg1 is not guaranteed to have a consistent structure *)
         let* dmap, _ = parse_map in_map in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dg1; dmap|], input)
    
      | _, MotifMulti partial, [|parse_mot; parse_core; _parse_pure; parse_mask_opt; parse_noise|] ->
         let v = value_of_input t input in
         let g_bgcolor = if partial then Grid.transparent else Grid.undefined in
         let* in_mot, in_core, in_mask_opt, in_noise =
           Ndseq.map_tup_myseq ~name:"parse/Motif/in_res" ~depth (0,0,0,0)
             (function
              | `GridDimsCols (g,rh,rw,nc) ->
                 let* mot, ru, rv, g_core, mask_opt, g_noise =
                   Myseq.from_list (GPat.Motif.from_grid GPat.Motif.candidates_multi g_bgcolor g) in
                 assert (Grid.has_valid_size g_core); (* to make sure oversized grids are pruned out *)
                 Myseq.return
                   (`Motif mot,
                    `GridDimsCols (g_core,ru,rv,nc),
                    (match partial, mask_opt with
                     | true, Some mask ->
                        let h, w = Grid.dims mask in (* same as grid and noise *)
                        let rh, rw = Range.make_exact h, Range.make_exact w in (* already encoded in noise *) 
                        `GridDimsCols (mask,rh,rw,1)
                     | _ -> `Null), (* TODO: revise handling of optional, ugly *)
                    `GridDimsCols (g_noise,rh,rw,nc))
              | _ -> assert false)
             (tup1 input) in
         let* dmot, _ = parse_mot in_mot in
         let* dcore, _ = parse_core in_core in
         let* dmask_opt, _ =
           if not partial || Ndseq.for_all (fun x -> x <> `Null) in_mask_opt
           then parse_mask_opt in_mask_opt
           else Myseq.empty in
         let* dnoise, _ = parse_noise in_noise in
         let input = Ndseq.const `Null input in
         let* dpure = make_motif_multi_dpure dmot dcore dnoise in
         Myseq.return (Data.make_dpat v c [|dmot; dcore; dpure; dmask_opt; dnoise|], input)
    
      | _, MotifBi partial, [|parse_mot; parse_bgcolor; parse_color; _parse_pure; parse_mask_opt; parse_noise|] ->
         let v = value_of_input t input in
         let g_bgcolor = if partial then Grid.transparent else Grid.undefined in
         let* in_mot, in_bgcolor, in_color, in_mask_opt, in_noise =
           Ndseq.map_tup_myseq ~name:"parse/MotifBi/in_res" ~depth (0,0,0,0,0)
             (function
              | `GridDimsCols (g,rh,rw,nc) ->
                 let* mot, _ru, _rv, g_core, mask_opt, g_noise =
                   Myseq.from_list (GPat.Motif.from_grid GPat.Motif.candidates_bi g_bgcolor g) in
                 assert (Grid.dims g_core = (2,1));
                 let bgcolor = Grid.get_pixel ~source:"parse MotifBi bgcolor" g_core 0 0 in
                 let color = Grid.get_pixel ~source:"parse MotifBi color" g_core 1 0 in
                 let* () = Myseq.from_bool (color <> Grid.transparent) in
                 Myseq.return
                   (`Motif mot,
                    `Color bgcolor,
                    `Color color,
                    (match partial, mask_opt with
                     | true, Some mask -> `GridDimsCols (mask,rh,rw,1)
                     | _ -> `Null), (* TODO: revise handling of optional, ugly *)
                    `GridDimsCols (g_noise,rh,rw,nc))
              | _ -> assert false)
             (tup1 input) in
         let* dmot, _ = parse_mot in_mot in
         let* dbgcolor, _ = parse_bgcolor in_bgcolor in
         let* dcolor, _ = parse_color in_color in
         let* dmask_opt, _ =
           if not partial || Ndseq.for_all (fun x -> x <> `Null) in_mask_opt
           then parse_mask_opt in_mask_opt
           else Myseq.empty in
         let* dnoise, _ = parse_noise in_noise in
         let input = Ndseq.const `Null input in
         let* dpure = make_motif_bi_dpure dmot dbgcolor dcolor dnoise in
         Myseq.return (Data.make_dpat v c [|dmot; dbgcolor; dcolor; dpure; dmask_opt; dnoise|], input)
    
      | _, Metagrid, [|parse_sepcolor; parse_borders; parse_dims; parse_heights; parse_widths; parse_gridss|] ->
         let v = value_of_input t input in
         let make_input_dim kl rhw =
           let r = (* do not use kl to define r *)
             match rhw with
             | Range.Closed (a,b) -> Range.make_closed 1 ((b+1) / 2)
             | Range.Open a -> Range.make_open 1 in
           assert (Range.mem kl r);
           `IntRange (kl,r)
         and make_input_sizes start stop kl rhw sizes =
           let r, l =
             let init_range = Range.sub rhw (Range.make_exact (start + kl-1 + stop + kl)) in (* minus frontiers, and minus at least 1 for each size *)
             Array.fold_right
               (fun hw1 (r,xs) ->
                 let r1 = (* do not use hw1 to define r1 *)
                   match Range.upper r with
                   | None -> Range.make_open 1
                   | Some b -> Range.make_closed 1 (1+b) in
                 assert (Range.mem hw1 r1);
                 Range.sub r (Range.make_exact (hw1-1)), (* minus excess of current size viz default 1 *)
                 `IntRange (hw1,r1)::xs)
               sizes (init_range, []) in
           assert (Range.mem 0 r);
           Ndseq.seq 0 l
         in
         let* in_sepcolor, in_borders, in_dims, in_heights, in_widths, in_gridss =
           Ndseq.map_tup_myseq ~name:"parse/Metagrid/in_res" ~depth (0,0,0,1,1,2)
             (function
              | `GridDimsCols (g,rh,rw,nc) ->
                 let h, w = Grid.dims g in
                 assert (Range.mem h rh);
                 assert (Range.mem w rw);
                 let* mg : GPat.Metagrid.t = Myseq.from_list (GPat.Metagrid.parse g) in
                 let k, l = mg.k, mg.l in
                 let* () = Myseq.from_bool (k > 1 || l > 1) in (* avoiding degenerate metagrids *)
                 let top, bot, left, right =
                   let b = mg.borders.matrix in
                   let offset c = if c = Grid.Mask.one then 1 else 0 in
                   offset b.{0,0}, offset b.{0,1},
                   offset b.{1,0}, offset b.{1,1} in
                 Myseq.return
                   (`Color mg.sepcolor,
                    `GridDimsCols (mg.borders,
                                   Range.make_exact 2,
                                   Range.make_exact 2,
                                   1),
                    `Vec (make_input_dim k rh,
                          make_input_dim l rw),
                    make_input_sizes top bot k rh mg.part_heights,
                    make_input_sizes left right l rw mg.part_widths,
                    Ndseq.seq 1
                      (Array.to_list
                         (Array.map
                            (fun row ->
                              Ndseq.seq 0
                                (Array.to_list
                                   (Array.map
                                      (fun g1 ->
                                        (* dims are known from heigths and widths *)
                                        let h1, w1 = Grid.dims g1 in
                                        let rh1 = Range.make_exact h1 in
                                        let rw1 = Range.make_exact w1 in
                                        `GridDimsCols (g1, rh1, rw1, nc))
                                      row)))
                            mg.parts)))
              | _ -> assert false)
             (tup1 input) in
         let* dsepcolor, _ = parse_sepcolor in_sepcolor in
         let* dborders, _ = parse_borders in_borders in
         let* ddims, _ = parse_dims in_dims in
         let* dheights, _ = parse_heights in_heights in
         let* dwidths, _ = parse_widths in_widths in
         let* dgridss, _ = parse_gridss in_gridss in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dsepcolor; dborders; ddims; dheights; dwidths; dgridss|], input)

(*      | _, Repeat, [|parse_grid; parse_nis; parse_njs|], `GridDimsCols (g,rh,rw,nc) ->
         let rec aux_inputs min max_opt = function
           | [] -> []
           | [n] ->
              assert (n >= min);
              let r =
                match max_opt with
                | None -> Range.make_open min
                | Some max -> Range.make_closed min max in
              assert (Range.mem n r);
              [`IntRange (n, r)]
           | n::l ->
              let r =
                match max_opt with
                | None -> Range.make_open 1
                | Some max -> Range.make_closed 1 max in
              assert (Range.mem n r);
              `IntRange (n, r)
              :: aux_inputs (min - n) (Option.map (fun max -> (max - n + 1)) max_opt) l
         in
         let* g1, nis, njs = Myseq.from_option (Grid_patterns.parse_repeat g) in
         let h1, w1 = Grid.dims g1 in
         let min_h, max_h_opt = Range.lower rh, Range.upper rh in
         let min_w, max_w_opt = Range.lower rw, Range.upper rw in
         let* dnis, _ =
           let min = min_h in
           let max_opt = Option.map (fun m -> m - h1 + 1) max_h_opt in
           parse_nis (`Seq (aux_inputs min max_opt nis)) in
         let* dnjs, _ =
           let min = min_w in
           let max_opt = Option.map (fun m -> m - w1 + 1) max_w_opt in
           parse_njs (`Seq (aux_inputs min max_opt njs)) in
         let* dgrid, _ =
           let rh1 = Range.make_exact h1 in (* encoded as sequence length of nis *)
           let rw1 = Range.make_exact w1 in (* encoded as sequence length of njs *)
           parse_grid (`GridDimsCols (g1,rh1,rw1,nc)) in
         let* data = Myseq.from_result (make_drepeat dgrid dnis dnjs) in
         Myseq.return (data, `Null) *)
    
      | _, (Empty | Full as c), [|parse_size|] ->
         let v = value_of_input t input in
         let pred =
           match c with
           | Empty -> (fun i j c -> c = Grid.Mask.zero)
           | Full -> (fun i j c -> c = Grid.Mask.one)
           | _ -> assert false
         in
         let* in_size =
           Ndseq.map_myseq ~depth 0
             (function
              | `GridDimsCols (mask,rh,rw,nc) -> (* nc = 1 *)
                 let h, w = Grid.dims mask in
                 if Grid.for_all_pixels pred mask
                 then Myseq.return (`Vec (`IntRange (h, rh),
                                          `IntRange (w, rw)))
                 else Myseq.empty
              | _ -> assert false)
             input in
         let* dsize, _ = parse_size in_size in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dsize|], input)

      | _, Point, [||] ->
         let v = value_of_input t input in
         if Ndseq.for_all ~depth
              (function
               | `GridDimsCols (mask,rh,rw,nc) -> (* nc=1 *)
                  let h, w = Grid.dims mask in
                  h=1 && w=1 && Grid.Mask.mem 0 0 mask
               | _ -> assert false)
              input
         then
           let input = Ndseq.const `Null input in
           Myseq.return (Data.make_dpat v c [||], input)
         else Myseq.empty
    
      | _, Line, [|parse_len; parse_dir|] ->
         let v = value_of_input t input in
         let* in_len, in_dir =
           Ndseq.map_tup_myseq ~name:"parse/Line/in_res" ~depth (0,0)
             (function
              | `GridDimsCols (mask,rh,rw,nc) -> (* nc = 1 *)
                 (match GPat.parse_line mask with
                  | Some (len, (di,dj)) ->
                     Myseq.return
                       (`IntRange (len, Range.union rh rw),
                        `Vec (`IntRange (di, Range.make_closed 0 1),
                              `IntRange (dj, Range.make_closed (-1) 1)))
                  | None -> Myseq.empty)
              | _ -> assert false)
             (tup1 input) in
         let* dlen, _ = parse_len in_len in
         let* ddir, _ = parse_dir in_dir in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dlen; ddir|], input)

      | _, ColorSeq dir, [|parse_size; parse_colors|] ->
         let v = value_of_input t input in
         let* in_size, in_colors =
           Ndseq.map_tup_myseq ~name:"parse/ColorSeq/in_res" ~depth (0,1)
             (function
              | `GridDimsCols (g,rh,rw,nc) ->
                 let h, w = Grid.dims g in
                 (match dir with
                  | `H ->
                     let* () = Myseq.from_bool (h = 1 && w > 1) in
                     Myseq.return
                       (`IntRange (w, Range.inter rw (Range.make_open 2)),
                        Ndseq.seq 0
                          (List.init w (fun j ->
                               `Color (Grid.get_pixel g 0 j))))
                  | `V ->
                     let* () = Myseq.from_bool (w = 1 && h > 1) in
                     Myseq.return
                       (`IntRange (h, Range.inter rh (Range.make_open 2)),
                        Ndseq.seq 0
                          (List.init h (fun i ->
                               `Color (Grid.get_pixel g i 0)))))
              | _ -> assert false)
             (tup1 input) in
         let* dsize, _ = parse_size in_size in
         let* dcolors, _ = parse_colors in_colors in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dsize; dcolors|], input)
    
      | _, ColorMat, [|parse_size; parse_colorss|] ->
         let v = value_of_input t input in
         let* in_size, in_colorss =
           Ndseq.map_tup_myseq ~name:"parse/ColorMat/in_res" ~depth (0,2)
             (function
              | `GridDimsCols (g,rh,rw,nc) ->
                 let h, w = Grid.dims g in
                 let* () = Myseq.from_bool (h > 1 && h <= 3 && w > 1 && w <= 3) in
                 Myseq.return
                   (`Vec (`IntRange (h, Range.inter rh (Range.make_closed 2 3)),
                          `IntRange (w, Range.inter rw (Range.make_closed 2 3))),
                    Ndseq.seq 1
                      (List.init h (fun i ->
                           Ndseq.seq 0
                             (List.init w (fun j ->
                                  `Color (Grid.get_pixel g i j))))))
              | _ -> assert false)
             (tup1 input) in
         let* dsize, _ = parse_size in_size in
         let* dcolorss, _ = parse_colorss in_colorss in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dsize; dcolorss|], input)

      | _, SeqCons dep, [|parse_hd; parse_tl|] ->
         if Ndseq.is_complete ~depth:dep input
         then
           let* xhd = Myseq.from_option (Ndseq.head ~depth:dep input) in
           let* xtl = Myseq.from_option (Ndseq.tail ~depth:dep input) in
           let* dhd, xhd = parse_hd xhd in
           let* dtl, xtl = parse_tl xtl in
           let v : value = Ndseq.cons ~depth:dep (Data.value dhd) (Data.value dtl) in
           let x : input = Ndseq.cons ~depth:dep xhd xtl in
           Myseq.return (Data.make_dpat v c [|dhd;dtl|], x)
         else parseur_any t input

      | _, SeqRepeat dep, [|parse_e|] ->
         assert (dep < depth);
         if Ndseq.is_complete ~depth:dep input
         then
           let* xe =
             Ndseq.map_myseq ~depth:dep (-1)
               (fun input ->
                 match Ndseq.as_seq input with
                 | Some (d, []) -> Myseq.empty
                 | Some (d, x::l1) ->
                    (try
                      let v : value = value_of_input t x in
                      if List.for_all (fun x1 -> value_of_input t x1 = v) l1 (* all elts should be the same value *)
                      then Myseq.return x
                      else Myseq.empty
                     with _ -> Myseq.empty)
                 | None -> assert false)
               input in
           let* de, xe = parse_e xe in
           let v, x =
             Ndseq.map_tup ~depth:dep (depth - dep, depth - dep)
               (fun (ve,xe,input) ->
                 match Ndseq.as_seq input with
                 | Some (d,l) ->
                    Ndseq.seq d (List.map (fun _ -> ve) l),
                    Ndseq.seq d (List.map (fun _ -> xe) l)
                 | _ -> assert false)
               (Data.value de, xe, input) in
           Myseq.return (Data.make_dpat v c [|de|], x)
         else parseur_any t input (* TODO: improve by using parse_e ? *)

      | _, SeqRange, [|parse_start; parse_step|] ->
         let dep = depth - 1 in
         assert (dep >= 0);
         if Ndseq.is_complete ~depth:dep input
         then
           let* in_start, in_step =
             Ndseq.map_tup_myseq ~depth:dep (0,0)
               (fun input ->
                 match Ndseq.as_seq input with
                 | Some (_,l) ->
                    let lint =
                      List.map
                        (function
                         | `IntRange (x,_) -> x
                         | _ -> assert false)
                        l in
                    (match l with
                     | `IntRange (x0,r0)::`IntRange (x1,r1)::_ ->
                        let step = x1 - x0 in
                        let* () = Myseq.from_bool (lint = List.mapi (fun i _ -> x0 + i * step) l) in
                        let* range_step = (* TODO: ambiguity with ranges including negative values and Range.sub *)
                          match r0, r1 with
                          | Range.Closed (a0,b0), Range.Closed (a1,b1) ->
                             Myseq.return (Range.Closed (a1 - b0, b1 - a0))
                          | Range.Closed (a0,b0), Range.Open a1 ->
                             Myseq.return (Range.Open (a1 - b0))
                          | Range.Open a0, _ -> Myseq.empty in
                        Myseq.return (`IntRange (x0, r0),
                                      `IntRange (step, range_step))
                     | _ -> Myseq.empty)
                 | _ -> assert false)
               (tup1 input) in
           let* dstart, _ = parse_start in_start in
           let* dstep, _ = parse_step in_step in
           let v = value_of_input t input in
           let input = Ndseq.const `Null input in
           Myseq.return (Data.make_dpat v c [|dstart; dstep|], input)
         else parseur_any t input

      | _, SeqIndex, [|parse_seq; parse_index|] ->
         let v = value_of_input t input in
         let* dseq, _ = parse_seq `Null in (* expression only *)
         let vseq = Data.value dseq in
         let depth_seq = Ndseq.depth vseq in
         let* () = Myseq.from_bool (depth < depth_seq) in (* v must be an element or proper substructure of vseq *)
         let* in_index =
           let rec aux rev_path depseq vseq = (* iterating over substructures, searching v *)
             if depseq = depth
             then
               if vseq = v
               then
                 let in_index = Ndseq.seq 0 (List.rev rev_path) in
                 Myseq.return in_index
               else Myseq.empty
             else
               match Ndseq.as_seq vseq with
               | Some (d, l) ->
                  if l = []
                  then Myseq.empty
                  else
                    let n = List.length l in
                    let range = Range.make_closed 0 (n-1) in
                    let* i, vi = Myseq.zip (Myseq.range 0 (n-1)) (Myseq.from_list l) in
                    aux (`IntRange (i, range) :: rev_path) d vi
               | None -> assert false
           in
           aux [] depth_seq vseq in
         let* dindex, _ = parse_index in_index in
         let input = Ndseq.const `Null input in
         Myseq.return (Data.make_dpat v c [|dseq; dindex|], input)

      | _, SeqIndexOf kvalue, [|parse_seq; parse_value|] ->
         let v = value_of_input t input in
         let* dseq, _ = parse_seq `Null in (* expression only *)
         let vseq = Data.value dseq in
         let index : int option list =
           match Ndseq.as_seq v with
           | Some (0, l) ->
              List.map
                (function
                 | `Int i -> Some i
                 | _ -> assert false)
                l
           | _ -> assert false in
         let len = List.length index in
         let* () = Myseq.from_bool (len > 0 && len <= Ndseq.depth vseq) in
         (match Ndseq.index_list vseq index with
          | Some value ->
             let tvalue = scalar kvalue in
             let in_value = input_of_value tvalue value in
             let* dvalue, _ = parse_value in_value in
             let input = Ndseq.const `Null input in
             Myseq.return (Data.make_dpat v c [|dseq; dvalue|], input)
          | None -> Myseq.empty)
    
      | _ -> assert false
    

    (* description length *)

    let dl_color (c : Grid.color) (tc : typ_color) : dl =
      (* Mdl.Code.uniform Grid.nb_color *)
      if c = Grid.undefined then 0. (* no information on color *)
      else
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

    let dl_seg (seg : GPat.Objects.segmentation) : dl =
      Mdl.Code.uniform GPat.Objects.nb_candidate_segmentations_connected
         
    let dl_motif (tmot : typ_motif) (m : GPat.Motif.t) : dl =
      match tmot with
      | MULTI ->
         (* Mdl.Code.uniform GPat.Motif.nb_candidates_multi *)
         Mdl.Code.usage (GPat.Motif.prob_multi m)
      | BI -> Mdl.Code.uniform GPat.Motif.nb_candidates_bi
         
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
      let n = Mymap.cardinal m in
      Mdl.Code.universal_int_star n
      +. Mymap.fold
           (fun a b res -> dl_a a +. dl_b b +. res)
           m 0.

    let rec dl_value t v =
      let k = t.kind in
      Ndseq.fold_left
        (fun dl v -> dl +. dl_value_scalar k v)
        0. v
    and dl_value_scalar k v = (* on scalars *)
      match k, v with
      | _, `Null -> 0. (* for optional parts *)
      | BOOL, `Bool b -> 1.
      | INT (CARD | INDEX), `Int i ->
         if i >= 0
         then Mdl.Code.universal_int_star i
         else (print_int i; assert false)
      | INT (COORD (axis,tv)), `Int ij ->
         (match tv with
          | POS ->
             if ij >= 0 && ij < Grid.max_size
             then Range.dl ij (Range.make_closed 0 (Grid.max_size-1))
             else (print_int ij; assert false)
          | SIZE ->
             if ij > 0
             then Mdl.Code.universal_int_plus ij
             else (print_int ij; assert false)
          | MOVE ->
             1. +. Mdl.Code.universal_int_star (abs ij))
      | VEC tv, `Vec (i,j) ->
         dl_value_scalar (INT (COORD (I,tv))) (`Int i)
         +. dl_value_scalar (INT (COORD (J,tv))) (`Int j)
      | COLOR tc, `Color c -> dl_color c tc
      | SEG, `Seg seg -> dl_seg seg
      | MOTIF tmot, `Motif m -> dl_motif tmot m
      | GRID tg, `Grid g ->
         let rmax = Range.make_closed 1 Grid.max_size in
         dl_grid g tg rmax rmax Grid.nb_color
      | OBJ tg, `Obj (`Vec (i,j), `Grid g) ->
         dl_value_scalar (INT (COORD (I, POS))) (`Int i)
         +. dl_value_scalar (INT (COORD (J, POS))) (`Int j)
         +. dl_value_scalar (GRID (`Sprite,false)) (`Grid g)
      | MAP (ka,kb), `Map m ->
         dl_map (dl_value_scalar ka) (dl_value_scalar kb) m
      | _ -> pp xp_value v; assert false

    let encoding_dany vr =
      let rec aux = function
        | `IntRange (ij,range) -> Range.dl ij range
        | `VecRange (i,j,ri,rj) -> Range.dl i ri +. Range.dl j rj
        | `ColorTyp (c,tc) -> dl_color c tc
        | `Seg seg -> dl_seg seg
        | `MotifTyp (m,tm) -> dl_motif tm m
        | `GridRange (g,tg,rh,rw,nc) -> dl_grid g tg rh rw nc
        | `Obj (pos,g1) -> aux pos +. aux g1
        | `MapTyp (m,ka,kb) -> dl_map (dl_value_scalar ka) (dl_value_scalar kb) m
        | _ -> assert false
      in
      Ndseq.fold_left (fun dl vr -> dl +. aux vr) 0. vr
    
    let encoding_dpat dc encs =
      match dc, encs with
      | Vec, [|enc_i; enc_j|] ->  enc_i +. enc_j
      | Obj, [|enc_pos; enc_g1|] -> enc_pos +. enc_g1
      | DomMap keys, [|enc_vals|] -> enc_vals (* keys encoded in model *)
      | Replace, [|enc_a; enc_b|] -> enc_a +. enc_b
      | Swap, [|enc_a; enc_b|] -> enc_a +. enc_b
      | BgColor, [|enc_col; enc_g1|] -> enc_col +. enc_g1
      | IsFull, [|enc_g1|] -> enc_g1
      | Crop, [|enc_g; enc_pos; enc_size|] -> assert (enc_g = 0.); enc_g +. enc_pos +. enc_size
      | Objects (nmax,mode), [|enc_size; enc_seg; enc_card; enc_objs; _enc_merger|] -> enc_size +. enc_seg +. enc_card +. enc_objs (* TODO: take seg into account for encoding objects *)
      | ColorPartition, [|enc_size; enc_grids|] -> enc_size +. enc_grids
      | Monocolor, [|enc_col; enc_mask|] -> enc_col +. enc_mask
      | Recoloring, [|enc_grid; enc_map|] -> assert (enc_grid = 0.); enc_grid +. enc_map
      | MotifMulti partial, [|enc_motif; enc_core; _enc_pure; enc_mask_opt; enc_noise|] ->
         enc_motif +. enc_core +. enc_mask_opt +. enc_noise
      | MotifBi partial, [|enc_motif; enc_bgcolor; enc_color; _enc_pure; enc_mask_opt; enc_noise|] ->
         enc_motif +. enc_bgcolor +. enc_color +. enc_mask_opt +. enc_noise
      | Metagrid, [|enc_sepcolor; enc_borders; enc_dims; enc_heights; enc_widths; enc_gridss|] ->
         enc_sepcolor +. enc_borders +. enc_dims +. enc_heights +. enc_widths +. enc_gridss
      | Repeat, [|enc_grid; enc_nis; enc_njs|] -> enc_grid +. enc_nis +. enc_njs
      | Empty, [|enc_size|] -> enc_size
      | Full, [|enc_size|] -> enc_size
      | Point, [||] -> 0.
      | Line, [|enc_len; enc_dir|] -> enc_len +. enc_dir
      | ColorSeq dir, [|enc_size; enc_colors|] -> enc_size +. enc_colors
      | ColorMat, [|enc_size; enc_colorss|] -> enc_size +. enc_colorss
      | SeqCons depth, [|enc_hd; enc_tl|] -> enc_hd +. enc_tl
      | SeqRepeat depth, [|enc_e|] -> enc_e
      | SeqRange, [|enc_start; enc_step|] -> enc_start +. enc_step
      | SeqIndex, [|enc_seq; enc_index|] -> assert (enc_seq = 0.); enc_seq +. enc_index
      | SeqIndexOf _, [|enc_seq; enc_value|] -> assert (enc_seq = 0.); enc_seq +. enc_value
      | _ -> assert false
    let encoding_alt dl_choice enc = dl_choice +. enc
    let encoding_expr_value v = 0.
    let dl_of_encoding enc = enc
           
    let dl_var ~nb_env_vars t p =
      let k = max 1 nb_env_vars in (* to avoid 0, happens in pruning mode *)
      Mdl.Code.uniform k

    let dl_constr_params t c =
      match c with
      | Vec -> 0.
      | Obj -> 0.
      | DomMap keys -> (* 0. (* assuming keys derived from context pattern/data *) *)
         (match t.kind with
          | MAP (ka,kb) ->
             Mdl.Code.universal_int_star (List.length keys)
             +. List.fold_left
                  (fun res a -> dl_value_scalar ka a)
                  0. keys
          | _ -> assert false)
      | Replace -> 0.
      | Swap -> 0.
      | BgColor -> 0.
      | IsFull -> 0.
      | Crop -> 0.
      | Objects (nmax,mode) ->
         (*Mdl.Code.usage
           (match seg with
            | `Connected -> 0.33
            | `ConnectedSameColor -> 0.33
            | `SameColor -> 0.33)
         +. *) 1. +. Mdl.Code.universal_int_plus nmax
      | ColorPartition -> 0.
      | Monocolor -> 0.
      | Recoloring -> 0.
      | MotifMulti partial -> 1.
      | MotifBi partial -> 1.
      | Metagrid -> 0.
      | Repeat -> 0.
      | Empty -> 0.
      | Full -> 0.
      | Point -> 0.
      | Line -> 0.
      | ColorSeq dir -> 1. (* encoding direction *)
      | ColorMat -> 0.
      | SeqCons depth -> Mdl.Code.universal_int_star depth
      | SeqRepeat depth -> Mdl.Code.universal_int_star depth
      | SeqRange -> 0.
      | SeqIndex -> 0.
      | SeqIndexOf tvalue -> 0. (* tvalue information available from first argument *)

    let dl_periodicity_mode : Grid.Transf.periodicity_mode -> dl = function
      | `Total -> Mdl.Code.usage 0.25
      | `Strict -> Mdl.Code.usage 0.25
      | `TradeOff -> Mdl.Code.usage 0.5

    let dl_cast_kind k k' =
      (* encoding k' given k *)
      match k with
      | INT CARD ->
         (match k' with
          | INT INDEX -> Mdl.Code.usage 0.5
          | INT (COORD (axis,tv)) -> Mdl.Code.usage 0.5 +. Mdl.Code.uniform 2 (* axis *) +. Mdl.Code.uniform 3 (* tv *)
          | _ -> assert false)
      | COLOR C_OBJ ->
         (match k' with
          | COLOR (C_BG full) -> 1. (* encoding full *)
          | _ -> assert false)
      | COLOR (C_BG true) ->
         (match k' with
          | COLOR C_OBJ -> Mdl.Code.usage 0.4
          | COLOR (C_BG false) -> Mdl.Code.usage 0.6
          | _ -> assert false)
      | GRID (filling,nocolor) ->
         (match k' with
          | GRID (filling', nocolor') when filling' <> filling && nocolor' = nocolor -> 1. (* one of the two other fillings *)
          | _ -> assert false)
      | OBJ (filling,nocolor) ->
         (match k' with
          | OBJ (filling', nocolor') when filling' <> filling && nocolor' = nocolor -> 1.
          | _ -> assert false)
      | _ -> assert false
    
    let dl_func_params (t : typ) : func -> dl = function
      | `Cast_1 (k,k') -> dl_cast_kind k k'
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
      | `Tail_1 -> 0.
      | `Reverse_1 -> 0.
      | `Rotate_1 shift ->
         assert (shift <> 0);
         1. (* sign *) +. Mdl.Code.universal_int_plus (abs shift)
      | `Transpose_1 -> 0.
      | `Flatten_1 (rows,snake) -> 1. +. Mdl.Code.usage (if snake then 0.1 else 0.9)
      | `Cardinal_1 -> 0.
      | `Plus_2 -> 0.
      | `Minus_2 -> 0.
      | `Modulo_2 -> 0.
      | `ScaleUp_2 -> 0.
      | `ScaleDown_2 -> 0.
      | `ScaleTo_2 -> 0.
      | `I_1 -> 0.
      | `J_1 -> 0.
      | `IJTranspose_1 -> 0.
      | `Direction_1 -> 0.
      | `Abs_1 -> 0.
      | `AsTVec_1 tv -> Mdl.Code.uniform nb_typ_vec
      | `Pos_1 -> 0.
      | `Grid_1 -> 0.
      | `Size_1 -> 0.
      | `Crop_2 -> 0.
      | `Strip_1 -> 0.
      | `Corner_2 -> 0.
      | `Sum_1 -> 0.
      | `Min_1 -> 0.
      | `Max_1 -> 0.
      | `ArgMin_1 -> 0.
      | `ArgMax_1 -> 0.
      | `Average_n -> 0.
      | `Span_2 -> 0.
      | `Norm_1 -> 0.
      | `Diag1_1 k -> Mdl.Code.universal_int_star k
      | `Diag2_1 k -> Mdl.Code.universal_int_star k
      | `LogAnd_1 | `LogOr_1 | `LogXOr_1 | `LogNot_1 -> 0.
      | `Stack_n -> 0.
      | `Area_1 -> 0.
      | `Left_1 | `Right_1 | `Center_1 | `Top_1 | `Bottom_1 | `Middle_1 -> 0.
      | `MiddleCenter_1 -> 0.
      | `Halves_1 dir -> 1.
      | `ProjI_1 | `ProjJ_1 -> 0.
      | `MaskOfGrid_1 | `GridOfMask_2 -> 0.
      | `TranslatedOnto_1 -> 0.
      | `Tiling_1 (k,l) -> Mdl.Code.universal_int_plus k +. Mdl.Code.universal_int_plus l
      | `Unrepeat_1 -> 0.
      | `PeriodicFactor_2 p -> dl_periodicity_mode p
      | `FillResizeAlike_3 p -> dl_periodicity_mode p
      | `SelfCompose_3 -> 0.
      | `ApplySymVec_1 (sym,tv) -> Mdl.Code.uniform nb_symmetry +. Mdl.Code.uniform nb_typ_vec
      | `ApplySymGrid_1 sym -> Mdl.Code.uniform nb_symmetry
      | `UnfoldSym_1 symar -> Mdl.Code.uniform nb_symmetry_unfold
      | `CloseSym_2 symar -> Mdl.Code.uniform nb_symmetry_unfold
      | `TranslationSym_2 sym -> Mdl.Code.uniform nb_symmetry
      | `MajorityColor_1 -> 0.
      | `MinorityColor_1 -> 0.
      | `ColorCount_1 -> 0.
      | `Coloring_2 -> 0.
      | `SwapColors_3 -> 0.

    (* expression index *)

(* XX    let make_index (bindings : bindings) : expr_index =
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
      let index = (* LEVEL 0 - seq indexes *)
        Expr.index_apply_functions
          ~eval_func
          index 1
          (fun (t_args, v_args) ->
            match t_args with
            | [|t1|] ->
               let ndim = t1.ndim in
               let res = [] in
               let res =
                 if ndim >= 1
                 then
                   let$ res, i = res, [0; 1; 2; -2; -1] in
                   ({t1 with ndim = ndim-1}, `Index_1 [Some i], `Default)::
                     (t1, `Tail_1, `Default)::
                       res
                 else res in
               let res =
                 if ndim >= 2
                 then
                   let res =
                     let$ res, j = res, [0; 1; 2; -2; -1] in
                     ({t1 with ndim = ndim-1}, `Index_1 [None; Some j], `Default) :: res in
                   let res =
                     let$ res, i = res, [0; 1; -1] in
                     let$ res, j = res, [0; 1; -1] in
                     ({t1 with ndim = ndim-2}, `Index_1 [Some i; Some j], `Default) :: res in
                   res
                 else res in
               res
            | _ -> []) in
      (*pp (xp_expr_index ~on_typ:(function VEC POS -> true | _ -> false)) index;*)
      (*test "TEST LEVEL 0" index;*)
      let index = (* LEVEL 0' - components *)
        Expr.index_apply_functions
          ~eval_func
          index 1
          (fun (t_args, v_args) ->
            let res : (typ * func * _ Expr.args_spec) list = [] in
            match t_args with
            | [| {kind = VEC tv} as t1 |] ->
               ({t1 with kind = INT (COORD (I, tv))}, `I_1, `Default)
               ::({t1 with kind = INT (COORD (J, tv))}, `J_1, `Default)
               ::res
            | [| {kind = OBJ tg} as t1 |] ->
               ({t1 with kind = VEC POS}, `Pos_1, `Default)
               ::({t1 with kind = GRID tg}, `Grid_1, `Default)
               ::res
            | _ -> res) in
      let index = (* LEVEL 1 *)
        Expr.index_apply_functions
          ~eval_func
          index 2 (* TEST *)
          (fun (t_args, v_args) ->
            let res : (typ * func * _ Expr.args_spec) list = [] in
            let res = (* Norm_1 *)
              match t_args with
              | [| {kind = VEC tv} as t1 |] ->
                 ({t1 with kind = INT CARD}, `Norm_1, `Default)::res
              | _ -> res in
            let res = (* Size_1, Height, Width, Area_1 *)
              match t_args with
              | [| {kind = GRID (filling,nocolor)} as t1 |] ->
                 ({t1 with kind = VEC SIZE}, `Size_1, `Default)
                 ::({t1 with kind = INT (COORD (I, SIZE))}, `Height_1, `Default)
                 ::({t1 with kind = INT (COORD (J, SIZE))}, `Width_1, `Default)
                 ::({t1 with kind = INT CARD}, `Area_1, `Default)
                 ::({t1 with kind = INT (COORD (I, SIZE))}, `Area_1, `Default) (* TODO: add conversion function from CARD to COORD *)
                 ::({t1 with kind = INT (COORD (J, SIZE))}, `Area_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Right, Center, Bottom, Middle *)
              match t_args with
              | [| {kind = OBJ tg} as t1 |] ->
                 ({t1 with kind = INT (COORD (J,POS))}, `Right_1, `Default)
                 ::({t1 with kind = INT (COORD (J,POS))}, `Center_1, `Default)
                 ::({t1 with kind = INT (COORD (I,POS))}, `Bottom_1, `Default)
                 ::({t1 with kind = INT (COORD (I,POS))}, `Middle_1, `Default)
                 ::res
              | _ -> res in
            let res = (* TopHalf, BottomHalf, LeftHalf, RightHalf *)
              match t_args with
              | [| {kind = GRID tg} as t1 |] ->
                 ({t1 with kind = GRID tg}, `TopHalf_1, `Default)
                 ::({t1 with kind = GRID tg}, `BottomHalf_1, `Default)
                 ::({t1 with kind = GRID tg}, `LeftHalf_1, `Default)
                 ::({t1 with kind = GRID tg}, `RightHalf_1, `Default)
                 ::res
              | _ -> res in
            let res = (* ProjI/J_1 *)
              match t_args with
              | [| {kind = VEC tv} as t1|] ->
                 ({t1 with kind = VEC tv}, `ProjI_1, `Default)
                 ::({t1 with kind = VEC tv}, `ProjJ_1, `Default)
                 ::res
              | _ -> res in
            let res = (* IJTranspose_1 *)
              match t_args with
              | [| {kind = INT (COORD (axis,tv))} as t1 |] ->
                 ({t1 with kind = INT (COORD (axis_transpose axis, tv))}, `Transpose_1, `Default)::res
              | [| {kind = VEC tv} as t1 |] ->
                 ({t1 with kind = VEC tv}, `IJTranspose_1, `Default)::res
              | _ -> res in
            let res = (* Direction_1, Abs_1 *)
              match t_args with
              | [| {kind = INT ti} as t|] ->
                 (t, `Direction_1, `Default)
                 ::(t, `Abs_1, `Default)
                 ::res
              | [| {kind = VEC tv} as t |] ->
                 (t, `Direction_1, `Default)
                 ::(t, `Abs_1, `Default)
                 ::res
              | _ -> res in
            let res = (* AsTVec_1 *)
              match t_args with
              | [| {kind = INT (COORD (axis,tv))} as t1 |] ->
                 let$ res, tv' = res, (match tv with
                                       | POS -> [SIZE; MOVE]
                                       | SIZE -> [POS; MOVE]
                                       | MOVE -> [POS; SIZE]) in
                 ({t1 with kind = INT (COORD (axis,tv'))}, `AsTVec_1 tv', `Default)::res
              | [| {kind = VEC tv} as t1 |] ->
                 let$ res, tv' = res, (match tv with
                                       | POS -> [SIZE; MOVE]
                                       | SIZE -> [POS; MOVE]
                                       | MOVE -> [POS; SIZE]) in
                 ({t1 with kind = VEC tv'}, `AsTVec_1 tv', `Default)::res
              | _ -> res in
            let res = (* MajorityColor_1, MinorityColor_1 *)
              match t_args with
              | [| {kind = GRID (filling,false)} as t1 |] ->
                 let full = (filling = `Full) in
                 let$ res, tc = res, [C_BG full; C_OBJ] in
                 ({t1 with kind = COLOR tc}, `MajorityColor_1, `Default)
                 ::({t1 with kind = COLOR tc}, `MinorityColor_1, `Default)
                 ::res
              | _ -> res in
            let res = (* ColorCount_1 *)
              match t_args with
              | [| {kind = GRID (filling,false)} as t1 |] ->
                 ({t1 with kind = INT CARD}, `ColorCount_1, `Default)::res
              | _ -> res in
            (*let res = (* Strip_1: covered by pattern Crop *)
              match t_args with
              | [|GRID (filling,nocolor)|] -> (GRID (false,nocolor), `Strip_1)::res
              | _ -> res in *)
            (* TODO: PeriodicFactor_2, as pattern *)
            let res = (* Corner_2 *)
              match t_args with
              | [| {kind = VEC POS} as t1;
                   {kind = VEC POS} as t2 |] ->
                 ({kind = VEC POS; ndim = max t1.ndim t2.ndim}, `Corner_2, `Default)::res
              | _ -> res in
            let res = (* Span_2 *)
              match t_args with
              | [| {kind = INT (COORD (axis1,POS))} as t1;
                   {kind = INT (COORD (axis2,POS))} as t2 |] when axis1=axis2 ->
                 ({kind = INT (COORD (axis1,POS)); ndim = max t1.ndim t2.ndim}, `Span_2, `Default)::res
              | [| {kind = VEC POS} as t1;
                   {kind = VEC POS} as t2 |] ->
                 ({kind = VEC POS; ndim = max t1.ndim t2.ndim}, `Span_2, `Default)::res
              | _ -> res in
            let res = (* translation = pos - pos *)
              match t_args with
              | [| {kind = INT (COORD (axis1,POS))} as t1;
                   {kind = INT (COORD (axis2,POS))} as t2 |] when axis1=axis2 ->
                 ({kind = INT (COORD (axis1,MOVE)); ndim = max t1.ndim t2.ndim}, `Minus_2, `Default)::res
              | [| {kind = VEC POS} as t1;
                   {kind = VEC POS} as t2 |] ->
                 ({kind = VEC POS; ndim = max t1.ndim t2.ndim}, `Minus_2, `Default)::res
              | _ -> res in
(* REM            let res = (* TranslationOnto *)
              match t_args with
              | [| {kind = OBJ _} as t1;
                   {kind = OBJ _} as t2 |] ->
                 ({kind = VEC MOVE; ndim = max t1.ndim t2.ndim}, `TranslationOnto_2, `Default)::res
              | _ -> res in *)
            let res = (* TranslationSym *)
              match t_args with
              | [| {kind = OBJ _} as t1;
                   {kind = (OBJ _ | GRID _)} as t2 |] ->
                 let$ res, sym =
                   res,
                   [`FlipHeight; `FlipWidth; `FlipDiag1; `FlipDiag2;
                    `Rotate180; `Rotate90; `Rotate270] in
                 ({kind = VEC MOVE; ndim = max t1.ndim t2.ndim}, `TranslationSym_2 sym, `Default)::res
              | _ -> res in
            (*let res = (* Crop *)
              match t_args with
              | [|GRID tg1; OBJ _|] -> (GRID tg1, `Crop_2)::res
              | _ -> res in*)
            let res = (* MaskOfGrid *)
              match t_args with
              | [| {kind = GRID ((`Sprite|`Noise as filling), false)} as t1 |] ->
                 ({t1 with kind = GRID (filling, true)}, `MaskOfGrid_1, `Default)::res
              | _ -> res in
            let res = (* Cardinal *)
              match t_args with
              | [| {kind = OBJ _} as t1 |] when t1.ndim > 0 ->
                 (scalar (INT CARD), `Cardinal_1, `Default)::res (* TODO: generalize beyond OBJ ? *)
              | _ -> res in
            res) in
      (*pp (xp_expr_index ~on_typ:(function VEC POS -> true | _ -> false)) index;*)
      (*test "TEST LEVEL 1" index;*)
      let index = (* LEVEL 2 *)
        Expr.index_apply_functions
          ~eval_func
          index 2 (* TEST *)
          (fun (t_args,v_args) ->
            let res : (typ * func * _ Expr.args_spec) list = [] in
            let res = (* Reverse, Rotate, Transpose, Flatten *)
              match t_args with
              | [|t1|] ->
                 let res =
                   if t1.ndim >= 1 (* only defined on sequences *)
                   then
                     let res = (t1, `Reverse_1, `Default)::res in
                     let$ res, shift = res, [-1; 1] in
                     (t1, `Rotate_1 shift, `Default)::res
                   else res in
                 let res =
                   if t1.ndim >= 2 (* only defined on sequences of sequences *)
                   then
                     let res = (t1, `Transpose_1, `Default)::res in
                     let$ res, rows = res, [true; false] in
                     let$ res, snake = res, [false; true] in
                     (t1, `Flatten_1 (rows,snake), `Default)::res
                   else res in
                 res
              | _ -> res in
            let res = (* ScaleUp, ScaleDown *)
              match t_args with
              | [| {kind = INT _ | VEC _ | GRID _} as t1 |] ->
                 let$ res, k = res, [2;3] in
                 let args_spec = `Custom [|`Pos 0; `Val (scalar (INT CARD), `Int k)|] in
                 (t1, `ScaleUp_2, args_spec)::(t1, `ScaleDown_2, args_spec)::res
              | _ -> res in
            (* MOVE as POS ? *)
            let res = (* ApplySymGrid *)
              match t_args with
              | [| {kind = GRID _} as t1 |] ->
                 let$ res, sym = res, all_symmetry in
                 (t1, `ApplySymGrid_1 sym, `Default)::res
              | _ -> res in
            let res = (* Coloring *)
              match t_args with
              | [| {kind = GRID _} as t1;
                   {kind = COLOR _} as t2 |] ->
                 ({t1 with ndim = max t1.ndim t2.ndim}, `Coloring_2, `Default)::res
              | _ -> res in
            let res = (* SelfCompose *)
              match t_args with
              | [| {kind = GRID (filling,nocolor)} as t1 |] ->
                 let full = filling = `Full in
                 let bgcolor = if full then Grid.black else Grid.transparent in 
                 let$ res, color = res, if nocolor then [Grid.black] else Grid.all_colors in
                 let args_spec = `Custom [| `Val (scalar (COLOR (C_BG full)), `Color bgcolor);
                                            `Val (scalar (COLOR C_OBJ), `Color color);
                                            `Pos 0|] in
                 (t1, `SelfCompose_3, args_spec)::res
              | [| {kind = COLOR C_OBJ} as t1;
                   {kind = GRID (filling,nocolor)} as t2 |] ->
                 let full = filling = `Full in
                 let bgcolor = if full then Grid.black else Grid.transparent in
                 let args_spec = `Custom [| `Val (scalar (COLOR (C_BG full)), `Color bgcolor);
                                            `Pos 0;
                                            `Pos 1|] in                 
                 ({t2 with ndim = max t1.ndim t2.ndim}, `SelfCompose_3, args_spec)::res
              | _ -> res in
            let res = (* Plus *)
              match t_args with
              | [| {kind = INT (CARD | INDEX)} as t1 |] ->
                 let$ res, i2 = res, [1;2;3] in
                 let args_spec = `Custom [|`Pos 0; `Val (scalar (INT CARD), `Int i2)|] in
                 (t1, `Plus_2, args_spec)::res
              | [| {kind = INT (CARD | INDEX)} as t1;
                   {kind = INT (CARD | INDEX)} as t2 |] ->
                 ({t1 with ndim = max t1.ndim t2.ndim}, `Plus_2, `Default)::res
              | [| {kind = INT (COORD (axis,tv1))} as t1 |] when tv1 <> MOVE ->
                 let$ res, i2 = res, [1;2;3] in
                 let args_spec = `Custom [|`Pos 0; `Val (scalar (INT (COORD (axis,MOVE))), `Int i2)|] in
                 (t1, `Plus_2, args_spec)::res
              | [| {kind = INT (COORD (_,tv1))} as t1;
                   {kind = INT (COORD (_,(SIZE|MOVE)))} as t2 |] when tv1 <> MOVE ->
                 ({t1 with ndim = max t1.ndim t2.ndim}, `Plus_2, `Default)::res
              | [| {kind = VEC tv1} as t1 |] when tv1 <> MOVE ->
                 let$ res, i2 = res, [0;1;2;3] in
                 let$ res, j2 = res, (if i2=0 then [1;2;3] else [0;1;2;3]) in
                 let args_spec = `Custom [|`Pos 0; `Val (scalar (VEC MOVE), `Vec (i2,j2))|] in
                 (t1, `Plus_2, args_spec)::res
              | [| {kind = VEC tv1} as t1;
                   {kind = VEC (SIZE|MOVE)} as t2 |] when tv1 <> MOVE ->
                 ({t1 with ndim = max t1.ndim t2.ndim}, `Plus_2, `Default)::res
              | _ -> res in
            let res = (* Minus *)
              match t_args with
              | [| {kind = INT (CARD | INDEX)} as t1|] ->
                 let$ res, i2 = res, [1;2;3] in
                 let args_spec = `Custom [|`Pos 0; `Val (scalar (INT CARD), `Int i2)|] in
                 (t1, `Minus_2, args_spec)::res
              | [| {kind = INT (CARD | INDEX)} as t1;
                   {kind = INT (CARD | INDEX)} as t2 |] ->
                 ({t1 with ndim = max t1.ndim t2.ndim}, `Minus_2, `Default)::res
              | [| {kind = INT (COORD (axis,tv1))} as t1 |] when tv1 <> MOVE ->
                 let$ res, i2 = res, [1;2;3] in
                 let args_spec = `Custom [|`Pos 0; `Val (scalar (INT (COORD (axis,MOVE))), `Int i2)|] in
                 (t1, `Minus_2, args_spec)::res
              | [| {kind = INT (COORD (_,tv1))} as t1;
                   {kind = INT (COORD (_, (SIZE|MOVE)))} as t2 |] when tv1 <> MOVE ->
                 ({t1 with ndim = max t1.ndim t2.ndim}, `Minus_2, `Default)::res
              | [| {kind = VEC tv1} as t1 |] when tv1 <> MOVE ->
                 let$ res, i2 = res, [0;1;2;3] in
                 let$ res, j2 = res, (if i2=0 then [1;2;3] else [0;1;2;3]) in
                 let args_spec = `Custom [|`Pos 0; `Val (scalar (VEC MOVE), `Vec (i2,j2))|] in
                 (t1, `Minus_2, args_spec)::res
              | [| {kind = VEC tv1} as t1;
                   {kind = VEC (SIZE|MOVE)} as t2 |] when tv1 <> MOVE ->
                 ({t1 with ndim = max t1.ndim t2.ndim}, `Minus_2, `Default)::res
              | _ -> res in
            let res = (* Min, Max, ArgMin, ArgMax *)
              match t_args with
              | [| {kind = INT _} as t1 |] when t1.ndim > 0 ->
                 ({t1 with ndim = 0}, `Min_1, `Default)
                 ::({t1 with ndim = 0}, `Max_1, `Default)
                 ::(typ_index, `ArgMin_1, `Default)
                 ::(typ_index, `ArgMax_1, `Default)
                 ::res
              | _ -> res in
(*            let res = (* And, Or, XOr, AndNOt *)
              match t_args with
              | [| {kind = GRID (`Sprite,true)} as t1; t2 |] when t2.kind = t1.kind ->
                 let$ res, f = res, [`LogAnd_2; `LogOr_2; `LogXOr_2; `LogAndNot_2] in
                 ({t1 with ndim = max t1.ndim t2.ndim}, f, `Default)::res
              | _ -> res in *)
            res) in
      (*test "TEST LEVEL 2" index;*)
      let index = (* LEVEL 3 *)
        Expr.index_apply_functions
          ~eval_func
          index 1 (* TEST: 2, binary, is too expansive *)
          (fun (t_args,v_args) ->
            let res : (typ * func * _ Expr.args_spec) list = [] in
            let res = (* AsTVec_1 *)
              match t_args with
              | [| {kind = INT CARD} as t1 |] ->
                 let$ res, axis = res, [I; J] in
                 let$ res, tv = res, [POS; SIZE; MOVE] in
                 ({t1 with kind = INT (COORD (axis,tv))}, `AsTVec_1 tv, `Default)::res
              | _ -> res in
            let res = (* LogNot *)
              match t_args with
              | [| {kind = GRID (`Sprite,true)} as t1 |] ->
                 (t1, `LogNot_1, `Default)::res
              | _ -> res in
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
                 let args_spec = `Custom [|`Val (scalar (COLOR (C_BG full)), `Color bgcolor); `Pos 0; `Pos 1|] in
                 let$ res, mode =
                   res, (if full
                         then [`TradeOff; `Total; `Strict]
                         else [`TradeOff; `Strict]) in
                 (t3, `FillResizeAlike_3 mode, args_spec)::res
              | _ -> res in*)
            let res = (* Unrepeat *)
              match t_args with
              | [| {kind = GRID _} as t1 |] ->
                 (t1, `Unrepeat_1, `Default)::res
              | _ -> res in
            (*let res = (* UnfoldSym *)
              match t_args with
              | [|GRID _ as t1|] ->
                 let$ res, sym_matrix = res, all_symmetry_unfold in
                 (t1, `UnfoldSym_1 sym_matrix, `Default)::res
              | _ -> res in*)
            let res = (* CloseSym *)
              match t_args with
              | [| {kind = GRID (filling,_)} as t2 |] ->
                 let full = (filling = `Full) in
                 let$ res, bgcolor = res, bgcolors full in
                 let args_spec = `Custom [|`Val (scalar (COLOR (C_BG full)), `Color bgcolor); `Pos 0|] in
                 let$ res, sym_seq = res, all_symmetry_close in
                 (t2, `CloseSym_2 sym_seq, args_spec)::res
              | _ -> res in
            (*let res = (* SwapColors *)
              match t_args with
              | [|GRID (_,_) as t1; COLOR C_OBJ; COLOR C_OBJ|] -> (t1, `SwapColors_3)::res
              | _ -> res in*)
            (*let res = (* ScaleTo *)
              match t_args with
              | [|GRID _ as t1; VEC SIZE|] -> (t1, `ScaleTo_2, `Default)::res
              | _ -> res in*)
            (* Stack *)
            res) in
      (*test "TEST LEVEL 4" index;*)
      let index = (* LEVEL 4 *)
        Expr.index_apply_functions
          ~eval_func
          index 1
          (fun (t_args,v_args) ->
            let res : (typ * func * _ Expr.args_spec) list = [] in
            let res = (* Cast *)
              match t_args with
              | [| {kind} as t1 |] ->
                 let lk' =
                   match kind with
                   | INT CARD -> [INT INDEX]
                   | COLOR C_OBJ -> [COLOR (C_BG true); COLOR (C_BG false)]
                   | COLOR (C_BG true) -> [COLOR C_OBJ; COLOR (C_BG false)]
                   | GRID (filling,nocolor) ->
                      let$ res, filling' = [], [`Full; `Sprite; `Noise] in
                      if filling' = filling then res else GRID (filling',nocolor)::res
                   | OBJ (filling,nocolor) ->
                      let$ res, filling' = [], [`Full; `Sprite; `Noise] in
                      if filling' = filling then res else OBJ (filling',nocolor)::res
                   | _ -> [] in
                 let$ res, k' = res, lk' in
                 assert (k' <> kind);
                 ({t1 with kind = k'}, `Cast_1 (kind,k'), `Default)::res
              | _ -> res in
            res) in
      (* pp (xp_expr_index ~on_typ:(function VEC POS -> true | _ -> false)) index; *)
      index *)

    let affine_params = [
        `ScaleUp_2, 1, `Plus_2, 1;
        `ScaleUp_2, 1, `Plus_2, 2;
        `ScaleUp_2, 1, `Plus_2, 3;
        `ScaleUp_2, 1, `Minus_2, 1;
        `ScaleUp_2, 1, `Minus_2, 2;
        `ScaleUp_2, 1, `Minus_2, 3;
        `ScaleUp_2, 2, `Plus_2, 0;
        `ScaleUp_2, 2, `Plus_2, 1;
        `ScaleUp_2, 2, `Minus_2, 1;
        `ScaleDown_2, 2, `Plus_2, 0;
        `ScaleDown_2, 2, `Plus_2, 1;
        `ScaleDown_2, 2, `Minus_2, 1;
        `ScaleUp_2, 3, `Plus_2, 0;
        `ScaleUp_2, 3, `Plus_2, 1;
        `ScaleUp_2, 3, `Minus_2, 1;
        `ScaleDown_2, 3, `Plus_2, 0;
        `ScaleDown_2, 3, `Plus_2, 1;
        `ScaleDown_2, 3, `Minus_2, 1;
      ]
    
    let make_index (bindings : bindings) : expr_index = (* NEW VERSION *)
      Common.prof "make_index" (fun () ->
      let bgcolors full =
        Grid.black :: if full then [] else [Grid.transparent] in
      let index = Expr.Index.empty in
      let index = Expr.index_add_bindings index bindings in
      let index = (* LEVEL: Grid features *)
        Common.prof "make_index/grid_features" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* Grid_1 *)
              match t1.kind with
              | OBJ tg ->
                 ({t1 with kind = GRID tg}, `Grid_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Halves_1 *)
              match t1.kind with
              | GRID tg ->
                 ({kind = GRID tg; ndim = t1.ndim+1}, `Halves_1 `H, `Default)
                 ::({kind = GRID tg; ndim = t1.ndim+1}, `Halves_1 `V, `Default)
                 ::res
              | _ -> res in
            res)) in
      let index = (* LEVEL: Color features, Vec features *)
        Common.prof "make_index/color_vec_features" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res =  (* MajorityColor_1, MinorityColor_1 *)
              match t1.kind with
              | GRID (filling,false) ->
                 let full = (filling = `Full) in
                 let$ res, tc = res, [C_BG full; C_OBJ] in
                 let tres = {t1 with kind = COLOR tc} in
                 (tres, `MajorityColor_1, `Default)
                 ::(tres, `MinorityColor_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Size_1 *)
              match t1.kind with
              | GRID (filling,nocolor) ->
                 ({t1 with kind = VEC SIZE}, `Size_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Pos_1 *)
              match t1.kind with
              | OBJ tg ->
                 ({t1 with kind = VEC POS}, `Pos_1, `Default)
                 ::res
              | _ -> res in
            let res = (* TranslatedOnto_1 *)
              match t1.kind with
              | OBJ _ when t1.ndim > 0 ->
                 ({kind = VEC POS; ndim = t1.ndim + 1}, `TranslatedOnto_1, `Default)::res
              | _ -> res in
            (* TODO: TranslationSym, only inter objects, handle against GRID with negative object positions *)
            res)) in
      let index = (* LEVEL: Int features *)
        Common.prof "make_index/int_features" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* I_1, J_1, Norm_1 *)
              match t1.kind with
              | VEC tv ->
                 ({t1 with kind = INT (COORD (I, tv))}, `I_1, `Default)
                 ::({t1 with kind = INT (COORD (J, tv))}, `J_1, `Default)
                 ::({t1 with kind = INT CARD}, `Norm_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Area_1 *)
              match t1.kind with
              | GRID (filling,nocolor) ->
                 ({t1 with kind = INT CARD}, `Area_1, `Default)
                 ::({t1 with kind = INT (COORD (I, SIZE))}, `Area_1, `Default) (* TODO: add cast from CARD to COORD? *)
                 ::({t1 with kind = INT (COORD (J, SIZE))}, `Area_1, `Default)
                 ::res
              | _ -> res in
            let res = (* ColorCount_1 *)
              match t1.kind with
              | GRID (filling,false) ->
                 ({t1 with kind = INT CARD}, `ColorCount_1, `Default)::res
              | _ -> res in
            let res = (* Left, Right, Center, Top, Bottom, Middle, MiddleCenter *)
              match t1.kind with
              | OBJ tg ->
                 ({t1 with kind = INT (COORD (J,POS))}, `Left_1, `Default)
                 ::({t1 with kind = INT (COORD (J,POS))}, `Right_1, `Default)
                 ::({t1 with kind = INT (COORD (J,POS))}, `Center_1, `Default)
                 ::({t1 with kind = INT (COORD (I,POS))}, `Top_1, `Default)
                 ::({t1 with kind = INT (COORD (I,POS))}, `Bottom_1, `Default)
                 ::({t1 with kind = INT (COORD (I,POS))}, `Middle_1, `Default)
                 ::({t1 with kind = VEC POS}, `MiddleCenter_1, `Default)
                 ::res
              | _ -> res in
            res)) in
  (* TODO: binary exprs too costly
      let index = (* LEVEL: Int+Vec bin *)
        Common.prof "make_index/int_vec_bin" (fun () ->
        Expr.index_apply_functions_2
          ~eval_func
          index
          (function ({kind = (INT _ | VEC _); ndim}, _) -> ndim <= 1 | _ -> false)
          (fun t1 v1 t2 v2 ->
            let res = [] in
            let res = (* x + y, x - y, abs(x-y), direction(x-y) *)
              if t1.ndim <= 1 && t1.ndim = t2.ndim then (* TODO: ideally, only when v1 and v2 derive from same sequence axis *)
              match t1.kind, t2.kind with
              | INT ti1, INT ti2 ->
                 let tres = {t1 with ndim = max t1.ndim t2.ndim} in
                 let res =
                   (tres, `Plus_2, `Default)
                   ::(tres, `Minus_2, `Default)
                   ::res in
                 let res =
                   if ti1 = ti2
                   then
                     (tres, `Abs_1, `Custom [| `Apply (tres, `Minus_2, [|`Pos 0; `Pos 1|]) |])
                     ::(tres, `Direction_1, `Custom [| `Apply (tres, `Minus_2, [|`Pos 0; `Pos 1|]) |])
                     ::res
                   else res in
                 res
              | VEC tv1, VEC tv2 ->
                 let tres = {t1 with ndim = max t1.ndim t2.ndim} in
                 let res =
                   (tres, `Plus_2, `Default)
                   ::(tres, `Minus_2, `Default)
                   ::res in
                 let res =
                   if tv1 = tv2
                   then
                     (tres, `Abs_1, `Custom [| `Apply (tres, `Minus_2, [|`Pos 0; `Pos 1|]) |])
                     ::(tres, `Direction_1, `Custom [| `Apply (tres, `Minus_2, [|`Pos 0; `Pos 1|]) |])
                     ::res
                   else res in
                 res
              | _ -> res
              else res in
            res)) in *)
      let index = (* LEVEL: INT+VEC affine, GRID derived *)
        Common.prof "make_index/int_vec_affine" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* ax + b, for x : INT | VEC *)
              match t1.kind with
              | INT (COORD (_, MOVE)) -> res
              | INT ti ->
                 let ta = scalar (INT CARD) in
                 let tb = scalar (INT (match ti with
                                       | COORD (axis,_) -> COORD (axis,MOVE)
                                       | CARD -> INDEX
                                       | INDEX -> INDEX)) in
                 let$ res, (opmult,a,opadd,b) = res, affine_params in
                 let f, spec_args =
                   if b = 0 then opmult, `Custom [| `Pos 0; `Val (ta, `Int a) |]
                   else if a = 1 then opadd, `Custom [| `Pos 0; `Val (tb, `Int b) |]
                   else opadd, `Custom [| `Apply (t1, opmult, [| `Pos 0; `Val (ta, `Int a) |]);
                                          `Val (tb, `Int b) |] in
                 (t1, f, spec_args)::res
              | VEC MOVE -> res
              | VEC tv ->
                 let ta = scalar (VEC SIZE) in (* should be CARD *)
                 let tb = scalar (VEC MOVE) in
                 let$ res, (opmult,a,opadd,b) = res, affine_params in
                 let$ res, (a1,a2) = res, if a = 1 then [(1,1)] else [(a,a); (1,a); (a,1)] in
                 let$ res, (b1,b2) = res, if b = 0 then [(0,0)] else [(b,b); (0,b); (b,0)] in
                 let f, spec_args =
                   if b = 0 then
                     opmult, `Custom [| `Pos 0; `Val (ta, `Vec (a1,a2)) |]
                   else if a = 1 then
                     opadd, `Custom [| `Pos 0; `Val (tb, `Vec (b1,b2)) |]
                   else
                     opadd, `Custom [| `Apply (t1, opmult, [| `Pos 0; `Val (ta, `Vec (a1,a2)) |]);
                                       `Val (tb, `Vec (b1,b2)) |] in
                 (t1, f, spec_args)::res
              | _ -> res in
            let res = (* Unrepeat *)
              match t1.kind with
              | GRID _ ->
                 (t1, `Unrepeat_1, `Default)::res
              | _ -> res in
            res)) in
      let index = (* LEVEL: INT+VEC transpose *)
        Common.prof "make_index/int_vec_transpose" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* IJTranspose *)
              match t1.kind with
              | INT (COORD (axis,tv)) ->
                 ({t1 with kind = INT (COORD (axis_transpose axis, tv))}, `Transpose_1, `Default)::res
              | VEC tv ->
                 ({t1 with kind = VEC tv}, `IJTranspose_1, `Default)::res
              | _ -> res in
            let res = (* ApplySymGrid *)
              match t1.kind with
              | GRID _ ->
                 let$ res, sym = res, all_symmetry in
                 (t1, `ApplySymGrid_1 sym, `Default)::res
              | _ -> res in
            res)) in
      let index = (* LEVEL: GRID part+compose *)
        Common.prof "make_index/grid_part_compose" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* CloseSym *)
              match t1.kind with
              | GRID (filling,_) ->
                 let full = (filling = `Full) in
                 let$ res, bgcolor = res, bgcolors full in
                 let args_spec = `Custom [|`Val (scalar (COLOR (C_BG full)), `Color bgcolor); `Pos 0|] in
                 let$ res, sym_seq = res, all_symmetry_close in
                 (t1, `CloseSym_2 sym_seq, args_spec)::res
              | _ -> res in
            let res = (* SelfCompose *)
              match t1.kind with
              | GRID (filling,nocolor) ->
                 let full = filling = `Full in
                 let bgcolor = if full then Grid.black else Grid.transparent in
                 let$ res, color_arg =
                   let tcol = scalar (COLOR C_OBJ) in
                   res,
                   if nocolor
                   then [`Val (tcol, `Color Grid.black)]
                   else
                     let colors =
                       [ `Apply (tcol, `MajorityColor_1, [|`Pos 0|]);
                         `Apply (tcol, `MinorityColor_1, [|`Pos 0|]) ] in
                     let$ colors, color = colors, Grid.all_colors in
                     `Val (tcol, `Color color)::colors in
                 let args_spec = `Custom [| `Val (scalar (COLOR (C_BG full)), `Color bgcolor);
                                            color_arg;
                                            `Pos 0|] in
                 (t1, `SelfCompose_3, args_spec)::res
              | _ -> res in
(* TODO            let res = (* SelfCompose/2 - TODO: should be added with unary SelfCompose but avoid full binary fold *)
              match t_args with
              | [| {kind = COLOR C_OBJ} as t1;
                   {kind = GRID (filling,nocolor)} as t2 |] ->
                 let full = filling = `Full in
                 let bgcolor = if full then Grid.black else Grid.transparent in
                 let args_spec = `Custom [| `Val (scalar (COLOR (C_BG full)), `Color bgcolor);
                                            `Pos 0;
                                            `Pos 1|] in                 
                                            ({t2 with ndim = max t1.ndim t2.ndim}, `SelfCompose_3, args_spec)::res
                                            | _ -> res in *)
            res)) in
      let index = (* LEVEL: GRID mask *)
        Common.prof "make_index/grid_mask" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* MaskOfGrid *)
              match t1.kind with
              | GRID ((`Sprite|`Noise as filling), false) ->
                 let tres = {t1 with kind = GRID (filling, true)} in
                 (tres, `MaskOfGrid_1, `Default)
                 ::(tres, `LogNot_1, `Custom [| `Apply (tres, `MaskOfGrid_1, [|`Pos 0|]) |])
                 ::res
              | _ -> res in
            res)) in
      let index = (* LEVEL: ALL items and slices *)
        Common.prof "make_index/items_slices" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let ndim = t1.ndim in
            let res = [] in
            let res = (* Index_1[i], Tail_1 *)
              if ndim >= 1
              then
                let$ res, i = res, [0; 1; 2; -2; -1] in
                ({t1 with ndim = ndim-1}, `Index_1 [Some i], `Default)
                ::(t1, `Tail_1, `Default)
                ::res
              else res in
            let res = (* Index_1[i,j] *)
              if ndim >= 2
              then
                let res =
                  let$ res, j = res, [0; 1; 2; -2; -1] in
                  ({t1 with ndim = ndim-1}, `Index_1 [None; Some j], `Default) :: res in
                let res =
                  let$ res, i = res, [0; 1; -1] in
                  let$ res, j = res, [0; 1; -1] in
                  ({t1 with ndim = ndim-2}, `Index_1 [Some i; Some j], `Default) :: res in
                res
              else res in
            res)) in
      let index = (* LEVEL: collection-wise *)
        Common.prof "make_index/collection" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let ndim = t1.ndim in
            let res = [] in
            if ndim > 0
            then
              let t1_scalar = {t1 with ndim = 0} in
              let res = (* Reverse, Rotate *)
                let res = (t1, `Reverse_1, `Default)::res in
                let$ res, shift = res, [-1; 1] in
                (t1, `Rotate_1 shift, `Default)::res in
              let res = (* Transpose, Flatten *)
                if ndim >= 2 (* only defined on sequences of sequences *)
                then
                  let res = (t1, `Transpose_1, `Default)::res in
                  let$ res, rows = res, [true; false] in
                  let$ res, snake = res, [false; true] in
                  ({t1 with ndim = t1.ndim - 1}, `Flatten_1 (rows,snake), `Default)::res
                else res in
              let res = (* Sum, Min, Max, ArgMin, ArgMax *)
                match t1.kind with
                | INT _ ->
                   (t1_scalar, `Sum_1, `Default)
                   ::(t1_scalar, `Min_1, `Default)
                   ::(t1_scalar, `Max_1, `Default)
                   ::(typ_index, `ArgMin_1, `Default)
                   ::(typ_index, `ArgMax_1, `Default)
                   ::res
                | _ -> res in
              let res = (* And, Or, XOr *)
                match t1.kind with
                | GRID (`Sprite,true) ->
                   (t1_scalar, `LogAnd_1, `Default)
                   ::(t1_scalar, `LogOr_1, `Default)
                   ::(t1_scalar, `LogXOr_1, `Default)
                   ::res
                | _ -> res in
              res
            else res)) in
      let index = (* LEVEL: cast *)
        Common.prof "make_index/cast" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let kind = t1.kind in
            let res = [] in
            let lk' =
              match kind with
              | INT CARD ->
                 let res = [INT INDEX] in
                 let$ res, tv = res, [SIZE; POS; MOVE] in
                 let$ res, axis = res, [I; J] in
                 INT (COORD (axis,tv))::res
              | COLOR C_OBJ -> [COLOR (C_BG true); COLOR (C_BG false)]
              | COLOR (C_BG true) -> [COLOR C_OBJ; COLOR (C_BG false)]
              | GRID (filling,nocolor) ->
                 let$ res, filling' = [], [`Full; `Sprite; `Noise] in
                 if filling' = filling then res else GRID (filling',nocolor)::res
              | OBJ (filling,nocolor) ->
                 let$ res, filling' = [], [`Full; `Sprite; `Noise] in
                 if filling' = filling then res else OBJ (filling',nocolor)::res
              | _ -> [] in
            let$ res, k' = res, lk' in
            assert (k' <> kind);
            ({t1 with kind = k'}, `Cast_1 (kind,k'), `Default)::res)) in
      index)

    let make_index, reset_make_index =
      Memo.memoize ~size:103 make_index

    (* refining *)

    let decompositions ~env_vars (t : typ) (varseq : varseq) (valuess : value list list) : (model * varseq) list =
      let ndim = t.ndim in
      (*if not (ndim = Ndseq.depth (List.hd (List.hd valuess))) then (
        pp_endline xp_typ t;
        pp_endline xp_value (List.hd (List.hd valuess))
      );*)
      let rs = [] in
      let rs = (* adding SeqCons *)
        if ndim = 1 (* > 0 : TODO BUG: this entails missing refinements, unrelated ones *)
        then
          let xhd, varseq = Refining.new_var varseq in
          let xtl, varseq = Refining.new_var varseq in
          let$ rs, depth = rs, List.init ndim (fun i -> i) in
          if List.for_all (* TODO: not necessary, check if more efficient *)
               (fun vs ->
                 List.exists
                   (fun v ->
                     Ndseq.for_all ~depth
                       (fun v ->
                         match Ndseq.as_seq v with
                         | Some (_,l) -> l <> []
                         | _ -> assert false)
                       v)
                   vs)
               valuess
          then
            (Model.make_pat t (SeqCons depth)
               [| Model.make_def xhd (Model.make_any {t with ndim = ndim-1});
                  Model.make_def xtl (Model.make_any t) |],
             varseq) :: rs
          else rs
        else rs in
      let rs = (* adding SeqRepeat, to better reach repeated values *)
        if ndim = 1 (* TODO: generalize to > 0 *)
        then
          let xe, varseq = Refining.new_var varseq in
          let$ rs, depth = rs, List.init ndim (fun i -> i) in
          if List.for_all (* TODO: see above *)
               (fun vs ->
                 List.exists
                   (fun v ->
                     Ndseq.for_all ~depth
                       (fun v ->
                         match Ndseq.as_seq v with
                         | Some (_,l) -> l <> []
                         | _ -> assert false)
                       v)
                   vs)
               valuess
          then 
            (Model.make_pat t (SeqRepeat depth)
               [| Model.make_def xe (Model.make_any {t with ndim = ndim-1}) |],
             varseq) :: rs
          else rs
        else rs in
(*      let rs = (* adding SeqIndexOf *) (* NOT specific enough, too many matches *)
        match t.kind with
        | INT INDEX ->
           let xvalue, varseq = Refining.new_var varseq in
           let$ rs, (x,tx) = rs, Mymap.bindings env_vars in
           (Model.make_pat {tx with ndim = 1} (SeqIndexOf tx.kind)
              [| Model.make_expr tx (Expr.Ref (tx, x));
                 Model.make_def xvalue (Model.make_any (scalar tx)) |],
            varseq) :: rs
        | _ -> rs in *)
      let rs = (* adding Vec *)
        match t.kind with
        | VEC tv ->
           let xi, varseq = Refining.new_var varseq in
           let xj, varseq = Refining.new_var varseq in
           (Model.make_pat t Vec
              [| Model.make_def xi (Model.make_any {t with kind = INT (COORD (I, tv))});
                 Model.make_def xj (Model.make_any {t with kind = INT (COORD (J, tv))}) |],
            varseq) :: rs
        | _ -> rs in
(*      let rs = (* adding Obj: implicit with Objects *)
        match t with
        | OBJ tg ->
           let xpos, varseq = Refining.new_var varseq in
           let xg1, varseq = Refining.new_var varseq in
           (make_obj tg
              (Model.make_def xpos (make_anyvec POS))
              (Model.make_def xg1 (make_anygrid tg)),
            varseq) :: rs
        | _ -> rs in *)
(*      let rs = (* Monocolor when always single color. Because of SameColor segmentations... *)
        match t with
        | GRID (filling,false) ->
           if List.for_all
                (fun vs ->
                  List.exists
                    (fun v ->
                      Ndseq.for_all ~depth
                        (function
                         | `Grid g -> Grid.color_count Grid.transparent g = 1
                         | v -> pp_endline xp_value v; assert false)
                        v)
                    vs)
                valuess
           then
             let xg1_color, varseq = Refining.new_var varseq in
             let xg1_mask, varseq = Refining.new_var varseq in
             (make_monocolor
                (Model.make_def xg1_color (make_anycolor C_OBJ))
                (Model.make_def xg1_mask (make_anygrid (filling,true))),
              varseq) :: rs
           else rs
        | _ -> rs in *)
      rs
    
    let refinements_any ~env_vars (t : typ) (varseq : varseq) (value : value) : (model * varseq) list = (* QUICK *)
      let ndim = t.ndim in
      let rs = [] in
      let rs = (* adding SeqRepeat *)
        if ndim > 0
        then
          let xe, varseq = Refining.new_var varseq in
          let$ rs, depth = rs, List.init ndim (fun i -> i) in
          (Model.make_pat t (SeqRepeat depth)
             [| Model.make_def xe (Model.make_any {t with ndim = ndim-1}) |],
           varseq) :: rs
        else rs in
      let rs = (* adding SeqIndex *)
        let xindex, varseq = Refining.new_var varseq in
        let compatible_vars = (* same type vars from env *)
          Mymap.fold
            (fun x tx res ->
              if tx.kind = t.kind && tx.ndim > t.ndim
              then (x,tx)::res
              else res)
            env_vars [] in
        let$ rs, (x,tx) = rs, compatible_vars in
        (Model.make_pat t SeqIndex
           [| Model.make_expr tx (Expr.Ref (tx, x));
              Model.make_def xindex (Model.make_any typ_index) |],
         varseq) :: rs in
      match t.kind with
      | INT ti ->
         let rs = (* adding SeqRange *)
           if ndim > 0
           then
             let xstart, varseq = Refining.new_var varseq in
             let xstep, varseq = Refining.new_var varseq in
             (Model.make_pat t SeqRange
                [| Model.make_def xstart (Model.make_any {t with ndim = ndim-1});
                   Model.make_def xstep (Model.make_any {kind = INT (COORD (I, MOVE)); ndim = ndim-1}) |],
              varseq) :: rs
           else rs in
         rs
      | VEC tv -> rs
      | COLOR tc -> rs
      | SEG -> rs
      | MOTIF tmot -> rs
      | MAP (ka,kb) ->
         let refs : (model * varseq) list = rs in
(* TODO(needs Cons)         let refs = (* DomMap *)
           match tb, value with
           | COLOR tc, `Map m -> (* TODO: generalize to other types *)
              let keys = mymap_keys m in
              let xloop, varseq = Refining.new_var varseq in
              let xvals, varseq = Refining.new_var varseq in
              let mvals, varseq = (* explicit sequence of same length as keys *)
                List.fold_right
                  (fun _ (mvals, varseq) ->
                    let xcol, varseq = Refining.new_var varseq in
                    let mcol = Model.make_def xcol (make_anycolor tc) in
                    let mvals = Model.make_cons xloop mcol mvals in
                    mvals, varseq)
                  keys (Model.make_nil tb, varseq) in
              (make_dommap ta tb keys
                 (Model.make_loop xloop
                    (Model.make_def xvals mvals)),
               varseq)
              :: refs
           | _ -> refs in *)
         let refs = (* DomMap *)
           match kb with
           | COLOR tc -> (* TODO: generalize to other types *)
              let l_keys =
                Ndseq.fold_left
                  (fun res -> function
                    | `Map m ->
                       let keys = mymap_keys m in
                       keys :: res
                    | _ -> assert false)
                  [] value in
              let xloop, varseq = Refining.new_var varseq in
              let xvals, varseq = Refining.new_var varseq in
              let mvals, varseq = Model.make_any {kind = COLOR tc; ndim = ndim+1}, varseq in
              let$ refs, keys = refs, l_keys in (* TODO: check for single keys ? *)
              (* TODO: needs Cons
                 List.fold_right (* explicit sequence of same length as keys *)
                  (fun _ (mvals, varseq) ->
                    let xcol, varseq = Refining.new_var varseq in
                    let mcol = Model.make_def xcol (make_anycolor tc) in
                    let mvals = Model.make_cons xloop mcol mvals in
                    mvals, varseq)
                  keys (Model.make_nil tb, varseq) in *)
              (Model.make_pat t (DomMap keys)
                 [| Model.make_def xvals mvals |],
               varseq)
              :: refs
           | _ -> refs in
        let refs = (* Replace *)
           if ka = kb then
             let xa, varseq = Refining.new_var varseq in
             let xb, varseq = Refining.new_var varseq in
             (Model.make_pat t Replace
                [| Model.make_def xa (Model.make_any {t with kind = COLOR C_OBJ});
                   Model.make_def xb (Model.make_any {t with kind = COLOR C_OBJ}) |],
              varseq)
             :: refs
           else refs in
         let refs = (* Swap *)
           if ka = kb then
             let xa, varseq = Refining.new_var varseq in
             let xb, varseq = Refining.new_var varseq in
             (Model.make_pat t Swap
                [| Model.make_def xa (Model.make_any {t with kind = COLOR C_OBJ});
                   Model.make_def xb (Model.make_any {t with kind = COLOR C_OBJ}) |],
              varseq)
             :: refs
           else refs in
         refs
      | GRID (filling,nocolor as tg) ->
         let refs : (model * varseq) list = rs in
         let refs = (* BgColor *)
           if filling = `Full && not nocolor then
             let xcol, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             (Model.make_pat t BgColor
                [| Model.make_def xcol (Model.make_any {t with kind = COLOR (C_BG true)});
                   Model.make_def xg1 (Model.make_any {t with kind = GRID (`Sprite,nocolor)}) |],
              varseq)
             :: refs
           else refs in
         let refs = (* IsFull *)
           if filling = `Sprite && not nocolor then (* nocolor isfull covered by full mask *)
             let xgrid1, varseq = Refining.new_var varseq in
             (Model.make_pat t IsFull
                [| Model.make_def xgrid1 (Model.make_any {t with kind = GRID (`Full,nocolor)}) |],
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
               (fun x tx res ->
                 match tx.kind with
                 | GRID tgx when tgx = tg && tx.ndim <= ndim -> (x,tx)::res
                 | _ -> res)
               env_vars [] in
           let$ refs, (gvar,tvar) = refs, cropable_vars in
           (Model.make_pat t Crop
              [| Model.make_expr tvar (Expr.Ref (tvar, gvar));
                 Model.make_def xpos (Model.make_any {t with kind = VEC POS});
                 Model.make_def xsize (Model.make_any {t with kind = VEC SIZE}) |],
            varseq)
           :: refs in
         let refs = (* Objects - Connected *)
           if filling <> `Full then
             let xsize, varseq = Refining.new_var varseq in
             let xsize_i, varseq = Refining.new_var varseq in
             let xsize_j, varseq = Refining.new_var varseq in
             let xseg, varseq = Refining.new_var varseq in
             let xcard, varseq = Refining.new_var varseq in
             let xloop, varseq = Refining.new_var varseq in
             let xobj, varseq = Refining.new_var varseq in
             let xpos, varseq = Refining.new_var varseq in
             let xpos_i, varseq = Refining.new_var varseq in
             let xpos_j, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             let xmerger, varseq = Refining.new_var varseq in
             let$ refs, nmax = refs, [1;9] in
             (Model.make_pat {t with kind = GRID (`Sprite,nocolor)} (Objects (nmax, `Connected))
                [| Model.make_def xsize (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def xseg (Model.make_any {t with kind = SEG});
                   Model.make_def xcard (Model.make_any {t with kind = INT CARD});
                   Model.make_def xobj
                     (Model.make_pat {kind = OBJ (`Sprite,nocolor); ndim = ndim+1} Obj
                        [| Model.make_def xpos (Model.make_any {kind = VEC POS; ndim = ndim+1});
                           Model.make_def xg1 (Model.make_any {kind = GRID (`Sprite,nocolor); ndim = ndim+1}) |]);
                   Model.make_def xmerger (Model.make_derived {t with kind = OBJ (`Sprite,nocolor)}) |],
              varseq)
             :: refs
           else refs in
         let refs = (* Objects - SameColor *)
           if filling <> `Full && not nocolor then
             let xsize, varseq = Refining.new_var varseq in
             let xsize_i, varseq = Refining.new_var varseq in
             let xsize_j, varseq = Refining.new_var varseq in
             let xcard, varseq = Refining.new_var varseq in
             let xloop, varseq = Refining.new_var varseq in
             let xobj, varseq = Refining.new_var varseq in
             let xpos, varseq = Refining.new_var varseq in
             let xpos_i, varseq = Refining.new_var varseq in
             let xpos_j, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             let xg1_color, varseq = Refining.new_var varseq in
             let xg1_mask, varseq = Refining.new_var varseq in
             let xmerger, varseq = Refining.new_var varseq in
             let nmax = 9 in
             (Model.make_pat t (Objects (nmax, `SameColor))
                [| Model.make_def xsize (Model.make_any {t with kind = VEC SIZE});
                   Model.make_expr {t with kind = SEG}
                     (Expr.Const ({t with kind = SEG}, `Seg GPat.Objects.SameColor));
                   Model.make_def xcard (Model.make_any {t with kind = INT CARD});
                   Model.make_def xobj
                     (Model.make_pat {kind = OBJ (`Sprite,nocolor); ndim = ndim+1} Obj
                        [| Model.make_def xpos (Model.make_any {kind = VEC POS; ndim = ndim+1});
                           Model.make_def xg1
                             (Model.make_pat {kind = GRID (`Sprite,nocolor); ndim = ndim+1} Monocolor
                                [| Model.make_def xg1_color (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+1});
                                   Model.make_def xg1_mask (Model.make_any {kind = GRID (filling,true); ndim = ndim+1}) |]) |]);
                   Model.make_def xmerger (Model.make_derived {t with kind = OBJ (`Sprite,nocolor)}) |],
              varseq)
             :: refs
           else refs in
         (*let refs = (* ColorPartition *) (* too eager *)
           if not nocolor then
             let xsize, varseq = Refining.new_var varseq in
             let xsize_i, varseq = Refining.new_var varseq in
             let xsize_j, varseq = Refining.new_var varseq in
             let xloop, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             let xg1_color, varseq = Refining.new_var varseq in
             let xg1_mask, varseq = Refining.new_var varseq in
             (make_colorpartition filling
                (Model.make_def xsize
                   (make_vec SIZE
                      (Model.make_def xsize_i (make_anycoord I SIZE))
                      (Model.make_def xsize_j (make_anycoord J SIZE))))
                (Model.make_loop xloop (Range.make_closed 1 Grid.nb_color)
                   (Model.make_def xg1
                      (make_monocolor
                         (Model.make_def xg1_color (make_anycolor C_OBJ))
                         (Model.make_def xg1_mask (make_anygrid (filling,true)))))),
              varseq)
             ::refs
           else refs in*)
         let refs = (* Monocolor *)
           if not nocolor then
             let xcol, varseq = Refining.new_var varseq in
             let xmask, varseq = Refining.new_var varseq in
             let mmask, varseq =
               if filling = `Full
               then (* a monocolor full grid must have a full mask of some size *)
                 let msize, varseq =
                   let xsize, varseq = Refining.new_var varseq in
                   let xsize_i, varseq = Refining.new_var varseq in
                   let xsize_j, varseq = Refining.new_var varseq in
                   Model.make_def xsize (Model.make_any {t with kind = VEC SIZE}),
                   varseq in
                 Model.make_pat {t with kind = GRID (`Sprite,true)} Full [|msize|], varseq
               else
                 Model.make_any {t with kind = GRID (filling,true)}, varseq in
             (Model.make_pat t Monocolor
                [| Model.make_def xcol (Model.make_any {t with kind = COLOR C_OBJ});
                   Model.make_def xmask mmask |],
              varseq)
             :: refs
           else refs in
         let refs = (* Recoloring *)
           if not nocolor then
             let xmap, varseq = Refining.new_var varseq in
             let xg1s =
               Mymap.fold
                 (fun x tx res ->
                   match tx.kind with
                   | GRID (_,false) when tx.ndim <= ndim -> (x,tx)::res
                   | _ -> res)
                 env_vars [] in
             let eg1s =
               let vg1_res =
                 Ndseq.map_result 0
                   (function
                    | `Grid g ->
                       let| g1, _ = Grid_patterns.recoloring g in
                       Result.Ok (`Grid g1)
                    | _ -> Result.Error (Invalid_argument "refinement: Recoloring"))
                   value in
               match vg1_res with
               | Result.Ok vg1 -> [Expr.Const (t, vg1)]
               | _ -> [] in
             let eg1s =
               List.fold_left
                 (fun res (xg1,tg1) ->
                   let rg1 = Expr.Ref (tg1, xg1) in
                   rg1
                   (* :: Expr.Apply (t, `Index_1 [Some 0], [|rg1|])
                   :: Expr.Apply (t, `Index_1 [Some (-1)], [|rg1|]) *) (* need to know var dim *)
                   :: res)
                 eg1s xg1s in
             let$ refs, eg1 = refs, eg1s in
             (Model.make_pat t Recoloring
                [| Model.make_expr (Expr.typ eg1) eg1;
                   Model.make_def xmap (Model.make_any {t with kind = MAP (COLOR C_OBJ, COLOR C_OBJ)}) |],
              varseq)
             :: refs
           else refs in
         let refs = (* MotifMulti *)
           let t_mask = {t with kind = GRID (`Sprite,true)} in
           let xmot, varseq = Refining.new_var varseq in
           let xcore, varseq = Refining.new_var varseq in
           let xpure, varseq = Refining.new_var varseq in
           let xmask, varseq = Refining.new_var varseq in
           let xnoise, varseq = Refining.new_var varseq in
           let$ refs, partial = refs, (match filling with
                                       | `Full -> [false]
                                       | _ -> [false; true]) in
           (Model.make_pat t (MotifMulti partial)
              [| Model.make_def xmot (Model.make_any {t with kind = MOTIF MULTI});
                 Model.make_def xcore (Model.make_any {t with kind = GRID ((if filling = `Noise then `Sprite else filling), nocolor)});
                 Model.make_def xpure (Model.make_derived t);
                 (if partial
                  then Model.make_def xmask (Model.make_any t_mask)
                  else Model.make_expr t_mask (Expr.Const (t_mask, `Null)));
                 Model.make_def xnoise (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |],
            varseq)
           :: refs in
         let refs = (* MotifBi *)
           let t_mask = {t with kind = GRID (`Sprite,true)} in
           let xmot, varseq = Refining.new_var varseq in
           let xbgcolor, varseq = Refining.new_var varseq in
           let xcolor, varseq = Refining.new_var varseq in
           let xpure, varseq = Refining.new_var varseq in
           let xmask, varseq = Refining.new_var varseq in
           let xnoise, varseq = Refining.new_var varseq in
           let$ refs, partial = refs, (match filling with
                                       | `Full -> [false]
                                       | _ -> [false; true]) in
           (Model.make_pat t (MotifBi partial)
              [| Model.make_def xmot (Model.make_any {t with kind = MOTIF BI});
                 Model.make_def xbgcolor (Model.make_any {t with kind = COLOR (C_BG (filling = `Full))});
                 Model.make_def xcolor (Model.make_any {t with kind = COLOR C_OBJ});
                 Model.make_def xpure (Model.make_derived t);
                 (if partial
                  then Model.make_def xmask (Model.make_any t_mask)
                  else Model.make_expr t_mask (Expr.Const (t_mask, `Null)));
                 Model.make_def xnoise (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |],
            varseq)
           :: refs in
         let refs = (* Metagrid *)
           let xsepcolor, varseq = Refining.new_var varseq in
           let xborders, varseq = Refining.new_var varseq in
           let xdims, varseq = Refining.new_var varseq in
           let xk, varseq = Refining.new_var varseq in
           let xl, varseq = Refining.new_var varseq in
           let xl_heights, varseq = Refining.new_var varseq in
           let xheight, varseq = Refining.new_var varseq in
           let xl_widths, varseq = Refining.new_var varseq in
           let xwidth, varseq = Refining.new_var varseq in
           let xl_i, varseq = Refining.new_var varseq in
           let xl_j, varseq = Refining.new_var varseq in
           let xg1, varseq = Refining.new_var varseq in
           (Model.make_pat t Metagrid
              [| Model.make_def xsepcolor (Model.make_any {t with kind = COLOR (C_BG (filling = `Full))});
                 Model.make_def xborders (Model.make_any {t with kind = GRID (`Sprite,true)});
                 Model.make_def xdims (Model.make_any {t with kind = VEC SIZE});
                 Model.make_def xheight (Model.make_any {kind = INT (COORD (I, SIZE)); ndim = ndim+1});
                 Model.make_def xwidth (Model.make_any {kind = INT (COORD (J, SIZE)); ndim = ndim+1});
                 Model.make_def xg1 (Model.make_any {t with ndim = ndim+2})|],
            varseq)
           :: refs in
         (* let refs = (* Repeat - too catchy, replaced by function *)
           let xg1, varseq = Refining.new_var varseq in
           let xli, varseq = Refining.new_var varseq in
           let xni, varseq = Refining.new_var varseq in
           let xlj, varseq = Refining.new_var varseq in
           let xnj, varseq = Refining.new_var varseq in
           (make_repeat tg
              (Model.make_def xg1 (make_anygrid (filling,nocolor)))
              (Model.make_loop xli (Range.make_open 1)
                 (Model.make_def xni (make_anycoord I SIZE)))
              (Model.make_loop xlj (Range.make_open 1)
                 (Model.make_def xnj (make_anycoord J SIZE))),
            varseq)
           ::refs in *)
         let refs = (* Masks *)
           let msize, varseq_msize =
             let xsize, varseq = Refining.new_var varseq in
             let xsize_i, varseq = Refining.new_var varseq in
             let xsize_j, varseq = Refining.new_var varseq in
             Model.make_def xsize (Model.make_any {t with kind = VEC SIZE}),
             varseq in
           (* TODO: consider casting functions rather than normalizing model type *)
           (Model.make_pat {t with kind = GRID (`Sprite,false)} Empty [|msize|], varseq_msize)
           :: (if nocolor then
                 (Model.make_pat {t with kind = GRID (`Sprite,true)} Full [|msize|], varseq_msize)
                 :: (Model.make_pat {t with kind = GRID (`Sprite,true)} Point [||], varseq)
                 :: refs
               else refs) in
         let refs = (* Line *)
           if filling <> `Full && nocolor then
             let xlen, varseq = Refining.new_var varseq in
             let xdir, varseq = Refining.new_var varseq in
             let xdir_i, varseq = Refining.new_var varseq in
             let xdir_j, varseq = Refining.new_var varseq in
             (Model.make_pat {t with kind = GRID (`Sprite,true)} Line
                [| Model.make_def xlen (Model.make_any {t with kind = INT (COORD (I, SIZE))});
                   Model.make_def xdir (Model.make_any {t with kind = VEC MOVE}) |],
              varseq)
             ::refs
           else refs in
         let refs = (* ColorSeq *)
           if filling = `Full && not nocolor then
             let xsize, varseq = Refining.new_var varseq in
             let xloop, varseq = Refining.new_var varseq in
             let xcol, varseq = Refining.new_var varseq in
             let$ refs, (dir,axis) = refs, [`H, J; `V, I] in
             (Model.make_pat t (ColorSeq dir)
                [| Model.make_def xsize (Model.make_any {t with kind = INT (COORD (axis, SIZE))});
                   Model.make_def xcol (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+1}) |],
              varseq)
             ::refs
           else refs in
         let refs = (* ColorMat *)
           if filling = `Full && not nocolor then
             let xsize, varseq = Refining.new_var varseq in
             let xh, varseq = Refining.new_var varseq in
             let xw, varseq = Refining.new_var varseq in
             let xloop1, varseq = Refining.new_var varseq in
             let xloop2, varseq = Refining.new_var varseq in
             let xcol, varseq = Refining.new_var varseq in
             (Model.make_pat t ColorMat
                [| Model.make_def xsize (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def xcol (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+2}) |],
              varseq)
             ::refs
           else refs in
         refs
      | OBJ _ -> rs
      | _ -> assert false    
    let refinements_pat ~env_vars (t : typ) (c : constr) (args : model array) (varseq : varseq) (value : value) : (model * varseq) list = (* QUICK *)
      []
    (* TODO: add SeqCons/SeqRepeat(m,m) but requires global change of depths for head model *) 
    let refinements_postprocessing t m =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    let prunings_value t v varseq =
      match t.kind, v with
      | _, `Null -> [] (* for when Null is used as a missing optional arg *)
      | INT ti, _ ->
         [ Model.make_any t, varseq ]
      | VEC tv, _ ->
         let x, varseq = Refining.new_var varseq in
         let y, varseq = Refining.new_var varseq in
         [ Model.make_any t, varseq ] 
      | COLOR tc, _ ->
         [ Model.make_any t, varseq ]
      | SEG, _ ->
         [ Model.make_any t, varseq ]
      | MOTIF tmot, _ ->
         [ Model.make_any t, varseq ]
      | GRID tg, _ ->
         [ Model.make_any t, varseq ]
      | OBJ tg, _ ->
      (*         [ Model.make_any t, varseq ] *)
         let xpos, varseq = Refining.new_var varseq in
         let xi, varseq = Refining.new_var varseq in
         let xj, varseq = Refining.new_var varseq in
         let xg1, varseq = Refining.new_var varseq in
         [ Model.make_pat t Obj
             [| Model.make_def xpos (Model.make_any {t with kind = VEC POS});
                Model.make_def xg1 (Model.make_any {t with kind = GRID tg}) |],
           varseq ]
      | MAP (ka,kb), _ ->
         [ Model.make_any t, varseq ]
      | _ -> pp_endline xp_typ t; pp_endline xp_value v; assert false
    let prunings_any ~env_vars t varseq value =
      []
    let prunings_pat ~env_vars t c args varseq value =
      let refs =
        match c with
        | SeqCons _ ->
           [Model.make_any t, varseq]
        | SeqRepeat _ -> (* TODO: how to relax the Repeat part, beware of not confusing depths *)
           [Model.make_any t, varseq]
        | _ -> [] in      
      match t.kind, c with
      | GRID tg, _ -> (Model.make_any t, varseq) :: refs
      | MAP (ka,kb), _ -> (Model.make_any t, varseq) :: refs
      | _ -> refs (* TODO: why not pruning for all types? *)
    let prunings_postprocessing t m =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    (* initialization *)
      
    let varseq0 : varseq = Myseq.range 1 max_int

    let get_init_config name task =
      let open Task_model in
      let env = Data.make_dany (`Color Grid.black) (`ColorTyp (Grid.black, C_BG true)) in (* dummy *)
      let varseq = varseq0 in
      let xi, varseq = Refining.new_var varseq in
      let xo, varseq = Refining.new_var varseq in
      let input_model = Model.make_def xi (Model.make_any (scalar (GRID (`Full,false)))) in
      let output_model = Model.make_def xo (Model.make_any (scalar (GRID (`Full,false)))) in
      let output_generator_info = `Grid ((1,Grid.max_size),(1,Grid.max_size),Grid.all_colors) in
      { env;
        varseq;
        input_model;
        output_model;
        output_generator_info }

    let log_reading r m ~status =
      (*print_endline "READING";
      pp_endline xp_refinement r;
      pp_endline xp_task_model m;
      flush stdout;*)
      ()
    let log_refining r m prs lmd lrido =
      Printf.printf "REF  %.3f  %.3f  " lmd lrido;
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
      (*reset_default_grid ();*)
      reset_make_index ()
  end

module MyMadil = Madil.Make(MyDomain)
