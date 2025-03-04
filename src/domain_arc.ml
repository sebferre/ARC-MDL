 
open Madil_common
open Arc_common

module GPat = Grid_patterns

let () = (* performance and debugging flags *)
  Printexc.record_backtrace true;
  Common.prof_on := true;
  Common.prof_logging := false;
  Arc_common.Memo.log_on := false (* BEWARE: true seems to neutralize timeout/memout *)

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
      | ORDER of bool (* nocolor *)
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
      | ORDER nocolor -> print#string "ORDER"
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
      | `Vec of int * int
      | `Color of Grid.color
      | `Seg of GPat.Objects.segmentation
      | `Order of GPat.Objects.order
      | `Motif of GPat.Motif.t
      | `Grid of Grid.t
      | `Obj of value * value (* position at (i,j) of the subgrid *)
      | `Map of (value,value) Mymap.t
      | value Ndseq.seq ]

    let rec xp_value ~html (print : Xprint.t) : value -> unit = function
      | `Null -> print#string "null"
      | `Bool b -> xp_bool ~html print b
      | `Int i -> xp_int ~html print i
      | `Vec (i,j) -> xp_vec xp_int xp_int ~html print i j
      | `Color c -> Grid.xp_color ~html print c
      | `Seg seg -> GPat.Objects.xp_segmentation ~html print seg
      | `Order order -> GPat.Objects.xp_order ~html print order
      | `Motif motif -> GPat.Motif.xp ~html print motif
      | `Grid g -> Grid.xp_grid ~html print g
      | `Obj (pos,g1) ->
         print#string "an object "; xp_value ~html print g1;
         print#string " at position "; xp_value ~html print pos
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
      | #Ndseq.seq as vs -> Ndseq.xp_seq xp_value ~html print vs

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

    (* value ranges, distributions *)

    type distrib =
      [ `Null (* the null value *)
      | `IntRange of Range.t
      | `VecRange of Range.t * Range.t
      | `ColorRange of typ_color * Grid.color list
      | `MotifRange of GPat.Motif.t list
      | `SegRange of GPat.Objects.segmentation list
      | `OrderRange of GPat.Objects.order list
      | `GridRange of typ_grid * Range.t * Range.t * Grid.color list (* height, width, colors *) (* TODO: consider removing typ_grid *)
      | `ObjRange of distrib * distrib (* pos, grid *)
      | `MapRange of distrib * distrib (* src, dst *)
      | distrib Ndseq.seq ]

    let rec xp_distrib ~html print : distrib -> unit = function
      | `Null -> print#string "null"
      | `IntRange ri -> print#string (Range.to_string ri)
      | `VecRange (ri,rj) ->
         print#string "("; print#string (Range.to_string ri);
         print#string ","; print#string (Range.to_string rj);
         print#string ")"
      | `ColorRange (tc,lc) ->
         xp_typ_kind ~html print (COLOR tc);
         print#string " in ";
         xp_list Grid.xp_color ~html print lc
      | `MotifRange lmot -> xp_list GPat.Motif.xp ~html print lmot
      | `SegRange lseg -> xp_list GPat.Objects.xp_segmentation ~html print lseg
      | `OrderRange lorder -> xp_list GPat.Objects.xp_order ~html print lorder
      | `GridRange (tg,rh,rw,lc) ->
         xp_typ_kind ~html print (GRID tg);
         print#string "(size ~ "; xp_distrib ~html print (`VecRange (rh,rw));
         print#string ", colors ~ "; xp_distrib ~html print (`ColorRange (C_OBJ, lc));
         print#string ")"
      | `ObjRange (rpos,rg1) ->
         print#string "OBJ(pos ~ "; xp_distrib ~html print rpos;
         print#string ", grid ~ "; xp_distrib ~html print rg1;
         print#string ")"
      | `MapRange (ra,rb) ->
         print#string "MAP(src ~ "; xp_distrib ~html print ra;
         print#string ", dst ~ "; xp_distrib ~html print rb;
         print#string ")"
      | #Ndseq.seq as rs -> Ndseq.xp_seq xp_distrib ~html print rs
    
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
      | Square (* COORD : VEC *)
      | Obj (* POS, SPRITE : OBJ *)
      | DomMap of value list (* B+ : MAP(A,B) *) (* fixed set of keys, assumed known from ctx *)
      | Replace (* A, A : MAP(A,A) *)
      | Swap (* A, A : MAP(A,A) *)
      | BgColor (* COLOR, SPRITE : GRID *)
      | IsFull (* SPRITE : GRID *)
      | Crop (* [SPRITE] POS, SIZE : SPRITE *)
      | Objects of int (* nmax *) * [`Connected|`SameColor] (* mode *) (* SIZE, SEG, ORDER, CARD, OBJ+, derived OBJ (merge), NOISE : SPRITE *) (* int is for max seq length, mode constrains SEG *)
      | ColorPartition (* SIZE, INT, COLOR+, MASK+ : SPRITE *)
      | Monocolor (* COLOR, MASK : SPRITE *)
      | Recoloring (* [SPRITE] MAP(COLOR,COLOR) : SPRITE *)
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
      | MakeGrid (* GRID : COLOR++ *)
      | SeqCons of int (* depth *) (* head:X^k-1, tail:X^k : X^k *)
      | SeqRepeat of int (* depth *) (* X^(k-1) : X^k *)
      | SeqRange (* start:INT, step:INT : INT+ *) (* TODO: add depth arg *)
      | SeqIndex (* [seq:X^n] index:INT^1 : X^(n-k) *)

    let xp_any t ~html print () =
      xp_html_elt "span" ~classe:"model-any" ~html print
        (fun () -> print#string "?")

    let xp_direction ~html print dir =
      print#string (match dir with `H -> "horizontal" | `V -> "vertical")
    
    let xp_pat c xp_src xp_args ~html print () =
      match c, xp_src, xp_args with
      | Vec, [||], [|xp_i; xp_j|] ->
         xp_vec xp_i xp_j ~html print () ()
      | Square, [||], [|xp_ij|] ->
         print#string "square("; xp_ij ~html print (); print#string ")"
      | Obj, [||], [|xp_pos; xp_sprite|] ->
         print#string "at position "; xp_pos ~html print ();
         print#string ": ";
         xp_sprite ~html print ()
      | DomMap keys, [||], [|xp_vals|] ->
         xp_list ~delims:("〈","〉") xp_value ~html print keys;
         print#string " -> ";
         xp_vals ~html print ()
      | Replace, [||], [|xp_a; xp_b|] ->
         xp_a ~html print ();
         print#string " is replaced by ";
         xp_b ~html print ()
      | Swap, [||], [|xp_a; xp_b|] ->
         xp_a ~html print ();
         print#string " is swapped with ";
         xp_b ~html print ()
      | BgColor, [||], [|xp_color; xp_sprite|] ->
         print#string "a grid with background color "; xp_color ~html print ();
         print#string " and with contents"; xp_newline ~html print ();
         xp_sprite ~html print ()
      | IsFull, [||], [|xp_sprite|] ->
         print#string "a full grid that is";
         xp_newline ~html print ();
         xp_sprite ~html print ()
      | Crop, [|xp_sprite|], [|xp_pos; xp_size|] ->
         print#string "the crop of "; xp_sprite ~html print ();
         print#string " at position "; xp_pos ~html print ();
         print#string " with size "; xp_size ~html print ()
         (*print#string "a grid of size "; xp_size ~html print ();
           print#string " that contains at position "; xp_pos ~html print ();
           xp_newline ~html print ();
           xp_sprite ~html print ()*)
      | Objects (nmax,_mode), [||], [|xp_size; xp_seg; xp_order; xp_card; xp_objs; xp_merger; xp_noise|] ->
         print#string "a grid of size "; xp_size ~html print ();
         print#string " that contains "; xp_card ~html print ();
         print#string " <= "; print#int nmax;
         print#string " "; xp_seg ~html print ();
         print#string " objects, ordered by "; xp_order ~html print ();
         xp_newline ~html print ();
         xp_objs ~html print ();
         print#string " forming the constellation object: ";
         xp_merger ~html print ();
         print#string "  plus the noise:";
         xp_newline ~html print ();
         xp_noise ~html print ()
      | ColorPartition, [||], [|xp_size; xp_ncol; xp_colors; xp_masks|] ->
         print#string "a grid of size "; xp_size ~html print ();
         print#string " that is composed of "; xp_ncol ~html print ();
         print#string " layers with colors "; xp_colors ~html print ();
         print#string ", and masks:";
         xp_newline ~html print ();
         xp_masks ~html print ()
      | Monocolor, [||], [|xp_color; xp_mask|] ->
         print#string "a grid with only color "; xp_color ~html print ();
         print#string " and with mask"; xp_newline ~html print ();
         xp_mask ~html print ()
         (* let xp_recoloring xp_colors xp_grid ~html print () =
            print#string "recoloring with "; xp_colors ~html print ();
            xp_newline ~html print ();
            xp_grid ~html print () *)
      | Recoloring, [|xp_grid|], [|xp_map|] ->
         print#string "a recoloring of "; xp_grid ~html print ();
         xp_newline ~html print ();
         print#string "where "; xp_map ~html print ()
      | MotifMulti partial, [||], [|xp_mot; xp_core; xp_pure; xp_mask_opt; xp_noise|] ->
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
      | MotifBi partial, [||], [|xp_mot; xp_bgcolor; xp_color; xp_pure; xp_mask_opt; xp_noise|] ->
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
      | Metagrid, [||], [|xp_sepcolor; xp_borders; xp_dims; xp_heights; xp_widths; xp_gridss|] ->
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
      | Repeat, [||], [|xp_grid; xp_nis; xp_njs|] ->
         print#string "a repeat pattern on rows "; xp_nis ~html print ();
         print#string " and on columns "; xp_njs ~html print ();
         print#string " of grid: "; xp_grid ~html print ()
      | Empty, [||], [|xp_size|] ->
         print#string "an empty mask of size "; xp_size ~html print ()
      | Full, [||], [|xp_size|] ->
         print#string "a full mask of size "; xp_size ~html print ()
      | Point, [||], [||] ->
         print#string "a point mask"
      | Line, [||], [|xp_len; xp_dir|] ->
         print#string "a line of length "; xp_len ~html print ();
         print#string " and direction "; xp_dir ~html print ()
      | ColorSeq dir, [||], [|xp_size; xp_colors|] ->
         print#string "a ";
         xp_direction ~html print dir;
         print#string " 1D grid with size "; xp_size ~html print ();
         print#string " and colors: ";
         xp_colors ~html print ()
      | ColorMat, [||], [|xp_size; xp_colorss|] ->
         print#string "a 2D grid with size "; xp_size ~html print ();
         print#string " and colors: ";
         xp_colorss ~html print ()
      | MakeGrid, [||], [|xp_grid|] ->
         print#string "as grid:";
         xp_newline ~html print ();
         xp_grid ~html print ()
      | SeqCons depth, [||], [|xp_hd; xp_tl|] ->
         print#string ("Cons[" ^ string_of_int depth ^ "]");
         xp_tuple2 xp_hd xp_tl ~html print ((),())
      | SeqRepeat depth, [||], [|xp_e|] ->
         print#string ("Repeat[" ^ string_of_int depth ^ "](");
         xp_e ~html print (); print#string ")"
      | SeqRange, [||], [|xp_start; xp_step|] ->
         print#string "Range";
         xp_tuple2 xp_start xp_step ~html print ((),())
      | SeqIndex, [|xp_seq|], [|xp_index|] ->
         print#string "Index";
         xp_tuple2 xp_seq xp_index ~html print ((),())
      | _ -> assert false

    let xp_field ~html print = function
      | Vec, 0 -> print#string "i"
      | Vec, 1 -> print#string "j"
      | Vec, _ -> assert false
      | Square, 0 -> print#string "ij"
      | Square, _ -> assert false
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
      | Crop, 0 -> print#string "pos"
      | Crop, 1 -> print#string "size"
      | Crop, _ -> assert false
      | Objects _, 0 -> print#string "size"
      | Objects _, 1 -> print#string "seg"
      | Objects _, 2 -> print#string "order"
      | Objects _, 3 -> print#string "card"
      | Objects _, 4 -> print#string "obj"
      | Objects _, 5 -> print#string "merger"
      | Objects _, 6 -> print#string "noise"
      | Objects _, _ -> assert false
      | ColorPartition, 0 -> print#string "size"
      | ColorPartition, 1 -> print#string "ncol"
      | ColorPartition, 2 -> print#string "colors"
      | ColorPartition, 3 -> print#string "masks"
      | ColorPartition, _ -> assert false
      | Monocolor, 0 -> print#string "color"
      | Monocolor, 1 -> print#string "mask"
      | Monocolor, _ -> assert false
      | Recoloring, 0 -> print#string "colormap"
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
      | MakeGrid, 0 -> print#string "grid"
      | MakeGrid, _ -> assert false
      | SeqCons _, 0 -> print#string "head"
      | SeqCons _, 1 -> print#string "tail"
      | SeqCons _, _ -> assert false
      | SeqRepeat _, 0 -> print#string "elt"
      | SeqRepeat _, _ -> assert false
      | SeqRange, 0 -> print#string "start"
      | SeqRange, 1 -> print#string "step"
      | SeqRange, _ -> assert false
      | SeqIndex, 0 -> print#string "index"
      | SeqIndex, _ -> assert false
    
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
      | `Left_1 (* on Obj, Grid *)
      | `Right_1 (* on Obj, Grid *)
      | `Center_1 (* on Obj, Grid *)
      | `Top_1 (* on Obj, Grid *)
      | `Bottom_1 (* on Obj, Grid *)
      | `Middle_1 (* on Obj, Grid *)
      | `MiddleCenter_1 (* on Obj, Grid *)
      | `ProjI_1 (* on Vec *)
      | `ProjJ_1 (* on Vec *)
      | `MaskOfGrid_1 (* Sprite -> Mask *)
      | `GridOfMask_2 (* Mask, Color -> Grid *)
      | `Tiling_1 of int * int (* on Vec/Mask/Shape *)
      | `Border_1 (* on Grid, Object *)
      | `Interior_1 (* on Grid, Object *)
      | `DNeighbors_1 (* on Grid, Object *)
      | `INeighbors_1 (* on Grid, Object *)
      | `Neighbors_1 (* on Grid, Object *)
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
      | `MostCommon_1 (* X^k -> X *)
      | `LeastCommon_1 (* X^k -> X *)
      | `LogAnd_1 (* Mask^k -> Mask *)
      | `LogOr_1 (* Mask^k -> Mask *)
      | `LogXOr_1 (* Mask^k -> Mask *)
      | `GridOfColorSeq_1 of direction (* Color^k -> Grid^(k-1) *)
      | `GridOfColorMat_1 (* Color^k -> Grid^(k-2) *)
      | `Colors_1 (* Grid -> Color^1, in decreasing frequency *)
      | `Halves_1 of direction (* Grid^k -> Grid^(k+1) *)
      | `Quadrants_1 (* Grid^k -> Grid^(k+2) *)
      | `RelativePos_1 (* Obj^k -> Pos^(k+1) *)
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
      | `MostCommon_1 -> print#string "most_common"
      | `LeastCommon_1 -> print#string "least_common"
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
      | `Quadrants_1 -> print#string "quadrants"
      | `ProjI_1 -> print#string "projI"
      | `ProjJ_1 -> print#string "projJ"
      | `MaskOfGrid_1 -> print#string "maskOfGrid"
      | `GridOfMask_2 -> print#string "gridOfMask"
      | `GridOfColorSeq_1 dir -> print#string "gridOfColorSeq["; xp_direction ~html print dir; print#string "]"
      | `GridOfColorMat_1 -> print#string "gridOfColorMat"
      | `RelativePos_1 -> print#string "relativePos"
      | `TranslatedOnto_1 -> print#string "translatedOnto"
      | `Tiling_1 (k,l) ->
         print#string "tiling";
         xp_tuple2 ~delims:("[","]") xp_int xp_int ~html print (k,l)
      | `Border_1 -> print#string "border"
      | `Interior_1 -> print#string "interior"
      | `DNeighbors_1 -> print#string "dneighbors"
      | `INeighbors_1 -> print#string "ineighbors"
      | `Neighbors_1 -> print#string "neighbors"
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
      | `Colors_1 -> print#string "colors"
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
        inherit [typ,typ] Model.asd
        method abstract t = {t with ndim = 0} (* ignoring ndim to avoid infinite recursion *)
        method pats t (* abstract *) =
          (* synchronize with is_default_constr *)
          assert (t.ndim = 0);
          let res =
            [ "SeqCons", [||], [|t; t|];
              "SeqRepeat", [||], [|t|];
              "SeqIndex", [||], [|t; scalar (INT INDEX)|] ] in
          match t.kind with
          | BOOL -> res
          | INT ti ->
             ("SeqRange", [||], [|t; {t with kind = INT (COORD (I, MOVE))} |])
             :: res
          | VEC tv ->
             ("Vec", [||], [| {t with kind = INT (COORD (I, tv))};
                              {t with kind = INT (COORD (J, tv))} |])
             ::("Square", [||], [| {t with kind = INT (COORD (I, tv))} |])
             :: res
          | COLOR tc ->
             (* let filling =
               match tc with
               | C_OBJ | C_BG true -> `Full
               | C_BG false -> `Sprite in *)
             ("MakeGrid", [||], [| {t with kind = GRID (`Sprite, false)} |])
             :: res
          | SEG -> res
          | ORDER nocolor -> res
          | MOTIF tm -> res
          | GRID (filling,nocolor) ->
             let full = (filling = `Full) in
             List.fold_left
               (fun res (cond,c_args) ->
                 if cond
                 then c_args::res
                 else res)
               res
               [ full, ("BgColor", [||],
                        [| {t with kind = COLOR (C_BG full)};
                           {t with kind = GRID (`Sprite,nocolor)} |]);
                 not full, ("IsFull", [||], [| {t with kind = GRID (`Full,nocolor)} |]);
                 true, ("Crop",
                        [| {t with kind = GRID (filling,nocolor)} |],
                        [| {t with kind = VEC POS};
                           {t with kind = VEC SIZE} |]);
                 not full, ("Objects", [||],
                            [| {t with kind = VEC SIZE};
                               {t with kind = SEG};
                               {t with kind = ORDER nocolor};
                               {t with kind = INT CARD};
                               {t with kind = OBJ (`Sprite,nocolor)};
                               (* derived merger, not counting *)
                               {t with kind = GRID (`Noise,nocolor)} |]);
                 (* not nocolor, (ColorPartition, [||],
                               [| {t with kind = VEC SIZE};
                                  {t with kind = INT CARD};
                                  {t with kind = COLOR C_OBJ};
                                  {t with kind = GRID (`Sprite,true)} |]); *)
                 not nocolor, ("Monocolor", [||],
                               [| {t with kind = COLOR C_OBJ};
                                  {t with kind = GRID (filling,true)} |]);
                 not nocolor, ("Recoloring",
                               [| {t with kind = GRID (filling,nocolor)} |],
                               [| {t with kind = MAP (COLOR C_OBJ, COLOR C_OBJ)} |]);
                 true, ("MotifMulti", [||],
                        [| {t with kind = MOTIF MULTI};
                           {t with kind = GRID ((if filling = `Noise then `Sprite else filling), nocolor)};
                          (* derived pure, not counting *)
                           {t with kind = GRID (`Sprite,true)}; (* TODO: encode optional *)
                           {t with kind = GRID (`Noise,nocolor)} |]);
                 (*true, ("Repeat", [|GRID (filling,nocolor);
                                  INT (COORD (I, SIZE));
                                  INT (COORD (J, SIZE))|]);*)
                 true, ("MotifBi", [||],
                        [| {t with kind = MOTIF BI};
                           {t with kind = COLOR (C_BG full)};
                           {t with kind = COLOR C_OBJ};
                           (* derived pure, not counting *)
                           {t with kind = GRID (`Sprite,true)}; (* TODO: encode optional *)
                           {t with kind = GRID (`Noise,nocolor)} |]);
                 (*true, (Repeat, [|GRID (filling,nocolor);
                                  INT (COORD (I, SIZE));
                                  INT (COORD (J, SIZE))|]);*)
                 true, ("Metagrid", [||],
                        [| {t with kind = COLOR (C_BG full)};
                           {t with kind = GRID (`Noise,true)};
                           {t with kind = VEC SIZE};
                           {t with kind = INT (COORD (I,SIZE))};
                           {t with kind = INT (COORD (J,SIZE))};
                           {t with kind = GRID (filling,nocolor)} |]);
                 not full (*&& nocolor*), ("Empty", [||], [| {t with kind = VEC SIZE} |]);
                 not full && nocolor, ("Full", [||], [| {t with kind = VEC SIZE} |]);
                 not full && nocolor, ("Point", [||], [||]);
                 not full && nocolor, ("Line", [||],
                                       [| {t with kind = INT (COORD (I, SIZE))};
                                          {t with kind = VEC MOVE} |]);
                 full && not nocolor, ("ColorSeq", [||],
                                       [| {t with kind = INT (COORD (I,SIZE))};
                                          {t with kind = COLOR C_OBJ} |]);
                 full && not nocolor, ("ColorMat", [||],
                                       [| {t with kind = VEC SIZE};
                                          {t with kind = COLOR C_OBJ} |]) ]
          | OBJ tg ->
             ("Obj", [||],
              [| {t with kind = VEC POS};
                 {t with kind = GRID tg} |])
             :: res
          | MAP (ka,kb) ->
             List.fold_left
               (fun res (cond,c_args) ->
                 if cond
                 then c_args::res
                 else res)
               res
               [ true, ("DomMap", [||], [| {t with kind = kb} |]);
                 ka=kb, ("Replace", [||],
                         [| {t with kind = ka};
                            {t with kind = ka} |]);
                 ka=kb, ("Swap", [||],
                         [| {t with kind = ka};
                            {t with kind = ka} |]) ]
        method funcs t (* abstract *) =
          assert (t.ndim = 0);
          let res =
            [ "Cast_1", [|t|];
              "MostCommon_1", [|t|];
              "LeastCommon_1", [|t|];
              "Index_1", [|t|];
              "Flatten_1", [|t|];
              "Tail_1", [|t|];
              "Reverse_1", [|t|];
              "Rotate_1", [|t|];
              "Transpose_1", [|t|] ] in
          match t.kind with
          | BOOL -> res
          | INT CARD ->
             ("Cardinal_1", [| {t with kind = OBJ (`Sprite,false)} |]) (* TODO: generalize to other kinds, and other ndims, param and result *)
             ::("Sum_1", [|t|])
             ::("Min_1", [|t|])
             ::("Max_1", [|t|])
             ::("Plus_2", [|t (* const *)|])
             ::("Minus_2", [|t (* const *)|])
             ::("Area_1", [| {t with kind = GRID (`Sprite,false)} |])
             ::("ColorCount_1", [| {t with kind = GRID (`Sprite,false)} |]) (* also for `Noise? *)
             ::("Average_n", [|t; t|])
             ::res
          | INT INDEX ->
             ("Sum_1", [|t|])
             ::("Min_1", [|t|])
             ::("Max_1", [|t|])
             ::("ArgMin_1", [| {t with kind = INT CARD} |]) (* TODO: should be any INT, except maybe INDEX *)
             ::("ArgMax_1", [| {t with kind = INT CARD} |]) (* TODO: should be any INT, except maybe INDEX *)
             ::res
          | INT (COORD (axis,tv)) ->
             ("Sum_1", [|t|])
             ::("Min_1", [|t|])
             ::("Max_1", [|t|])
             ::("I_1", [| {t with kind = VEC tv} |])
             ::("J_1", [| {t with kind = VEC tv} |])
             ::("Left_1", [| {t with kind = OBJ (`Sprite,false) } |])
             ::("Right_1", [| {t with kind = OBJ (`Sprite,false) } |])
             ::("Center_1", [| {t with kind = OBJ (`Sprite,false) } |])
             ::("Top_1", [| {t with kind = OBJ (`Sprite,false) } |])
             ::("Bottom_1", [| {t with kind = OBJ (`Sprite,false) } |])
             ::("Middle_1", [| {t with kind = OBJ (`Sprite,false) } |])
             ::("Right_1", [| {t with kind = GRID (`Sprite,false) } |])
             ::("Center_1", [| {t with kind = GRID (`Sprite,false) } |])
             ::("Bottom_1", [| {t with kind = GRID (`Sprite,false) } |])
             ::("Middle_1", [| {t with kind = GRID (`Sprite,false) } |])
             ::("IJTranspose_1", [| {t with kind = INT (COORD (axis_transpose axis, tv))} |])
             ::("Direction_1", [|t|])
             ::("Abs_1", [|t|])
             ::("AsTVec_1", [| {t with kind = INT (COORD (axis, tv))} |]) (* should be any other tv *)
             ::("Area_1", [| {t with kind = GRID (`Sprite,false)} |])
             ::("Plus_2", [|t (* const *)|])
             ::("Minus_2", [|t (* const *)|])
             ::("ScaleUp_2", [|t (* const: {t with kind = INT CARD} *) |])
             ::("ScaleDown_2", [|t (* const: {t with kind = INT CARD} *) |])
             ::res
          | VEC tv ->
             ("Pos_1", [| {t with kind = OBJ (`Sprite,false)} |])
             ::("MiddleCenter_1", [| {t with kind = OBJ (`Sprite,false) } |])
             ::("MiddleCenter_1", [| {t with kind = GRID (`Sprite,false) } |])
             ::("Size_1", [| {t with kind = GRID (`Sprite,false)} |])
             ::("Plus_2", [|t (* const: t *)|])
             ::("Minus_2", [|t (* const: t *)|])
             ::("ScaleUp_2", [|t (* const: {t with kind = INT CARD} *) |])
             ::("ScaleDown_2", [|t (* const: {t with kind = INT CARD} *) |])
             ::("ProjI_1", [|t|])
             ::("ProjJ_1", [|t|])
             ::("IJTranspose_1", [|t|])
             ::("Direction_1", [|t|])
             ::("Abs_1", [|t|])
             ::("AsTVec_1", [| {t with kind = VEC tv} |]) (* should be any other tv *)
             ::("RelativePos_1", [| {t with kind = OBJ (`Sprite,false)} |])
             ::("TranslatedOnto_1", [| {t with kind = OBJ (`Sprite,false)} |])
             (* ::("TranslationSym_2", [| {t with kind = OBJ (`Sprite,false)};
                                          {t with kind = GRID (`Sprite,false)} |]) *)
             (* ::("ApplySymVec_1", [|t|]) *)
             (* ::("Tiling_1", [|t|]) *)
             ::res
          | COLOR tc ->
             ("Colors_1", [| {t with kind = GRID (`Sprite,false)} |])
             ::("MajorityColor_1", [| {t with kind = GRID (`Sprite,false)}|]) (* also `Full and `Noise *)
             ::("MinorityColor_1", [| {t with kind = GRID (`Sprite,false)} |]) (* also `Full and `Noise *)
             ::res
          | SEG -> res
          | ORDER _ -> res
          | MOTIF tm -> res
          | GRID (filling,nocolor) ->
             (*let full = (filling = `Full) in*)
             ("Grid_1", [| {t with kind = OBJ (filling,nocolor)} |])
             ::("Halves_1", [|t|])
             ::("Quadrants_1", [|t|])
             ::("MaskOfGrid_1", [| {t with kind = OBJ (`Sprite,false)} |])
             ::("GridOfColorSeq_1", [| {t with kind = COLOR C_OBJ} |])
             ::("GridOfColorMat_1", [| {t with kind = COLOR C_OBJ} |])
             ::("ScaleUp_2", [|t (* const:{t with kind = INT CARD} *) |])
             ::("ScaleDown_2", [|t (* const: {t with kind = INT CARD} *) |])
             (* ::("ScaleTo_2", [|t; {t with kind = VEC SIZE} |]) *)
             (* ::("PeriodicFactor_2", [| {t with kind = COLOR (C_BG full)}; t|]) *)
             (* ::("Crop_2", [| {t with kind = GRID (`Full,false)};
                            {t with kind = OBJ (`Sprite,false)} |]) *)
             ::("ApplySymGrid_1", [|t|])
             (* ::("Coloring_2", [|t; {t with kind = COLOR C_OBJ} |]) *)
             ::("Border_1", [|t|])
             ::("Interior_1", [|t|])
             ::("DNeighbors_1", [|t|])
             ::("INeighbors_1", [|t|])
             ::("Neighbors_1", [|t|])
             ::("Unrepeat_1", [|t|])
             (* ::("FillResizeAlike_3", [| {t with kind = COLOR (C_BG full)};
                                                 {t with kind = VEC SIZE};
                                                 t |]) *)
             ::("SelfCompose_3", [| (* const: {t with kind = COLOR (C_BG full)};*)
                  (* const: {t with kind = COLOR C_OBJ};*)
                                   t |])
             ::("SelfCompose_3", [| (* const: {t with kind = COLOR (C_BG full)};*)
                                   {t with kind = COLOR C_OBJ};
                                   t |])
             (* ::("UnfoldSym_1", [|t|]) *)
             ::("CloseSym_2", [| (* const: {t with kind = COLOR (C_BG full)};*) t|])
             (* ::("SwapColors_3", [|t; {t with kind = COLOR C_OBJ}; {t with kind = COLOR C_OBJ} |]) *)
             (* ::("Stack_n", [|t; t|]) *)
             (* on masks *)
             ::("LogNot_1", [|t|])
             ::("LogAnd_1", [|t|])
             ::("LogOr_1", [|t|])
             ::("LogXOr_1", [|t|])
             ::res
          | OBJ (filling,nocolor) ->
             (*let full = (filling = `Full) in*)
             (* ("PeriodicFactor_2", [| {t with kind = COLOR (C_BG full)}; t |]) *)
             (* ::("FillResizeAlike_3", [| {t with kind = COLOR (C_BG full)};
                                                 {t with kind = VEC SIZE};
                                                 t |]) *)
             (* ::("ApplySymGrid_1", [|t|]) *)
             (* ::("UnfoldSym_1", [|t|]) *)
             (* ::("CloseSym_2", [| {t with kind = COLOR (C_BG full)}; t |]) *)
             ("Strip_1", [| {t with kind = GRID (filling,nocolor)} |])
             ::("Border_1", [|t|])
             ::("Interior_1", [|t|])
             ::("DNeighbors_1", [|t|])
             ::("INeighbors_1", [|t|])
             ::("Neighbors_1", [|t|])
             ::res
          | MAP (ka,kb) -> res
        
        method expr_opt t = true
        method alt_opt t = false (* LATER *)
      end

    (* model processing *)
      
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
    let max_refinement_steps = def_param "max_refinement_steps" 4 string_of_int (* max nb of refinements steps into a single refined model, for decompositions *)
    let max_refinements = def_param "max_refinements" 100 string_of_int (* max nb of considered refinements *)
    let refinement_branching = def_param "refinement_branching" 3 string_of_int (* max nb of explored pattern refinements at some model path during learning (refining phase). min=1 *)
    let input_branching = def_param "input_branching" 10 string_of_int (* max nb of explored input models during output model learning (refining phase). min=1 *)
    let solution_pool = def_param "solution_pool" 3 string_of_int (* max nb of solutions before choosing best one *)
    let search_temperature = def_param "search_temperature" 1. string_of_float (* DEPRECATED by MCTS approach - to control choice of model to jump to and refine, based on softmax: base-2 log, values between -2. and 0. *)

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

    let make_color_partition ~depth vsize vcolors vmasks : value Myseq.t =
      Ndseq.map_tup_myseq ~depth 0
        (function
         | `Vec (h,w), seq_colors, seq_masks ->
            let colors =
              match Ndseq.as_seq seq_colors with
              | Some (_,colors) ->
                 List.map
                   (function
                    | `Color c -> c
                    | _ -> assert false)
                   colors
              | _ -> assert false in
            let masks =
              match Ndseq.as_seq seq_masks with
              | Some (_,masks) ->
                 List.map
                   (function
                    | `Grid m -> m
                    | _ -> assert false)
                   masks
              | _ -> assert false in
            assert (List.length colors = List.length masks);
            if true || List.for_all (fun m -> Grid.dims m = (h,w)) masks
            then (
              let g = Grid.make h w Grid.transparent in
              List.iter2
                (fun c m ->
                  Grid.add_grid_at g 0 0 (Grid.Mask.to_grid m Grid.transparent c))
                colors masks;
              Myseq.return (`Grid g))
            else Myseq.empty
         | _ -> assert false)
        (vsize, vcolors, vmasks)
    
    let make_objects_v_dmerger ~depth dsize dseg dorder dcard dobjs dnoise : value * data =
      let v, merger, rmerger = (* TODO: use dseg and dorder *)
        Ndseq.map_tup ~depth (0,0,0)
          (function
           | `Vec (h, w), `Int card, seq_objs, `Grid g_noise ->
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
              Grid.add_grid_at g 0 0 g_noise;
              (`Grid g, `Obj (`Vec (i0,j0), `Grid g0), `Null) (* TODO: define better distrib for merger *)
           | _ -> assert false)
          (Data.value dsize, Data.value dcard, Data.value dobjs, Data.value dnoise) in
      v, Data.make_dexpr merger rmerger

    let make_motif_dpure ~depth dmot vcore dnoise : data Myseq.t =
      let* vpure, rpure =
        Ndseq.map_tup_myseq ~name:"make_motif_dpure" ~depth (0,0)
          (function
           | `Motif mot, `Grid g_core, `Grid g_noise ->
              let h, w = Grid.dims g_noise in
              let* g_pure = Myseq.from_result (GPat.Motif.make_grid h w mot g_core) in
              Myseq.return (`Grid g_pure, `Null) (* TODO: define better rpure *)
           | _ -> assert false)
          (Data.value dmot, vcore, Data.value dnoise) in
      Myseq.return (Data.make_dexpr vpure rpure)
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

    let make_grid_from_color_seq dir vcolors =
      let| acolors =
        match Ndseq.as_seq vcolors with
        | Some (0,lcolors) when lcolors <> [] ->
           let acolors = Array.of_list lcolors in
           array_map_result
             (function
              | `Color c -> Result.Ok c
              | _ -> Result.Error (Undefined_result "make_grid_from_color_seq: not a color"))
             acolors
        | _ -> Result.Error (Undefined_result "make_grid_from_color_seq: not a non-empty color seq") in
      let n = Array.length acolors in
      assert (n > 0);
      if n <= Grid.max_size
      then
        let g =
          match dir with
          | `H -> Grid.init 1 n (fun i j -> acolors.(j))
          | `V -> Grid.init n 1 (fun i j -> acolors.(i)) in
        Result.Ok g
      else Result.Error (Undefined_result "make_grid_from_color_seq: too large")
    
    let make_grid_from_color_seq_seq vcolorss =
      let| acolorss : Grid.color array array =
        match Ndseq.as_seq vcolorss with
        | Some (1, lcolorss) when lcolorss <> [] ->
           let acolorss = Array.of_list lcolorss in
           array_map_result
             (fun vcolors ->
               match Ndseq.as_seq vcolors with
               | Some (0,lcolors) when lcolors <> [] ->
                  let acolors = Array.of_list lcolors in
                  array_map_result
                    (function
                     | `Color c -> Result.Ok c
                     | _ -> Result.Error (Undefined_result "make_grid_from_color_seq_seq: not a color"))
                    acolors
               | _ -> Result.Error (Undefined_result "make_grid_from_color_seq_seq: not a non-empty color seq"))
             acolorss
        | _ -> Result.Error (Undefined_result "make_grid_from_color_seq_seq: not a non-empty color seq seq") in
      let h = Array.length acolorss in
      assert (h > 0);
      let w =
        Array.fold_left
          (fun res acolors -> min res (Array.length acolors))
          max_int acolorss in
      assert (w > 0);
      if h <= Grid.max_size && w <= Grid.max_size
      then
        let g = Grid.init h w (fun i j -> acolorss.(i).(j)) in
        Result.Ok g
      else Result.Error (Undefined_result "make_grid_from_color_seq_seq: too large")
    
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
          | [| `Obj (pos, _)|] -> Result.Ok (pos :> value)
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
             (*let| bgcolor = Grid.majority_color Grid.transparent g in*)
             let| i, j, _, _, g1 = Grid.Transf.strip Grid.transparent g Grid.transparent in
             Result.Ok (`Obj (`Vec (i,j), `Grid g1))
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
          | [| `Grid g |] -> Result.Ok (`Int 0)
          | _ -> Result.Error (Invalid_expr e))
      | `Right_1 ->
         (function
          | [| `Obj (`Vec (_, j), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             Result.Ok (`Int (j+w-1))
          | [| `Grid g |] ->
             let h, w = Grid.dims g in
             Result.Ok (`Int (w - 1))
          | _ -> Result.Error (Invalid_expr e))
      | `Center_1 ->
         (function
          | [| `Obj (`Vec (_, j), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             if w mod 2 = 0
             then Result.Error (Undefined_result "Center: no center, even width")
             else Result.Ok (`Int (j + w/2))
          | [| `Grid g |] ->
             let h, w = Grid.dims g in
             if w mod 2 = 0
             then Result.Error (Undefined_result "Center: no center, even width")
             else Result.Ok (`Int (w/2))
          | _ -> Result.Error (Invalid_expr e))
      | `Top_1 ->
         (function
          | [| `Obj (`Vec (i, _), _) |] -> Result.Ok (`Int i)
          | [| `Grid g |] -> Result.Ok (`Int 0)
          | _ -> Result.Error (Invalid_expr e))
      | `Bottom_1 ->
         (function
          | [| `Obj (`Vec (i, _), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             Result.Ok (`Int (i+h-1))
          | [| `Grid g |] ->
             let h, w = Grid.dims g in
             Result.Ok (`Int (h - 1))
          | _ -> Result.Error (Invalid_expr e))
      | `Middle_1 ->
         (function
          | [| `Obj (`Vec (i, _), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             if h mod 2 = 0
             then Result.Error (Undefined_result "Middle: no middle, even height")
             else Result.Ok (`Int (i + h/2))
          | [| `Grid g |] ->
             let h, w = Grid.dims g in
             if h mod 2 = 0
             then Result.Error (Undefined_result "Middle: no middle, even height")
             else Result.Ok (`Int (h/2))
          | _ -> Result.Error (Invalid_expr e))
      | `MiddleCenter_1 ->
         (function
          | [| `Obj (`Vec (i, j), `Grid shape)|] ->
             let h, w = Grid.dims shape in
             if h mod 2 = 0 || w mod 2 = 0
             then Result.Error (Undefined_result "MiddleCenter: no middle or no center, even height or width")
             else Result.Ok (`Vec (i + h/2, j + w/2))
          | [| `Grid g |] ->
             let h, w = Grid.dims g in
             if h mod 2 = 0 || w mod 2 = 0
             then Result.Error (Undefined_result "MiddleCenter: no middle or no center, even height or width")
             else Result.Ok (`Vec (h/2, w/2))
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
      | `Border_1 ->
         (function
          | [| `Grid g|] ->
             Result.Ok (`Grid (Grid.Transf.border Grid.transparent g))
          | [| `Obj (`Vec (i,j), `Grid g)|] ->
             let| i, j, g = Grid.Transf.border_at_pos Grid.transparent (i,j) g in
             Result.Ok (`Obj (`Vec (i,j), `Grid g))
          | _ -> Result.Error (Invalid_expr e))
      | `Interior_1 ->
         (function
          | [| `Grid g|] ->
             Result.Ok (`Grid (Grid.Transf.interior Grid.transparent g))
          | [| `Obj (`Vec (i,j), `Grid g)|] ->
             let| i, j, g = Grid.Transf.interior_at_pos Grid.transparent (i,j) g in
             Result.Ok (`Obj (`Vec (i,j), `Grid g))
          | _ -> Result.Error (Invalid_expr e))
      | `DNeighbors_1 ->
         (function
          | [| `Grid g|] ->
             Result.Ok (`Grid (Grid.Transf.dneighbors Grid.transparent g))
          | [| `Obj (`Vec (i,j), `Grid g)|] ->
             let| i, j, g = Grid.Transf.dneighbors_at_pos Grid.transparent (i,j) g in
             Result.Ok (`Obj (`Vec (i,j), `Grid g))
          | _ -> Result.Error (Invalid_expr e))
      | `INeighbors_1 ->
         (function
          | [| `Grid g|] ->
             Result.Ok (`Grid (Grid.Transf.ineighbors Grid.transparent g))
          | [| `Obj (`Vec (i,j), `Grid g)|] ->
             let| i, j, g = Grid.Transf.ineighbors_at_pos Grid.transparent (i,j) g in
             Result.Ok (`Obj (`Vec (i,j), `Grid g))
          | _ -> Result.Error (Invalid_expr e))
      | `Neighbors_1 ->
         (function
          | [| `Grid g|] ->
             Result.Ok (`Grid (Grid.Transf.neighbors Grid.transparent g))
          | [| `Obj (`Vec (i,j), `Grid g)|] ->
             let| i, j, g = Grid.Transf.neighbors_at_pos Grid.transparent (i,j) g in
             Result.Ok (`Obj (`Vec (i,j), `Grid g))
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
      | `MostCommon_1 ->
         (function
          | [|v1|] when Ndseq.depth v1 > 0 ->
             let cnt = new Common.counter in
             let| () =
               eval_aggreg "mostcommon"
                 (fun v -> cnt#add v; Some ())
                 (fun (res, v) -> cnt#add v; Some res)
                 v1 in
             (match cnt#most_frequents with
              | _, [v] -> Result.Ok v
              | _ -> Result.Error (Undefined_result "mostcommon: ambiguous"))
          | _ -> assert false)
      | `LeastCommon_1 ->
         (function
          | [|v1|] when Ndseq.depth v1 > 0 ->
             let cnt = new Common.counter in
             let| () =
               eval_aggreg "leastcommon"
                 (fun v -> cnt#add v; Some ())
                 (fun (res, v) -> cnt#add v; Some res)
                 v1 in
             (match cnt#least_frequents with
              | _, [v] -> Result.Ok v
              | _ -> Result.Error (Undefined_result "leastcommon: ambiguous"))
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
      | `GridOfColorSeq_1 dir ->
         (function
          | [|v1|] ->
             let ndim = Ndseq.depth v1 in
             if ndim > 0
             then
               Ndseq.map_result ~depth:(ndim-1) (-1)
                 (fun vcolors ->
                   let| g = make_grid_from_color_seq dir vcolors in
                   Result.Ok (`Grid g))
                 v1
             else Result.Error (Undefined_result "gridOfColorSeq: not a sequence")
          | _ -> assert false)
      | `GridOfColorMat_1 ->
         (function
          | [|v1|] ->
             let ndim = Ndseq.depth v1 in
             if ndim > 1
             then
               Ndseq.map_result ~depth:(ndim-2) (-2)
                 (fun vcolorss ->
                   let| g = make_grid_from_color_seq_seq vcolorss in
                   Result.Ok (`Grid g))
                 v1
             else Result.Error (Undefined_result "gridOfColorMat: not matrix")
          | _ -> assert false)
      | `Colors_1 ->
         (function
          | [|v1|] ->
             Ndseq.map_result 1
               (function
                | `Grid g ->
                   let lnc = Grid.color_freq_desc g in
                   Result.Ok (Ndseq.seq 0 (List.map (fun (n,c) -> `Color c) lnc))
                | _ -> Result.Error (Undefined_result "colors: not a grid"))
               v1
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
      | `Quadrants_1 ->
         (function
          | [|v1|] ->
             Ndseq.map_result 2
               (function
                | `Grid g ->
                   let h, w = Grid.dims g in
                   let h' = h / 2 in
                   let w' = w / 2 in
                   let| g00 = Grid.Transf.crop g 0 0 h' w' in
                   let| g01 = Grid.Transf.crop g 0 (w-w') h' w' in
                   let| g10 = Grid.Transf.crop g (h-h') 0 h' w' in
                   let| g11 = Grid.Transf.crop g (h-h') (w-w') h' w' in
                   Result.Ok
                     (Ndseq.seq 1
                        [ Ndseq.seq 0 [`Grid g00; `Grid g01];
                          Ndseq.seq 0 [`Grid g10; `Grid g11]])
                | _ -> Result.Error (Undefined_result "quadrants: not a grid"))
               v1
          | _ -> assert false)             
      | `RelativePos_1 ->
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
                                       let i = abs (mini2 - mini1) in
                                       let j = abs (minj2 - minj1) in
                                       `Vec (i, j)
                                    | _ -> assert false)
                                  objs))
                           objs)
                   | None -> assert false)
                 v1)
             else Result.Error (Undefined_result "relativePos_1: not a sequence")
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

    (* pattern args distributions *)

    let distrib_vec_i_j ~depth src r =
      Ndseq.map_tup ~depth (0,0)
        (function
         | `VecRange (ri,rj) -> `IntRange ri, `IntRange rj
         | _ -> assert false)
        (tup1 r)
    
    (* model-based generation *)
      
    let generator_value (v0 : value) (r : distrib) =
      let* v =
        Ndseq.match_myseq 0
          (fun v0 r -> Myseq.return v0)
          (* DO NOT check that v agrees with r BECAUSE r is more of a generation hint, not exhaustive *)
          v0 r in
      (* Warning: v' may be different from v because of broadcasting in Ndseq.match_myseq *)
      Myseq.return (Data.make_dexpr v r)

    let generator_any t (r : distrib) =
      let depth = t.ndim in
      assert (Ndseq.depth r = depth);
      let rec aux = function
        | `Null ->
           Myseq.return `Null
        | `IntRange (Range.Closed (a,b)) ->
           let* n = Myseq.range a b in
           Myseq.return (`Int n)
        | `VecRange (Range.Closed (i1,i2), Range.Closed (j1,j2)) ->
           let* i = Myseq.range i1 i2 in
           let* j = Myseq.range j1 j2 in
           Myseq.return (`Vec (i,j))
        | `ColorRange (tc,lc) ->
           let* c = Myseq.from_list lc in
           Myseq.return (`Color c)
        | `SegRange lseg ->
           let* seg = Myseq.from_list lseg in
           Myseq.return (`Seg seg)
        | `OrderRange lorder ->
           let* order = Myseq.from_list lorder in
           Myseq.return (`Order order)
        | `MotifRange lmot ->
           let* mot = Myseq.from_list lmot in
           Myseq.return (`Motif mot)               
        | `GridRange (_,
                      Range.Closed (minh,maxh),
                      Range.Closed (minw,maxw),
                      lc) ->
           let* h, w, c =
             Myseq.product_fair3
               (Myseq.range minh maxh,
                Myseq.range minw maxw,
                Myseq.from_list lc) in
           let g = Grid.make h w c in
           Myseq.return (`Grid g)
        | `ObjRange (rpos,rg1) ->
           let* vpos = aux rpos in
           let* vg1 = aux rg1 in
           Myseq.return (`Obj (vpos,vg1))
        | `MapRange (ra, rb) ->
           let* a = aux ra in
           let* b = aux rb in
           let m = Mymap.singleton a b in
           Myseq.return (`Map m) (* empty map = identity map *)
        | _ -> assert false
      in
      let* v =
        Ndseq.map_myseq ~depth 0
          (fun r -> aux r)
          r in
      Myseq.return (Data.make_dany v r)

    let generator_pat t c src gen_args (r : distrib) =
      let depth = t.ndim in
      assert (Ndseq.depth r = depth);
      match c, src, gen_args with
      | Vec, [||], [|gen_i; gen_j|] ->
         let r_i, r_j = distrib_vec_i_j ~depth src r in
         let* di, dj = Myseq.product_fair2 (gen_i r_i, gen_j r_j) in
         let v : value =
           Ndseq.map_tup ~depth 0
             (function
              | `Int i, `Int j -> `Vec (i,j)
              | _ -> assert false)
             (Data.value di, Data.value dj) in
         Myseq.return (Data.make_dpat v r c [|di; dj|])

      | Square, [||], [|gen_ij|] ->
         let r_ij =
           Ndseq.map ~depth 0
             (function
              | `VecRange (Range.Closed (mini,maxi), Range.Closed (minj,maxj)) ->
                 `IntRange (Range.Closed (max mini minj, min maxi maxj)) (* interval intersection because i = j *)
              | _ -> assert false)
             r in
         let* dij = gen_ij r_ij in
         let v : value =
           Ndseq.map ~depth 0
             (function
              | `Int i -> `Vec (i,i)
              | _ -> assert false)
             (Data.value dij) in
         Myseq.return (Data.make_dpat v r c [|dij|])    
    
      | Obj, [||], [|gen_pos; gen_g1|] ->
         let r_pos, r_g1 =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `ObjRange (r_pos, r_g1) -> r_pos, r_g1
              | _ -> assert false)
             (tup1 r) in
         let* dpos, dg1 = Myseq.product_fair2 (gen_pos r_pos, gen_g1 r_g1) in
         let v : value =
           Ndseq.map_tup ~depth 0
             (fun (vpos,vg1) -> `Obj (vpos,vg1))
             (Data.value dpos, Data.value dg1) in
         Myseq.return (Data.make_dpat v r c [|dpos; dg1|])
    
      | DomMap keys, [||], [|gen_vals|] ->
         let k = List.length keys in
         let r_vals =
           Ndseq.map ~depth (+1)
             (Ndseq.seq_of_item
                (function
                 | `MapRange (r_a,r_b) -> List.init k (fun _ -> r_b)
                 | _ -> assert false))
             r in
         let* dvals = gen_vals r_vals in
         let v : value =
           Ndseq.map ~depth (-1)
             (Ndseq.item_of_seq
                (fun vals ->
                  assert (List.length vals = k);
                  let m = mymap_of_list (List.combine keys vals) in
                  `Map m))
             (Data.value dvals) in
         Myseq.return (Data.make_dpat v r c [|dvals|])
    
      | Replace, [||], [|gen_a; gen_b|] ->
         let r_a, r_b =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `MapRange (r_a,r_b) -> r_a, r_b
              | _ -> assert false)
             (tup1 r) in
         let* da, db = Myseq.product_fair2 (gen_a r_a, gen_b r_b) in
         let v : value =
           Ndseq.map_tup ~depth 0
             (fun (va, vb) -> `Map (mymap_of_list [va, vb; vb, vb]))
             (Data.value da, Data.value db) in
         Myseq.return (Data.make_dpat v r c [|da; db|])
    
      | Swap, [||], [|gen_a; gen_b|] ->
         let r_a, r_b =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `MapRange (r_a,r_b) -> r_a,r_b
              | _ -> assert false)
             (tup1 r) in
         let* da, db = Myseq.product_fair2 (gen_a r_a, gen_b r_b) in
         let v : value =
           Ndseq.map_tup ~depth 0
             (fun (va, vb) -> `Map (mymap_of_list [va, vb; vb, va]))
             (Data.value da, Data.value db) in
         Myseq.return (Data.make_dpat v r c [|da; db|])
    
      | BgColor, [||], [|gen_col; gen_g1|] ->
         let r_col =
           Ndseq.map ~depth 0
             (function
              | `GridRange ((filling,nocol), rh, rw, lc) -> `ColorRange (C_BG (filling = `Full), lc)
              | _ -> assert false)
             r in
         let* dbc = gen_col r_col in
         let r_g1 =
           Ndseq.map_tup ~depth 0
             (function
              | `Color bc, `GridRange ((filling,nocolor), rh, rw, lc) ->
                 let lc1 = List.filter ((<>) bc) lc in
                 `GridRange ((`Sprite,nocolor), rh, rw, lc1)
              | _ -> assert false)
             (Data.value dbc, r) in
         let* dg1 = gen_g1 r_g1 in
         let v : value =
           Ndseq.map_tup ~depth 0
             (function
              | `Color bc, `Grid g1 -> `Grid (Grid.fill_transparent g1 bc)
              | _ -> assert false)
             (Data.value dbc, Data.value dg1) in
         Myseq.return (Data.make_dpat v r c [|dbc; dg1|])
    
      | IsFull, [||], [|gen_g1|] ->
         let* dg1 = gen_g1 r in
         let v = Data.value dg1 in
         Myseq.return (Data.make_dpat v r c [|dg1|])
    
      | Crop, [|vg|], [|gen_pos; gen_size|] ->
         let* r_pos, r_size =
           Ndseq.mapi_tup_myseq ~depth (0,0)
             (fun is r ->
               match r, Ndseq.index_list_broadcast vg is 0 with
               | `GridRange ((filling,nocolor),
                             Range.Closed (h1min,h1max),
                             Range.Closed (w1min,w1max),
                             lc1),
                 Some (`Grid g) ->
                  let h, w = Grid.dims g in
                  Myseq.return
                    (`VecRange (Range.Closed (0,0), Range.Closed (0,0)),
                     `VecRange (Range.Closed (min h h1min, min h h1max), Range.Closed (min w w1min, min w w1max)))
               | _, None -> Myseq.empty
               | _ -> assert false)
             (tup1 r) in
         let* dpos, dsize = Myseq.product_fair2 (gen_pos r_pos, gen_size r_size) in
         let* v =
           Ndseq.mapi_tup_myseq ~name:"gen/Crop" ~depth 0
             (fun is (vpos,vsize) ->
               match vpos, vsize, Ndseq.index_list_broadcast vg is 0 with
               | `Vec (i,j), `Vec (h1,w1), Some (`Grid g) ->
                  let* g  = Myseq.from_result (Grid.Transf.crop g i j h1 w1) in
                  Myseq.return (`Grid g)
               | _, _, None -> Myseq.empty
               | _ -> assert false)
             (Data.value dpos, Data.value dsize) in
         Myseq.return (Data.make_dpat v r c ~src [|dpos; dsize|])
    
      | Objects (nmax,mode), [||], [|gen_size; gen_seg; gen_order; gen_card; gen_objs; _gen_merger; gen_noise|] ->
         let r_seg, r_order, r_card =
           Ndseq.map_tup ~depth (0,0,0)
             (fun _ ->
               `SegRange [ match mode with
                           | `Connected -> GPat.Objects.(Connected (Connect8,false))
                           | `SameColor -> GPat.Objects.SameColor ],
               `OrderRange [GPat.Objects.Pos], (* this will not be used when generating objects *)
               `IntRange (Range.Closed (1,nmax)))
             (tup1 r) in
         let* dseg, dorder, dcard =
           Myseq.product_fair3
             (gen_seg r_seg,
              gen_order r_order,
              gen_card r_card) in
         let vcard = Data.value dcard in
         (* let* () = Myseq.from_bool
            (Ndseq.for_all
            (function
            | `Int card -> card <= nmax
            | _ -> assert false)
            vcard) in *)
         let r_objs =
           Ndseq.map_tup ~depth 1
             (function
              | `Int card, `GridRange ((filling,nocolor),
                                       Range.Closed (minh,maxh),
                                       Range.Closed (minw,maxw),
                                       lc) ->
                 let r_obj = `ObjRange (`VecRange (Range.Closed (0,0),
                                                   Range.Closed (0,0)),
                                        `GridRange ((`Sprite,nocolor),
                                                    Range.Closed (1,3),
                                                    Range.Closed (1,3),
                                                    lc)) in
                 Ndseq.seq 0 (List.init card (fun _ -> r_obj))
              | _ -> assert false)
                 (vcard, r) in
         let* dobjs = gen_objs r_objs in
         let r_size, r_noise =
           Ndseq.map_tup ~depth (0,0)
             (fun (seq_objs, r) ->
               match Ndseq.as_seq seq_objs, r with
               | Some (0, objs), `GridRange ((filling,nocolor),
                                             Range.Closed (minh,maxh),
                                             Range.Closed (minw,maxw),
                                             lc) ->
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
                  `VecRange (Range.Closed (minh,maxh), Range.Closed (minw,maxw)),
                  `GridRange ((`Noise,nocolor),
                              Range.Closed (minh,maxh),
                              Range.Closed (minw,maxw),
                              [Grid.transparent])
               | _ -> assert false)
             (Data.value dobjs, r) in
         let* dsize = gen_size r_size in
         let* dnoise = gen_noise r_noise in
         let v, dmerger = make_objects_v_dmerger ~depth dsize dseg dorder dcard dobjs dnoise in
         Myseq.return (Data.make_dpat v r c [|dsize; dseg; dorder; dcard; dobjs; dmerger; dnoise|])

      | ColorPartition, [||], [|gen_size; gen_ncol; gen_colors; gen_masks|] ->
         let r_size, r_ncol =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `GridRange ((filling,nocolor),
                            Range.Closed (hmin,hmax),
                            Range.Closed (wmin,wmax),
                            lc) ->
                 `VecRange (Range.Closed (hmin,hmax), Range.Closed (wmin,wmax)),
                 `IntRange (Range.Closed (1, List.length lc))
              | _ -> assert false)
             (tup1 r) in
         let* dsize, dncol = Myseq.product_fair2 (gen_size r_size, gen_ncol r_ncol) in
         let r_colors, r_masks =
           Ndseq.map_tup ~depth (1,1)
             (function
              | `Vec (h,w), `Int ncol, `GridRange (_, _, _, lc) ->
                 let r_color = `ColorRange (C_OBJ, lc) in (* TODO: constrain different colors across sequence *)
                 let r_mask = `GridRange ((`Sprite,true),
                                          Range.Closed (h,h),
                                          Range.Closed (w,w),
                                          [Grid.one]) in
                 Ndseq.seq 0 (List.init ncol (fun _ -> r_color)),
                 Ndseq.seq 0 (List.init ncol (fun _ -> r_mask))
              | _ -> assert false)
             (Data.value dsize, Data.value dncol, r) in
         let* dcolors = gen_colors r_colors in
         let* dmasks = gen_masks r_masks in
         let* v = make_color_partition ~depth
                    (Data.value dsize)
                    (Data.value dcolors)
                    (Data.value dmasks) in
         Myseq.return (Data.make_dpat v r c [|dsize; dncol; dcolors; dmasks|])
    
      | Monocolor, [||], [|gen_col; gen_mask|] ->
         let r_col, r_mask =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `GridRange ((filling,nocolor), rh, rw, lc) ->
                 `ColorRange (C_OBJ, lc), `GridRange ((filling,true), rh, rw, [Grid.one])
              | _ -> assert false)
             (tup1 r) in
         let* dcol, dmask = Myseq.product_fair2 (gen_col r_col, gen_mask r_mask) in
         let* v =
           Ndseq.map_tup_myseq ~name:"gen/Monocolor" ~depth 0
             (function
              | `Color c, `Grid g1 ->
                 let* g = Myseq.from_result (Grid.Transf.swap_colors g1 Grid.one c) in
                 Myseq.return (`Grid g)
              | _ -> assert false)
             (Data.value dcol, Data.value dmask) in
         Myseq.return (Data.make_dpat v r c [|dcol; dmask|])
    
      | Recoloring, [|vgrid|], [|gen_map|] ->
         let r_map =
           Ndseq.map ~depth 0
             (function
              | `GridRange (tg, rh, rw, lc) ->
                 let rc = `ColorRange (C_OBJ, lc) in
                 `MapRange (rc, rc)
              | _ -> assert false)
             r in
         let* dmap = gen_map r_map in
         let* v =
           Ndseq.mapi_tup_myseq ~depth 0
             (fun is vmcol ->
               match vmcol, Ndseq.index_list_broadcast vgrid is 0 with
               | `Map mcol, Some (`Grid g1) ->
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
                  Myseq.return (`Grid g)
               | _, None -> Myseq.empty
               | _ -> assert false)
             (tup1 (Data.value dmap)) in
         Myseq.return (Data.make_dpat v r c ~src [|dmap|])
    
      | MotifMulti partial, [||], [|gen_mot; gen_core; _gen_pure; gen_mask_opt; gen_noise|] ->
         let r_mot, r_noise =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `GridRange ((filling,nocolor), rh, rw, lc) ->
                 `MotifRange GPat.Motif.candidates_multi,
                 `GridRange ((`Noise,nocolor), rh, rw, [Grid.transparent])
              | _ -> assert false)
             (tup1 r) in
         let* dmot, dnoise = Myseq.product_fair2 (gen_mot r_mot, gen_noise r_noise) in
         let* r_mask_opt, r_core =
           Ndseq.map_tup_myseq ~name:"gen/MotifMulti/r_res" ~depth (0,0)
             (function
              | `GridRange (tg, rh, rw, lc), `Motif mot, `Grid gnoise ->
                 let h, w = Grid.dims gnoise in
                 let _, _, luv = GPat.Motif.all_coredims_of_motif mot h w in
                 let* u, v = Myseq.from_list luv in
                 Myseq.return
                   ((if partial
                     then `GridRange ((`Sprite,true),
                                      Range.Closed (h,h),
                                      Range.Closed (w,w),
                                      [Grid.one])
                     else `Null),
                    `GridRange (tg, Range.Closed (u,u), Range.Closed (v,v), lc))
              | _ -> assert false)
                 (r, Data.value dmot, Data.value dnoise) in
         let* dcore = gen_core r_core in
         let* dmask_opt = gen_mask_opt r_mask_opt in
         let* v, vpure, rpure =
           Ndseq.map_tup_myseq ~name:"gen/Motif" ~depth (0,0,0)
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
                 Myseq.return (`Grid g, `Grid g_pure, `Null) (* TODO: define better rpure *)
              | _ -> assert false)
             (Data.value dmot, Data.value dcore, Data.value dmask_opt, Data.value dnoise) in
         let dpure = Data.make_dexpr vpure rpure in (* computed data *)
         Myseq.return (Data.make_dpat v r c [|dmot; dcore; dpure; dmask_opt; dnoise|])

      | MotifBi partial, [||], [|gen_mot; gen_bgcolor; gen_color; _gen_pure; gen_mask_opt; gen_noise|] ->
         let* r_mot, r_noise =
           Ndseq.map_tup_myseq ~depth (0,0)
             (function
              | `GridRange ((filling,nocolor), Range.Closed (minh,maxh), Range.Closed (minw,maxw), lc) ->
                 if maxh >= 3 && maxw >= 3 (* bicolor motifs have size at least 3x3 *)
                 then
                   Myseq.return
                     (`MotifRange GPat.Motif.candidates_bi,
                      `GridRange ((`Noise,nocolor),
                                  Range.Closed (max 3 minh, maxh),
                                  Range.Closed (max 3 minw, maxw),
                                  [Grid.transparent]))
                 else Myseq.empty
              | _ -> assert false)
             (tup1 r) in
         let* dmot, dnoise = Myseq.product_fair2 (gen_mot r_mot, gen_noise r_noise) in
         let* r_mask_opt, r_bgcolor =
           Ndseq.map_tup_myseq ~name:"gen/Motif/r_res" ~depth (0,0)
             (function
              | `GridRange ((filling,nocolor), Range.Closed (minh,maxh), Range.Closed (minw,maxw), lc), `Motif mot, `Grid gnoise ->
                 let h, w = Grid.dims gnoise in
                 let lbgcolor =
                   if filling = `Full
                   then lc
                   else Grid.transparent :: lc in
                 Myseq.return
                   ((if partial
                     then `GridRange ((`Sprite,true),
                                      Range.Closed (h,h),
                                      Range.Closed (w,w),
                                      [Grid.one])
                     else `Null),
                    `ColorRange (C_BG (filling = `Full), lbgcolor))
              | _ -> assert false)
             (r, Data.value dmot, Data.value dnoise) in
         let* dbgcolor = gen_bgcolor r_bgcolor in
         let r_color =
           Ndseq.map_tup ~depth 0
             (function
              | `GridRange (_, _, _, lc), `Color bgcolor ->
                 `ColorRange (C_OBJ, list_remove bgcolor lc)
              | _ -> assert false)
             (r, Data.value dbgcolor) in
         let* dcolor = gen_color r_color in
         let* dmask_opt = gen_mask_opt r_mask_opt in
         let* v, vpure, rpure =
           Ndseq.map_tup_myseq ~name:"gen/Motif" ~depth (0,0,0)
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
                 Myseq.return (`Grid g, `Grid g_pure, `Null) (* TODO: define better rpure *)
              | _ -> assert false)
             (Data.value dmot, Data.value dbgcolor, Data.value dcolor, Data.value dmask_opt, Data.value dnoise) in
         let dpure = Data.make_dexpr vpure rpure in (* computed data *)
         Myseq.return (Data.make_dpat v r c [|dmot; dbgcolor; dcolor; dpure; dmask_opt; dnoise|])

      | Metagrid, [||], [|gen_sepcolor; gen_borders; gen_dims; gen_heights; gen_widths; gen_gridss|] ->
         let r_sepcolor, r_borders, r_dims =
           Ndseq.map_tup ~depth (0,0,0)
             (function
              | `GridRange ((filling,nocolor), rh, rw, lc) ->
                 `ColorRange (C_BG (filling = `Full), lc),
                 `GridRange ((`Noise,true), Range.Closed (2,2), Range.Closed (2,2), [Grid.one]),
                 `VecRange (Range.Closed (1,3), Range.Closed (1,3))                 
              | _ -> assert false)
             (tup1 r) in
         let* dsepcolor, dborders, ddims =
           Myseq.product_fair3
             (gen_sepcolor r_sepcolor,
              gen_borders r_borders,
              gen_dims r_dims) in
         let r_heights, r_widths =
           Ndseq.map_tup ~depth (1,1)
             (function
              | `Vec (k,l) ->
                 Ndseq.seq 0 (List.init k (fun _ -> `IntRange (Range.Closed (1,10)))),
                 Ndseq.seq 0 (List.init l (fun _ -> `IntRange (Range.Closed (1,10))))
              | _ -> assert false)
             (tup1 (Data.value ddims)) in
         let* dheights, dwidths =
           Myseq.product_fair2 (gen_heights r_heights, gen_widths r_widths) in
         let r_gridss =
           Ndseq.map_tup ~name:"gen/Metagrid/vx" ~depth 2
             (function
              | `GridRange ((filling,nocolor), _, _, _),
                `ColorRange (C_BG full, lc), `Color sepcolor,
                `Vec (k,l), vheights, vwidths ->
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
                               `GridRange ((filling,nocolor),
                                           Range.Closed (h1,h1),
                                           Range.Closed (w1,w1),
                                           lc1)))))
              | _ -> assert false)
             (r, r_sepcolor, Data.value dsepcolor, Data.value ddims, Data.value dheights, Data.value dwidths) in
         let* dgridss = gen_gridss r_gridss in
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
         Myseq.return (Data.make_dpat v r c [|dsepcolor; dborders; ddims; dheights; dwidths; dgridss|])
    
(* TODO      | Repeat, [|gen_grid; gen_nis; gen_njs|], _ ->
         let* dgrid, dnis, dnjs = Myseq.product_fair3 (gen_grid r, gen_nis r, gen_njs r) in
         let* data = Myseq.from_result (make_drepeat dgrid dnis dnjs) in
         Myseq.return (data, `Null) *)
    
      | Empty, [||], [|gen_size|] ->
         let r_size =
           Ndseq.map ~depth 0
             (function
              | `GridRange (tg, rh, rw, _) -> `VecRange (rh, rw)
              | _ -> assert false)
             r in
         let* dsize = gen_size r_size in
         let v =
           Ndseq.map ~depth 0
             (function
              | `Vec (h,w) -> `Grid (Grid.Mask.empty h w)
              | _ -> assert false)
             (Data.value dsize) in
         Myseq.return (Data.make_dpat v r c [|dsize|])
      | Full, [||], [|gen_size|] ->
         let r_size =
           Ndseq.map ~depth 0
             (function
              | `GridRange (_,rh,rw,_) -> `VecRange (rh,rw)
              | _ -> assert false)
             r in
         let* dsize = gen_size r_size in
         let v =
           Ndseq.map ~depth 0
             (function
              | `Vec (h,w) -> `Grid (Grid.Mask.full h w)
              | _ -> assert false)
             (Data.value dsize) in
         Myseq.return (Data.make_dpat v r c [|dsize|])
      | Point, [||], [||] ->
         let v =
           Ndseq.map ~depth 0
             (fun _ -> `Grid (Grid.Mask.full 1 1))
             r in
         Myseq.return (Data.make_dpat v r c [||])
      | Line, [||], [|gen_len; gen_dir|] ->
         let r_len, r_dir =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `GridRange (_, Range.Closed (minh,maxh), Range.Closed (minw,maxw), _) ->
                 `IntRange (Range.Closed (min minh minw, max maxh maxw)),
                 `VecRange (Range.Closed (0,1), Range.Closed (-1,1))
              | _ -> assert false)
             (tup1 r) in
         let* dlen = gen_len r_len in
         let* ddir = gen_dir r_dir in (* TODO: avoid (0,0) *)
         let* v =
           Ndseq.map_tup_myseq ~depth 0
             (function
              | `Int len, `Vec dir ->
                 let* g = Myseq.from_result (GPat.generate_line len dir) in
                 Myseq.return (`Grid g)
              | _ -> assert false)
             (Data.value dlen, Data.value ddir) in
         Myseq.return (Data.make_dpat v r c [|dlen; ddir|])
    
      | ColorSeq dir, [||], [|gen_size; gen_colors|] ->
         let r_size =
           Ndseq.map ~depth 0
             (function
              | `GridRange (tg,rh,rw,lc) ->
                 (match dir with
                  | `H -> `IntRange rw
                  | `V -> `IntRange rh)
              | _ -> assert false)
             r in
         let* dsize = gen_size r_size in
         let r_colors =
           Ndseq.map_tup ~depth 1
             (function
              | `GridRange (tg,rh,rw,lc), `Int k ->
                 let rc = `ColorRange (C_OBJ,lc) in
                 Ndseq.seq 0 (List.init k (fun _ -> rc))
              | _ -> assert false)
             (r,Data.value dsize) in
         let* dcolors = gen_colors r_colors in
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
         Myseq.return (Data.make_dpat v r c [|dsize; dcolors|])
    
      | ColorMat, [||], [|gen_size; gen_colorss|] ->
         let r_size =
           Ndseq.map ~depth 0
             (function
              | `GridRange (tg,rh,rw,lc) ->
                 `VecRange (rh,rw)
              | _ -> assert false)
             r in
         let* dsize = gen_size r_size in
         let r_colorss =
           Ndseq.map_tup ~depth 2
             (function
              | `GridRange (tg,rh,rw,lc), `Vec (k,l) ->
                 let rc = `ColorRange (C_OBJ, lc) in
                 Ndseq.seq 1
                   (List.init k (fun _ ->
                        Ndseq.seq 0
                          (List.init l (fun _ ->
                               rc))))
              | _ -> assert false)
             (r, Data.value dsize) in                                   
         let* dcolorss = gen_colorss r_colorss in
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
         Myseq.return (Data.make_dpat v r c [|dsize; dcolorss|])

      | MakeGrid, [||], [|gen_grid|] ->
         let depth = Ndseq.depth r in
         let* r_grid =
           Ndseq.map_myseq ~depth:(depth - 2) (-2)
             (fun r ->
               let* h, w, tc, lc =
                 match Ndseq.as_seq r with
                 | Some (1, row0::rows1) ->
                    (match Ndseq.as_seq row0 with
                     | Some (0, (`ColorRange (tc,lc) :: cells)) ->
                        let h = 1 + List.length rows1 in
                        let w = 1 + List.length cells in
                        if List.for_all
                             (fun row1 ->
                               match Ndseq.as_seq row1 with
                               | Some (0, cells) -> List.length cells = w
                               | _ -> false)
                             rows1
                        then Myseq.return (h, w, tc, lc)
                        else Myseq.empty (* not rectangular *)
                     | Some (0, []) -> Myseq.empty (* a grid cannot have size 0x0 *)
                     | _ -> assert false)
                 | Some (1, []) -> Myseq.empty (* a grid cannot have size 0x0 *)
                 | _ -> assert false in
               (* let filling =
                 match tc with
                 | C_OBJ | C_BG true -> `Full
                 | C_BG false -> `Sprite in *)
               Myseq.return (`GridRange ((`Sprite,false), Range.Closed (h,h), Range.Closed (w,w), lc)))
             r in
         let* dgrid = gen_grid r_grid in
         let* v =
           Ndseq.map_tup_myseq ~depth:(depth - 2) 2
             (function
              | `Grid g, `GridRange (_, Range.Closed (h,_), Range.Closed (w,_), _) ->
                 if Grid.dims g = (h,w)
                 then Myseq.return
                        (Ndseq.seq 1
                           (List.init h
                              (fun i ->
                                Ndseq.seq 0
                                  (List.init w
                                     (fun j ->
                                       `Color (Grid.get_pixel ~source:"gen/MakeGrid" g i j))))))
                 else Myseq.empty
              | _ -> assert false)
             (Data.value dgrid, r_grid) in
         Myseq.return (Data.make_dpat v r c [|dgrid|])
    
      | SeqCons depth, [||], [|gen_hd; gen_tl|] ->
         let* r_hd, r_tl = Ndseq.head_tail ~depth r in
         let* dhd = gen_hd r_hd in
         let* dtl = gen_tl r_tl in
         let v : value =
           Ndseq.cons ~depth (Data.value dhd) (Data.value dtl) in
         Myseq.return (Data.make_dpat v r c [|dhd;dtl|])
    
      | SeqRepeat dep, [||], [|gen_e|] ->
         let* r_e =
           Ndseq.map_myseq ~depth:dep (-1)
             (Ndseq.item_of_seq
                (function
                 | [] -> Myseq.empty
                 | r::_ -> Myseq.return r))
             r in
         let* de = gen_e r_e in
         let v : value =
           Ndseq.map_tup ~depth:dep (depth - dep) 
             (fun (r,ve) ->
               match Ndseq.as_seq r with
               | Some (d,rs) ->
                  assert (d = Ndseq.depth ve);
                  Ndseq.seq d (List.map (fun _ -> ve) rs)
               | _ -> assert false)
             (r, Data.value de) in
         Myseq.return (Data.make_dpat v r c [|de|])

      | SeqRange, [||], [|gen_start; gen_step|] ->
         let* r_start, r_step =
           Ndseq.map_tup_myseq ~depth:(depth-1) (0,0)
             (fun r ->
               match Ndseq.as_seq r with
               | Some (_,l) ->
                  (match l with
                   | `IntRange (Range.Closed (a1,b1))::`IntRange (Range.Closed (a2,b2))::_ ->
                      Myseq.return (`IntRange (Range.Closed (a1,b1)),
                                    `IntRange (Range.Closed (a2-b1, b2-a1)))
                   | _ -> Myseq.empty)
               | _ -> assert false)
             (tup1 r) in
         let* dstart = gen_start r_start in
         let* dstep = gen_step r_step in
         let v : value =
           Ndseq.map_tup ~depth:(depth-1) 1
             (fun (r,vstart,vstep) ->
               match Ndseq.as_seq r, vstart, vstep with
               | Some (_,l), `Int start, `Int step ->
                  let n = List.length l in
                  Ndseq.seq 0 (List.init n (fun i -> `Int (start + i * step)))
               | _ -> assert false)
             (r, Data.value dstart, Data.value dstep) in
         Myseq.return (Data.make_dpat v r c [|dstart; dstep|])

      | SeqIndex, [|vseq|], [|gen_index|] ->
         let depth_seq = Ndseq.depth vseq in
         let r_index =
           Ndseq.seq 0 (List.init (depth_seq - depth) (fun _ -> `IntRange (Range.Closed (0,2)))) in (* default index *)
         let* dindex = gen_index r_index in
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
         Myseq.return (Data.make_dpat v r c ~src [|dindex|])

      | _ ->
         pp_endline xp_typ t;
         pp_endline (xp_pat c
                       (Array.map (fun v -> fun ~html print () -> xp_value ~html print v) src) 
                       (Array.map (fun _ -> fun ~html print _ -> print#string "_") gen_args)) ();
         (if r = `Null then print_endline "r = NULL");
         assert false
    
    (* model-based parsing *)
           
    let parseur_value (v0 : value) (v : value) (r : distrib) =
      let* v' =
        Ndseq.match_myseq 0
          (fun v0 v ->
            if v = v0
            then Myseq.return v0
            else Myseq.empty)
          v0 v in
      Myseq.return (Data.make_dexpr v' r)
    
    let parseur_any t (v : value) (r : distrib) =
      let depth = t.ndim in
      assert (Ndseq.depth v = depth);
      assert (Ndseq.depth r = depth);
      Myseq.return (Data.make_dany v r)
    
    let parseur_pat t c src parse_args (v : value) (r : distrib) =
      let depth = t.ndim in
      assert (Ndseq.depth v = depth);
      assert (Ndseq.depth r = depth);
      match c, src, parse_args with
      | Vec, [||], [|parse_i; parse_j|] ->
         let i, j =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Vec (i,j) -> `Int i, `Int j
              | _ -> assert false)
             (tup1 v) in
         let r_i, r_j = distrib_vec_i_j ~depth src r in
         let* di = parse_i i r_i in
         let* dj = parse_j j r_j in
         Myseq.return (Data.make_dpat v r c [|di; dj|])

      | Square, [||], [|parse_ij|] ->
         let* ij, r_ij =
           Ndseq.map_tup_myseq ~depth (0,0)
             (function
              | `Vec (i,j), `VecRange (ri,rj) ->
                 if i = j
                 then Myseq.return (`Int i, `IntRange (Range.inter ri rj))
                 else Myseq.empty
              | _ -> assert false)
             (v,r) in
         let* dij = parse_ij ij r_ij in
         Myseq.return (Data.make_dpat v r c [|dij|])

      | Obj, [||], [|parse_pos; parse_g1|] ->
         let pos, r_pos, g1, r_g1 =
           Ndseq.map_tup ~depth (0,0,0,0)
             (function
              | `Obj (pos,g1), `ObjRange (rpos,rg1) -> pos, rpos, g1, rg1
              | _ -> assert false)
             (v,r) in
         let* dpos = parse_pos pos r_pos in
         let* dg1 = parse_g1 g1 r_g1 in
         Myseq.return (Data.make_dpat v r c [|dpos; dg1|])

      | DomMap keys, [||], [|parse_vals|] ->
         let* vals, r_vals =
           Ndseq.map_tup_myseq ~depth (1,1)
             (function
              | `Map m, `MapRange (ra,rb) ->
                 let pairs = Mymap.bindings m in
                 let m_keys = List.map fst pairs in
                 if m_keys = keys
                 then
                   let vals = List.map snd pairs in
                   Myseq.return (* TODO: replace 0 by values-dependent expr ? if seqs in vals *)
                     (Ndseq.seq 0 vals,
                      Ndseq.seq 0 (List.map (fun _ -> rb) vals))
                 else Myseq.empty
              | _ -> assert false)
             (v,r) in
         let* dvals = parse_vals vals r_vals in
         Myseq.return (Data.make_dpat v r c [|dvals|])
    
      | Replace, [||], [|parse_a; parse_b|] ->
         let* a, r_a, b, r_b =
           Ndseq.map_tup_myseq ~name:"parse/Repalce/in_a_b" ~depth (0,0,0,0)
             (function
              | `Map m, `MapRange (ra,rb) ->
                 let m_diff = Mymap.filter (fun a b -> a <> b) m in
                 (match Mymap.bindings m_diff with
                  | [a, b] ->
                     Myseq.return (a, ra,
                                   b, rb)
                  | _ -> Myseq.empty)
              | _ -> assert false)
             (v,r) in
         let* da = parse_a a r_a in
         let* db = parse_b b r_b in
         Myseq.return (Data.make_dpat v r c [|da; db|])
    
      | Swap, [||], [|parse_a; parse_b|] ->
         let* a, r_a, b, r_b =
           Ndseq.map_tup_myseq ~name:"parse/Swap/in_a_b" ~depth (0,0,0,0)
             (function
              | `Map m, `MapRange (ra,rb) ->
                 let m_diff = Mymap.filter (fun a b -> a <> b) m in
                 (match Mymap.bindings m_diff with
                  | [a, b; c, d] when a=d && b=c ->
                     Myseq.return (a, ra,
                                   b, rb)
                  | _ -> Myseq.empty)
              | _ -> assert false)
             (v,r) in
         let* da = parse_a a r_a in
         let* db = parse_b b r_b in
         Myseq.return (Data.make_dpat v r c [|da; db|])
    
      | BgColor, [||], [|parse_col; parse_g1|] ->
         let* col, r_col, g1, r_g1 =
           Ndseq.map_tup_myseq ~name:"parse/BgColor/in_col_g1" ~depth (0,0,0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc) ->
                 if Grid.is_full g
                 then
                   let tg1 = (`Sprite,nocolor) in
                   let* bc = Myseq.from_list (Segment.background_colors g) in
                   let* g1 = Myseq.from_result (Grid.Transf.swap_colors g bc Grid.transparent) in
                   let lc1 = list_remove bc lc in
                   Myseq.return (`Color bc, `ColorRange (C_BG (filling = `Full), lc),
                                 `Grid g1, `GridRange (tg1, rh, rw, lc1))
                 else Myseq.empty
              | _ -> assert false)
             (v,r) in
         let* dcol = parse_col col r_col in
         let* dg1 = parse_g1 g1 r_g1 in
         Myseq.return (Data.make_dpat v r c [|dcol; dg1|])

      | IsFull, [||], [|parse_g1|] ->
         let* dg1 = parse_g1 v r in
         let* v =
           Ndseq.map_myseq ~depth 0
             (function
              | `Grid g1 ->
                 if Grid.is_full g1
                 then Myseq.return (`Grid g1)
                 else Myseq.empty
              | _ -> assert false)
             (Data.value dg1) in
         Myseq.return (Data.make_dpat v r c [|dg1|])
    
      |  Crop, [|vg|], [|parse_pos; parse_size|] ->
         let depth = Ndseq.depth v in
         let size, r_size =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Grid g1, `GridRange (tg1, rh1, rw1, rc1) ->
                 let h1, w1 = Grid.dims g1 in
                 `Vec (h1, w1), `VecRange (rh1, rw1)
              | _ -> assert false)
             (v,r) in
         let* dsize = parse_size size r_size in
         let* pos, r_pos =
           try
             Ndseq.mapi_tup_myseq ~name:"parse/Crop/in_pos" ~depth (0,0)
               (fun is (v,vsize) ->
                 match v, vsize, Ndseq.index_list_broadcast vg is 0 with
                 | `Grid g1, `Vec (h1,w1), Some (`Grid g) ->
                    let h1, w1 = Grid.dims g1 in
                    let h, w = Grid.dims g in
                    let* i, j = Myseq.from_list (Grid_patterns.parse_crop g g1) in
                    Myseq.return
                      (`Vec (i,j),
                       `VecRange (Range.make_closed 0 (h-h1),
                                  Range.make_closed 0 (w-w1)))
                 | _, _, (Some `Null | None) -> Myseq.empty (* failed computation for source grid *)
                 | _ -> assert false)
               (v, Data.value dsize)
           with Invalid_argument _ -> Myseq.empty in (* dg may have an inconsistent structure *)
         let* dpos = parse_pos pos r_pos in
         Myseq.return (Data.make_dpat v r c ~src [|dpos; dsize|])
    
      | Objects (nmax,mode), [||], [|parse_size; parse_seg; parse_order; parse_card; parse_objs; _parse_merger; parse_noise|] ->
         let filling, nocolor =
           match t.kind with
           | GRID (filling,nocolor) -> filling, nocolor
           | _ -> assert false in
         let size, r_size =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc) ->
                 let h, w = Grid.dims g in
                 `Vec (h,w), `VecRange (rh,rw)
              | _ -> assert false)
             (v,r) in
         let* dsize = parse_size size r_size in
         let* seg, r_seg =
           let lseg =
             match mode with
             | `Connected -> GPat.Objects.candidate_segmentations_connected
             | `SameColor -> [GPat.Objects.SameColor] in             
           let* seg = (* common choice for all sequence items *)
             Myseq.from_list lseg in
           Myseq.return
             (Ndseq.map_tup ~depth (0,0)
                (function
                 | `Grid g -> `Seg seg, `SegRange lseg
                 | _ -> assert false)
                (tup1 v)) in
         let* dseg = parse_seg seg r_seg in
         let* order, r_order = (* common choice for all sequence items *)
           let lorder = GPat.Objects.candidate_orders nmax nocolor in
           let* order = Myseq.from_list lorder in
           Myseq.return
             (Ndseq.map_tup ~depth (0,0)
                (function
                 | `Grid g -> `Order order, `OrderRange lorder
                 | _ -> assert false)
                (tup1 v)) in
         let* dorder = parse_order order r_order in
         let* card, r_card, objs, r_objs, noise, r_noise =
           Ndseq.map_tup_myseq ~name:"parse/Objects/in_objs" ~depth (0,0,1,1,0,0)
             (function
              | `Grid g, `GridRange (tg, rh, rw, lc),
                `Seg seg, `Order order, `Vec (h,w) ->
                 let tg1 = (`Sprite,nocolor) in
                 let lc1 = lc in
                 (* PB: not robust segmentation choice, and makes monocolor non-compresive
                   match seg with
                   | GPat.Objects.Connected (_,true) | GPat.Objects.SameColor -> 1
                   | _ -> nc in *)
                 let tg_noise = (`Noise,nocolor) in
                 let* objs, g_noise = GPat.Objects.parse nmax seg g in
                 let objs = GPat.Objects.sort order objs in
                 let card = List.length objs in
                 Myseq.return
                   (`Int card, `IntRange (Range.make_closed 0 nmax),
                    
                    Ndseq.seq 0
                      (List.map
                         (fun (i,j,g1) -> `Obj (`Vec (i,j), `Grid g1))
                         objs),
                    Ndseq.seq 0
                      (List.map
                         (fun (i,j,g1) ->
                           let h1, w1 = Grid.dims g1 in
                           `ObjRange (`VecRange (Range.make_closed 0 (h-h1), (* (h-1)), *)
                                                 Range.make_closed 0 (w-w1)), (* (w-1))), *)
                                      `GridRange (tg1,
                                                  Range.make_closed 1 h, (* (h-i), *)
                                                  Range.make_closed 1 w, (* (w-j), *)
                                                  lc1)))
                         objs),

                    `Grid g_noise, `GridRange (tg_noise, Range.make_exact h, Range.make_exact w, lc))
              (* TODO: remove size, as included in noise, like for Motif? *)
              | _ -> assert false)
             (v, r, Data.value dseg, Data.value dorder, Data.value dsize) in
         let* dcard = parse_card card r_card in
         let* dobjs = parse_objs objs r_objs in
         let* dnoise = parse_noise noise r_noise in
         let _v, dmerger = make_objects_v_dmerger ~depth dsize dseg dorder dcard dobjs dnoise in
         Myseq.return (Data.make_dpat v r c [|dsize; dseg; dorder; dcard; dobjs; dmerger; dnoise|])

      | ColorPartition, [||], [|parse_size; parse_ncol; parse_colors; parse_masks|] ->
         let* size, r_size, ncol, r_ncol, colors, r_colors, masks, r_masks =
           Ndseq.map_tup_myseq ~depth (0,0,0,0,1,1,1,1)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc) ->
                 let h, w = Grid.dims g in
                 let nc = List.length lc in
                 let layers = Grid_patterns.partition_by_color g in
                 let ncol = List.length layers in
                 let* () = Myseq.from_bool (ncol > 0) in
                 let tm = (`Sprite, true) in
                 let* layers = (* permutations of first three objects *)
                   match layers with
                   | [] -> Myseq.return layers
                   | [o1] -> Myseq.return layers
                   | [o1;o2] -> Myseq.cons layers (Myseq.return [o2;o1])
                   | o1::o2::o3::os ->
                      Myseq.cons layers
                        (Myseq.cons (o1::o3::o2::os)
                           (Myseq.cons (o2::o1::o3::os)
                              (Myseq.cons (o2::o3::o1::os)
                                 (Myseq.cons (o3::o2::o1::os)
                                    (Myseq.return (o3::o1::o2::os)))))) in
                 Myseq.return
                   (`Vec (h,w), `VecRange (rh, rw),
                    `Int ncol, `IntRange (Range.make_closed 1 nc),

                    Ndseq.seq 0 (List.map (fun (c,m) -> `Color c) layers),
                    Ndseq.seq 0 (List.map (fun (c,m) -> `ColorRange (C_OBJ,lc)) layers),

                    Ndseq.seq 0 (List.map (fun (c,m) -> `Grid m) layers),
                    Ndseq.seq 0 (List.map (fun (c,m) -> `GridRange (tm, Range.make_exact h, Range.make_exact w, [c])) layers))
              | _ -> assert false)
             (v,r) in
         let* dsize = parse_size size r_size in
         let* dncol = parse_ncol ncol r_ncol in
         let* dcolors = parse_colors colors r_colors in
         let* dmasks = parse_masks masks r_masks in
         Myseq.return (Data.make_dpat v r c [|dsize; dncol; dcolors; dmasks|])
        
      | Monocolor, [||], [|parse_col; parse_mask|] ->
         let* col, r_col, mask, r_mask =
           Ndseq.map_tup_myseq ~name:"parse/Monocolor/in_col_mask" ~depth (0,0,0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc) ->
                 if Grid.color_count Grid.transparent g = 1
                 then
                   let* c = Myseq.from_result (Grid.majority_color Grid.transparent g) in
                   let* mask = Myseq.from_result (Grid.Transf.swap_colors g c Grid.one) in
                   Myseq.return (`Color c, `ColorRange (C_OBJ, lc),
                                 `Grid mask, `GridRange ((filling,true), rh, rw, [c]))
                 else Myseq.empty
              | _ -> assert false)
             (v,r) in
         let* dcol = parse_col col r_col in
         let* dmask = parse_mask mask r_mask in
         Myseq.return (Data.make_dpat v r c [|dcol; dmask|])

      | Recoloring, [|vg1|], [|parse_map|] ->
         let depth = Ndseq.depth v in
         let* map, r_map =
           try
             Ndseq.mapi_tup_myseq ~depth (0,0)
               (fun is (v,r) ->
                 match v, r, Ndseq.index_list_broadcast vg1 is 0 with
                 | `Grid g, `GridRange (tg, rh, rw, lc), Some (`Grid g1) ->
                    (match Grid_patterns.parse_recoloring g g1 with
                     | Some mcol ->
                        let m =
                          Mymap.fold
                            (fun c1 c2 res ->
                              Mymap.add (`Color c1) (`Color c2) res)
                            mcol (Mymap.empty : (value,value) Mymap.t) in
                        let rca = `ColorRange (C_OBJ, Grid.all_colors) in (* TODO: or colors in g1 ? *)
                        let rcb = `ColorRange (C_OBJ, lc) in
                        Myseq.return (`Map m, `MapRange (rca, rcb))
                     | None -> Myseq.empty)
                 | _, _, None -> Myseq.empty
                 | _, _, Some vg1 ->
                    pp_endline xp_value v;
                    pp_endline xp_distrib r;
                    pp_endline xp_value vg1;
                    assert false)
               (v,r)
           with Invalid_argument _ -> Myseq.empty in (* dg1 is not guaranteed to have a consistent structure *)
         let* dmap = parse_map map r_map in
         Myseq.return (Data.make_dpat v r c ~src [|dmap|])
    
      | MotifMulti partial, [||], [|parse_mot; parse_core; _parse_pure; parse_mask_opt; parse_noise|] ->
         let g_bgcolor = if partial then Grid.transparent else Grid.undefined in
         let* mot, r_mot =
           let* mot = (* common choice for all items *)
             Myseq.from_list GPat.Motif.candidates_multi in
           Myseq.return
             (Ndseq.map ~depth 0 (fun _ -> `Motif mot) v,
              Ndseq.map ~depth 0 (fun _ -> `MotifRange GPat.Motif.candidates_multi) v) in
         let* dmot = parse_mot mot r_mot in
         let* core, r_core, mask_opt, r_mask_opt, noise, r_noise =
           Ndseq.map_tup_myseq ~name:"parse/Motif/in_res" ~depth (0,0,0,0,0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc), `Motif mot ->
                 let* _mot, ru, rv, g_core, mask_opt, g_noise =
                   Myseq.from_list (GPat.Motif.from_grid [mot] g_bgcolor g) in
                 assert (Grid.has_valid_size g_core); (* to make sure oversized grids are pruned out *)
                 let mask_opt, r_mask_opt =
                   match partial, mask_opt with
                   | true, Some mask ->
                      let h, w = Grid.dims mask in (* same as grid and noise *)
                      let rh, rw = Range.make_exact h, Range.make_exact w in (* already encoded in noise *) 
                      `Grid mask, `GridRange ((`Sprite,true), rh, rw, [Grid.one])
                   | _ -> `Null, `Null in (* TODO: revise handling of optional, ugly *)
                 Myseq.return
                   (`Grid g_core, `GridRange ((filling,nocolor), ru, rv, lc),
                    mask_opt, r_mask_opt,                    
                    `Grid g_noise, `GridRange ((`Noise,nocolor), rh, rw, lc))
              | _ -> assert false)
             (v, r, Data.value dmot) in
         let* dcore = parse_core core r_core in
         let* dmask_opt =
           if not partial || Ndseq.for_all (function `Null -> false | _ -> true) mask_opt
           then parse_mask_opt mask_opt r_mask_opt
           else Myseq.empty in
         let* dnoise = parse_noise noise r_noise in
         let* dpure = make_motif_multi_dpure ~depth dmot dcore dnoise in
         Myseq.return (Data.make_dpat v r c [|dmot; dcore; dpure; dmask_opt; dnoise|])
    
      | MotifBi partial, [||], [|parse_mot; parse_bgcolor; parse_color; _parse_pure; parse_mask_opt; parse_noise|] ->
         let g_bgcolor = if partial then Grid.transparent else Grid.undefined in
         let* mot, r_mot =
           let* mot = (* common choice for all items *)
             Myseq.from_list GPat.Motif.candidates_bi in
           Myseq.return
             (Ndseq.map ~depth 0 (fun _ -> `Motif mot) v,
              Ndseq.map ~depth 0 (fun _ -> `MotifRange GPat.Motif.candidates_bi) v) in
         let* dmot = parse_mot mot r_mot in
         let* bgcolor, r_bgcolor, color, r_color, mask_opt, r_mask_opt, noise, r_noise =
           Ndseq.map_tup_myseq ~name:"parse/MotifBi/in_res" ~depth (0,0,0,0,0,0,0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc), `Motif mot ->
                 let* _mot, _ru, _rv, g_core, mask_opt, g_noise =
                   Myseq.from_list (GPat.Motif.from_grid [mot] g_bgcolor g) in
                 assert (Grid.dims g_core = (2,1));
                 let bgcolor = Grid.get_pixel ~source:"parse MotifBi bgcolor" g_core 0 0 in
                 let color = Grid.get_pixel ~source:"parse MotifBi color" g_core 1 0 in
                 let* () = Myseq.from_bool (color <> Grid.transparent) in
                 let mask_opt, r_mask_opt =
                   match partial, mask_opt with
                   | true, Some mask -> `Grid mask, `GridRange ((`Sprite,true), rh, rw, [Grid.one])
                   | _ -> `Null, `Null in (* TODO: revise handling of optional, ugly *)
                 Myseq.return
                   (`Color bgcolor, `ColorRange (C_BG (filling = `Full), lc),
                    `Color color, `ColorRange (C_OBJ, list_remove bgcolor lc),
                    mask_opt, r_mask_opt,
                    `Grid g_noise, `GridRange ((`Noise,nocolor), rh, rw, lc))
              | _ -> assert false)
             (v, r, Data.value dmot) in
         let* dbgcolor = parse_bgcolor bgcolor r_bgcolor in
         let* dcolor = parse_color color r_color in
         let* dmask_opt =
           if not partial || Ndseq.for_all (function `Null -> false | _ -> true) mask_opt
           then parse_mask_opt mask_opt r_mask_opt
           else Myseq.empty in
         let* dnoise = parse_noise noise r_noise in
         let* dpure = make_motif_bi_dpure ~depth dmot dbgcolor dcolor dnoise in
         Myseq.return (Data.make_dpat v r c [|dmot; dbgcolor; dcolor; dpure; dmask_opt; dnoise|])
    
      | Metagrid, [||], [|parse_sepcolor; parse_borders; parse_dims; parse_heights; parse_widths; parse_gridss|] ->
         let make_input_dims k l rh rw =
           let aux_r rhw = (* do not use kl to define r *)
             match rhw with
             | Range.Closed (a,b) -> Range.make_closed 1 ((b+1) / 2)
             | Range.Open a -> Range.make_open 1
           in
           let rk = aux_r rh in
           let rl = aux_r rw in
           assert (Range.mem k rk);
           assert (Range.mem l rl);
           `Vec (k,l), `VecRange (rk,rl)
         and make_input_sizes start stop kl rhw sizes =
           let r, l, r_l =
             let init_range = Range.sub rhw (Range.make_exact (start + kl-1 + stop + kl)) in (* minus frontiers, and minus at least 1 for each size *)
             Array.fold_right
               (fun hw1 (r,xs,r_xs) ->
                 let r1 = (* do not use hw1 to define r1 *)
                   match Range.upper r with
                   | None -> Range.make_open 1
                   | Some b -> Range.make_closed 1 (1+b) in
                 assert (Range.mem hw1 r1);
                 Range.sub r (Range.make_exact (hw1-1)), (* minus excess of current size viz default 1 *)
                 `Int hw1 :: xs,
                 `IntRange r1 :: r_xs)
               sizes (init_range, [], []) in
           assert (Range.mem 0 r);
           Ndseq.seq 0 l,
           Ndseq.seq 0 r_l
         and make_input_gridss parts f =
           Ndseq.seq 1
             (Array.to_list
                (Array.map
                   (fun row ->
                     Ndseq.seq 0
                       (Array.to_list
                          (Array.map
                             (fun g1 -> f g1)
                             row)))
                   parts))           
         in
         let* sepcolor, r_sepcolor, borders, r_borders, dims, r_dims,
            heights, r_heights, widths, r_widths, gridss, r_gridss =
           Ndseq.map_tup_myseq ~name:"parse/Metagrid/in_res" ~depth (0,0,0,0,0,0,1,1,1,1,2,2)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc) ->
                 let h, w = Grid.dims g in
                 assert (Range.mem h rh);
                 assert (Range.mem w rw);
                 let* mg : GPat.Metagrid.t = Myseq.from_list (GPat.Metagrid.parse g) in
                 let k, l = mg.k, mg.l in
                 let* () = Myseq.from_bool (k > 1 || l > 1) in (* avoiding degenerate metagrids *)
                 let top, bot, left, right =
                   let b = mg.borders.matrix in
                   let offset c = if c = Grid.one then 1 else 0 in
                   offset b.{0,0}, offset b.{0,1},
                   offset b.{1,0}, offset b.{1,1} in
                 let dims, r_dims = make_input_dims k l rh rw in
                 let heights, r_heights = make_input_sizes top bot k rh mg.part_heights in
                 let widths, r_widths = make_input_sizes left right l rw mg.part_widths in
                 Myseq.return
                   (`Color mg.sepcolor, `ColorRange (C_BG (filling = `Full), lc),
                    `Grid mg.borders, `GridRange ((`Noise,true),
                                                  Range.make_exact 2,
                                                  Range.make_exact 2,
                                                  [Grid.one]),
                    dims, r_dims,
                    heights, r_heights,
                    widths, r_widths,
                    make_input_gridss mg.parts (fun g1 -> `Grid g1),
                    make_input_gridss mg.parts (fun g1 ->
                        (* dims are known from heigths and widths *)
                        let h1, w1 = Grid.dims g1 in
                        let rh1 = Range.make_exact h1 in
                        let rw1 = Range.make_exact w1 in
                        `GridRange ((filling,nocolor), rh1, rw1, lc)))
              | _ -> assert false)
             (v, r) in
         let* dsepcolor = parse_sepcolor sepcolor r_sepcolor in
         let* dborders = parse_borders borders r_borders in
         let* ddims = parse_dims dims r_dims in
         let* dheights = parse_heights heights r_heights in
         let* dwidths = parse_widths widths r_widths in
         let* dgridss = parse_gridss gridss r_gridss in
         Myseq.return (Data.make_dpat v r c [|dsepcolor; dborders; ddims; dheights; dwidths; dgridss|])

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
         let* dnis =
           let min = min_h in
           let max_opt = Option.map (fun m -> m - h1 + 1) max_h_opt in
           parse_nis (`Seq (aux_inputs min max_opt nis)) in
         let* dnjs =
           let min = min_w in
           let max_opt = Option.map (fun m -> m - w1 + 1) max_w_opt in
           parse_njs (`Seq (aux_inputs min max_opt njs)) in
         let* dgrid =
           let rh1 = Range.make_exact h1 in (* encoded as sequence length of nis *)
           let rw1 = Range.make_exact w1 in (* encoded as sequence length of njs *)
           parse_grid (`GridDimsCols (g1,rh1,rw1,nc)) in
         let* data = Myseq.from_result (make_drepeat dgrid dnis dnjs) in
         Myseq.return data *)
    
      | (Empty | Full as c), [||], [|parse_size|] ->
         let pred =
           match c with
           | Empty -> (fun i j c -> c = Grid.zero)
           | Full -> (fun i j c -> c = Grid.one)
           | _ -> assert false
         in
         let* size, r_size =
           Ndseq.map_tup_myseq ~depth (0,0)
             (function
              | `Grid mask, `GridRange (tmask, rh, rw, lc) -> (* nc = 1 *)
                 let h, w = Grid.dims mask in
                 if Grid.for_all_pixels pred mask
                 then Myseq.return (`Vec (h,w), `VecRange (rh,rw))
                 else Myseq.empty
              | _ -> assert false)
             (v,r) in
         let* dsize = parse_size size r_size in
         Myseq.return (Data.make_dpat v r c [|dsize|])

      | Point, [||], [||] ->
         if Ndseq.for_all ~depth
              (function
               | `Grid mask -> (* nc=1 *)
                  let h, w = Grid.dims mask in
                  h=1 && w=1 && Grid.Mask.mem 0 0 mask
               | _ -> assert false)
              v
         then Myseq.return (Data.make_dpat v r c [||])
         else Myseq.empty
    
      | Line, [||], [|parse_len; parse_dir|] ->
         let* len, r_len, dir, r_dir =
           Ndseq.map_tup_myseq ~name:"parse/Line/in_res" ~depth (0,0,0,0)
             (function
              | `Grid mask, `GridRange (tmask, rh, rw, lc) -> (* 1 color *)
                 (match GPat.parse_line mask with
                  | Some (len, (di,dj)) ->
                     Myseq.return
                       (`Int len, `IntRange (Range.union rh rw),
                        `Vec (di,dj), `VecRange (Range.make_closed 0 1, Range.make_closed (-1) 1))
                  | None -> Myseq.empty)
              | _ -> assert false)
             (v,r) in
         let* dlen = parse_len len r_len in
         let* ddir = parse_dir dir r_dir in
         Myseq.return (Data.make_dpat v r c [|dlen; ddir|])

      | ColorSeq dir, [||], [|parse_size; parse_colors|] ->
         let* size, r_size, colors, r_colors =
           Ndseq.map_tup_myseq ~name:"parse/ColorSeq/in_res" ~depth (0,0,1,1)
             (function
              | `Grid g, `GridRange (tg, rh, rw, lc) ->
                 let h, w = Grid.dims g in
                 let rc = `ColorRange (C_OBJ, lc) in
                 (match dir with
                  | `H ->
                     let* () = Myseq.from_bool (h = 1) in
                     Myseq.return
                       (`Int w, `IntRange rw,

                        Ndseq.seq 0
                          (List.init w (fun j ->
                               `Color (Grid.get_pixel g 0 j))),
                        Ndseq.seq 0 (List.init w (fun j -> rc)))
                  | `V ->
                     let* () = Myseq.from_bool (w = 1) in
                     Myseq.return
                       (`Int h, `IntRange rh,

                        Ndseq.seq 0
                          (List.init h (fun i ->
                               `Color (Grid.get_pixel g i 0))),
                        Ndseq.seq 0 (List.init h (fun i -> rc))))
              | _ -> assert false)
             (v,r) in
         let* dsize = parse_size size r_size in
         let* dcolors = parse_colors colors r_colors in
         Myseq.return (Data.make_dpat v r c [|dsize; dcolors|])
    
      | ColorMat, [||], [|parse_size; parse_colorss|] ->
         let* size, r_size, colorss, r_colorss =
           Ndseq.map_tup_myseq ~name:"parse/ColorMat/in_res" ~depth (0,0,2,2)
             (function
              | `Grid g, `GridRange (tg, rh, rw, lc) ->
                 let h, w = Grid.dims g in
                 let rc = `ColorRange (C_OBJ, lc) in
                 (* XX let* () = Myseq.from_bool (h > 1 && h <= 3 && w > 1 && w <= 3) in *)
                 Myseq.return
                   (`Vec (h,w), `VecRange (rh,rw),
                    
                    Ndseq.seq 1
                      (List.init h (fun i ->
                           Ndseq.seq 0
                             (List.init w (fun j ->
                                  `Color (Grid.get_pixel g i j))))),
                    Ndseq.seq 1
                      (List.init h (fun i ->
                           Ndseq.seq 0
                             (List.init w (fun j ->
                                  rc)))))
              | _ -> assert false)
             (v,r) in
         let* dsize = parse_size size r_size in
         let* dcolorss = parse_colorss colorss r_colorss in
         Myseq.return (Data.make_dpat v r c [|dsize; dcolorss|])

      | MakeGrid, [||], [|parse_grid|] ->
         let depth = Ndseq.depth v in
         assert (depth >= 2);
         let* grid, r_grid =
           Ndseq.map_tup_myseq ~name:"parse/MakeGrid/grid" ~depth:(depth - 2) (0,0)
             (fun (v,r) ->
               let* g = Myseq.from_result (make_grid_from_color_seq_seq v) in
               (* let filling =
                 match tc with
                 | C_OBJ | C_BG true -> `Full
                 | C_BG false -> `Sprite in *)
               let h, w = Grid.dims g in
               let rh = Range.make_exact h in (* grid dims known from above, patterns introducing color seq seq *)
               let rw = Range.make_exact w in
               let lc = Grid.all_colors in
               Myseq.return (`Grid g, `GridRange ((`Sprite,false), rh, rw, lc)))
             (v,r) in
         let* dgrid = parse_grid grid r_grid in
         Myseq.return (Data.make_dpat v r c [|dgrid|])

      | SeqCons dep, [||], [|parse_hd; parse_tl|] ->
         if Ndseq.is_complete ~depth:dep v
         then
           let* hd, tl = Ndseq.head_tail ~depth:dep v in
           let* r_hd, r_tl = Ndseq.head_tail ~depth:dep r in
           let* dhd = parse_hd hd r_hd in
           let* dtl = parse_tl tl r_tl in
           let v : value = Ndseq.cons ~depth:dep (Data.value dhd) (Data.value dtl) in
           Myseq.return (Data.make_dpat v r c [|dhd;dtl|])
         else parseur_any t v r

      | SeqRepeat dep, [||], [|parse_e|] ->
         assert (depth >= 1);
         assert (dep < depth);
         if Ndseq.is_complete ~depth:dep v
         then
           let delta_d = depth - dep - 1 in
           let* e, r_e =
             Ndseq.map_tup_myseq ~name:"parse/SeqRepeat/e" ~depth:dep (delta_d, delta_d)
               (fun (v,r) ->
                 match Ndseq.as_seq v, Ndseq.as_seq r with
                 | Some (d, []), _ -> Myseq.empty
                 | Some (d, v0::l1), Some (_, r0::_) ->
                    (try
                      if List.for_all (fun v1 -> v1 = v0) l1 (* all elts should be the same value *)
                      then Myseq.return (v0,r0)
                      else Myseq.empty
                     with _ -> Myseq.empty)
                 | _ -> assert false)
               (v,r) in
           let* de = parse_e e r_e in
           let v =
             Ndseq.map_tup ~depth:dep (depth - dep)
               (fun (ve,v) ->
                 match Ndseq.as_seq v with
                 | Some (d,l) ->
                    Ndseq.seq d (List.map (fun _ -> ve) l)
                 | _ -> assert false)
               (Data.value de, v) in
           Myseq.return (Data.make_dpat v r c [|de|])
         else parseur_any t v r (* TODO: improve by using parse_e ? *)

      | SeqRange, [||], [|parse_start; parse_step|] ->
         let dep = depth - 1 in
         assert (dep >= 0);
         if Ndseq.is_complete ~depth:dep v
         then
           let* start, r_start, step, r_step =
             Ndseq.map_tup_myseq ~depth:dep (0,0,0,0)
               (fun (v,r) ->
                 match Ndseq.as_seq v, Ndseq.as_seq r with
                 | Some (_,l), Some (_,r_l) ->
                    let lint =
                      List.map
                        (function
                         | `Int x -> x
                         | _ -> assert false)
                        l in
                    (match l, r_l with
                     | `Int x0 :: `Int x1 :: _,
                       `IntRange r0 :: `IntRange r1 :: _ ->
                        let step = x1 - x0 in
                        let* () = Myseq.from_bool (lint = List.mapi (fun i _ -> x0 + i * step) l) in
                        let* range_step = (* TODO: ambiguity with ranges including negative values and Range.sub *)
                          match r0, r1 with
                          | Range.Closed (a0,b0), Range.Closed (a1,b1) ->
                             Myseq.return (Range.Closed (a1 - b0, b1 - a0))
                          | Range.Closed (a0,b0), Range.Open a1 ->
                             Myseq.return (Range.Open (a1 - b0))
                          | Range.Open a0, _ -> Myseq.empty in
                        Myseq.return (`Int x0, `IntRange r0,
                                      `Int step, `IntRange range_step)
                     | _ -> Myseq.empty)
                 | _ -> assert false)
               (v,r) in
           let* dstart = parse_start start r_start in
           let* dstep = parse_step step r_step in
           Myseq.return (Data.make_dpat v r c [|dstart; dstep|])
         else parseur_any t v r

      | SeqIndex, [|vseq|], [|parse_index|] ->
         let depth_seq = Ndseq.depth vseq in
         let* () = Myseq.from_bool (depth < depth_seq) in (* v must be an element or proper substructure of vseq *)
         let* index, r_index =
           let rec aux rev_path rev_r_path depseq vseq = (* iterating over substructures, searching v *)
             if depseq = depth
             then
               if vseq = v
               then
                 let index = Ndseq.seq 0 (List.rev rev_path) in
                 let r_index = Ndseq.seq 0 (List.rev rev_r_path) in
                 Myseq.return (index, r_index)
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
                    aux (`Int i :: rev_path) (`IntRange range :: rev_r_path) d vi
               | None -> assert false
           in
           aux [] [] depth_seq vseq in
         let* dindex = parse_index index r_index in
         Myseq.return (Data.make_dpat v r c ~src [|dindex|])

      | _ -> assert false
    

    (* description length *)

    let dl_color (c : Grid.color) (tc : typ_color) (lc : Grid.color list) : dl =
      (* Mdl.Code.uniform Grid.nb_color *)
      (* TODO: make use of lc *)
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

    let dl_seg (seg : GPat.Objects.segmentation) (lseg : GPat.Objects.segmentation list) : dl =
      Mdl.Code.uniform (List.length lseg)

    let dl_order (order : GPat.Objects.order) lorder : dl =
      Mdl.Code.uniform (List.length lorder)
         
    let dl_motif (m : GPat.Motif.t) (lm : GPat.Motif.t list) : dl =
      let sum = List.fold_left (fun res m -> res +. GPat.Motif.weight m) 0. lm in
      let prob = GPat.Motif.weight m /. sum in
      Mdl.Code.usage prob    
         
    let dl_grid g (filling,nocolor) rh rw lc : dl = (* too efficient a coding for being useful? *)
      (* lc is a list of colors, not including transparent or undefined, nocolor implies nc=1 *)
      let h, w = Grid.dims g in
      let area = h * w in
      let nc = List.length lc in
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
      | COLOR tc, `Color c -> dl_color c tc Grid.all_colors
      | SEG, `Seg seg -> dl_seg seg GPat.Objects.candidate_segmentations_connected
      | ORDER nocolor, `Order order ->
         dl_order order (GPat.Objects.candidate_orders 2 nocolor)
      | MOTIF tmot, `Motif m ->
         let lm =
           match tmot with
           | MULTI -> GPat.Motif.candidates_multi
           | BI -> GPat.Motif.candidates_bi in
         dl_motif m lm
      | GRID tg, `Grid g ->
         let rmax = Range.make_closed 1 Grid.max_size in
         dl_grid g tg rmax rmax Grid.all_colors
      | OBJ tg, `Obj (`Vec (i,j), `Grid g) ->
         dl_value_scalar (INT (COORD (I, POS))) (`Int i)
         +. dl_value_scalar (INT (COORD (J, POS))) (`Int j)
         +. dl_value_scalar (GRID (`Sprite,false)) (`Grid g)
      | MAP (ka,kb), `Map m ->
         dl_map (dl_value_scalar ka) (dl_value_scalar kb) m
      | _ -> pp xp_value v; assert false

    let encoding_dany v r =
      let rec aux v r =
        match v, r with
        | `Int ij, `IntRange range -> Range.dl ij range
        | `Vec (i,j), `VecRange (ri,rj) -> Range.dl i ri +. Range.dl j rj
        | `Color c, `ColorRange (tc,lc) -> dl_color c tc lc
        | `Seg seg, `SegRange lseg -> dl_seg seg lseg
        | `Order order, `OrderRange lorder -> dl_order order lorder
        | `Motif m, `MotifRange lm -> dl_motif m lm
        | `Grid g, `GridRange (tg, rh, rw, lc) -> dl_grid g tg rh rw lc
        | `Obj (pos,g1), `ObjRange (rpos,rg1) -> aux pos rpos +. aux g1 rg1
        | `Map m, `MapRange (ra,rb) -> dl_map (fun a -> aux a ra) (fun b -> aux b rb) m
        | _ ->
           pp_endline xp_value v;
           assert false (* TODO: cover other distributions *)
      in
      Ndseq.fold_left2
        (fun dl v r -> dl +. aux v r)
        0. v r
    
    let encoding_dpat dc vsrc encs =
      match dc, encs with
      | Vec, [|enc_i; enc_j|] ->  enc_i +. enc_j
      | Square, [|enc_ij|] -> enc_ij
      | Obj, [|enc_pos; enc_g1|] -> enc_pos +. enc_g1
      | DomMap keys, [|enc_vals|] -> enc_vals (* keys encoded in model *)
      | Replace, [|enc_a; enc_b|] -> enc_a +. enc_b
      | Swap, [|enc_a; enc_b|] -> enc_a +. enc_b
      | BgColor, [|enc_col; enc_g1|] -> enc_col +. enc_g1
      | IsFull, [|enc_g1|] -> enc_g1
      | Crop, [|enc_pos; enc_size|] -> enc_pos +. enc_size
      | Objects (nmax,mode), [|enc_size; enc_seg; enc_order; enc_card; enc_objs; _enc_merger; enc_noise|] -> enc_size +. enc_seg +. enc_order +. enc_card +. enc_objs +. enc_noise (* TODO: take seg into account for encoding objects *)
      | ColorPartition, [|enc_size; enc_ncol; enc_colors; enc_masks|] -> enc_size +. enc_ncol +. enc_colors +. enc_masks
      | Monocolor, [|enc_col; enc_mask|] -> enc_col +. enc_mask
      | Recoloring, [|enc_map|] -> enc_map
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
      | MakeGrid, [|enc_grid|] -> enc_grid
      | SeqCons depth, [|enc_hd; enc_tl|] -> enc_hd +. enc_tl
      | SeqRepeat depth, [|enc_e|] -> enc_e
      | SeqRange, [|enc_start; enc_step|] -> enc_start +. enc_step
      | SeqIndex, [|enc_index|] -> enc_index
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
      | Square -> 0.
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
      | MakeGrid -> 0.
      | SeqCons depth -> Mdl.Code.universal_int_star depth
      | SeqRepeat depth -> Mdl.Code.universal_int_star depth
      | SeqRange -> 0.
      | SeqIndex -> 0.

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
      | `MostCommon_1 -> 0.
      | `LeastCommon_1 -> 0.
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
      | `Quadrants_1 -> 0.
      | `ProjI_1 | `ProjJ_1 -> 0.
      | `MaskOfGrid_1 | `GridOfMask_2 -> 0.
      | `GridOfColorSeq_1 dir -> 1.
      | `GridOfColorMat_1 -> 0.
      | `RelativePos_1 -> 0.
      | `TranslatedOnto_1 -> 0.
      | `Tiling_1 (k,l) -> Mdl.Code.universal_int_plus k +. Mdl.Code.universal_int_plus l
      | `Border_1 -> 0.
      | `Interior_1 -> 0.
      | `DNeighbors_1 -> 0.
      | `INeighbors_1 -> 0.
      | `Neighbors_1 -> 0.
      | `Unrepeat_1 -> 0.
      | `PeriodicFactor_2 p -> dl_periodicity_mode p
      | `FillResizeAlike_3 p -> dl_periodicity_mode p
      | `SelfCompose_3 -> 0.
      | `ApplySymVec_1 (sym,tv) -> Mdl.Code.uniform nb_symmetry +. Mdl.Code.uniform nb_typ_vec
      | `ApplySymGrid_1 sym -> Mdl.Code.uniform nb_symmetry
      | `UnfoldSym_1 symar -> Mdl.Code.uniform nb_symmetry_unfold
      | `CloseSym_2 symar -> Mdl.Code.uniform nb_symmetry_unfold
      | `TranslationSym_2 sym -> Mdl.Code.uniform nb_symmetry
      | `Colors_1 -> 0.
      | `MajorityColor_1 -> 0.
      | `MinorityColor_1 -> 0.
      | `ColorCount_1 -> 0.
      | `Coloring_2 -> 0.
      | `SwapColors_3 -> 0.

    (* expression index *)

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
    
    let make_index_bind (bindings : bindings) : expr_index = (* NEW VERSION *)
      Common.prof "make_index_bind" (fun () ->
      let bgcolors full =
        Grid.black :: if full then [] else [Grid.transparent] in
      let index = new Expr.index_bind in
      let () = Expr.index_add_bindings index bindings in
      let () = (* LEVEL: Obj features *)
        Common.prof "make_index/obj_features" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* Strip *)
              match t1.kind with
              | GRID (filling,nocolor) when filling <> `Full ->
                 ({t1 with kind = OBJ (filling,nocolor)}, `Strip_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Border, Interior *)
              match t1.kind with
              | OBJ (filling, nocolor) ->
                 let tres = {t1 with kind = OBJ (filling,nocolor)} in
                 (tres, `Border_1, `Default)
                 ::(tres, `Interior_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Neighbors *)
              match t1.kind with
              | OBJ (filling, nocolor) ->
                 let tres = {t1 with kind = OBJ (filling,true)} in
                 (tres, `DNeighbors_1, `Default)
                 ::(tres, `INeighbors_1, `Default)
                 ::(tres, `Neighbors_1, `Default)
                 ::res
              | _ -> res in
            res)) in
      let () = (* LEVEL: Grid features *)
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
            let res = (* Halves_1, Quadrants_1 *)
              match t1.kind with
              | GRID tg ->
                 ({t1 with ndim = t1.ndim+1}, `Halves_1 `H, `Default)
                 ::({t1 with ndim = t1.ndim+1}, `Halves_1 `V, `Default)
                 ::({t1 with ndim = t1.ndim+2}, `Quadrants_1, `Default)
                 ::res
              | _ -> res in
            res)) in
      let () = (* LEVEL: Color features, Vec features *)
        Common.prof "make_index/color_vec_features" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* Colors_1 *)
              match t1.kind with
              | GRID (filling,false) ->
                 ({kind = COLOR C_OBJ; ndim = t1.ndim+1}, `Colors_1, `Default)
                 ::res
              | _ -> res in
            let res =  (* MajorityColor_1, MinorityColor_1 *)
              match t1.kind with
              | GRID (filling,false) ->
                 (* let full = (filling = `Full) in *)
                 (* let$ res, tc = res, [C_BG full; C_OBJ] in *)
                 let tres = {t1 with kind = COLOR C_OBJ} in
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
            let res = (* RelativePos_1, TranslatedOnto_1 *)
              match t1.kind with
              | OBJ _ when t1.ndim > 0 ->
                 ({kind = VEC POS; ndim = t1.ndim + 1}, `TranslatedOnto_1, `Default)
                 ::({kind = VEC POS; ndim = t1.ndim + 1}, `RelativePos_1, `Default)
                 ::res
              | _ -> res in
            (* TODO: TranslationSym, only inter objects, handle against GRID with negative object positions *)
            res)) in
      let () = (* LEVEL: Int features, Color to Grid *)
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
              | OBJ tg | GRID tg ->
                 ({t1 with kind = INT (COORD (J,POS))}, `Left_1, `Default)
                 ::({t1 with kind = INT (COORD (J,POS))}, `Right_1, `Default)
                 ::({t1 with kind = INT (COORD (J,POS))}, `Center_1, `Default)
                 ::({t1 with kind = INT (COORD (I,POS))}, `Top_1, `Default)
                 ::({t1 with kind = INT (COORD (I,POS))}, `Bottom_1, `Default)
                 ::({t1 with kind = INT (COORD (I,POS))}, `Middle_1, `Default)
                 ::({t1 with kind = VEC POS}, `MiddleCenter_1, `Default)
                 ::res
              | _ -> res in
            let res = (* GridOfColorSeq, GridOfColorMat *)
              match t1.kind with
              | COLOR tc ->
                 let filling =
                   match tc with
                   | C_BG false -> `Sprite
                   | _ -> `Full in
                 let kind = GRID (filling,false) in
                 let res =
                   if t1.ndim >= 1
                   then
                     ({kind; ndim = t1.ndim-1}, `GridOfColorSeq_1 `H, `Default)
                     ::({kind; ndim = t1.ndim-1}, `GridOfColorSeq_1 `V, `Default)
                     ::res
                   else res in
                 let res =
                   if t1.ndim >= 2
                   then ({kind; ndim = t1.ndim-2}, `GridOfColorMat_1, `Default)::res
                   else res in
                 res
              | _ -> res in
            res)) in
  (* TODO: binary exprs too costly
      let () = (* LEVEL: Int+Vec bin *)
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
      let () = (* LEVEL: INT+VEC affine, GRID derived *)
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
                 let tb = scalar (INT ti) in
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
      let () = (* LEVEL: INT+VEC transpose *)
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
      let () = (* LEVEL: GRID compose *)
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
      let () = (* LEVEL: GRID mask *)
        Common.prof "make_index/grid_mask" (fun () ->
        Expr.index_apply_functions_1
          ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* MaskOfGrid, LogNot *)
              match t1.kind with
              | GRID ((`Sprite|`Noise as filling), false) ->
                 let tres = {t1 with kind = GRID (filling, true)} in
                 (tres, `MaskOfGrid_1, `Default)
                 ::(tres, `LogNot_1, `Custom [| `Apply (tres, `MaskOfGrid_1, [|`Pos 0|]) |])
                 ::res
              | _ -> res in
            res)) in
      let () = (* LEVEL: ALL items and slices *)
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
      let () = (* LEVEL: collection-wise *)
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
              let res = (* MostCommon, LeastCommon *)
                (t1_scalar, `MostCommon_1, `Default)
                ::(t1_scalar, `LeastCommon_1, `Default)
                ::res in
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
      let () = (* LEVEL: cast *)
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
      (index :> expr_index))

    let make_index_bind, reset_make_index_bind =
      Memo.memoize ~name:"make_index_bind" ~size:103 make_index_bind

    let make_index_union (bindings : bindings) : expr_index =
      Common.prof "make_index_union" (fun () ->
      let index = new Expr.index_union in
      let () =
        Mymap.iter
          (fun x tv ->
            index#add_index (make_index_bind (Mymap.singleton x tv)))
          bindings in
      (index :> expr_index))

    let make_index_union, reset_make_index_union =
      Memo.memoize ~name:"make_index_union" ~size:103 make_index_union

    let make_index = make_index_union
    
    (* refining *)

    let decompositions (t : typ) (varseq : varseq) (valuess : value list list) : (model * varseq) list =
      let ndim = t.ndim in
      (*if not (ndim = Ndseq.depth (List.hd (List.hd valuess))) then (
        pp_endline xp_typ t;
        pp_endline xp_value (List.hd (List.hd valuess))
      );*)
      let rs = [] in
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
      let rs = (* ColorSeq *)
        match t.kind with
        | GRID (filling,nocolor) ->
           if filling = `Full && not nocolor
              && List.for_all
                   (fun values ->
                     List.for_all
                       (function
                        | `Grid g ->
                           let h, w = Grid.dims g in
                           (h = 1 && w <= 6) || (h <= 6 && w = 1)
                        | _ -> false)
                       values)
                   valuess
           then (* TODO: allow when not full, impact on color type *)
             let xsize, varseq = Refining.new_var varseq in
             let xloop, varseq = Refining.new_var varseq in
             let xcol, varseq = Refining.new_var varseq in
             let$ rs, (dir,axis) = rs, [`H, J; `V, I] in
             (Model.make_pat t (ColorSeq dir)
                [| Model.make_def xsize (Model.make_any {t with kind = INT (COORD (axis, SIZE))});
                   Model.make_def xcol (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+1}) |],
              varseq)
             ::rs
           else rs
        | _ -> rs in
      let rs = (* ColorMat *)
        match t.kind with
        | GRID (filling,nocolor) ->
           if filling = `Full && not nocolor
              && List.for_all
                   (fun values ->
                     List.for_all
                       (function
                        | `Grid g ->
                           let h, w = Grid.dims g in
                           h <= 3 && w <= 3
                        | _ -> false)
                       values)
                   valuess
           then
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
             ::rs
           else rs
        | _ -> rs in
      let rs = (* adding MakeGrid *)
        match t.kind with
        | COLOR tc when ndim >= 2 ->
           (* let filling =
             match tc with
             | C_OBJ | C_BG true -> `Full
             | C_BG false -> `Sprite in *)
           let xgrid, varseq = Refining.new_var varseq in
           (Model.make_pat t MakeGrid
              [| Model.make_def xgrid (Model.make_any {kind = GRID (`Sprite,false); ndim = ndim-2})|],
            varseq) :: rs
        | _ -> rs in
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
    
    let refinements_any (t : typ) (varseq : varseq) (value : value) : (model * varseq) list = (* QUICK *)
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
      | VEC tv ->
         let rs = (* Square *)
           match tv with
           | SIZE | MOVE ->
              let xij, varseq = Refining.new_var varseq in
              (Model.make_pat t Square
                 [| Model.make_def xij (Model.make_any {t with kind = INT (COORD (I, tv))}) |],
               varseq) :: rs
           | POS -> rs in (* not relevant for positions *)
         rs
      | COLOR tc -> rs
      | SEG -> rs
      | ORDER _ -> rs
      | MOTIF tmot -> rs
      | MAP (ka,kb) ->
         let refs : (model * varseq) list = rs in
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
      | GRID (filling,nocolor) ->
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
         let refs = (* Objects - Connected *)
           if filling <> `Full then
             let xsize, varseq = Refining.new_var varseq in
             let xsize_i, varseq = Refining.new_var varseq in
             let xsize_j, varseq = Refining.new_var varseq in
             let xseg, varseq = Refining.new_var varseq in
             let xorder, varseq = Refining.new_var varseq in
             let xcard, varseq = Refining.new_var varseq in
             let xobj, varseq = Refining.new_var varseq in
             let xpos, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             let xmerger, varseq = Refining.new_var varseq in
             let xnoise, varseq = Refining.new_var varseq in
             let$ refs, nmax = refs, [1;9] in
             (Model.make_pat t (Objects (nmax, `Connected))
                [| Model.make_def xsize (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def xseg (Model.make_any {t with kind = SEG});
                   Model.make_def xorder (Model.make_any {t with kind = ORDER nocolor});
                   Model.make_def xcard (Model.make_any {t with kind = INT CARD});
                   Model.make_def xobj
                     (Model.make_pat {kind = OBJ (`Sprite,nocolor); ndim = ndim+1} Obj
                        [| Model.make_def xpos (Model.make_any {kind = VEC POS; ndim = ndim+1});
                           Model.make_def xg1 (Model.make_any {kind = GRID (`Sprite,nocolor); ndim = ndim+1}) |]);
                   Model.make_def xmerger (Model.make_derived {t with kind = OBJ (`Sprite,nocolor)});
                   Model.make_def xnoise (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |],
              varseq)
             :: refs
           else refs in
         let refs = (* Objects - SameColor *)
           if filling <> `Full && not nocolor then
             let xsize, varseq = Refining.new_var varseq in
             let xorder, varseq = Refining.new_var varseq in
             let xcard, varseq = Refining.new_var varseq in
             let xobj, varseq = Refining.new_var varseq in
             let xpos, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             let xg1_color, varseq = Refining.new_var varseq in
             let xg1_mask, varseq = Refining.new_var varseq in
             let xmerger, varseq = Refining.new_var varseq in
             let xnoise, varseq = Refining.new_var varseq in
             let$ refs, nmax = refs, [1;9] in
             (Model.make_pat t (Objects (nmax, `SameColor))
                [| Model.make_def xsize (Model.make_any {t with kind = VEC SIZE});
                   Model.make_expr
                     (Expr.Const ({t with kind = SEG}, `Seg GPat.Objects.SameColor));
                   Model.make_def xorder (Model.make_any {t with kind = ORDER nocolor});
                   Model.make_def xcard (Model.make_any {t with kind = INT CARD});
                   Model.make_def xobj
                     (Model.make_pat {kind = OBJ (`Sprite,nocolor); ndim = ndim+1} Obj
                        [| Model.make_def xpos (Model.make_any {kind = VEC POS; ndim = ndim+1});
                           Model.make_def xg1
                             (Model.make_pat {kind = GRID (`Sprite,nocolor); ndim = ndim+1} Monocolor
                                [| Model.make_def xg1_color (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+1});
                                   Model.make_def xg1_mask (Model.make_any {kind = GRID (filling,true); ndim = ndim+1}) |]) |]);
                   Model.make_def xmerger (Model.make_derived {t with kind = OBJ (`Sprite,nocolor)});
                   Model.make_def xnoise (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |],
              varseq)
             :: refs
           else if filling <> `Full && nocolor then
             let xsize, varseq = Refining.new_var varseq in
             let xorder, varseq = Refining.new_var varseq in
             let xcard, varseq = Refining.new_var varseq in
             let xobj, varseq = Refining.new_var varseq in
             let xpos, varseq = Refining.new_var varseq in
             let xg1, varseq = Refining.new_var varseq in
             let xmerger, varseq = Refining.new_var varseq in
             let xnoise, varseq = Refining.new_var varseq in
             let nmax = 1 in
             (Model.make_pat t (Objects (nmax, `SameColor))
                [| Model.make_def xsize (Model.make_any {t with kind = VEC SIZE});
                   Model.make_expr
                     (Expr.Const ({t with kind = SEG}, `Seg GPat.Objects.SameColor));
                   Model.make_def xorder (Model.make_any {t with kind = ORDER nocolor});
                   Model.make_def xcard (Model.make_any {t with kind = INT CARD});
                   Model.make_def xobj
                     (Model.make_pat {kind = OBJ (`Sprite,nocolor); ndim = ndim+1} Obj
                        [| Model.make_def xpos (Model.make_any {kind = VEC POS; ndim = ndim+1});
                           Model.make_def xg1 (Model.make_any {kind = GRID (filling,nocolor); ndim = ndim+1}) |]);
                   Model.make_def xmerger (Model.make_derived {t with kind = OBJ (`Sprite,nocolor)});
                   Model.make_def xnoise (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |],
              varseq)
             :: refs
           else refs in
         (* let refs = (* ColorPartition *)
           if filling <> `Full && not nocolor then
             let xsize, varseq = Refining.new_var varseq in
             let xncol, varseq = Refining.new_var varseq in
             let xcolors, varseq = Refining.new_var varseq in
             let xmasks, varseq = Refining.new_var varseq in
             (Model.make_pat t ColorPartition
                [| Model.make_def xsize (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def xncol (Model.make_any {t with kind = INT CARD});
                   Model.make_def xcolors (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+1});
                   Model.make_def xmasks
                     (Model.make_any
                        {kind = GRID (`Noise, true);
                         ndim = ndim+1}) |],
              varseq)
             :: refs
           else refs in *)
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
                 Model.make_pat {t with kind = GRID (`Full,true)} Full [|msize|], varseq
               else
                 Model.make_any {t with kind = GRID (filling,true)}, varseq in
             (Model.make_pat t Monocolor
                [| Model.make_def xcol (Model.make_any {t with kind = COLOR C_OBJ});
                   Model.make_def xmask mmask |],
              varseq)
             :: refs
           else refs in
         let refs = (* Recoloring-const *)
           if not nocolor then
             let vg1_res =
               Ndseq.map_result 0
                 (function
                  | `Grid g ->
                     let| g1, _ = Grid_patterns.recoloring g in
                     Result.Ok (`Grid g1)
                  | _ -> Result.Error (Invalid_argument "refinement: Recoloring"))
                 value in
             match vg1_res with
             | Result.Ok vg1 ->
                let eg1 = Expr.Const (t, vg1) in
                let xmap, varseq = Refining.new_var varseq in
                (Model.make_pat t Recoloring ~src:[|eg1|]
                   [| Model.make_def xmap (Model.make_any {t with kind = MAP (COLOR C_OBJ, COLOR C_OBJ)}) |],
                 varseq)
                :: refs
             | _ -> refs
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
                  else Model.make_expr (Expr.Const (t_mask, `Null)));
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
                  else Model.make_expr (Expr.Const (t_mask, `Null)));
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
                 Model.make_def xborders (Model.make_any {t with kind = GRID (`Noise,true)});
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
(* XX         let refs = (* ColorSeq *)
           if filling = `Full && not nocolor
           then (* TODO: allow when not full, impact on color type *)
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
           if filling = `Full && not nocolor
           then
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
           else refs in *)
         refs
      | OBJ _ -> rs
      | _ -> assert false    
    let refinements_pat (t : typ) (c : constr) (args : model array) (varseq : varseq) (value : value) : (model * varseq) list = (* QUICK *)
      []
    (* TODO: add SeqCons/SeqRepeat(m,m) but requires global change of depths for head model *) 
    let refinements_pat_expr ~env_vars (t : typ) (varseq : varseq) (value : value) : (model * varseq) list = (* QUICK *)
      let ndim = t.ndim in
      let rs = [] in
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
        (Model.make_pat t SeqIndex ~src:[|Expr.Ref (tx, x)|]
           [| Model.make_def xindex (Model.make_any typ_index) |],
         varseq) :: rs in
      match t.kind with
      | GRID (filling,nocolor as tg) ->
         let refs = rs in
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
           (Model.make_pat t Crop ~src:[|Expr.Ref (tvar, gvar)|]
              [| Model.make_def xpos (Model.make_any {t with kind = VEC POS});
                 Model.make_def xsize (Model.make_any {t with kind = VEC SIZE}) |],
            varseq)
           :: refs in
         let refs = (* Recoloring-expr *)
           if not nocolor then
             let xmap, varseq = Refining.new_var varseq in
             let eg1s =
               Mymap.fold
                 (fun x tx res ->
                   match tx.kind with
                   | GRID (_,false) ->
                      let eg1 = Expr.Ref (tx, x) in
                      eg1::res
                   | _ -> res)
                 env_vars [] in
             let$ refs, eg1 = refs, eg1s in
             (Model.make_pat t Recoloring ~src:[|eg1|]
                [| Model.make_def xmap (Model.make_any {t with kind = MAP (COLOR C_OBJ, COLOR C_OBJ)}) |],
              varseq)
             :: refs
           else refs in
         refs
      | _ -> rs
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
      | ORDER _, _ ->
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
    let prunings_any t varseq value =
      []
    let prunings_pat t c args varseq value =
      [Model.make_any t, varseq]
    let prunings_postprocessing t m =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    (* initialization *)
      
    let varseq0 : varseq = Myseq.range 1 max_int

    let get_init_config name task =
      let open Task_model in
      let varseq = varseq0 in
      let xi, varseq = Refining.new_var varseq in
      let xo, varseq = Refining.new_var varseq in
      let input_model = Model.make_def xi (Model.make_any (scalar (GRID (`Full,false)))) in
      let output_model = Model.make_def xo (Model.make_any (scalar (GRID (`Full,false)))) in
      let distrib = `GridRange ((`Full,false),
                                Range.Closed (1,Grid.max_size),
                                Range.Closed (1,Grid.max_size),
                                Grid.all_colors) in
      { varseq;
        input_model;
        output_model;
        input_distrib = distrib;
        output_distrib = distrib }

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
      reset_make_index_bind ();
      reset_make_index_union ()
  end

module MyMadil = Madil.Make(MyDomain)
