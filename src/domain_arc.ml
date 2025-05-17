 
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
      | NAT
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
    let typ_index = {kind = INT NAT; ndim = 1}
    let typ_card = {kind = INT NAT; ndim = 0}

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
      | NAT -> print#string "NAT"
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
      | `GridRange of typ_grid * Range.t * Range.t * Grid.color list * GPat.Objects.connectedness option (* height, width, colors, conn_opt *) (* TODO: consider removing typ_grid *)
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
      | `GridRange (tg,rh,rw,lc,conn_opt) ->
         xp_typ_kind ~html print (GRID tg);
         print#string "(size ~ "; xp_distrib ~html print (`VecRange (rh,rw));
         print#string ", colors ~ "; xp_distrib ~html print (`ColorRange (C_OBJ, lc));
         (match conn_opt with
          | None -> ()
          | Some conn -> print#string ", connectedness ~ "; GPat.Objects.xp_connectedness ~html print conn);
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
        (fun () -> print#string "$"; if x <> 0 then print#int x)

    let var0 = 0
    
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
      | Objects of int (* nmax *) * [`Connected|`SameColor] (* mode *) (* SIZE, SEG, ORDER, NAT, OBJ+, derived OBJ (merge), NOISE : SPRITE *) (* int is for max seq length, mode constrains SEG *)
      | Object of [`Connected|`SameColor] (* mode *) (* SIZE, SEG, OBJ, NOISE : SPRITE *) (* mode constrains SEG *)
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
      | Skyline (* SIZE, VEC MOVE, NAT+, derived NAT+ : MASK *)
      | ColorSeq of direction (* INT SIZE, COLOR+ : GRID *)
      | ColorMat (* VEC SIZE, COLOR++ : GRID *)
      | MakeGrid (* GRID : COLOR++ *)
      | Map (* [seq: X^1] Y^1 (f(unique(seq))) : Y^1 (f(seq)) *)
      | Unique (* INT, X+, NAT+ : X+ *)
      | SeqSingle of int (* depth of seq items *) (* X : X^1 *)
      | SeqPair of int (* depth of seq items *) (* X, X : X^1 *)
      | SeqCons of int (* depth of seq items *) (* head:X^k-1, tail:X^k : X^k *)
      | SeqRepeat of int (* depth of seq items *) (* X^(k-1) : X^k *)
      | SeqRange (* start:INT, step:INT : INT+ *) (* TODO: add depth arg *)
      | SeqIndex (* [seq:X^n] index:INT^1 : X^(n-k) *)
      | NdseqMap of constr

    let xp_any t ~html print () =
      xp_html_elt "span" ~classe:"model-any" ~html print
        (fun () -> print#string "?")

    let xp_direction ~html print dir =
      print#string (match dir with `H -> "horizontal" | `V -> "vertical")
    
    let rec xp_pat c xp_src xp_args ~html print () =
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
      | Object _mode, [||], [|xp_size; xp_seg; xp_obj; xp_noise|] ->
         print#string "a grid of size "; xp_size ~html print ();
         print#string " that contains 1 "; xp_seg ~html print ();
         print#string " object:";
         xp_newline ~html print ();
         xp_obj ~html print ();
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
      | Skyline, [||], [|xp_size; xp_dir; xp_pos; xp_pos_compl|] ->
         print#string "a skyline of size "; xp_size ~html print ();
         print#string " and direction "; xp_dir ~html print ();
         print#string " and positions:"; xp_pos ~html print ();
         print#string " and complement positions: "; xp_pos_compl ~html print ()
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
      | Map, [|xp_seq|], [|xp_vals|] ->
         print#string "map unique of "; xp_seq ~html print ();
         print#string " to "; xp_vals ~html print ()
      | Unique, [||], [|xp_n; xp_vals; xp_ranks|] ->
         print#string "unique with nb: "; xp_n ~html print ();
         print#string " with values: "; xp_vals ~html print ();
         print#string " with ranks: "; xp_ranks ~html print ()
      | SeqSingle depth, [||], [|xp1|] ->
         print#string ("Single[" ^ string_of_int depth ^ "]");
         xp_tuple1 xp1 ~html print ()
      | SeqPair depth, [||], [|xp1; xp2|] ->
         print#string ("Pair[" ^ string_of_int depth ^ "]");
         xp_tuple2 xp1 xp2 ~html print ((),())
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
      | NdseqMap c, xp_src, xp_args ->
         print#string "*";
         xp_pat c xp_src xp_args ~html print ()
      | _ -> assert false

    let rec xp_field ~html print = function
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
      | Object _, 0 -> print#string "size"
      | Object _, 1 -> print#string "seg"
      | Object _, 2 -> print#string "obj"
      | Object _, 3 -> print#string "noise"
      | Object _, _ -> assert false
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
      | Skyline, 0 -> print#string "size"
      | Skyline, 1 -> print#string "direction"
      | Skyline, 2 -> print#string "pos"
      | Skyline, 3 -> print#string "compl"
      | Skyline, _ -> assert false
      | ColorSeq _, 0 -> print#string "size"
      | ColorSeq _, 1 -> print#string "colors"
      | ColorSeq _, _ -> assert false
      | ColorMat, 0 -> print#string "size"
      | ColorMat, 1 -> print#string "colors"
      | ColorMat, _ -> assert false
      | MakeGrid, 0 -> print#string "grid"
      | MakeGrid, _ -> assert false
      | Map, 0 -> print#string "vals"
      | Map, _ -> assert false
      | Unique, 0 -> print#string "n"
      | Unique, 1 -> print#string "unique"
      | Unique, 2 -> print#string "ranks"
      | Unique, _ -> assert false
      | SeqSingle _, 0 -> print#string "1st"
      | SeqSingle _, _ -> assert false
      | SeqPair _, 0 -> print#string "1st"
      | SeqPair _, 1 -> print#string "2nd"
      | SeqPair _, _ -> assert false
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
      | NdseqMap c, i -> xp_field ~html print (c, i) 

    let rec constr_v_args_ndims : constr -> int * int array = function
      (* provides arity and ndim of whole value and parts (args) *)
      | Vec -> 0, [|0; 0|]
      | Square -> 0, [|0|]
      | Obj -> 0, [|0; 0|]
      | DomMap _ -> 0, [|1|]
      | Replace -> 0, [|0; 0|]
      | Swap -> 0, [|0; 0|]
      | BgColor -> 0, [|0; 0|]
      | IsFull -> 0, [|0|]
      | Crop -> 0, [|0; 0|]
      | Objects _ -> 0, [|0; 0; 0; 0; 1; 0; 0|]
      | Object _ -> 0, [|0; 0; 0; 0|]
      | ColorPartition -> 0, [|0; 0; 1; 1|]
      | Monocolor -> 0, [|0; 0|]
      | Recoloring -> 0, [|0|]
      | MotifMulti _ -> 0, [|0; 0; 0; 0; 0|]
      | MotifBi _ -> 0, [|0; 0; 0; 0; 0; 0|]
      | Metagrid -> 0, [|0; 0; 0; 1; 1; 2|]
      | Repeat -> 0, [|0; 1; 1|]
      | Empty -> 0, [|0|]
      | Full -> 0, [|0|]
      | Point -> 0, [||]
      | Line -> 0, [|0; 0|]
      | Skyline -> 0, [|0; 0; 1; 1|]
      | ColorSeq _ -> 0, [|0; 1|]
      | ColorMat -> 0, [|0; 2|]
      | MakeGrid -> 2, [|0|]
      | Map -> 1, [|1; 1|]
      | Unique -> 1, [| 0; 1; 1|]
      | SeqSingle dep -> dep+1, [|dep|]
      | SeqPair dep -> dep+1, [|dep; dep|]
      | SeqCons dep -> dep+1, [| dep; dep+1|]
      | SeqRepeat dep -> dep+1, [|dep|]
      | SeqRange -> 1, [|0; 0|]
      | SeqIndex -> raise TODO (* v_ndim not well-defined *)
      | NdseqMap c -> raise TODO

    
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
      | `UniqueVals_1 (* X^k -> X^k: keeping only first occurrences of items *)
      | `UniqueRanks_1 (* X^1 -> Index^1: rank of items among unique items *)
      | `Transpose_1 (* X^k -> X^k *)
      | `Flatten_1 of bool (* by rows vs cols *) * bool (* like snake *) (* X^k -> X^k-1 *)
      | `Cardinal_1 (* X^k -> Int *)
      | `Count_1 (* Int^k -> Int *)
      | `DistinctCount_1 (* Int^k -> Int *)
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
      | `UniqueVals_1 -> print#string "unique_vals"
      | `UniqueRanks_1 -> print#string "unique_ranks"
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
      | `Count_1 -> print#string "count"
      | `DistinctCount_1 -> print#string "distinct_count"
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
            [ "Map", [|t|], [|t|]; (* the src may have any other type *)
              "Unique", [||], [|{t with kind = INT NAT}; t; {t with kind = INT NAT}|];
              "SeqSingle", [||], [|t|];
              "SeqPair", [||], [|t; t|];
              "SeqCons", [||], [|t; t|];
              "SeqRepeat", [||], [|t|];
              "SeqIndex", [||], [|t; scalar (INT NAT)|] ] in
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
                               {t with kind = INT NAT};
                               {t with kind = OBJ (`Sprite,nocolor)};
                               (* derived merger, not counting *)
                               {t with kind = GRID (`Noise,nocolor)} |]);
                 not full, ("Object", [||],
                            [| {t with kind = VEC SIZE};
                               {t with kind = SEG};
                               {t with kind = OBJ (`Sprite,nocolor)};
                               {t with kind = GRID (`Noise,nocolor)} |]);
                 (* not nocolor, (ColorPartition, [||],
                               [| {t with kind = VEC SIZE};
                                  {t with kind = INT NAT};
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
                 not full && nocolor, ("Skyline", [||],
                                       [| {t with kind = VEC SIZE};
                                          {t with kind = VEC MOVE};
                                          {t with kind = INT NAT} |]); (* derived compl not counting *)
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
              "UniqueVals_1", [|t|];
              "Transpose_1", [|t|] ] in
          match t.kind with
          | BOOL -> res
          | INT NAT ->
             (* not used("Cardinal_1", [| {t with kind = OBJ (`Sprite,false)} |]) (* TODO: generalize to other kinds, and other ndims, param and result *) *)
             ("Count_1", [|t|])
             ::("DistinctCount_1", [|t|])
             ::("Sum_1", [|t|])
             ::("Min_1", [|t|])
             ::("Max_1", [|t|])
             ::("ArgMin_1", [| {t with kind = INT NAT} |]) (* TODO: should be any INT *)
             ::("ArgMax_1", [| {t with kind = INT NAT} |]) (* TODO: should be any INT *)
             ::("Plus_2", [|t (* const *)|])
             ::("Minus_2", [|t (* const *)|])
             ::("Area_1", [| {t with kind = GRID (`Sprite,false)} |])
             ::("ColorCount_1", [| {t with kind = GRID (`Sprite,false)} |]) (* also for `Noise? *)
             (* ::("Average_n", [|t; t|]) *)
             ::("UniqueRanks_1", [|{t with kind = GRID (`Sprite,false)}|]) (* TODO: should be any type, not only GRID *)
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
             ::("ScaleUp_2", [|t (* const: {t with kind = INT NAT} *) |])
             ::("ScaleDown_2", [|t (* const: {t with kind = INT NAT} *) |])
             ::res
          | VEC tv ->
             ("Pos_1", [| {t with kind = OBJ (`Sprite,false)} |])
             ::("MiddleCenter_1", [| {t with kind = OBJ (`Sprite,false) } |])
             ::("MiddleCenter_1", [| {t with kind = GRID (`Sprite,false) } |])
             ::("Size_1", [| {t with kind = GRID (`Sprite,false)} |])
             ::("Plus_2", [|t (* const: t *)|])
             ::("Minus_2", [|t (* const: t *)|])
             ::("ScaleUp_2", [|t (* const: {t with kind = INT NAT} *) |])
             ::("ScaleDown_2", [|t (* const: {t with kind = INT NAT} *) |])
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
             ::("ScaleUp_2", [|t (* const:{t with kind = INT NAT} *) |])
             ::("ScaleDown_2", [|t (* const: {t with kind = INT NAT} *) |])
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
    let max_expr_size = def_param "max_expr_size" 6 (* TEST 9 *) string_of_int (* max size of candidate expressions *)
    let max_expr_refinements_per_read = def_param "max_expr_refinements_per_read" 100 (* TEST 1000 *) string_of_int (* max nb of considered expr refinements per grid read *)
    let max_expr_refinements_per_var = def_param "max_expr_refinements_per_var" 3 string_of_int (* max nb of considered expr refinements per model var *)
    let max_refinements = def_param "max_refinements" 100 string_of_int (* max nb of considered refinements *)
    let refinement_branching = def_param "refinement_branching" 9 (* TEST 3 *) string_of_int (* max nb of explored pattern refinements at some model path during learning (refining phase). min=1 *)
    let input_branching = def_param "input_branching" 10 string_of_int (* max nb of explored input models during output model learning (refining phase). min=1 *)
    let solution_pool = def_param "solution_pool" 1 (* more is not beneficial *) string_of_int (* max nb of solutions before choosing best one *)
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

    let make_color_partition (vsize : value) (vcolors : value) (vmasks : value) : value Myseq.t =
      match vsize, Ndseq.as_seq vcolors, Ndseq.as_seq vmasks with
      | `Vec (h,w), Some (_,colors), Some (_,masks) ->
         let colors =
           List.map
             (function
              | `Color c -> c
              | _ -> assert false)
             colors in
         let masks =
           List.map
             (function
              | `Grid m -> m
              | _ -> assert false)
             masks in
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
      | _ -> assert false
    
    let make_objects_v_merger_itemwise h w card objs g_noise : value * value * distrib =
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

    let make_objects_v_merger ~depth size card objs noise : value * value * distrib =
      Ndseq.map_tup ~depth (0,0,0)
        (fun (size, card, objs, noise) ->
          match size, card, Ndseq.as_seq objs, noise with
          | `Vec (h,w), `Int card, Some (_,objs), `Grid g_noise ->
             let objs =
               List.map
                 (function
                  | `Obj (`Vec (i,j), `Grid g1) -> (i,j,g1)
                  | _ -> assert false)
                 objs in
             make_objects_v_merger_itemwise h w card objs g_noise
          | _ -> assert false)
        (size, card, objs, noise)

    let make_skyline_v_compl size dir pos : value * value * distrib =
      match size, dir, Ndseq.as_seq pos with
      | `Vec (h,w), `Vec (i,j), Some (_, lpos) ->
         let lpos =
           List.map
             (function
              | `Int p -> p
              | _ -> assert false)
             lpos in
         assert ((i=0) <> (j=0));
         assert (List.length lpos = (if i = 0 then h else w));
         let ar_pos = Array.of_list lpos in
         let pred =
           match i, j with
           | 0, 1 (* base on the left *) -> (fun i j -> j < ar_pos.(i))
           | 0, -1 (* on the right *) -> (fun i j -> j >= w - ar_pos.(i))
           | 1, 0 (* base on the top *) -> (fun i j -> i < ar_pos.(j))
           | -1, 0 (* on the bottom *) -> (fun i j -> i >= h - ar_pos.(j))
           | _ -> assert false in
         let g = Grid.Mask.init h w pred in
         let vcompl =
           let max = if i = 0 then w else h in
           Ndseq.seq 0 (List.map (fun p -> `Int (max - p)) lpos) in
         (`Grid g, vcompl, `Null)
      | _ -> assert false
    
    let make_motif_multi_pure_itemwise mot g_core g_noise : (value * distrib) Myseq.t =
      let h, w = Grid.dims g_noise in
      let* g_pure = Myseq.from_result (GPat.Motif.make_grid h w mot g_core) in
      Myseq.return (`Grid g_pure, `Null) (* TODO: define better rpure *)
    let make_motif_bi_pure_itemwise mot bgcolor color g_noise =
      let g_core = GPat.Motif.make_core_bi bgcolor color in
      make_motif_multi_pure_itemwise mot g_core g_noise

    let make_motif_multi_pure ~depth mot core noise : (value * distrib) Myseq.t =
      Ndseq.map_tup_myseq ~name:"make_motif_pure" ~depth (0,0)
        (function
         | `Motif mot, `Grid g_core, `Grid g_noise ->
            make_motif_multi_pure_itemwise mot g_core g_noise
         | _ -> assert false)
        (mot, core, noise)
    let make_motif_bi_pure ~depth mot bgcolor color noise =
      Ndseq.map_tup_myseq ~depth (0,0)
        (function
         | `Motif mot, `Color bgcolor, `Color color, `Grid g_noise ->
            make_motif_bi_pure_itemwise mot bgcolor color g_noise
         | _ -> assert false)
        (mot, bgcolor, color, noise)

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
      | `UniqueVals_1 ->
         (function
          | [|v1|] ->
             let ndim = Ndseq.ndim v1 in
             if ndim >= 1
             then
               Result.Ok (Ndseq.map ~depth:(ndim-1) 0
                            (Ndseq.seq_of_seq list_unique_vals)
                          v1)
             else Result.Error (Undefined_result "unique_vals: not defined on scalars")
          | _ -> assert false)
      | `UniqueRanks_1 ->
         (function
          | [|v1|] ->
             let ndim = Ndseq.ndim v1 in
             if ndim >= 1
             then
               Result.Ok
                 (Ndseq.map ~depth:(ndim-1) 0
                    (fun v1 ->
                      match Ndseq.as_seq v1 with
                      | Some (0, l) ->
                         let _unique, ranks = list_unique_ranks l in
                         Ndseq.seq 0 (List.map (fun n -> `Int n) ranks)
                      | _ -> assert false)
                    v1)
             else Result.Error (Undefined_result "unique_ranks: not defined on scalars")
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
      | `Count_1 ->
         (function
          | [|v1|] ->
             let| count =
               eval_aggreg "count"
                 (fun v -> Some 1)
                 (fun (sum, _) -> Some (sum + 1))
                 v1 in
             Result.Ok (`Int count)
          | _ -> assert false)
      | `DistinctCount_1 ->
         (function
          | [|v1|] ->
             let| seen =
               eval_aggreg "distinct_count"
                 (fun v -> Some (Bintree.singleton v))
                 (fun (seen, v) -> Some (Bintree.add v seen))
                 v1 in
             Result.Ok (`Int (Bintree.cardinal seen))
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
                      lc,
                      conn_opt) ->
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

    type generator_pat_ndseq = [generator_pat | generator_pat_ndseq Ndseq.seq]

    let ( let+ ) ir f = Myseq.return (`NextArg (ir,f))
    let ( let++ ) (ir1,ir2) f = Myseq.return (`NextArg2 (ir1,ir2,f))
    let ( let+++ ) irs f = Myseq.return (`NextArgs (irs,f))
    let ( let= ) ivr f = Myseq.return (`NextDerived (ivr,f))
    let res_val v = Myseq.return (`ResVal v)

    let rec generator_pat t c src k (r : distrib) : generator_pat Myseq.t =
      let depth = t.ndim in
      let ndim = t.ndim in
      assert (Ndseq.depth r = depth);
      let args_index = Array.init k (fun i -> i) in
      match c, src, args_index, r with
      | Vec, [||], [|i;j|], `VecRange (ri,rj) ->
         let++ vi, vj =
           (i, `IntRange ri),
           (j, `IntRange rj) in
         let v =
           match vi, vj with
           | `Int i, `Int j -> `Vec (i,j)
           | _ -> assert false in
         res_val v

      | Square, [||], [|ij|], `VecRange (Range.Closed (mini,maxi), Range.Closed (minj,maxj)) ->
         let+ vij =
           ij, `IntRange (Range.Closed (max mini minj,
                                        min maxi maxj)) (* interval intersection because i = j *) in
         let v =
           match vij with
           | `Int i -> `Vec (i,i)
           | _ -> assert false in
         res_val v
    
      | Obj, [||], [|pos; g1|], `ObjRange (r_pos, r_g1) ->
         let++ vpos, vg1 = (pos, r_pos), (g1, r_g1) in
         let v = `Obj (vpos,vg1) in
         res_val v
    
      | DomMap keys, [||], [|vals|], `MapRange (r_a,r_b) ->
         let k = List.length keys in
         let+ vals = vals, Ndseq.seq 0 (List.init k (fun _ -> r_b)) in
         let v =
           match Ndseq.as_seq vals with
           | Some (_, lv) ->
              assert (List.length lv = k);
              let m = mymap_of_list (List.combine keys lv) in
              `Map m
           | None -> assert false in
         res_val v

      | Replace, [||], [|a; b|], `MapRange (r_a,r_b) ->
         let++ va, vb = (a, r_a), (b, r_b) in
         let v = `Map (mymap_of_list [va, vb; vb, vb]) in
         res_val v
    
      | Swap, [||], [|a; b|], `MapRange (r_a,r_b) ->
         let++ va, vb = (a, r_a), (b, r_b) in
         let v = `Map (mymap_of_list [va, vb; vb, va]) in
         res_val v
    
      | BgColor, [||], [|bc; g1|], `GridRange ((filling,nocolor), rh, rw, lc, None) ->
         let+ vbc = bc, `ColorRange (C_BG (filling = `Full), lc) in
         let r_g1 =
           match vbc with
           | `Color bc ->
              let lc1 = List.filter ((<>) bc) lc in
              `GridRange ((`Sprite,nocolor), rh, rw, lc1, None)
           | _ -> assert false in
         let+ vg1 = g1, r_g1 in
         let v =
           match vbc, vg1 with
           | `Color bc, `Grid g1 -> `Grid (Grid.fill_transparent g1 bc)
           | _ -> assert false in
         res_val v

      | IsFull, [||], [|g1|], _ ->
         let+ vg1 = g1, r in
         res_val vg1
    
      | Crop, [|vg|], [|pos; size|], `GridRange ((filling,nocolor),
                                                        Range.Closed (h1min,h1max),
                                                        Range.Closed (w1min,w1max),
                                                        lc1,
                                                        conn1_opt) ->
         (match vg with
          | `Grid g ->
             let h, w = Grid.dims g in
             let++ vpos, vsize =
               (pos, `VecRange (Range.Closed (0,0), Range.Closed (0,0))),
               (size, `VecRange (Range.Closed (min h h1min, min h h1max), Range.Closed (min w w1min, min w w1max))) in
             let* v =
               match vpos, vsize with
               | `Vec (i,j), `Vec (h1,w1) ->
                  let* g  = Myseq.from_result (Grid.Transf.crop g i j h1 w1) in
                  Myseq.return (`Grid g)
               | _ -> assert false in
             res_val v
          | _ -> Myseq.empty)

      | Objects (nmax,mode), [||], [|size; seg; order; card; objs; merger; noise|], _ ->
         let r_seg, r_order, r_card =
           Ndseq.map_tup ~depth (0,0,0)
             (fun _ ->
               `SegRange [ match mode with
                           | `Connected -> GPat.Objects.(Connected (Connect8,false))
                           | `SameColor -> GPat.Objects.SameColor ],
               `OrderRange [GPat.Objects.Pos], (* this will not be used when generating objects *)
               `IntRange (Range.Closed (1,nmax)))
             (tup1 r) in
         let+++ l = [seg, r_seg; order, r_order; card, r_card] in
         (match l with
          | [vseg; vorder; vcard] ->
             (* let* () = Myseq.from_bool
                (Ndseq.for_all
                (function
                | `Int card -> card <= nmax
                | _ -> assert false)
                vcard) in *)
             let r_objs =
               Ndseq.map_tup ~depth 1
                 (function
                  | `Seg seg, `Int card, `GridRange ((filling,nocolor),
                                                     Range.Closed (minh,maxh),
                                                     Range.Closed (minw,maxw),
                                                     lc,
                                                     conn_opt) ->
                     let r_obj = `ObjRange (`VecRange (Range.Closed (0,0),
                                                       Range.Closed (0,0)),
                                            `GridRange ((`Sprite,nocolor),
                                                        Range.Closed (1,3),
                                                        Range.Closed (1,3),
                                                        lc,
                                                        GPat.Objects.seg_conn_opt seg)) in
                     Ndseq.seq 0 (List.init card (fun _ -> r_obj))
                  | _ -> assert false)
                 (vseg, vcard, r) in
             let+ vobjs = objs, r_objs in
             let r_size, r_noise =
               Ndseq.map_tup ~depth (0,0)
                 (fun (seq_objs, r) ->
                   match Ndseq.as_seq seq_objs, r with
                   | Some (0, objs), `GridRange ((filling,nocolor),
                                                 Range.Closed (minh,maxh),
                                                 Range.Closed (minw,maxw),
                                                 lc,
                                                 conn_opt) ->
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
                                  [Grid.transparent],
                                  None)
                   | _ -> assert false)
                 (vobjs, r) in
             let+ vsize = size, r_size in
             let+ vnoise = noise, r_noise in
             let v, vmerger, r_merger =
               make_objects_v_merger ~depth vsize vcard vobjs vnoise in
             let= () = merger, vmerger, r_merger in
             res_val v
          | _ -> assert false)

      | Object mode, [||], [|size; seg; obj; noise|], _ ->
         let r_seg =
           Ndseq.map_tup ~depth 0
             (fun _ ->
               `SegRange [ match mode with
                           | `Connected -> GPat.Objects.(Connected (Connect8,false))
                           | `SameColor -> GPat.Objects.SameColor ])
             (tup1 r) in
         let+ vseg = seg, r_seg in
         let r_obj =
           Ndseq.map_tup ~depth 1
             (function
              | `Seg seg, `GridRange ((filling,nocolor),
                                      Range.Closed (minh,maxh),
                                      Range.Closed (minw,maxw),
                                      lc,
                                      conn_opt) ->
                 `ObjRange (`VecRange (Range.Closed (0,0),
                                       Range.Closed (0,0)),
                            `GridRange ((`Sprite,nocolor),
                                        Range.Closed (1,3),
                                        Range.Closed (1,3),
                                        lc,
                                        GPat.Objects.seg_conn_opt seg))
              | _ -> assert false)
             (vseg, r) in
         let+ vobj = obj, r_obj in
         let r_size, r_noise =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `Obj (`Vec (i,j), `Grid g1),
                `GridRange ((filling,nocolor),
                            Range.Closed (minh,maxh),
                            Range.Closed (minw,maxw),
                            lc,
                            conn_opt) ->
                 let h1, w1 = Grid.dims g1 in
                 let minh, minw = max minh (i+h1), max minw (j+w1) in
                 let maxh, maxw = max maxh minh, max maxw minw in
                 `VecRange (Range.Closed (minh,maxh), Range.Closed (minw,maxw)),
                 `GridRange ((`Noise,nocolor),
                             Range.Closed (minh,maxh),
                             Range.Closed (minw,maxw),
                             [Grid.transparent],
                             None)
              | _ -> assert false)
             (vobj, r) in
         let+ vsize = size, r_size in
         let+ vnoise = noise, r_noise in
         let v, _vmerger, _r_merger =
           let vcard =
             Ndseq.map ~depth 0
               (fun _ -> `Int 1)
               r in
           let vobjs =
             Ndseq.map ~depth 1
               (fun vobj -> Ndseq.seq 0 [vobj])
               vobj in             
           make_objects_v_merger ~depth vsize vcard vobjs vnoise in
         res_val v

      | ColorPartition, [||], [|size; ncol; colors; masks|],
        `GridRange ((filling,nocolor),
                    Range.Closed (hmin,hmax),
                    Range.Closed (wmin,wmax),
                    lc,
                    conn_opt) ->
         let++ vsize, vncol =
           (size, `VecRange (Range.Closed (hmin,hmax), Range.Closed (wmin,wmax))),
           (ncol, `IntRange (Range.Closed (1, List.length lc))) in
         let r_colors, r_masks =
           match vsize, vncol with
           | `Vec (h,w), `Int ncol ->
              let r_color = `ColorRange (C_OBJ, lc) in (* TODO: constrain different colors across sequence *)
              let r_mask = `GridRange ((`Sprite,true),
                                       Range.Closed (h,h),
                                       Range.Closed (w,w),
                                       [Grid.one],
                                       None) in
              Ndseq.seq 0 (List.init ncol (fun _ -> r_color)),
              Ndseq.seq 0 (List.init ncol (fun _ -> r_mask))
           | _ -> assert false in
         let+ vcolors = colors, r_colors in
         let+ vmasks = masks, r_masks in
         let* v = make_color_partition vsize vcolors vmasks in
         res_val v
    
      | Monocolor, [||], [|col; mask|], `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
              
         let++ vcol, vmask =
           (col, `ColorRange (C_OBJ, lc)),
           (mask, `GridRange ((filling,true), rh, rw, [Grid.one], conn_opt)) in
         let* v =
           match vcol, vmask with
           | `Color c, `Grid g1 ->
              let* g = Myseq.from_result (Grid.Transf.swap_colors g1 Grid.one c) in
              Myseq.return (`Grid g)
           | _ -> assert false in
         res_val v

      | Recoloring, [|vg1|], [|map|], `GridRange (tg, rh, rw, lc, conn_opt) ->
         (match vg1 with
          | `Grid g1 ->
             let rc = `ColorRange (C_OBJ, lc) in
             let+ vmap = map, `MapRange (rc, rc) in
             let* v =
               match vmap with
               | `Map mcol ->
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
               | _ -> assert false in
             res_val v
          | _ -> Myseq.empty)

      | MotifMulti partial, [||], [|mot; core; pure; mask_opt; noise|], _ ->
         let r_mot, r_noise =
           Ndseq.map_tup ~depth (0,0)
             (function
              | `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
                 `MotifRange GPat.Motif.candidates_multi,
                 `GridRange ((`Noise,nocolor), rh, rw, [Grid.transparent], None)
              | _ -> assert false)
             (tup1 r) in
         let++ vmot, vnoise = (mot, r_mot), (noise, r_noise) in
         let* r_mask_opt, r_core =
           Ndseq.map_tup_myseq ~name:"gen/MotifMulti/r_res" ~depth (0,0)
             (function
              | `GridRange (tg, rh, rw, lc, conn_opt), `Motif mot, `Grid gnoise ->
                 let h, w = Grid.dims gnoise in
                 let _, _, luv = GPat.Motif.all_coredims_of_motif mot h w in
                 let* u, v = Myseq.from_list luv in
                 Myseq.return
                   ((if partial
                     then `GridRange ((`Sprite,true),
                                      Range.Closed (h,h),
                                      Range.Closed (w,w),
                                      [Grid.one],
                                      conn_opt)
                     else `Null),
                    `GridRange (tg, Range.Closed (u,u), Range.Closed (v,v), lc, None))
              | _ -> assert false)
             (r, vmot, vnoise) in
         let+ vcore = core, r_core in
         let+ vmask_opt = mask_opt, r_mask_opt in
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
             (vmot, vcore, vmask_opt, vnoise) in
         let= () = pure, vpure, rpure in
         res_val v

      | MotifBi partial, [||], [|mot; bgcolor; color; pure; mask_opt; noise|], _ ->
         let* r_mot, r_noise =
           Ndseq.map_tup_myseq ~depth (0,0)
             (function
              | `GridRange ((filling,nocolor), Range.Closed (minh,maxh), Range.Closed (minw,maxw), lc, conn_opt) ->
                 if maxh >= 3 && maxw >= 3 (* bicolor motifs have size at least 3x3 *)
                 then
                   Myseq.return
                     (`MotifRange GPat.Motif.candidates_bi,
                      `GridRange ((`Noise,nocolor),
                                  Range.Closed (max 3 minh, maxh),
                                  Range.Closed (max 3 minw, maxw),
                                  [Grid.transparent],
                     None))
                 else Myseq.empty
              | _ -> assert false)
             (tup1 r) in
         let++ vmot, vnoise = (mot, r_mot), (noise, r_noise) in
         let* r_mask_opt, r_bgcolor =
           Ndseq.map_tup_myseq ~name:"gen/Motif/r_res" ~depth (0,0)
             (function
              | `GridRange ((filling,nocolor), Range.Closed (minh,maxh), Range.Closed (minw,maxw), lc, conn_opt), `Motif mot, `Grid gnoise ->
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
                                      [Grid.one],
                                      conn_opt)
                     else `Null),
                    `ColorRange (C_BG (filling = `Full), lbgcolor))
              | _ -> assert false)
             (r, vmot, vnoise) in
         let+ vbgcolor = bgcolor, r_bgcolor in
         let r_color =
           Ndseq.map_tup ~depth 0
             (function
              | `GridRange (_, _, _, lc, _), `Color bgcolor ->
                 `ColorRange (C_OBJ, list_remove bgcolor lc)
              | _ -> assert false)
             (r, vbgcolor) in
         let+ vcolor = color, r_color in
         let+ vmask_opt = mask_opt, r_mask_opt in
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
             (vmot, vbgcolor, vcolor, vmask_opt, vnoise) in
         let= () = pure, vpure, rpure in
         res_val v

      | Metagrid, [||], [|sepcolor; borders; dims; heights; widths; gridss|],
        `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
         let r_sepcolor, r_borders, r_dims =
           `ColorRange (C_BG (filling = `Full), lc),
           `GridRange ((`Noise,true), Range.Closed (2,2), Range.Closed (2,2), [Grid.one], None),
           `VecRange (Range.Closed (1,3), Range.Closed (1,3)) in
         let+++ l = [sepcolor, r_sepcolor; borders, r_borders; dims, r_dims] in
         (match l with
          | [vsepcolor; vborders; vdims] ->
             let r_heights, r_widths =
               match vdims with
               | `Vec (k,l) ->
                  Ndseq.seq 0 (List.init k (fun _ -> `IntRange (Range.Closed (1,3)))),
                  Ndseq.seq 0 (List.init l (fun _ -> `IntRange (Range.Closed (1,3))))
               | _ -> assert false in
             let++ vheights, vwidths = (heights, r_heights), (widths, r_widths) in
             let r_gridss =
               match vsepcolor, vdims, vheights, vwidths with
               | `Color sepcolor, `Vec (k,l), vheights, vwidths ->
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
                                            lc1,
                                            None)))))
               | _ -> assert false in
             let+ vgridss = gridss, r_gridss in
             let* v =
               match vsepcolor, vborders, vdims, vheights, vwidths, vgridss with
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
               | _ -> assert false in
             res_val v
          | _ -> assert false)
    
(* TODO      | Repeat, [|gen_grid; gen_nis; gen_njs|], _ ->
         let* dgrid, dnis, dnjs = Myseq.product_fair3 (gen_grid r, gen_nis r, gen_njs r) in
         let* data = Myseq.from_result (make_drepeat dgrid dnis dnjs) in
         Myseq.return (data, `Null) *)
    
      | Empty, [||], [|size|], `GridRange (tg, rh, rw, _, _) -> 
         let+ vsize = size, `VecRange (rh, rw) in
         let v =
           match vsize with
           | `Vec (h,w) -> `Grid (Grid.Mask.empty h w)
           | _ -> assert false in
         res_val v

      | Full, [||], [|size|], `GridRange (_,rh,rw,_,_) -> 
         let+ vsize = size, `VecRange (rh,rw) in
         let v =
           match vsize with
           | `Vec (h,w) -> `Grid (Grid.Mask.full h w)
           | _ -> assert false in
         res_val v

      | Point, [||], [||], _ ->
         let v = `Grid (Grid.Mask.full 1 1) in
         res_val v

      | Line, [||], [|len; dir|], `GridRange (_, Range.Closed (minh,maxh), Range.Closed (minw,maxw), _, _) ->
         let+ vlen = len, `IntRange (Range.Closed (min minh minw, max maxh maxw)) in
         let+ vdir = dir, `VecRange (Range.Closed (0,1), Range.Closed (-1,1)) in
         let* v =
           match vlen, vdir with
           | `Int len, `Vec dir ->
              let* g = Myseq.from_result (GPat.generate_line len dir) in
              Myseq.return (`Grid g)
           | _ -> assert false in
         res_val v

      | Skyline, [||], [|size; dir; pos; compl|],
        `GridRange (_, Range.Closed (minh,maxh), Range.Closed (minw,maxw), _, _) ->
         let r_size, r_dir =
           `VecRange (Range.Closed (minh,maxh), Range.Closed (minw,maxw)),
           `VecRange (Range.Closed (-1,1), Range.Closed (-1,1)) in
         let+ vsize = size, r_size in
         let+ vdir = dir, r_dir in
         let* r_pos =
           match vsize, vdir with
           | `Vec (h,w), `Vec (i,j) ->
              if (i=0) = (j=0) (* invalid dir *)
              then Myseq.empty
              else
                let len, max_pos =
                  if i = 0 (* vertical skyline, pos on j axis *)
                  then h, w
                  else w, h in
                let r_pos = `IntRange (Range.Closed (0, min 2 max_pos)) in
                Myseq.return (Ndseq.seq 0 (List.init len (fun _ -> r_pos)))
           | _ -> assert false in
         let+ vpos = pos, r_pos in
         let v, vcompl, r_compl =
           make_skyline_v_compl vsize vdir vpos in
         let= () = compl, vcompl, r_compl in
         res_val v
    
      | ColorSeq dir, [||], [|size; colors|], `GridRange (tg,rh,rw,lc,conn_opt) ->
         let r_size =
           match dir with
           | `H -> `IntRange rw
           | `V -> `IntRange rh in
         let+ vsize = size, r_size in
         let r_colors =
           match vsize with
           | `Int k ->
              let rc = `ColorRange (C_OBJ,lc) in
              Ndseq.seq 0 (List.init k (fun _ -> rc))
           | _ -> assert false in
         let+ vcolors = colors, r_colors in
         let v =
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
           | _ -> assert false in
         res_val v
    
      | ColorMat, [||], [|size; colorss|], `GridRange (tg,rh,rw,lc,conn_opt) ->
         let r_size = `VecRange (rh,rw) in
         let+ vsize = size, r_size in
         let r_colorss =
           match vsize with
           | `Vec (k,l) ->
              let rc = `ColorRange (C_OBJ, lc) in
              Ndseq.seq 1
                (List.init k (fun _ ->
                     Ndseq.seq 0
                       (List.init l (fun _ ->
                            rc))))
           | _ -> assert false in
         let+ vcolorss = colorss, r_colorss in
         let v =
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
           | _ -> assert false in
         res_val v

      | MakeGrid, [||], [|grid|], _ ->
         let* r_grid =
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
           Myseq.return (`GridRange ((`Sprite,false), Range.Closed (h,h), Range.Closed (w,w), lc, None)) in
         let+ vgrid = grid, r_grid in
         let* v =
           match vgrid, r_grid with
           | `Grid g, `GridRange (_, Range.Closed (h,_), Range.Closed (w,_), _, _) ->
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
           | _ -> assert false in
         res_val v

      | Map, [|vseq|], [|vals|], `Seq (0, rs) ->
         let* r_vals =
           match vseq with
           | `Seq (0, vitems) when List.length rs = List.length vitems ->
              Myseq.return (Ndseq.seq 0 (list_unique_assoc (List.combine vitems rs)))
           | _ -> Myseq.empty in
         let+ vvals = vals, r_vals in
         let v =
           match vseq, vvals with
           | `Seq (0, items), `Seq (0, vals) ->
              let m = try List.combine (list_unique_vals items) vals with _ -> assert false in
              Ndseq.seq 0
                (List.map
                   (fun item -> try List.assoc item m with _ -> assert false)
                   items)
           | _ -> assert false in
         res_val v

      | Unique, [||], [|n; vals; ranks|], `Seq (0, l) ->
         let r_n = `IntRange (Range.Closed (0, List.length l)) in
         let+ vn = n, r_n in
         let r_ranks =
           match vn with
           | `Int n ->
              let r_rank = `IntRange (Range.Closed (0, n-1)) in
              Ndseq.seq 0 (List.map (fun _ -> r_rank) l)
           | _ -> assert false in
         let+ vranks = ranks, r_ranks in
         let r_vals =
           match vranks with
           | `Seq (0,ranks) ->
              Ndseq.seq 0 (list_unique_assoc (List.combine ranks l))
           | _ -> assert false in
         let+ vvals = vals, r_vals in
         let v =
           match vvals, vranks with
           | `Seq (0, lu), `Seq (0, lr) ->
              let ar_u = Array.of_list lu in
              let n = Array.length ar_u in
              Ndseq.seq 0
                (List.map
                   (function
                    | `Int i -> assert (i >= 0 && i < n); ar_u.(i)
                    | _ -> assert false)
                   lr)
           | _ -> assert false in
         res_val v
    
      | SeqSingle dep, [||], [|p1|], `Seq (d, rs) ->
         assert (d = dep);
         let* r1 =
           match rs with
           | r1::_ -> Myseq.return r1
           | _ -> Myseq.empty in
         let+ v1 = p1, r1 in
         let v = Ndseq.seq dep [v1] in
         res_val v

      | SeqPair dep, [||], [|p1; p2|], `Seq (d, rs) ->
         assert (d = dep);
         let* r1, r2 =
           match rs with
           | r1::r2::_ -> Myseq.return (r1, r2)
           | r1::_ -> Myseq.return (r1, r1)
           | _ -> Myseq.empty in
         let++ v1, v2 = (p1, r1), (p2, r2) in
         let v = Ndseq.seq dep [v1; v2] in
         res_val v
    
      | SeqCons dep, [||], [|hd; tl|], `Seq (d, rs) ->
         assert (d = dep);
         let* r_hd, r_tl =
           match rs with
           | r_hd::rs_tl -> Myseq.return (r_hd, `Seq (d, rs_tl))
           | _ -> Myseq.empty in
         let++ vhd, vtl = (hd, r_hd), (tl, r_tl) in
         let v =
           match vtl with
           | `Seq (_, l) -> `Seq (dep, vhd::l)
           | _ -> assert false in
         res_val v

      | SeqRepeat dep, [||], [|e|], `Seq (d, rs) ->
         assert (d = dep);
         let* r_e =
           match rs with
           | r::_ -> Myseq.return r
           | _ -> Myseq.empty in
         let+ ve = e, r_e in
         let v = `Seq (dep, List.map (fun _ -> ve) rs) in
         res_val v

      | SeqRange, [||], [|start; step|], `Seq (d, rs) ->
         assert (d = 0);
         let* r_start, r_step =
           match rs with
           | `IntRange (Range.Closed (a1,b1))::`IntRange (Range.Closed (a2,b2))::_ ->
              Myseq.return (`IntRange (Range.Closed (a1,b1)),
                            `IntRange (Range.Closed (a2-b1, b2-a1)))
           | _ -> Myseq.empty in
         let++ vstart, vstep = (start, r_start), (step, r_step) in
         let v =
           match vstart, vstep with
           | `Int start, `Int step ->
              let n = List.length rs in
              `Seq (0, List.init n (fun i -> `Int (start + i * step)))
           | _ -> assert false in
         res_val v

      | SeqIndex, [|vseq|], [|index|], _ ->
         let depth_seq = Ndseq.depth vseq in
         let r_index =
           Ndseq.seq 0 (List.init (depth_seq - depth) (fun _ -> `IntRange (Range.Closed (0,2)))) in (* default index *)
         let+ vindex = index, r_index in
         let i_index =
           match Ndseq.as_seq vindex with
           | Some (0, l) ->
              List.map
                (function
                 | `Int i -> Some i
                 | _ -> assert false)
                l
           | _ -> assert false in
         let* v : value =
           match Ndseq.index_list vseq i_index with
           | Some v -> Myseq.return v
           | None -> Myseq.empty (* index undefined *) in
         res_val v

      | NdseqMap c, _, _, _ ->
         let v_ndim, args_ndim = constr_v_args_ndims c in
         assert (v_ndim <= ndim);
         assert (Array.length args_ndim = Array.length args_index);
         let dep = ndim - v_ndim in
         let* gps : generator_pat_ndseq =
           Ndseq.mapi_tup_myseq ~depth:dep 0
             (fun is ri ->
               match array_map_option
                       (fun vsrc -> Ndseq.index_list_broadcast vsrc is 0)
                       src with
               | Some src ->
                  let* gp = generator_pat {t with ndim = v_ndim} c src k ri in
                  Myseq.return (gp : generator_pat :> generator_pat_ndseq)
               | None -> Myseq.empty)
             (tup1 r) in
         let rec aux (gps : generator_pat_ndseq) : generator_pat Myseq.t =
           match Ndseq.choose gps with
           | Some (`NextArg ((i0,_), _)) ->
              let dn = args_ndim.(i0) in
              let rs, fs =
                Ndseq.map_tup ~depth:dep (dn,0)
                  (function
                   | `NextArg ((i,ri), fi) when i=i0 -> ri, `Fun fi
                   | _ -> assert false)
                  (tup1 gps) in
              let+ vs = i0, rs in
              let* gps1 =
                Ndseq.map_tup_myseq ~depth:dep 0
                  (function
                   | `Fun f, v ->
                      let* gp1 = f v in
                      Myseq.return (gp1 : generator_pat :> generator_pat_ndseq)
                   | _ -> assert false)
                  (fs, vs) in
              aux gps1
           | Some (`NextArg2 ((i0,_),(j0,_), _)) ->
              let dni, dnj = args_ndim.(i0), args_ndim.(j0) in
              let rsi, rsj, fs =
                Ndseq.map_tup ~depth:dep (dni,dnj,0)
                  (function
                   | `NextArg2 ((i,ri), (j,rj), f) when i=i0 && j=j0 -> ri, rj, `Fun f
                   | _ -> assert false)
                  (tup1 gps) in
              let++ vsi, vsj = (i0,rsi), (j0,rsj) in
              let* gps1 =
                Ndseq.map_tup_myseq ~depth:dep 0
                  (function
                   | `Fun f, vi, vj ->
                      let* gp1 = f (vi, vj) in
                      Myseq.return (gp1 : generator_pat :> generator_pat_ndseq)
                   | _ -> assert false)
                  (fs, vsi, vsj) in
              aux gps1
           | Some (`NextArgs (lir, _)) ->
              let li0 = List.map fst lir in
              let dns = Array.of_list (List.map (fun i0 -> args_ndim.(i0)) li0) in
              let lrs =
                Array.to_list
                  (Ndseq.mapn_n ~depth:dep dns
                     (fun is ->
                       function
                       | [| `NextArgs (lir, f) |] ->
                          let li, lr = List.split lir in
                          if li = li0
                          then Array.of_list lr
                          else assert false
                       | _ -> assert false)
                     [|gps|]) in
              let fs =
                Ndseq.map ~depth:dep 0
                  (function
                   | `NextArgs (lir, f) when List.map fst lir = li0 -> `Fun f
                   | _ -> assert false)
                  gps in
              let+++ lvs = List.combine li0 lrs in
              let* ar_gps1 =
                Ndseq.mapn_n_myseq ~depth:dep [|0|]
                  (fun is ar_v ->
                    let lv = Array.to_list ar_v in
                    let f =
                      match Ndseq.index_list fs (List.map (fun i -> Some i) is) with
                      | Some (`Fun f) -> f
                      | _ -> assert false in
                    let* gp1 = f lv in
                    Myseq.return [| (gp1 : generator_pat :> generator_pat_ndseq) |])
                  (Array.of_list lvs) in
              (match ar_gps1 with
               | [|gps1|] -> aux gps1
               | _ -> assert false)
           | Some (`NextDerived ((i0,_,_), _)) ->
              let dn = args_ndim.(i0) in
              let vs, rs, fs =
                Ndseq.map_tup ~depth:dep (dn,dn,0)
                  (function
                   | `NextDerived ((i,vi,ri), fi) when i=i0 -> vi, ri, `Fun fi
                   | _ -> assert false)
                  (tup1 gps) in
              let= () = i0, vs, rs in
              let* gps1 =
                Ndseq.map_myseq ~depth:dep 0
                  (function
                   | `Fun f ->
                      let* gp1 = f () in
                      Myseq.return (gp1 : generator_pat :> generator_pat_ndseq)
                   | _ -> assert false)
                  fs in
              aux gps1
           | Some (`ResVal _) ->
              let v =
                Ndseq.map ~depth:dep v_ndim
                  (function
                   | `ResVal vi -> vi
                   | _ -> assert false)
                  gps in
              res_val v
           | _ -> assert false
         in
         aux gps

      | _ ->
         pp_endline xp_typ t;
         pp_endline (xp_pat c
                       (Array.map (fun v -> fun ~html print () -> xp_value ~html print v) src) 
                       (Array.init k (fun _ -> fun ~html print _ -> print#string "_"))) ();
         (if r = `Null then print_endline "r = NULL");
         assert false

    (* model-based parsing *)
           
    let parseur_value (v0 : value) (v : value) =
      Ndseq.matches 0 (=) v0 v

    let rec parseur_pat t c src k (v : value) (r : distrib) =
      let pp_params () =
        pp_endline xp_typ t;
        pp_endline (xp_pat c
                      (Array.map (fun v -> fun ~html print () -> xp_value ~html print v) src) 
                      (Array.init k (fun _ -> fun ~html print _ -> print#string "_"))) ();
        pp_endline xp_value v;
        pp_endline xp_distrib r
      in
      let depth = t.ndim in
      let ndim = t.ndim in
      if not (Ndseq.depth v = depth) then (
        pp_params ();
        assert false);
      assert (Ndseq.depth r = depth);
      match c, src, k, v, r with
      | Vec, [||], 2, `Vec (i,j), `VecRange (ri,rj) ->
         Myseq.return
           (v, [| `Int i, `IntRange ri;
                  `Int j, `IntRange rj |])

      | Square, [||], 1, `Vec (i,j), `VecRange (ri,rj) ->
         if i = j
         then Myseq.return
                (v, [| `Int i, `IntRange (Range.inter ri rj) |])
         else Myseq.empty

      | Obj, [||], 2, `Obj (pos,g1), `ObjRange (rpos,rg1) ->
         Myseq.return
           (v, [| pos, rpos;
                  g1, rg1 |])

      | DomMap keys, [||], 1, `Map m, `MapRange (ra,rb) ->
         let pairs = Mymap.bindings m in
         let m_keys = List.map fst pairs in
         if m_keys = keys
         then
           let vals = List.map snd pairs in
           Myseq.return (* TODO: replace 0 by values-dependent expr ? if seqs in vals *)
             (v, [| Ndseq.seq 0 vals,
                    Ndseq.seq 0 (List.map (fun _ -> rb) vals) |])
         else Myseq.empty
    
      | Replace, [||], 2, `Map m, `MapRange (ra,rb) ->
         let m_diff = Mymap.filter (fun a b -> a <> b) m in
         (match Mymap.bindings m_diff with
          | [a, b] ->
             Myseq.return
               (v, [| a, ra;
                      b, rb |])
          | _ -> Myseq.empty)
    
      | Swap, [||], 2, `Map m, `MapRange (ra,rb) ->
         let m_diff = Mymap.filter (fun a b -> a <> b) m in
         (match Mymap.bindings m_diff with
          | [a, b; c, d] when a=d && b=c ->
             Myseq.return
               (v, [| a, ra;
                      b, rb |])
          | _ -> Myseq.empty)
    
      | BgColor, [||], 2, `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, None) ->
         if Grid.is_full g
         then
           let tg1 = (`Sprite,nocolor) in
           let* bc = Myseq.from_list (Segment.background_colors g) in
           let* g1 = Myseq.from_result (Grid.Transf.swap_colors g bc Grid.transparent) in
           let lc1 = list_remove bc lc in
           Myseq.return
             (v, [| `Color bc, `ColorRange (C_BG (filling = `Full), lc);
                    `Grid g1, `GridRange (tg1, rh, rw, lc1, None) |])
         else Myseq.empty

      | IsFull, [||], 1, `Grid g, `GridRange ((_filling,nocolor),rh,rw,lc,conn_opt) ->
         if Grid.is_full g
         then Myseq.return
                (v, [| `Grid g, `GridRange ((`Full,nocolor),rh,rw,lc,None) |])
         else Myseq.empty
    
      | Crop, [|vg|], 2, `Grid g1, `GridRange (tg1,rh1,rw1,lc1,conn1_opt) ->
          (match vg with
           | `Grid g ->
              let h1, w1 = Grid.dims g1 in
              let h, w = Grid.dims g in
              let* i, j = Myseq.from_list (Grid_patterns.parse_crop g g1) in
              Myseq.return
                (v, [| `Vec (i,j), `VecRange (Range.make_closed 0 (h-h1),
                                              Range.make_closed 0 (w-w1));
                       `Vec (h1, w1), `VecRange (rh1, rw1) |])
           | _ -> Myseq.empty)

      | Objects (nmax,mode), [||], 7, _, _ ->
         let filling, nocolor =
           match t.kind with
           | GRID (filling,nocolor) -> filling, nocolor
           | _ -> assert false in
         let lseg =
           match mode with
           | `Connected -> GPat.Objects.candidate_segmentations_connected nocolor
           | `SameColor -> [GPat.Objects.SameColor] in             
         let* seg = Myseq.from_list lseg in (* common choice for all sequence items *)
         let lorder = GPat.Objects.candidate_orders nmax nocolor in
         let* order = Myseq.from_list lorder in  (* common choice for all sequence items *)
         let* size, r_size, seg, r_seg, order, r_order,
            card, r_card, objs, r_objs, merger, r_merger, noise, r_noise =
           Ndseq.map_tup_myseq ~depth (0,0, 0,0, 0,0,
                                       0,0, 1,1, 0,0, 0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
                 let h, w = Grid.dims g in
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
                 let _v, merger, r_merger =
                   make_objects_v_merger_itemwise h w card objs g_noise in
                 Myseq.return
                   (`Vec (h,w), `VecRange (rh,rw), (* size *)
                    `Seg seg, `SegRange lseg, (* seg *)
                    `Order order, `OrderRange lorder, (* order *)
                    `Int card, `IntRange (Range.make_closed 1 nmax), (* card *)
                    
                    Ndseq.seq 0 (* objs *)
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
                                                  lc1,
                                                  GPat.Objects.seg_conn_opt seg)))
                         objs),

                    merger, r_merger, (* merger *)
                    `Grid g_noise, `GridRange (tg_noise, Range.make_exact h, Range.make_exact w, lc, None)) (* noise *)
              | _ -> assert false)
             (v,r) in
         Myseq.return (v, [|size, r_size;
                            seg, r_seg;
                            order, r_order;
                            card, r_card;
                            objs, r_objs;
                            merger, r_merger;
                            noise, r_noise|])

      | Object mode, [||], 4, _, _ ->
         let filling, nocolor =
           match t.kind with
           | GRID (filling,nocolor) -> filling, nocolor
           | _ -> assert false in
         let lseg =
           match mode with
           | `Connected -> GPat.Objects.candidate_segmentations_connected nocolor
           | `SameColor -> [GPat.Objects.SameColor] in             
         let* seg = Myseq.from_list lseg in (* common choice for all sequence items *)
         let* size, r_size, seg, r_seg, obj, r_obj, noise, r_noise =
           Ndseq.map_tup_myseq ~depth (0,0, 0,0, 0,0, 0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
                 let h, w = Grid.dims g in
                 let tg1 = (`Sprite,nocolor) in
                 let lc1 = lc in
                 (* PB: not robust segmentation choice, and makes monocolor non-compresive
                   match seg with
                   | GPat.Objects.Connected (_,true) | GPat.Objects.SameColor -> 1
                   | _ -> nc in *)
                 let tg_noise = (`Noise,nocolor) in
                 let* objs, g_noise = GPat.Objects.parse 1 seg g in
                 let i, j, g1 =
                   match objs with
                   | [obj] -> obj
                   | _ -> assert false in
                 let h1, w1 = Grid.dims g1 in
                 Myseq.return
                   (`Vec (h,w), `VecRange (rh,rw), (* size *)
                    `Seg seg, `SegRange lseg, (* seg *)
                    
                    `Obj (`Vec (i,j), `Grid g1), (* obj *)
                    `ObjRange (`VecRange (Range.make_closed 0 (h-h1), (* (h-1)), *)
                                          Range.make_closed 0 (w-w1)), (* (w-1))), *)
                               `GridRange (tg1,
                                           Range.make_closed 1 h, (* (h-i), *)
                                           Range.make_closed 1 w, (* (w-j), *)
                                           lc1,
                                           GPat.Objects.seg_conn_opt seg)),
                    
                    `Grid g_noise, `GridRange (tg_noise, Range.make_exact h, Range.make_exact w, lc, None)) (* noise *)
              | _ -> assert false)
             (v,r) in
         Myseq.return (v, [|size, r_size;
                            seg, r_seg;
                            obj, r_obj;
                            noise, r_noise|])

      | ColorPartition, [||], 4, `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
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
           (v, [| `Vec (h,w), `VecRange (rh, rw);
                  `Int ncol, `IntRange (Range.make_closed 1 nc);

                  Ndseq.seq 0 (List.map (fun (c,m) -> `Color c) layers),
                  Ndseq.seq 0 (List.map (fun (c,m) -> `ColorRange (C_OBJ,lc)) layers);

                  Ndseq.seq 0 (List.map (fun (c,m) -> `Grid m) layers),
                  Ndseq.seq 0 (List.map (fun (c,m) -> `GridRange (tm, Range.make_exact h, Range.make_exact w, [c], None)) layers) |])
        
      | Monocolor, [||], 2, `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
         if Grid.color_count Grid.transparent g = 1
         then
           let* c = Myseq.from_result (Grid.majority_color Grid.transparent g) in
           let* mask = Myseq.from_result (Grid.Transf.swap_colors g c Grid.one) in
           Myseq.return
             (v, [| `Color c, `ColorRange (C_OBJ, lc);
                    `Grid mask, `GridRange ((filling,true), rh, rw, [c], conn_opt) |])
         else Myseq.empty

      | Recoloring, [|vg1|], 1, `Grid g, `GridRange (tg, rh, rw, lc, conn_opt) ->
         (match vg1 with
          | `Grid g1 ->
             (match Grid_patterns.parse_recoloring g g1 with
              | Some mcol ->
                 let m =
                   Mymap.fold
                     (fun c1 c2 res ->
                       Mymap.add (`Color c1) (`Color c2) res)
                     mcol (Mymap.empty : (value,value) Mymap.t) in
                 let rca = `ColorRange (C_OBJ, Grid.all_colors) in (* TODO: or colors in g1 ? *)
                 let rcb = `ColorRange (C_OBJ, lc) in
                 Myseq.return
                   (v, [| `Map m, `MapRange (rca, rcb) |])
              | None -> Myseq.empty)
          | _ -> Myseq.empty)

      | MotifMulti partial, [||], 5, _, _ ->
         let g_bgcolor = if partial then Grid.transparent else Grid.undefined in
         let lmot = GPat.Motif.candidates_multi in
         let* mot = Myseq.from_list lmot in (* common choice for all items *)
         let* mot, r_mot, core, r_core, pure, r_pure, mask_opt, r_mask_opt, noise, r_noise =
           Ndseq.map_tup_myseq ~depth (0,0, 0,0, 0,0, 0,0, 0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
                 let* _mot, ru, rv, g_core, mask_opt, g_noise =
                   Myseq.from_list (GPat.Motif.from_grid [mot] g_bgcolor g) in
                 assert (Grid.has_valid_size g_core); (* to make sure oversized grids are pruned out *)
                 if partial && mask_opt = None
                 then Myseq.empty
                 else              
                   let* pure, r_pure = make_motif_multi_pure_itemwise mot g_core g_noise in
                   let mask_opt, r_mask_opt =
                     match partial, mask_opt with
                     | true, Some mask ->
                        let h, w = Grid.dims mask in (* same as grid and noise *)
                        let rh, rw = Range.make_exact h, Range.make_exact w in (* already encoded in noise *) 
                        `Grid mask, `GridRange ((`Sprite,true), rh, rw, [Grid.one], conn_opt)
                     | _ -> `Null, `Null in (* TODO: revise handling of optional, ugly *)
                   Myseq.return
                     (`Motif mot, `MotifRange lmot, (* mot *)
                      `Grid g_core, `GridRange ((filling,nocolor), ru, rv, lc, None), (* core *)
                      pure, r_pure, (* pure *)
                      mask_opt, r_mask_opt, (* mask_opt *)
                      `Grid g_noise, `GridRange ((`Noise,nocolor), rh, rw, lc, None)) (* noise *)
              | _ -> assert false)
             (v,r) in
         Myseq.return (v, [|mot, r_mot; core, r_core; pure, r_pure;
                            mask_opt, r_mask_opt; noise, r_noise|])
    
      | MotifBi partial, [||], 6, _, _ ->
         let g_bgcolor = if partial then Grid.transparent else Grid.undefined in
         let lmot = GPat.Motif.candidates_bi in
         let* mot = Myseq.from_list lmot in (* common choice for all items *)
         let* mot, r_mot, bgcolor, r_bgcolor, color, r_color, pure, r_pure,
            mask_opt, r_mask_opt, noise, r_noise =
           Ndseq.map_tup_myseq ~depth (0,0, 0,0, 0,0, 0,0, 0,0, 0,0)
             (function
              | `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
                 let* _mot, _ru, _rv, g_core, mask_opt, g_noise =
                   Myseq.from_list (GPat.Motif.from_grid [mot] g_bgcolor g) in
                 assert (Grid.dims g_core = (2,1));
                 if partial && mask_opt = None
                 then Myseq.empty
                 else              
                   let bgcolor = Grid.get_pixel ~source:"parse MotifBi bgcolor" g_core 0 0 in
                   let color = Grid.get_pixel ~source:"parse MotifBi color" g_core 1 0 in
                   let* () = Myseq.from_bool (color <> Grid.transparent) in
                 
                   let* pure, r_pure = make_motif_bi_pure_itemwise mot bgcolor color g_noise in
                   let mask_opt, r_mask_opt =
                     match partial, mask_opt with
                     | true, Some mask -> `Grid mask, `GridRange ((`Sprite,true), rh, rw, [Grid.one], conn_opt)
                     | _ -> `Null, `Null in (* TODO: revise handling of optional, ugly *)
                   Myseq.return
                     (`Motif mot, `MotifRange lmot, (* mot *)
                      `Color bgcolor, `ColorRange (C_BG (filling = `Full), lc), (* bgcolor *)
                      `Color color, `ColorRange (C_OBJ, list_remove bgcolor lc), (* color *)
                      pure, r_pure, (* pure *)
                      mask_opt, r_mask_opt, (* mask_opt *)
                      `Grid g_noise, `GridRange ((`Noise,nocolor), rh, rw, lc, None)) (* noise *)
              | _ -> assert false)
             (v,r) in         
         Myseq.return (v, [|mot, r_mot; bgcolor, r_bgcolor; color, r_color;
                            pure, r_pure; mask_opt, r_mask_opt; noise, r_noise|])
    
      | Metagrid, [||], 6, `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
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
           (v, [| `Color mg.sepcolor, `ColorRange (C_BG (filling = `Full), lc);
                  `Grid mg.borders, `GridRange ((`Noise,true),
                                                Range.make_exact 2,
                                                Range.make_exact 2,
                                                [Grid.one],
                                                None);
                  dims, r_dims;
                  heights, r_heights;
                  widths, r_widths;
                  
                  make_input_gridss mg.parts (fun g1 -> `Grid g1),
                  make_input_gridss mg.parts (fun g1 ->
                      (* dims are known from heigths and widths *)
                      let h1, w1 = Grid.dims g1 in
                      let rh1 = Range.make_exact h1 in
                      let rw1 = Range.make_exact w1 in
                      `GridRange ((filling,nocolor), rh1, rw1, lc, None)) |])

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
    
      | (Empty | Full as c), [||], 1, `Grid mask, `GridRange (tmask, rh, rw, lc, conn_opt) -> (* nc = 1 *)
         let pred =
           match c with
           | Empty -> (fun i j c -> c = Grid.zero)
           | Full -> (fun i j c -> c = Grid.one)
           | _ -> assert false
         in
         let h, w = Grid.dims mask in
         if Grid.for_all_pixels pred mask
         then Myseq.return
                (v, [| `Vec (h,w), `VecRange (rh,rw) |])
         else Myseq.empty

      | Point, [||], 0, `Grid mask, _ ->
         let h, w = Grid.dims mask in
         if h=1 && w=1 && Grid.Mask.mem 0 0 mask
         then Myseq.return (v, [||])
         else Myseq.empty
    
      | Line, [||], 2, `Grid mask, `GridRange (tmask, rh, rw, lc, conn_opt) -> (* 1 color *)
         (match GPat.parse_line mask with
          | Some (len, (di,dj)) ->
             Myseq.return
               (v, [| `Int len, `IntRange (Range.union rh rw);
                      `Vec (di,dj), `VecRange (Range.make_closed 0 1, Range.make_closed (-1) 1) |])
          | None -> Myseq.empty)

      | Skyline, [||], 4, `Grid g, `GridRange ((filling,nocolor), rh, rw, lc, conn_opt) ->
         let h, w = Grid.dims g in
         (match GPat.parse_skyline g with
          | Some ((i,j),lpos) ->
             let max_pos = if i = 0 then w else h in
             Myseq.return
               (v, [| `Vec (h,w), `VecRange (rh, rw);
                      `Vec (i,j), `VecRange (Range.Closed (-1,1), Range.Closed (-1, 1));
                        
                      Ndseq.seq 0 (List.map (fun p -> `Int p) lpos),
                      Ndseq.seq 0 (List.map (fun p -> `IntRange (Range.Closed (0,max_pos))) lpos);

                      Ndseq.seq 0 (List.map (fun p -> `Int (max_pos - p)) lpos),
                      Ndseq.seq 0 (List.map (fun p -> `IntRange (Range.make_exact (max_pos - p))) lpos) |])
          | None -> Myseq.empty)

      | ColorSeq dir, [||], 2, `Grid g, `GridRange (tg, rh, rw, lc, conn_opt) ->
         let h, w = Grid.dims g in
         let rc = `ColorRange (C_OBJ, lc) in
         (match dir with
          | `H ->
             let* () = Myseq.from_bool (h = 1) in
             Myseq.return
               (v, [| `Int w, `IntRange rw;

                      Ndseq.seq 0
                        (List.init w (fun j ->
                             `Color (Grid.get_pixel g 0 j))),
                      Ndseq.seq 0 (List.init w (fun j -> rc)) |])
          | `V ->
             let* () = Myseq.from_bool (w = 1) in
             Myseq.return
               (v, [| `Int h, `IntRange rh;

                      Ndseq.seq 0
                        (List.init h (fun i ->
                             `Color (Grid.get_pixel g i 0))),
                      Ndseq.seq 0 (List.init h (fun i -> rc)) |]))
    
      | ColorMat, [||], 2, `Grid g, `GridRange (tg, rh, rw, lc, conn_opt) ->
         let h, w = Grid.dims g in
         let rc = `ColorRange (C_OBJ, lc) in
         Myseq.return
           (v, [| `Vec (h,w), `VecRange (rh,rw);
                    
                  Ndseq.seq 1
                    (List.init h (fun i ->
                         Ndseq.seq 0
                           (List.init w (fun j ->
                                `Color (Grid.get_pixel g i j))))),
                  Ndseq.seq 1
                    (List.init h (fun i ->
                         Ndseq.seq 0
                           (List.init w (fun j ->
                                rc)))) |])

      | MakeGrid, [||], 1, _, _ ->
         let* g = Myseq.from_result (make_grid_from_color_seq_seq v) in
         (* let filling =
            match tc with
            | C_OBJ | C_BG true -> `Full
            | C_BG false -> `Sprite in *)
         let h, w = Grid.dims g in
         let rh = Range.make_exact h in (* grid dims known from above, patterns introducing color seq seq *)
         let rw = Range.make_exact w in
         let lc = Grid.all_colors in
         Myseq.return
           (v, [| `Grid g, `GridRange ((`Sprite,false), rh, rw, lc, None) |])

      | Map, [|vseq|], 1, `Seq (0, ys), `Seq (0, yrs) ->
         (match vseq with
          | `Seq (0, xs) when List.length xs = List.length ys ->
             let vals, r_vals =
               List.split (list_unique_assoc (List.combine xs (List.combine ys yrs))) in
             let unique_pairs = list_unique_vals (List.combine xs ys) in
             if List.length vals = List.length unique_pairs
             then Myseq.return
                    (v, [|Ndseq.seq 0 vals, Ndseq.seq 0 r_vals|])
             else Myseq.empty (* not a map *)
          | _ -> Myseq.empty)

      | Unique, [||], 3, `Seq (0, lv), `Seq (0, lr) ->
         let len = List.length lv in
         let vals, ranks = list_unique_ranks lv in
         let n = List.length vals in
         Myseq.return
           (v, [| `Int n, `IntRange (Range.Closed (0,len));
                  
                  Ndseq.seq 0 vals,
                  Ndseq.seq 0 (list_unique_assoc (List.combine lv lr));
                  
                  Ndseq.seq 0 (List.map (fun i -> `Int i) ranks),
                  Ndseq.seq 0 (List.init len (fun pos -> `IntRange (Range.Closed (0, min pos (n-1))))) |])
    
      | SeqSingle dep, [||], 1, `Seq (_, vs), `Seq (_, rs) ->
         (match vs, rs with
          | [v1], [r1] ->
             Myseq.return (v, [|v1,r1|])
          | _ -> Myseq.empty)

      | SeqPair dep, [||], 2, `Seq (_, vs), `Seq (_, rs) ->
         (match vs, rs with
          | [v1;v2], [r1;r2] ->
             Myseq.return (v, [|v1, r1; v2,r2|])
          | _ -> Myseq.empty)

      | SeqCons dep, [||], 2, `Seq (_, vs), `Seq (_, rs) ->
         (match vs, rs with
          | hd::tl, r_hd::r_tl ->
             Myseq.return (v, [| hd, r_hd;
                                 `Seq (dep, tl), `Seq (dep, r_tl)|])
          | _ -> Myseq.empty)

      | SeqRepeat dep, [||], 1, `Seq (_, vs), `Seq (_, rs) ->
         (match vs, rs with
          | [], _ -> Myseq.empty
          | v0::l1, r0::_ ->
             (try
                if List.for_all (fun v1 -> v1 = v0) l1 (* all elts should be the same value *)
                then Myseq.return (v, [|v0,r0|])
                else Myseq.empty
              with _ -> Myseq.empty)
          | _ -> assert false)

      | SeqRange, [||], 2, `Seq (_, l), `Seq (_, r_l) ->
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
             Myseq.return (v, [|`Int x0, `IntRange r0;
                               `Int step, `IntRange range_step|])
          | _ -> Myseq.empty)

      | SeqIndex, [|vseq|], 1, _, _ ->
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
         Myseq.return (v, [|index, r_index|])

      | NdseqMap c, _, _, _, _ ->
         let as_ndseq x = (Obj.magic x : _ Ndseq.t) [@@inline] in
         let as_value x = (Obj.magic x : value) [@@inline] in
         let as_distrib x = (Obj.magic x : distrib) [@@inline] in
         let v_ndim, args_ndim = constr_v_args_ndims c in
         assert (v_ndim <= ndim);
         assert (Array.length args_ndim = k);
         let dep = ndim - v_ndim in
         let dns = Array.init (1 + 2*k)
                     (fun i ->
                       if i = 0 then v_ndim (* v *)
                       else args_ndim.((i - 1) / 2)) in (* v_r_args *)
         let* repr_v_v_r_args =
           Ndseq.mapn_n_myseq ~depth:dep dns
             (fun is -> function
               | [|repr_v; repr_r|] ->
                  (match array_map_option
                           (fun vsrc -> Ndseq.index_list_broadcast vsrc is 0)
                           src with
                   | Some src ->
                      let v, r = as_value repr_v, as_distrib repr_r in
                      let* v, vr_args = parseur_pat {t with ndim = v_ndim} c src k v r in
                      let repr_v_v_r_args = Array.make (1 + 2*k) (as_ndseq v) in
                      Array.iteri
                        (fun i (v,r) ->
                          repr_v_v_r_args.(1 + 2*i) <- as_ndseq v;
                          repr_v_v_r_args.(1 + 2*i + 1) <- as_ndseq r)
                        vr_args;
                      Myseq.return repr_v_v_r_args
                   | None -> Myseq.empty)
               | _ -> assert false)
             [|as_ndseq v; as_ndseq r|] in
         let v = as_value repr_v_v_r_args.(0) in
         let vr_args =
           Array.init k
             (fun i ->
               as_value repr_v_v_r_args.(1 + 2*i),
               as_distrib repr_v_v_r_args.(1 + 2*i + 1)) in
         Myseq.return (v, vr_args)

      | _ ->
         pp_params ();
         assert false
    

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
         
    let dl_grid g (filling,nocolor) rh rw lc conn_opt : dl = (* too efficient a coding for being useful? *)
      (* lc is a list of colors, not including transparent or undefined, nocolor implies |lc|=1 *)
      (* conn_opt is an optional connectedness constraint *)
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
                       -. (match conn_opt with
                           | None -> 0.
                           | Some conn -> float (GPat.Objects.disconnected_area conn g)) (* unreachable cells by conn-based propagation *)
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
      | INT NAT, `Int i ->
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
      | SEG, `Seg seg ->
         dl_seg seg (GPat.Objects.candidate_segmentations_connected false)
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
         dl_grid g tg rmax rmax Grid.all_colors None
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
        | `Grid g, `GridRange (tg, rh, rw, lc, conn_opt) -> dl_grid g tg rh rw lc conn_opt
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
      | Object mode, [|enc_size; enc_seg; enc_obj; enc_noise|] -> enc_size +. enc_seg +. enc_obj +. enc_noise (* TODO: take seg into account for encoding objects *)
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
      | Skyline, [|enc_size; enc_dir; enc_pos; enc_compl|] -> enc_size +. enc_dir +. enc_pos (* compl derived *)
      | ColorSeq dir, [|enc_size; enc_colors|] -> enc_size +. enc_colors
      | ColorMat, [|enc_size; enc_colorss|] -> enc_size +. enc_colorss
      | MakeGrid, [|enc_grid|] -> enc_grid
      | Map, [|enc_vals|] -> enc_vals
      | Unique, [|enc_n; enc_vals; enc_ranks|] -> enc_n +. enc_vals +. enc_ranks
      | SeqSingle dep, [|enc1|] -> enc1
      | SeqPair dep, [|enc1; enc2|] -> enc1 +. enc2
      | SeqCons dep, [|enc_hd; enc_tl|] -> enc_hd +. enc_tl
      | SeqRepeat dep, [|enc_e|] -> enc_e
      | SeqRange, [|enc_start; enc_step|] -> enc_start +. enc_step
      | SeqIndex, [|enc_index|] -> enc_index
      | NdseqMap c, encs -> Array.fold_left (+.) 0. encs
      | _ -> assert false
    let encoding_alt dl_choice enc = dl_choice +. enc
    let encoding_expr_value v = 0.
    let dl_of_encoding enc = enc
           
    let dl_var ~nb_env_vars t p = (* TODO: take t into account, filtering env vars *)
      let k = max 1 nb_env_vars in (* to avoid 0, happens in pruning mode *)
      Mdl.Code.uniform k

    let rec dl_constr_params t c =
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
      | Object mode ->
         (*Mdl.Code.usage
           (match seg with
            | `Connected -> 0.33
            | `ConnectedSameColor -> 0.33
            | `SameColor -> 0.33)
         +. *) 1.
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
      | Skyline -> 0.
      | ColorSeq dir -> 1. (* encoding direction *)
      | ColorMat -> 0.
      | MakeGrid -> 0.
      | Map -> 0.
      | Unique -> 0.
      | SeqSingle dep -> Mdl.Code.universal_int_star dep
      | SeqPair dep -> Mdl.Code.universal_int_star dep
      | SeqCons dep -> Mdl.Code.universal_int_star dep
      | SeqRepeat dep -> Mdl.Code.universal_int_star dep
      | SeqRange -> 0.
      | SeqIndex -> 0.
      | NdseqMap c -> dl_constr_params {t with ndim=0} c 

    let dl_periodicity_mode : Grid.Transf.periodicity_mode -> dl = function
      | `Total -> Mdl.Code.usage 0.25
      | `Strict -> Mdl.Code.usage 0.25
      | `TradeOff -> Mdl.Code.usage 0.5

    let dl_cast_kind k k' =
      (* encoding k' given k *)
      match k with
      | INT NAT ->
         (match k' with
          | INT (COORD (axis,tv)) -> Mdl.Code.usage 0.5 +. Mdl.Code.uniform 2 (* axis *) +. Mdl.Code.uniform 3 (* tv *)
          | _ -> assert false)
      | INT (COORD _) ->
         (match k' with
          | INT NAT -> 0.
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
      | `UniqueVals_1 -> 0.
      | `UniqueRanks_1 -> 0.
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
      | `Count_1 -> 0.
      | `DistinctCount_1 -> 0.
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
        `ScaleUp_2, 2, `Plus_2, 2;
        `ScaleUp_2, 2, `Plus_2, 3;
        `ScaleUp_2, 2, `Minus_2, 1;
        `ScaleUp_2, 2, `Minus_2, 2;
        `ScaleUp_2, 2, `Minus_2, 3;

        `ScaleDown_2, 2, `Plus_2, 0;
        `ScaleDown_2, 2, `Plus_2, 1;
        `ScaleDown_2, 2, `Minus_2, 1;

        `ScaleUp_2, 3, `Plus_2, 0;
        `ScaleUp_2, 3, `Plus_2, 1;
        `ScaleUp_2, 3, `Plus_2, 2;
        `ScaleUp_2, 3, `Plus_2, 3;
        `ScaleUp_2, 3, `Plus_2, 4;
        `ScaleUp_2, 3, `Minus_2, 1;
        `ScaleUp_2, 3, `Minus_2, 2;
        `ScaleUp_2, 3, `Minus_2, 3;
        `ScaleUp_2, 3, `Minus_2, 4;

        `ScaleDown_2, 3, `Plus_2, 0;
        `ScaleDown_2, 3, `Plus_2, 1;
        `ScaleDown_2, 3, `Minus_2, 1;
      ]
    
    let make_index_bind (bindings : bindings) : expr_index = (* NEW VERSION *)
      Common.prof "make_index_bind" (fun () ->
      let max_expr_size = !max_expr_size in
      let bgcolors full =
        Grid.black :: if full then [] else [Grid.transparent] in
      let index = new Expr.index_bind in
      let () = Expr.index_add_bindings index bindings in
      let () = (* LEVEL: Obj features *)
        Common.prof "make_index/obj_features" (fun () ->
        Expr.index_apply_functions_1
          ~max_expr_size ~eval_func
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
          ~max_expr_size ~eval_func
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
          ~max_expr_size ~eval_func
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
          ~max_expr_size ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* I_1, J_1, Norm_1 *)
              match t1.kind with
              | VEC tv ->
                 ({t1 with kind = INT (COORD (I, tv))}, `I_1, `Default)
                 ::({t1 with kind = INT (COORD (J, tv))}, `J_1, `Default)
                 ::({t1 with kind = INT NAT}, `Norm_1, `Default)
                 ::res
              | _ -> res in
            let res = (* Area_1 *)
              match t1.kind with
              | GRID (filling,nocolor) ->
                 ({t1 with kind = INT NAT}, `Area_1, `Default)
                 ::({t1 with kind = INT (COORD (I, SIZE))}, `Area_1, `Default) (* TODO: add cast from NAT to COORD? *)
                 ::({t1 with kind = INT (COORD (J, SIZE))}, `Area_1, `Default)
                 ::res
              | _ -> res in
            let res = (* ColorCount_1 *)
              match t1.kind with
              | GRID (filling,false) ->
                 ({t1 with kind = INT NAT}, `ColorCount_1, `Default)::res
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
      let () = (* LEVEL: Collection features *)
        Common.prof "make_index/coll_features" (fun () ->
        Expr.index_apply_functions_1
          ~max_expr_size ~eval_func
          index
          (fun t1 v1 ->
            let ndim = Ndseq.ndim v1 in
            let res = [] in
            let res = (* UniqueVals *)
              if ndim > 0
              then
                (t1, `UniqueVals_1, `Default)
                ::({t1 with kind = INT NAT}, `UniqueRanks_1, `Default)
                ::res
              else res in
            res)) in
  (* TODO: binary exprs too costly
      let () = (* LEVEL: Int+Vec bin *)
        Common.prof "make_index/int_vec_bin" (fun () ->
        Expr.index_apply_functions_2
          ~max_expr_size ~eval_func
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
          ~max_expr_size ~eval_func
          index
          (fun t1 v1 ->
            let res = [] in
            let res = (* ax + b, for x : INT | VEC *)
              match t1.kind with
              | INT (COORD (_, MOVE)) -> res
              | INT ti ->
                 let ta = scalar (INT NAT) in
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
                 let ta = scalar (VEC SIZE) in (* should be NAT *)
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
          ~max_expr_size ~eval_func
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
          ~max_expr_size ~eval_func
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
          ~max_expr_size ~eval_func
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
          ~max_expr_size ~eval_func
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
          ~max_expr_size ~eval_func
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
              let res = (* Count, DistinctCount *)
                (typ_card, `Count_1, `Default)
                ::(typ_card, `DistinctCount_1, `Default)
                ::res in
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
          ~max_expr_size ~eval_func
          index
          (fun t1 v1 ->
            let kind = t1.kind in
            let res = [] in
            let lk' =
              match kind with
              | INT NAT ->
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

    let lift_constr ndim c = if ndim=0 then c else NdseqMap c
    
    let refinements_any (t : typ) (value : value) : model list = (* QUICK *)
      let ndim = t.ndim in
      let lift_constr = lift_constr ndim in
      let rs = [] in
      let rs = (* adding SeqRepeat : almost DECOMP *)
        if ndim > 0
        then
          let$ rs, dep = rs, List.init ndim (fun i -> i) in
          if Ndseq.for_all ~depth:(ndim-dep-1)
               (fun v ->
                 match Ndseq.as_seq v with
                 | Some (_,l) -> l <> []
                 | _ -> assert false)
               value
          then
            (Model.make_pat t (lift_constr (SeqRepeat dep))
               [| Model.make_def var0 (Model.make_any {t with ndim = ndim-1}) |])
            :: rs
          else rs
        else rs in
      (* let rs = (* adding SeqSingle : DECOMP *)
        if ndim > 0
        then
          let$ rs, dep = rs, List.init ndim (fun i -> i) in
          if Ndseq.for_all ~depth:(ndim-dep-1)
               (fun v ->
                  match Ndseq.as_seq v with
                  | Some (_,l) -> List.length l = 1
                  | _ -> assert false)
                value
                then
            (Model.make_pat t (lift_constr (SeqSingle dep))
               [| Model.make_def var0 (Model.make_any {t with ndim = ndim-1}) |])
            :: rs
            else rs
        else rs in *)
      let rs = (* adding SeqPair : DECOMP *)
        if ndim > 0
        then
          let$ rs, dep = rs, List.init ndim (fun i -> i) in
          if Ndseq.for_all ~depth:(ndim-dep-1)
               (fun v ->
                  match Ndseq.as_seq v with
                  | Some (_,l) -> List.length l = 2
                  | _ -> assert false)
                value
          then
            (Model.make_pat t (lift_constr (SeqPair dep))
               [| Model.make_def var0 (Model.make_any {t with ndim = ndim-1});
                  Model.make_def var0 (Model.make_any {t with ndim = ndim-1}) |])
            :: rs
          else rs
        else rs in
      let rs = (* adding Unique *)
        if ndim > 0
        then
          (Model.make_pat t (lift_constr Unique)
             [| Model.make_def var0 (Model.make_any {kind = INT NAT; ndim = ndim-1});
                Model.make_def var0 (Model.make_any t);
                Model.make_def var0 (Model.make_any {t with kind = INT NAT}) |])
          :: rs
        else rs in
      let rs = (* adding SeqCons : DECOMP *) (* TODO: find better, for any position, matching some pattern *)
        if ndim > 0 (* > 0 : TODO BUG: this entails missing refinements, unrelated ones *)
        then
          let$ rs, dep = rs, List.init ndim (fun i -> i) in
          if Ndseq.for_all ~depth:(ndim-dep-1)
               (fun v ->
                 match Ndseq.as_seq v with
                 | Some (_,l) -> l <> []
                 | _ -> assert false)
               value
          then
            (Model.make_pat t (lift_constr (SeqCons dep))
               [| Model.make_def var0 (Model.make_any {t with ndim = ndim-1});
                  Model.make_def var0 (Model.make_any t) |])
            :: rs
          else rs
        else rs in
      match t.kind with
      | INT ti ->
         let rs = (* adding SeqRange *)
           if ndim > 0
           then
             (Model.make_pat t (lift_constr SeqRange)
                [| Model.make_def var0 (Model.make_any {t with ndim = ndim-1});
                   Model.make_def var0 (Model.make_any {kind = INT (COORD (I, MOVE)); ndim = ndim-1}) |])
             :: rs
           else rs in
         rs
      | VEC tv ->
         let rs = (* adding Vec : DECOMP *)
           (Model.make_pat t (lift_constr Vec)
              [| Model.make_def var0 (Model.make_any {t with kind = INT (COORD (I, tv))});
                 Model.make_def var0 (Model.make_any {t with kind = INT (COORD (J, tv))}) |])
            :: rs in
         let rs = (* Square *)
           match tv with
           | SIZE | MOVE ->
              (Model.make_pat t (lift_constr Square)
                 [| Model.make_def var0 (Model.make_any {t with kind = INT (COORD (I, tv))}) |])
               :: rs
           | POS -> rs in (* not relevant for positions *)
         rs
      | COLOR tc ->
         let rs = (* adding MakeGrid : DECOMP *)
           if ndim >= 2
           then
             (* let filling =
                match tc with
                | C_OBJ | C_BG true -> `Full
                | C_BG false -> `Sprite in *)
             (Model.make_pat t (lift_constr MakeGrid)
                [| Model.make_def var0 (Model.make_any {kind = GRID (`Sprite,false); ndim = ndim-2})|])
              :: rs
           else rs in
         rs
      | SEG -> rs
      | ORDER _ -> rs
      | MOTIF tmot -> rs
      | MAP (ka,kb) ->
         let refs : model list = rs in
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
              let mvals = Model.make_any {kind = COLOR tc; ndim = ndim+1} in
              let$ refs, keys = refs, l_keys in (* TODO: check for single keys ? *)
              (* TODO: needs Cons
                 List.fold_right (* explicit sequence of same length as keys *)
                  (fun _ mvals ->
                    let mcol = Model.make_def var0 (make_anycolor tc) in
                    let mvals = Model.make_cons var0 mcol mvals in
                    mvals)
                  keys (Model.make_nil tb) in *)
              (Model.make_pat t (lift_constr (DomMap keys))
                 [| Model.make_def var0 mvals |])
              :: refs
           | _ -> refs in
        let refs = (* Replace *)
           if ka = kb then
             (Model.make_pat t (lift_constr Replace)
                [| Model.make_def var0 (Model.make_any {t with kind = COLOR C_OBJ});
                   Model.make_def var0 (Model.make_any {t with kind = COLOR C_OBJ}) |])
             :: refs
           else refs in
         let refs = (* Swap *)
           if ka = kb then
             (Model.make_pat t (lift_constr Swap)
                [| Model.make_def var0 (Model.make_any {t with kind = COLOR C_OBJ});
                   Model.make_def var0 (Model.make_any {t with kind = COLOR C_OBJ}) |])
             :: refs
           else refs in
         refs
      | GRID (filling,nocolor) ->
         let refs : model list = rs in
         let refs = (* BgColor *)
           if filling = `Full && not nocolor then
             (Model.make_pat t (lift_constr BgColor)
                [| Model.make_def var0 (Model.make_any {t with kind = COLOR (C_BG true)});
                   Model.make_def var0 (Model.make_any {t with kind = GRID (`Sprite,nocolor)}) |])
             :: refs
           else refs in
         let refs = (* IsFull : almost DECOMP *)
           if filling = `Sprite && not nocolor then (* nocolor isfull covered by full mask *)
             (Model.make_pat t (lift_constr IsFull)
                [| Model.make_def var0 (Model.make_any {t with kind = GRID (`Full,nocolor)}) |])
             :: refs
           else refs in
         let refs = (* Objects - Connected *)
           if filling <> `Full then
             let nmax = 9 in
             (Model.make_pat t (Objects (nmax, `Connected))
                [| Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def var0 (Model.make_any {t with kind = SEG});
                   Model.make_def var0 (Model.make_any {t with kind = ORDER nocolor});
                   Model.make_def var0 (Model.make_any {t with kind = INT NAT});
                   Model.make_def var0
                     (Model.make_pat {kind = OBJ (`Sprite,nocolor); ndim = ndim+1} (NdseqMap Obj)
                        [| Model.make_def var0 (Model.make_any {kind = VEC POS; ndim = ndim+1});
                           Model.make_def var0 (Model.make_any {kind = GRID (`Sprite,nocolor); ndim = ndim+1}) |]);
                   Model.make_def var0 (Model.make_derived {t with kind = OBJ (`Sprite,nocolor)});
                   Model.make_def var0 (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |])
             :: refs
           else refs in
         let refs = (* Objects - SameColor *)
           if filling <> `Full && not nocolor then
             let nmax = 9 in
             (Model.make_pat t (Objects (nmax, `SameColor))
                [| Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                   Model.make_expr_const {t with kind = SEG} (`Seg GPat.Objects.SameColor);
                   Model.make_def var0 (Model.make_any {t with kind = ORDER nocolor});
                   Model.make_def var0 (Model.make_any {t with kind = INT NAT});
                   Model.make_def var0
                     (Model.make_pat {kind = OBJ (`Sprite,nocolor); ndim = ndim+1} (NdseqMap Obj)
                        [| Model.make_def var0 (Model.make_any {kind = VEC POS; ndim = ndim+1});
                           Model.make_def var0
                             (Model.make_pat {kind = GRID (`Sprite,nocolor); ndim = ndim+1} (NdseqMap Monocolor)
                                [| Model.make_def var0 (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+1});
                                   Model.make_def var0 (Model.make_any {kind = GRID (filling,true); ndim = ndim+1}) |]) |]);
                   Model.make_def var0 (Model.make_derived {t with kind = OBJ (`Sprite,nocolor)});
                   Model.make_def var0 (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |])
             :: refs
           else refs in
         let refs = (* Object - Connected *)
           if filling <> `Full then
             (Model.make_pat t (Object `Connected)
                [| Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def var0 (Model.make_any {t with kind = SEG});
                   Model.make_def var0
                     (Model.make_pat {t with kind = OBJ (`Sprite,nocolor)} (NdseqMap Obj)
                        [| Model.make_def var0 (Model.make_any {t with kind = VEC POS});
                           Model.make_def var0 (Model.make_any {t with kind = GRID (`Sprite,nocolor)}) |]);
                   Model.make_def var0 (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |])
             :: refs
           else refs in
         let refs = (* Object - SameColor - colored grid *)
           if filling <> `Full && not nocolor then
             (Model.make_pat t (Object `SameColor)
                [| Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                   Model.make_expr_const {t with kind = SEG} (`Seg GPat.Objects.SameColor);
                   Model.make_def var0
                     (Model.make_pat {t with kind = OBJ (`Sprite,nocolor)} (NdseqMap Obj)
                        [| Model.make_def var0 (Model.make_any {t with kind = VEC POS});
                           Model.make_def var0
                             (Model.make_pat {t with kind = GRID (`Sprite,nocolor)} (NdseqMap Monocolor)
                                [| Model.make_def var0 (Model.make_any {t with kind = COLOR C_OBJ});
                                   Model.make_def var0 (Model.make_any {t with kind = GRID (filling,true)}) |]) |]);
                   Model.make_def var0 (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |])
             :: refs
           else refs in
         let refs = (* Object - SameColor - mask *)
           if filling <> `Full && nocolor then
             (Model.make_pat t (Object `SameColor)
                [| Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                   Model.make_expr_const {t with kind = SEG} (`Seg GPat.Objects.SameColor);
                   Model.make_def var0
                     (Model.make_pat {t with kind = OBJ (`Sprite,nocolor)} (NdseqMap Obj)
                        [| Model.make_def var0 (Model.make_any {t with kind = VEC POS});
                           Model.make_def var0 (Model.make_any {t with kind = GRID (filling,nocolor)}) |]);
                   Model.make_def var0 (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |])
             :: refs
           else refs in
         (* let refs = (* ColorPartition *)
           if filling <> `Full && not nocolor then
             (Model.make_pat t (lift_constr ColorPartition)
                [| Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def var0 (Model.make_any {t with kind = INT NAT});
                   Model.make_def var0 (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+1});
                   Model.make_def var0
                     (Model.make_any
                        {kind = GRID (`Noise, true);
                         ndim = ndim+1}) |])
             :: refs
           else refs in *)
         let refs = (* Monocolor *)
           if not nocolor then
             let mmask =
               if filling = `Full
               then (* a monocolor full grid must have a full mask of some size *)
                 let msize =
                   Model.make_def var0 (Model.make_any {t with kind = VEC SIZE}) in
                 Model.make_pat {t with kind = GRID (`Full,true)} (lift_constr Full) [|msize|]
               else
                 Model.make_any {t with kind = GRID (filling,true)} in
             (Model.make_pat t (lift_constr Monocolor)
                [| Model.make_def var0 (Model.make_any {t with kind = COLOR C_OBJ});
                   Model.make_def var0 mmask |])
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
                (Model.make_pat t (lift_constr Recoloring) ~src:[|eg1|]
                   [| Model.make_def var0 (Model.make_any {t with kind = MAP (COLOR C_OBJ, COLOR C_OBJ)}) |])
                :: refs
             | _ -> refs
           else refs in
         let refs = (* MotifMulti *)
           let t_mask = {t with kind = GRID (`Sprite,true)} in
           let$ refs, partial = refs, (match filling with
                                       | `Full -> [false]
                                       | _ -> [false; true]) in
           (Model.make_pat t (MotifMulti partial)
              [| Model.make_def var0 (Model.make_any {t with kind = MOTIF MULTI});
                 Model.make_def var0 (Model.make_any {t with kind = GRID ((if filling = `Noise then `Sprite else filling), nocolor)});
                 Model.make_def var0 (Model.make_derived t);
                 (if partial
                  then Model.make_def var0 (Model.make_any t_mask)
                  else Model.make_expr_const t_mask `Null);
                 Model.make_def var0 (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |])
           :: refs in
         let refs = (* MotifBi *)
           let t_mask = {t with kind = GRID (`Sprite,true)} in
           let$ refs, partial = refs, (match filling with
                                       | `Full -> [false]
                                       | _ -> [false; true]) in
           (Model.make_pat t (MotifBi partial)
              [| Model.make_def var0 (Model.make_any {t with kind = MOTIF BI});
                 Model.make_def var0 (Model.make_any {t with kind = COLOR (C_BG (filling = `Full))});
                 Model.make_def var0 (Model.make_any {t with kind = COLOR C_OBJ});
                 Model.make_def var0 (Model.make_derived t);
                 (if partial
                  then Model.make_def var0 (Model.make_any t_mask)
                  else Model.make_expr_const t_mask `Null);
                 Model.make_def var0 (Model.make_any {t with kind = GRID (`Noise,nocolor)}) |])
           :: refs in
         let refs = (* Metagrid *)
           (Model.make_pat t (lift_constr Metagrid)
              [| Model.make_def var0 (Model.make_any {t with kind = COLOR (C_BG (filling = `Full))});
                 Model.make_def var0 (Model.make_any {t with kind = GRID (`Noise,true)});
                 Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                 Model.make_def var0 (Model.make_any {kind = INT (COORD (I, SIZE)); ndim = ndim+1});
                 Model.make_def var0 (Model.make_any {kind = INT (COORD (J, SIZE)); ndim = ndim+1});
                 Model.make_def var0 (Model.make_any {t with ndim = ndim+2})|])
           :: refs in
         (* let refs = (* Repeat - too catchy, replaced by function *)
           (make_repeat tg
              (Model.make_def var0 (make_anygrid (filling,nocolor)))
              (Model.make_loop var0 (Range.make_open 1)
                 (Model.make_def var0 (make_anycoord I SIZE)))
              (Model.make_loop var0 (Range.make_open 1)
                 (Model.make_def var0 (make_anycoord J SIZE))))
           ::refs in *)
         let refs = (* Masks *)
           let msize =
             Model.make_def var0 (Model.make_any {t with kind = VEC SIZE}) in
           (* TODO: consider casting functions rather than normalizing model type *)
           (Model.make_pat {t with kind = GRID (`Sprite,false)} (lift_constr Empty) [|msize|])
           :: (if nocolor then
                 (Model.make_pat {t with kind = GRID (`Sprite,true)} (lift_constr Full) [|msize|])
                 :: (Model.make_pat {t with kind = GRID (`Sprite,true)} (lift_constr Point) [||])
                 :: refs
               else refs) in
         let refs = (* Line *)
           if filling <> `Full && nocolor then
             (Model.make_pat {t with kind = GRID (`Sprite,true)} (lift_constr Line)
                [| Model.make_def var0 (Model.make_any {t with kind = INT (COORD (I, SIZE))});
                   Model.make_def var0 (Model.make_any {t with kind = VEC MOVE}) |])
             ::refs
           else refs in
         let refs = (* Skyline *)
           if filling <> `Full && nocolor then
             (Model.make_pat {t with kind = GRID (`Sprite,true)} (lift_constr Skyline)
                [| Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def var0 (Model.make_any {t with kind = VEC MOVE});
                   Model.make_def var0 (Model.make_any {kind = INT NAT; ndim = ndim+1});
                   Model.make_def var0 (Model.make_derived {kind = INT NAT; ndim = ndim+1}) |])
             ::refs
           else refs in
         let refs = (* ColorSeq : DECOMP *)
           if filling = `Full && not nocolor
              && (match value with
                    | `Grid g ->
                       let h, w = Grid.dims g in
                       (h = 1 && w <= 6) || (h <= 6 && w = 1)
                    | _ -> false)
           then (* TODO: allow when not full, impact on color type *)
             let$ refs, (dir,axis) = refs, [`H, J; `V, I] in
             (Model.make_pat t (lift_constr (ColorSeq dir))
                [| Model.make_def var0 (Model.make_any {t with kind = INT (COORD (axis, SIZE))});
                   Model.make_def var0 (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+1}) |])
             ::refs
           else refs in
         let refs = (* ColorMat : DECOMP *)
           if filling = `Full && not nocolor
              && (match value with
                  | `Grid g ->
                     let h, w = Grid.dims g in
                     h <= 3 && w <= 3
                  | _ -> false)
           then
             (Model.make_pat t (lift_constr ColorMat)
                [| Model.make_def var0 (Model.make_any {t with kind = VEC SIZE});
                   Model.make_def var0 (Model.make_any {kind = COLOR C_OBJ; ndim = ndim+2}) |])
             ::refs
           else refs in
         refs
      | OBJ _ -> rs
      | _ -> assert false    
    let refinements_pat (t : typ) (c : constr) (args : model array) (value : value) : model list = (* QUICK *)
      []
    (* TODO: add SeqCons/SeqRepeat(m,m) but requires global change of depths for head model *) 
    let refinements_pat_expr ~env_vars (t : typ) (value : value) : model list = (* QUICK *)
      let ndim = t.ndim in
      let rs = [] in
      let rs = (* adding SeqIndex *)
        let compatible_vars = (* same type vars from env *)
          Mymap.fold
            (fun x tx res ->
              if tx.kind = t.kind && tx.ndim > t.ndim
              then (x,tx)::res
              else res)
            env_vars [] in
        let$ rs, (x,tx) = rs, compatible_vars in
        (Model.make_pat t SeqIndex ~src:[|Expr.Ref (tx, x)|]
           [| Model.make_def var0 (Model.make_any typ_index) |])
         :: rs in
      (* let rs = (* adding Map => see Unique *)
        if ndim > 0
        then
          let compatible_vars =
            Mymap.fold
              (fun x tx res ->
                if tx.ndim = ndim
                then (x,tx)::res
                else res)
              env_vars [] in
          let$ rs, (x,tx) = rs, compatible_vars in
          (Model.make_pat t (lift_constr Map) ~src:[|Expr.Ref (tx, x)|]
             [| Model.make_def var0 (Model.make_any t) |])
          :: rs
        else rs in *)
      match t.kind with
      | GRID (filling,nocolor as tg) ->
         let refs = rs in
         let refs = (* Crop *)
           let cropable_vars =
             Mymap.fold
               (fun x tx res ->
                 match tx.kind with
                 | GRID tgx when tgx = tg && tx.ndim <= ndim -> (x,tx)::res
                 | _ -> res)
               env_vars [] in
           let$ refs, (gvar,tvar) = refs, cropable_vars in
           (Model.make_pat t (lift_constr ndim Crop) ~src:[|Expr.Ref (tvar, gvar)|]
              [| Model.make_def var0 (Model.make_any {t with kind = VEC POS});
                 Model.make_def var0 (Model.make_any {t with kind = VEC SIZE}) |])
           :: refs in
         let refs = (* Recoloring-expr *)
           if not nocolor then
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
             (Model.make_pat t (lift_constr ndim Recoloring) ~src:[|eg1|]
                [| Model.make_def var0 (Model.make_any {t with kind = MAP (COLOR C_OBJ, COLOR C_OBJ)}) |])
             :: refs
           else refs in
         refs
      | _ -> rs
    let refinements_postprocessing t m =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    let prunings_value t v =
      match t.kind, v with
      | _, `Null -> [] (* for when Null is used as a missing optional arg *)
      | INT ti, _ ->
         [ Model.make_any t ]
      | VEC tv, _ ->
         [ Model.make_any t ] 
      | COLOR tc, _ ->
         [ Model.make_any t ]
      | SEG, _ ->
         [ Model.make_any t ]
      | ORDER _, _ ->
         [ Model.make_any t ]
      | MOTIF tmot, _ ->
         [ Model.make_any t ]
      | GRID tg, _ ->
         [ Model.make_any t ]
      | OBJ tg, _ ->
      (*         [ Model.make_any t ] *)
         [ Model.make_pat t (lift_constr t.ndim Obj)
             [| Model.make_def var0 (Model.make_any {t with kind = VEC POS});
                Model.make_def var0 (Model.make_any {t with kind = GRID tg}) |] ]
      | MAP (ka,kb), _ ->
         [ Model.make_any t ]
      | _ -> pp_endline xp_typ t; pp_endline xp_value v; assert false
    let prunings_any t value =
      []
    let prunings_pat t c args value =
      [Model.make_any t]
    let prunings_postprocessing t m =
      fun m' ~supp ~nb ~alt best_reads ->
      Myseq.return (m', best_reads)

    (* initialization *)
      
    let varseq0 : varseq = Myseq.range 1 max_int

    let get_init_config name task =
      let open Task_model in
      let varseq = varseq0 in
      let xi, varseq = Model.new_var varseq in
      let xo, varseq = Model.new_var varseq in
      let input_model = Model.make_def xi (Model.make_any (scalar (GRID (`Full,false)))) in
      let output_model = Model.make_def xo (Model.make_any (scalar (GRID (`Full,false)))) in
      let distrib = `GridRange ((`Full,false),
                                Range.Closed (1,Grid.max_size),
                                Range.Closed (1,Grid.max_size),
                                Grid.all_colors,
                                None) in
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
    let log_refining r m prs lmd lrido lema =
      Printf.printf "REF  %.3f (%.3f)  %.3f  " lmd lema lrido;
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
