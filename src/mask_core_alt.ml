(* alternative definitions of Grid.MaskCore, which have not shown to be more efficient *)

module MaskCoreIntrel2 : MaskCore = (* slower than Bigarray.Array2 *)
  struct
    module Rel = Intrel2.Intmap
                   
    type t = { height : int;
               width : int;
               rel : Rel.t }

    let height m = m.height
    let width m = m.width

    let empty height width = { height; width; rel = Rel.empty }

    let rel_full height width = Common.prof "Grid.MaskCoreIntrel2.rel_full" (fun () ->        
      let rel = ref Rel.empty in
      for i = 0 to height-1 do
        for j = 0 to width-1 do
          rel := Rel.add i j !rel
        done
      done;
      !rel) (* TODO: implement in Intrel2.ml *)
      
    let full height width =
      { height; width; rel = rel_full height width }
    let singleton height width i j =
      let rel = Rel.singleton i j in
      { height; width; rel }

    let rel_subset rel1 rel2 = Common.prof "Grid.MaskCoreIntrel2.rel_subset" (fun () ->
      Rel.fold (fun res i j -> res && Rel.mem i j rel2) true rel1)
    (* TODO: implement in Intrel2.ml *)                       
      
    let equal m1 m2 =
      m1.height = m2.height
      && m1.width = m2.width
      && rel_subset m1.rel m2.rel && rel_subset m2.rel m1.rel
    let is_empty m = Rel.is_empty m.rel
    let is_subset m1 m2 = rel_subset m1.rel m2.rel

    let area m = Rel.cardinal m.rel
                        
    let mem i j m =
      i >= 0 && i < m.height
      && j >= 0 && j < m.width
      && Rel.mem i j m.rel
    let add i j m =
      { m with rel = Rel.add i j m.rel }
    let remove i j m =
      { m with rel = Rel.remove i j m.rel }

    let union m1 m2 = { m1 with rel = Rel.union m1.rel m2.rel }
    let inter m1 m2 = { m1 with rel = Rel.inter m1.rel m2.rel }
    let diff m1 m2 = { m1 with rel = Rel.diff m1.rel m2.rel }
    let diff_sym m1 m2 = { m1 with rel = Rel.union (Rel.diff m1.rel m2.rel) (Rel.diff m2.rel m1.rel) }
    let compl m = { m with rel = Rel.diff (rel_full m.height m.width) m.rel }

    let iter f m = Rel.iter f m.rel
    let fold f acc m = Rel.fold f acc m.rel

  end
  
module MaskCoreBigarray : MaskCore = (* about 3-4x slower than Z *)
  struct
    open Bigarray

    (* pure interface, functions must not change their argument arrays *)
       
    type t = (int, int8_unsigned_elt, c_layout) Array2.t

    let height m = Array2.dim1 m
    let width m = Array2.dim2 m

    let empty height width =
      let m = Array2.create Int8_unsigned C_layout height width in
      Array2.fill m 0;
      m
    let full height width =
      let m = Array2.create Int8_unsigned C_layout height width in
      Array2.fill m 1;
      m
    let singleton height width i j =
      let m = Array2.create Int8_unsigned C_layout height width in
      Array2.fill m 0;
      Array2.set m i j 1;
      m
    let copy m =
      let m' = Array2.create Int8_unsigned C_layout (Array2.dim1 m) (Array2.dim2 m) in
      Array2.blit m m';
      m' [@@inline]

    let fold f acc m =
      let h, w = Array2.dim1 m, Array2.dim2 m in
      let rec aux2 acc i j =
        if j >= w
        then acc
        else
          let acc =
            if Array2.get m i j <> 0
            then f acc i j
            else acc in (* processing cell *)
          aux2 acc i (j+1) in (* processing next cells *)
      let rec aux acc i =
        if i >= h
        then acc
        else
          let acc = aux2 acc i 0 in (* processing row *)
          aux acc (i+1) (* processing next rows *)
      in
      aux acc 0
      
    let for_all f m =
      let h, w = Array2.dim1 m, Array2.dim2 m in
      let rec aux2 i j =
        j >= w
        || (if Array2.get m i j <> 0
            then f i j && aux2 i (j+1)
            else aux2 i (j+1)) in
      let rec aux i =
        i >= h
        || (aux2 i 0 && aux (i+1))
      in
      aux 0
      
    let iter f m =
      let h, w = Array2.dim1 m, Array2.dim2 m in
      for i = 0 to h-1 do
        for j = 0 to w-1 do
          if Array2.get m i j <> 0 then
            f i j
        done
      done

    let map f m =
      let h, w = Array2.dim1 m, Array2.dim2 m in
      let res = Array2.create Int8_unsigned C_layout h w in
      for i = 0 to h-1 do
        for j = 0 to w-1 do
          Array2.set res i j (f i j (Array2.get m i j))
        done
      done;
      res

    let map2 f m1 m2 =
      let h1, w1 = Array2.dim1 m1, Array2.dim2 m1 in
      let h2, w2 = Array2.dim1 m2, Array2.dim2 m2 in
      assert (h1=h2 && w1=w2);
      let res = Array2.create Int8_unsigned C_layout h1 w1 in
      for i = 0 to h1-1 do
        for j = 0 to w1-1 do
          Array2.set res i j (f i j (Array2.get m1 i j) (Array2.get m2 i j))
        done
      done;
      res   

    let area m = fold (fun res i j -> res+1) 0 m
      
    let equal m1 m2 = (m1 = m2)
    let is_empty m = for_all (fun i j -> false) m
    let is_subset m1 m2 = for_all (fun i j -> Array2.get m2 i j <> 0) m1
      
    let mem i j m =
      i >= 0 && i < Array2.dim1 m
      && j >= 0 && j < Array2.dim2 m
      && Array2.get m i j <> 0
    let add i j m =
      let m' = copy m in
      Array2.set m' i j 1;
      m'
    let remove i j m =
      let m' = copy m in
      Array2.set m' i j 0;
      m'

    let union m1 m2 = map2 (fun i j b1 b2 -> b1 lor b2) m1 m2
    let inter m1 m2 = (* map2 (fun i j b1 b2 -> b1 land b2) m1 m2 *)
      let res = copy m1 in
      iter
        (fun i j ->
          if Array2.get m2 i j = 0 then
            Array2.set res i j 0)
        m1;
      res
    let diff m1 m2 = (* map2 (fun i j b1 b2 -> b1 land (1 - b2)) m1 m2 *)
      let res = copy m1 in
      iter
        (fun i j -> Array2.set res i j 0)
        m2;
      res
    let diff_sym m1 m2 = map2 (fun i j b1 b2 -> b1 lxor b2) m1 m2
    let compl m = map (fun i j b -> 1 - b) m
               
  end
  
module MaskCoreZ : MaskCore = (* based on Z arithmetics, as compact bitsets *)
  struct
    type t = { height : int;
	       width : int;
	       bits : Z.t; }

    let height m = m.height [@@inline]
    let width m = m.width [@@inline]
    let area m = Z.hamdist Z.zero m.bits
               
    let empty height width =
      { height; width; bits = Z.zero }
    let full height width =
      { height; width; bits = Z.pred (Z.shift_left Z.one (height * width)) }
    let singleton height width i j =
      { height; width; bits = Z.shift_left Z.one (i * width + j) }

    let equal m1 m2 =
      m1.height = m2.height
      && m1.width = m2.width
      && Z.equal m1.bits m2.bits
    let is_empty m =
      Z.equal m.bits Z.zero
    let is_subset m1 m2 =
      Z.equal (Z.logand m1.bits (Z.lognot m2.bits)) Z.zero

    let mem i j m =
      i >=0 && i < m.height
      && j >= 0 && j < m.width
      && Z.testbit m.bits (i * m.width + j)

    let add i j m =
      { m with
	bits = Z.logor m.bits (Z.shift_left Z.one (i * m.width + j)) }
    let remove i j m =
      { m with
	bits = Z.logand m.bits (Z.lognot (Z.shift_left Z.one (i * m.width + j))) }

    let union m1 m2 =
      { m1 with bits = Z.logor m1.bits m2.bits }
    let inter m1 m2 =
      { m1 with bits = Z.logand m1.bits m2.bits }
    let diff m1 m2 =
      { m1 with bits = Z.logand m1.bits (Z.lognot m2.bits) }
    let diff_sym m1 m2 =
      { m1 with bits = Z.logxor m1.bits m2.bits }
    let compl m1 =
      { m1 with bits = Z.lognot m1.bits }

    let iter f m =
      for i = 0 to m.height - 1 do
        let i_w = i * m.width in
	for j = 0 to m.width - 1 do
	  if Z.testbit m.bits (i_w + j) then
	    f i j
	done
      done

    let fold f acc m =
      let res = ref acc in
      for i = 0 to m.height - 1 do
        let i_w = i * m.width in
	for j = 0 to m.width - 1 do
	  if Z.testbit m.bits (i_w + j) then
	    res := f !res i j
	done
      done;
      !res

end
                                                 
module MaskCoreIntset : MaskCore = (* about 2x slower than Z *)
  struct
    module Intset = Intset.Intmap
                  
    type t = { height : int;
	       width : int;
	       set : Intset.t; }

    let height m = m.height [@@inline]
    let width m = m.width [@@inline]
    let area m = Intset.cardinal m.set

    let set_full n = Common.prof "Grid.MaskCoreIntset.set_full" (fun () ->        
      let set = ref Intset.empty in
      for i = 0 to n-1 do
        set := Intset.add i !set
      done;
      !set) (* TODO: implement in Intset.ml *)
               
    let empty height width =
      { height; width; set = Intset.empty }
    let full height width =
      { height; width; set = set_full (height * width) }
    let singleton height width i j =
      { height; width; set = Intset.singleton (i * width + j) }

    let equal m1 m2 =
      m1.height = m2.height
      && m1.width = m2.width
      && Intset.subset m1.set m2.set && Intset.subset m2.set m1.set
    let is_empty m = Intset.is_empty m.set
    let is_subset m1 m2 = Intset.subset m1.set m2.set

    let mem i j m =
      i >= 0 && i < m.height
      && j >= 0 && j < m.width
      && Intset.mem (i * m.width + j) m.set

    let add i j m = { m with set = Intset.add (i * m.width + j) m.set }
    let remove i j m = { m with set = Intset.remove (i * m.width + j) m.set }

    let union m1 m2 =
      { m1 with set = Intset.union m1.set m2.set }
    let inter m1 m2 =
      { m1 with set = Intset.inter m1.set m2.set }
    let diff m1 m2 =
      { m1 with set = Intset.diff m1.set m2.set }
    let diff_sym m1 m2 =
      { m1 with set = Intset.union (Intset.diff m1.set m2.set) (Intset.diff m2.set m1.set) }
    let compl m1 =
      { m1 with set = Intset.diff (set_full (m1.height * m1.width)) m1.set }

    let iter f m =
      let w = m.width in
      Intset.iter
        (fun k ->
          let i, j = k / w, k mod w in
          f i j)
        m.set

    let fold f acc m =
      let w = m.width in
      Intset.fold
        (fun acc k ->
          let i, j = k / w, k mod w in
          f acc i j)
        acc m.set

end
