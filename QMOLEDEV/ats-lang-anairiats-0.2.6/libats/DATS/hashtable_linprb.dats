(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** A hashtable implementation based on linear probing
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2010
**
*)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload "libats/SATS/hashtable_linprb.sats"

(* ****** ****** *)

sortdef t0p = t@ype and vt0p = viewt@ype

(* ****** ****** *)

//
// HX-2010-03-20:
// special versions of these two functions can be implemented
// on the spot for gaining some efficiency
//
implement{key} hash_key (x, hash) = hash (x)
implement{key} equal_key_key (x1, x2, eqfn) = eqfn (x1, x2)

(* ****** ****** *)

stadef sizeof2
  (a1:viewt@ype, a2: viewt@ype) = sizeof @(a1, a2)
// end of [sizeof2]
stadef b2i = int_of_bool
dataview hashtbl_v // it is just an array of chains
  (key:t@ype, itm:viewt@ype+, int(*sz*), addr, addr) =
  | {sz:nat} {b:bool} {l_beg,l_end:addr}
    hashtbl_v_cons (key, itm, sz+1, l_beg, l_end) of
      (Opt @(key, itm) @ l_beg, hashtbl_v (key, itm, sz, l_beg+sizeof2(key,itm), l_end))
  | {l:addr} hashtbl_v_nil (key, itm, 0, l, l)
// end of [hashtbl_v]

viewtypedef HASHTBL (
  key: t0p, itm: vt0p, sz: int, l_beg: addr, l_end: addr
) = @{
  pfgc= free_gc_v (l_beg)
, pftbl= hashtbl_v (key, itm, sz, l_beg, l_end)
, sz= size_t sz
, tot= size_t
, pbeg= ptr l_beg
, hash= hash key
, eqfn = eqfn key
} // end of [HASHTBL]

viewtypedef HASHTBL (key: t0p, itm: vt0p) =
  [sz:int;l_beg,l_end:addr] [sz > 0] HASHTBL (key, itm, sz, l_beg, l_end)
// end of [HASHTBL]

extern typedef "HASHTBL" = HASHTBL (void, void)

extern
castfn HASHTBLptr_tblget
  {key:t0p;itm:vt0p} {l:agz} (ptbl: !HASHTBLptr (key, itm, l))
  :<> (HASHTBL (key, itm) @ l, minus (HASHTBLptr (key, itm, l), HASHTBL (key, itm) @ l) | ptr l)
// end of [HASHTBLptr_get]

(* ****** ****** *)

implement
hashtbl_size {key,itm} (ptbl) = sz where {
  val (pf, fpf | p) = HASHTBLptr_tblget {key,itm} (ptbl)
  val sz = p->sz
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_size]  

implement
hashtbl_total {key,itm} (ptbl) = tot where {
  val (pf, fpf | p) = HASHTBLptr_tblget {key,itm} (ptbl)
  val tot = p->tot
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_total]

implement{key,itm}
hashtbl_clear (ptbl) = () where {
  viewtypedef keyitm = @(key, itm)
  val (pf, fpf | p) = HASHTBLptr_tblget {key,itm} (ptbl)
  val () = __clear (p->pbeg, p->sz, sizeof<keyitm>) where {
    extern fun __clear (pbeg: ptr, sz: size_t, keyitmsz: size_t):<> void
      = "atslib_hashtbl_ptr_clear__linprb"
  } // end of [val]
  val () = p->tot := (size_of_int1)0
  prval () = minus_addback (fpf, pf | ptbl)  
} // [hashtbl_clear]

(* ****** ****** *)

extern fun hashtbl_ptr_make
  {key:t0p;itm:vt0p} {sz:pos}
  (sz: size_t sz, keyitmsz: sizeof_t @(key,itm))
  :<> [l_beg,l_end:addr] @(
    free_gc_v l_beg, hashtbl_v (key, itm, sz, l_beg, l_end) | ptr l_beg
  ) // end of [hashtbl_ptr_make]
  = "atslib_hashtbl_ptr_make__linprb"
// end of [hashtbl_ptr_make]

extern fun hashtbl_ptr_free
  {key:t0p;itm:vt0p} {sz:pos} {l_beg,l_end:addr} (
    pf_gc: free_gc_v l_beg
  , pf_tbl: hashtbl_v (key, itm, sz, l_beg, l_end) | p_beg: ptr l_beg
  ) :<> void
  = "atslib_hashtbl_ptr_free__linprb"
// end of [hashtbl_ptr_free]

(* ****** ****** *)

extern prfun // proof is omitted
  hashtbl_v_split {key:t0p;itm:vt0p}
  {sz,sz1:nat | sz1 <= sz} {l_beg,l_end:addr} {ofs:int} (
    pf_mul: MUL (sz1, sizeof2 (key,itm), ofs)
  , pf_tbl: hashtbl_v (key, itm, sz, l_beg, l_end)
  ) :<> @(
    hashtbl_v (key, itm, sz1, l_beg, l_beg+ofs)
  , hashtbl_v (key, itm, sz-sz1, l_beg+ofs, l_end)
  ) // end of [hashtbl_v_split]

extern prfun // proof is omitted
  hashtbl_v_unsplit {key:t0p;itm:vt0p}
  {sz1,sz2:nat} {l_beg,l_mid,l_end:addr} (
    pf1: hashtbl_v (key, itm, sz1, l_beg, l_mid)
  , pf2: hashtbl_v (key, itm, sz2, l_mid, l_end)
  ) :<prf> hashtbl_v (key, itm, sz1+sz2, l_beg, l_end)
// end of [hashtbl_v_unsplit]

(* ****** ****** *)

fn{key:t0p;itm:vt0p} hashtbl_ptr_split 
  {sz,sz1:nat | sz1 <= sz} {l_beg,l_end:addr} (
    pf_tbl: hashtbl_v (key, itm, sz, l_beg, l_end)
  | pbeg: ptr l_beg, sz1: size_t sz1
  ) :<> [l_mid:addr] @(
      hashtbl_v (key, itm, sz1, l_beg, l_mid)
    , hashtbl_v (key, itm, sz-sz1, l_mid, l_end)
    | ptr l_mid
    ) = let
  viewtypedef keyitm = @(key, itm)
  val (pf_mul | ofs) = mul2_size1_size1 (sz1, sizeof<keyitm>)
  prval (pf1_tbl, pf2_tbl) = hashtbl_v_split {key,itm} (pf_mul, pf_tbl)
in
  (pf1_tbl, pf2_tbl | pbeg + ofs)
end // end of [hashtbl_ptr_split]

(* ****** ****** *)

extern castfn size1_of_ulint (x: ulint):<> [i:nat] size_t i

(* ****** ****** *)

#define i2sz size1_of_int1
#define sz1mul mul_size1_size1
#define sz1mod mod1_size1_size1

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_ptr_probe_ofs
  {sz,ofs:nat | ofs < sz}
  {l_beg,l_end:addr} (
    pf: !hashtbl_v (key, itm, sz, l_beg, l_end)
  | pbeg: ptr l_beg
  , k0: key, eqfn: eqfn key, sz: size_t sz, ofs: size_t ofs
  , found: &bool? >> bool
  ) :<> Ptr (* pointing to the found item or where it should be *) = let
  val (pf1, pf2 | p_mid) =
    hashtbl_ptr_split<key,itm> {sz,ofs} (pf | pbeg, ofs)
  viewtypedef keyitm = @(key, itm)
  val keyitmsz = sizeof<keyitm>
  fun loop {n:nat} {l1,l2:addr} .<n>. (
      pf: !hashtbl_v (key, itm, n, l1, l2)
    | p1: ptr l1, n: size_t n, pres: &Ptr, found: &bool
    ) :<cloref> void =
    if n > 0 then let
      prval hashtbl_v_cons (pf1, pf2) = pf
      val isnotnull = keyitem_isnot_null<keyitm> (!p1)
      val () = if isnotnull then let
        prval () = opt_unsome {keyitm} (!p1)
        val k = p1->0
        prval () = Opt_some {keyitm} (!p1)
      in
        if equal_key_key (k0, k, eqfn) then
          (pres := p1; found := true)
        else
          loop (pf2 | p1 + keyitmsz, n-1, pres, found)
        // end of [if]
      end else let
        prval () = Opt_encode {keyitm} (!p1) in pres := p1
      end // end of [if]
      prval () = pf := hashtbl_v_cons (pf1, pf2)
    in
      // nothing
    end // end of [if]
  // end of [loop]
  var pres: Ptr = null
  val () = found := false
  val () = loop (pf2 | p_mid, sz - ofs, pres, found)
  val () = if :(found: bool) =>
    pres = null then loop (pf1 | pbeg, ofs, pres, found) else ()
  prval () = pf := hashtbl_v_unsplit (pf1, pf2)
in
  pres
end // end of [hashtbl_ptr_probe_ofs]

implement{key,itm}
hashtbl_search_ref (ptbl, k0) = let
  val (pf, fpf | p) = HASHTBLptr_tblget {key,itm} (ptbl)
  val h = hash_key (k0, p->hash)
  val h = size1_of_ulint (h); val ofs = sz1mod (h, p->sz)
  var found: bool // uninitalized
  val [l:addr] pkeyitm =
    hashtbl_ptr_probe_ofs<key,itm> (p->pftbl | p->pbeg, k0, p->eqfn, p->sz, ofs, found)
  prval () = minus_addback (fpf, pf | ptbl)
in
  if found then let
    prval (pf, fpf) = __assert () where {
      extern praxi __assert (): ((key,itm) @ l, (key,itm) @ l -<prf> void)
    } // end of [prval]
    val pitm = &(pkeyitm->1)
    prval () = fpf (pf)
  in
    pitm
  end else null
end // end of [hashtbl_search_ref]

(* ****** ****** *)

implement{key,itm}
hashtbl_search (ptbl, k0, res) = let
  val (pf, fpf | p) = HASHTBLptr_tblget {key,itm} (ptbl)
  val h = hash_key (k0, p->hash)
  val h = size1_of_ulint (h); val ofs = sz1mod (h, p->sz)
  var found: bool // uninitalized
  val [l:addr] pkeyitm =
    hashtbl_ptr_probe_ofs<key,itm> (p->pftbl | p->pbeg, k0, p->eqfn, p->sz, ofs, found)
  prval () = minus_addback (fpf, pf | ptbl)
in
  if found then let
    prval (pf, fpf) = __assert () where {
      extern praxi __assert (): ((key,itm) @ l, (key,itm) @ l -<prf> void)
    } // end of [prval]
    val () = res := pkeyitm->1
    prval () = opt_some {itm} (res)
    prval () = fpf (pf)
  in
    true
  end else let
    prval () = opt_none {itm} (res) in false
  end // end of [if]
end // end of [hashtbl_search]

(* ****** ****** *)

#define HASHTABLE_DOUBLE_FACTOR 2

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
  hashtbl_ptr_relocate
  {sz1:nat;sz2:pos} .<sz1>.
  {l1_beg,l2_beg,l1_end,l2_end:addr} (
    pf1: !hashtbl_v (key, itm, sz1, l1_beg, l1_end)
  , pf2: !hashtbl_v (key, itm, sz2, l2_beg, l2_end)
  | sz1: size_t sz1, sz2: size_t sz2, p1_beg: ptr l1_beg, p2_beg: ptr l2_beg
  , fhash: hash key, eqfn: eqfn key
  ) :<> void = let
  viewtypedef keyitm = @(key, itm)
in
  if sz1 > 0 then let
    prval hashtbl_v_cons (pf11, pf12) = pf1
    val isnotnull = keyitem_isnot_null<keyitm> (!p1_beg)
    val () = if isnotnull then let
      prval () = opt_unsome {keyitm} (!p1_beg)
      val k0 = p1_beg->0
      val i0 = p1_beg->1
      prval () = Opt_none {keyitm} (!p1_beg)
      val h = hash_key (k0, fhash)
      val h = size1_of_ulint (h); val ofs = sz1mod (h, sz2)
      var found: bool // uninitalized
      val [l:addr] pkeyitm =
        hashtbl_ptr_probe_ofs<key,itm> (pf2 | p2_beg, k0, eqfn, sz2, ofs, found)
      prval (pf, fpf) = __assert () where {
        extern praxi __assert (): ((key,itm?) @ l, (key,itm) @ l -<prf> void)
      } // end of [prval]
      val () = pkeyitm->0 := k0
      val () = pkeyitm->1 := i0
      prval () = fpf (pf)
    in
      // nothing
    end else let
      prval () = Opt_encode {keyitm} (!p1_beg) in (*nothing*)
    end // end of [if]
    val () = hashtbl_ptr_relocate<key,itm>
      (pf12, pf2 | sz1-1, sz2, p1_beg+sizeof<keyitm>, p2_beg, fhash, eqfn)
    prval () = pf1 := hashtbl_v_cons (pf11, pf12)
  in
    // empty
  end // end of [if]
end // end of [hashtbl_ptr_relocate]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_resize {l:agz} {sz_new:pos} (
  ptbl: !HASHTBLptr (key, itm, l), sz_new: size_t sz_new
) :<> void = () where {
  viewtypedef keyitm = @(key, itm)
  val (pf, fpf | p) = HASHTBLptr_tblget {key,itm} (ptbl)
  val (pfgc2, pftbl2 | pbeg2) = hashtbl_ptr_make {key,itm} (sz_new, sizeof<keyitm>)
  val () = hashtbl_ptr_relocate<key,itm>
    (p->pftbl, pftbl2 | p->sz, sz_new, p->pbeg, pbeg2, p->hash, p->eqfn)
  val () = hashtbl_ptr_free (p->pfgc, p->pftbl | p->pbeg)
  prval () = p->pfgc := pfgc2
  prval () = p->pftbl := pftbl2
  val () = p->sz := sz_new
  val () = p->pbeg := pbeg2
  prval () = minus_addback (fpf, pf | ptbl)
(*
  val () = $effmask_all begin
    print "hashtbl_resize(aft): sz_new = "; print sz_new; print_newline ()
  end // end of [val]
*)
} // end of [hashtbl_resize]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_ptr_reinsert
  {sz:pos} {l_beg,l_end:addr} (
    pf: !hashtbl_v (key, itm, sz, l_beg, l_end)
  | sz: size_t sz, p_beg: ptr l_beg, fhash: hash key, pkeyitm: Ptr
  ) :<> void = let
  viewtypedef keyitm = @(key, itm)
  viewtypedef keyitmopt = Opt @(key, itm)
  extern praxi __assert1
    {l:addr} (p: ptr l): (keyitmopt @ l, keyitmopt @ l -<prf> void)
  extern praxi __assert2
    {l:addr} (p: ptr l): ((key,itm?) @ l, (key,itm) @ l -<prf> void)
  extern praxi __assert3
    {l:addr} (p: ptr l): ((key,itm) @ l, keyitmopt @ l -<prf> void)
  val p_end = p_beg + sz1mul (sz, sizeof<keyitm>)
  fun ins {l1,l2:addr}
    (p1: ptr l1, p2: ptr l2, i0: &itm >> opt (itm, b)):<cloref1> #[b:bool] bool b =
    if p1 < p2 then let
      prval (pf, fpf) = __assert1 (p1)
      val isnotnull = keyitem_isnot_null<keyitm> (!p1)
      prval () = Opt_encode {keyitm} (!p1)
      prval () = fpf (pf)
    in
      if isnotnull then
        ins (p1+sizeof<keyitm>, p2, i0) // trying the next slot
      else let // an empty slot is found
        prval (pf, fpf) = __assert2 (p1)
        val () = p1->1 := i0
        prval () = fpf (pf)
        prval () = opt_none {itm} (i0)
      in
        false(*inserted*) // nothing is left
      end // end of [if]
    end else let
      prval () = opt_some {itm} (i0) in true(*notinserted*) // item is left
    end // end of [if]
  // end of [ins]
  fun rem {l1,l2:addr}
    (p1: ptr l1, p2: ptr l2):<cloref1> bool =
    if p1 < p2 then let
      prval (pf, fpf) = __assert1 (p1)
      val isnotnull = keyitem_isnot_null<keyitm> (!p1)
      prval () = Opt_encode {keyitm} (!p1)
      prval () = fpf (pf)
    in
      if isnotnull then let
        prval (pf, fpf) = __assert3 (p1)
        var i0 = p1->1
        val h = hash_key (p1->0, fhash)
        val () = keyitem_nullify<keyitm> (!p1)
        prval () = fpf (pf)
        val h = size1_of_ulint (h)
        val ofs = sz1mod (h, sz)
        val p_cur = p_beg + sz1mul (ofs, sizeof<keyitm>)
        val b1 = ins (p_cur, p_end, i0)
        val () = if :(i0: itm?) => (b1) then let
          prval () = opt_unsome {itm} (i0)
          val b2 = ins (p_beg, p_cur, i0)
          prval () = __assert (b2) where {
            // the item must have been inserted at this point
            extern praxi __assert {b:bool} (b: bool b): [b==false] void
          } // end of [val]
          prval () = opt_unnone {itm} (i0)
        in
          // nothing
        end else let
          prval () = opt_unnone {itm} (i0)
        in
          // nothing
        end // end of [if]
      in
        rem (p1+sizeof<keyitm>, p2)
      end else true(*done*)
    end else false(*notdone*)
  // end of [rem]
  val done = $effmask_all (
    if rem (pkeyitm+sizeof<keyitm>, p_end) then true else rem (p_beg, pkeyitm)
  ) : bool
in
  // nothing
end // end of [hashtbl_ptr_reinsert]

(* ****** ****** *)

implement{key,itm}
hashtbl_insert
  (ptbl, k0, i0, res) = found where {
  val (pf0, fpf0 | p) = HASHTBLptr_tblget {key,itm} (ptbl)
  val h = hash_key (k0, p->hash)
  val h = size1_of_ulint (h)
  val sz = p->sz
  val ofs = sz1mod (h, sz)
  var found: bool // uninitalized
  var doubleTag: int = 0
  val [l:addr] pkeyitm =
    hashtbl_ptr_probe_ofs<key,itm> (p->pftbl | p->pbeg, k0, p->eqfn, sz, ofs, found)
  val [b:bool] found = bool1_of_bool (found)
  val () = (if :(res: opt (itm, b)) => found then let
    prval (pf, fpf) = __assert () where {
      extern praxi __assert (): ((key,itm) @ l, (key,itm) @ l -<prf> void)
    } // end of [prval]
    val i = pkeyitm->1
    val () = pkeyitm->1 := i0
    val () = res := i
    prval () = fpf (pf)
    prval () = opt_some {itm} (res)
  in
    // nothing
  end else let
    val tot = p->tot
    val () = p->tot := tot + 1
//
    prval (pf, fpf) = __assert () where {
      extern praxi __assert (): ((key,itm?) @ l, (key,itm) @ l -<prf> void)
    } // end of [prval]
    val () = pkeyitm->0 := k0
    val () = pkeyitm->1 := i0
    prval () = fpf (pf)
//
    prval () = opt_none {itm} (res)
//
    val () = if (
      HASHTABLE_DOUBLE_FACTOR * (double_of_size)tot >= (double_of_size)sz
    ) then doubleTag := 1 // end of [if]
//
  in
    // nothing
  end) : void // end of [if]
  prval () = minus_addback (fpf0, pf0 | ptbl)
  val () = if (doubleTag > 0) then hashtbl_resize<key,itm> (ptbl, sz + sz)
} // end of [hashtbl_insert]

(* ****** ****** *)

(*
fun{key:t@ype;itm:viewt@ype}
hashtbl_remove {l:agz} (
  ptbl: !HASHTBLptr (key, itm, l), k0: key, res: &itm? >> opt (itm, b)
) :<> #[b:bool] bool b
// end of [hashtbl_remove]
*)
implement{key,itm}
hashtbl_remove {l} (ptbl, k0, res) = found where {
  val (pf0, fpf0 | p) = HASHTBLptr_tblget {key,itm} (ptbl)
  val h = hash_key (k0, p->hash)
  val h = size1_of_ulint (h)
  val sz = p->sz
  val ofs = sz1mod (h, sz)
  var found: bool // uninitalized
  // var halfTag: int = 0 // no shrinking
  val [l:addr] pkeyitm =
    hashtbl_ptr_probe_ofs<key,itm> (p->pftbl | p->pbeg, k0, p->eqfn, sz, ofs, found)
  val [b:bool] found = bool1_of_bool (found)
  val () = (if :(res: opt (itm, b)) => found then let
    val tot = p->tot
    val () = p->tot := tot - 1
    prval (pf, fpf) = __assert () where {
      extern praxi __assert (): ((key,itm) @ l, (key,itm?) @ l -<prf> void)
    } // end of [prval]
    val () = res := pkeyitm->1
    prval () = fpf (pf)
    prval () = opt_some {itm} (res)
  in
    hashtbl_ptr_reinsert<key,itm> (p->pftbl | sz, p->pbeg, p->hash, pkeyitm)
  end else let
    prval () = opt_none {itm} (res)
  in
    // nothing
  end) : void // end of [if]
  prval () = minus_addback (fpf0, pf0 | ptbl)
} // end of [hashtbl_remove]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
  hashtbl_ptr_foreach_vclo {v:view}
    {sz:nat} {l_beg,l_end:addr} {f:eff} .<sz>. (
    pf: !v, pf_tbl: !hashtbl_v (key, itm, sz, l_beg, l_end)
  | sz: size_t sz, pbeg: ptr l_beg, f: &(!v | key, &itm) -<clo,f> void
  ) :<f> void = begin
  if sz > 0 then let
    viewtypedef keyitm = @(key, itm)
    prval hashtbl_v_cons (pf1_tbl, pf2_tbl) = pf_tbl
    val isnotnull = keyitem_isnot_null<keyitm> (!pbeg)
    val () = if isnotnull then let
      prval () = opt_unsome {keyitm} (!pbeg)
      val () = f (pf | pbeg->0, pbeg->1)
      prval () = Opt_some {keyitm} (!pbeg)
    in
      // nothing
    end else let
      prval () = Opt_encode {keyitm} (!pbeg)
    in
      // nothing
    end // end of [val]
    val () = // segfault during typechecking if {v} is not provided!!!
      hashtbl_ptr_foreach_vclo<key,itm> {v} (pf, pf2_tbl | sz-1, pbeg+sizeof<keyitm>, f)
    prval () = pf_tbl := hashtbl_v_cons (pf1_tbl, pf2_tbl)
  in
    // empty
  end // end of [if]
end // end of [hashtbl_ptr_foreach_vclo]

implement{key,itm}
hashtbl_foreach_vclo {v}
  (pf0 | ptbl, f) = () where {
  val (pf, fpf | p) = HASHTBLptr_tblget {key,itm} (ptbl)  
  val () = begin
    hashtbl_ptr_foreach_vclo {v} (pf0, p->pftbl | p->sz, p->pbeg, f)
  end // end of [val]
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_foreach_vclo]

implement{key,itm}
hashtbl_foreach_cloref (tbl, f) = let
  val f = __cast (f) where { extern castfn __cast
    (f: (key, &itm) -<cloref> void):<> (!unit_v | key, &itm) -<cloref> void
  } // end of [val]
  typedef T = (!unit_v | key, &itm) -<clo> void
  val [l:addr] (pfbox | p_f) = cloref_get_view_ptr {T} (f)
  viewdef V = T @ l
  prval (pf, fpf) = __assert (pfbox) where {
    extern praxi __assert (_: vbox V): (V, V -<lin,prf> void)
  } // end of [prval]
  prval pf0 = unit_v ()
  val () = hashtbl_foreach_vclo<key,itm> {unit_v} (pf0 | tbl, !p_f)
  prval unit_v () = pf0
  prval () = fpf (pf)
in
  // empty
end // end of [hashtbl_foreach_cloref]

(* ****** ****** *)
//
#define HASHTABLE_MINSZ 97 // HX: it is chosen arbitrarily
//
extern
fun hashtbl_make_hint_tsz
  {key:t@ype;itm:viewt@ype} (
  hash: hash key
, eqfn: eqfn key
, hint: size_t
, keyitmsz: sizeof_t @(key,itm)
) :<> HASHTBLptr1 (key, itm) // tot = 0
  = "atslib_hashtbl_make_hint_tsz__linprb"
// end of [hashtbl_make_hint_tsz]

implement{key,itm}
hashtbl_make (_hash, _eqfn) = let
  viewtypedef keyitm = @(key, itm) in
  hashtbl_make_hint_tsz {key,itm} (_hash, _eqfn, 0, sizeof<keyitm>)
end // end of [hashtbl_make]

implement{key,itm}
hashtbl_make_hint
  (_hash, _eqfn, hint) = let
  viewtypedef keyitm = @(key, itm) in
  hashtbl_make_hint_tsz {key,itm} (_hash, _eqfn, hint, sizeof<keyitm>)
end // end of [hashtbl_make_hint]

(* ****** ****** *)

//
// HX-2010-07-01: is this really needed?
//
implement{key,itm}
hashtbl_listize (ptbl) = let
  typedef keyitm = @(key, itm)
  var res: List_vt keyitm = list_vt_nil ()
  viewdef V = List_vt keyitm @ res
  var !p_clo = @lam (
    pf: !V | k: key, x: &itm
  ) : void =<clo>
    (res := list_vt_cons ((k, x), res))
  // end of [var]
  val () = hashtbl_foreach_vclo<key,itm> {V} (view@ res | ptbl, !p_clo)
in
  res
end // end of [hashtbl_listize]

implement{key,itm}
hashtbl_listize_free
  {l} (ptbl) = kis where {
  typedef keyitm0 = @(key, itm?)
  viewtypedef keyitm = @(key, itm)
  val ptbl = __cast (ptbl) where {
    extern castfn __cast (x: HASHTBLptr (key, itm, l)):<> HASHTBLptr (key, itm?, l)
  } // end of [val]
  val kis = hashtbl_listize<key,itm?> (ptbl)
  val () = hashtbl_free (ptbl)
  val kis = __cast (kis) where {
    extern castfn __cast {n:int} (x: list_vt (keyitm0, n)):<> list_vt (keyitm, n)
  } // end of [val]
} // end of [hashtbl_listize_free]

(* ****** ****** *)

%{$
//
// declared in [string.h]
//
#ifndef memset
extern void *memset (void *buf, int chr, size_t n) ;
#endif
//
ats_ptr_type
atslib_hashtbl_ptr_make__linprb
  (ats_size_type sz, ats_size_type keyitmsz) {
  ats_ptr_type pbeg ;
  /* zeroing the allocated memory is mandatory! */
  pbeg = ATS_CALLOC(sz, keyitmsz) ;
  return pbeg ;
} // end of [atslib_hashtbl_ptr_make__linprb]
//
ats_void_type
atslib_hashtbl_ptr_clear__linprb (
  ats_ptr_type ptbl, ats_size_type sz, ats_size_type keyitmsz
) {
  memset (ptbl, 0, sz * keyitmsz) ; return ;
} // end of [atslib_hashtbl_clear__linprb]
//
ats_void_type
atslib_hashtbl_ptr_free__linprb
  (ats_ptr_type pbeg) { ATS_FREE(pbeg) ; return ; }
// end of [atslib_hashtbl_ptr_free__linprb]
//
%} // end of [%{$]

(* ****** ****** *)

%{$
//
// HX: shortcuts? yes. worth it? probably.
//
#define HASHTABLE_MINSZ 97 // it is chosen arbitrarily
//
ats_ptr_type
atslib_hashtbl_make_hint_tsz__linprb (
  ats_clo_ref_type hash
, ats_clo_ref_type eqfn
, ats_size_type hint
, ats_size_type keyitmsz
) {
  size_t sz ;
  HASHTBL *ptbl ; void *pbeg ;
  ptbl = ATS_MALLOC(sizeof(HASHTBL)) ;
  sz = (hint > 0 ? hint : HASHTABLE_MINSZ) ;
  /* zeroing the allocated memory is mandatory! */
  pbeg = ATS_CALLOC(sz, keyitmsz) ;
  ptbl->atslab_sz = sz ;
  ptbl->atslab_tot = 0 ;
  ptbl->atslab_pbeg = pbeg ;
  ptbl->atslab_hash = hash ;
  ptbl->atslab_eqfn = eqfn ;
  return ptbl ;
} // end of [atslib_hashtbl_make_hint_tsz__linprb]
//
ats_void_type
atslib_hashtbl_free__linprb (ats_ptr_type ptbl) {
  ATS_FREE(((HASHTBL*)ptbl)->atslab_pbeg) ; ATS_FREE(ptbl) ; return ;
} // end of [atslib_hashtbl_free__linprb]
//
ats_void_type
atslib_hashtbl_free_null__linprb (ats_ptr_type ptbl) { return ; }
// end of [atslib_hashtbl_free_null__linprb]
//
ats_int_type
atslib_hashtbl_free_vt__linprb (ats_ptr_type ptbl) {
  if (((HASHTBL*)ptbl)->atslab_tot != 0) return 1 ;
  ATS_FREE(((HASHTBL*)ptbl)->atslab_pbeg) ; ATS_FREE(ptbl) ; return 0 ;
} // end of [atslib_hashtbl_free_vt__linprb]
//
%} // end of [%{$]

(* ****** ****** *)

(* end of [hashtable_linprb.dats] *)
