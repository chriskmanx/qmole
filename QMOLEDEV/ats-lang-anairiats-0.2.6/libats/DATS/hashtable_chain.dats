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
** A hashtable implementation
** where buckets are represented as linked lists
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2010 // based on a version done in October, 2008
**
*)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload "libats/SATS/hashtable_chain.sats"

(* ****** ****** *)

sortdef t0p = t@ype and vt0p = viewt@ype

(* ****** ****** *)

implement{key} hash_key (x, hash) = hash (x)
implement{key} equal_key_key (x1, x2, eqfn) = eqfn (x1, x2)

(* ****** ****** *)

dataviewtype
chain (
  key:t@ype, itm:viewt@ype+, int
) =
  | {n:nat}
    CHAINcons (key, itm, n+1) of (key, itm, chain (key, itm, n))
  | CHAINnil (key, itm, 0)
// end of [chain]

viewtypedef
chain (
  key:t0p, itm:vt0p
) = [n:nat] chain (key, itm, n)

viewtypedef
chain0 = chain (void, void, 0)
stadef chainsz = sizeof (chain0)
extern typedef "chain0_ptr" = chain0

(* ****** ****** *)

fun{key:t0p;itm:t0p}
chain_free
  {n:nat} .<n>. (
  kis: chain (key, itm, n)
) :<> void = begin case+ kis of
  | ~CHAINcons (_(*key*), _(*itm*), kis) => chain_free (kis)
  | ~CHAINnil () => ()
end // end of [chain_free]

fun{key:t0p;itm:vt0p}
chain_free_fun
  {n:nat} .<n>. (
  kis: chain (key, itm, n), f: (&itm >> itm?) -<> void
) :<> void = begin case+ kis of
  | CHAINcons (_(*key*), !p_itm, kis1) => let
      val () = f (!p_itm)
      val () = free@ {key,itm}{0} (kis) in chain_free_fun (kis1, f)
    end (* end of [CHAINcon] *)
  | ~CHAINnil () => ()
end // end of [chain_free_fun]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
chain_search
  {n:nat} .<n>. (
  kis: !chain (key, itm, n)
, k0: key
, eqfn: eqfn key
) :<> Ptr =
  case+ kis of
  | CHAINcons (k, !i, !kis1) => let
      val keq = equal_key_key (k0, k, eqfn)
    in
      if keq then (fold@ kis; i) else let
        val ans = chain_search (!kis1, k0, eqfn)
      in
        fold@ kis; ans
      end // end of [if]
    end // end of [cons]
  | CHAINnil () => (fold@ kis; null)
// end of [chain_search]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
chain_insert {n:nat} (
  kis: &chain (key,itm,n) >> chain (key,itm,n+1), k: key, i: itm
) :<> void =
  kis := CHAINcons (k, i, kis)
// end of [chain_insert]

(* ****** ****** *)

stadef b2i = int_of_bool
fun{key:t0p;itm:vt0p}
chain_remove {n:nat} .<n>. (
  kis: &chain (key,itm,n) >> chain (key,itm,n-b2i b)
, k0: key, eqfn: eqfn key, res: &itm? >> opt (itm, b)
) :<> #[b:bool | b2i b <= n] bool b = begin case+ kis of
  | CHAINcons (k, !i, !kis1) => let
      val keq = equal_key_key (k0, k, eqfn)
    in
      if keq then let
        val () = res := !i
        prval () = opt_some {itm} (res)
        val kis1 = !kis1
      in
        free@ {key,itm}{n} (kis); kis := kis1; true
      end else let
        val ans = chain_remove<key,itm> {n-1} (!kis1, k0, eqfn, res)
      in
        fold@ kis; ans
      end // end of [if]
    end // end of [cons]
  | CHAINnil () => let
      prval () = opt_none {itm} (res)
      prval () = fold@ kis
    in
      false
    end // end of [nil]
end // end of [chain_remove]

fun{key:t0p;itm:vt0p}
chain_foreach_vclo {v:view} {n:nat} {f:eff} .<n>. (
  pf: !v | kis: !chain (key, itm, n), f: &(!v | key, &itm) -<clo,f> void
) :<f> void = begin case+ kis of
  | CHAINcons (k, !i, !kis1) => begin
      f (pf | k, !i); chain_foreach_vclo (pf | !kis1, f); fold@ kis
    end // end of [cons]
  | CHAINnil () => fold@ kis
end // end of [chain_foreach_vclo]

(* ****** ****** *)

dataview hashtbl_v // it is just an array of chains
  (key:t@ype, itm:viewt@ype+, int(*sz*), int(*tot*), addr, addr) =
  | {sz,tot,n:nat} {l_beg,l_end:addr}
    hashtbl_v_cons (key, itm, sz+1, tot+n, l_beg, l_end) of
      (chain (key, itm, n) @ l_beg, hashtbl_v (key, itm, sz, tot, l_beg+chainsz, l_end))
  | {l:addr} hashtbl_v_nil (key, itm, 0, 0, l, l)
// end of [hashtbl_v]

viewtypedef
HASHTBL (
  key: t0p, itm: vt0p
, sz: int, tot: int
, l_beg: addr, l_end: addr
) = @{
  pfgc= free_gc_v (l_beg)
, pftbl= hashtbl_v (key, itm, sz, tot, l_beg, l_end)
, sz= size_t sz
, tot= size_t tot
, pbeg= ptr l_beg
, hash= hash key
, eqfn = eqfn key
} // end of [HASHTBL]

viewtypedef
HASHTBL (
  key: t0p, itm: vt0p
) = [
  sz,tot:int;l_beg,l_end:addr;0 < sz; 0 <= tot
] HASHTBL (key, itm, sz, tot, l_beg, l_end)
// end of [HASHTBL]

extern typedef "HASHTBL_struct" = HASHTBL (void, void)

(* ****** ****** *)

extern
castfn HASHTBLptr_get_hashtbl
  {key:t0p;itm:vt0p}
  {l:agz} (
  ptbl: !HASHTBLptr (key, itm, l)
) :<> (
  HASHTBL (key, itm) @ l
, minus (HASHTBLptr (key, itm, l), HASHTBL (key, itm) @ l)
| ptr l
) // end of [HASHTBLptr_get_hashtble]

(* ****** ****** *)

implement
hashtbl_size
  {key,itm} (ptbl) = sz where {
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl {key,itm} (ptbl)
  val sz = p->sz
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_size]  

implement
hashtbl_total
  {key,itm} (ptbl) = tot where {
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl {key,itm} (ptbl)
  val tot = p->tot
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_total]  

(* ****** ****** *)

fun{key:t0p;itm:t0p}
hashtbl_ptr_clear
  {sz,tot:nat} {l_beg,l_end:addr} .<sz>. (
   pf: !hashtbl_v (key, itm, sz, tot, l_beg, l_end)
         >> hashtbl_v (key, itm, sz, 0(*tot*), l_beg, l_end)
| sz: size_t sz, p_beg: ptr l_beg
) :<> void = begin
  if sz > 0 then let
    prval hashtbl_v_cons (pf1, pf2) = pf
    val () = chain_free (!p_beg)
    val () = !p_beg := CHAINnil ()
    val () = hashtbl_ptr_clear<key,itm> (pf2 | sz-1, p_beg+sizeof<chain0>)
    prval () = pf := hashtbl_v_cons (pf1, pf2)
  in
    // empty
  end else let
    prval hashtbl_v_nil () = pf; prval () = pf := hashtbl_v_nil ()
  in
    // empty
  end // end of [if]
end // end of [hashtbl_ptr_clear]

implement{key,itm}
hashtbl_clear (ptbl) = () where {
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl {key,itm} (ptbl)
  val () = hashtbl_ptr_clear<key,itm> (p->pftbl | p->sz, p->pbeg)
  val () = p->tot := (size1_of_int1)0 // reset it to zero
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_clear]  

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
hashtbl_ptr_clear_fun
  {sz,tot:nat} {l_beg,l_end:addr} .<sz>. (
   pf: !hashtbl_v (key, itm, sz, tot, l_beg, l_end)
         >> hashtbl_v (key, itm, sz, 0(*tot*), l_beg, l_end)
| sz: size_t sz
, p_beg: ptr l_beg
, f: (&itm >> itm?) -<> void
) :<> void = begin
  if sz > 0 then let
    prval hashtbl_v_cons (pf1, pf2) = pf
    val () = chain_free_fun (!p_beg, f)
    val () = !p_beg := CHAINnil ()
    val () = hashtbl_ptr_clear_fun<key,itm> (pf2 | sz-1, p_beg+sizeof<chain0>, f)
    prval () = pf := hashtbl_v_cons (pf1, pf2)
  in
    // empty
  end else let
    prval hashtbl_v_nil () = pf; prval () = pf := hashtbl_v_nil ()
  in
    // empty
  end // end of [if]
end // end of [hashtbl_ptr_clear]

implement{key,itm}
hashtbl_clear_fun (ptbl, f) = () where {
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl {key,itm} (ptbl)
  val () = hashtbl_ptr_clear_fun<key,itm> (p->pftbl | p->sz, p->pbeg, f)
  val () = p->tot := (size1_of_int1)0 // reset it to zero
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_clear]  

(* ****** ****** *)

extern fun hashtbl_ptr_make
  {key:t0p;itm:vt0p} {sz:pos} (sz: size_t sz)
  :<> [l_beg,l_end:addr] @(
  free_gc_v l_beg
, hashtbl_v (key, itm, sz, 0(*tot*), l_beg, l_end)
| ptr l_beg
) = "atslib_hashtbl_ptr_make__chain"
// end of [hashtbl_ptr_make]

extern fun hashtbl_ptr_free
  {key:t0p;itm:vt0p} {sz:pos} {l_beg,l_end:addr} (
  pf_gc: free_gc_v l_beg
, pf_tbl: hashtbl_v (key, itm, sz, 0(*tot*), l_beg, l_end)
| p_beg: ptr l_beg
) :<> void
  = "atslib_hashtbl_ptr_free__chain"
// end of [hashtbl_ptr_free]

(* ****** ****** *)

extern prfun // HX: proof is omitted
hashtbl_v_split {key:t0p;itm:vt0p}
  {sz,sz1,tot:nat | sz1 <= sz} {l_beg,l_end:addr} {ofs:int} (
  pf_mul: MUL (sz1, chainsz, ofs)
, pf_tbl: hashtbl_v (key, itm, sz, tot, l_beg, l_end)
) :<> [tot1:nat | tot1 <= tot] @(
  hashtbl_v (key, itm, sz1, tot1, l_beg, l_beg+ofs)
, hashtbl_v (key, itm, sz-sz1, tot-tot1, l_beg+ofs, l_end)
) // end of [hashtbl_v_split]

extern prfun // HX: proof is omitted
hashtbl_v_unsplit {key:t0p;itm:vt0p}
  {sz1,sz2,tot1,tot2:nat} {l_beg,l_mid,l_end:addr} (
  pf1: hashtbl_v (key, itm, sz1, tot1, l_beg, l_mid)
, pf2: hashtbl_v (key, itm, sz2, tot2, l_mid, l_end)
) :<prf> hashtbl_v (
  key, itm, sz1+sz2, tot1+tot2, l_beg, l_end
) // end of [hashtbl_v_unsplit]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_ptr_split 
  {sz,sz1,tot:nat | sz1 <= sz} {l_beg,l_end:addr} (
  pf_tbl: hashtbl_v (key, itm, sz, tot, l_beg, l_end)
| p_beg: ptr l_beg, sz1: size_t sz1
) :<> [tot1:nat | tot1 <= tot] [l_mid:addr] @(
  hashtbl_v (key, itm, sz1, tot1, l_beg, l_mid)
, hashtbl_v (key, itm, sz-sz1, tot-tot1, l_mid, l_end)
| ptr l_mid
) = let
  val (pf_mul | ofs) = mul2_size1_size1 (sz1, sizeof<chain0>)
  prval (pf1_tbl, pf2_tbl) = hashtbl_v_split {key,itm} (pf_mul, pf_tbl)
in
  (pf1_tbl, pf2_tbl | p_beg + ofs)
end // end of [hashtbl_ptr_split]

(* ****** ****** *)

extern castfn size1_of_ulint (x: ulint):<> [i:nat] size_t i

(* ****** ****** *)

#define i2sz size1_of_int1
#define sz1mod mod1_size1_size1

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_ptr_search_ofs
  {sz,ofs,tot:nat | ofs < sz}
  {l_beg,l_end:addr} (
  pf: !hashtbl_v (key, itm, sz, tot, l_beg, l_end)
| p_beg: ptr l_beg, k0: key, eqfn: eqfn key, ofs: size_t ofs
) :<> Ptr (* null or pointing to the found item *) = let
  val (pf1, pf2 | p_mid) =
    hashtbl_ptr_split<key,itm> {sz,ofs,tot} (pf | p_beg, ofs)
  prval hashtbl_v_cons (pf21, pf22) = pf2
  val p_itm = chain_search (!p_mid, k0, eqfn)
  prval pf2 = hashtbl_v_cons (pf21, pf22)
  prval () = pf := hashtbl_v_unsplit (pf1, pf2)
in
  p_itm
end // end of [hashtbl_ptr_search_ofs]

implement{key,itm}
hashtbl_search_ref (ptbl, k0) = let
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl {key,itm} (ptbl)
  val h = hash_key (k0, p->hash)
  val h = size1_of_ulint (h); val ofs = sz1mod (h, p->sz)
  val [l:addr] p_itm =
    hashtbl_ptr_search_ofs (p->pftbl | p->pbeg, k0, p->eqfn, ofs)
  // end of [val]
  prval () = minus_addback (fpf, pf | ptbl)
in
  p_itm
end // end of [hashtbl_search_ref]

implement{key,itm}
hashtbl_search (ptbl, k0, res) = let
  val [l:addr] p_itm = hashtbl_search_ref (ptbl, k0)
in
  if p_itm <> null then let
    prval (fpf, pf) = __assert () where {
      extern praxi __assert (): (itm @ l -<prf> void, itm @ l)
    } // end of [prval]
    val () = res := !p_itm
    prval () = fpf (pf)
    prval () = opt_some {itm} (res)
  in
    true
  end else let
    prval () = opt_none {itm} (res) in false
  end // end of [if]
end // end of [hashtbl_search]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_ptr_insert_ofs
  {sz,ofs,tot:nat | ofs < sz}
  {l_beg,l_end:addr} (
  pf: !hashtbl_v (key, itm, sz, tot, l_beg, l_end)
        >> hashtbl_v (key, itm, sz, tot+1, l_beg, l_end)
| p_beg: ptr l_beg, k: key, i: itm, ofs: size_t ofs
) :<> void = let
  val (pf1, pf2 | p_mid) =
    hashtbl_ptr_split<key,itm> {sz,ofs,tot} (pf | p_beg, ofs)
  prval hashtbl_v_cons (pf21, pf22) = pf2
  val ans = chain_insert (!p_mid, k, i)
  prval pf2 = hashtbl_v_cons (pf21, pf22)
  prval () = pf := hashtbl_v_unsplit (pf1, pf2)
in
  // empty
end // end of [hashtbl_ptr_insert_ofs]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_ptr_remove_ofs
  {sz,ofs,tot:nat | ofs < sz}
  {l_beg,l_end:addr} (
  pf: !hashtbl_v (key, itm, sz, tot, l_beg, l_end)
        >> hashtbl_v (key, itm, sz, tot-b2i b, l_beg, l_end)
| p_beg: ptr l_beg, k0: key, eqfn: eqfn key, ofs: size_t ofs
, res: &itm? >> opt (itm, b)
) :<> #[b:bool | b2i b <= tot] bool b = let
  val (pf1, pf2 | p_mid) =
    hashtbl_ptr_split<key,itm> {sz,ofs,tot} (pf | p_beg, ofs)
  prval hashtbl_v_cons (pf21, pf22) = pf2
  val ans = chain_remove (!p_mid, k0, eqfn, res)
  prval pf2 = hashtbl_v_cons (pf21, pf22)
  prval () = pf := hashtbl_v_unsplit (pf1, pf2)
in
  ans
end // end of [hashtbl_ptr_remove_ofs]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
hashtbl_ptr_insert_chain
  {sz:pos;tot,n:nat}
  {l_beg,l_end:addr} .<n>. (
  pf: !hashtbl_v (key, itm, sz, tot, l_beg, l_end)
        >> hashtbl_v (key, itm, sz, tot+n, l_beg, l_end)
| sz: size_t sz
, p_beg: ptr l_beg
, kis: chain (key, itm, n)
, hash: hash key
) :<> void = begin case+ kis of
  | ~CHAINcons (k, i, kis1) => let
      // insertion must be done in the reverse order!
      val () = hashtbl_ptr_insert_chain (pf | sz, p_beg, kis1, hash)
      val h = hash_key (k, hash)
      val h = size1_of_ulint (h)
      val [ofs:int] ofs = sz1mod (h, sz)
      val (pf1, pf2 | p_mid) =
        hashtbl_ptr_split<key,itm> {sz,ofs,tot+n-1} (pf | p_beg, ofs)
      prval hashtbl_v_cons (pf21, pf22) = pf2
      val () = chain_insert (!p_mid, k, i)
      prval pf2 = hashtbl_v_cons (pf21, pf22)
      prval () = pf := hashtbl_v_unsplit (pf1, pf2)
    in
      // empty
    end // end of [cons]
  | ~CHAINnil () => ()
end // end of [hashtbl_ptr_insert_chain]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
hashtbl_ptr_relocate
  {sz1:nat;sz2:pos;tot1,tot2:nat}
  {l1_beg,l2_beg,l1_end,l2_end:addr}
  .<sz1>. (
  pf1: !hashtbl_v (key, itm, sz1, tot1, l1_beg, l1_end)
        >> hashtbl_v (key, itm, sz1, 0(*tot*), l1_beg, l1_end)
, pf2: !hashtbl_v (key, itm, sz2, tot2, l2_beg, l2_end)
        >> hashtbl_v (key, itm, sz2, tot1+tot2, l2_beg, l2_end)
| sz1: size_t sz1, sz2: size_t sz2
, p1_beg: ptr l1_beg, p2_beg: ptr l2_beg
, hash: hash key
) :<> void = begin
  if sz1 > 0 then let
    prval hashtbl_v_cons (pf11, pf12) = pf1
    val kis = !p1_beg; val () = !p1_beg := CHAINnil ()
    val () = hashtbl_ptr_insert_chain (pf2 | sz2, p2_beg, kis, hash)
    val () = hashtbl_ptr_relocate
      (pf12, pf2 | sz1-1, sz2, p1_beg+sizeof<chain0>, p2_beg, hash)
    prval () = pf1 := hashtbl_v_cons (pf11, pf12)
  in
    // empty
  end else let
    prval hashtbl_v_nil () = pf1; prval () = pf1 := hashtbl_v_nil ()
  in
    // empty
  end // end of [if]
end // end of [hashtbl_ptr_relocate]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_resize {l:agz} {sz_new:pos} (
  ptbl: !HASHTBLptr (key, itm, l), sz_new: size_t sz_new
) :<> void = () where {
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl {key,itm} (ptbl)
  val (pfgc2, pftbl2 | pbeg2) = hashtbl_ptr_make (sz_new)
  val () = hashtbl_ptr_relocate
    (p->pftbl, pftbl2 | p->sz, sz_new, p->pbeg, pbeg2, p->hash)
  // end of [val]
  val () = hashtbl_ptr_free (p->pfgc, p->pftbl | p->pbeg)
  prval () = p->pfgc := pfgc2
  prval () = p->pftbl := pftbl2
  val () = p->sz := sz_new
  val () = p->pbeg := pbeg2
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_resize]

(* ****** ****** *)

%{^
#define HASHTBL_MINSZ 97
%} // end of [%{^]
#define HASHTBL_MINSZ 97 // it is chosen, more or less, arbitrarily
#define HASHTABLE_DOUBLE_FACTOR 5.0
#assert (HASHTABLE_DOUBLE_FACTOR > 2.0)
#define HASHTABLE_HALF_FACTOR 0.5
#assert (HASHTABLE_HALF_FACTOR < 1.0)

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
hashtbl_resize_double
  {l:agz} (
  ptbl: !HASHTBLptr (key, itm, l)
) :<> void = let
  val sz = hashtbl_size (ptbl)
  val sz = size1_of_size (sz) // casting: no op
in
  if sz > 0 then hashtbl_resize<key,itm> (ptbl, sz + sz) else ()
end // end of [hashtbl_resize_double]

fn{key:t0p;itm:vt0p}
hashtbl_resize_half
  {l:agz} (
  ptbl: !HASHTBLptr (key, itm, l)
) :<> void = let
  val sz = hashtbl_size (ptbl)
  val sz = size1_of_size (sz) // casting: no op
  val sz2 = sz / 2
in
  if sz2 >= HASHTBL_MINSZ
    then hashtbl_resize<key,itm> (ptbl, sz2) else ()
  // end of [if]
end // end of [hashtbl_resize_half]

(* ****** ****** *)

implement{key,itm}
hashtbl_insert
  (ptbl, k, i) = () where {
  var ratio: double = 0.0
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl (ptbl)
  val tot1 = p->tot + 1
  val () = ratio := double_of_size (tot1) / double_of_size (p->sz)
  val h = hash_key (k, p->hash)
  val h = size1_of_ulint (h); val ofs = sz1mod (h, p->sz)
  val () = hashtbl_ptr_insert_ofs<key,itm> (p->pftbl | p->pbeg, k, i, ofs)
  val () = p->tot := tot1
  prval () = minus_addback (fpf, pf | ptbl)
  val () = if
    ratio >= HASHTABLE_DOUBLE_FACTOR then hashtbl_resize_double<key,itm> (ptbl)
  // end of [if]
} // end of [hashtbl_insert]

(* ****** ****** *)

implement{key,itm}
hashtbl_remove
  {l} (ptbl, k0, res) = ans where {
  var ratio: double = 1.0
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl {key,itm} (ptbl)
  val h = hash_key (k0, p->hash)
  val h = size1_of_ulint (h); val ofs = sz1mod (h, p->sz)
  val ans = hashtbl_ptr_remove_ofs<key,itm>
    (p->pftbl | p->pbeg, k0, p->eqfn, ofs, res)
  // end of [val]
  val () = (
    if :(pf: HASHTBL (key, itm) @ l) => ans then let
      val tot1 = p->tot - 1
      val () = ratio := double_of_size tot1 / double_of_size (p->sz)
      val () = p->tot := tot1
    in
      // nothing
    end else () // end of [if]
  ) : void // end of [val]
  prval () = minus_addback (fpf, pf | ptbl)
  val () = if
    ratio <= HASHTABLE_HALF_FACTOR then hashtbl_resize_half<key,itm> (ptbl)
  // end of [if]
} // end of [hashtbl_remove]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
hashtbl_ptr_foreach_vclo {v:view}
  {sz,tot:nat}
  {l_beg,l_end:addr}
  {f:eff} .<sz>. (
  pfv: !v
, pf_tbl: !hashtbl_v (key, itm, sz, tot, l_beg, l_end)
| sz: size_t sz, p_beg: ptr l_beg, f: &(!v | key, &itm) -<clo,f> void
) :<f> void = begin
  if sz > 0 then let
    prval hashtbl_v_cons (pf1_tbl, pf2_tbl) = pf_tbl
    val () = chain_foreach_vclo (pfv | !p_beg, f)
    val () = // segfault during typechecking if {v} is not provided!!!
      hashtbl_ptr_foreach_vclo<key,itm> {v} (pfv, pf2_tbl | sz-1, p_beg+sizeof<chain0>, f)
    prval () = pf_tbl := hashtbl_v_cons (pf1_tbl, pf2_tbl)
  in
    // empty
  end // end of [if]
end // end of [hashtbl_ptr_foreach_vclo]

implement{key,itm}
hashtbl_foreach_vclo
  {v} (pfv | ptbl, f) = () where {
  val (pf, fpf | p) = HASHTBLptr_get_hashtbl {key,itm} (ptbl)  
  val () = begin
    hashtbl_ptr_foreach_vclo {v} (pfv, p->pftbl | p->sz, p->pbeg, f)
  end // end of [val]
  prval () = minus_addback (fpf, pf | ptbl)
} // end of [hashtbl_foreach_vclo]

implement{key,itm}
hashtbl_foreach_cloref
  (tbl, f) = () where {
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
} // end of [hashtbl_foreach_cloref]

(* ****** ****** *)

(*
// some prime numbers
53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741
*)

implement
hashtbl_make {key,itm}
  (hash, eqfn) = hashtbl_make_hint {key,itm} (hash, eqfn, 0)
// end of [hashtbl_make]

(* ****** ****** *)

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
  list_vt_reverse (res) // list-reversing for the shadowing semantics
end // end of [hashtbl_listize]

implement{key,itm}
hashtbl_listize_free
  {l} (ptbl) = kis where {
  typedef keyitm0 = @(key, itm?)
  viewtypedef keyitm = @(key, itm)
  val ptbl = __cast (ptbl) where {
    extern castfn __cast (x: HASHTBLptr (key, itm, l)): HASHTBLptr (key, itm?, l)
  } // end of [val]
  val kis = hashtbl_listize<key,itm?> (ptbl)
  val () = hashtbl_free (ptbl)
  val kis = __cast (kis) where {
    extern castfn __cast {n:int} (x: list_vt (keyitm0, n)): list_vt (keyitm, n)
  } // end of [val]
} // end of [hashtbl_listize_free]

(* ****** ****** *)

%{$
//
// HX: shortcuts? yes. worth it? probably.
//
ats_ptr_type
atslib_hashtbl_ptr_make__chain
  (ats_size_type sz) {
  ats_ptr_type pbeg ;
/*
** HX: it is mandatory to initialize with zeros!
*/
  pbeg = ATS_CALLOC(sz, sizeof(chain0_ptr)) ;
  return pbeg ;
} // end of [atslib_hashtbl_ptr_make__chain]

ats_ptr_type
atslib_hashtbl_make_hint__chain (
  ats_clo_ref_type hash, ats_clo_ref_type eqfn
, ats_size_type hint
) {
  size_t sz ;
  HASHTBL_struct *ptbl ;
  void *pbeg ;
  ptbl = ATS_MALLOC(sizeof(HASHTBL_struct)) ;
  sz = (
    hint > 0 ? hint : HASHTBL_MINSZ
  ) ;
  /* zeroing the allocated memory is mandatory! */
  pbeg = ATS_CALLOC(sz, sizeof(chain0_ptr)) ;
  ptbl->atslab_sz = sz ;
  ptbl->atslab_tot = 0 ;
  ptbl->atslab_pbeg = pbeg ;
  ptbl->atslab_hash = hash ;
  ptbl->atslab_eqfn = eqfn ;
  return ptbl ;
} // end of [atslib_hashtbl_make_hint__chain]

%} // end of [%{$]

(* ****** ****** *)

%{$

ats_void_type
atslib_hashtbl_free__chain
  (ats_ptr_type ptbl) {
  ATS_FREE(((HASHTBL_struct*)ptbl)->atslab_pbeg) ; ATS_FREE(ptbl) ;
  return ;
} // end of [atslib_hashtbl_free__chain]

ats_bool_type
atslib_hashtbl_free_vt__chain
  (ats_ptr_type ptbl) {
  if (((HASHTBL_struct*)ptbl)->atslab_tot != 0)
    return ats_true_bool ;
  ATS_FREE(((HASHTBL_struct*)ptbl)->atslab_pbeg) ; ATS_FREE(ptbl) ;
  return ats_false_bool ;
} // end of [atslib_hashtbl_free_vt__chain]

%} // end of [%{$]

(* ****** ****** *)

(* end of [hashtable_chain.dats] *)
