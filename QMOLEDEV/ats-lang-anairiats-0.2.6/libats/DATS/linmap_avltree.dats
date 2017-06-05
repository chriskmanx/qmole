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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** A map implementation based on AVL trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2010 // based on a version done in October, 2008
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time
#define ATS_DYNLOADFLAG 0 // no dynamic loading at run-time

(* ****** ****** *)

staload "libats/SATS/linmap_avltree.sats"

(* ****** ****** *)
//
// a specialized version can be implemented on the spot
//
implement{key}
compare_key_key (x1, x2, cmp) = cmp (x1, x2)

(* ****** ****** *)
//
// HX-2010-03-24: this seems to work best!
//
#define HTDF 1 // max height difference
#define HTDF1 %(HTDF+1)
#define HTDF_1 %(HTDF-1)

(* ****** ****** *)

dataviewtype
avltree (
  key:t@ype, itm:viewt@ype+, int(*height*)
) =
  | {hl,hr:nat | hl <= hr+HTDF; hr <= hl+HTDF}
    B (key, itm, 1+max(hl,hr)) of
      (int (1+max(hl,hr)), key, itm, avltree (key, itm, hl), avltree (key, itm, hr))
  | E (key, itm, 0)
// end of [datatype avltree]
typedef avltree0 = avltree (void, void, 0)?

viewtypedef
avltree_inc (key:t@ype, itm:viewt@ype, h:int) =
  [h1:nat | h <= h1; h1 <= h+1] avltree (key, itm, h1)
// end of [avltree_inc]

viewtypedef
avltree_dec (key:t@ype, itm:viewt@ype, h:int) =
  [h1:nat | h1 <= h; h <= h1+1] avltree (key, itm, h1)
// end of [avltree_dec]

(* ****** ****** *)

assume
map_t0ype_viewt0ype_type
  (key:t@ype, itm:viewt@ype) = [h:nat] avltree (key, itm, h)
// end of [map_t0ype_viewt0ype_type]

(* ****** ****** *)

implement{} linmap_make_nil () = E ()

(* ****** ****** *)

implement{}
linmap_is_nil (t) =
  case+ t of | B _ => (fold@ t; false) | E () => (fold@ t; true)
// end of [linmap_is_nil]

implement{}
linmap_isnot_nil (t) =
  case+ t of | B _ => (fold@ t; true) | E () => (fold@ t; false)
// end of [linmap_isnot_nil]

(* ****** ****** *)

implement{key,itm}
linmap_size (t) = size (t) where {
  fun size {h:nat} .<h>.
    (t: !avltree (key, itm, h)):<> size_t = begin
    case+ t of
    | B (_(*h*), _(*key*), _(*itm*), !ptl, !ptr) => let
       val sz = (size_of_int1)1 + size (!ptl) + size (!ptr) in (fold@ t; sz)
      end // end of [B]
    | E () => (fold@ t; size_of_int1(0))
  end // end of [size]
} // end of [linmap_size]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
avltree_height {h:nat} (t: !avltree (key, itm, h)):<> int h =
  case+ t of B (h, _, _, _, _) => (fold@ t; h) | E _ => (fold@ t; 0)
// end of [avltree_height]

implement{key,itm} linmap_height (t) = avltree_height (t)

(* ****** ****** *)

implement{key,itm}
linmap_search
  (t, k0, cmp, res) = search (t, res) where {
  fun search {h:nat} .<h>. (
      t: !avltree (key, itm, h), res: &itm? >> opt (itm, b)
    ):<cloref> #[b:bool] bool b = begin
    case+ t of
    | B (_(*h*), k, x, !ptl, !ptr) => let
        val sgn = compare_key_key (k0, k, cmp)
      in
        case+ 0 of
        | _ when sgn < 0 => let
            val ans = search (!ptl, res) in fold@ t; ans
          end // end of [sgn < 0]
        | _ when sgn > 0 => let
            val ans = search (!ptr, res) in fold@ t; ans
          end // end of [sgn > 0]
        | _ => let
            val () = res := x; prval () = opt_some {itm} (res) in
            fold@ t; true
          end // end of [_]
      end // end of [B]
    | E () => let
        prval () = opt_none {itm} (res) in fold@ t; false
      end // end of [E]
  end // end of [search]
} // end of [linmap_search]

(* ****** ****** *)

(*
** left rotation for restoring height invariant
*)
fn{key:t0p;itm:vt0p}
avltree_lrotate
  {hl,hr:nat | hl+HTDF1 == hr}
  {l_h,l_k,l_x,l_tl,l_tr:addr} (
    pf_h: int? @ l_h
  , pf_k: key @ l_k, pf_x: itm @ l_x
  , pf_tl: avltree (key, itm, hl) @ l_tl
  , pf_tr: avltree (key, itm, hr) @ l_tr
  | p_h: ptr l_h
  , hl : int hl
  , p_tl: ptr l_tl
  , hr : int hr
  , p_tr: ptr l_tr
  , t: B_unfold (l_h, l_k, l_x, l_tl, l_tr)
  ) :<> avltree_inc (key, itm, hr) = let
  val tr = !p_tr
  val+ B {..} {hrl,hrr} (!p_hr, _, _, !p_trl, !p_trr) = tr
  val hrl = avltree_height<key,itm> (!p_trl)
  and hrr = avltree_height<key,itm> (!p_trr)
in
  if hrl <= hrr+HTDF_1 then let
    val hrl1 = hrl + 1
    val () = !p_h := hrl1
    // () = !p_tl := tl
    val () = !p_tr := !p_trl
    prval () = fold@ (t)
    val () = !p_hr := 1+max(hrl1,hrr)
    val () = !p_trl := t
    // val () = !p_trr := trr
    prval () = fold@ (tr)
  in
    tr // B (1+max(hrl1,hrr), kr, xr, B (hrl1, k, x, tl, trl), trr)
  end else let // [hrl=hrr+2]: deep rotation
    val trl = !p_trl
    val+ B {..} {hrll,hrlr} (!p_hrl, _, _, !p_trll, !p_trlr) = trl
    val hrll = avltree_height (!p_trll)
    val hrlr = avltree_height (!p_trlr)
    val () = !p_h := 1+max(hl,hrll)
    // val () = !p_tl := tl
    val () = !p_tr := !p_trll
    prval () = fold@ t
    val () = !p_hr := 1+max(hrlr, hrr)
    val () = !p_trl := !p_trlr
    // val () = !p_trr := trr
    prval () = fold@ tr
    val () = !p_hrl := hr
    val () = !p_trll := t
    val () = !p_trlr := tr
    prval () = fold@ (trl)
  in
    trl // B (hr, krl, xrl, B (1+max(hl,hrll), k, x, tl, trll), B (1+max(hrlr,hrr), kr, xr, trlr, trr))
  end // end of [if]
end // end of [avltree_lrotate]

(*
** right rotation for restoring height invariant
*)
fn{key:t0p;itm:vt0p}
avltree_rrotate
  {hl,hr:nat | hl == hr+HTDF1}
  {l_h,l_k,l_x,l_tl,l_tr:addr} (
    pf_h: int? @ l_h
  , pf_k: key @ l_k, pf_x: itm @ l_x
  , pf_tl: avltree (key, itm, hl) @ l_tl
  , pf_tr: avltree (key, itm, hr) @ l_tr
  | p_h: ptr l_h
  , hl : int hl
  , p_tl: ptr l_tl
  , hr : int hr
  , p_tr: ptr l_tr
  , t: B_unfold (l_h, l_k, l_x, l_tl, l_tr)
  ) :<> avltree_inc (key, itm, hl) = let
  val tl = !p_tl
  val+ B {..} {hll, hlr} (!p_hl, _, _, !p_tll, !p_tlr) = tl
  val hll = avltree_height<key,itm> (!p_tll)
  and hlr = avltree_height<key,itm> (!p_tlr)
in
  if hll+HTDF_1 >= hlr then let
    val hlr1 = hlr + 1
    val () = !p_h := hlr1
    val () = !p_tl := !p_tlr
    // () = !p_tr := tr
    prval () = fold@ (t)
    val () = !p_hl := 1+max(hll,hlr1)
    // val () = !p_tll := tll
    val () = !p_tlr := t
    prval () = fold@ (tl)
  in
    tl // B (1+max(hll,hlr1), kl, xl, tll, B (hlr1, k, x, tlr, tr))
  end else let
    val tlr = !p_tlr
    val+ B {..} {hlrl,hlrr} (!p_hlr, _, _, !p_tlrl, !p_tlrr) = tlr
    val hlrl = avltree_height (!p_tlrl)
    val hlrr = avltree_height (!p_tlrr)
    val () = !p_h := 1+max(hlrr,hr)
    val () = !p_tl := !p_tlrr
    // val () = !p_tr := tr
    prval () = fold@ t
    val () = !p_hl := 1+max(hll,hlrl)
    // val () = !p_tll := tll
    val () = !p_tlr := !p_tlrl
    prval () = fold@ tl
    val () = !p_hlr := hl
    val () = !p_tlrl := tl
    val () = !p_tlrr := t
    prval () = fold@ (tlr)
  in
    tlr // B (hl, klr, xlr, B (1+max(hll,hlrl), kl, xl, tll, tlrl), B (1+max(hlrr,hr), k, x, tlrr, tr))
  end // end of [if]
end // end of [avltree_rrotate]

(* ****** ****** *)

implement{key,itm}
linmap_insert
  (m, k0, x0, cmp, res) = let
//
fun insert {h:nat} .<h>. (
  t: &avltree (key, itm, h) >> avltree_inc (key, itm, h)
, x0: itm, res: &itm? >> opt (itm, b)
) :<cloref> #[b:bool] bool b = begin
  case+ t of
  | B {..} {hl,hr}
      (!p_h, !p_k, !p_x, !p_tl, !p_tr) => let
      prval pf_h = view@ !p_h
      prval pf_k = view@ !p_k
      prval pf_x = view@ !p_x
      prval pf_tl = view@ !p_tl
      prval pf_tr = view@ !p_tr        
      val sgn = compare_key_key (k0, !p_k, cmp)
    in
      if sgn < 0 then let
        val ans = insert (!p_tl, x0, res)
        val hl = avltree_height<key,itm> (!p_tl)
        and hr = avltree_height<key,itm> (!p_tr)
      in
        if hl - hr <= HTDF then let
          val () = !p_h := 1+max(hl,hr)
          prval () = fold@ (t)
        in
          ans // B (1+max(hl,hr), k, x, tl, tr)
        end else let // hl = hr+HTDF1
          val () = t := avltree_rrotate<key,itm>
            (pf_h, pf_k, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
        in
          ans
        end // end of [if]
      end else if sgn > 0 then let
        val ans = insert (!p_tr, x0, res)
        val hl = avltree_height<key,itm> (!p_tl)
        and hr = avltree_height<key,itm> (!p_tr)
      in
        if hr - hl <= HTDF then let
          val () = !p_h := 1+max(hl, hr)
          prval () = fold@ (t)
        in
          ans // B (1+max(hl, hr), k, x, tl, tr)
        end else let // hl+HTDF1 = hr
          val () = t := avltree_lrotate<key,itm>
            (pf_h, pf_k, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
        in
          ans
        end // end of [if]
      end else let (* key already exists *)
        val () = res := !p_x
        prval () = opt_some {itm} (res)
        val () = !p_x := x0
        prval () = fold@ t
      in
        true // B (h, k, x0, tl, tr)
      end // end of [if]
    end // end of [B]
  | ~E () => let
      val () = t := B (1, k0, x0, E (), E ()) // a new node is created
      prval () = opt_none {itm} (res)
    in
      false
    end // end of [E]
end // end of [insert]
//
in
//
insert (m, x0, res)
//
end // end of [linmap_insert]

(* ****** ****** *)

viewtypedef
B_node (key:t@ype, itm:viewt@ype) =
  B_pstruct (int?, key, itm, avltree0, avltree0)
// end of [B_node]

extern
castfn B_node_make
  {key:t@ype;itm:viewt@ype}
  {l_h,l_k,l_x,l_tl,l_tr:addr} (
  pf_h: int? @ l_h, pf_k: key @ l_k, pf_x: itm @ l_x, pf_tl: avltree? @ l_tl, pf_tr: avltree? @ l_tr
| x: B_unfold (l_h, l_k, l_x, l_tl, l_tr)) :<> B_node (key, itm) // end of [B_node_make]

fun{key:t0p;itm:vt0p}
avltree_takeout_min {h:pos} .<h>. (
  t: &avltree (key, itm, h) >> avltree_dec (key, itm, h)
) :<> B_node (key, itm) = let
  val+ B {..} {hl,hr} (!p_h, !p_k, !p_x, !p_tl, !p_tr) = t
  prval pf_h = view@ !p_h
  prval pf_k = view@ !p_k
  prval pf_x = view@ !p_x
  prval pf_tl = view@ !p_tl
  prval pf_tr = view@ !p_tr
in
  case+ !p_tl of
  | B _ => let
      prval () = fold@ (!p_tl)
      val node = avltree_takeout_min<key,itm> (!p_tl)
      val hl = avltree_height<key,itm> (!p_tl)
      and hr = avltree_height<key,itm> (!p_tr)
    in
      if hr - hl <= HTDF then let
        val () = !p_h := 1+max(hl,hr)
        prval () = fold@ t // B (1+max(hl,hr), k, x, tl, tr)
      in
        node
      end else let
        val () = t := avltree_lrotate<key,itm>
          (pf_h, pf_k, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
        // end of [val]
      in
        node
      end // end of [if]
    end // end of [B]
  | ~E () => let
      val tr = !p_tr; val t0 = t; val () = t := tr in
      B_node_make {key,itm} (pf_h, pf_k, pf_x, pf_tl, pf_tr | t0)
    end // end of [E]
end // end of [avltree_takeout_min]

(* ****** ****** *)
//
// HX-2010-03-25: unsafe but convenient to implement
//
extern
fun{key:t0p;itm:vt0p}
linmap_takeout_ptr {l_res:addr} (
  m: &map (key, itm), k0: key, cmp: cmp key, res: ptr l_res
) :<> bool
// end of [linmap_takeout]

implement{key,itm}
linmap_takeout_ptr {l_res}
  (m, k0, cmp, p_res) = takeout (m, p_res) where {
  fun takeout {h:nat} .<h>. (
    t: &avltree (key, itm, h) >> avltree_dec (key, itm, h)
  , p_res: ptr l_res
  ) :<cloref> bool = begin case+ t of
    | B {..} {hl,hr} (!p_h, !p_k, !p_x, !p_tl, !p_tr) => let
        stavar l_x:addr
        val p_x = p_x : ptr l_x
        prval pf_h = view@ !p_h
        prval pf_k = view@ !p_k
        prval pf_x = view@ !p_x
        prval pf_tl = view@ !p_tl
        prval pf_tr = view@ !p_tr
        val sgn = compare_key_key (k0, !p_k, cmp)
      in
        case+ 0 of
        | _ when sgn < 0 => let
            val ans(*removed*) = takeout (!p_tl, p_res)
            val hl = avltree_height<key,itm> (!p_tl)
            and hr = avltree_height<key,itm> (!p_tr)
          in
            if hr - hl <= HTDF then let
              prval () = !p_h := 1+max(hl,hr) in fold@ (t); ans
            end else let // hl+HTDF1 = hr
              val () = t := avltree_lrotate<key,itm>
                (pf_h, pf_k, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
              // end of [val]
            in
              ans
            end // end of [if]
          end // end of [sgn < 0]
        | _ when sgn > 0 => let
            val ans = takeout (!p_tr, p_res)
            val hl = avltree_height<key,itm> (!p_tl)
            and hr = avltree_height<key,itm> (!p_tr)
          in
            if hl - hr <= HTDF then let
              prval () = !p_h := 1+max(hl,hr) in fold@ (t); ans
            end else let // hl=hr+HTDF1
              val () = t := avltree_rrotate<key,itm>
                (pf_h, pf_k, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
              // end of [val]
            in
              ans
            end // end of [if]
          end // end of [sgn > 0]
        | _ (*sgn = 0*) => let
            val () = if :(pf_x: itm? @ l_x) =>
              (p_res > null) then let
              prval (pf, fpf) = __assert () where {
                extern praxi __assert (): (itm? @ l_res, itm @ l_res -<> void)
              } // end of [prval]
              val () = !p_res := !p_x
              prval () = fpf (pf)
            in
              // nothing
            end else let
              extern praxi __assert (pf: !itm @ l_x >> itm? @ l_x): void
              prval () = __assert (pf_x) // leak happens if [itm] contains resources!
            in
              // nothing
            end // end of [val]
            var tl = !p_tl and tr = !p_tr
            val () = free@ {key,itm} {0,0} (t)
          in
            case+ tr of
            | B _ => let
                prval () = fold@ tr
                val t1 = avltree_takeout_min<key,itm> (tr)
                val B (!p1_h, !p1_k, !p1_x, !p1_tl, !p1_tr) = t1
                prval pf1_h = view@ !p1_h
                prval pf1_k = view@ !p1_k
                prval pf1_x = view@ !p1_x                
                prval pf1_tl = view@ !p1_tl
                prval pf1_tr = view@ !p1_tr
                val hl = avltree_height<key,itm> (tl)
                and hr = avltree_height<key,itm> (tr)
                val () = !p1_tl := tl and () = !p1_tr := tr 
              in
                if hl - hr <= HTDF then let
                  val () = !p1_h := 1+max(hl,hr) in fold@ t1; t := t1; true
                end else let
                  val () = t := avltree_rrotate<key,itm>
                    (pf1_h, pf1_k, pf1_x, pf1_tl, pf1_tr | p1_h, hl, p1_tl, hr, p1_tr, t1)
                in
                  true
                end // end of [if]
              end // end of [B]
            | E _ => (t := tl; true)
          end // end of [sgn = 0]
      end // end of [B]
    | E () => (fold@ t; false(*~removed*))
  end // end of [takeout]
} // end of [linmap_takeout_ptr]

(* ****** ****** *)

implement{key,itm}
linmap_takeout
  (m, k0, cmp, res) = ans where {
  val ans = linmap_takeout_ptr<key,itm> (m, k0, cmp, &res)
  val [b:bool] ans = bool1_of_bool (ans)
  prval pf = __assert (view@ res) where {
    extern praxi __assert {l_res:addr} (pf: itm? @ l_res):<> opt (itm, b) @ l_res
  } // end of [prval]
  prval () = view@ res := pf
} // end of [linmap_takeout]

implement{key,itm}
linmap_remove (m, k0, cmp) = linmap_takeout_ptr<key,itm> (m, k0, cmp, null)

(* ****** ****** *)

(*
fun{key,itm:t@ype}
linmap_foreach_funenv {v:view} {vt:viewtype}
  (pf: !v | m: map (key, itm), f: (!v | key, itm, !vt) -<clo> void, env: !vt):<> void
// end of [linmap_foreach_funenv]
*)

implement{key,itm}
linmap_foreach_funenv {v} {vt}
  (pf | m, f, env) = foreach (pf | m, env) where {
  fun foreach {h:nat} .<h>.
    (pf: !v | t: !avltree (key, itm, h), env: !vt):<cloref> void =
    case+ t of
    | B (_(*h*), !p_k, !p_x, !p_tl, !p_tr) => begin
        foreach (pf | !p_tl, env); f (pf | !p_k, !p_x, env); foreach (pf | !p_tr, env); fold@ (t)
      end // end of [B]
    | E () => fold@ (t)
  // end of [foreach]
} // end of [linmap_foreach_funenv]

implement{key,itm}
linmap_foreach_fun
  (m, f) = let
//
  val f = coerce (f) where {
    extern castfn coerce
      (f: (key, &itm) -<fun> void):<> (!unit_v | key, &itm, !ptr) -<fun> void
  } // end of [val]
//
  prval pfu = unit_v ()
  val () = linmap_foreach_funenv<key,itm> {unit_v} {ptr} (pfu | m, f, null)
  prval unit_v () = pfu
//  
in
  // nothing
end // end of [linmap_foreach_fun]

(* ****** ****** *)

implement{key,itm}
linmap_foreach_vclo {v}
  (pf | m, f) = foreach (pf | m, f) where {
  fun foreach {h:nat} .<h>.
    (pf: !v | t: !avltree (key, itm, h), f: &(!v | key, &itm) -<clo> void):<> void =
    case+ t of
    | B (_(*h*), !p_k, !p_x, !p_tl, !p_tr) => begin
        foreach (pf | !p_tl, f); f (pf | !p_k, !p_x); foreach (pf | !p_tr, f); fold@ (t)
      end // end of [B]
    | E () => fold@ (t)
  // end of [foreach]
} // end of [linmap_foreach_vclo]

implement{key,itm}
linmap_foreach_cloref (m, f) = let
  val f = __cast (f) where { extern castfn __cast
    (f: (key, &itm) -<cloref> void):<> (!unit_v | key, &itm) -<cloref> void
  } // end of [val]
  typedef clo_type = (!unit_v | key, &itm) -<clo> void
  val (vbox pf_f | p_f) = cloref_get_view_ptr {clo_type} (f)
  prval pf0 = unit_v ()
  val () = $effmask_ref
    (linmap_foreach_vclo<key,itm> {unit_v} (pf0 | m, !p_f))
  prval unit_v () = pf0
in
  // empty
end // end of [linmap_foreach_cloref]

(* ****** ****** *)

implement{key,itm}
linmap_free (m) = _free (m) where {
  fun _free {h:nat} .<h>.
    (t: avltree (key, itm, h)):<> void = case+ t of
    | ~B (_, _, _, tl, tr) => (_free tl; _free tr) | ~E () => ()
  // end of [_free]
} // end of [linmap_free]

implement{key,itm}
linmap_free_vt (m) = let
  viewtypedef VT = map (key, itm) in
  case+ m of
  | B _ => true where {
      prval () = fold@ (m); prval () = opt_some {VT} (m)
    } // end of [B]
  | E () => false where {
      prval () = opt_none {VT} (m)
    } // end of [E]
end // end of [linmap_free]

(* ****** ****** *)
//
// HX: it can also be implemented based on [foreach]
//
implement{key,itm}
linmap_listize (m) = let
  viewtypedef res_t = List_vt @(key, itm)
  fun aux {h:nat} .<h>.
    (t: !avltree (key, itm, h), res: res_t):<> res_t =
    case+ t of
    | B (_(*h*), k, x, !p_tl, !p_tr) => let
        val res = aux (!p_tr, res)
        val res = list_vt_cons ((k, x), res)
        val res = aux (!p_tl, res)
        prval () = fold@ (t)
      in
        res
      end // end of [B]
    | E () => (fold@ (t); res)
  // end of [aux]
in
  aux (m, list_vt_nil)
end // end of [linmap_listize]

(* ****** ****** *)

implement{key,itm}
linmap_listize_free (m) = let
  viewtypedef res_t = List_vt @(key, itm)
  fun aux {h:nat} .<h>.
    (t: avltree (key, itm, h), res: res_t):<> res_t =
    case+ t of
    | ~B (_(*h*), k, x, tl, tr) => let
        val res = aux (tr, res)
        val res = list_vt_cons ((k, x), res)
        val res = aux (tl, res)
      in
        res
      end // end of [B]
    | ~E () => res // end of [E]
  // end of [aux]
in
  aux (m, list_vt_nil)
end // end of [linmap_listize_free]

(* ****** ****** *)

(* end of [linmap_avltree.dats] *)
