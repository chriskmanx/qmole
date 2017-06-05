(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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
** A set implementation based on AVL trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: October, 2011
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

staload "libats/SATS/linset_avltree.sats"

(* ****** ****** *)

sortdef t0p = t@ype

(* ****** ****** *)
//
// a specialized version can be implemented on the spot
//
implement{a}
compare_elt_elt (x1, x2, cmp) = cmp (x1, x2)

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
  a:t@ype+, int(*height*)
) =
  | {hl,hr:nat | hl <= hr+HTDF; hr <= hl+HTDF}
    B (a, 1+max(hl,hr)) of
      (int (1+max(hl,hr)), a, avltree (a, hl), avltree (a, hr))
  | E (a, 0)
// end of [datatype avltree]
typedef avltree0 = avltree (void, 0)?

viewtypedef
avltree_inc (a:t@ype, h:int) =
  [h1:nat | h <= h1; h1 <= h+1] avltree (a, h1)
// end of [avltree_inc]

viewtypedef
avltree_dec (a:t@ype, h:int) =
  [h1:nat | h1 <= h; h <= h1+1] avltree (a, h1)
// end of [avltree_dec]

(* ****** ****** *)

assume
set_t0ype_type
  (a:t@ype) = [h:nat] avltree (a, h)
// end of [set_t0ype_type]

(* ****** ****** *)

implement{} linset_make_nil () = E ()

(* ****** ****** *)

implement{}
linset_is_empty (t) =
  case+ t of | B _ => (fold@ t; false) | E () => (fold@ t; true)
// end of [linset_is_empty]

implement{}
linset_isnot_empty (t) =
  case+ t of | B _ => (fold@ t; true) | E () => (fold@ t; false)
// end of [linset_isnot_empty]

(* ****** ****** *)

implement{a}
linset_size (t) = size (t) where {
  fun size {h:nat} .<h>.
    (t: !avltree (a, h)):<> size_t = begin case+ t of
    | B (_(*h*), _(*elt*), !ptl, !ptr) => let
       val sz = (size_of_int1)1 + size (!ptl) + size (!ptr) in (fold@ t; sz)
      end // end of [B]
    | E () => (fold@ t; size_of_int1(0))
  end // end of [size]
} // end of [linset_size]

(* ****** ****** *)

fn{a:t@ype}
avltree_height {h:int}
  (t: !avltree (a, h)):<> int h =
  case+ t of B (h, _, _, _) => (fold@ t; h) | _ =>> 0
// end of [avltree_height]

(* ****** ****** *)

implement{a}
linset_is_member
  (xs, x0, cmp) = aux (xs) where {
  fun aux {h:nat} .<h>.
    (t: !avltree (a, h)):<cloref> bool =
    case+ t of
    | B (_, x, !p_tl, !p_tr) => let
        val sgn = compare_elt_elt (x0, x, cmp)
        val res = if sgn < 0 then
          aux (!p_tl) else (if sgn > 0 then aux (!p_tr) else true)
        // end of [val]
      in
        fold@ (t); res
      end // end of [B]
    | E () => (fold@ (t); false)
  // end of [aux]
} // end of [linset_is_member]

implement{a}
linset_isnot_member (xs, x0, cmp) = ~linset_is_member (xs, x0, cmp)

(* ****** ****** *)

(*
** left rotation for restoring height invariant
*)
fn{a:t0p}
avltree_lrotate
  {hl,hr:nat | hl+HTDF1 == hr}
  {l_h,l_k,l_x,l_tl,l_tr:addr} (
    pf_h: int? @ l_h
  , pf_x: a @ l_x
  , pf_tl: avltree (a, hl) @ l_tl
  , pf_tr: avltree (a, hr) @ l_tr
  | p_h: ptr l_h
  , hl : int hl
  , p_tl: ptr l_tl
  , hr : int hr
  , p_tr: ptr l_tr
  , t: B_unfold (l_h, l_x, l_tl, l_tr)
  ) :<> avltree_inc (a, hr) = let
  val tr = !p_tr
  val+ B {..} {hrl,hrr} (!p_hr, _, !p_trl, !p_trr) = tr
  val hrl = avltree_height<a> (!p_trl)
  and hrr = avltree_height<a> (!p_trr)
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
    val+ B {..} {hrll,hrlr} (!p_hrl, _, !p_trll, !p_trlr) = trl
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
fn{a:t0p}
avltree_rrotate
  {hl,hr:nat | hl == hr+HTDF1}
  {l_h,l_k,l_x,l_tl,l_tr:addr} (
    pf_h: int? @ l_h
  , pf_x: a @ l_x
  , pf_tl: avltree (a, hl) @ l_tl
  , pf_tr: avltree (a, hr) @ l_tr
  | p_h: ptr l_h
  , hl : int hl
  , p_tl: ptr l_tl
  , hr : int hr
  , p_tr: ptr l_tr
  , t: B_unfold (l_h, l_x, l_tl, l_tr)
  ) :<> avltree_inc (a, hl) = let
  val tl = !p_tl
  val+ B {..} {hll, hlr} (!p_hl, _, !p_tll, !p_tlr) = tl
  val hll = avltree_height<a> (!p_tll)
  and hlr = avltree_height<a> (!p_tlr)
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
    val+ B {..} {hlrl,hlrr} (!p_hlr, _, !p_tlrl, !p_tlrr) = tlr
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

implement{a}
linset_insert
  (xs, x0, cmp) = let
//
fun insert {h:nat} .<h>. (
  t: &avltree (a, h) >> avltree_inc (a, h), x0: a
) :<cloref> #[b:bool] bool b = begin
  case+ t of
  | B {..} {hl,hr}
      (!p_h, !p_x, !p_tl, !p_tr) => let
      prval pf_h = view@ !p_h
      prval pf_x = view@ !p_x
      prval pf_tl = view@ !p_tl
      prval pf_tr = view@ !p_tr        
      val sgn = compare_elt_elt (x0, !p_x, cmp)
    in
      if sgn < 0 then let
        val ans = insert (!p_tl, x0)
        val hl = avltree_height<a> (!p_tl)
        and hr = avltree_height<a> (!p_tr)
      in
        if hl - hr <= HTDF then let
          val () = !p_h := 1+max(hl,hr)
          prval () = fold@ (t)
        in
          ans // B (1+max(hl,hr), k, x, tl, tr)
        end else let // hl = hr+HTDF1
          val () = t := avltree_rrotate<a>
            (pf_h, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
        in
          ans
        end // end of [if]
      end else if sgn > 0 then let
        val ans = insert (!p_tr, x0)
        val hl = avltree_height<a> (!p_tl)
        and hr = avltree_height<a> (!p_tr)
      in
        if hr - hl <= HTDF then let
          val () = !p_h := 1+max(hl, hr)
          prval () = fold@ (t)
        in
          ans // B (1+max(hl, hr), k, x, tl, tr)
        end else let // hl+HTDF1 = hr
          val () = t := avltree_lrotate<a>
            (pf_h, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
        in
          ans
        end // end of [if]
      end else let (* key already exists *)
        prval () = fold@ t in true // B (h, k, x0, tl, tr)
      end // end of [if]
    end // end of [B]
  | ~E () => let
      val () = t := B (1, x0, E (), E ()) in false // a newly created node 
    end // end of [E]
end // end of [insert]
//
in
//
insert (xs, x0)
//
end // end of [linset_insert]

(* ****** ****** *)

viewtypedef
B_node (a:t@ype) =
  B_pstruct (int?, a, avltree0, avltree0)
// end of [B_node]

extern
castfn B_node_make
  {a:t@ype} {l_h,l_x,l_tl,l_tr:addr} (
  pf_h: int? @ l_h, pf_x: a @ l_x, pf_tl: avltree? @ l_tl, pf_tr: avltree? @ l_tr
| x: B_unfold (l_h, l_x, l_tl, l_tr)
) :<> B_node (a) // end of [B_node_make]

fun{a:t@ype}
avltree_takeout_min {h:pos} .<h>. (
  t: &avltree (a, h) >> avltree_dec (a, h)
) :<> B_node (a) = let
  val+ B {..} {hl,hr} (!p_h, !p_x, !p_tl, !p_tr) = t
  prval pf_h = view@ !p_h
  prval pf_x = view@ !p_x
  prval pf_tl = view@ !p_tl
  prval pf_tr = view@ !p_tr
in
  case+ !p_tl of
  | B _ => let
      prval () = fold@ (!p_tl)
      val node = avltree_takeout_min<a> (!p_tl)
      val hl = avltree_height<a> (!p_tl)
      and hr = avltree_height<a> (!p_tr)
    in
      if hr - hl <= HTDF then let
        val () = !p_h := 1+max(hl,hr)
        prval () = fold@ t // B (1+max(hl,hr), k, x, tl, tr)
      in
        node
      end else let
        val () = t := avltree_lrotate<a>
          (pf_h, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
        // end of [val]
      in
        node
      end // end of [if]
    end // end of [B]
  | ~E () => let
      val tr = !p_tr; val t0 = t; val () = t := tr in
      B_node_make {a} (pf_h, pf_x, pf_tl, pf_tr | t0)
    end // end of [E]
end // end of [avltree_takeout_min]

(* ****** ****** *)

implement{a}
linset_remove
  (xs, x0, cmp) = remove (xs) where {
  fun remove {h:nat} .<h>. (
    t: &avltree (a, h) >> avltree_dec (a, h)
  ) :<cloref> bool = begin case+ t of
    | B {..} {hl,hr} (!p_h, !p_x, !p_tl, !p_tr) => let
        stavar l_x:addr
        val p_x = p_x : ptr l_x
        prval pf_h = view@ !p_h
        prval pf_x = view@ !p_x
        prval pf_tl = view@ !p_tl
        prval pf_tr = view@ !p_tr
        val sgn = compare_elt_elt (x0, !p_x, cmp)
      in
        case+ 0 of
        | _ when sgn < 0 => let
            val ans(*removed*) = remove (!p_tl)
            val hl = avltree_height<a> (!p_tl)
            and hr = avltree_height<a> (!p_tr)
          in
            if hr - hl <= HTDF then let
              prval () = !p_h := 1+max(hl,hr) in fold@ (t); ans
            end else let // hl+HTDF1 = hr
              val () = t := avltree_lrotate<a>
                (pf_h, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
              // end of [val]
            in
              ans
            end // end of [if]
          end // end of [sgn < 0]
        | _ when sgn > 0 => let
            val ans = remove (!p_tr)
            val hl = avltree_height<a> (!p_tl)
            and hr = avltree_height<a> (!p_tr)
          in
            if hl - hr <= HTDF then let
              prval () = !p_h := 1+max(hl,hr) in fold@ (t); ans
            end else let // hl=hr+HTDF1
              val () = t := avltree_rrotate<a>
                (pf_h, pf_x, pf_tl, pf_tr | p_h, hl, p_tl, hr, p_tr, t)
              // end of [val]
            in
              ans
            end // end of [if]
          end // end of [sgn > 0]
        | _ (*sgn = 0*) => let
            var tl = !p_tl and tr = !p_tr
            val () = free@ {a} {0,0} (t)
          in
            case+ tr of
            | B _ => let
                prval () = fold@ tr
                val t1 = avltree_takeout_min<a> (tr)
                val B (!p1_h, !p1_x, !p1_tl, !p1_tr) = t1
                prval pf1_h = view@ !p1_h
                prval pf1_x = view@ !p1_x                
                prval pf1_tl = view@ !p1_tl
                prval pf1_tr = view@ !p1_tr
                val hl = avltree_height<a> (tl)
                and hr = avltree_height<a> (tr)
                val () = !p1_tl := tl and () = !p1_tr := tr 
              in
                if hl - hr <= HTDF then let
                  val () = !p1_h := 1+max(hl,hr) in fold@ t1; t := t1; true
                end else let
                  val () = t := avltree_rrotate<a>
                    (pf1_h, pf1_x, pf1_tl, pf1_tr | p1_h, hl, p1_tl, hr, p1_tr, t1)
                  // end of [val]
                in
                  true
                end // end of [if]
              end // end of [B]
            | E _ => (t := tl; true)
          end // end of [sgn = 0]
      end // end of [B]
    | E () => (fold@ t; false(*~removed*))
  end // end of [remove]
} // end of [linset_remove]

(* ****** ****** *)

(*
** left join: height(tl) >= height(tr)
*)
fun{a:t@ype}
avltree_ljoin
  {hl,hr:nat | hl >= hr} .<hl>. (
  xn: B_node (a)
, tl: avltree (a, hl), tr: avltree (a, hr)
) :<> avltree_inc (a, hl) = let
  val hl = avltree_height (tl): int hl
  and hr = avltree_height (tr): int hr
in
  if hl >= hr + HTDF1 then let
    val+ B {..} {hll, hlr} (!p_hl, !p_xl, !p_tll, !p_tlr) = tl
    val [hlr:int] tlr = avltree_ljoin<a> (xn, !p_tlr, tr)
    val () = !p_tlr := tlr
    val hll = avltree_height<a> (!p_tll): int hll
    and hlr = avltree_height<a> (!p_tlr): int hlr
  in
    if hlr <= hll + HTDF then let
      val () = !p_hl := max (hll, hlr) + 1
      prval () = fold@ {a} (tl)
    in
      tl
    end else let // hll+HTDF1 = hlr
      prval pf_hl = view@(!p_hl)
      prval pf_xl = view@(!p_xl)
      prval pf_tll = view@ (!p_tll)
      prval pf_tlr = view@ (!p_tlr)
    in
      avltree_lrotate<a> (
        pf_hl, pf_xl, pf_tll, pf_tlr | p_hl, hll, p_tll, hlr, p_tlr, tl
      ) // end of [avltree_lrotate]
    end // end of [if]
  end else let
    val B (!p_h, _, !p_tl, !p_tr) = xn
    val () = !p_h := hl + 1
    val () = !p_tl := tl and () = !p_tr := tr
  in
    fold@ {a} (xn); xn
  end // end of [if]
end // end of [avltree_ljoin]

(*
** right join: height(tl) <= height(tr)
*)
fun{a:t@ype}
avltree_rjoin
  {hl,hr:nat| hl <= hr} .<hr>. (
  xn: B_node (a)
, tl: avltree (a, hl), tr: avltree (a, hr)
) :<> avltree_inc (a, hr) = let
  val hl = avltree_height (tl): int hl
  and hr = avltree_height (tr): int hr
in
  if hr >= hl + HTDF1 then let
    val+ B {..} {hrl,hrr} (!p_hr, !p_xr, !p_trl, !p_trr) = tr
    val [hrl:int] trl = avltree_rjoin<a> (xn, tl, !p_trl)
    val () = !p_trl := trl
    val hrl = avltree_height<a> (!p_trl): int hrl
    and hrr = avltree_height<a> (!p_trr): int hrr
  in
    if hrl <= hrr + HTDF then let
      val () = !p_hr := max (hrl,hrr) + 1
      prval () = fold@ {a} (tr)
    in
      tr
    end else let // hrl = hrr+HTDF1
      prval pf_hr = view@(!p_hr)
      prval pf_xr = view@(!p_xr)
      prval pf_trl = view@ (!p_trl)
      prval pf_trr = view@ (!p_trr)
    in
      avltree_rrotate<a> (
        pf_hr, pf_xr, pf_trl, pf_trr | p_hr, hrl, p_trl, hrr, p_trr, tr
      ) // end of [avltree_lrotate]
    end // end of [if]
  end else let
    val B (!p_h, _, !p_tl, !p_tr) = xn
    val () = !p_h := hr + 1
    val () = !p_tl := tl and () = !p_tr := tr
  in
    fold@ {a} (xn); xn
  end // end of [if]
end // end of [avltree_rjoin]

(* ****** ****** *)

fn{a:t@ype}
avltree_join
  {hl,hr:nat} (
  xn: B_node (a)
, tl: avltree (a, hl), tr: avltree (a, hr)
) :<> [h:int | hl <= h; hr <= h; h <= max(hl,hr)+1] avltree (a, h) = let
  val hl = avltree_height tl: int hl
  and hr = avltree_height tr: int hr
in
  if hl >= hr
    then avltree_ljoin<a> (xn, tl, tr) else avltree_rjoin<a> (xn, tl, tr)
  // end of [if]
end // end of [avltree_join]

(* ****** ****** *)

fn{a:t@ype}
avltree_concat
  {hl,hr:nat} (
  tl: avltree (a, hl), tr: avltree (a, hr)
) :<> [h:nat | h <= max(hl,hr)+1] avltree (a, h) =
  case+ (tl, tr) of
  | (~E (), _) => tr
  | (_, ~E ()) => tl
  | (_, _) =>> let
      var tr = tr
      val xn_min = avltree_takeout_min<a> (tr)
    in
      avltree_join<a> (xn_min, tl, tr)
    end // end of [_, _]
// end of [avltree_concat]

(* ****** ****** *)

fun{a:t@ype}
avltree_split_at {h:nat} .<h>. (
  t: avltree (a, h)
, x0: a
, xn: &B_node(a)? >> opt (B_node(a), i>0)
, tl0: &avltree0 >> avltree (a, hl)
, tr0: &avltree0 >> avltree (a, hr)
, cmp: cmp a
) :<> #[i:two; hl,hr:nat | hl <= h; hr <= h] int i =
  case+ t of
  | B (!p_h, !p_x, !p_tl, !p_tr) => let
      val x = !p_x
      val tl = !p_tl and tr = !p_tr
      val t = B_node_make {a}
        (view@(!p_h), view@(!p_x), view@(!p_tl), view@(!p_tr) | t)
      val sgn = compare_elt_elt<a> (x0, x, cmp)
    in
      if sgn < 0 then let
        val i = avltree_split_at<a> (tl, x0, xn, tl0, tr0, cmp)
      in
        tr0 := avltree_join<a> (t, tr0, tr); i
      end else if sgn > 0 then let
        val i = avltree_split_at<a> (tr, x0, xn, tl0, tr0, cmp)
      in
        tl0 := avltree_join<a> (t, tl, tl0); i
      end else let
        val () = xn := t
        val ()= tl0 := tl and () = tr0 := tr
        prval () = opt_some {B_node(a)} (xn)
      in
        1 // [x0] found in t
      end // end of [if]
    end // end of [B]
  | ~E () => let
      val () = tl0 := E () and () = tr0 := E ()
      prval () = opt_none {B_node(a)} (xn)
    in
      0 // [x0] not found in t
    end // end of [E]
// end of [avltree_split_at]

(* ****** ****** *)

implement{a}
linset_choose
  (xs, x0) = case+ xs of
  | B (_(*h*), x, _(*tl*), _(*tr*)) => let
      prval () = fold@ (xs)
      val () = x0 := x
      prval () = opt_some {a} (x0)
    in
      true
    end // end of [B]
  | E () => let
      prval () = fold@ (xs)
      prval () = opt_none {a} (x0)
    in
      false
    end // end of [E]
// end of [linset_choose]

implement{a}
linset_takeout
  (xs, x0) = case+ xs of
  | ~B (_(*h*), x, tl, tr) => let
      val () = x0 := x
      val () = xs := avltree_concat<a> (tl, tr)
      prval () = opt_some {a} (x0)
    in
      true
    end // end of [E]
  | E () => let
      prval () = fold@ (xs)
      prval () = opt_none {a} (x0)
    in
      false
    end // end of [E]
// end of [linset_takeout]

(* ****** ****** *)

fn{} B_node_free_opt
  {a:t@ype} {i:nat} (
  xn: opt (B_node(a), i > 0), i: int i
) :<> void = let
  viewtypedef T = B_node(a)
in
  if i > 0 then let
    prval () = opt_unsome {T} (xn)
    val+ B _ = xn
  in
    free@ {a} {0,0} (xn)
  end else let
    prval () = opt_unnone {T} (xn)
    prval () = cleanup_top {T} (xn)
  in
    (*nothing*)
  end // end of [if]
end // end of [B_node_free_opt]

(* ****** ****** *)

implement{a}
linset_union
  (t1, t2, cmp) = union (t1, t2) where {
  fun union {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> [h:nat] avltree (a, h) = begin
    case+ (t1, t2) of
    | (~E (), _) => t2
    | (_, ~E ()) => t1
    | (_, _) =>> let
        val+ B (!p_h1, !p_x1, !p_t1l, !p_t1r) = t1
        val x1 = !p_x1
        val t1l = !p_t1l and t1r = !p_t1r
        var xn: B_node (a)
        var t2l0: avltree0 and t2r0: avltree0
        val i = avltree_split_at<a> (t2, x1, xn, t2l0, t2r0, cmp)
        val () = B_node_free_opt<> {a} (xn, i)
        val t12l = union (t1l, t2l0) and t12r = union (t1r, t2r0)
        val t1 = B_node_make {a} (
          view@(!p_h1), view@(!p_x1), view@(!p_t1l), view@(!p_t1r) | t1
        ) // end of [val]
      in
        avltree_join<a> (t1, t12l, t12r)
      end // end of [_, _]
    end // end of [uni]
  // end of [union]
} // end of [linset_union]

(* ****** ****** *)

implement{a}
linset_intersect
  (t1, t2, cmp) = inter (t1, t2) where {
  fun inter {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> [h:nat] avltree (a, h) = begin
    case+ (t1, t2) of
    | (~E (), _) => (linset_free (t2); E ())
    | (_, ~E ()) => (linset_free (t1); E ())
    | (_, _) =>> let
        val+ ~B (_(*h1*), x1, t1l, t1r) = t1
        stadef T = B_node(a)
        var xn: T?
        var t2l0: avltree0 and t2r0: avltree0
        val+ i = avltree_split_at<a> (t2, x1, xn, t2l0, t2r0, cmp)
        val t12l = inter (t1l, t2l0) and t12r = inter (t1r, t2r0)
      in
        if i = 0 then let
          prval () = opt_unnone {T} (xn)
        in
          avltree_concat (t12l, t12r)
        end else let
          prval () = opt_unsome {T} (xn)
        in
          avltree_join<a> (xn, t12l, t12r)
        end // end of [if]
      end // end of [_, _]
    end // end of [inter]
  // end of [inter]
} // end of [linset_intersect]

(* ****** ****** *)

implement{a}
linset_diff
  (t1, t2, cmp) = diff (t1, t2) where {
  fun diff {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> [h:nat] avltree (a, h) = begin
    case+ (t1, t2) of
    | (~E (), _) => (linset_free (t2); E ())
    | (_, ~E ()) => t1
    | (_, _) =>> let
        val+ B (!p_h1, !p_x1, !p_t1l, !p_t1r) = t1
        val x1 = !p_x1
        val t1l = !p_t1l and t1r = !p_t1r
        var xn: B_node(a)
        var t2l0: avltree0 and t2r0: avltree0
        val i = avltree_split_at<a> (t2, x1, xn, t2l0, t2r0, cmp)
        val () = B_node_free_opt {a} (xn, i)
        val t12l = diff (t1l, t2l0) and t12r = diff (t1r, t2r0)
      in
        if i > 0 then let
          val () = free@ {a} {0,0} (t1) in avltree_concat (t12l, t12r)
        end else let
          val t1 = B_node_make {a} (
            view@(!p_h1), view@(!p_x1), view@(!p_t1l), view@(!p_t1r) | t1
          ) // end of [val]
        in
          avltree_join<a> (t1, t12l, t12r)
        end (* end of [if] *)
      end // end of [_, _]
    end // end of [diff]
  // end of [diff]
} // end of [linset_diff]

(* ****** ****** *)

implement{a}
linset_symdiff
  (t1, t2, cmp) = symdiff (t1, t2) where {
  fun symdiff {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> [h:nat] avltree (a, h) = begin
    case+ (t1, t2) of
    | (~E (), _) => t2
    | (_, ~E ()) => t1
    | (_, _) =>> let
        val+ B (!p_h1, !p_x1, !p_t1l, !p_t1r) = t1
        val x1 = !p_x1
        val t1l = !p_t1l and t1r = !p_t1r
        var xn: B_node(a)
        var t2l0: avltree0 and t2r0: avltree0
        val i = avltree_split_at<a> (t2, x1, xn, t2l0, t2r0, cmp)
        val () = B_node_free_opt {a} (xn, i)
        val t12l = symdiff (t1l, t2l0) and t12r = symdiff (t1r, t2r0)
      in
        if i > 0 then let
          val () = free@ {a} {0,0} (t1) in avltree_concat (t12l, t12r)
        end else let
          val t1 = B_node_make {a} (
            view@(!p_h1), view@(!p_x1), view@(!p_t1l), view@(!p_t1r) | t1
          ) // end of [val]
        in
          avltree_join<a> (t1, t12l, t12r)
        end (* end of [if] *)
      end // end of [_, _]
    end // end of [symdiff]
  // end of [diff]
} // end of [linset_symdiff]

(* ****** ****** *)

implement{a}
linset_is_subset
  (xs1, xs2, cmp) = let
  fun is_subset
    {h1:nat} .<h1>. (
    t1: !avltree (a, h1), t2: !set(a), cmp: cmp(a)
  ) :<cloref> bool =
    case+ t1 of
    | B (_, x, !p_t1l, !p_t1r) => let
        val test = linset_is_member (t2, x, cmp)
      in
        if test then (
          if is_subset (!p_t1l, t2, cmp) then let
            val res = is_subset (!p_t1r, t2, cmp) in fold@ t1; res
          end else (fold@ t1; false) // end of [if]
        ) else (fold@ t1; false) // end of [if]
      end // end of [B]
    | E _ => (fold@ (t1); true)
  // end of [is_subset]
in
  is_subset (xs1, xs2, cmp)
end // end of [linset_is_supset]

implement{a}
linset_is_supset
  (xs1, xs2, cmp) = linset_is_subset<a> (xs2, xs1, cmp)
// end of [linset_is_supset]

implement{a}
linset_is_equal
  (xs1, xs2, cmp) =
  if linset_is_supset<a> (xs1, xs2, cmp)
    then linset_is_subset<a> (xs1, xs2, cmp) else false
  // end of [if]
(* end of [linset_is_equal] *)

(* ****** ****** *)

implement{a}
linset_foreach_funenv {v} {vt}
  (pf | xs, f, env) = foreach (pf | xs, env) where {
  fun foreach {h:nat} .<h>.
    (pf: !v | t: !avltree (a, h), env: !vt):<cloref> void =
    case+ t of
    | B (
        _(*h*), x, !p_tl, !p_tr
      ) => let
        val () = foreach (pf | !p_tl, env)
        val () = f (pf | x, env)
        val () = foreach (pf | !p_tr, env)
      in
        fold@ (t)
      end // end of [B]
    | E () => fold@ (t)
  // end of [foreach]
} // end of [linset_foreach_funenv]

implement{a}
linset_foreach_fun
  (xs, f) = let
//
  val f = coerce (f) where {
    extern castfn coerce
      (f: (a) -<fun> void):<> (!unit_v | a, !ptr) -<fun> void
  } // end of [val]
//
  prval pfu = unit_v ()
  val () = linset_foreach_funenv<a> {unit_v} {ptr} (pfu | xs, f, null)
  prval unit_v () = pfu
//  
in
  // nothing
end // end of [linset_foreach_fun]

(* ****** ****** *)

implement{a}
linset_foreach_vclo {v}
  (pf | m, f) = foreach (pf | m, f) where {
  fun foreach {h:nat} .<h>. (
    pf: !v | t: !avltree (a, h), f: &(!v | a) -<clo> void
  ) :<> void =
    case+ t of
    | B (
        _(*h*), x, !p_tl, !p_tr
      ) => let
        val () = foreach (pf | !p_tl, f)
        val () = f (pf | x)
        val () = foreach (pf | !p_tr, f)
      in
        fold@ (t)
      end // end of [B]
    | E () => fold@ (t)
  // end of [foreach]
} // end of [linset_foreach_vclo]

implement{a}
linset_foreach_cloref (m, f) = let
  val f = __cast (f) where { extern castfn __cast
    (f: (a) -<cloref> void):<> (!unit_v | a) -<cloref> void
  } // end of [val]
  typedef clo_type = (!unit_v | a) -<clo> void
  val (vbox pf_f | p_f) = cloref_get_view_ptr {clo_type} (f)
  prval pfu = unit_v ()
  val () = $effmask_ref
    (linset_foreach_vclo<a> {unit_v} (pfu | m, !p_f))
  prval unit_v () = pfu
in
  // empty
end // end of [linset_foreach_cloref]

(* ****** ****** *)

implement{a}
linset_listize (xs) = let
  viewtypedef res_t = List_vt (a)
  fun aux {h:nat} .<h>.
    (t: !avltree (a, h), res: res_t):<> res_t =
    case+ t of
    | B (_(*h*), x, !p_tl, !p_tr) => let
        val res = aux (!p_tr, res)
        val res = list_vt_cons (x, res)
        val res = aux (!p_tl, res)
        prval () = fold@ (t)
      in
        res
      end // end of [B]
    | E () => (fold@ (t); res)
  // end of [aux]
in
  aux (xs, list_vt_nil)
end // end of [linset_listize]

(* ****** ****** *)

implement{a}
linset_listize_free (xs) = let
  viewtypedef res_t = List_vt (a)
  fun aux {h:nat} .<h>.
    (t: avltree (a, h), res: res_t):<> res_t =
    case+ t of
    | ~B (_(*h*), x, tl, tr) => let
        val res = aux (tr, res)
        val res = list_vt_cons (x, res)
        val res = aux (tl, res)
      in
        res
      end // end of [B]
    | ~E () => res // end of [E]
  // end of [aux]
in
  aux (xs, list_vt_nil)
end // end of [linset_listize_free]

(* ****** ****** *)

(* end of [linset_avltree.dats] *)
