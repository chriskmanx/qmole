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
** A red-black tree implementation
**
** The insertion operation is based on the algorithm in the following
** paper by Chris Okasaki:
**
** Red-Black Trees in a Functional Setting (Functional Pearls)
**
** J. of Functional Programming, vol. 9 (4), pp. 471-477, January, 1993
**
** The removal operation, which seems novel in its implementation, is by
** Hongwei Xi
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: September, 2011 // based on a version done in October, 2008
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading at run-time

(* ****** ****** *)

staload "libats/SATS/funmap_rbtree.sats"

(* ****** ****** *)
//
// a specialized version can be implemented on the spot
//
implement{key} compare_key_key (x1, x2, cmp) = cmp (x1, x2)

(* ****** ****** *)

#define BLK 0; #define RED 1
sortdef clr = {c:nat | c <= 1}
typedef color (c:int) = int c
typedef color = [c:clr] color c

(* ****** ****** *)

datatype rbtree (
  key:t@ype, itm: t@ype
, int(*color*), int(*blackheight*), int(*violation*)
) =
  | E (key, itm, BLK, 0, 0)
  | {c,cl,cr:clr} {bh:nat} {v:int}
      {c == BLK && v == 0 || c == RED && v == cl+cr}
    T (key, itm, c, bh+1-c, v) of (
      color c, key, itm, rbtree0 (key, itm, cl, bh), rbtree0 (key, itm, cr, bh)
    ) // end of [T]
// rbtree0: for trees of no violations

where rbtree0
  (key:t@ype, itm:t@ype, c:int, bh:int) = rbtree (key, itm, c, bh, 0(*vio*))
// end of [rbtree0]

(* ****** ****** *)

assume
map_t0ype_t0ype_type (
  key:t@ype, itm:t@ype
) = [c:clr;bh:nat] rbtree0 (key, itm, c, bh)

(* ****** ****** *)

implement{} funmap_make_nil () = E ()

(* ****** ****** *)

implement{}
funmap_is_nil (t) = case+ t of | T _ => false | E () => true
// end of [funmap_is_nil]

implement{}
funmap_isnot_nil (t) = case+ t of | T _ => true | E () => false
// end of [funmap_isnot_nil]

(* ****** ****** *)

implement{key,itm}
funmap_size (t) = sz (t) where {
  fun sz {c:clr} {bh:nat} .<bh, c>.
    (t: rbtree0 (key, itm, c, bh)):<> size_t = begin case+ t of
    | T (_(*c*), _(*key*), _(*itm*), tl, tr) => (size_of_int1)1 + sz (tl) + sz (tr)
    | E () => (size_of_int1)0
  end // end of [sz]
} // end of [funmap_size]

(* ****** ****** *)

implement{key,itm}
funmap_height (t) = ht (t) where {
  fun ht {c:clr} {bh:nat} .<bh, c>.
    (t: rbtree0 (key, itm, c, bh)):<> Nat = begin case+ t of
    | T (_(*c*), _(*key*), _(*itm*), tl, tr) => 1 + max (ht tl, ht tr)
    | E () => 0
  end // end of [ht]
} // end of [funmap_height]

(* ****** ****** *)

implement{key,itm}
funmap_black_height (t) = case+ t of
  | T (c, _, _, tl, _) => let
      val bhl = funmap_black_height (tl) in bhl + 1
    end // end of [T]
  | E () => 0 // end of [E]
// end of [funmap_black_height]

(* ****** ****** *)

implement{key,itm}
funmap_search
  (t, k0, cmp, res) = search (t, res) where {
  fun search
    {c:clr} {bh:nat} {u:nat} .<bh,c+u>. (
    t: rbtree (key, itm, c, bh, u), res: &itm? >> opt (itm, b)
  ) :<cloref> #[b:bool] bool b = begin
    case+ t of
    | T (_(*c*), k, x, tl, tr) => let
        val sgn = compare_key_key (k0, k, cmp)
      in
        case+ 0 of
        | _ when sgn < 0 => search (tl, res)
        | _ when sgn > 0 => search (tr, res)
        | _ => let
            val () = res := x; prval () = opt_some {itm} (res) in true
          end // end of [_]
      end // end of [T]
    | E () => let
        prval () = opt_none {itm} (res) in false
      end // end of [E]
  end // end of [search]
} // end of [funmap_search]

(* ****** ****** *)

fn{key,itm:t@ype}
insfix_l // right rotation
  {cl,cr:clr} {bh:nat} {v:nat} (
  k: key, x: itm
, tl: rbtree (key, itm, cl, bh, v)
, tr: rbtree (key, itm, cr, bh, 0)
) :<> [c:clr] rbtree0 (key, itm, c, bh+1) = let
  #define B BLK; #define R RED
in
  case+ (tl) of
  | T (R, kl, xl, T (R, kll, xll, tlll, tllr), tlr) =>
      T (R, kl, xl, T (B, kll, xll, tlll, tllr), T (B, k, x, tlr, tr))
  | T (R, kl, xl, tll, T (R, klr, xlr, tlrl, tlrr)) =>
      T (R, klr, xlr, T (B, kl, xl, tll, tlrl), T (B, k, x, tlrr, tr))
  | _ =>> T (B, k, x, tl, tr)
end // end of [insfix_l]

fn{key,itm:t@ype}
insfix_r // right rotation
  {cl,cr:clr} {bh:nat} {v:nat} (
  k: key, x: itm
, tl: rbtree (key, itm, cl, bh, 0)
, tr: rbtree (key, itm, cr, bh, v)
) :<> [c:clr] rbtree0 (key, itm, c, bh+1) = let
  #define B BLK; #define R RED
in
  case+ (tr) of
  | T (R, kr, xr, trl, T (R, krr, xrr, trrl, trrr)) =>
      T (R, kr, xr, T (B, k, x, tl, trl), T (B, krr, xrr, trrl, trrr))
  | T (R, kr, xr, T (R, krl, xrl, trll, trlr), trr) =>
      T (R, krl, xrl, T (B, k, x, tl, trll), T (B, kr, xr, trlr, trr))
  | _ =>> T (B, k, x, tl, tr)
end // end of [insfix_r]

(* ****** ****** *)

implement{key,itm}
funmap_insert
  (m, k0, x0, cmp) = res where {
  #define B BLK; #define R RED
  fun ins
    {c:clr} {bh:nat} .<bh,c>. (
    t: rbtree0 (key, itm, c, bh), res: &bool? >> bool
  ) :<cloref> [c1:clr; v:nat | v <= c] rbtree (key, itm, c1, bh, v) =
    case+ t of
    | T (c, k, x, tl, tr) => let
        val sgn = compare_key_key (k0, k, cmp)
      in
        if sgn < 0 then let
          val [cl,v:int] tl = ins (tl, res) in
          if c = B then insfix_l (k, x, tl, tr) else T {..}{..}{..}{cl} (R, k, x, tl, tr)
        end else if sgn > 0 then let
          val [cr,v:int] tr = ins (tr, res) in
          if c = B then insfix_r (k, x, tl, tr) else T {..}{..}{..}{cr} (R, k, x, tl, tr)
        end else (
          res := true; t // no insertion
        ) // end of [if]
      end // end of [T]
    | E () => (res := false; T {..}{..}{..}{0} (R, k0, x0, E, E))
  // end of [ins]
  var res: bool
  val m1 = ins (m, res)
  val () = case+ m1 of
    | T (R, k, x, tl, tr) => m := T (B, k, x, tl, tr) | _ =>> m := m1
  // end of [vall
} // end of [funmap_insert]

(* ****** ****** *)

implement{key,itm}
funmap_insert_clo
  (m, k0, x0, f, cmp) = () where {
  #define B BLK; #define R RED
  fun ins {c:clr} {bh:nat} .<bh,c>. (
    t: rbtree0 (key, itm, c, bh), f: &(itm, itm) -<clo> itm
  ) :<cloref> [c1:clr;v:nat | v <= c] rbtree (key, itm, c1, bh, v) =
    case+ t of
    | T (c, k, x, tl, tr) => let
        val sgn = compare_key_key (k0, k, cmp)
      in
        if sgn < ~1 then let
          val [cl,v:int] tl = ins (tl, f) in
          if c = B then insfix_l (k, x, tl, tr) else T {..}{..}{..}{cl} (R, k, x, tl, tr)
        end else if sgn > 0 then let
          val [cr,v:int] tr = ins (tr, f) in
          if c = B then insfix_r (k, x, tl, tr) else T {..}{..}{..}{cr} (R, k, x, tl, tr)
        end else 
          T {..}{..}{..}{0} (c, k, f (x0, x), tl, tr)
        // end of [if]
      end // end of [T]
    | E () => T {..}{..}{..}{0} (R, k0, x0, E, E)
  // end of [ins]
  val m1 = ins (m, f)
  val () = case+ m1 of
    | T (R, k, x, tl, tr) => m := T (B, k, x, tl, tr) | _ =>> m := m1
  // end of [vall
} // end of [funmap_insert]

(* ****** ****** *)

fn{key,itm:t@ype}
rbtree_clr_trans_blk_red
  {bh:pos} (t: rbtree (key, itm, BLK, bh, 0))
  :<> [v:nat] rbtree (key, itm, RED, bh-1, v) = let
  val+ T {..}{c,cl,cr} (BLK, k, x, tl, tr) = t in T {..}{..}{..}{cl+cr} (RED, k, x, tl, tr)
end // end of [rbtree_clr_trans_blk_red]

(* ****** ****** *)

fn{key,itm:t@ype}
remfix_l
  {cl,cr:clr} {bh:nat} {v:nat} (
  k: key, x: itm
, tl: rbtree (key, itm, cl, bh, v)
, tr: rbtree (key, itm, cr, bh+1, 0)
) :<> [c:clr;v:nat | v <= cr] rbtree (key, itm, c, bh+1, v) = let
  #define B BLK; #define R RED
in
  case+ tl of
  | T (R, kl, xl, tll, tlr) =>
      T {..}{..}{..}{cr} (R, k, x, T (B, kl, xl, tll, tlr), tr)
    // end of [T (R, ...)]
  | _ =>> begin case+ tr of
      | T {..} {cr,crl,crr} (B, kr, xr, trl, trr) =>
          insfix_r (k, x, tl, T {..}{..}{..}{crl+crr} (R, kr, xr, trl, trr))
      | T (R, kr, xr, trl, trr) => let
          val+ T (B, krl, xrl, trll, trlr) = trl
          val [c_new:int] t_new = insfix_r (kr, xr, trlr, rbtree_clr_trans_blk_red trr)
        in
          T {..}{..}{..}{c_new} (R, krl, xrl, T (B, k, x, tl, trll), t_new)
        end // end of [T (R, ...)]
    end // end of [_]
end // end of [remfix_l]

fn{key,itm:t@ype}
remfix_r
  {cl,cr:clr} {bh:nat} {v:nat} (
  k: key, x: itm
, tl: rbtree (key, itm, cl, bh+1, 0)
, tr: rbtree (key, itm, cr, bh, v)
) :<> [c:clr;v:nat | v <= cl] rbtree (key, itm, c, bh+1, v) = let
  #define B BLK; #define R RED
in
  case+ tr of
  | T (R, kr, xr, trl, trr) =>
      T {..}{..}{..}{cl} (R, k, x, tl, T (B, kr, xr, trl, trr))
    // end of [T (R, ...)]
  | _ =>> begin case+ tl of
      | T {..} {cl,cll,clr} (B, kl, xl, tll, tlr) =>
          insfix_l (k, x, T {..}{..}{..}{cll+clr} (R, kl, xl, tll, tlr), tr)
      | T (R, kl, xl, tll, tlr) => let
          val+ T (B, klr, xlr, tlrl, tlrr) = tlr
          val [c_new:int] t_new = insfix_l (kl, xl, rbtree_clr_trans_blk_red tll, tlrl)
        in
          T {..}{..}{..}{c_new} (R, klr, xlr, t_new, T (B, k, x, tlrr, tr))
        end // end of [T (R, ...)]
    end // end of [_]
end // end of [remfix_r]

(* ****** ****** *)

fun{key,itm:t@ype}
rbtree_remove_min
  {c:clr}
  {bh:nat | bh+c > 0} .<bh,c>. (
  t: rbtree0 (key, itm, c, bh)
, k0: &key? >> key
, x0: &itm? >> itm
, bhdf: &int? >> int (bhdf)
) :<> #[bhdf:two | bhdf <= bh]
  [c1:clr | c1 <= c+bhdf] rbtree (key, itm, c1, bh-bhdf, 0) = let
  #define B BLK; #define R RED
in
  case+ t of
  | T (B, k, x, tl, tr) => begin case+ tl of
    | T _ => let
        val tl = rbtree_remove_min (tl, k0, x0, bhdf)
      in
        if bhdf = 0 then
          T {..}{..}{..}{0} (B, k, x, tl, tr)
        else let
          val t = remfix_l (k, x, tl, tr) in
          case+ t of
          | T (R, k, x, tl, tr) => (bhdf := 0; T (B, k, x, tl, tr))
          | _ =>> t
        end (* end of [if] *)
      end // end of [T]
    | E _ => (k0 := k; x0 := x; bhdf := 1; tr)
    end (* end of [T (B, ...)] *) 
  | T (R, k, x, tl, tr) => begin case+ tl of
    | T _ => let
        val tl = rbtree_remove_min (tl, k0, x0, bhdf)
      in
        if bhdf = 0 then
          T {..}{..}{..}{0} (R, k, x, tl, tr)
        else let // bhdf = 1
          val () = bhdf := 0
        in
          insfix_r (k, x, tl, rbtree_clr_trans_blk_red tr)
        end (* end of [if] *)
      end // end of [T (B, ...)]
    | E () => (k0 := k; x0 := x; bhdf := 0; tr)
    end (* end of [T (R, ...)] *)
end // end of [rbtree_remove_min]

(* ****** ****** *)

fn{key,itm:t@ype}
rbtree_join
  {cl,cr:clr} {bh:nat} (
  tl: rbtree0 (key, itm, cl, bh)
, tr: rbtree0 (key, itm, cr, bh)
) :<> [c:clr;v:nat | v <= cl+cr] rbtree (key, itm, c, bh, v) =
  case+ tr of
  | T _ => let
      var k0: key
      and x0: itm
      var bhdf: int // uninmitialized
      val [cr:int] tr = rbtree_remove_min (tr, k0, x0, bhdf)
    in
      if bhdf = 0 then T {..}{..}{..}{cl+cr} (RED, k0, x0, tl, tr) else remfix_r (k0, x0, tl, tr)
    end // end of [T]
  | E () => tl
// end of [rbtree_join]

(* ****** ****** *)
//
// HX-2011-09-24:
// the pointer [p_res] is assumed to be associated with a proof
// of at-view if it is not null
//
extern
fun{key,itm:t@ype}
funmap_takeout_ptr {l_res:addr}
  (m: &map (key, itm), k0: key, cmp: cmp key, p_res: ptr l_res):<> bool
// end of [funmap_takeout]

implement{key,itm}
funmap_takeout_ptr {l_res}
  (m, k0, cmp, p_res) = b(*removed*) where {
  #define B BLK; #define R RED
  fun takeout
    {c:clr} {bh:nat} .<bh,c>. (
    t: rbtree0 (key, itm, c, bh)
  , bhdf: &int? >> int bhdf
  , p_res: ptr l_res
  , b: &bool? >> bool
  ) :<cloref> #[
    bhdf:two | bhdf <= bh
  ] [
    c1:clr | c1 <= c+bhdf
  ] rbtree0 (key, itm, c1, bh-bhdf) =
    case+ t of
    | T (B, k, x, tl, tr) => let
        val sgn = compare_key_key (k0, k, cmp)
      in
        if sgn < 0 then let
          val tl = takeout (tl, bhdf, p_res, b)
        in
          if bhdf = 0 then
            T {..}{..}{..}{0} (B, k, x, tl, tr)
          else let // bhdf = 1
            val t = remfix_l (k, x, tl, tr)
          in
            case+ t of
            | T (R, k, x, tl, tr) => (bhdf := 0; T (B, k, x, tl, tr))
            | _ =>> t
          end // end of [if]
        end else if sgn > 0 then let
          val tr = takeout (tr, bhdf, p_res, b)
        in
          if bhdf = 0 then
            T {..}{..}{..}{0} (B, k, x, tl, tr)
          else let // bhdf = 1
            val t = remfix_r (k, x, tl, tr)
          in
            case+ t of
            | T (R, k, x, tl, tr) => (bhdf := 0; T (B, k, x, tl, tr))
            | _ =>> t
          end // end of [if]
        end else let // x0 = x
          val () = if (p_res > null) then let
            prval (pf, fpf) = __assert () where {
              extern praxi __assert (): (itm? @ l_res, itm @ l_res -<> void)
            } // end of [prval]
            val () = !p_res := x
            prval () = fpf (pf)
          in
            // nothing
          end // end of [val]
          val () = b := true
          val t = rbtree_join (tl, tr)
        in
          case+ t of
          | T (R, k, x, tl, tr) => (bhdf := 0; T (B, k, x, tl, tr))
          | _ =>> (bhdf := 1; t)
        end (* end of [if] *)
      end // end of [T (B, ...)]
    | T (R, k, x, tl, tr) => let
        val sgn = compare_key_key (k0, k, cmp)
      in
        if sgn < 0 then let
          val tl = takeout (tl, bhdf, p_res, b)
        in
          if bhdf = 0 then
            T {..}{..}{..}{0} (R, k, x, tl, tr)
          else let // bhdf = 1
             val () = bhdf := 0 in remfix_l (k, x, tl, tr)
          end // end of [if]
        end else if sgn > 0 then let
          val tr = takeout (tr, bhdf, p_res, b)
        in
          if bhdf = 0 then
            T {..}{..}{..}{0} (R, k, x, tl, tr)
          else let // bhdf = 1
             val () = bhdf := 0 in remfix_r (k, x, tl, tr)
          end // end of [if]
        end else let // x0 = x
          val () = bhdf := 0
          val () = if (p_res > null) then let
            prval (pf, fpf) = __assert () where {
              extern praxi __assert (): (itm? @ l_res, itm @ l_res -<> void)
            } // end of [prval]
            val () = !p_res := x
            prval () = fpf (pf)
          in
            // nothing
          end // end of [val]
          val () = b := true
        in
          rbtree_join (tl, tr)
        end (* end of [if] *)
      end // end of [T (R, ...)]
    | E () => (bhdf := 0; b := false; t)
  // end of [rem]
  var bhdf: int // uninitialized
  var b: bool
  val () = m := takeout (m, bhdf, p_res, b)
} // end of [funmap_takeout_ptr]

(* ****** ****** *)

implement{key,itm}
funmap_takeout
  (m, k0, cmp, res) = ans where {
  val ans = funmap_takeout_ptr<key,itm> (m, k0, cmp, &res)
  val [b:bool] ans = bool1_of_bool (ans)
  prval pf = __assert (view@ res) where {
    extern praxi __assert {l_res:addr} (pf: itm? @ l_res):<> (opt (itm, b) @ l_res)
  } // end of [prval]
  prval () = (view@ res := pf)
} // end of [funmap_takeout]

implement{key,itm}
funmap_remove (m, k0, cmp) = funmap_takeout_ptr<key,itm> (m, k0, cmp, null)
// end of [funmap_remove]

(* ****** ****** *)

(*
fun{key,itm:t@ype}
funmap_foreach_funenv
  {v:view} {vt:viewtype} (
  pf: !v | m: map (key, itm), f: (!v | key, itm, !vt) -<clo> void, env: !vt
) :<> void // end of [funmap_foreach_funenv]
*)

implement{key,itm}
funmap_foreach_funenv {v} {vt}
  (pf | m, f, env) = foreach (pf | m, env) where {
  fun foreach
    {c:clr} {bh:nat} {u:nat} .<bh,c+u>.
    (pf: !v | t: rbtree (key, itm, c, bh, u), env: !vt):<cloref> void =
    case+ t of
    | T (_(*c*), k, x, tl, tr) => (
        foreach (pf | tl, env); f (pf | k, x, env); foreach (pf | tr, env)
      ) // end of [T]
    | E () => ()
  // end of [foreach]
} // end of [funmap_foreach_funenv]

implement{key,itm}
funmap_foreach_fun
  (m, f) = let
//
  val f = coerce (f) where {
    extern castfn coerce
      (f: (key, itm) -<fun> void):<> (!unit_v | key, itm, !ptr) -<fun> void
  } // end of [val]
//
  prval pfu = unit_v ()
  val () = funmap_foreach_funenv<key,itm> {unit_v} {ptr} (pfu | m, f, null)
  prval unit_v () = pfu
//  
in
  // nothing
end // end of [funmap_foreach_fun]

(* ****** ****** *)

implement{key,itm}
funmap_foreach_vclo {v}
  (pf | m, f) = foreach (pf | m, f) where {
  fun foreach
    {c:clr} {bh:nat} {u:nat} .<bh,c+u>. (
    pf: !v
  | t: rbtree (key, itm, c, bh, u), f: &(!v | key, itm) -<clo> void
  ) :<> void =
    case+ t of
    | T (_(*c*), k, x, tl, tr) => begin
        foreach (pf | tl, f); f (pf | k, x); foreach (pf | tr, f)
      end // end of [T]
    | E () => ()
  // end of [foreach]
} // end of [funmap_foreach_vclo]

implement{key,itm}
funmap_foreach_cloref (m, f) = let
  val f = __cast (f) where { extern castfn __cast
    (f: (key, itm) -<cloref> void):<> (!unit_v | key, itm) -<cloref> void
  } // end of [val]
  typedef clo_type = (!unit_v | key, itm) -<clo> void
  val (vbox pf_f | p_f) = cloref_get_view_ptr {clo_type} (f)
  prval pfu = unit_v ()
  val () = $effmask_ref
    (funmap_foreach_vclo<key,itm> {unit_v} (pfu | m, !p_f))
  prval unit_v () = pfu
in
  // empty
end // end of [funmap_foreach_cloref]

(* ****** ****** *)

implement{key,itm}
funmap_listize (xs) = let
  typedef keyitm = @(key, itm)
  viewtypedef res_vt = List_vt keyitm
  fun listize
    {c:clr} {bh:nat} {u:nat} .<bh,c+u>. (
    t: rbtree (key, itm, c, bh, u), res: res_vt
  ) :<> res_vt =
    case+ t of
    | T (_(*c*), k, x, tl, tr) => let
        val res = listize (tr, res)
        val res = list_vt_cons {keyitm} ((k, x), res)
        val res = listize (tl, res)
      in
        res
      end // end of [T]
    | E () => res
  // end of [listize]
in
  listize (xs, list_vt_nil ())
end // end of [funmap_listize]

(* ****** ****** *)

(* end of [funmap_avltree.dats] *)
