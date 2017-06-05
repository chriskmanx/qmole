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
** Time: September, 2011
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading at run-time

(* ****** ****** *)

staload "libats/SATS/linmap_rbtree.sats"

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

sortdef t0p = t@ype
sortdef vt0p = viewt@ype

(* ****** ****** *)

dataviewtype rbtree (
  key:t@ype, itm: viewt@ype
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
  (key:t@ype, itm:viewt@ype, c:int, bh:int) = rbtree (key, itm, c, bh, 0(*vio*))
// end of [rbtree0]

(* ****** ****** *)

prfn
rbtree_get_color
  {key:t0p;itm:vt0p}
  {c:clr} {bh:int} {v:int}
  (t: !rbtree (key, itm, c, bh, v)):<> int (c) =
  case+ t of
  | T (c, _, _, _, _) => (fold@ (t); c) | E () => (fold@ (t); BLK)
// end of [rbtree_get_color]

(* ****** ****** *)

assume
map_t0ype_viewt0ype_type
  (key:t0p, itm:vt0p) = [c:clr;bh:nat] rbtree0 (key, itm, c, bh)
// end of [map_t0ype_viewt0ype_type]

(* ****** ****** *)

implement{} linmap_make_nil () = E ()

(* ****** ****** *)

implement{}
linmap_is_nil (t) =
  case+ t of | T _ => (fold@ t; false) | E () => (fold@ t; true)
// end of [linmap_is_nil]

implement{}
linmap_isnot_nil (t) =
  case+ t of | T _ => (fold@ t; true) | E () => (fold@ t; false)
// end of [linmap_isnot_nil]

(* ****** ****** *)

implement{key,itm}
linmap_size (t) = sz (t) where {
  fun sz
    {c:clr} {bh:nat} {v:nat} .<bh,c+v>.
    (t: !rbtree (key, itm, c, bh, v)):<> size_t =
    case+ t of
    | T (_(*c*), _(*key*), _(*itm*), !ptl, !ptr) => let
       val sz = (size_of_int1)1 + sz (!ptl) + sz (!ptr) in (fold@ t; sz)
      end // end of [B]
    | E () => (fold@ t; size_of_int1(0))
  // end of [sz]
} // end of [linmap_size]

(* ****** ****** *)

implement{key,itm}
linmap_height (t) = ht (t) where {
  fun ht
    {c:clr} {bh:nat} {v:nat} .<bh,c+v>.
    (t: !rbtree (key, itm, c, bh, v)):<> Nat =
    case+ t of
    | T (_(*c*), _(*key*), _(*itm*), !ptl, !ptr) => let
       val ht = 1 + max (ht (!ptl), ht (!ptr)) in (fold@ t; ht)
      end // end of [B]
    | E () => (fold@ t; 0)
  // end of [ht]
} // end of [linmap_height]

(* ****** ****** *)

implement{key,itm}
linmap_search
  (t, k0, cmp, res) = search (t, res) where {
  fun search
    {c:clr} {bh:nat} .<bh,c>. (
      t: !rbtree0 (key, itm, c, bh), res: &itm? >> opt (itm, b)
    ):<cloref> #[b:bool] bool b = begin
    case+ t of
    | T (_(*c*), k, x, !ptl, !ptr) => let
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

fn{key:t0p;itm:vt0p}
insfix_l // right rotation
  {cl,cr:clr}
  {bh:nat}
  {v:nat}
  {l_c,l_k,l_x,l_tl,l_tr:addr} (
  pf_c: int(BLK) @ l_c
, pf_k: key @ l_k
, pf_x: itm @ l_x
, pf_tl: rbtree (key, itm, cl, bh, v) @ l_tl
, pf_tr: rbtree (key, itm, cr, bh, 0) @ l_tr
| t: T_unfold (l_c, l_k, l_x, l_tl, l_tr)
, p_tl: ptr (l_tl)
) :<> [c:clr] rbtree0 (key, itm, c, bh+1) = let
  #define B BLK; #define R RED
in
  case+ !p_tl of
  | T (!p_cl as R, _, _, !p_tll as T (!p_cll as R, _, _, _, _), !p_tlr) => let
//
      val () = !p_cll := B
      val () = fold@ (!p_tll)
//
      val tl = !p_tl
      val () = !p_tl := !p_tlr
      val () = fold@ (t)
//
      val () = !p_tlr := t
    in
      fold@ (tl); tl
    end // end of [T (R, T (R, ...), ...)]
  | T (!p_cl as R, _, _, !p_tll, !p_tlr as T (!p_clr as R, _, _, !p_tlrl, !p_tlrr)) => let
//
      val tl = !p_tl
      val () = !p_tl := !p_tlrr
      val () = fold@ (t)
      val () = !p_tlrr := t
//
      val tlr = !p_tlr
      val () = !p_tlr := !p_tlrl
      val () = !p_cl := B
      val () = fold@ (tl)
      val () = !p_tlrl := tl
//
    in
      fold@ (tlr); tlr
    end // end of [T (R, ..., T (R, ...))]
  | _ =>> (fold@ (t); t)
end // end of [insfix_l]

fn{key:t0p;itm:vt0p}
insfix_r // left rotation
  {cl,cr:clr}
  {bh:nat}
  {v:nat}
  {l_c,l_k,l_x,l_tl,l_tr:addr} (
  pf_c: int(BLK) @ l_c
, pf_k: key @ l_k
, pf_x: itm @ l_x
, pf_tl: rbtree (key, itm, cl, bh, 0) @ l_tl
, pf_tr: rbtree (key, itm, cr, bh, v) @ l_tr
| t: T_unfold (l_c, l_k, l_x, l_tl, l_tr)
, p_tr: ptr (l_tr)
) :<> [c:clr] rbtree0 (key, itm, c, bh+1) = let
  #define B BLK; #define R RED
in
  case+ !p_tr of
  | T (!p_cr as R, _, _, !p_trl, !p_trr as T (!p_crr as R, _, _, _, _)) => let
//
      val () = !p_crr := B
      val () = fold@ (!p_trr)
//
      val tr = !p_tr
      val () = !p_tr := !p_trl
      val () = fold@ (t)
//
      val () = !p_trl := t
    in
      fold@ (tr); tr
    end // end of [T (R, ..., T (R, ...))]
  | T (!p_cr as R, _, _, !p_trl as T (!p_crr as R, _, _, !p_trll, !p_trlr), !p_trr) => let
//
      val tr = !p_tr
      val () = !p_tr := !p_trll
      val () = fold@ (t)
      val () = !p_trll := t
//
      val trl = !p_trl
      val () = !p_trl := !p_trlr
      val () = !p_cr := B
      val () = fold@ (tr)
      val () = !p_trlr := tr
//
    in
      fold@ (trl); trl
    end // end of [T (R, T (R, ...), ...)]
  | _ =>> (fold@ (t); t)
end // end of [insfix_r]

(* ****** ****** *)

implement{key,itm}
linmap_insert
  (m, k0, x0, cmp, res) = let
//
#define B BLK; #define R RED
//
fun ins
  {c:clr} {bh:nat} .<bh,c>. (
  t: &rbtree0 (key, itm, c, bh) >> rbtree (key, itm, cl, bh, v)
, x0: itm, res: &itm? >> opt (itm, b)
) :<cloref> #[b: bool; cl:clr; v:nat | v <= c] bool (b) =
  case+ t of
  | T (
      !p_c, !p_k, !p_x, !p_tl, !p_tr
    ) => let
      val sgn = compare_key_key (k0, !p_k, cmp)
    in
      if sgn < 0 then let
        val b = ins (!p_tl, x0, res)
        stavar cl: int
        prval cl: int (cl) = rbtree_get_color (!p_tl)
      in
        if !p_c = B then let
          val () = t := insfix_l
            (view@(!p_c), view@(!p_k), view@(!p_x), view@(!p_tl), view@(!p_tr) | t, p_tl)
          // end of [val]
        in
          b
        end else let
          val () = !p_c := R in fold@ {..}{..}{..}{cl} (t); b
        end // end of [if]
      end else if sgn > 0 then let
        val b = ins (!p_tr, x0, res)
        stavar cr: int
        prval cr: int (cr) = rbtree_get_color (!p_tr)
      in
        if !p_c = B then let
          val () = t := insfix_r
            (view@(!p_c), view@(!p_k), view@(!p_x), view@(!p_tl), view@ (!p_tr) | t, p_tr)
          // end of [val]
        in
          b
        end else let
          val () = !p_c := R in fold@ {..}{..}{..}{cr} (t); b
        end // end of [if]
      end else let
        val () = res := !p_x
        prval () = opt_some {itm} (res)
        val () = !p_x := x0
        val () = fold@ {..}{..}{..}{0} (t)
      in
        true
      end (* end of [if] *)
    end // end of [cons]
  | ~E () => let
      prval () = opt_none {itm} (res)
      val () = t := T {..}{..}{..}{0} (R, k0, x0, E, E) in false
    end // end of [E]
// end of [ins]
val b = ins (m, x0, res)
//
in
//
case+ m of
| T (!p_c as R, _, _, _, _) => (!p_c := B; fold@ (m); b) | _ =>> b
//
end // end of [linmap_insert]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
rbtree_clr_trans_blk_red
  {bh:pos} (t: rbtree (key, itm, BLK, bh, 0))
  :<> [v:nat] rbtree (key, itm, RED, bh-1, v) = let
  val+ T {..}{c,cl,cr} (!p_c, _, _, _, _) = t in !p_c := RED; fold@ {..}{..}{..}{cl+cr} (t); t
end // end of [rbtree_clr_trans_blk_red]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
remfix_l
  {cl,cr:clr}
  {bh:nat}
  {v:nat}
  {l_c,l_k,l_x,l_tl,l_tr:addr} (
  pf_c: int(BLK) @ l_c
, pf_k: key @ l_k
, pf_x: itm @ l_x
, pf_tl: rbtree (key, itm, cl, bh, v) @ l_tl
, pf_tr: rbtree (key, itm, cr, bh+1, 0) @ l_tr
| t: T_unfold (l_c, l_k, l_x, l_tl, l_tr)
, p_c: ptr (l_c)
, p_tl: ptr (l_tl)
, p_tr: ptr (l_tr)
) :<> [c:clr;v:nat | v <= cr] rbtree (key, itm, c, bh+1, v) = let
  #define B BLK; #define R RED
in
  case+ !p_tl of
  | T (!p_cl as R, _, _, _, _) => (
      !p_cl := B; fold@ (!p_tl); !p_c := R; fold@ {..}{..}{..}{cr} (t); t
    ) // end of [T (R, ...)]
  | _ =>> let
    in
      case+ !p_tr of
      | T {..} {cr,crl,crr} (!p_cr as B, _, _, _, _) => let
          val () = !p_cr := R; val () = fold@ {..}{..}{..}{crl+crr} (!p_tr)
        in
          insfix_r (pf_c, pf_k, pf_x, pf_tl, pf_tr | t, p_tr)
        end // end of [T (B, ...)]
      | T (!p_cr as R, !p_kr, !p_xr, !p_trl, !p_trr) => let
          val trl = !p_trl
          val+ T (!p_crl, !p_krl, !p_xrl, !p_trll, !p_trlr) = trl
//
          val tr = !p_tr
          val () = !p_tr := !p_trll
          val () = fold@ (t)
          val () = !p_trll := t
//
          val () = !p_trl := !p_trlr
          val () = !p_trr := rbtree_clr_trans_blk_red (!p_trr)
//
          val () = !p_cr := B
          val [c_new:int] t_new = insfix_r (
            view@(!p_cr), view@(!p_kr), view@(!p_xr), view@(!p_trl), view@(!p_trr) | tr, p_trr
          ) // end of [val]
          val () = !p_trlr := t_new
//
        in
          !p_crl := R; fold@ {..}{..}{..}{c_new} (trl); trl
        end // end of [T (R, ...)]
    end // end of [_]
end // end of [remfix_l]

fn{key:t0p;itm:vt0p}
remfix_r
  {cl,cr:clr}
  {bh:nat}
  {v:nat}
  {l_c,l_k,l_x,l_tl,l_tr:addr} (
  pf_c: int(BLK) @ l_c
, pf_k: key @ l_k
, pf_x: itm @ l_x
, pf_tl: rbtree (key, itm, cl, bh+1, 0) @ l_tl
, pf_tr: rbtree (key, itm, cr, bh, v) @ l_tr
| t: T_unfold (l_c, l_k, l_x, l_tl, l_tr)
, p_c: ptr (l_c)
, p_tl: ptr (l_tl)
, p_tr: ptr (l_tr)
) :<> [c:clr;v:nat | v <= cl] rbtree (key, itm, c, bh+1, v) = let
  #define B BLK; #define R RED
in
  case+ !p_tr of
  | T (!p_cr as R, _, _, _, _) => (
      !p_cr := B; fold@ (!p_tr); !p_c := R; fold@ {..}{..}{..}{cl} (t); t
    ) // end of [T (R, ...)]
  | _ =>> let
    in
      case+ !p_tl of
      | T {..} {cl,cll,clr} (!p_cl as B, _, _, _, _) => let
          val () = !p_cl := R; val () = fold@ {..}{..}{..}{cll+clr} (!p_tl)
        in
          insfix_l (pf_c, pf_k, pf_x, pf_tl, pf_tr | t, p_tl)
        end // end of [T (B, ...)]
      | T (!p_cl as R, !p_kl, !p_xl, !p_tll, !p_tlr) => let
          val tlr = !p_tlr
          val+ T (!p_clr, !p_klr, !p_xlr, !p_tlrl, !p_tlrr) = tlr
//
          val tl = !p_tl
          val () = !p_tl := !p_tlrr
          val () = fold@ (t)
          val () = !p_tlrr := t
//
          val () = !p_tll := rbtree_clr_trans_blk_red (!p_tll)
          val () = !p_tlr := !p_tlrl
//
          val () = !p_cl := B
          val [c_new:int] t_new = insfix_l (
            view@(!p_cl), view@(!p_kl), view@(!p_xl), view@(!p_tll), view@(!p_tlr) | tl, p_tll
          ) // end of [val]
          val () = !p_tlrl := t_new
//
        in
          !p_clr := R; fold@ {..}{..}{..}{c_new} (tlr); tlr
        end // end of [T (R, ...)]
    end // end of [_]
end // end of [remfix_r]

(* ****** ****** *)

typedef rbtree0 = rbtree (void, void, 0, 0, 0)?
viewtypedef
T_node (key:t@ype, itm:viewt@ype) =
  T_pstruct (int?, key, itm, rbtree0, rbtree0)
// end of [T_node]

extern
castfn T_node_make
  {key:t0p;itm:vt0p}
  {l_c,l_k,l_x,l_tl,l_tr:addr} (
    pf_c: int? @ l_c, pf_k: key @ l_k, pf_x: itm @ l_x, pf_tl: rbtree? @ l_tl, pf_tr: rbtree? @ l_tr
  | x: T_unfold (l_c, l_k, l_x, l_tl, l_tr)
  ) :<> T_node (key, itm)
// end of [T_node_make]

fun{key:t0p;itm:vt0p}
rbtree_remove_min
  {c:clr}
  {bh:nat | bh+c > 0} .<bh,c>. (
  t: &rbtree0 (key, itm, c, bh) >> rbtree (key, itm, c1, bh-bhdf, 0)
, bhdf: &int? >> int (bhdf)
) :<> #[
  bhdf:two; c1:clr | bhdf <= bh; c1 <= c+bhdf
] T_node (key, itm) = let
  #define B BLK; #define R RED
  val+ T {..} {c,cl,cr} (!p_c, !p_k, !p_x, !p_tl, !p_tr) = t
  prval pf_c = view@ !p_c
  prval pf_k = view@ !p_k
  prval pf_x = view@ !p_x
  prval pf_tl = view@ !p_tl
  prval pf_tr = view@ !p_tr
in
  case+ !p_c of
  | B => (case+ !p_tl of
    | T _ => let
        val () = fold@ {..}{..}{..}{0} (!p_tl)
        val node = rbtree_remove_min (!p_tl, bhdf)
      in
        if bhdf = 0 then let
          val () = fold@ {..}{..}{..}{0} (t) in node
        end else let
          val () = t := remfix_l (
            pf_c, pf_k, pf_x, pf_tl, pf_tr | t, p_c, p_tl, p_tr
          ) // end of [val]
        in
          case+ t of
          | T (!p_c as R, _, _, _, _) => (
              bhdf := 0; !p_c := B; fold@ {..}{..}{..}{0} (t); node
            ) // end of [T]
          | _ =>> node
        end (* end of [if] *)
      end // end of [T]
    | ~E () => let
        val () = bhdf := 1
        val tr = !p_tr; val t0 = t; val () = t := tr in
        T_node_make {key,itm} (pf_c, pf_k, pf_x, pf_tl, pf_tr | t0)
      end // end of [E]
    ) // end of [T (B, ...)]
  | R => begin case+ !p_tl of
    | T _ => let
        val () = fold@ {..}{..}{..}{0} (!p_tl)
        val node = rbtree_remove_min (!p_tl, bhdf)
      in
        if bhdf = 0 then let
          val () = fold@ {..}{..}{..}{0} (t) in node
        end else let // bhdf = 1
          val () = bhdf := 0
          val () = !p_tr := rbtree_clr_trans_blk_red (!p_tr)
          val () = !p_c := B
          val () = t := insfix_r (pf_c, pf_k, pf_x, pf_tl, pf_tr | t, p_tr)
        in
          node
        end (* end of [if] *)
      end // end of [T (B, ...)]
    | ~E () => let
        val () = bhdf := 0
        val tr = !p_tr; val t0 = t; val () = t := tr in
        T_node_make {key,itm} (pf_c, pf_k, pf_x, pf_tl, pf_tr | t0)
      end // end of [E]
    end (* end of [T (R, ...)] *)
end // end of [rbtree_remove_min]

(* ****** ****** *)

fn{key:t0p;itm:vt0p}
rbtree_join
  {cl,cr:clr} {bh:nat} (
  tl: rbtree0 (key, itm, cl, bh)
, tr: rbtree0 (key, itm, cr, bh)
) :<> [c:clr;v:nat | v <= cl+cr] rbtree (key, itm, c, bh, v) =
  case+ tr of
  | T _ => let
      val () = fold@ {..}{..}{..}{0} (tr)
      var tr = tr
      var bhdf: int
      val node = rbtree_remove_min (tr, bhdf)
      stavar cr: int
      prval cr: int (cr) = rbtree_get_color (tr)
      val+ T (!p_c, !p_k, !p_x, !p_tl, !p_tr) = node
    in
      if bhdf = 0 then let
        val () = !p_c := RED
        val () = !p_tl := tl
        val () = !p_tr := tr
      in
        fold@ {..}{..}{..}{cl+cr} (node); node
      end else let
        val () = !p_c := BLK
        val () = !p_tl := tl
        val () = !p_tr := tr
      in
        remfix_r (
          view@ (!p_c), view@ (!p_k), view@ (!p_x), view@ (!p_tl), view@ (!p_tr)
        | node, p_c, p_tl, p_tr
        ) // end of [remfix_r]
      end (* end of [if] *)
    end // end of [T]
  | ~E () => tl
// end of [rbtree_join]

(* ****** ****** *)
//
// HX: unsafe but convenient to implement
//
extern
fun{key:t0p;itm:vt0p}
linmap_takeout_ptr {l_res:addr} (
  m: &map (key, itm), k0: key, cmp: cmp key, res: ptr l_res
) :<> bool
// end of [linmap_takeout]

implement{key,itm}
linmap_takeout_ptr {l_res}
  (m, k0, cmp, p_res) = let
//
#define B BLK; #define R RED
//
fun takeout
  {c:clr} {bh:nat} .<bh,c>. (
  t: &rbtree0 (key, itm, c, bh) >> rbtree0 (key, itm, c1, bh-bhdf)
, bhdf: &int? >> int bhdf
, p_res: ptr l_res
) :<cloref> #[bhdf:two; c1:clr | bhdf <= bh; c1 <= c+bhdf] bool =
//
case+ t of
| T _ => let
    val () = fold@ {..}{..}{..}{0} (t)
    val+ T (!p_c, !p_k, !p_x, !p_tl, !p_tr) = t
    stavar l_x: addr
    val p_x = p_x : ptr l_x
    prval pf_c = view@ !p_c
    prval pf_k = view@ !p_k
    prval pf_x = view@ !p_x
    prval pf_tl = view@ !p_tl
    prval pf_tr = view@ !p_tr
    val sgn = compare_key_key (k0, !p_k, cmp)
  in
    case+ !p_c of
    | B => if sgn < 0 then let
        val bval = takeout (!p_tl, bhdf, p_res)
      in
        if bhdf = 0 then let
          val () = fold@ {..}{..}{..}{0} (t) in bval
        end else let // bhdf = 1
          val () = t := remfix_l
            (pf_c, pf_k, pf_x, pf_tl, pf_tr | t, p_c, p_tl, p_tr)
          // end of [val]
        in
          case+ t of
          | T (!p_c as R, _, _, _, _) => (
              bhdf := 0; !p_c := B; fold@ (t); bval
            ) // end of [T]
          | _ =>> bval
        end // end of [if]
      end else if sgn > 0 then let
        val bval = takeout (!p_tr, bhdf, p_res)
      in
        if bhdf = 0 then let
          val () = fold@ {..}{..}{..}{0} (t) in bval
        end else let // bhdf = 1
          val () = t := remfix_r
            (pf_c, pf_k, pf_x, pf_tl, pf_tr | t, p_c, p_tl, p_tr)
          // end of [val]
        in
          case+ t of
          | T (!p_c as R, _, _, _, _) => (
              bhdf := 0; !p_c := B; fold@ (t); bval
            ) // end of [T]
          | _ =>> bval
        end // end of [if]
      end else let // x0 = x
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
        val tl = !p_tl and tr = !p_tr
        val () = free@ {key,itm}{0,0,0}{0}{0} (t)
        val bval = true
        val () = t := rbtree_join (tl, tr)
      in
        case+ t of
        | T (!p_c as R, _, _, _, _) => (
            bhdf := 0; !p_c := B; fold@ (t); bval
          ) // end of [T]
        | _ =>> (bhdf := 1; bval)
      end (* end of [if] *)
    // end of [B]
    | R => if sgn < 0 then let
        val bval = takeout (!p_tl, bhdf, p_res)
      in
        if bhdf = 0 then let
          val () = fold@ {..}{..}{..}{0} (t) in bval
        end else let // bhdf = 1
          val () = bhdf := 0
          val () = !p_c := BLK
          val () = t := remfix_l
            (pf_c, pf_k, pf_x, pf_tl, pf_tr | t, p_c, p_tl, p_tr)
          // end of [val]
        in
          bval
        end // end of [if]
      end else if sgn > 0 then let
        val bval = takeout (!p_tr, bhdf, p_res)
      in
        if bhdf = 0 then let
          val () = fold@ {..}{..}{..}{0} (t) in bval
        end else let // bhdf = 1
          val () = bhdf := 0
          val () = !p_c := BLK
          val () = t := remfix_r
            (pf_c, pf_k, pf_x, pf_tl, pf_tr | t, p_c, p_tl, p_tr)
          // end of [val]
        in
          bval   
        end // end of [if]
      end else let // x0 = x
        val () = bhdf := 0
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
        val tl = !p_tl and tr = !p_tr
        val () = free@ {key,itm}{0,0,0}{0}{0} (t)
        val bval = true
        val () = t := rbtree_join (tl, tr)
      in
         true
      end (* end of [if] *)
    // end of [R]
  end // end of [T]
| E () => (bhdf := 0; fold@ (t); false)
// end of [takeout]
//
var bhdf: int // uninitialized
//
in
//
takeout (m, bhdf, p_res)
//
end // end of [linmap_takeout_ptr]

(* ****** ****** *)

implement{key,itm}
linmap_takeout
  (m, k0, cmp, res) = ans where {
  val ans = linmap_takeout_ptr<key,itm> (m, k0, cmp, &res)
  val [b:bool] ans = bool1_of_bool (ans)
  prval pf = __assert (view@ res) where {
    extern praxi __assert
      {l_res:addr} (pf: itm? @ l_res):<> opt (itm, b) @ l_res
    // end of [__assert]
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
  fun foreach {c:clr} {bh:nat} .<bh,c>.
    (pf: !v | t: !rbtree0 (key, itm, c, bh), env: !vt):<cloref> void =
    case+ t of
    | T (_(*c*), !p_k, !p_x, !p_tl, !p_tr) => (
        foreach (pf | !p_tl, env); f (pf | !p_k, !p_x, env); foreach (pf | !p_tr, env); fold@ (t)
      ) // end of [B]
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
  fun foreach {c:clr} {bh:nat} .<bh,c>. (
    pf: !v | t: !rbtree0 (key, itm, c, bh), f: &(!v | key, &itm) -<clo> void
  ) :<> void =
    case+ t of
    | T (_(*c*), !p_k, !p_x, !p_tl, !p_tr) => begin
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
  fun _free {c:clr} {bh:nat} .<bh,c>.
    (t: rbtree0 (key, itm, c, bh)):<> void = case+ t of
    | ~T (_, _, _, tl, tr) => (_free tl; _free tr) | ~E () => ()
  // end of [_free]
} // end of [linmap_free]

implement{key,itm}
linmap_free_vt (m) = let
  viewtypedef VT = map (key, itm) in
  case+ m of
  | T _ => true where {
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
  fun aux {c:clr} {bh:nat} .<bh,c>.
    (t: !rbtree0 (key, itm, c, bh), res: res_t):<> res_t =
    case+ t of
    | T (_(*c*), k, x, !p_tl, !p_tr) => let
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
  fun aux {c:clr} {bh:nat} .<bh,c>.
    (t: rbtree0 (key, itm, c, bh), res: res_t):<> res_t =
    case+ t of
    | ~T (_(*c*), k, x, tl, tr) => let
        val res = aux (tr, res)
        val res = list_vt_cons ((k, x), res)
        val res = aux (tl, res)
      in
        res
      end // end of [B]
    | ~E () => res
  // end of [aux]
in
  aux (m, list_vt_nil)
end // end of [linmap_listize_free]

(* ****** ****** *)

(* end of [linmap_avltree.dats] *)
