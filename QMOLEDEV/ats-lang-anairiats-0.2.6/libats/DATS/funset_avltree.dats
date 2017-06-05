(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
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
** A functional set implementation based on AVL trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: May, 2011 // based on a version done in October, 2008
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading at run-time

(* ****** ****** *)

staload "libats/SATS/funset_avltree.sats"

(* ****** ****** *)
//
// a specialized version can be implemented on the spot
//
implement{a} compare_elt_elt (x1, x2, cmp) = cmp (x1, x2)
//
(* ****** ****** *)

//
// HX-2010-03-24: this seems to work best!
//
#define HTDF 1 // max height difference
#define HTDF1 %(HTDF+1)
#define HTDF_1 %(HTDF-1)

(* ****** ****** *)

datatype avltree (
  a:t@ype+, int(*height*)
) =
  | {hl,hr:nat | hl <= hr+HTDF; hr <= hl+HTDF}
    B (a, 1+max(hl,hr)) of
      (int (1+max(hl,hr)), a, avltree (a, hl), avltree (a, hr))
  | E (a, 0)
// end of [datatype avltree]

typedef avltree_inc (a:t@ype, h:int) =
  [h1:nat | h <= h1; h1 <= h+1] avltree (a, h1)
// end of [avltree_inc]

typedef avltree_dec (a:t@ype, h:int) =
  [h1:nat | h1 <= h; h <= h1+1] avltree (a, h1)
// end of [avltree_dec]

(* ****** ****** *)

assume
set_t0ype_type (a:t@ype) = [h:nat] avltree (a, h)

(* ****** ****** *)

implement{} funset_make_nil () = E ()
implement{a} funset_make_sing (x) = B (1, x, E, E)

implement{a}
funset_make_list
  (xs, cmp) = res where {
  fun loop {n:nat} .<n>. (
    xs:  list (a, n), res: &set a
  ) :<cloref> void =
    case+ xs of
    | list_cons (x, xs) => let
        val _(*inserted*) = funset_insert<a> (res, x, cmp) in loop (xs, res)
      end // end of [list_cons]
    | list_nil () => ()
  var res: set a = funset_make_nil ()
  val () = loop (xs, res)
} // end of [funset_make_list]

(* ****** ****** *)

implement{}
funset_is_nil (xs) = case+ xs of B _ => false | E () => true
implement{}
funset_isnot_nil (xs) = case+ xs of B _ => true | E () => false 

(* ****** ****** *)

implement{a}
funset_size (xs) = aux (xs) where {
  fun aux {h:nat} .<h>.
    (t: avltree (a, h)):<> size_t =
    case+ t of
    | B (_, _, tl, tr) => aux (tl) + 1 + aux (tr)
    | E () => 0
  // end of [aux]
} // end of [funset_size]

implement{a}
funset_height (xs) = aux (xs) where {
  fun aux {h:nat} .<h>.
    (t: avltree (a, h)):<> size_t =
    case+ t of
    | B (_, _, tl, tr) =>
        (size_of_int1)1 + max_size_size (aux (tl), aux (tr))
    | E () => 0
  // end of [aux]
} // end of [funset_size]

(* ****** ****** *)

macdef avltree_height (t) =
  case+ ,(t) of B (h, _, _, _) => h | E _ => 0
// end of [avltree_height]

(* ****** ****** *)

implement{a}
funset_is_member
  (xs, x0, cmp) = aux (xs) where {
  fun aux {h:nat} .<h>.
    (t: avltree (a, h)):<cloref> bool =
    case+ t of
    | B (_, x, tl, tr) => let
        val sgn = compare_elt_elt (x0, x, cmp)
      in
        if sgn < 0 then aux (tl) else (if sgn > 0 then aux (tr) else true)
      end
    | E () => false
  // end of [aux]
} // end of [funset_is_member]

implement{a}
funset_isnot_member (xs, x0, cmp) = ~funset_is_member (xs, x0, cmp)

(* ****** ****** *)

(*
** left rotation for restoring height invariant
*)
fn{a:t@ype}
avltree_lrotate {hl,hr:nat | hl+HTDF1 == hr} (
    x: a
  , hl : int hl
  , tl: avltree (a, hl)
  , hr : int hr
  , tr: avltree (a, hr)
  ) :<> avltree_inc (a, hr) = let
  val+ B {..} {hrl,hrr} (_(*hr*), xr, trl, trr) = tr
  val hrl = avltree_height trl : int hrl
  and hrr = avltree_height trr : int hrr
in
  if hrl <= hrr+HTDF_1 then let
    val hrl1 = hrl + 1
  in
    B (1+max(hrl1,hrr), xr, B (hrl1, x, tl, trl), trr)
  end else let // [hrl=hrr+2]: deep rotation
    val+ B {..} {hrll,hrlr} (_(*hrl*), xrl, trll, trlr) = trl
    val hrll = avltree_height trll : int hrll
    val hrlr = avltree_height trlr : int hrlr
  in
    B (hr, xrl, B (1+max(hl,hrll), x, tl, trll), B (1+max(hrlr,hrr), xr, trlr, trr))
  end // end of [if]
end // end of [avltree_lrotate]

(*
** right rotation for restoring height invariant
*)
fn{a:t@ype}
avltree_rrotate {hl,hr:nat | hl == hr+HTDF1} (
    x: a
  , hl: int hl
  , tl: avltree (a, hl)
  , hr: int hr
  , tr: avltree (a, hr)
  ) :<> avltree_inc (a, hl) = let
  val+ B {..} {hll, hlr} (_(*hl*), xl, tll, tlr) = tl
  val hll = avltree_height tll : int hll
  and hlr = avltree_height tlr : int hlr
in
  if hll+HTDF_1 >= hlr then let
    val hlr1 = hlr + 1
  in
    B (1+max(hll,hlr1), xl, tll, B (hlr1, x, tlr, tr))
  end else let
    val+ B {..} {hlrl,hlrr} (_(*hlr*), xlr, tlrl, tlrr) = tlr
    val hlrl = avltree_height tlrl : int hlrl
    val hlrr = avltree_height tlrr : int hlrr
  in
    B (hl, xlr, B (1+max(hll,hlrl), xl, tll, tlrl), B (1+max(hlrr,hr), x, tlrr, tr))
  end // end of [if]
end // end of [avltree_rrotate]

(* ****** ****** *)

implement{a}
funset_insert
  (xs, x0, cmp) = res where {
  fun insert {h:nat} .<h>. (
      t: avltree (a, h), res: &bool? >> bool
    ) :<cloref> avltree_inc (a, h) = begin case+ t of
    | B {..} {hl,hr} (h, x, tl, tr) => let
        val sgn = compare_elt_elt (x0, x, cmp)
      in
        if sgn < 0 then let
          val [hl:int] tl = insert (tl, res)
          val hl = avltree_height (tl) : int hl
          and hr = avltree_height (tr) : int hr
        in
          if hl - hr <= HTDF then begin
            B (1+max(hl,hr), x, tl, tr)
          end else begin // hl = hr+HTDF1
            avltree_rrotate (x, hl, tl, hr, tr)
          end // end of [if]
        end else if sgn > 0 then let
          val [hr:int] tr = insert (tr, res)
          val hl = avltree_height (tl) : int hl
          and hr = avltree_height (tr) : int hr
        in
          if hr - hl <= HTDF then begin
            B (1+max(hl, hr), x, tl, tr)
          end else begin // hl+HTDF1 = hr
            avltree_lrotate (x, hl, tl, hr, tr)
          end // end of [if]
        end else let (* [k0] already exists *)
          val () = res := true in B (h, x0, tl, tr)
        end // end of [if]
      end // end of [B]
    | E () => let (* [x0] is not in [m] *)
        val () = res := false in B (1, x0, E (), E ())
      end // end of [E]
  end // end of [insert]
  var res: bool // uninitialized
  val () = xs := insert (xs, res)
} // end of [funset_insert]

(* ****** ****** *)

fun{a:t@ype}
avltree_takeout_min
  {h:pos} .<h>. (
  t: avltree (a, h), x0: &a? >> a
) :<> avltree_dec (a, h) = let
  val+ B {..} {hl,hr} (_, x, tl, tr) = t
in
  case+ tl of
  | B _ => let
      val [hl:int] tl = avltree_takeout_min<a> (tl, x0)
      val hl = avltree_height (tl) : int hl
      and hr = avltree_height (tr) : int hr
    in
      if hr - hl <= HTDF then begin
        B (1+max(hl,hr), x, tl, tr)
      end else begin // hl+HTDF1 = hr
        avltree_lrotate (x, hl, tl, hr, tr)
      end // end of [if]
    end // end of [B]
  | E () => (x0 := x; tr)
end // end of [avltree_takeout_min]

(* ****** ****** *)

implement{a}
funset_remove
  (m, x0, cmp) = b(*removed*) where {
  fun remove {h:nat} .<h>. (
    t: avltree (a, h), b: &bool? >> bool
  ) :<cloref> avltree_dec (a, h) = begin
    case+ t of
    | B {..} {hl,hr} (h, x, tl, tr) => let
        val sgn = compare_elt_elt (x0, x, cmp)
      in
        case+ 0 of
        | _ when sgn < 0 => let
            val [hl:int] tl = remove (tl, b)
            val hl = avltree_height (tl) : int hl
            and hr = avltree_height (tr) : int hr
          in
            if hr - hl <= HTDF then
              B (1+max(hl,hr), x, tl, tr)
            else // hl+HTDF1 = hr
              avltree_lrotate (x, hl, tl, hr, tr)
            // end of [if]
          end // end of [sgn < 0]
        | _ when sgn > 0 => let
            val [hr:int] tr = remove (tr, b)
            val hl = avltree_height (tl) : int hl
            and hr = avltree_height (tr) : int hr
          in
            if hl - hr <= HTDF then
              B (1+max(hl,hr), x, tl, tr)
            else // hl=hr+HTDF1
              avltree_rrotate (x, hl, tl, hr, tr)
            // end of [if]
          end // end of [sgn > 0]
        | _ (*sgn = 0*) => let
            val () = b := true
          in
            case+ tr of
            | B _ => let
                var x_min: a?
                val [hr:int] tr = avltree_takeout_min<a> (tr, x_min)
                val hl = avltree_height (tl) : int hl
                and hr = avltree_height (tr) : int hr
              in
                if hl - hr <= HTDF then
                  B (1+max(hl,hr), x_min, tl, tr)
                else // hl=hr+HTDF1
                  avltree_rrotate (x_min, hl, tl, hr, tr)
                // end of [if]
              end // end of [B]
            | E _ => tl
          end // end of [sgn = 0]
      end // end of [B]
    | E () => t where {
        val () = b := false
      } // end of [E]
  end // end of [remove]
  var b: bool // unitialized
  val () = m := remove (m, b)
} // end of [funset_remove]

(* ****** ****** *)

(*
** left join: height(tl) >= height(tr)
*)
fun{a:t@ype}
avltree_ljoin
  {hl,hr:nat | hl >= hr} .<hl>. (
  x: a, tl: avltree (a, hl), tr: avltree (a, hr)
) :<> avltree_inc (a, hl) = let
  val hl = avltree_height (tl): int hl
  and hr = avltree_height (tr): int hr
in
  if hl >= hr + HTDF1 then let
    val+ B {..} {hll, hlr} (_, xl, tll, tlr) = tl
    val [hlr:int] tlr = avltree_ljoin<a> (x, tlr, tr)
    val hll = avltree_height (tll): int hll
    and hlr = avltree_height (tlr): int hlr
  in
    if hlr <= hll + HTDF then
      B (max(hll,hlr)+1, xl, tll, tlr)
    else // hll+HTDF1 = hlr
      avltree_lrotate<a> (xl, hll, tll, hlr, tlr)
    // end of [if]
  end else begin
    B (hl+1, x, tl, tr)
  end // end of [if]
end // end of [avltree_ljoin]

(*
** right join: height(tl) <= height(tr)
*)
fun{a:t@ype}
avltree_rjoin
  {hl,hr:nat| hl <= hr} .<hr>. (
  x: a, tl: avltree (a, hl), tr: avltree (a, hr)
) :<> avltree_inc (a, hr) = let
  val hl = avltree_height (tl): int hl
  and hr = avltree_height (tr): int hr
in
  if hr >= hl + HTDF1 then let
    val+ B {..} {hrl,hrr} (_, xr, trl, trr) = tr
    val [hrl:int] trl = avltree_rjoin<a> (x, tl, trl)
    val hrl = avltree_height (trl): int hrl
    and hrr = avltree_height (trr): int hrr
  in
    if hrl <= hrr + HTDF then
      B (max(hrl,hrr)+1, xr, trl, trr)
    else // hrl = hrr+HTDF1
      avltree_rrotate (xr, hrl, trl, hrr, trr)
    // end of [if]
  end else begin
    B (hr+1, x, tl, tr)
  end // end of [if]
end // end of [avltree_rjoin]

(* ****** ****** *)

fn{a:t@ype}
avltree_join {hl,hr:nat} (
  x: a, tl: avltree (a, hl), tr: avltree (a, hr)
) :<> [h:int | hl <= h; hr <= h; h <= max(hl,hr)+1] avltree (a, h) = let
  val hl = avltree_height tl: int hl
  and hr = avltree_height tr: int hr
in
  if hl >= hr then avltree_ljoin (x, tl, tr) else avltree_rjoin (x, tl, tr)
end // end of [avltree_join]

(* ****** ****** *)

fn{a:t@ype}
avltree_concat {hl,hr:nat} (
  tl: avltree (a, hl), tr: avltree (a, hr)
) :<> [h:nat | h <= max(hl,hr)+1] avltree (a, h) =
  case+ (tl, tr) of
  | (E (), _) => tr
  | (_, E ()) => tl
  | (_, _) =>> let
      var x_min: a // uninitialized
      val tr = avltree_takeout_min<a> (tr, x_min)
    in
      avltree_join (x_min, tl, tr)
    end // end of [_, _]
// end of [avltree_concat]

(* ****** ****** *)

typedef avltree0 = avltree (void, 0)?

fun{a:t@ype}
avltree_split_at {h:nat} .<h>. (
  t: avltree (a, h), x0: a
, tl0: &avltree0 >> avltree (a, hl)
, tr0: &avltree0 >> avltree (a, hr)
, cmp: cmp a
) :<> #[i:two; hl,hr:nat | hl <= h; hr <= h] int i =
  case+ t of
  | B (_(*h*), x, tl, tr) => let
      val sgn = compare_elt_elt<a> (x0, x, cmp)
    in
      if sgn < 0 then let
        val i = avltree_split_at (tl, x0, tl0, tr0, cmp)
      in
        tr0 := avltree_join (x, tr0, tr); i
      end else if sgn > 0 then let
        val i = avltree_split_at (tr, x0, tl0, tr0, cmp)
      in
        tl0 := avltree_join (x, tl, tl0); i
      end else begin
        tl0 := tl; tr0 := tr; 1 // [x] is found in [t]
      end // end of [if]
    end // end of [B]
  | E () => (tl0 := E (); tr0 := E (); 0)
// end of [avltree_split_at]

(* ****** ****** *)

implement{a}
funset_choose
  (xs, x0) = case+ xs of
  | B (_(*h*), x, _(*tl*), _(*tr*)) => let
      val () = x0 := x
      prval () = opt_some {a} (x0)
    in
      true
    end // end of [B]
  | E () => let
      prval () = opt_none {a} (x0)
    in
      false
    end // end of [E]
// end of [funset_choose]

implement{a}
funset_takeout
  (xs, x0) = case+ xs of
  | B (_(*h*), x, tl, tr) => let
      val () = x0 := x
      val () = xs := avltree_concat<a> (tl, tr)
      prval () = opt_some {a} (x0)
    in
      true
    end // end of [E]
  | E () => let
      prval () = opt_none {a} (x0)
    in
      false
    end // end of [E]
// end of [funset_takeout]

(* ****** ****** *)

implement{a}
funset_union
  (t1, t2, cmp) = union (t1, t2) where {
  fun union {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> [h:nat] avltree (a, h) = begin
    case+ (t1, t2) of
    | (E (), _) => t2
    | (_, E ()) => t1
    | (_, _) =>> let
        val+ B (_(*h1*), x1, t1l, t1r) = t1
        var t2l0: avltree0 and t2r0: avltree0
        val+ _(*i*) = avltree_split_at (t2, x1, t2l0, t2r0, cmp)
        val t12l = union (t1l, t2l0) and t12r = union (t1r, t2r0)
      in
        avltree_join (x1, t12l, t12r)
      end // end of [_, _]
    end // end of [uni]
  // end of [union] // [union] is a keyword
} // end of [funset_union]

(* ****** ****** *)

implement{a}
funset_intersect
  (t1, t2, cmp) = inter (t1, t2) where {
  fun inter {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> [h:nat] avltree (a, h) = begin
    case+ (t1, t2) of
    | (E (), _) => E ()
    | (_, E ()) => E ()
    | (_, _) =>> let
        val+ B (_(*h1*), x1, t1l, t1r) = t1
        var t2l0: avltree0 and t2r0: avltree0
        val+ i = avltree_split_at (t2, x1, t2l0, t2r0, cmp)
        val t12l = inter (t1l, t2l0) and t12r = inter (t1r, t2r0)
      in
        if i = 0 then avltree_concat (t12l, t12r) else avltree_join (x1, t12l, t12r)
      end // end of [_, _]
    end // end of [inter]
  // end of [inter]
} // end of [funset_intersect]

(* ****** ****** *)

implement{a}
funset_diff
  (t1, t2, cmp) = diff (t1, t2) where {
  fun diff {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> [h:nat] avltree (a, h) = begin
    case+ (t1, t2) of
    | (E (), _) => E ()
    | (_, E ()) => t1
    | (_, _) =>> let
        val+ B (_(*h1*), x1, t1l, t1r) = t1
        var t2l0: avltree0 and t2r0: avltree0
        val+ i = avltree_split_at (t2, x1, t2l0, t2r0, cmp)
        val t12l = diff (t1l, t2l0) and t12r = diff (t1r, t2r0)
      in
        if i > 0 then avltree_concat (t12l, t12r) else avltree_join (x1, t12l, t12r)
      end // end of [_, _]
    end // end of [diff]
  // end of [diff]
} // end of [funset_diff]

(* ****** ****** *)

implement{a}
funset_symdiff
  (t1, t2, cmp) = symdiff (t1, t2) where {
  fun symdiff {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> [h:nat] avltree (a, h) = begin
    case+ (t1, t2) of
    | (E (), _) => t2
    | (_, E ()) => t1
    | (_, _) =>> let
        val+ B (_(*h1*), x1, t1l, t1r) = t1
        var t2l0: avltree0 and t2r0: avltree0
        val+ i = avltree_split_at (t2, x1, t2l0, t2r0, cmp)
        val t12l = symdiff (t1l, t2l0) and t12r = symdiff (t1r, t2r0)
      in
        if i > 0 then avltree_concat (t12l, t12r) else avltree_join (x1, t12l, t12r)
      end // end of [_, _]
    end // end of [symdiff]
  // end of [diff]
} // end of [funset_symdiff]

(* ****** ****** *)

implement{a}
funset_is_subset
  (t1, t2, cmp) = test (t1, t2) where {
  fun test {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> bool = begin case+ (t1, t2) of
    | (E (), _) => true
    | (_, E ()) => false
    | (_, _) =>> let
        val+ B(_(*h1*), x1, t1l, t1r) = t1
        var t2l0: avltree0 and t2r0: avltree0
        val+ i = avltree_split_at (t2, x1, t2l0, t2r0, cmp)
      in
        if i > 0 then
          (if test (t1l, t2l0) then test (t1r, t2r0) else false)
        else false
      end // end of [_, _]
  end // end of [test]    
} // end of [funset_is_subset]

implement{a}
funset_is_supset
  (t1, t2, cmp) = funset_is_subset<a> (t2, t1, cmp)
// end of [funset_is_supset]

(* ****** ****** *)

implement{a}
funset_is_equal
  (t1, t2, cmp) = test (t1, t2) where {
  fun test {h1,h2:nat} .<h1>. (
    t1: avltree (a, h1), t2: avltree (a, h2)
  ) :<cloref> bool = begin case+ (t1, t2) of
    | (E _, E _) => true
    | (E _, B _) => false
    | (B _, E _) => false
    | (_, _) =>> let
        val+ B(_(*h1*), x1, t1l, t1r) = t1
        var t2l0: avltree0 and t2r0: avltree0
        val+ i = avltree_split_at (t2, x1, t2l0, t2r0, cmp)
      in
        if i > 0 then
          (if test (t1l, t2l0) then test (t1r, t2r0) else false)
        else false
      end // end of [_, _]
  end // end of [test]    
} // end of [funset_is_equal]

(* ****** ****** *)

implement{a}
funset_foreach_funenv {v} {vt}
  (pf | xs, f, env) = foreach (pf | xs, env) where {
  fun foreach {h:nat} .<h>.
    (pf: !v | t: avltree (a, h), env: !vt):<cloref> void =
    case+ t of
    | B (_(*h*), x, tl, tr) => begin
        foreach (pf | tl, env); f (pf | x, env); foreach (pf | tr, env)
      end // end of [B]
    | E () => ()
  // end of [foreach]
} // end of [funset_foreach_funenv]

implement{a}
funset_foreach_fun
  (xs, f) = let
//
  val f = coerce (f) where {
    extern castfn coerce
      (f: (a) -<fun> void):<> (!unit_v | a, !ptr) -<fun> void
  } // end of [val]
//
  prval pfu = unit_v ()
  val () = funset_foreach_funenv<a> {unit_v} {ptr} (pfu | xs, f, null)
  prval unit_v () = pfu
//  
in
  // nothing
end // end of [funset_foreach_fun]

(* ****** ****** *)

implement{a}
funset_foreach_vclo {v}
  (pf | m, f) = foreach (pf | m, f) where {
  fun foreach {h:nat} .<h>. (
    pf: !v | t: avltree (a, h), f: &(!v | a) -<clo> void
  ) :<> void =
    case+ t of
    | B (_(*h*), x, tl, tr) => begin
        foreach (pf | tl, f); f (pf | x); foreach (pf | tr, f)
      end // end of [B]
    | E () => ()
  // end of [foreach]
} // end of [funset_foreach_vclo]

implement{a}
funset_foreach_cloref (m, f) = let
  val f = __cast (f) where { extern castfn __cast
    (f: (a) -<cloref> void):<> (!unit_v | a) -<cloref> void
  } // end of [val]
  typedef clo_type = (!unit_v | a) -<clo> void
  val (vbox pf_f | p_f) = cloref_get_view_ptr {clo_type} (f)
  prval pfu = unit_v ()
  val () = $effmask_ref
    (funset_foreach_vclo<a> {unit_v} (pfu | m, !p_f))
  prval unit_v () = pfu
in
  // empty
end // end of [funset_foreach_cloref]

(* ****** ****** *)

implement{a}
funset_listize (xs) = let
  viewtypedef res_vt = List_vt (a)
  fun listize {h:nat} .<h>. (
    t: avltree (a, h), res: res_vt
  ) :<> res_vt =
    case+ t of
    | B (_(*h*), x, tl, tr) => let
        val res = listize (tr, res)
        val res = list_vt_cons {a} (x, res)
        val res = listize (tl, res)
      in
        res
      end // end of [B]
    | E () => res
  // end of [listize]
in
  listize (xs, list_vt_nil ())
end // end of [funset_listize]

(* ****** ****** *)

(* end of [funset_avltree.dats] *)
