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
** A functional heap implementation based on Braun trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: April, 2010 // based on a version done in November, 2008
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading at run-time

(* ****** ****** *)

staload "libats/SATS/funheap_braun.sats"

(* ****** ****** *)

//
// a specialized version can be implemented on the spot
//
implement{elt} compare_elt_elt (x1, x2, cmp) = cmp (x1, x2)

(* ****** ****** *)

datatype brauntree (a:t@ype+, int) =
  | {n1,n2:nat | n2 <= n1; n1 <= n2+1}
    B (a, n1+n2+1) of (a, brauntree (a, n1), brauntree (a, n2))
  | E (a, 0) of ()
// end of [brauntree]

stadef bt = brauntree // an abbreviation

(* ****** ****** *)

assume heap_t0ype_type (elt:t@ype) = [n:nat] brauntree (elt, n)

(* ****** ****** *)

implement{} funheap_make_nil () = E ()

(* ****** ****** *)

implement{elt}
funheap_size (hp) = size (hp) where {
//
// this algorithm is taken from a paper by Chris Okasaki
//
  fun diff {nl,nr:nat | nr <= nl && nl <= nr+1} .<nr>. 
    (nr: size_t nr, t: bt (elt, nl)):<> int (nl-nr) = begin case+ t of
    | B (_, tl, tr) => begin
        if nr > 0 then let
          val nr2 = nr / 2
        in
          if nr > nr2 + nr2 then diff (nr2, tl) else diff (nr2-1, tr)
        end else begin
          1 // return value
        end // end of [if]
      end // end of [B]
     | E () => 0
  end // end of [diff]
//
  fun size {n:nat} .<n>.
    (t: bt (elt, n)):<> size_t n = case+ t of
    | B (_, tl, tr) => let
        val nr = size tr; val d1 = diff (nr, tl) + 1
      in
        nr + nr + (size1_of_int1)d1
      end // end of [B]
    | E () => 0
  // end of [size]
} // end of [funheap_size]

(* ****** ****** *)

implement{elt}
funheap_height (hp) = loop (hp, 0) where {
  fun loop {n:nat} .<n>.
    (t: bt (elt, n), res: Nat):<> Nat =
    case+ t of B (_, tl, _) => loop (tl, res + 1) | E () => res
  // end of [loop]
} // end of [funheap_height]

(* ****** ****** *)

implement{elt}
funheap_insert (hp, x, cmp) = () where {
  fun insert {n:nat} .<n>.
    (t: bt (elt, n), x: elt):<cloref> bt (elt, n+1) =
    case+ t of
    | E () => B (x, E (), E ())
    | B (x0, t1, t2) => let
        val sgn = compare_elt_elt (x0, x, cmp)
      in
        if sgn >= 0 then
          B (x, insert (t2, x0), t1) else B (x0, insert (t2, x), t1)
        // end of [if]
      end // end of [B]
  // end of [insert]
  val () = hp := insert (hp, x)
} // end of [funheap_insert]

(* ****** ****** *)

fun{elt:t@ype}
brauntree_leftrem {n:pos} .<n>.
  (t: bt (elt, n), x_r: &elt? >> elt):<> bt (elt, n-1) = let
  val+ B (x, t1, t2) = t
in
  case+ t1 of
  | B _ => let
      val t1 = brauntree_leftrem (t1, x_r) in B (x, t2, t1)
    end // end of [B]
  | E () => (x_r := x; E ())
end // end of [brauntree_leftrem]

(* ****** ****** *)

fn{elt:t@ype}
brauntree_siftdn
  {nl,nr:nat | nr <= nl; nl <= nr+1}  (
    x: elt
  , tl: bt (elt, nl), tr: bt (elt, nr)
  , cmp: cmp elt
  ) :<> bt (elt, nl+nr+1) = siftdn (x, tl, tr) where {
  fun siftdn {nl,nr:nat | nr <= nl; nl <= nr+1} .<nl+nr>.
    (x: elt, tl: bt (elt, nl), tr: bt (elt, nr))
    :<cloref> bt (elt, nl+nr+1) = case+ (tl, tr) of
    | (B (xl, tll, tlr), B (xr, trl, trr)) => begin
        if compare_elt_elt (xl, x, cmp) >= 0 then begin // xl >= x
          if compare_elt_elt (xr, x, cmp) >= 0
            then B (x, tl, tr) else B (xr, tl, siftdn (x, trl, trr))
          // end of [if]
        end else begin // xl < x
          if compare_elt_elt (xr, x, cmp) >= 0 then B (xl, siftdn (x, tll, tlr), tr)
          else begin // xr < x
            if compare_elt_elt (xl, xr, cmp) >= 0
              then B (xr, tl, siftdn (x, trl, trr)) else B (xl, siftdn (x, tll, tlr), tr)
            // end of [if]
          end // end of [if]
        end (* end of [if] *)
      end (* end of [B _, B _] *)
    | (_, _) =>> begin case+ tl of
      | B (xl, _, _) =>
          if compare_elt_elt (xl, x, cmp) >= 0 then B (x, tl, E) else B (xl, B (x, E, E), E)
        // end of [B]
      | E () => B (x, E (), E ())
      end // end of [_, _]
  // end of [siftdn]
} // end of [brauntree_siftdn]

(* ****** ****** *)

implement{elt}
funheap_delmin (hp, res, cmp) = let
  fn delmin {n:pos} (
      t: bt (elt, n), res: &elt? >> elt
    ) :<cloref> bt (elt, n-1) = let
    val+ B (x, t1, t2) = t; val () = res := x in
    case+ t1 of
    | B _ => let
        var x_lrm: elt // uninitialized
        val t1 = brauntree_leftrem<elt> (t1, x_lrm) in
        brauntree_siftdn<elt> (x_lrm, t2, t1, cmp)
      end // end of [B]
    | E () => E ()
  end // end of [demin]
in
  case+ hp of
  | B _ => let
      val () = hp := delmin (hp, res)
      prval () = opt_some {elt} (res) in true (*removed*)
    end // end of [B_]
  | E _ => let
      prval () = opt_none {elt} (res) in false(*notremoved*)
    end // end of [E]
end // end of [funheap_delmin]

(* ****** ****** *)

(* end of [funheap_brauntree.dats] *)
