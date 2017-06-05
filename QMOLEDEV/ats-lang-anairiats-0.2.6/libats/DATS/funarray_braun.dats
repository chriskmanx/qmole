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
** A functional array implementation based on Braun trees
** An functional array as such can also be used as a deque
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: April, 2010 // based on a version done in October, 2008
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)
//
// HX: staloading can actually be skipped as the
// following declared datatype [brauntree] is list-like!
//
#define ATS_DYNLOADFLAG 0 // no dynloading at run-time

(* ****** ****** *)

staload "libats/SATS/funarray_braun.sats"

(* ****** ****** *)

datatype brauntree (a:t@ype+, int) =
  | {n1,n2:nat | n2 <= n1; n1 <= n2+1}
    B (a, n1+n2+1) of (a, brauntree (a, n1), brauntree (a, n2))
  | E (a, 0) of ()
// end of [brauntree]

stadef bt = brauntree // an abbreviation

(* ****** ****** *)

assume array_t0ype_int_type (a:t@ype, n:int) = brauntree (a, n)

(* ****** ****** *)

implement{} funarray_make_nil {a} () = E ()

(* ****** ****** *)

implement{a}
funarray_size (A) = size (A) where {
//
// HX: this algorithm is taken from a paper by Chris Okasaki
//
  fun diff {nl,nr:nat | nr <= nl && nl <= nr+1} .<nr>. 
    (nr: size_t nr, t: bt (a, nl)):<> int (nl-nr) = case+ t of
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
  // end of [diff]
//
  fun size {n:nat} .<n>.
    (t: bt (a, n)):<> size_t n = case+ t of
    | B (_, tl, tr) => let
        val nr = size tr
        val d1 = size1_of_int1 (1 + diff (nr, tl))
      in
        2 * size tr + d1
      end // end of [B]
    | E () => 0
  // end of [size]
} // end of [funarray_size]

(* ****** ****** *)

implement{a}
funarray_get_elt_at
  (A, i) = get_at (A, i) where {
  fun get_at {n,i:nat | i < n} .<n>.
    (t: bt (a, n), i: int i):<> a = begin
    if i > 0 then let
      val i2 = i / 2
    in
      if i > i2 + i2 then let
        val+ B (_, tl, _) = t in get_at (tl, i2)
      end else let
        val+ B (_, _, tr) = t in get_at (tr, i2-1)
      end // end of [if]
    end else let
      val+ B (x, _, _) = t in x
    end // end of [if]
  end (* end of [get_at] *)
} // end of [funarray_get_at]

implement{a}
funarray_set_elt_at
  (A, i, x0) = A := set_at (A, i, x0) where {
  fun set_at {n,i:nat | i < n} .<n>.
    (t: bt (a, n), i: int i, x0: a):<> bt (a, n) =
    if i > 0 then let
      val+ B (x, tl, tr) = t; val i2 = i / 2
    in
      if i > i2 + i2 then begin
        B (x, set_at (tl, i2, x0), tr)
      end else begin
        B (x, tl, set_at (tr, i2-1, x0))
      end // end of [if]
    end else let
      val+ B (_, t1, t2) = t in B (x0, t1, t2)
    end // end of [if]
  // end of [set_at]
} // end of [funarray_set_at]

(* ****** ****** *)

implement{a}
funarray_xch_elt_at
  (A, i, x0) = let
  fun xch_at {n,i:nat | i < n} .<n>.
    (t: bt (a, n), i: int i, x0: &a >> a):<> bt (a, n) =
    if i > 0 then let
      val+ B (x, tl, tr) = t; val i2 = i / 2
    in
      if i > i2 + i2 then begin
        B (x, xch_at (tl, i2, x0), tr)
      end else begin
        B (x, tl, xch_at (tr, i2-1, x0))
      end // end of [if]
    end else let
      val x1 = x0; val+ B (x, t1, t2) = t; val () = x0 := x
    in
      B (x1, t1, t2)
    end // end of [if]
  // end of [xch_at]
  var x0 = x0
  val () = A := xch_at (A, i, x0)
in
  x0 (* return value *)
end // end of [funarray_xch_at]

(* ****** ****** *)

implement{a}
funarray_get_elt_at_exn
  (A, i) = get_at (A, i) where {
  fun get_at {n,i:nat} .<n>. (t: bt (a, n), i: int i):<!exn> a =
    if i > 0 then let
      val i2 = i / 2 in
      if i > i2 + i2 then begin case+ t of
        | B (_, tl, _) => get_at (tl, i2) | _ => $raise SubscriptException()
      end else begin case+ t of
        | B (_, _, tr) => get_at (tr, i2-1) | _ => $raise SubscriptException()
      end // end of [if]
    end else begin case+ t of // i = 0
      | B (x, _, _) => x | _ => $raise SubscriptException
    end // end of [if]
} // end of [funarray_get_at_exn]

implement{a}
funarray_set_elt_at_exn
  (A, i, x0) = A := set_at (A, i, x0) where {
  fun set_at {n,i:nat} .<n>.
    (t: bt (a, n), i: int i, x0: a):<!exn> bt (a, n) =
    if i > 0 then let
      val i2 = i / 2
    in
      if i > i2 + i2 then begin case+ t of
        | B (x, tl, tr) => let
            val tl = set_at (tl, i2, x0) in B (x, tl, tr)
          end // end of [B]
        | _ => $raise SubscriptException
      end else begin case+ t of
        | B (x, tl, tr) => let
            val tr = set_at (tr, i2-1, x0) in B (x, tl, tr)
          end // end of [B]
        | _ => $raise SubscriptException
      end // end of [if]
    end else begin case+ t of // i = 0
      | B (x, tl, tr) => B (x0, tl, tr) | _ => $raise SubscriptException()
    end // end of [if]
} // end of [funarray_set_at_exn]

(* ****** ****** *)

implement{a}
funarray_loadd
  (A, x0) = A := loadd (A, x0) where {
  fun loadd {n:nat} .<n>.
    (t: bt (a, n), x0: a):<> bt (a, n+1) = begin
    case+ t of
    | B (x, tl, tr) => B (x0, loadd (tr, x), tl)
    | E () => B (x0, E (), E ())
  end // end of [loadd]
} // end of [funarray_loadd]

implement{a}
funarray_lorem
  (A) = A := lorem (A) where {
  fun lorem {n:int | n > 0} .<n>.
    (t: bt (a, n)):<> bt (a, n-1) = let
    val+ B (_, tl, tr) = t
  in
    case+ tl of
    | B (xl, _, _) => B (xl, tr, lorem tl) | E () => E ()
  end // end of [lorem]
} // end of [brauntree_lorem]

implement{a}
funarray_lorem_get {n} (A) = let
  fun lorem {n:int | n > 0} .<n>.
    (t: bt (a, n)):<> bt (a, n-1) = let
    val+ B (_, tl, tr) = t
  in
    case+ tl of
    | B (xl, _, _) => B (xl, tr, lorem tl) | E () => E ()
  end // end of [lorem]
  val+ B (x0, tl, tr) = A
  val () = case+
    :(A: array (a, n-1)) => tl of
    | B (xl, _, _) => (A := B (xl, tr, lorem tl))
    | E () => (A := E ())
  // end of [val]
in
  x0 (* return value *)
end // end of [funarray_lorem_get]

(* ****** ****** *)

implement{a}
funarray_hiadd
  (A, n, x0) = A := hiadd (A, n, x0) where {
  fun hiadd {n:nat} .<n>.
    (t: bt (a, n), n: int n, x0: a):<> bt (a, n+1) =
    if n > 0 then let
      val+ B (x, tl, tr) = t; val n2 = n / 2
    in
      if n > n2 + n2 then begin
        B (x, hiadd (tl, n2, x0), tr)
      end else begin
        B (x, tl, hiadd (tr, n2-1, x0))
      end
    end else begin
      B (x0, E (), E ())
    end // end of [if]
} // end of [funarray_hiadd]

implement{a}
funarray_hirem
  (A, n) = A := hirem (A, n) where {
  fun hirem {n:pos} .<n>.
    (t: bt (a, n), n: int n):<> bt (a, n-1) = let
    val+ B (x, tl, tr) = t; val n2 = n / 2
  in
    case+ tl of
    | B _ => begin
        if n > n2 + n2 then begin
          B (x, tl, hirem (tr, n2))
        end else begin
          B (x, hirem (tl, n2), tr)
        end // end of [if]
      end // end of [B]
    | E () => E ()
  end // end of [hirem]
} // end of [funarray_hirem]

implement{a}
funarray_hirem_get (A, n) = let
  fun hirem_get {n:pos} .<n>. (
      t: bt (a, n), n: int n, x0: &a? >> a
    ) :<> bt (a, n-1) = let
    val+ B (x, tl, tr) = t; val n2 = n / 2
  in
    case+ tl of
    | B _ => begin
        if n > n2 + n2 then begin
          B (x, tl, hirem_get (tr, n2, x0))
        end else begin
          B (x, hirem_get (tl, n2, x0), tr)
        end // end of [if]
      end // end of [B]
    | E () => (x0 := x; E ())
  end // end of [hirem_get]
  var x0: a // unintialized
  val () = A := hirem_get (A, n, x0)
in
  x0 (* return value *)
end // end of [funarray_hirem_get]

(* ****** ****** *)

implement{a}
funarray_foreach_vclo {v} {n} (pf | A, n, f) = let
  var i: natLte n
in
  for* {i:nat | i <= n} .<n-i>. // term metric
    (i: int i) => (i := 0; i < n; i := i+1) f (pf | A[i])
end // end of [funarray_foreach_vclo]

implement{a}
funarray_foreach_cloptr
  {n} (A, n, f) = () where {
//
  viewtypedef cloptr0_t = (a) -<cloptr> void
  viewtypedef cloptr1_t = (!unit_v | a) -<cloptr> void
//
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr0_t >> cloptr1_t): void
  } // end of [val]
  prval pfu = unit_v ()
  val () = funarray_foreach_vcloptr<a> {unit_v} (pfu | A, n, f)
  prval unit_v () = pfu
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr1_t >> cloptr0_t): void
  } // end of [val]
} // end of [funarray_foreach_cloptr]
implement{a}
funarray_foreach_vcloptr {v} {n} (pf | A, n, f) = let
  var i: natLte n
in
  for* {i:nat | i <= n} .<n-i>. // term metric
    (i: int i) => (i := 0; i < n; i := i+1) f (pf | A[i])
end // end of [funarray_foreach_vcloptr]

implement{a}
funarray_foreach_cloref {n} (A, n, f) = let
  var i: natLte n
in
  for* {i:nat | i <= n} .<n-i>. // term metric
    (i: int i) => (i := 0; i < n; i := i+1) f (A[i])
end // end of [funarray_foreach_cloref]

(* ****** ****** *)

implement{a}
funarray_iforeach_vclo {v} {n} (pf | A, n, f) = let
  var i: natLte n
in
  for* {i:nat | i <= n} .<n-i>. // term metric
    (i: int i) => (i := 0; i < n; i := i+1) f (pf | i, A[i])
end // end of [funarray_iforeach_vclo]

implement{a}
funarray_iforeach_cloptr
  {n} (A, n, f) = () where {
//
  viewtypedef cloptr0_t = (natLt n, a) -<cloptr> void
  viewtypedef cloptr1_t = (!unit_v | natLt n, a) -<cloptr> void
//
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr0_t >> cloptr1_t): void
  } // end of [val]
  prval pfu = unit_v ()
  val () = funarray_iforeach_vcloptr<a> {unit_v} (pfu | A, n, f)
  prval unit_v () = pfu
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr1_t >> cloptr0_t): void
  } // end of [val]
} // end of [funarray_iforeach_cloptr]
implement{a}
funarray_iforeach_vcloptr {v} {n} (pf | A, n, f) = let
  var i: natLte n
in
  for* {i:nat | i <= n} .<n-i>. // term metric
    (i: int i) => (i := 0; i < n; i := i+1) f (pf | i, A[i])
end // end of [funarray_iforeach_vcloptr]

implement{a}
funarray_iforeach_cloref {n} (A, n, f) = let
  var i: natLte n
in
  for* {i:nat | i <= n} .<n-i>. // term metric
    (i: int i) => (i := 0; i < n; i := i+1) f (i, A[i])
end // end of [funarray_iforeach_cloref]

(* ****** ****** *)

(* end of [funarray_braun.dats] *)
