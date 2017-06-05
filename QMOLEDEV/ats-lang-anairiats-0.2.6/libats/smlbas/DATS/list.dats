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
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: List (http://www.standardml.org/Basis/list.html)
//

(* ****** ****** *)

staload "libats/smlbas/SATS/general.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/list.sats"

(* ****** ****** *)

implement null (xs) =
  case+ xs of list0_nil _ => true | list0_cons _ => false
// end of [null]

implement{a} length (xs) = list0_length<a> (xs)

implement{a}
append (xs, ys) = list0_append<a> (xs, ys) // tail-recursive

implement{a} hd (xs) = case+ xs of
  | list0_cons (x, _) => x | list0_nil () => $raise Empty ()
// end of [hd]

implement{a} tl (xs) = case+ xs of
  | list0_cons (_, xs) => xs | list0_nil () => $raise Empty ()
// end of [hd]

implement{a} last (xs) = let
  fun loop (x: a, xs: list0 a): a = case+ xs of
    | list0_cons (x, xs) => loop (x, xs) | list0_nil () => x
  // end of [loop]
in
  case xs of
  | list0_cons (x, xs) => loop (x, xs)
  | list0_nil () => $raise Empty ()
end (* end of [last] *)

implement{a} getItem (xs) = case+ xs of
  | list0_cons (x, xs) => option0_some @(x, xs)
  | list0_nil () => option0_none ()
// end of [getItem]

(* ****** ****** *)

implement{a} nth (xs, i) = try
  list0_nth_exn<a> (xs, i) // [prelude/DATS/list0.dats]
with
  | ~ListSubscriptException () => $raise Subscript ()
// end of [nth]

(* ****** ****** *)

implement{a} take (xs, i) = try
  list0_take_exn<a> (xs, i) // [prelude/DATS/list0.dats]
with
  | ~ListSubscriptException () => $raise Subscript ()
// end of [take] 

implement{a} drop (xs, i) = try
  list0_drop_exn<a> (xs, i) // [prelude/DATS/list0.dats]
with
  | ~ListSubscriptException () => $raise Subscript ()
// end of [drop] 

implement{a} rev (xs) = list0_reverse<a> (xs)
implement{a} revAppend (xs, ys) = list0_reverse_append<a> (xs, ys)

implement{a} concat (xss) = list0_concat<a> (xss)

(* ****** ****** *)

implement{a}
app (f, xs) = loop (f, xs) where {
  fun loop (f: a -<cloref1> void, xs: list0 a): void = case+ xs of
    | list0_cons (x, xs) => (f x; loop (f, xs)) | list0_nil () => ()
  // end of [loop] 
} // end of [app]

(* ****** ****** *)

implement{a}{b}
map (f, xs) = list0_map_cloref<a><b> (xs, f) 

implement{a}{b}
mapPartial (f, xs) = res where {
  typedef res_t = list0 b
  fun loop (
      f: a -<cloref1> option0 b, xs: list0 a, res: &res_t? >> res_t
    ) : void = case+ xs of
    | list0_cons (x, xs) => begin case+ f (x) of
      | option0_some y => let
          val () = res := list0_cons (y, ?)
          val+ list0_cons (_, !p_res1) = res
        in
          loop (f, xs, !p_res1); fold@ (res)
        end // end of [option0_some]
      | option0_none () => loop (f, xs, res)
      end // end of [list0_cons]
    | list0_nil () => res := list0_nil ()  
  // end of [loop]
  var res: res_t; val () = loop (f, xs, res)
} // end of [mapPartial]  

(* ****** ****** *)

implement{a}
find (f, xs) = loop (f, xs) where {
  fun loop (f: a -<cloref1> bool, xs: list0 a): option0 a =
    case+ xs of
    | list0_cons (x, xs) =>
        if f (x) then option0_some x else loop (f, xs)
    | list0_nil () => option0_none ()
  // end of [loop]  
} // end of [find]

implement{a} filter (f, xs) = list0_filter_cloref<a> (xs, f)

(* ****** ****** *)

implement{a}
partition (f, xs) = (res_p, res_n) where {
  typedef res_t = list0 a
  fun loop (
      f: a -<cloref1> bool
    , xs: list0 a
    , res_p: &res_t? >> res_t 
    , res_n: &res_t? >> res_t
    ) : void = case+ xs of
    | list0_cons (x, xs) => begin
        if f (x) then let
          val () = res_p := list0_cons (x, ?)
          val+ list0_cons (_, !p_res1_p) = res_p
        in
          loop (f, xs, !p_res1_p, res_n); fold@ res_p
        end else let
          val () = res_n := list0_cons (x, ?)
          val+ list0_cons (_, !p_res1_n) = res_n
        in
          loop (f, xs, res_p, !p_res1_n); fold@ res_n
        end // end of [if]  
      end (* end of [list0_cons] *)
    | list0_nil () => begin
        res_p := list0_nil (); res_n := list0_nil ()
      end // end of [list0_nil]
  // end of [loop]
  var res_p: res_t and res_n: res_t // uninitialized
  val () = loop (f, xs, res_p, res_n)
} // end of [partition]

(* ****** ****** *)

implement{a,b}
foldl (f, ini, xs) = loop (f, ini, xs) where {
  fun loop (
    f: (a, b) -<cloref1> b, ini: b, xs: list0 a
  ) : b = case+ xs of
    | list0_cons (x, xs) => loop (f, f (x, ini), xs) | list0_nil () => ini
  // end of [loop]  
} // end of [foldl]

implement{a,b}
foldr (f, snk, xs) = list0_fold_right<a><b> (f, xs, snk)

(* ****** ****** *)

implement{a}
all (f, xs) = loop (f, xs) where {
  fun loop (f: a -<cloref1> bool, xs: list0 a): bool =
    case+ xs of
    | list0_cons (x, xs) => if f (x) then loop (f, xs) else false
    | list0_nil () => true
  // end of [loop]  
} // end of [all]

implement{a}
exists (f, xs) = loop (f, xs) where {
  fun loop (f: a -<cloref1> bool, xs: list0 a): bool =
    case+ xs of
    | list0_cons (x, xs) => if f (x) then true else loop (f, xs)
    | list0_nil () => false
  // end of [loop]  
} // end of [exists]

(* ****** ****** *)

implement{a}
tabulate (lsz, f) = let
  typedef res_t = list0 a
  fun loop {n,i:nat | i <= n} .<n-i>. (
      n: int n, i: int i, f: int -<cloref1> a, res: &res_t? >> res_t
    ) : void =
    if i < n then let
      val () = res := list0_cons (f i, ?)
      val+ list0_cons (_, !p_res1) = res
    in  
      loop (n, i+1, f, !p_res1); fold@ (res)
    end else begin
      res := list0_nil () // loop exits
    end // end of [if]  
  // end of [loop]
  val lsz = int1_of_int (lsz)
in
  if lsz >= 0 then let
    var res: res_t in loop (lsz, 0, f, res); res
  end else begin
    $raise Size ()
  end // end of [if]
end // end of [tabulate]

(* ****** ****** *)

implement{a}
collate (cmp, xs, ys) = loop (cmp, xs, ys) where {
  fun loop (cmp: (a, a) -<cloref1> int, xs: list0 a, ys: list0 a): int =
    case+ (xs, ys) of
    | (list0_cons (x, xs), list0_cons (y, ys)) => let
        val sgn = cmp (x, y) in if sgn = 0 then loop (cmp, xs, ys) else sgn
      end // end of [list0_cons, list0_cons]   
    | (list0_cons _, list0_nil ()) =>  1
    | (list0_nil (), list0_cons _) => ~1
    | (list0_nil (), list0_nil ()) =>  0
  // end of [loop]
} // end of [collate]

(* ****** ****** *)

(* end of [list.dats] *)
