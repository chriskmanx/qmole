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
** A functional map implementation based on ordered lists
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: May 18, 2011
**
*)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading at run-time

(* ****** ****** *)

staload
_(*anon*) = "prelude/DATS/list.dats"

(* ****** ****** *)

staload "libats/SATS/funset_listord.sats"

(* ****** ****** *)
//
// a specialized version can be implemented on the spot
//
implement{elt} compare_elt_elt (x1, x2, cmp) = cmp (x1, x2)
//
(* ****** ****** *)

assume
set_t0ype_type (elt: t@ype) = List (elt)

(* ****** ****** *)

implement{} funset_make_nil () = list_nil ()

implement{a} funset_make_sing (x) = list_cons (x, list_nil)

implement{a}
funset_make_list (xs, cmp) = let
  var env: ptr = null
  fn cmp (x1: a, x2: a, env: !ptr):<> int = cmp (x1, x2)
in
  list_mergesort (xs, cmp, env)
end // end of [funset_make_list]

(* ****** ****** *)

implement{a}
funset_size (xs) = size1_of_int1 (list_length (xs))

(* ****** ****** *)

implement{a}
funset_is_member
  (xs, x0, cmp) = let
  fun aux {n:nat} .<n>.
    (xs: list (a, n)):<cloref> bool = case+ xs of
    | list_cons (x, xs) => let
        val sgn = compare_elt_elt (x0, x, cmp) in
        if sgn < 0 then false else (if sgn > 0 then aux (xs) else true)
      end // end of [list_cons]
    | list_nil () => false
  // end of [aux]
in
  aux (xs)
end // end of [funset_is_member]

implement{a}
funset_isnot_member (xs, x0, cmp) = not (funset_is_member (xs, x0, cmp))

(* ****** ****** *)

implement{a}
funset_insert
  (xs, x0, cmp) = let
  fun aux {n:nat} .<n>. (
    xs: list (a, n), flag: &int
  ) :<cloref> List (a) =
    case+ xs of
    | list_cons (x, xs1) => let
        val sgn = compare_elt_elt (x0, x, cmp)
      in
        if sgn < 0 then let
          val () = flag := flag + 1 in list_cons (x0, xs)
        end else if sgn > 0 then let
          val flag0 = flag
          val xs1 = aux (xs1, flag)
        in
          if flag = flag0 then xs else list_cons (x, xs1)
        end else xs // end of [if]
      end // end of [list_cons]
    | list_nil () => let
        val () = flag := flag + 1 in list_cons (x0, list_nil)
      end // end of [val]
  // end of [val]
  var flag: int = 0
  val () = xs := aux (xs, flag)
in
  if flag = 0 then true else false
end // end of [funset_insert]

(* ****** ****** *)

implement{a}
funset_remove (xs, x0, cmp) = let
  fun aux {n:nat} .<n>. (
    xs: list (a, n), flag: &int
  ) :<cloref> List (a) =
    case xs of
    | list_cons (x, xs1) => let
        val sgn = compare_elt_elt (x0, x, cmp)
      in
        if sgn < 0 then xs
        else if sgn > 0 then let
          val flag0 = flag
          val xs1 = aux (xs1, flag)
        in
          if flag = flag0 then xs else list_cons (x, xs1)
        end else let
          val () = flag := flag + 1 in xs1
        end (* end of [if] *)
      end // end of [list_cons]
    | list_nil () => list_nil ()
  // end of [aux]
  var flag: int = 0
  val () = xs := aux (xs, flag)
in
  if flag > 0 then true else false
end // end of [funset_remove]

(* ****** ****** *)

implement{a}
funset_union
  (xs1, xs2, cmp) = let
  fun aux {n1,n2:nat} .<n1+n2>. (
    xs1: list (a, n1), xs2: list (a, n2)
  ) :<cloref> List (a) =
  case xs1 of
  | list_cons (x1, xs11) => (
      case+ xs2 of
      | list_cons (x2, xs21) => let
          val sgn = compare_elt_elt (x1, x2, cmp)
        in
          if sgn < 0 then
            list_cons (x1, aux (xs11, xs2))
          else if sgn > 0 then
            list_cons (x2, aux (xs1, xs21))
          else
            list_cons (x1, aux (xs11, xs21))
          // end of [if]
        end // end of [list_cons]
      | list_nil () => xs1
    ) // end of [list_cons]
  | list_nil () => xs2
in
  aux (xs1, xs2)
end // end of [funset_union]

implement{a}
funset_intersect
  (xs1, xs2, cmp) = let
  fun aux {n1,n2:nat} .<n1+n2>. (
    xs1: list (a, n1), xs2: list (a, n2)
  ) :<cloref> List (a) =
  case xs1 of
  | list_cons (x1, xs11) => (
      case+ xs2 of
      | list_cons (x2, xs21) => let
          val sgn = compare_elt_elt (x1, x2, cmp)
        in
          if sgn < 0 then
            aux (xs11, xs2)
          else if sgn > 0 then
            aux (xs1, xs21)
          else
            list_cons (x1, aux (xs11, xs21))
          // end of [if]
        end // end of [list_cons]
      | list_nil () => list_nil ()
    ) // end of [list_cons]
  | list_nil () => list_nil ()
in
  aux (xs1, xs2)
end // end of [funset_intersect]

implement{a}
funset_diff
  (xs1, xs2, cmp) = let
  fun aux {n1,n2:nat} .<n1+n2>. (
    xs1: list (a, n1), xs2: list (a, n2)
  ) :<cloref> List (a) =
  case xs1 of
  | list_cons (x1, xs11) => (
      case+ xs2 of
      | list_cons (x2, xs21) => let
          val sgn = compare_elt_elt (x1, x2, cmp)
        in
          if sgn < 0 then
            list_cons (x1, aux (xs11, xs2))
          else if sgn > 0 then
            aux (xs1, xs21)
          else
            aux (xs11, xs21)
          // end of [if]
        end // end of [list_cons]
      | list_nil () => xs1
    ) // end of [list_cons]
  | list_nil () => xs2
in
  aux (xs1, xs2)
end // end of [funset_diff]

implement{a}
funset_symdiff
  (xs1, xs2, cmp) = let
  fun aux {n1,n2:nat} .<n1+n2>. (
    xs1: list (a, n1), xs2: list (a, n2)
  ) :<cloref> List (a) =
  case xs1 of
  | list_cons (x1, xs11) => (
      case+ xs2 of
      | list_cons (x2, xs21) => let
          val sgn = compare_elt_elt (x1, x2, cmp)
        in
          if sgn < 0 then
            list_cons (x1, aux (xs11, xs2))
          else if sgn > 0 then
            list_cons (x2, aux (xs1, xs21))
          else
            aux (xs11, xs21)
          // end of [if]
        end // end of [list_cons]
      | list_nil () => xs1
    ) // end of [list_cons]
  | list_nil () => xs2
in
  aux (xs1, xs2)
end // end of [funset_symdiff]

(* ****** ****** *)

implement{a}
funset_is_subset
  (xs1, xs2, cmp) = let
  fun aux {n1,n2:nat} .<n1+n2>. (
    xs1: list (a, n1), xs2: list (a, n2)
  ) :<cloref> bool =
    case+ xs1 of
    | list_cons (x1, xs11) => (
      case+ xs2 of
      | list_cons (x2, xs21) => let
          val sgn = compare_elt_elt (x1, x2, cmp)
        in
          if sgn < 0 then false
          else if sgn > 0 then aux (xs1, xs21)
          else aux (xs11, xs21)
        end
      | list_nil () => false
      ) // end of [list_cons]
    | list_nil () => true
in
  aux (xs1, xs2)
end // end of [funset_is_subset]

implement{a}
funset_is_equal
  (xs1, xs2, cmp) = let
  fun aux {n1,n2:nat} .<n1+n2>. (
    xs1: list (a, n1), xs2: list (a, n2)
  ) :<cloref> bool =
    case+ xs1 of
    | list_cons (x1, xs11) => (
      case+ xs2 of
      | list_cons (x2, xs21) => let
          val sgn = compare_elt_elt (x1, x2, cmp)
        in
          if sgn = 0 then aux (xs11, xs21) else false
        end
      | list_nil () => false
      ) // end of [list_cons]
    | list_nil () => (case+ xs2 of
      | list_cons _ => false | list_nil () => true
      )
  // end of [aux]
in
  aux (xs1, xs2)
end // end of [funset_is_subset]

(* ****** ****** *)

implement funset_listize (xs) = xs

(* ****** ****** *)

(* end of [funset_listord.sats] *)
