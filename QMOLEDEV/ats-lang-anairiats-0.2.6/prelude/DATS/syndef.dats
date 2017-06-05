(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // there is no need for dynloading at run-time

(* ****** ****** *)

staload "prelude/SATS/syndef.sats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list.dats"

(* ****** ****** *)

implement{}
atsyndef__list_is_cons (xs) = case+ xs of
  | list_cons _ => true | list_nil () => false
// end of [list_is_cons]

implement{a}
atsyndef__list_uncons (rxs, rx) = let
  val+ list_cons (x, xs) = rxs in rx := x; xs
end // end of [list_uncons_ref]

(* ****** ****** *)

(*
implement{a}
atsyndef__forlist_in_do
  (xs, f) = let
//
  viewtypedef cloptr0_t = a -<cloptr1> void
  viewtypedef cloptr1_t = (!unit_v | a) -<cloptr1> void
//
  val _ptr = __cast (f) where {
    extern castfn __cast (f: !cloptr0_t >> cloptr1_t):<> ptr
  } // end of [val]
//
  prval pf = unit_v ()
  val () = list_foreach_cloptr<a> {unit_v} (pf | xs, f)
  prval unit_v () = pf
//
  val _ptr = __cast (f) where {
    extern castfn __cast (f: !cloptr1_t >> cloptr0_t):<> ptr
  } // end of [val]
//
in
  // empty
end // end of [forlist_in_do]
*)

(* ****** ****** *)

(*
implement{a}
atsyndef__iforlist_in_do
  {n} (xs, f) = let
//
  viewtypedef cloptr0_t = (natLt n, a) -<cloptr1> void
  viewtypedef cloptr1_t = (!unit_v | natLt n, a) -<cloptr1> void
//
  val _ptr = __cast (f) where {
    extern castfn __cast (f: !cloptr0_t >> cloptr1_t):<> ptr
  } // end of [val]
//
  prval pf = unit_v ()
  val () = list_iforeach_cloptr<a> {unit_v} (pf | xs, f)
  prval unit_v () = pf
//
  val _ptr = __cast (f) where {
    extern castfn __cast (f: !cloptr1_t >> cloptr0_t):<> ptr
  } // end of [val]
//
in
  // empty
end // end of [iforlist_in_do]
*)

(* ****** ****** *)

(* end of [syndef.dats] *)
