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
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: February, 2010
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for static loading
#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload "contrib/glib/SATS/glib.sats"

(* ****** ****** *)

implement{a}
g_array_get_elt_at (array, i) = let
  val (pf, fpf | p) = g_array_takeout_tsz {a} (array, i, sizeof<a>)
  val x = !p
  prval () = minus_addback (fpf, pf | array)
in
  x
end // end of [g_array_get_elt_at]

implement{a}
g_array_set_elt_at (array, i, x) = let
  val (pf, fpf | p) = g_array_takeout_tsz {a} (array, i, sizeof<a>)
  val () = !p := x
  prval () = minus_addback (fpf, pf | array)
in
  // nothing
end // end of [g_array_set_elt_at]

(* ****** ****** *)

(* end of [glib.dats] *)
