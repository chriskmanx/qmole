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

// some built-in static constants for reference operations

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/reference.sats"

(* ****** ****** *)

(*

assume ref_viewt0ype_type (a:viewt@ype) =
  [l:addr] @(vbox (a @ l) | ptr l)

*)

(* ****** ****** *)

implement{a} ref (x) = begin
  let var x = x in ref_make_elt_tsz {a} (x, sizeof<a>) end
end // end of [ref_make_elt]

// [ref_make_elt] is the same as [ref]
implement{a} ref_make_elt (x) = ref<a> (x)

(* ****** ****** *)

implement{a} ref_get_elt (r) = !r
implement{a} ref_set_elt (r, x) = (!r := x)

(* ****** ****** *)

// implement{a} refconst_get_elt (r) = !r

(* ****** ****** *)

implement{a} ref_swap (r, x) = let
  val (vbox pf | p) = ref_get_view_ptr r; val tmp = !p
in
  !p := x; x := tmp
end // end of [ref_swap]

(* ****** ****** *)

implement ref_map {a} (r, f) = begin
  let val (vbox pf | p) = ref_get_view_ptr r in f !p end
end // end of [ref_map]

(* ****** ****** *)

(* end of [reference.dats] *)
