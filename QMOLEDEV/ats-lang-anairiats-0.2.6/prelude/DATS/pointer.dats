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

//
// some common pointer operations
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/pointer.sats"

(* ****** ****** *)
//
// HX: this is a proof function
//
implement ptr_is_gtez {l} (p) = addr_is_gtez {l} ()

(* ****** ****** *)

implement{a} ptr_alloc () = ptr_alloc_tsz {a} (sizeof<a>)

(* ****** ****** *)

implement{a}
ptr_zero (pf | x) = ptr_zero_tsz {a} (pf | x, sizeof<a>)

(* ****** ****** *)

implement{a} ptr_get_t (pf | p) = !p
implement{a} ptr_set_t (pf | p, x) = (!p := x)
implement{a} ptr_move_t (pf1, pf2 | p1, p2) = (!p2 := !p1)

(* ****** ****** *)

implement{a} ptr_get_vt (pf | p) = !p
implement{a} ptr_set_vt (pf | p, x) = (!p := x)
implement{a} ptr_move_vt (pf1, pf2 | p1, p2) = (!p2 := !p1)

(* ****** ****** *)

implement{a} ptr_get_inv (pf | p) = !p
implement{a} ptr_set_inv (pf | p, x) = (!p := x)

(* ****** ****** *)

(* end of [pointer.dats] *)
