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

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

abstype
heap_t0ype_type (elt:t@ype+)
stadef heap = heap_t0ype_type

(* ****** ****** *)

typedef cmp (elt:t@ype) = (elt, elt) -<cloref> Sgn
fun{elt:t@ype}
compare_elt_elt (x1: elt, x2: elt, cmp: cmp elt):<> Sgn

(* ****** ****** *)

fun{} funheap_make_nil {elt:t@ype} ():<> heap (elt)

(* ****** ****** *)

fun{elt:t@ype} funheap_size (hp: heap elt): size_t
fun{elt:t@ype} funheap_height (hp: heap elt): Nat

(* ****** ****** *)

fun{elt:t@ype}
funheap_insert (t: &heap (elt), x: elt, cmp: cmp elt):<> void

(* ****** ****** *)

fun{elt:t@ype}
funheap_delmin (
  t: &heap (elt), res: &elt? >> opt (elt, b), cmp: cmp elt
) :<> #[b:bool] bool b // end of [funheap_delim]

(* ****** ****** *)

(* end of [funheap_braun.sats] *)
