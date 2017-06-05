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

// some common functions on references

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [reference.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)
//
// HX: implemented in [prelude/CATS/reference.cats]
//
fun ref_make_elt_tsz
  {a:viewt@ype} (x: &a >> a?, tsz: sizeof_t a):<> ref a
  = "atspre_ref_make_elt_tsz"
// end of [ref_make_elt_tsz]

//
// HX:
// [ref] and [ref_make_elt] are the same
// implemented in [prelude/DATS/reference.dats]
//
fun{a:viewt@ype} ref (x: a):<> ref a
fun{a:viewt@ype} ref_make_elt (x: a):<> ref a

//
// HX:
// this is really an identity function; it is
// implemented in [prelude/CATS/reference.cats]
fun ref_make_view_ptr
  {a:viewt@ype} {l:addr} (pf: a @ l | p: ptr l):<> ref a
  = "atspre_ref_make_view_ptr"
// end of [ref_make_view_ptr]

//
// HX: implemented in [prelude/CATS/reference.cats]
//
fun ref_void_make ():<> ref void = "atspre_ref_void_make"

(* ****** ****** *)
//
// HX: Operationally, it is the same as [ref_make_view_ptr]
//
fun refconst_make_view_ptr
  {a:t@ype} {l:addr} (pf: a @ l | p: ptr l):<> refconst a
  = "atspre_ref_make_view_ptr"
// end of [refconst_make_view_ptr]

(* ****** ****** *)
//
// HX: implemented in [prelude/DATS/reference.dats]
//
fun{a:t@ype}
ref_get_elt (r: ref a):<!ref> a = "atspre_ref_get_elt"

//
// HX: implemented in [prelude/DATS/reference.dats]
//
fun{a:t@ype}
ref_set_elt (r: ref a, x: a):<!ref> void = "atspre_ref_set_elt"

(* ****** ****** *)
//
// Operationally, it is the same as [ref_get_elt]
//
fun{a:t@ype} refconst_get_elt (r: refconst a):<> a

(* ****** ****** *)
//
// HX: implemented in [prelude/CATS/reference.cats]
//
castfn ref_get_ptr
  {a:viewt@ype} (r: ref a):<> [l:agz] ptr (l)
castfn ref_get_view_ptr
  {a:viewt@ype} (r: ref a):<> [l:agz] (vbox (a @ l) | ptr l)
  = "atspre_ref_get_view_ptr"
// end of [ref_get_view_ptr]

(* ****** ****** *)
//
// HX: implemented in [prelude/DATS/reference.dats]
//
fun{a:viewt@ype} ref_swap (r: ref a, x: &a):<!ref> void

//
// HX: implemented in [prelude/DATS/reference.dats]
//
fun ref_map {a:viewt@ype} (r: ref a, f: (&a) -<0> void):<!ref> void

(*

macdef ++r = let val x = !r + 1 in r := x; x end
macdef r++ = let val x = !r in r := x + 1; x end

macdef --r = let val x = !r - 1 in r := x; x end
macdef r-- = let val x = !r in r := x - 1; x end

*)

(* ****** ****** *)

(*

//
// HX: should this be added?
//

// extval (ats_ptr_type, "0")
val refopt_none: {a:viewt@ype} refopt (a, false)
  = "atspre_refopt_none"

fun refopt_some {a:viewt@ype} (r: ref a): refopt (a, true)

fun refopt_is_some {a:viewt@ype}
  {b:bool} (r: refopt (a, b)): bool (b)

fun refopt_is_none {a:viewt@ype}
  {b:bool} (r: refopt (a, b)): bool (~b)

*)

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [reference.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [reference.sats] *)
