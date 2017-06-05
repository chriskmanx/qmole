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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#
#include "libc/CATS/pthread_upbarr.cats"
%} // end of [%{#]

(* ****** ****** *)
//
// HX: any pthread-error leads to CRASH: [ats_crash] is called
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for static loading at run-time

(* ****** ****** *)
//
// linear barrier for uploading
//
absviewtype upbarr_view_viewtype (v:view)
stadef upbarr = upbarr_view_viewtype
//
// HX: [upticket] is declared in pthread_uplock.sats as well
//
absviewtype upticket_view_viewtype (v:view)
stadef upticket = upticket_view_viewtype

(* ****** ****** *)

prfun pthread_upbarr_trans
  {v1:view} {v2:view}
  (f: v1 -<prf> v2 | barr: !upbarr (v1) >> upbarr (v2)): void
// end of [pthread_upbarr_trans]

prfun pthread_upbarr_unitelim
  {v:view} (barr: !upbarr @(unit_v, v) >> upbarr (v)): void
// end of [pthread_upbarr_unitelim]

(* ****** ****** *)

viewtypedef upbarr0 = upbarr (unit_v)

fun pthread_upbarr_create
  (): upbarr (unit_v) = "atslib_pthread_upbarr_create"
// end of [pthread_upbarr_create]

fun pthread_upbarr_download {v:view}
  (barr: !upbarr (v) >> upbarr0): (v | void) = "atslib_pthread_upbarr_download"
// end of [pthread_upbarr_download]

fun pthread_upbarr_destroy
  (barr: upbarr0): void = "atslib_pthread_upbarr_destroy"
// end of [pthread_upbarr_destroy]

fun pthread_upbarr_download_and_destroy {v:view} (barr: upbarr (v)): (v | void)

(* ****** ****** *)

fun pthread_upticket_create
  {v1:view} {v2:view} (barr: !upbarr (v1) >> upbarr @(v1, v2)): upticket (v2)
  = "atslib_pthread_upbarr_upticket_create"
// end of [pthread_upticket_create]

fun pthread_upticket_upload_and_destroy
  {v:view} (pf: v | ticket: upticket v): void
  = "atslib_pthread_upbarr_upticket_upload_and_destroy"
// end of [pthread_upticket_upload_and_destroy]

(* ****** ****** *)

(* end of [pthread_upbarr.sats] *)
