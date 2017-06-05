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
#include "libc/CATS/pthread_uplock.cats"
%} // end of [%{#]

(* ****** ****** *)
//
// HX: no consideration for errors: any pthread-error leads to SEGFAULTS
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for static loading at run-time

(* ****** ****** *)
//
// linear lock for uploading
//
absviewtype uplock0_viewtype
stadef uplock0 = uplock0_viewtype
absviewtype uplock_view_viewtype (v:view)
stadef uplock = uplock_view_viewtype
absviewtype upticket_view_viewtype (v:view)
stadef upticket = upticket_view_viewtype

(* ****** ****** *)

fun pthread_uplock_create
  (): uplock0 = "atslib_pthread_uplock_create"
// end of [pthread_uplock_create]

fun pthread_uplock_download
  {v:view} (lock: !uplock (v) >> uplock0): (v | void)
  = "atslib_pthread_uplock_download"
// end of [pthread_uplock_download]

fun pthread_uplock_destroy (lock: uplock0): void
  = "atslib_pthread_uplock_destroy"
// end of [pthread_uplock_destroy]

fun pthread_uplock_download_and_destroy {v:view} (lock: uplock v): (v | void)

(* ****** ****** *)

fun pthread_upticket_create
  {v:view} (lock: !uplock0 >> uplock (v)): upticket (v)
  = "atslib_pthread_uplock_upticket_create"
// end of [pthread_upticket_create]

fun pthread_upticket_upload_and_destroy
  {v:view} (pf: v | ticket: upticket v): void
  = "atslib_pthread_uplock_upticket_upload_and_destroy"
// end of [pthread_upticket_upload_and_destroy]

(* ****** ****** *)

(* end of [pthread_uplock.sats] *)
