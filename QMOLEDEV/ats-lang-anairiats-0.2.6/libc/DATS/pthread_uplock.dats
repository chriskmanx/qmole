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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

staload "libc/SATS/pthread_uplock.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

implement
pthread_uplock_download_and_destroy
  (lock) = (pf | ()) where {
  val (pf | ()) = pthread_uplock_download (lock)
  val () = pthread_uplock_destroy (lock)
} // end of [pthread_uplock_download_and_destroy]

(* ****** ****** *)

%{^

/* ****** ****** */

ats_ptr_type
atslib_pthread_uplock_create () {
  ats_pthread_uplock_t *p ;
  p = (ats_pthread_uplock_t*)ATS_MALLOC(sizeof(ats_pthread_uplock_t)) ;
  if (pthread_mutex_init(&p->mutex_res, NULL))
    { ATS_FREE(p) ; ats_crash() ; }
  // end of [if]
  return p ;
} // end of [atslib_pthread_uplock_create]

ats_ptr_type
atslib_pthread_uplock_upticket_create
  (ats_ptr_type p) {
  if (pthread_mutex_lock(&((ats_pthread_upticket_t*)p)->mutex_res))
    ats_crash() ;
  // end of [if]
  return p ;
} // end of [atslib_pthread_uplock_upticket_create]

ats_void_type
atslib_pthread_uplock_upticket_upload_and_destroy
  (ats_ptr_type p) {
  if (pthread_mutex_unlock(&((ats_pthread_upticket_t*)p)->mutex_res))
    ats_crash() ;
  // end of [if]
  return ;
} // end of [atslib_pthread_uplock_upticket_upload_and_destroy]

/* ****** ****** */

ats_void_type
atslib_pthread_uplock_download
  (ats_ptr_type p) {
  if (pthread_mutex_lock(&((ats_pthread_uplock_t*)p)->mutex_res))
    ats_crash() ;
  // end of [if]
  if (pthread_mutex_unlock(&((ats_pthread_upticket_t*)p)->mutex_res))
    ats_crash() ;
  // end of [if]
  return ;
} // end of [atslib_pthread_uplock_download]

/* ****** ****** */

%} // end of [%{^]

(* ****** ****** *)

(* end of [pthread_uplock.dats] *)
