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

staload STDIO = "libc/SATS/stdio.sats"

(* ****** ****** *)

staload "libc/SATS/pthread.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

(*
fun pthread_create_detached_cloptr
  (f: () -<cloptr1> void): void // closure must be freed to avoid leak!
// end of [pthread_create_detached_cloptr]
*)

implement
pthread_create_detached_exn
  {vt} (f, env) = let
  val ret = pthread_create_detached (f, env)
in
  if :(env: vt?) =>
    (ret = 0) then let
    prval () = opt_unnone {vt} (env)
  in
    // nothing
  end else let
    prval () = opt_unsome {vt} (env)
    prval () = __assert (env) where {
      extern prfun __assert (x: !vt >> vt?):<> void
    } // end of [prval]
    val () = $STDIO.perror ("pthread_create")
  in
    exit_errmsg (1, "exit(ATS): [pthread_create] failed\n")
  end // end of [if]
end // end of [pthread_create_detached_exn]

implement
pthread_create_detached_cloptr (f) = let
  fun app (f: () -<lincloptr1> void): void = (f (); cloptr_free (f))
in
  pthread_create_detached_exn(app, f)
end // end of [pthread_create_detached_cloptr]

(* ****** ****** *)

%{^

/* ****** ****** */

ats_int_type
atslib_pthread_mutex_init_locked
  (ats_ptr_type p) {
  int err ;
  err = pthread_mutex_init((pthread_mutex_t*)p, NULL) ;
  if (err) return err ;
  err = pthread_mutex_lock((pthread_mutex_t*)p) ;
  if (err) {
     pthread_mutex_destroy((pthread_mutex_t*)p) ; return err ;
  } // end of [if]
  return 0 ;
} // end of [atslib_pthread_mutex_init_locked]

ATSinline()
ats_int_type
atslib_pthread_mutex_init_unlocked
  (ats_ptr_type p) {
  int err = pthread_mutex_init((pthread_mutex_t*)p, NULL) ;
  return err ;
} // end of [atslib_pthread_mutex_init_unlocked]

/* ****** ****** */

ats_ptr_type
atslib_pthread_mutex_create_locked () {
  int err ;
  pthread_mutex_t *p ;
  p = (pthread_mutex_t*)ATS_MALLOC(sizeof (pthread_mutex_t)) ;
  err = atslib_pthread_mutex_init_locked(p) ;
  if (err) {
    ATS_FREE(p) ; return (pthread_mutex_t*)0 ;
  } // end of [if]
  return p ;
} // end of [atslib_pthread_mutex_create_locked]

ats_ptr_type
atslib_pthread_mutex_create_unlocked () {
  int err ;
  pthread_mutex_t *p ;
  p = (pthread_mutex_t*)ATS_MALLOC(sizeof (pthread_mutex_t)) ;
  err = atslib_pthread_mutex_init_unlocked(p) ;
  if (err) {
    ATS_FREE(p) ; return (pthread_mutex_t*)0 ;
  } // end of [if]
  return p ;
} // end of [atslib_pthread_mutex_create_unlocked]

/* ****** ****** */

ats_ptr_type
atslib_pthread_cond_create () {
  pthread_cond_t *p ;
  p = (pthread_cond_t*)ATS_MALLOC(sizeof (pthread_cond_t)) ;
  if (pthread_cond_init(p, NULL)) {
    ATS_FREE(p) ; return (pthread_cond_t*)0 ;
  } // end of [if]
  return p ;
} // end of [atslib_pthread_cond_create]

/* ****** ****** */

%} // end of [%{^]

(* ****** ****** *)

(* end of [pthread.dats] *)
