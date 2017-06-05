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

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

%{^
extern char **environ ; // in [unistd.h]
extern void _exit (int status) ; // in [unistd.h]
extern pid_t wait (int*) ; // in [sys/wait.h]
%} // end of [%{^]

(* ****** ****** *)

staload TYPES = "libc/sys/SATS/types.sats"

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/stdio.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

implement
fork_exn () = pid where {
  val pid = fork_err ()
  val iserr = $TYPES.int_of_pid(pid) < 0
  val () = if iserr then let
    val errno = int_of (errno_get ())
    val () = perror ("fork")
  in
    exit_errmsg (errno, "exit(ATS): [fork] failed.\n")
  end // end of [val]
} // end of [atslib_fork_exn]

(* ****** ****** *)

%{^

ats_void_type
atslib_fork_exec_cloptr_exn
  (ats_ptr_type f_child) {
  pid_t pid ;
  pid = fork () ;
//
  if (pid < 0) {
    ats_exit_errmsg (errno, "exit(ATS): [fork] failed.\n") ;
  } // end of [if]
//
  /* this is the parent */
  if (pid > 0) { ATS_FREE (f_child) ; return ; }
//  
  /* this is the child */
  ((ats_void_type (*)(ats_clo_ptr_type))((ats_clo_ptr_type)f_child)->closure_fun)(f_child) ;
  _exit (0) ; /* no need to flush STDIN, STDOUT and STDERR */
//
  return ; /* deadcode */
} /* end of [atslib_fork_exec_cloptr] */

/* ****** ****** */

ats_int_type
atslib_fork_exec_and_wait_cloptr_exn
  (ats_ptr_type f_child) {
  pid_t pid ;
  int status ;
//
  pid = fork () ;
//
  if (pid < 0) {
    ats_exit_errmsg (errno, "exit(ATS): [fork] failed.\n") ;
  } // end of [if]
//
  if (pid > 0) {
    ATS_FREE (f_child) ;
    if (wait (&status) < 0) return -1 ;
    return status ;
  } // end of [if]
//
  /* this is the child */
  ((ats_void_type (*)(ats_clo_ptr_type))((ats_clo_ptr_type)f_child)->closure_fun)(f_child) ;
  _exit (0) ; /* no need to flush STDIN, STDOUT and STDERR */
//
  return 0 ; /* deadcode */
} /* atslib_fork_exec_and_wait_cloptr_exn */

%} // end of [%{^]

/* ****** ****** */

%{^

#define atslib_GETCWD_BUFSZ 64

ats_ptr_type
atslib_getcwd0 () {
  char *buf, *res ;
  int sz = atslib_GETCWD_BUFSZ ;
//
  buf = (char*)ats_malloc_gc(atslib_GETCWD_BUFSZ) ;
//
// HX: this strategy may not be so attractive;
// an alternative is to use pathconf to get the maximum pathname length
//
  while (1) {
    res = getcwd (buf, sz) ;
    if (!res) {
      ATS_FREE (buf) ; sz = sz + sz ; buf = ATS_MALLOC (sz) ;
      continue ;
    } // end of [if]
    break ;
  } // end of [while]
//
  return buf ;
} // end of [atslib_getcwd0]

%} // end of [%{^]

(* ****** ****** *)

%{^

ats_int_type
atslib_pipe (
  ats_ptr_type pfd1, ats_ptr_type pfd2
) {
  int err ;
  int pfd[2] ;
  err = pipe(pfd) ;
  if (err==0) {
    *(int*)pfd1 = pfd[0] ; *(int*)pfd2 = pfd[1] ;
  } // end of [if]
  return err ;
} // end of [atslib_pipe]

%} // end of [%{^]

(* ****** ****** *)

%{^

ats_ptr_type
atslib_environ_get_arrsz
  (size_t *sizep) {
  *sizep = atspre_ptrarr_size (environ) ;
  return environ;
} // end of [atslib_environ_get_arrsz]

%} // end of [%{^]

(* ****** ****** *)

(* end of [unistd.dats] *)
