(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
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

staload "libc/SATS/stdio.sats" // for [perror]

(* ****** ****** *)

staload "libc/sys/SATS/stat.sats"

(* ****** ****** *)

%{^

/* ****** ****** */

ats_void_type
atslib_chmod_exn (
  ats_ptr_type path, ats_mode_type mode
) {
  int err = chmod ((char*)path, mode) ;
  if (err < 0) {
    perror ("chmod"); ats_exit_errmsg (1, "exit(ATS): [chmod] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_chmod_exn] */

/* ****** ****** */

ats_void_type
atslib_mkdir_exn (
  ats_ptr_type path, ats_mode_type mode
) {
  int err = mkdir ((char*)path, mode) ;
  if (err < 0) {
    perror ("mkdir"); ats_exit_errmsg (1, "exit(ATS): [mkdir] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_mkdir_exn] */

/* ****** ****** */

ats_void_type
atslib_stat_exn (
  ats_ptr_type name, ats_ptr_type buf
) {
  int err ;
  err = stat ((char*)name, (ats_stat_type*)buf) ;
  if (err < 0) {
    perror ("stat"); ats_exit_errmsg (1, "exit(ATS): [stat] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_stat_exn] */

/* ****** ****** */

ats_void_type
atslib_fstat_exn (
  ats_int_type fd, ats_ptr_type buf
) {
  int err ;
  err = fstat (fd, (ats_stat_type*)buf) ;
  if (err < 0) {
    perror ("fstat"); ats_exit_errmsg (1, "exit(ATS): [fstat] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_fstat_exn] */

/* ****** ****** */

ats_void_type
atslib_lstat_exn (
  ats_ptr_type name, ats_ptr_type buf
) {
  int err ;
  err = lstat ((char*)name, (ats_stat_type*)buf) ;
  if (err < 0) {
    perror ("lstat"); ats_exit_errmsg (1, "exit(ATS): [lstat] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_lstat_exn] */

/* ****** ****** */

%} // end of [%{^]

(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"

implement
isfdtype (fd, m0) = let
  var stbuf: stat?
  val err = fstat_err (fd, stbuf)
in
  if (err >= 0) then let
    prval () = opt_unsome {stat} (stbuf)
    val m1 = $T.lor_mode_mode (stbuf.st_mode, S_IFMT)
  in
    if $T.eq_mode_mode (m0, m1) then 1 else 0
  end else let
    prval () = opt_unnone {stat} (stbuf)
  in
    ~1 (* error indication *)
  end // end of [if]
end // end of [isfdtype]

(* ****** ****** *)

(* end of [stat.dats] *)
