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
#include "libc/CATS/fcntl.cats"
%} // end of [%{#]

(* ****** ****** *)

staload TYPES = "libc/sys/SATS/types.sats"
typedef mode_t = $TYPES.mode_t

(* ****** ****** *)

abst@ype
flag_t = $extype"ats_fcntlflag_type"
(*
castfn flag_of_uint (u: uint):<> flag_t
*)
castfn uint_of_flag (f: flag_t):<> uint
overload uint_of with uint_of_flag
abst@ype disjflag_t = flag_t // for [lor]
abst@ype conjflag_t = flag_t // for [land] // for masks

(* ****** ****** *)

macdef O_RDONLY = $extval (flag_t, "O_RDONLY")
macdef O_WRONLY = $extval (flag_t, "O_WRONLY")
macdef O_RDWR   = $extval (flag_t, "O_RDWR")

macdef O_CREAT = $extval (disjflag_t, "O_CREAT")
macdef O_APPEND = $extval (disjflag_t, "O_APPEND")

macdef O_EXCL = $extval (disjflag_t, "O_EXCL")
macdef O_NOCTTY = $extval (disjflag_t, "O_NOCTTY")
macdef O_NONBLOCK = $extval (disjflag_t, "O_NONBLOCK")
macdef O_SYNC = $extval (disjflag_t, "O_SYNC")
macdef O_TRUNC = $extval (disjflag_t, "O_TRUNC")

(*
macdef O_NDELAY
macdef O_NOFOLLOW
macdef O_DIRECTORY
macdef O_DIRECT
macdef O_ASYNC
macdef O_LARGEFILE
*)

fun lnot_disjflag
  (df: disjflag_t): conjflag_t = "atslib_lnot_disjflag"
overload ~ with lnot_disjflag

fun lor_flag_disjflag
  (f: flag_t, df: disjflag_t): flag_t = "atslib_lor_flag_disjflag"
overload lor with lor_flag_disjflag

fun land_flag_conjflag
  (f: flag_t, cf: conjflag_t): flag_t = "atslib_land_flag_conjflag"
overload land with land_flag_conjflag

(* ****** ****** *)

absview fildes_v (int) // file descriptor view

(* ****** ****** *)

dataview open_v (int) =
  | {i:nat} open_v_succ (i) of fildes_v (i) | open_v_fail (~1) of ()
// end of [open_v]

fun open_flag_err
  (path: !READ(string), flag: flag_t): [i: int] (open_v (i) | int i)
  = "atslib_open_flag_err"
// end of [open_flag_err]

fun open_flag_mode_err (
  path: !READ(string), flag: flag_t, mode: mode_t
) : [i: int] (open_v (i) | int i)
  = "atslib_open_flag_mode_err"
// end of [open_flag_mode_err]

fun open_flag_exn
  (path: !READ(string), flag: flag_t): [i: int] (fildes_v i | int i)
  = "atslib_open_flag_exn"
// end of [open_flag_exn]

fun open_flag_mode_exn
  (path: !READ(string), flag: flag_t, mode: mode_t)
  : [i: int] (fildes_v (i) | int i) = "atslib_open_flag_mode_exn"
// end of [open_flag_mode_exn]

(* ****** ****** *)

dataview close_v (fd: int, int) =
  | close_v_succ (fd,  0) of () | close_v_fail (fd, ~1) of fildes_v (fd)
// end of [close_v]

fun close_err {fd:int}
  (pf: fildes_v (fd) | fd: int fd)
  : [i:int] (close_v (fd, i) | int i)
  = "atslib_close_err"
// end of [close_err]

fun close_exn {fd:int}
  (pf: fildes_v (fd) | fd: int fd): void = "atslib_close_exn"
// end of [close_exn]

//
// HX: implemented in [libc/DATS/fcntl.dats]
//
fun close_loop_err {fd:int}
  (pf: fildes_v (fd) | fd: int fd)
  :<> [i:int] (close_v (fd, i) | int i)
// end of [close_loop_err]

//
// HX: implemented in [libc/DATS/fcntl.dats]
//
fun close_loop_exn {fd:int}
  (pf: fildes_v (fd) | fd: int fd): void
// end of [close_loop_exn]

(* ****** ****** *)
//
// HX: implemented in [libc/CATS/fcntl.cats]
//
fun read_err
  {fd:int} {sz,n:nat | n <= sz} (
  pf: !fildes_v (fd)
| fd: int fd, buf: &b0ytes(sz) >> bytes(sz), ntotal: size_t n
) : ssizeBtw(~1, n+1) = "atslib_fildes_read_err"
// end of [read_err]

fun read_exn
  {fd:int} {sz,n:nat | n <= sz} (
  pf: !fildes_v (fd)
| fd: int fd, buf: &b0ytes(sz) >> bytes(sz), ntotal: size_t n
) : sizeLte n = "atslib_fildes_read_exn"
// end of [read_exn]

(* ****** ****** *)
//
// HX:
// this one is implemented in [libc/DATS/fcntl.dats]
// note that it is used only when it is known ahead how many bytes are expected;
// otherwise, there is the risk of forever blocking!!!
//
fun read_all_err
  {fd:int} {sz,n:nat | n <= sz} (
  pf: !fildes_v (fd) | fd: int fd, buf: &bytes sz, ntotal: size_t n
) : ssizeBtw (~1, n+1) = "atslib_fildes_read_all_err"
// end of [read_all_err]

fun read_all_exn
  {fd:int} {sz,n:nat | n <= sz} (
  pf: !fildes_v (fd) | fd: int fd, buf: &bytes sz, ntotal: size_t n
) : sizeLte n = "atslib_fildes_read_all_exn"
// end of [read_all_exn]

(* ****** ****** *)
//
// HX: implemented in [libc/CATS/fcntl.cats]
//
fun write_err
  {fd:int} {sz,n:nat | n <= sz} (
  pf: !fildes_v (fd) | fd: int fd, buf: &bytes sz, ntotal: size_t n
) : ssizeBtw(~1, n+1) = "atslib_fildes_write_err"
// end of [write_err]

fun write_exn
  {fd:int} {sz,n:nat | n <= sz} (
  pf: !fildes_v (fd) | fd: int fd, buf: &bytes sz, ntotal: size_t n
) : sizeLte n = "atslib_fildes_write_exn"
// end of [write_exn]

(* ****** ****** *)
//
// HX: implemented in [libc/DATS/fcntl.dats]
//
fun write_all_err
  {fd:int} {sz,n:nat | n <= sz} (
  pf: !fildes_v (fd) | fd: int fd, buf: &bytes sz, ntotal: size_t n
) : ssizeBtw(~1, n+1) = "atslib_fildes_write_all_err"
// end of [write_all_err]
// 
// HX: all bytes must have been written if this function returns
//
fun write_all_exn
  {fd:int} {sz,n:nat | n <= sz} (
  pf: !fildes_v (fd) | fd: int fd, buf: &bytes sz, ntotal: size_t n
) : void = "atslib_fildes_write_all_exn"
// end of [write_all_exn]

(* ****** ****** *)
//
// HX: implemented in [libc/CATS/fcntl.cats]
//
fun write_substring_err
  {fd:int} {sz:int} {i,n:nat | i+n <= sz} (
  pf: !fildes_v (fd)
| fd: int fd, str: !READ(string sz), start: size_t i, n: size_t n
) : ssizeBtw(~1, n+1) = "atslib_fildes_write_substring_err"
// end of [write_substring_err]

fun write_substring_exn
  {fd:int} {sz:int} {i,n:nat | i+n <= sz} (
  pf: !fildes_v (fd)
| fd: int fd, str: !READ(string sz), start: size_t i, n: size_t n
) : sizeLte n = "atslib_fildes_write_substring_exn"
// end of [write_substring_exn]

(* ****** ****** *)

fun fcntl_getfl {fd:int}
  (pf: !fildes_v (fd) | fd: int fd): flag_t = "atslib_fcntl_getfl"
// end of [fcntl_getfl]

fun fcntl_setfl {fd:int}
  (pf: !fildes_v (fd) | fd: int fd, flag: flag_t): int = "atslib_fcntl_setfl"
// end of [fcntl_setfl]

(* ****** ****** *)

(* end of [fcntl.sats] *)
