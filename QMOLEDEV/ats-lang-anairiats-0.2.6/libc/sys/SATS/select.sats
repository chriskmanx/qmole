(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)  *)

(* ****** ****** *)

%{#
#include "libc/sys/CATS/select.cats"
%} // end of [%{#]

(* ****** ****** *)

abst@ype fd_set_t0ype = $extype"ats_fd_set_type"
typedef fd_set = fd_set_t0ype

(* ****** ****** *)

fun FD_ZERO
  (fdset: &fd_set? >> fd_set):<> void = "atslib_FD_ZERO"

fun FD_SET {fd:nat}
  (fd: int fd, fdset: &fd_set):<> void = "atslib_FD_SET"

fun FD_CLR {fd:nat}
  (fd: int fd, fdset: &fd_set):<> void = "atslib_FD_CLR"

fun FD_ISSET {fd:nat}
  (fd: int fd, fdset: &fd_set):<> bool = "atslib_FD_ISSET"
// end of [FD_ISSET]

(* ****** ****** *)

(* end of [select.sats] *)
