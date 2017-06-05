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
#include "libc/CATS/unistd_pathconf.cats"
%} // end of [%{#]

(* ****** ****** *)

abst@ype pathconfname_t = int

(* ****** ****** *)

macdef _PC_LINK_MAX = $extval (pathconfname_t, "_PC_LINK_MAX")
macdef _PC_MAX_CANON = $extval (pathconfname_t, "_PC_MAX_CANON")
macdef _PC_MAX_INPUT = $extval (pathconfname_t, "_PC_MAX_INPUT")
macdef _PC_NAME_MAX = $extval (pathconfname_t, "_PC_NAME_MAX")
macdef _PC_PATH_MAX = $extval (pathconfname_t, "_PC_PATH_MAX")
macdef _PC_PIPE_BUF = $extval (pathconfname_t, "_PC_PIPE_BUF")
macdef _PC_CHOWN_RESTRICTED = $extval (pathconfname_t, "_PC_CHOWN_RESTRICTED")
macdef _PC_NO_TRUNC = $extval (pathconfname_t, "_PC_NO_TRUNC")
macdef _PC_VDISABLE = $extval (pathconfname_t, "_PC_VDISABLE")
macdef _PC_SYNC_IO = $extval (pathconfname_t, "_PC_SYNC_IO")
macdef _PC_ASYNC_IO = $extval (pathconfname_t, "_PC_ASYNC_IO")
macdef _PC_PRIO_IO = $extval (pathconfname_t, "_PC_PRIO_IO")
macdef _PC_SOCK_MAXBUF = $extval (pathconfname_t, "_PC_SOCK_MAXBUF")
macdef _PC_FILESIZEBITS = $extval (pathconfname_t, "_PC_FILESIZEBITS")
macdef _PC_REC_INCR_XFER_SIZE = $extval (pathconfname_t, "_PC_REC_INCR_XFER_SIZE")
macdef _PC_REC_MAX_XFER_SIZE = $extval (pathconfname_t, "_PC_REC_MAX_XFER_SIZE")
macdef _PC_REC_MIN_XFER_SIZE = $extval (pathconfname_t, "_PC_REC_MIN_XFER_SIZE")
macdef _PC_REC_XFER_ALIGN = $extval (pathconfname_t, "_PC_REC_XFER_ALIGN")
macdef _PC_ALLOC_SIZE_MIN = $extval (pathconfname_t, "_PC_ALLOC_SIZE_MIN")
macdef _PC_SYMLINK_MAX = $extval (pathconfname_t, "_PC_SYMLINK_MAX")
macdef _PC_2_SYMLINK = $extval (pathconfname_t, "_PC_2_SYMLINK")

(* ****** ****** *)

fun pathconf (
  path: !READ(string), name: pathconfname_t
) : lint = "mac#atslib_pathconf" // end of [pathconf]

(* ****** ****** *)
//
// HX-2010-09-21: for simplicity, [fd] is assumed to be valid
//
fun fpathconf {fd:nat}
  (fd: int fd, name: pathconfname_t): lint = "mac#atslib_fpathconf"
// end of [fpathconf]

(* ****** ****** *)

(* end of [unistd_pathconf.sats] *)
