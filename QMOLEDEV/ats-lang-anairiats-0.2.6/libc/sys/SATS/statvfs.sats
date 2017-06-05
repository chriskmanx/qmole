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
#include "libc/sys/CATS/statvfs.cats"
%} // end of [%{#]

(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"
typedef fsblkcnt_t = $T.fsblkcnt_t
typedef fsfilcnt_t = $T.fsfilcnt_t

(* ****** ****** *)

typedef
statvfs_struct =
$extype_struct "ats_statvfs_type" of {
  f_bsize= ulint
, f_frsize= ulint 
, f_blocks= fsblkcnt_t
, f_bfree= fsblkcnt_t
, f_bavail= fsblkcnt_t
, f_files= fsfilcnt_t
, f_ffree= fsfilcnt_t
, f_favail= fsfilcnt_t
, f_fsid= ulint
, f_flag= ulint
, f_namemax= ulint
, _rest = undefined_t // unknown quantity
} // end of [statvfs]
typedef statvfs = statvfs_struct

(* ****** ****** *)

fun statvfs ( // -1 on error // errno set
  path: !READ(string), buf: &statvfs? >> opt (statvfs, i==0)
) : #[i:int | i <= 0] int i = "mac#atslib_statvfs"
// end of [statvfs]

fun fstatvfs {fd:nat} ( // -1 on error // errno set
  fd: int fd, buf: &statvfs? >> opt (statvfs, i==0)
) : #[i:int | i <= 0] int i = "mac#atslib_fstatvfs"
// end of [fstatvfs]

(* ****** ****** *)

(* end of [statvfs.sats] *)
