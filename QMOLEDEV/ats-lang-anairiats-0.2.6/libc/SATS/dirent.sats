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

staload TYPES = "libc/sys/SATS/types.sats"
typedef ino_t = $TYPES.ino_t
typedef off_t = $TYPES.off_t

(* ****** ****** *)

%{#
#include "libc/CATS/dirent.cats" // included after types.cats
%} // end of [%{#]

(* ****** ****** *)

// defined in dirent.cats
abst@ype DIR_t0ype = $extype"ats_DIR_type" // = DIR
typedef DIR = DIR_t0ype
abst@ype dirent_t0ype = $extype"ats_dirent_type" // = struct dirent
typedef dirent = dirent_t0ype

(* ****** ****** *)

fun dirent_get_d_ino
  (ent: &dirent):<> ino_t = "atslib_direct_get_d_ino"
// end of [dirent_get_d_ino]

fun dirent_get_d_name
  (ent: &dirent):<> [l:agz] (strptr l -<lin,prf> void | strptr l)
  = "atslib_dirent_get_d_name"
// end of [dirent_get_d_name]

(* ****** ****** *)

fun opendir_err (s: string)
  : [l_dir:addr] (option_v (DIR @ l_dir, l_dir > null) | ptr l_dir)
  = "mac#atslib_opendir_err" // macro!

fun opendir_exn (s: string)
  : [l_dir:addr] (DIR @ l_dir | ptr l_dir) = "atslib_opendir_exn"
// end of [opendir_exn] // function!

(* ****** ****** *)

fun closedir_err {l_dir:addr} (
  pf: DIR @ l_dir | p: ptr l_dir
) :<> int = "mac#atslib_closedir_err" // macro!

fun closedir_exn {l_dir:addr} (
  pf: DIR @ l_dir | p: ptr l_dir
) :<!exn> void = "atslib_closedir_exn" // function!

(* ****** ****** *)

fun readdir (
  dir: &DIR
) :<> [l_ent:addr] (
  vptroutopt (dirent, l_ent)
| ptr l_ent
) = "mac#atslib_readdir"
// end of [readdir]

(* ****** ****** *)

fun rewinddir (dir: &DIR): void = "mac#atslib_rewinddir"
fun seekdir (dir: &DIR, off: off_t): void = "mac#atslib_seekdir"
fun telldir (dir: &DIR): off_t = "mac#atslib_telldir"

(* ****** ****** *)

viewtypedef direntptr_gc =
  [l:addr] (free_gc_v (dirent?, l), dirent @ l | ptr l)
// end of [direntptr_gc]

//
// HX: [dirent_stream_vt_make_DIR] is reentrant
//
fun dirent_stream_vt_make_DIR {l_dir:addr}
  (pf: DIR @ l_dir | p: ptr l_dir):<!laz> stream_vt (dirent)
// end of [dirent_strean_vt_make_DIR]

//
// HX: [direntptr_stream_vt_make_DIR] is reentrant
//
fun direntptr_stream_vt_make_DIR {l_dir:addr}
  (pf: DIR @ l_dir | p: ptr l_dir):<!laz> stream_vt (direntptr_gc)
// end of [direntptr_stream_vt_make_DIR]

(* ****** ****** *)

(* end of [dirent.sats] *)
