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

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010

(* ****** ****** *)

%{#
#include "libc/CATS/dlfcn.cats"
%}

(* ****** ****** *)

absview dlopen_v (addr) // abstract view for [dlopen]

(* ****** ****** *)

abst@ype dlopen_flag_t = int
macdef RTLD_LAZY = $extval (dlopen_flag_t, "RTLD_LAZY")
macdef RTLD_NOW = $extval (dlopen_flag_t, "RTLD_NOW")

abst@ype dlopen_flagext_t = int
macdef RTLD_GLOBAL = $extval (dlopen_flagext_t, "RTLD_GLOBAL")
macdef RTLD_LOCAL = $extval (dlopen_flagext_t, "RTLD_LOCAL")

// since glibc-2.2
macdef RTLD_NODELETE = $extval (dlopen_flagext_t, "RTLD_NODELETE")

// since glibc-2.2
macdef RTLD_NOLOAD = $extval (dlopen_flagext_t, "RTLD_NOLOAD")

// since glibc-2.3.4
macdef RTLD_DEEPBIND = $extval (dlopen_flagext_t, "RTLD_DEEPBIND")

(* ****** ****** *)

fun lor_dlopen_flag_dlopen_flagext
  (flag: dlopen_flag_t, ext: dlopen_flagext_t):<> dlopen_flag_t
  = "atslib_lor_dlopen_flag_dlopen_flagext"
overload lor with lor_dlopen_flag_dlopen_flagext

(* ****** ****** *)

(*
void *dlopen(const char *filename, int flag);
*)
fun dlopen
  (filename: string, flag: dlopen_flag_t)
  :<> [l:addr] (option_v (dlopen_v l, l > null) | ptr l)
  = "mac#atslib_dlopen"
// end of [dlopen]

fun dlopen_exn
  (filename: string, flag: dlopen_flag_t)
  :<!exn> [l:agz] (dlopen_v l | ptr l)
  = "atslib_dlopen_exn" // this is a function
// end of [dlopen_exn]

(* ****** ****** *)

// success/failure: err = 0/1
fun dlclose {l:agz} (
    pf: !dlopen_v l >> option_v (dlopen_v l, i > 0) | handle: ptr l
  ) :<> #[i:int | i >= 0] int(i) (*err*)
  = "mac#atslib_dlclose"
// end of [dlclose]

fun dlclose_exn {l:agz}
  (pf: dlopen_v l | handle: ptr l):<!exn> void
  = "atslib_dlclose_exn" // this is a function
// end of [dlclose_exn]

(* ****** ****** *)
//
// HX:
// for reporting error;
// there is no error if the return is null
//
fun dlerror ()
  : [l:addr] (strptr (l) -<lin,prf> void | strptr (l))
  = "mac#atslib_dlerror"
// end of [dlerror]

fun dlerror_clr (): void // end of [dlerror_clr]

(* ****** ****** *)

fun dlsym {l:addr}
  (pf: !dlopen_v l | handle: ptr l, sym: string): ptr
  = "mac#atslib_dlsym"
// end of [dlsym]

(* ****** ****** *)

(* end of [dlfcn.sats] *)
