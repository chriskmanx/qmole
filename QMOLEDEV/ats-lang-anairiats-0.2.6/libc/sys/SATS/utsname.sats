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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)  *)

(* ****** ****** *)

%{#
#include "libc/sys/CATS/utsname.cats"
%} // end of [%{#]

(* ****** ****** *)

abst@ype utsname_rest
typedef utsname_struct =
$extype_struct "ats_utsname_type" of {
  _rest= utsname_rest // unknown quantity
} // end of [utsname_struct]
typedef utsname = utsname_struct

(* ****** ****** *)

fun utsname_get_sysname
  (x: &utsname):<> [l:agz] (strptr l -<lin,prf> void | strptr l)
  = "mac#atslib_utsname_get_sysname"
// end of [utsname_get_sysname]

fun utsname_get_nodename
  (x: &utsname):<> [l:agz] (strptr l -<lin,prf> void | strptr l)
  = "mac#atslib_utsname_get_nodename"
// end of [utsname_get_nodename]

fun utsname_get_release
  (x: &utsname):<> [l:agz] (strptr l -<lin,prf> void | strptr l)
  = "mac#atslib_utsname_get_release"
// end of [utsname_get_release]

fun utsname_get_version
  (x: &utsname):<> [l:agz] (strptr l -<lin,prf> void | strptr l)
  = "mac#atslib_utsname_get_version"
// end of [utsname_get_version]

fun utsname_get_machine
  (x: &utsname):<> [l:agz] (strptr l -<lin,prf> void | strptr l)
  = "mac#atslib_utsname_get_machine"
// end of [utsname_get_machine]

fun utsname_get_domainname
  (x: &utsname):<> [l:agz] (strptr l -<lin,prf> void | strptr l)
  = "mac#atslib_utsname_get_domainname"
// end of [utsname_get_domainname]

(* ****** ****** *)
//
// HX: 0/-1 : succ/fail // errno set
//
fun uname (
  x: &utsname? >> opt (utsname, i==0)
) : #[i:int | i <= 0] int i = "mac#atslib_uname"
// end of [uname]

(* ****** ****** *)

(* end of [utsname.sats] *)
