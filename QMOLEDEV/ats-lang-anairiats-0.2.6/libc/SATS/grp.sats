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
#include "libc/CATS/grp.cats"
%} // end of [%{#]

(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"
typedef gid_t = $T.gid_t

(* ****** ****** *)

(*
struct group {
  char *gr_name;		/* Group name.	*/
  char *gr_passwd;		/* Password.	*/
  __gid_t gr_gid;		/* Group ID.	*/
  char **gr_mem;		/* Member list.	*/ // null-terminated
} ;
*)
abst@ype
group_rest // unknown quantity
typedef group_struct =
$extype_struct "ats_group_type" of {
  gr_name= ptr // char*
, gr_passwd= ptr // char*
, gr_gid= gid_t
, gr_mem= ptr // char** // member list // null-terminated
, _rest= undefined_t
} // end of [group]
typedef group = group_struct

(* ****** ****** *)

fun group_get_gr_name (
  grp: &READ(group)
) : [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "atslib_group_get_gr_name" // function!
// end of [group_get_gr_name]

fun group_get_gr_passwd (
  grp: &READ(group)
) : [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "atslib_group_get_gr_passwd" // function!
// end of [group_get_gr_passwd]

(* ****** ****** *)
//
// HX: ptrarr: null-terminated array of pointers
//
fun group_get_gr_mem (
  grp: &READ(group)
) : [n:nat;l:addr] (
  ptrarr(n) @ l, ptrarr(n) @ l -<lin,prf> void | ptr l
) = "atslib_group_get_gr_mem"
// end of [group_get_gr_mem]

(* ****** ****** *)
//
// HX: non-reentrant
//
fun getgrnam (
  nam: !READ(string)
) :<!ref> [l:addr] (
  vptroutopt (group, l) | ptr l
) = "mac#atslib_getgrnam"
// end of [getgrnam]

(* ****** ****** *)
//
// HX: non-reentrant
//
fun getgrgid (
  gid: gid_t
) :<!ref> [l:addr] (
  vptroutopt (group, l) | ptr l
) = "mac#atslib_getgrgid"
// end of [getgrgid]

(* ****** ****** *)

(* end of [grp.sats] *)
