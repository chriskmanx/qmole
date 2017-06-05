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
#include "libc/CATS/pwd.cats"
%} // end of [%{#]

(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"
typedef gid_t = $T.gid_t
typedef uid_t = $T.uid_t

(* ****** ****** *)

(*
struct passwd
{
  char *pw_name;		/* Username.  */
  char *pw_passwd;		/* Password.  */
  __uid_t pw_uid;		/* User ID.  */
  __gid_t pw_gid;		/* Group ID.  */
  char *pw_gecos;		/* Real name.  */
  char *pw_dir;			/* Home directory.  */
  char *pw_shell;		/* Shell program.  */
} ;
*)
abst@ype
passwd_rest // unknown quantity
typedef passwd_struct =
$extype_struct "ats_passwd_type" of {
  pw_name= ptr // char*
, pw_passwd= ptr // char*
, pw_uid= uid_t
, pw_gid= gid_t
, pw_gecos= ptr // char*
, pw_dir= ptr // char*
, pw_shell= ptr // char*
, _rest= undefined_t
} // end of [passwd]
typedef passwd = passwd_struct

(* ****** ****** *)

fun passwd_get_pw_name (
  pwd: &READ(passwd)
) : [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "atslib_passwd_get_pw_name" // function!
// end of [passwd_get_pw_name]

fun passwd_get_pw_passwd (
  pwd: &READ(passwd)
) : [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "atslib_passwd_get_pw_passwd" // function!
// end of [passwd_get_pw_passwd]

fun passwd_get_pw_gecos (
  pwd: &READ(passwd)
) : [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "atslib_passwd_get_pw_gecos" // function!
// end of [passwd_get_pw_gecos]

fun passwd_get_pw_dir (
  pwd: &READ(passwd)
) : [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "atslib_passwd_get_pw_dir" // function!
// end of [passwd_get_pw_dir]

fun passwd_get_pw_shell (
  pwd: &READ(passwd)
) : [l:addr] (
  strptr l -<lin,prf> void | strptr l
) = "atslib_passwd_get_pw_shell" // function!
// end of [passwd_get_pw_shell]

(* ****** ****** *)

// HX: non-reentrant
fun getpwnam (
  nam: !READ(string)
) :<!ref> [l:addr] (
  vptroutopt (passwd, l) | ptr l
) = "mac#atslib_getpwnam"
// end of [getpwnam]

// HX: reentrant
fun getpwnam_r {n:nat} (
  nam: !READ(string)
, pwbuf: &passwd? >> opt (passwd, i==0)
, buf: &b0ytes(n) >> bytes(n)
, n: size_t (n)
, ppwbuf: &ptr? >> ptr
) :<> #[i:int | i >= 0] int (i) = "mac#atslib_getpwnam_r"
// end of [getpwnam_r]

(* ****** ****** *)

// HX: non-reentrant
fun getpwuid (
  uid: uid_t
) :<!ref> [l:addr] (
  vptroutopt (passwd, l) | ptr l
) = "mac#atslib_getpwuid"
// end of [getpwuid]

// HX: reentrant
fun getpwuid_r
  {n:nat} (
  uid: uid_t
, pwbuf: &passwd? >> opt (passwd, i==0)
, buf: &b0ytes(n) >> bytes(n)
, n: size_t (n)
, ppwbuf: &ptr? >> ptr
) :<> #[i:int | i >= 0] int (i)
  = "mac#atslib_getpwuid_r"
// end of [getpwuid_r]

(* ****** ****** *)

(* end of [pwd.sats] *)
