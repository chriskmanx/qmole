(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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
#include "libc/CATS/errno.cats"
%} // end of [%{#]

(* ****** ****** *)

abst@ype errno_t = int
castfn int_of_errno (e: errno_t):<> [i:nat] int i
overload int_of with int_of_errno
castfn errno_of_int {i:nat} (i: int i):<> errno_t

(* ****** ****** *)
//
macdef ENONE = $extval (errno_t, "ENONE") // = 0
//
macdef E2BIG = $extval (errno_t, "E2BIG")
macdef EACCES = $extval (errno_t, "EACCES")
macdef EADDRINUSE = $extval (errno_t, "EADDRINUSE")
macdef EADDRNOTAVAIL = $extval (errno_t, "EADDRNOTAVAIL")
macdef EAGAIN = $extval (errno_t, "EAGAIN")
macdef EALREADY = $extval (errno_t, "EALREADY")
macdef EBADF = $extval (errno_t, "EBADF")
macdef EBADMSG = $extval (errno_t, "EBADMSG")
macdef ECANCELED = $extval (errno_t, "ECANCELED")
macdef ECHILD = $extval (errno_t, "ECHILD")
macdef ECONNABORTED = $extval (errno_t, "ECONNABORTED")
macdef ECONNREFUSED = $extval (errno_t, "ECONNREFUSED")
macdef ECONNRESET = $extval (errno_t, "ECONNRESET")
macdef EDEADLK = $extval (errno_t, "EDEADLK")
macdef EDESTADDRREQ = $extval (errno_t, "EDESTADDRREQ")
macdef EDOM = $extval (errno_t, "EDOM")
macdef EEXIST = $extval (errno_t, "EEXIST")
macdef EFAULT = $extval (errno_t, "EFAULT")
macdef EFBIG = $extval (errno_t, "EFBIG")
macdef EHOSTUNREACH = $extval (errno_t, "EHOSTUNREACH")
macdef EIDRM = $extval (errno_t, "EIDRM")
macdef EILSEQ = $extval (errno_t, "EILSEQ")
macdef EINPROGRESS = $extval (errno_t, "EINPROGRESS")
macdef EINTR = $extval (errno_t, "EINTR")
macdef EINVAL = $extval (errno_t, "EINVAL")
macdef EIO = $extval (errno_t, "EIO")
macdef EISCONN = $extval (errno_t, "EISCONN")
macdef EISDIR = $extval (errno_t, "EISDIR")
macdef ELOOP = $extval (errno_t, "ELOOP")
macdef EMFILE = $extval (errno_t, "EMFILE")
macdef EMLINK = $extval (errno_t, "EMLINK")
macdef EMSGSIZE = $extval (errno_t, "EMSGSIZE")
macdef ENAMETOOLONG = $extval (errno_t, "ENAMETOOLONG")
macdef ENETDOWN = $extval (errno_t, "ENETDOWN")
macdef ENETRESET = $extval (errno_t, "ENETRESET")
macdef ENETUNREACH = $extval (errno_t, "ENETUNREACH")
macdef ENFILE = $extval (errno_t, "ENFILE")
macdef ENOBUFS = $extval (errno_t, "ENOBUFS")
macdef ENODATA = $extval (errno_t, "ENODATA")
macdef ENODEV = $extval (errno_t, "ENODEV")
macdef ENOENT = $extval (errno_t, "ENOENT")
macdef ENOEXEC = $extval (errno_t, "ENOEXEC")
macdef ENOLCK = $extval (errno_t, "ENOLCK")
macdef ENOLINK = $extval (errno_t, "ENOLINK")
macdef ENOMEM = $extval (errno_t, "ENOMEM")
macdef ENOMSG = $extval (errno_t, "ENOMSG")
macdef ENOPROTOOPT = $extval (errno_t, "ENOPROTOOPT")
macdef ENOSPC = $extval (errno_t, "ENOSPC")
macdef ENOSR = $extval (errno_t, "ENOSR")
macdef ENOSTR = $extval (errno_t, "ENOSTR")
macdef ENOSYS = $extval (errno_t, "ENOSYS")
macdef ENOTCONN = $extval (errno_t, "ENOTCONN")
macdef ENOTDIR = $extval (errno_t, "ENOTDIR")
macdef ENOTEMPTY = $extval (errno_t, "ENOTEMPTY")
macdef ENOTSOCK = $extval (errno_t, "ENOTSOCK")
macdef ENOTSUP = $extval (errno_t, "ENOTSUP")
macdef ENOTTY = $extval (errno_t, "ENOTTY")
macdef ENXIO = $extval (errno_t, "ENXIO")
macdef EOPNOTSUPP = $extval (errno_t, "EOPNOTSUPP")
macdef EOVERFLOW = $extval (errno_t, "EOVERFLOW")
macdef EPERM = $extval (errno_t, "EPERM")
macdef EPIPE = $extval (errno_t, "EPIPE")
macdef EPROTO = $extval (errno_t, "EPROTON")
macdef EPROTONOSUPPORT = $extval (errno_t, "EPROTONOSUPPORT")
macdef EPROTOTYPE = $extval (errno_t, "EPROTOTYPE")
macdef ERANGE = $extval (errno_t, "ERANGE")
macdef EROFS = $extval (errno_t, "EROFS")
macdef ESPIPE = $extval (errno_t, "ESPIPE")
macdef ESRCH = $extval (errno_t, "ESRCH")
macdef ETIME = $extval (errno_t, "ETIME")
macdef ETIMEDOUT = $extval (errno_t, "ETIMEDOUT")
macdef ETXTBUSY = $extval (errno_t, "ETXTBUSY")
macdef EWOULDBLOCK = $extval (errno_t, "EWOULDBLOCK")
macdef EXDEV = $extval (errno_t, "EXDEV")

(* ****** ****** *)
//
// HX: due to some special treatment for [errno]
//
fun errno_get ():<> errno_t = "atslib_errno_get"
fun errno_set (n: errno_t):<> void = "atslib_errno_set"
fun errno_reset ():<> void = "atslib_errno_reset"

(* ****** ****** *)

fun eq_errno_errno
  (n1: errno_t, n2: errno_t):<> bool = "atslib_eq_errno_errno"
overload = with eq_errno_errno

fun neq_errno_errno
  (n1: errno_t, n2: errno_t):<> bool = "atslib_neq_errno_errno"
overload <> with neq_errno_errno

(* ****** ****** *)

(* end of [errno.sats] *)
