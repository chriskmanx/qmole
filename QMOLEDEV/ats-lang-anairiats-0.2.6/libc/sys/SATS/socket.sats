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
#include "libc/sys/CATS/socket.cats"
%} // end of [%{#]

(* ****** ****** *)

staload SA = "libc/sys/SATS/sockaddr.sats"
typedef sa_family_t = $SA.sa_family_t
stadef socklen_t = $SA.socklen_t
stadef sockaddr_struct = $SA.sockaddr_struct

(* ****** ****** *)

abst@ype socktype_t = int
(*
// HX: using $UNSAFE.cast2int instead
castfn int_of_socktype (x: socktype_t):<> int
overload int_of with int_of_socktype
*)
macdef SOCK_STREAM = $extval (socktype_t, "SOCK_STREAM")
macdef SOCK_DGRAM = $extval (socktype_t, "SOCK_DGRAM")
macdef SOCK_RAW = $extval (socktype_t, "SOCK_RAW")
macdef SOCK_RDM = $extval (socktype_t, "SOCK_RDM")
macdef SOCK_PACKET = $extval (socktype_t, "SOCK_PACKET")
macdef SOCK_SEQPACKET = $extval (socktype_t, "SOCK_SEQPACKET")
macdef SOCK_DCCP = $extval (socktype_t, "SOCK_DCCP")

abst@ype socktypeflag_t = int
macdef SOCK_CLOEXEC = $extval (socktypeflag_t, "SOCK_CLOEXEC")
macdef SOCK_NONBLOCK = $extval (socktypeflag_t, "SOCK_NONBLOCK")
fun lor_socktype_socktypeflag (
  t: socktype_t, f: socktypeflag_t
) :<> socktype_t
  = "atspre_lor_uint_uint"
overload lor with lor_socktype_socktypeflag

(* ****** ****** *)

abst@ype sockprot_t = int
(*
// HX: using $UNSAFE.cast2int instead
castfn int_of_sockprot (x: sockprot_t):<> int
overload int_of with int_of_sockprot
*)

(* ****** ****** *)
//
// HX:
// client: init -> connect
// server: init -> bind -> listen -> accept
//
datasort status = init | bind | listen | conn

absview socket_v (int, status)

fun socket_family_type_err (
  af: sa_family_t, t: socktype_t
) : [fd:int] (
  option_v (socket_v (fd, init), fd >= 0) | int fd
) = "atslib_socket_family_type_err"
// end of [socket_family_type_err]

fun socket_family_type_exn
  (af: sa_family_t, t: socktype_t): [fd:nat] (socket_v (fd, init) | int fd)
// end of [socket_family_type_exn]

(* ****** ****** *)

dataview connect_v (fd: int, int) =
  | connect_v_succ (fd,  0) of socket_v (fd, conn)
  | connect_v_fail (fd, ~1) of socket_v (fd, init)
// end of [connect_v]

fun connect_err
  {fd:int} {n:int} (
  pfskt: socket_v (fd, init)
| fd: int fd
, servaddr: &sockaddr_struct(n), salen: socklen_t(n)
) : [i:int | i <= 0] (
  connect_v (fd, i) | int i
) = "mac#atslib_connect_err"
// end of [connect_err]

(* ****** ****** *)

dataview bind_v (fd:int, int) = 
  | bind_v_fail (fd, ~1) of socket_v (fd, init)
  | bind_v_succ (fd,  0) of socket_v (fd, bind)
// end of [bind_v]

fun bind_err
  {fd:int} {n:int} (
  pfskt: socket_v (fd, init)
| fd: int fd
, servaddr: &sockaddr_struct(n), salen: socklen_t(n)
) : [i:int] (
  bind_v (fd, i) | int i
) = "mac#atslib_bind_err"
// end of [bind_err]

(* ****** ****** *)

macdef SOMAXCONN = $extval (Pos, "SOMAXCONN")

dataview listen_v (fd:int, int) = 
  | listen_v_fail (fd, ~1) of socket_v (fd, bind) 
  | listen_v_succ (fd,  0) of socket_v (fd, listen)
// end of [listen_v]

fun listen_err {fd:int}
  (pfskt: socket_v (fd, bind) | fd: int fd, backlog: Pos)
  : [i:int] (listen_v (fd, i) | int i)
  = "atslib_listen_err"
// end of [listen_err]

fun listen_exn {fd:int} (
  pfskt: !socket_v (fd, bind) >> socket_v (fd, listen)
| fd: int fd, backlog: Pos // [backlog = 0] is not supported on all systems
) : void // end of [listen_exn]

(* ****** ****** *)

dataview
accept_v (fd:int, int(*err*)) =
  | accept_v_fail (fd, ~1) of ()
  | {cfd:nat} accept_v_succ (fd, cfd) of socket_v (cfd, conn)
// end of [accept_v]

fun accept_err
  {sfd:int} {n:int} (
  pfskt: !socket_v (sfd, listen)
| sfd: int sfd
, sa: &sockaddr_struct(n)? >> opt (sockaddr_struct(n), cfd >= 0)
, salen: &socklen_t(n) >> socklen_t(n1)
) : #[cfd:int;n1:nat] (accept_v (sfd, cfd) | int cfd)
  = "mac#atslib_accept_err"
// end of [accept_err]

(* ****** ****** *)

fun accept_null_err {sfd:int}
  (pfskt: !socket_v (sfd, listen) | sfd: int sfd)
  : [cfd:int] (option_v (socket_v (cfd, conn), cfd >= 0) | int cfd)
  = "atslib_accept_null_err"
// end of [accept_null_err]

fun accept_null_exn {sfd:int} (
  pfskt: !socket_v (sfd, listen) | sfd: int sfd
) : [cfd:nat] (socket_v (cfd, conn) | int cfd) // for client side
// end of [accept_null_exn]

(* ****** ****** *)

fun socket_close_err
  {fd:int} {s:status} (
  pfskt: socket_v (fd, s) | fd: int fd
) : [i:int | i <= 0] (option_v (socket_v (fd, s), i < 0) | int i)
  = "mac#atslib_socket_close_err" // = atslib_close_err
// end of [socket_close_err]

//
// HX: this one is like [fildes_close_loop_exn]
//
fun socket_close_exn {fd:int} {s:status}
  (pfskt: socket_v (fd, s) | fd: int fd): void
// end of [socket_close_exn]

(* ****** ****** *)

abst@ype shutkind_t = int
macdef SHUT_RD = $extval (shutkind_t, "SHUT_RD")
macdef SHUT_WR = $extval (shutkind_t, "SHUT_WR")
macdef SHUT_RDWR = $extval (shutkind_t, "SHUT_RDWR")

//
// HX: what error can occur?
//
fun shutdown_err
  {fd:int} ( // 0/-1 : succ/fail // errno set
  pfskt: socket_v (fd, conn)
| fd: int fd, how: shutkind_t
) : [i:int | i <= 0] (
  option_v (socket_v (fd, conn), i < 0) | int i
) = "mac#atslib_shutdown_err"
// end of [shutdown_err]

fun shutdown_exn // HX: this one is preferred!
  {fd:int} // 0/-1 : succ/fail // errno set
  (pfskt: socket_v (fd, conn) | fd: int fd, how: shutkind_t): void
// end of [shutdown_exn]

(* ****** ****** *)
//
// HX: actually implemented in [libc/CATS/fcntl.cats]
//
fun socket_read_err
  {fd:int}
  {n,sz:nat | n <= sz} (
  pfskt: !socket_v (fd, conn)
| fd: int fd, buf: &bytes sz, ntotal: size_t n
) : ssizeBtw(~1, n+1)
  = "mac#atslib_socket_read_err" // = atslib_fildes_read_err
// end of [socket_read_err]
//
// HX: implemented in [libc/sys/DATSsocket.dats]
//
fun socket_read_exn
  {fd:int}
  {n,sz:nat | n <= sz} (
  pfskt: !socket_v (fd, conn)
| fd: int fd, buf: &bytes sz, ntotal: size_t n
) : sizeLte n
// end of [socket_read_exn]

(* ****** ****** *)
//
// HX: actually implemented in [libc/CATS/fcntl.cats]
//
fun socket_write_err
  {fd:int}
  {n,sz:nat | n <= sz} (
  pfskt: !socket_v (fd, conn)
| fd: int fd, buf: &bytes sz, ntotal: size_t n
) : ssizeBtw(~1, n+1) = "mac#atslib_socket_write_err" // = atslib_fildes_write_err
// end of [socket_write_err]
//
// HX: [socket_write_exn]: plesae use [socket_write_all_exn] instead
//
(* ****** ****** *)
//
// HX:
// this one is actually implemented in [libc/DATS/fcntl.dats]
// note that it is used only when it is known ahead how many bytes are expected;
// otherwise, there is the risk of forever blocking!!!
//
fun socket_read_all_err
  {fd:int}
  {n,sz:nat | n <= sz} (
  pfskt: !socket_v (fd, conn)
| fd: int fd, buf: &bytes sz, ntotal: size_t n
) : ssizeBtw (~1, n+1)
  = "mac#atslib_socket_read_all_err" // = atslib_fildes_read_all_err
// end of [socket_read_all_err]

(* ****** ****** *)
//
// HX: this one is actually implemented in [libc/DATS/fcntl.dats]
//
fun socket_write_all_err
  {fd:int}
  {n,sz:nat | n <= sz} (
  pfskt: !socket_v (fd, conn)
| fd: int fd, buf: &bytes sz, ntotal: size_t n
) : ssizeBtw(~1, n+1)
  = "mac#atslib_socket_write_all_err" // = atslib_fildes_write_all_err
// end of [socket_write_all_err]
//
// HX: this one is implemented in [libc/sys/DATS/socket.dats]
//
fun socket_write_all_exn
  {fd:int}
  {n,sz:nat | n <= sz} (
  pfskt: !socket_v (fd, conn)
| fd: int fd, buf: &bytes sz, ntotal: size_t n
) : void // all bytes must be written if this function returns
  = "atslib_socket_write_all_exn"
// end of [socket_write_all_exn]

(* ****** ****** *)

fun socket_write_substring
  {fd:int} {n:int}
  {st,ln:nat | st+ln <= n} (
  pf_sock: !socket_v (fd, conn)
| fd: int fd, str: !READ(string n), st: size_t st, ln: size_t ln
) : void // all bytes must be written if this function returns
// end of [socket_write_substring]

(* ****** ****** *)

dataview socket_fdopen_v
  (fd: int, m: file_mode, addr) =
  | socket_fdopen_v_fail (fd, m, null) of socket_v (fd, conn)
  | {l:agz} socket_fdopen_v_succ (fd, m, l) of FILE m @ l
fun socket_fdopen_err
  {fd:int} {m:file_mode} (
  pf: socket_v (fd, conn) | fd: int fd, m: file_mode m
) : [l:addr] (
  socket_fdopen_v (fd, m, l) | ptr l
) = "mac#atslib_socket_fdopen_err"
// end of [socket_fdopen_err]

(* ****** ****** *)

fun setsockopt
  {a:t@ype} {fd:nat} (
  fd: int fd
, level: int
, option: int
, value: &a
, valen: sizeof_t a
) : int
  = "atslib_setsockopt" // function!
// end of [setsockopt]

fun getsockopt_err {a:t@ype} {fd:nat} (
  fd: int fd
, level: int, option: int
, value: &a? >> opt (a, i==0), valen: sizeof_t a
) : #[i:int | i <= 0] int i = "atslib_getsockopt_err" // function!
// end of [getsockopt_err]

(* ****** ****** *)

fun sockatmark {fd:nat} (fd: int fd): int = "mac#atslib_sockatmark"

(* ****** ****** *)

(* end of [socket.sats] *)
