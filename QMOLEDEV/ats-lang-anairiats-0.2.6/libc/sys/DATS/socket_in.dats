(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
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

staload "libc/SATS/stdio.sats" // for [perror]
staload "libc/SATS/stdlib.sats" // for [EXIT_FAILURE]

(* ****** ****** *)

staload "libc/netinet/SATS/in.sats"
staload "libc/sys/SATS/socket.sats"

(* ****** ****** *)

staload "libc/sys/SATS/socket_in.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

implement
bind_in_exn
  (pfsock | sfd, servaddr) = let
  prval () = sockaddr_in_trans (view@ servaddr)
  val (pfopt | err) = bind_err (pfsock | sfd, servaddr, socklen_in)
  prval () = sockaddr_trans_in (view@ servaddr)
in
  if err >= 0 then let
    prval bind_v_succ (pf) = pfopt
    prval () = pfsock := pf
  in
    // nothing
  end else let
    prval bind_v_fail (pf) = pfopt
    prval () = pfsock := pf
    val () = perror ("bind")
    val () = exit (EXIT_FAILURE)
  in
    bind_in_exn (pfsock | sfd, servaddr) // HX: this is deadcode
  end // end of [if]
end // end of [bind_in_exn]

(* ****** ****** *)

implement
connect_in_exn
  (pfsock | sfd, servaddr) = let
  prval () = sockaddr_in_trans (view@ servaddr)
  val (pfopt | err) = connect_err (pfsock | sfd, servaddr, socklen_in)
  prval () = sockaddr_trans_in (view@ servaddr)
in
  if err >= 0 then let
    prval connect_v_succ (pf) = pfopt
    prval () = pfsock := pf
  in
    // nothing
  end else let
    prval connect_v_fail (pf) = pfopt
    prval () = pfsock := pf
    val () = perror ("connect")
    val () = exit (EXIT_FAILURE)
  in
    connect_in_exn (pfsock | sfd, servaddr) // HX: this is deadcode
  end // end of [if]
end // end of [connect_in_exn]

(* ****** ****** *)

(*
fun accept_in_err
  {sfd:int} (
    pfskt: !socket_v (sfd, listen)
  | sfd: int sfd
  , sa: &sockaddr_in_struct? >> opt (sockaddr_in_struct, cfd >= 0)
  , salen: &socklen_t(0)? >> socklen_t(n)
  ) : #[cfd:int;n:nat] (accept_v (sfd, cfd) | int cfd)
// end of [accept_in_err]
*)
implement
accept_in_err
  (pfskt | sfd, sa, salen) = let
//
extern
prfun trans1 {l:addr}
  (pf: !sockaddr_in? @ l >> sockaddr_struct(socklen_in)? @ l): void
extern
prfun trans2 {b:bool} {l:addr}
  (pf: !opt (sockaddr_struct(socklen_in), b) @ l >> opt (sockaddr_in, b) @ l): void
//
  prval () = trans1 (view@(sa))
  val () = salen := socklen_in
  val (pfopt | cfd) = accept_err (pfskt | sfd, sa, salen)
  prval () = trans2 (view@(sa))
//
in
  (pfopt | cfd)
end // end of [accept_in_err]

(* ****** ****** *)

(* end of [socket_in.dats] *)
