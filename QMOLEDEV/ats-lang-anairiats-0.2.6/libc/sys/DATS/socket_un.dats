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

staload "libc/sys/SATS/un.sats"
staload "libc/sys/SATS/socket.sats"

(* ****** ****** *)

staload "libc/sys/SATS/socket_un.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

implement
connect_un_exn
  (pfsock | sfd, servaddr) = let
  prval () = sockaddr_un_trans (view@ servaddr)
  val (pfopt | err) = connect_err (pfsock | sfd, servaddr, socklen_un)
  prval () = sockaddr_trans_un (view@ servaddr)
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
    connect_un_exn (pfsock | sfd, servaddr) // HX: this is deadcode
  end // end of [if]
end // end of [connect_un_exn]

(* ****** ****** *)

implement
bind_un_exn
  (pfsock | sfd, servaddr) = let
  prval () = sockaddr_un_trans (view@ servaddr)
  val (pfopt | err) = bind_err (pfsock | sfd, servaddr, socklen_un)
  prval () = sockaddr_trans_un (view@ servaddr)
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
    bind_un_exn (pfsock | sfd, servaddr) // HX: this is deadcode
  end // end of [if]
end // end of [bind_un_exn]

(* ****** ****** *)

(* end of [socket_un.dats] *)
