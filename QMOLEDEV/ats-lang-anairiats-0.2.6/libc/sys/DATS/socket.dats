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

staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libc/SATS/errno.sats" // for [errno_get]
staload "libc/SATS/stdio.sats" // for [perror]
staload "libc/SATS/stdlib.sats" // for [EXIT_FAILURE]

(* ****** ****** *)

staload "libc/sys/SATS/socket.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

implement
socket_family_type_exn (af, socktp) = let
  val (pfopt | fd) = socket_family_type_err (af, socktp)
in
  if fd >= 0 then let
    prval Some_v (pf) = pfopt in (pf | fd)
  end else let
    prval None_v () = pfopt
    val () = perror "socket"
    val () = exit_errmsg (EXIT_FAILURE, "exit(ATS): [socket] failed\n")
  in
    socket_family_type_exn (af, socktp)
  end // end of [if]
end // end of [socket_family_type_exn]

(* ****** ****** *)

implement
socket_close_exn (pfskt | fd) = let
  val (pfopt | i) = socket_close_err (pfskt | fd)
in
  if i >= 0 then let
    prval None_v () = pfopt in (*nothing*)
  end else let
    prval Some_v (pfskt) = pfopt
  in
    if (errno_get () = EINTR) then
      socket_close_exn (pfskt | fd)
    else let
      val () = perror "close"
      val () = prerrf ("exit(ATS): [socket_close] failed.\n", @())
      val () = exit_main {void} {..} {unit_v} (pfskt | EXIT_FAILURE)
      prval unit_v () = pfskt
    in
      // nothing
    end // end of [if]
  end // end of [if]
end // end of [socket_close_exn]

(* ****** ****** *)

implement
listen_exn (pfskt | fd, nbacklog) = let
  val (pfopt | err) = listen_err (pfskt | fd, nbacklog)
in
  if (err >= 0) then let
    prval listen_v_succ (pf) = pfopt
    prval () = pfskt := pf
  in
    // nothing
  end else let
    prval listen_v_fail (pf) = pfopt
    prval () = pfskt := pf    
    val () = perror "listen"
    val () = exit_errmsg (EXIT_FAILURE, "exit(ATS): [listen] failed\n")
  in
    listen_exn (pfskt | fd, nbacklog) // HX: this is deadcode
  end // end of [if]
end // end of [listen_exn]

(* ****** ****** *)

implement
accept_null_exn (pfskt | sfd) = let
  val (pfopt | cfd) = accept_null_err (pfskt | sfd)
in
  if (cfd >= 0) then let
    prval Some_v (pfconn) = pfopt in (pfconn | cfd)
  end else let
    prval None_v () = pfopt
    val () = perror "accept"
    val () = exit_errmsg (EXIT_FAILURE, "exit(ATS): [accept] failed\n")
  in
    accept_null_exn (pfskt | sfd) // HX: this is deadcode
  end // end of [if]
end // end of [accept_null_exn]

(* ****** ****** *)

implement
shutdown_exn (pfskt | fd, how) = let
  val (pfopt | err)  = shutdown_err (pfskt | fd, how)
in
  if err >= 0 then let
    prval None_v () = pfopt in (*nothing*)
  end else let
    prval Some_v (pf) = pfopt
    prval () = pfskt := pf
    val () = perror "shutdown"
    val () = exit_errmsg (EXIT_FAILURE, "exit(ATS): [shutdown] failed\n")
  in
    shutdown_exn (pfskt | fd, how) // HX: this is deadcode
  end (* end of [if] *)
end // end of [shutdown_exn]

(* ****** ****** *)

implement
socket_read_exn
  (pfskt | fd, buf, ntot) = let
  val nread = socket_read_err (pfskt | fd, buf, ntot)
in
  if nread >= 0 then
    size1_of_ssize1 (nread)
  else let
    val () = perror "socket_read"
  in
    exit_errmsg (EXIT_FAILURE, "[socket_read] failed.\n")
  end // end of [if]
end // end of [socket_read_exn]

(* ****** ****** *)

implement
socket_write_all_exn
  (pfskt | fd, buf, ntot) = let
  var err: int = 1
  val nwrit = socket_write_all_err (pfskt | fd, buf, ntot)
  val () = if nwrit >= 0 then let
    val nwrit = size1_of_ssize1 (nwrit)
  in
    if (nwrit = ntot) then (err := 0)
  end // end of [if]
in
  if err > 0 then let
    val () = perror "socket_write"
  in
    exit_errmsg (EXIT_FAILURE, "[socket_write_all] failed.\n")
  end (* end of [if] *)
end // end of [socket_write_all_exn]

(* ****** ****** *)

implement
socket_write_substring
  {fd} {n} {st,ln}
  (pfsock | fd, str, st, ln) = let
//
  val str = $UN.castvwtp1 {string(n)} (str)
//
  val (pf, fpf | p) =
    string_takeout_bufptr {n} {st} {ln} (str, st)
  val () = socket_write_all_exn (pfsock | fd, !p, ln)
  prval () = fpf (pf)
in
  // nothing
end // end of [socket_write_substring]

(* ****** ****** *)

%{$
ats_int_type
atslib_getsockopt_err (
  ats_int_type fd
, ats_int_type level
, ats_int_type option
, ats_ref_type value
, ats_size_type valen
) {
  socklen_t valen_int = (socklen_t)valen ;
  int err = getsockopt
    (fd, level, option, (void*)value, &valen_int) ;
/*
  if (err == 0) {
    if (valen_int != (socklen_t)valen) ats_crash () ;
  } // end of [if]
*/
  return err ;
} // end of [atslib_getsockopt_err]
%} // end of [%{$]

(* ****** ****** *)

(* end of [socket.dats] *)
