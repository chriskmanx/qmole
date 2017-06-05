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
#include "libc/sys/CATS/poll.cats"
%} // end of [%{#]

(* ****** ****** *)

macdef POLLIN = $extval (usint, "POLLIN") // there is data to read
macdef POLLPRI = $extval (usint, "POLLPRI") // there is urgent data to read
macdef POLLOUT = $extval (usint, "POLLOUT") // writing now will not block

//
// HX: -D_XOPEN_SOURCE
//
macdef POLLRDNORM = $extval (usint, "POLLRDNORM") // normal data may be read
macdef POLLRDBAND = $extval (usint, "POLLRDBAND") // priority data may be read
macdef POLLWRNORM = $extval (usint, "POLLWRNORM") // writing now will not block
macdef POLLWRBAND = $extval (usint, "POLLWRBAND") // priority data may be written

//
// HX: these are only available in Linux
//
macdef POLLMSG = $extval (usint, "POLLMSG")
macdef POLLREMOVE = $extval (usint, "POLLREMOVE")
macdef POLLRDHUP = $extval (usint, "POLLRDHUP")

macdef POLLERR = $extval (usint, "POLLERR") // error condition
macdef POLLHUP = $extval (usint, "POLLHUP") // hung up
macdef POLLNVAL = $extval (usint, "POLLNVAL") // invalid polling request

(* ****** ****** *)
//
// HX: typedef usint = uint_short_t0ype
//
typedef pollfd_struct =
$extype_struct "ats_pollfd_type" of {
  fd= int // file descriptor to poll
, events= usint // types of events poller cares about
, revents= usint // types of events that actually occurred
} // end of [pollfd_struct]
typedef pollfd = pollfd_struct

(* ****** ****** *)
//
// HX-2010-09-26:
// By definition, we should assigne nfds_t (unsigned long) to nfds.
// Is this really necessary?
//
fun poll {n:nat}
  (fds: &(@[pollfd][n]), nfds: int n, timeout: int): int = "atslib_poll"
// end of [poll]

(* ****** ****** *)

(* end of [poll.sats] *)
