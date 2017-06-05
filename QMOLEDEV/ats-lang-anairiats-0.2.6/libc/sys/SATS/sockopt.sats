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

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

%{#
#include "libc/sys/CATS/sockopt.cats"
%} // end of [%{#]

(* ****** ****** *)
//
// socket-level (in contrast to protocol-level)
//
macdef SOL_SOCKET = $extval (int, "SOL_SOCKET")

(* ****** ****** *)

macdef SO_DEBUG = $extval (int, "SO_DEBUG")
macdef SO_REUSEADDR = $extval (int, "SO_REUSEADDR")
macdef SO_TYPE = $extval (int, "SO_TYPE")
macdef SO_ERROR = $extval (int, "SO_ERROR")
macdef SO_DONTROUTE = $extval (int, "SO_DONTROUTE")
macdef SO_BROADCAST = $extval (int, "SO_BROADCAST")
macdef SO_SNDBUF = $extval (int, "SO_SNDBUF")
macdef SO_RCVBUF = $extval (int, "SO_RCVBUF")
macdef SO_SNDBUFFORCE = $extval (int, "SO_SNDBUFFORCE")
macdef SO_RCVBUFFORCE = $extval (int, "SO_RCVBUFFORCE")
macdef SO_KEEPALIVE = $extval (int, "SO_KEEPALIVE")
macdef SO_OOBINLINE = $extval (int, "SO_OOBINLINE")
macdef SO_NO_CHECK = $extval (int, "SO_NO_CHECK")
macdef SO_PRIORITY = $extval (int, "SO_PRIORITY")
macdef SO_LINGER = $extval (int, "SO_LINGER")
macdef SO_BSDCOMPAT = $extval (int, "SO_BSDCOMPAT")

macdef SO_PASSCRED = $extval (int, "SO_PASSCRED")
macdef SO_PEERCRED = $extval (int, "SO_PEERCRED")
macdef SO_RCVLOWAT = $extval (int, "SO_RCVLOWAT")
macdef SO_SNDLOWAT = $extval (int, "SO_SNDLOWAT")
macdef SO_RCVTIMEO = $extval (int, "SO_RCVTIMEO")
macdef SO_SNDTIMEO = $extval (int, "SO_SNDTIMEO")

macdef SO_SECURITY_AUTHENTICATION = $extval (int, "SO_SECURITY_AUTHENTICATION")
macdef SO_SECURITY_ENCRYPTION_TRANSPORT = $extval (int, "SO_SECURITY_ENCRYPTION_TRANSPORT")
macdef SO_SECURITY_ENCRYPTION_NETWORK = $extval (int, "SO_SECURITY_ENCRYPTION_NETWORK")

macdef SO_BINDTODEVICE = $extval (int, "SO_BINDTODEVICE")

macdef SO_ATTACH_FILTER = $extval (int, "SO_ATTACH_FILTER")
macdef SO_DETACH_FILTER = $extval (int, "SO_DETACH_FILTER")

macdef SO_PEERNAME = $extval (int, "SO_PEERNAME")
macdef SO_TIMESTAMP = $extval (int, "SO_TIMESTAMP")
macdef SCM_TIMESTAMP = $extval (int, "SCM_TIMESTAMP")

macdef SO_ACCEPTCONN = $extval (int, "SO_ACCEPTCONN")

macdef SO_PEERSEC = $extval (int, "SO_PEERSEC")
macdef SO_PASSSEC = $extval (int, "SO_PASSSEC")
macdef SO_TIMESTAMPNS = $extval (int, "SO_TIMESTAMPNS")
macdef SCM_TIMESTAMPNS = $extval (int, "SCM_TIMESTAMPNS")

macdef SO_MARK = $extval (int, "SO_MARK")

macdef SO_TIMESTAMPING = $extval (int, "SO_TIMESTAMPING")
macdef SCM_TIMESTAMPING = $extval (int, "SCM_TIMESTAMPING")

macdef SO_PROTOCOL = $extval (int, "SO_PROTOCOL")
macdef SO_DOMAIN = $extval (int, "SO_DOMAIN")

macdef SO_RXQ_OVFL = $extval (int, "SO_RXQ_OVFL")

(* ****** ****** *)
//
// HX: this one is used for SO_LINGER
//
typedef
linger_struct =
$extype_struct "ats_linger_type" of {
  l_onoff= int // on/off 1/0
, l_linger= int // time to linger in seconds
} // end of [linger_struct]
typedef linger = linger_struct

(* ****** ****** *)

(* end of [sockopt.sats] *)
