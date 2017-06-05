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

abst@ype sa_family_t = $extype"sa_family_t"
//
// HX: these are the primary ones:
//
macdef AF_UNIX = $extval (sa_family_t, "AF_UNIX")
macdef AF_INET = $extval (sa_family_t, "AF_INET")
macdef AF_INET6 = $extval (sa_family_t, "AF_INET6")
macdef AF_UNSPEC = $extval (sa_family_t, "AF_UNSPEC")
//
macdef AF_LOCAL = $extval (sa_family_t, "AF_LOCAL")
macdef AF_FILE = $extval (sa_family_t, "AF_FILE")
macdef AF_AX25 = $extval (sa_family_t, "AF_AX25")
macdef AF_IPX = $extval (sa_family_t, "AF_IPX")
macdef AF_APPLETALK = $extval (sa_family_t, "AF_APPLETALK")
macdef AF_NETROM = $extval (sa_family_t, "AF_NETROM")
macdef AF_BRIDGE = $extval (sa_family_t, "AF_BRIDGE")
macdef AF_ATMPVC = $extval (sa_family_t, "AF_ATMPVC")
macdef AF_X25 = $extval (sa_family_t, "AF_X25")
macdef AF_ROSE = $extval (sa_family_t, "AF_ROSE")
macdef AF_DECnet = $extval (sa_family_t, "AF_DECnet")
macdef AF_NETBEUI = $extval (sa_family_t, "AF_NETBEUI")
macdef AF_SECURITY = $extval (sa_family_t, "AF_SECURITY")
macdef AF_KEY = $extval (sa_family_t, "AF_KEY")
macdef AF_NETLINK = $extval (sa_family_t, "AF_NETLINK")
macdef AF_ROUTE = $extval (sa_family_t, "AF_ROUTE")
macdef AF_PACKET = $extval (sa_family_t, "AF_PACKET")
macdef AF_ASH = $extval (sa_family_t, "AF_ASH")
macdef AF_ECONET = $extval (sa_family_t, "AF_ECONET")
macdef AF_ATMSVC = $extval (sa_family_t, "AF_ATMSVC")
macdef AF_RDS = $extval (sa_family_t, "AF_RDS")
macdef AF_SNA = $extval (sa_family_t, "AF_SNA")
macdef AF_IRDA = $extval (sa_family_t, "AF_IRDA")
macdef AF_PPPOX = $extval (sa_family_t, "AF_PPPOX")
macdef AF_WANPIPE = $extval (sa_family_t, "AF_WANPIPE")
macdef AF_LLC = $extval (sa_family_t, "AF_LLC")
macdef AF_CAN = $extval (sa_family_t, "AF_CAN")
macdef AF_TIPC = $extval (sa_family_t, "AF_TIPC")
macdef AF_BLUETOOTH = $extval (sa_family_t, "AF_BLUETOOTH")
macdef AF_IUCV = $extval (sa_family_t, "AF_IUCV")
macdef AF_RXRPC = $extval (sa_family_t, "AF_RXRPC")
macdef AF_ISDN = $extval (sa_family_t, "AF_ISDN")
macdef AF_PHONET = $extval (sa_family_t, "AF_PHONET")
macdef AF_IEEE802154 = $extval (sa_family_t, "AF_IEEE802154")
macdef AF_MAX = $extval (sa_family_t, "AF_MAX")

(* ****** ****** *)

sta socklen_max: int // length of [sockaddr_storage]

(* ****** ****** *)

abst@ype socklen_t(n:int) = $extype"socklen_t"
castfn socklen_of_int1 {n:nat} (n: int n): socklen_t n
castfn socklen_of_size1 {n:nat} (n: size_t n): socklen_t n

(* ****** ****** *)

abst@ype sockaddr_struct(n:int) // a generic type

(* ****** ****** *)

abst@ype sockaddr_storage_struct
  = $extype"ats_sockaddr_storage_type"
typedef sockaddr_max_struct = sockaddr_storage_struct

(* ****** ****** *)

macdef socklen_max =
  $extval (socklen_t(socklen_max), "atslib_socklen_max")
praxi sockaddr_max_trans {l:addr}
  (pf: !sockaddr_max_struct @ l >> sockaddr_struct(socklen_max) @ l): void
praxi sockaddr_trans_max {l:addr}
  (pf: !sockaddr_struct(socklen_max) @ l >> sockaddr_max_struct @ l): void

(* ****** ****** *)

(* end of [sockaddr.sats] *)
