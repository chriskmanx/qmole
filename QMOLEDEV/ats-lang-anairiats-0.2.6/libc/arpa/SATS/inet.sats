(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)  *)

(* ****** ****** *)

%{#
#include "libc/arpa/CATS/inet.cats"
%} // end of [%{#]

(* ****** ****** *)

staload IN = "libc/netinet/SATS/in.sats"
typedef in_port_nbo_t = $IN.in_port_nbo_t
typedef in_addr_hbo_t = $IN.in_addr_hbo_t
typedef in_addr_nbo_t = $IN.in_addr_nbo_t
typedef in_addr_struct = $IN.in_addr_struct

(* ****** ****** *)

abst@ype
uint16_t0ype_netbyteord = uint16_t0ype
typedef uint16_nbo = uint16_t0ype_netbyteord
fun htons
  (i: uint16_t0ype): uint16_t0ype_netbyteord = "atslib_htons"
fun ntohs
  (i: uint16_t0ype_netbyteord): uint16_t0ype = "atslib_ntohs"

abst@ype
uint32_t0ype_netbyteord = uint32_t0ype
typedef uint32_nbo = uint32_t0ype_netbyteord
fun htonl (i: uint32_t0ype): uint32_t0ype_netbyteord = "atslib_htonl"
fun ntohl (i: uint32_t0ype_netbyteord): uint32_t0ype = "atslib_ntohl"

(* ****** ****** *)

castfn
in_port_of_uint16_nbo (x: uint16_nbo): in_port_nbo_t

castfn
uint16_of_in_port_nbo (x: in_port_nbo_t): uint16_nbo

(* ****** ****** *)

fun inet_aton_err (
  cp: !READ(string)
, inp: &in_addr_struct? >> opt (in_addr_struct, b)
) : #[b:bool] bool b = "atslib_inet_aton_err"
// end of [inet_aton_err]

fun inet_aton_exn (
  cp: !READ(string), inp: &in_addr_struct? >> in_addr_struct
) :<!exn> void = "atslib_inet_aton_exn"
// end of [inet_aton_exn]

(* ****** ****** *)
//
// HX: note that this one cannot tell
// -1 from 255.255.255.255 (a valid address)
//
fun inet_addr
  (cp: !READ(string)): in_addr_nbo_t = "mac#atslib_inet_addr"
fun inet_network
  (cp: !READ(string)): in_addr_hbo_t = "mac#atslib_inet_network"

(* ****** ****** *)

fun inet_makeaddr
  (net: int, host: int): in_addr_struct = "mac#atslib_inet_makeaddr"
// end of [inet_makeaddr]

(* ****** ****** *)

//
// HX: this function is not reentrant
//
fun inet_ntoa (
  inp: in_addr_struct
) :<!ref> [l:agz] (
  strptr l -<lin,prf> void | strptr l
) = "mac#atslib_inet_ntoa"
// end of [inet_ntoa]

(* ****** ****** *)

fun inet_lnaof
  (addr: in_addr_struct): in_addr_hbo_t = "mac#atslib_inet_lnaof"
// end of [inet_lnaof]

fun inet_netof
  (addr: in_addr_struct): in_addr_hbo_t = "mac#atslib_inet_netof"
// end of [inet_netof]

(* ****** ****** *)

fun inet4_pton (
  cp: !READ(string) // af=AF_INET
, inp: &in_addr_struct? >> opt (in_addr_struct, i > 0)
) : #[i:int] int (i)
  = "mac#atslib_inet4_pton"
// end of [inet4_pton]

fun inet6_pton (
  cp: !READ(string) // af= AF_INET6
, inp: &in_addr_struct? >> opt (in_addr_struct, i > 0)
) : #[i:int] int (i)
   = "mac#atslib_inet6_pton"
// end of [inet6_pton]

(* ****** ****** *)

(* end of [inet.sats] *)
