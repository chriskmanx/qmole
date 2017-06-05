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
#include "libc/CATS/netdb.cats"
%} // end of [%{#]

(* ****** ****** *)

staload SA = "libc/sys/SATS/sockaddr.sats"
typedef sa_family_t = $SA.sa_family_t
stadef socklen_t = $SA.socklen_t
stadef sockaddr = $SA.sockaddr_struct
staload SOCKET = "libc/sys/SATS/socket.sats"
typedef socktype_t = $SOCKET.socktype_t
typedef sockprot_t = $SOCKET.sockprot_t

staload IN = "libc/netinet/SATS/in.sats"
typedef sockaddr_in = $IN.sockaddr_in_struct
typedef sockaddr_in6 = $IN.sockaddr_in6_struct
(*
staload UN = "libc/sys/SATS/un.sats"
typedef sockaddr_un = $UN.sockaddr_un_struct
*)

(* ****** ****** *)

abst@ype ai_flag_t = uint
//
macdef AI_NONE = $extval (ai_flag_t, "0x0")
//
macdef AI_ALL = $extval (ai_flag_t, "AI_ALL")
macdef AI_ADDRCONFIG = $extval (ai_flag_t, "AI_ADDRCONFIG")
macdef AI_CANNONNAME = $extval (ai_flag_t, "AI_CANNONNAME")
macdef AI_NUMERICHOST = $extval (ai_flag_t, "AI_NUMERICHOST")
macdef AI_NUMERICSERV = $extval (ai_flag_t, "AI_NUMERICSERV")
macdef AI_PASSIVE = $extval (ai_flag_t, "AI_PASSIVE")
macdef AI_V4MAPPED = $extval (ai_flag_t, "AI_V4MAPPED")
//
fun lor_ai_flag_ai_flag (
  x1: ai_flag_t, x2: ai_flag_t
) : ai_flag_t
  = "mac#atspre_lor_uint_uint"
overload lor with lor_ai_flag_ai_flag

(* ****** ****** *)

typedef
addrinfo_struct (n:int) =
$extype_struct "ats_addrinfo_type" of {
  ai_flags= ai_flag_t
, ai_family= sa_family_t
, ai_socktype= socktype_t
, ai_protocol= sockprot_t
, ai_addrlen=socklen_t(n)
// , ai_addr= ptr // sockaddr*
// , ai_canonname= string // char*
// , ai_next= ptr // struct addrinfo* 
} // end of [addrinfo_struct]
stadef addrinfo = addrinfo_struct
absviewtype addrinfoptr (l:addr) = ptr
viewtypedef addrinfoptr = [l:addr] addrinfoptr(l)

fun addrinfoptr_is_null
  {l:addr} (
  x: !addrinfoptr l
) : bool (l==null) = "mac#atspre_ptr_is_null"
fun addrinfoptr_isnot_null
  {l:addr} (
  x: !addrinfoptr l
) : bool (l > null) = "mac#atspre_ptr_isnot_null"

fun addrinfoptr_get_next
  {l:agz} (x: !addrinfoptr l)
  :<> [l1:addr] (
  minus (addrinfoptr l, addrinfoptr l1)
| addrinfoptr l1
) = "mac#atslib_addrinfoptr_get_next"
// end of [addrinfoptr_get_next]

fun addrinfoptr_get_canonname
  {l:agz} (
  x: !addrinfoptr l
) :<> [l1:addr] (
  minus (addrinfoptr l, strptr l1)
| strptr l1
) = "mac#atslib_addrinfoptr_get_canonname"
// end of [addrinfoptr_get_cannonname]

fun addrinfoptr_get_family
  {l:agz} (x: !addrinfoptr l):<> sa_family_t = "mac#atslib_addrinfoptr_get_family"
// end of [addrinfoptr_get_family]
fun addrinfoptr_get_socktype
  {l:agz} (x: !addrinfoptr l):<> socktype_t = "mac#atslib_addrinfoptr_get_socktype"
// end of [addrinfoptr_get_socktype]
fun addrinfoptr_get_protocol
  {l:agz} (x: !addrinfoptr l):<> sockprot_t = "mac#atslib_addrinfoptr_get_protocol"
// end of [addrinfoptr_get_protocol]

(* ****** ****** *)
//
// HX: if the info is obtained by setting hint.ai_family = AF_INET
//
fun addrinfoptr_get_addr_in
  {l:agz} (
  x: !addrinfoptr l
) :<> [l1:addr] (
  sockaddr_in @ l1, minus (addrinfoptr l, sockaddr_in @ l1)
|  ptr l1
) = "mac#atslib_addrinfoptr_get_addr"
// end of [fun]

//
// HX: if the info is obtained by setting hint.ai_family = AF_INET6
//
fun addrinfoptr_get_addr_in6
  {l:agz} (
  x: !addrinfoptr l
) :<> [l1:addr] (
  sockaddr_in6 @ l1, minus (addrinfoptr l, sockaddr_in6 @ l1)
| ptr l1
) = "mac#atslib_addrinfoptr_get_addr"
// end of [fun]

(*
// HX-2010-10-13: I doubt this is usefull
fun addrinfoptr_get_addr_un {l:agz} (x: !addrinfoptr l)
  :<> [l1:addr] (
  sockaddr_un @ l1, minus (addrinfoptr l, sockaddr_un @ l1)
|  ptr l1
) = "mac#atslib_addrinfoptr_get_addr"
// end of [addrinfoptr_get_addr_un]
*)

(* ****** ****** *)

fun getaddrinfo (
  nodename: string
, portname: string
, hint: &addrinfo(0)
, infop: &addrinfoptr? >> opt (addrinfoptr, i == 0)
) : #[i:int | i <= 0] int (i) // HX: error codes are negative
  = "mac#atslib_getaddrinfo"
// end of [getaddrinfo]

(* ****** ****** *)

fun gai_strerror (
  code: int
) : [l:agz] (
  strptr l -<lin,prf> void | strptr l
) = "mac#atslib_gai_strerror"

(* ****** ****** *)

fun freeaddrinfo
  (infop: addrinfoptr): void = "mac#atslib_freeaddrinfo"
// end of [freeaddrinfo]

(* ****** ****** *)

typedef
hostent_struct =
$extype_struct
"ats_hostent_type" of {
  h_addrtype= int // address family
, h_length= int // length of each address
// , h_name= string // official hostname
// , h_aliases= ptr(strarr) // array of alternative names
// , h_addr_list= ptr(strarr) // array of pointers to network address
} // end of [hostent_struct]
typedef hostent = hostent_struct

fun hostent_get_name (
  h: &hostent
) :<!ref> [l:agz] (
  strptr l -<lin,prf> void
| strptr l
) = "mac#atslib_hostent_get_name"
// end of [hostent_get_name]

fun hostent_get_aliases (
  h: &hostent
) :<!ref> [n:nat;l:agz] (
  ptrarr n @ l, ptrarr n @ l -<lin,prf> void
| ptr l
) = "mac#atslib_hostent_get_aliases"
// end of [hostent_get_aliases]

fun hostent_get_addr_list (
  h: &hostent
) :<!ref> [n:nat;l:agz] (
  ptrarr n @ l, ptrarr n @ l -<lin,prf> void
| ptr l
) = "mac#atslib_hostent_get_addr_list"
// end of [hostent_get_addr_list]

(* ****** ****** *)

absview sethostent_v

fun sethostent
  {b:bool} (
  stayopen: bool (b)
) : (
  sethostent_v | void
) = "mac#atslib_sethostent"
// end of [sethostent]

fun gethostent (
  pf: !sethostent_v | (*none*)
) :<!ref> [l:addr] (
  vptroutopt (hostent, l)
| ptr l
) = "mac#atslib_gethostent"
// end of [gethostent]

fun endhostent (
  pf: sethostent_v | (*none*)
) : void = "mac#atslib_endhostent"
// end of [endhostent]

(* ****** ****** *)

//
// HX: [gethostbyname] does not handle [IPv6] addresses
//
fun gethostbyname (
  name: string
) :<!ref> [l:addr] (
  vptroutopt (hostent, l)
| ptr l
) = "mac#atslib_gethostbyname"
// end of [gethostbyname]

//
// HX: [addr] is often obtained by calling [inet_addr]
//
fun gethostbyaddr
  {a:t@ype} (
  addr: &a, n: sizeof_t(a), af: sa_family_t
) :<!ref> [l:addr] (
  vptroutopt (hostent, l)
| ptr l
) = "mac#atslib_gethostbyaddr"
// end of [gethostbyaddr]

(* ****** ****** *)

abst@ype niflag_t = int
macdef NI_ZERO = $extval (niflag_t, "0")
macdef NI_NUMERICHOST = $extval (niflag_t, "NI_NUMERICHOST")
macdef NI_NUMERICSERV = $extval (niflag_t, "NI_NUMERICSERV")
macdef NI_NOFQDN = $extval (niflag_t, "NI_NOFQDN")
macdef NI_NAMEREQD = $extval (niflag_t, "NI_NAMEREQD")
macdef NI_DGRAM = $extval (niflag_t, "NI_DGRAM")
macdef NI_IDN = $extval (niflag_t, "NI_IDN") // HX: -D_GNU_SOUCRCE

//
// HX: note that [a] should be a [sockaddr]!
// [nodename] and [servname] are fill with null-terminated strings if
// a call to [getnameinfo] succeeds
//
fun getnameinfo{a:t@ype} {n1,n2:nat} (
    sa: &a, salen: sizeof_t (a)
  , nodename: &b0ytes(n1) >> bytes(n1), nodelen: size_t(n1)
  , servname: &b0ytes(n2) >> bytes(n2), servlen: size_t(n1)
  , flags: niflag_t
  ) : [i:int | i <= 0] int (i) = "mac#atslib_getnameinfo" // 0/neg : succ/fail
// end of [getnameinfo]

(* ****** ****** *)

fun gethostid (): lint = "mac#atslib_gethostid"
fun sethostid (id: lint): int = "mac#atslib_sethostid" // for superuser only

(* ****** ****** *)

(* end of [netdb.sats] *)
