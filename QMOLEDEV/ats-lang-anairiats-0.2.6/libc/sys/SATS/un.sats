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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)  *)

(* ****** ****** *)

%{#
#include "libc/sys/CATS/un.cats"
%} // end of [%{#]

(* ****** ****** *)

staload SA = "libc/sys/SATS/sockaddr.sats"
typedef sa_family_t = $SA.sa_family_t
stadef socklen_t = $SA.socklen_t // int: length of a sockaddr
stadef sockaddr_struct = $SA.sockaddr_struct

(* ****** ****** *)

typedef sockaddr_un_struct =
$extype_struct "ats_sockaddr_un_type" of {
  sun_family= sa_family_t
, sun_path= @[byte][0] // @[byte][X] for X <= 100; X is implementation-dependent
} // end of [sockaddr_un_struct]
typedef sockaddr_un = sockaddr_un_struct
//
sta socklen_un : int // length of [sockaddr_un]
(*
stadef socklen_un = sizeof (sockaddr_un_struct)
*)
macdef socklen_un = $extval (socklen_t(socklen_un), "atslib_socklen_un")
//
praxi socklen_lte_un (): [socklen_un <= $SA.socklen_max] void
praxi sockaddr_un_trans {l:addr}
  (pf: !sockaddr_un_struct @ l >> sockaddr_struct(socklen_un) @ l): void
praxi sockaddr_trans_un {l:addr}
  (pf: !sockaddr_struct(socklen_un) @ l >> sockaddr_un_struct @ l): void
//
(* ****** ****** *)

(* end of [un.sats] *)
