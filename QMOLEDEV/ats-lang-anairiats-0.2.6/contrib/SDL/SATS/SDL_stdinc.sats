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
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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
//
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January, 2010
//
(* ****** ****** *)

abst@ype Sint8 = int8_t0ype
abst@ype Uint8 = uint8_t0ype

castfn Sint8_of_int (x: int):<> Sint8
overload Sint8 with Sint8_of_int
castfn int_of_Sint8 (x: Sint8):<> int

castfn Uint8_of_int (x: int):<> Uint8
overload Uint8 with Uint8_of_int
castfn int_of_Uint8 (x: Uint8):<> int

castfn Uint8_of_uint (x: uint):<> Uint8
overload Uint8 with Uint8_of_uint
castfn uint_of_Uint8 (x: Uint8):<> uint

fun land_Uint8_Uint8 (x1: Uint8, x2: Uint8): Uint8
  = "atsctrb_land_Uint8_Uint8" // function
overload land with land_Uint8_Uint8

fun lor_Uint8_Uint8 (x1: Uint8, x2: Uint8): Uint8
  = "atsctrb_lor_Uint8_Uint8" // function
overload lor with lor_Uint8_Uint8

(* ****** ****** *)

abst@ype Sint16 = int16_t0ype
abst@ype Uint16 = uint16_t0ype

castfn Sint16_of_int (x: int):<> Sint16
overload Sint16 with Sint16_of_int
castfn int_of_Sint16 (x: Sint16):<> int

castfn Uint16_of_int (x: int):<> Uint16
overload Uint16 with Uint16_of_int
castfn int_of_Uint16 (x: Uint16):<> int

castfn Uint16_of_uint (x: uint):<> Uint16
overload Uint16 with Uint16_of_uint
castfn uint_of_Uint16 (x: Uint16):<> uint

(* ****** ****** *)

abst@ype Sint32 = int32_t0ype
abst@ype Uint32 = uint32_t0ype

castfn Sint32_of_int (x: int):<> Sint32
overload Sint32 with Sint32_of_int
castfn int_of_Sint32 (x: Sint32):<> int

castfn Uint32_of_int (x: int):<> Uint32
overload Uint32 with Uint32_of_int
castfn int_of_Uint32 (x: Uint32):<> int

castfn Uint32_of_uint (x: uint):<> Uint32
overload Uint32 with Uint32_of_uint
castfn uint_of_Uint32 (x: Uint32):<> uint

(* ****** ****** *)

abst@ype Sint64 = int64_t0ype
abst@ype Uint64 = uint64_t0ype

(* ****** ****** *)

fun add_Uint16_Uint16
  (x1: Uint16, x2: Uint16): Uint16
  = "atsctrb_add_Uint16_Uint16" // function
overload + with add_Uint16_Uint16

fun sub_Uint16_Uint16
  (x1: Uint16, x2: Uint16): Uint16
  = "atsctrb_sub_Uint16_Uint16" // function
overload - with sub_Uint16_Uint16

fun land_Uint16_Uint16
  (x1: Uint16, x2: Uint16): Uint16
  = "atsctrb_land_Uint16_Uint16" // function
overload land with land_Uint16_Uint16

fun lor_Uint16_Uint16
  (x1: Uint16, x2: Uint16): Uint16
  = "atsctrb_lor_Uint16_Uint16" // function
overload lor with lor_Uint16_Uint16

(* ****** ****** *)

fun add_Uint32_Uint32
  (x1: Uint32, x2: Uint32): Uint32
  = "atsctrb_add_Uint32_Uint32" // function
overload + with add_Uint32_Uint32

fun sub_Uint32_Uint32
  (x1: Uint32, x2: Uint32): Uint32
  = "atsctrb_sub_Uint32_Uint32" // function
overload - with sub_Uint32_Uint32

fun land_Uint32_Uint32
  (x1: Uint32, x2: Uint32): Uint32
  = "atsctrb_land_Uint32_Uint32" // function
overload land with land_Uint32_Uint32

fun lor_Uint32_Uint32
  (x1: Uint32, x2: Uint32): Uint32
  = "atsctrb_lor_Uint32_Uint32" // function
overload lor with lor_Uint32_Uint32

(* ****** ****** *)

(* end of [SDL_stdinc.sats] *)
