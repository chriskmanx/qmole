(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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

absviewtype
dptr_addr_int_viewtype (l:addr, n:int)
stadef dptr = dptr_addr_int_viewtype 

castfn ptr_of_dptr
  {l:addr} {n:int} (x: !dptr(l, n)):<> ptr (l)
overload ptr_of with ptr_of_dptr

viewtypedef
datum (l:addr, n:int) =
  $extype_struct "datum" of {
  dptr= dptr(l, n), dsize= int(n)
} // end of [datum]

viewtypedef datum0 = [l:addr;n:int] datum (l, n)
viewtypedef datum1 =
  [l:addr;n:int | l > null; n >= 0] datum (l, n) // for valid data
// end of [datum1]

(* ****** ****** *)

viewdef fdptr (l:addr, n:int) = dptr (l, n) -<lin,prf> void

(* ****** ****** *)

fun datum_is_valid {n:int} {l:addr}
  (x: datum (l, n)): bool (l > null) = "mac#atslib_gdbm_datum_is_valid"
// end of [datum_is_valid]

fun datum_takeout_ptr
  {l:addr} {n:int} (x: datum (l, n)):<> dptr (l, n)
  = "mac#atslib_gdbm_datum_takeout_ptr"
// end of [datum_takeout_ptr]

(* ****** ****** *)
//
// HX: implemented in [gdbm.cats]
//
fun datum_make0_string
  (str: string): [l:agz;n:nat] (dptr (l, n) -<lin,prf> void | datum (l, n))
  = "mac#atslib_gdbm_datum_make0_string"
// end of [datum_make0_string]

fun datum_make1_string
  (str: string): datum1 = "mac#atslib_gdbm_datum_make1_string"
// end of [datum_make1_string]

fun datum_free (x: datum0): void = "mac#atslib_gdbm_datum_free"

(* ****** ****** *)

(* end of [datum.sats] *)
