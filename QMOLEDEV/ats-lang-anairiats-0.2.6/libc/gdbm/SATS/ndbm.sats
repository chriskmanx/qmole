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

#define ATS_STALOADFLAG 0 // there is need for staloading at run0time

(* ****** ****** *)

%{#
#include "libc/gdbm/CATS/ndbm.cats"
%} // end of [%{#]

(* ****** ****** *)

staload FCNTL = "libc/SATS/fcntl.sats"
typedef flag_t = $FCNTL.flag_t
staload TYPES = "libc/sys/SATS/types.sats"
typedef mode_t = $TYPES.mode_t

(* ****** ****** *)

#include "libc/gdbm/SATS/datum.sats"

(* ****** ****** *)

absviewtype DBMptr (lf:addr)
viewtypedef DBMptr0 = [lf:agez] DBMptr (lf)

castfn DBMptr_free_null (dbf: DBMptr null): ptr null
castfn ptr_of_DBMptr {l:addr} (dbf: !DBMptr l): ptr l
overload ptr_of with ptr_of_DBMptr

(* ****** ****** *)

(*
DBM * dbm_open (char *file , int flags , int mode )
*)

fun dbm_open (
  file: !READ(string), flags: flag_t, mode: mode_t
) : DBMptr0 = "mac#atslib_ndbm_open"

(* ****** ****** *)

fun dbm_close
  {lf:addr} (dbf: DBMptr lf): void = "mac#atslib_ndbm_close"
// end of [dbm_close]

(* ****** ****** *)

(*
datum dbm_fetch (DBM *dbf , datum key )
*)
fun dbm_fetch
  {lf:agz} {l:agz} {n:nat} (
  dbf: !DBMptr (lf), key: !datum (l, n)
) : [l1:addr;n1:int] (fdptr (l1, n1) | datum (l1, n1))
  = "mac#atslib_ndbm_fetch"

(* ****** ****** *)

macdef DBM_INSERT = $extval (int, "DBM_INSERT")
macdef DBM_REPLACE = $extval (int, "DBM_REPLACE")

(*
int dbm_store (DBM *dbf , datum key , datum content , int mode )
*)
fun dbm_store
  {lf:agz} {l1,l2:agz} {n1,n2:nat} (
  dbf: !DBMptr (lf), key: !datum (l1, n1), content: !datum (l2, n2), mode: int
) : int = "mac#atslib_ndbm_store"

(* ****** ****** *)

(*
int dbm_delete (DBM *dbf , datum key )
*)
fun dbm_delete
  {lf:agz} {l:agz} {n:nat} (
  dbf: !DBMptr lf, key: !datum (l, n)
) : int(*err*) = "mac#dbm_delete" // succ/fail: 0/-1
// end of [dbm_delete]

(* ****** ****** *)

(*
datum dbm_firstkey (DBM *dbf )
*)
fun dbm_firstkey
  {lf:agz} (
  dbf: !DBMptr (lf)
) : [l1:addr;n1:int] (fdptr (l1, n1) | datum (l1, n1))
  = "mac#atslib_ndbm_firstkey"

fun dbm_nextkey
  {lf:agz} (
  dbf: !DBMptr (lf)
) : [l1:addr;n1:int] (fdptr (l1, n1) | datum (l1, n1))
  = "mac#atslib_ndbm_nextkey"
  
(* ****** ****** *)

fun dbm_error
  {lf:agz} (dbf: !DBMptr (lf)): int = "mac#atslib_ndbm_error"
// end of [dbm_error]

fun dbm_clearerr
  {lf:agz} (dbf: !DBMptr (lf)): void = "mac#atslib_ndbm_clearerr"
// end of [dbm_clearerr]

(* ****** ****** *)

fun dbm_dirfno
  {lf:agz} (dbf: !DBMptr (lf)): int = "mac#atslib_ndbm_dirfno"
// end of [dbm_dirfno]

fun dbm_pagfno
  {lf:agz} (dbf: !DBMptr (lf)): int = "mac#atslib_ndbm_pagfno"
// end of [dbm_pagfno]

(* ****** ****** *)

fun dbm_rdonly
  {lf:agz} (dbf: !DBMptr (lf)): int = "mac#atslib_ndbm_rdonly"
// end of [dbm_rdonly]

(* ****** ****** *)

(* end of [ndbm.sats] *)
