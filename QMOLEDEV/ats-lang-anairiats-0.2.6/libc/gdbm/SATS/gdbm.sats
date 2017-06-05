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
#include "libc/gdbm/CATS/gdbm.cats"
%} // end of [%{#]

(* ****** ****** *)

staload FCNTL = "libc/SATS/fcntl.sats"
typedef flag_t = $FCNTL.flag_t
staload TYPES = "libc/sys/SATS/types.sats"
typedef mode_t = $TYPES.mode_t

(* ****** ****** *)

#include "libc/gdbm/SATS/datum.sats"

(* ****** ****** *)

absviewtype GDBM_FILE (lf:addr) // HX: a boxed viewtype

castfn GDBM_FILE_free_null
  (dbf: GDBM_FILE (null)):<> ptr null
castfn ptr_of_GDBM_FILE {lf:addr} (dbf: !GDBM_FILE lf):<> ptr (lf)
overload ptr_of with ptr_of_GDBM_FILE

(* ****** ****** *)
//
abst@ype gdbm_error = int
//
macdef
GDBM_NO_ERROR = $extval (gdbm_error, "GDBM_NO_ERROR")
macdef
GDBM_MALLOC_ERROR = $extval (gdbm_error, "GDBM_MALLOC_ERROR")
macdef
GDBM_BLOCK_SIZE_ERROR = $extval (gdbm_error, "GDBM_BLOCK_SIZE_ERROR")
macdef
GDBM_FILE_OPEN_ERROR = $extval (gdbm_error, "GDBM_FILE_OPEN_ERROR")
macdef
GDBM_FILE_WRITE_ERROR = $extval (gdbm_error, "GDBM_FILE_WRITE_ERROR")
macdef
GDBM_FILE_SEEK_ERROR = $extval (gdbm_error, "GDBM_FILE_SEEK_ERROR")
macdef
GDBM_FILE_READ_ERROR = $extval (gdbm_error, "GDBM_FILE_READ_ERROR")
macdef
GDBM_BAD_MAGIC_NUMBER = $extval (gdbm_error, "GDBM_BAD_MAGIC_NUMBER")
macdef
GDBM_EMPTY_DATABASE = $extval (gdbm_error, "GDBM_EMPTY_DATABASE")
macdef
GDBM_CANT_BE_READER = $extval (gdbm_error, "GDBM_CANT_BE_READER")
macdef
GDBM_CANT_BE_WRITER = $extval (gdbm_error, "GDBM_CANT_BE_WRITER")
macdef
GDBM_READER_CANT_DELETE = $extval (gdbm_error, "GDBM_READER_CANT_DELETE")
macdef
GDBM_READER_CANT_STORE = $extval (gdbm_error, "GDBM_READER_CANT_STORE")
macdef
GDBM_READER_CANT_REORGANIZE = $extval (gdbm_error, "GDBM_READER_CANT_REORGANIZE")
macdef
GDBM_UNKNOWN_UPDATE = $extval (gdbm_error, "GDBM_UNKNOWN_UPDATE")
macdef
GDBM_ITEM_NOT_FOUND = $extval (gdbm_error, "GDBM_ITEM_NOT_FOUND")
macdef
GDBM_REORGANIZE_FAILED = $extval (gdbm_error, "GDBM_REORGANIZE_FAILED")
macdef
GDBM_CANNOT_REPLACE = $extval (gdbm_error, "GDBM_CANNOT_REPLACE")
macdef
GDBM_ILLEGAL_DATA = $extval (gdbm_error, "GDBM_ILLEGAL_DATA")
macdef
GDBM_OPT_ALREADY_SET = $extval (gdbm_error, "GDBM_OPT_ALREADY_SET")
macdef
GDBM_OPT_ILLEGAL = $extval (gdbm_error, "GDBM_OPT_ILLEGAL")
macdef
GDBM_BYTE_SWAPPED = $extval (gdbm_error, "GDBM_BYTE_SWAPPED")
macdef
GDBM_BAD_FILE_OFFSET = $extval (gdbm_error, "GDBM_BAD_FILE_OFFSET")
macdef
GDBM_BAD_OPEN_FLAGS = $extval (gdbm_error, "GDBM_BAD_OPEN_FLAGS")
//
fun gdbm_errno_get (): gdbm_error = "mac#atslib_gdbm_errno_get"
//
(* ****** ****** *)

val gdbm_version
  : string = "mac#gdb_version" // pre-allocated
val gdbm_version_number
  : array (int, 3) = "mac#gdb_version_number" // pre-allocated
// end of [gdbm_version_number]

fun gdm_version_cmp
  (x: &(@[int][3]), y: &(@[int][3])):<> int
  = "mac#gdbm_version_cmp"
// end of [gdm_version_cmp]
  
(* ****** ****** *)

(*
#define  GDBM_READER  0		/* A reader. */
#define  GDBM_WRITER  1		/* A writer. */
#define  GDBM_WRCREAT 2		/* A writer.  Create the db if needed. */
#define  GDBM_NEWDB   3		/* A writer.  Always create a new db. */
#define  GDBM_FAST    0x10	/* Write fast! => No fsyncs.  OBSOLETE. */
#define  GDBM_SYNC    0x20	/* Sync operations to the disk. */
#define  GDBM_NOLOCK  0x40	/* Don't do file locking operations. */
*)
macdef GDBM_READER = $extval (int, "GDBM_READER")
macdef GDBM_WRITER = $extval (int, "GDBM_WRITER")
macdef GDBM_WRCREAT = $extval (int, "GDBM_WRCREAT")
macdef GDBM_NEWDB = $extval (int, "GDBM_NEWDB")
macdef GDBM_FAST = $extval (int, "GDBM_FAST")
macdef GDBM_SYNC = $extval (int, "GDBM_SYNC")
macdef GDBM_NOLOCK = $extval (int, "GDBM_NOLOCK")
(*
GDBM_FILE gdbm_open (name, block_size, flags, mode, fatal_func);
*)
//
// fatal_func: (!READ(string)) -> void
//
fun gdbm_open (
  name: !READ(string)
, block_size: int, flags: int, mode: mode_t, fatal_func: ptr
) : [lf:agez] GDBM_FILE (lf)
  = "mac#atslib_gdbm_open"
// end of [gdbm_open]

(* ****** ****** *)

(*
void gdbm_close(dbf);
*)
fun gdbm_close
  {lf:addr} (
  dbf: GDBM_FILE lf
) : void = "mac#atslib_gdbm_close"
// end of [gdbm_close]

(* ****** ****** *)

(*
int gdbm_store(dbf, key, content, flag);
*)

(*
#define  GDBM_INSERT  0		/* Never replace old data with new. */
#define  GDBM_REPLACE 1		/* Always replace old data with new. */
*)
macdef GDBM_INSERT = $extval (int, "GDBM_INSERT")
macdef GDBM_REPLACE = $extval (int, "GDBM_REPLACE")

fun gdbm_store
  {lf:agz} {l1,l2:addr} {n1,n2:nat} (
  dbf: !GDBM_FILE lf, key: !datum(l1, n1), content: !datum(l2, n2), flag: int
) : int(*err*)
  = "mac#atslib_gdbm_store"
// end of [gdbm_store]

(* ****** ****** *)

(*
datum gdbm_fetch(dbf, key);
*)
fun gdbm_fetch
  {lf:agz} {l:agz} {n:int} (
  dbf: !GDBM_FILE lf, key: !datum (l, n)
) : datum0 = "mac#atslib_gdbm_fetch" // the return value is malloced
// end of [gdbm_fetch]

(*
int gdbm_exists(dbf, key);
*)
fun gdbm_exists
  {lf:agz} {l:agz} {n:int} (
  dbf: !GDBM_FILE lf, key: !datum (l, n)
) : int // true/false: 1/0
  = "mac#atslib_gdbm_exists"
// end of [gdbm_exists]

(* ****** ****** *)

(*
int gdbm_delete(dbf, key);
*)
fun gdbm_delete
  {lf:agz} {l:agz} {n:int} (
  dbf: !GDBM_FILE lf, key: !datum (l, n)
) : int // succ/fail: 0/-1
  = "mac#atslib_gdbm_delete"
// end of [gdbm_delete]

(* ****** ****** *)

(*
datum gdbm_firstkey(dbf);
*)
fun gdbm_firstkey
  {lf:agz} (
  dbf: !GDBM_FILE lf
) : datum0
  = "mac#atslib_gdbm_firstkey"
// end of [gdbm_firstkey]

(* ****** ****** *)

(*
datum gdbm_nextkey(dbf, key);
*)

fun gdbm_nextkey
  {lf:agz} {l:agz} {n:int} (
  dbf: !GDBM_FILE l, prev: !datum(l, n)
) : datum0 = "mac#atslib_gdbm_nextkey"
// end of [gdbm_nextkey]

(* ****** ****** *)

(*
int gdbm_reorganize(dbf);
*)
fun gdbm_reorganize {lf:agz}
  (dbf: !GDBM_FILE lf): int = "mac#gdbm_reorganize"
// end of [gdbm_reorganize]

(* ****** ****** *)

(*
void gdbm_sync(dbf);
*)
fun gdbm_sync {lf:agz}
  (dbf: !GDBM_FILE lf): void = "mac#gdbm_sync"
// end of [gdbm_sync]

(* ****** ****** *)

(*
int gdbm_export (GDBM FILE dbf, const char *exportfile,int flag, int mode);
*)
fun gdbm_export {lf:agz} (
  dbf: !GDBM_FILE lf, exportfile: !READ(string), flag: int, mode: mode_t
) : int = "mac#atslib_gdbm_export"
// end of [gdbm_export]

(* ****** ****** *)

(*
int gdbm_import (GDBM FILE dbf , const char *importfile , int flag);
*)
fun gdbm_import {lf:agz} (
  dbf: !GDBM_FILE lf, importfile: !READ(string), flag: int
) : int = "mac#atslib_gdbm_import"
// end of [gdbm_import]

(* ****** ****** *)

(*
char *gdbm_strerror(int errno);
*)
fun gdbm_strerror
  (errno: gdbm_error): string(*pre-allocated*) = "mac#atslib_gdbm_strerror"
// end of [gdbm_strerror]

(* ****** ****** *)

(*
int gdbm_setopt(dbf, option, value, size);
*)
abst@ype
gdbmsetopt(a:t@ype) = int
abst@ype
gdbmgetopt(a:t@ype) = int
//
macdef
GDBM_CACHESIZE = $extval (gdbmsetopt(size_t), "GDBM_CACHESIZE")
macdef
GDBM_SETCACHESIZE = $extval (gdbmsetopt(size_t), "GDBM_SETCACHESIZE")
macdef
GDBM_GETCACHESIZE = $extval (gdbmgetopt(size_t), "GDBM_GETCACHESIZE")
//
macdef GDBM_GETFLAGS = $extval (gdbmgetopt(int), "GDBM_GETFLAGS")
//
macdef GDBM_FASTMODE = $extval (gdbmsetopt(int), "GDBM_FASTMODE")
//
macdef
GDBM_SYNCMODE = $extval (gdbmsetopt(int), "GDBM_SYNCMODE")
macdef
GDBM_SETSYNCMODE = $extval (gdbmsetopt(int), "GDBM_SETSYNCMODE")
macdef
GDBM_GETSYNCMODE = $extval (gdbmgetopt(int), "GDBM_GETSYNCMODE")
//
macdef
GDBM_COALESCEBLKS = $extval (gdbmsetopt(int), "GDBM_COALESCEBLKS")
macdef
GDBM_SETCOALESCEBLKS = $extval (gdbmsetopt(int), "GDBM_SETCOALESCEBLKS")
macdef
GDBM_GETCOALESCEBLKS = $extval (gdbmgetopt(int), "GDBM_GETCOALESCEBLKS")
//
macdef
GDBM_SETMAXMAPSIZE = $extval (gdbmsetopt(size_t), "GDBM_SETMAXMAPSIZE")
macdef
GDBM_GETMAXMAPSIZE = $extval (gdbmgetopt(size_t), "GDBM_GETMAXMAPSIZE")
//
macdef
GDBM_SETMMAP = $extval (gdbmsetopt(int), "GDBM_SETMMAP")
macdef
GDBM_GETMMAP = $extval (gdbmgetopt(int), "GDBM_GETMMAP")
//
(*
GDBM_GETDBNAME = $extval (gdbmgetopt(ptr), "GDBM_GETDBNAME")
*)
//
(* ****** ****** *)

fun gdbm_setopt
  {a:t@ype} {lf:agz} (
  dbf: !GDBM_FILE lf
, option: gdbmsetopt(a), value: &a, size: sizeof_t(a)
) : int(*err*) = "mac#atslib_gdbm_setopt"
// end of [gdbm_setopt]

fun gdbm_getopt
  {a:t@ype} {lf:agz} (
  dbf: !GDBM_FILE lf
, option: gdbmgetopt(a), value: &a? >> a, size: sizeof_t(a)
) : int(*err*) = "mac#atslib_gdbm_getopt"
// end of [gdbm_setopt]

fun gdbm_getdbname {lf:agz}
  (dbf: !GDBM_FILE (lf)): strptr0 = "atslib_gdbm_getdbname"
// end of [gdbm_getdbname]

(* ****** ****** *)

(*
int gdbm_fdesc(dbf);
*)
fun gdbm_fdesc {lf:agz}
  (dbf: !GDBM_FILE lf): int(*fd*) = "mac#gdbm_fdesc" // no failure
// end of [gdbm_fdesc]

(* ****** ****** *)

(* end of [gdbm.sats] *)
