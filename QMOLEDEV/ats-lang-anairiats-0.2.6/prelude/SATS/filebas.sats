(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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

(* Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)
(* Time: 2008 *)

(* ****** ****** *)
//
// HX: Attention!
//
// The functions declared here are mostly done for the sake of convenience.
// Please turn on GC if functions like [input_line] and [output_line] are
// called repeatedly.
//
// For full-fledged IO support, please see [$ATSHOME/libc/SATS/stdio.sats].
//
(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)
//
// some basic IO operations
//

#if VERBOSE_PRELUDE #then
#print "Loading [filebas.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

typedef file_mode = [m:file_mode] file_mode (m)

(* ****** ****** *)

macdef file_mode_r = $extval (file_mode r, "\"r\"")
macdef file_mode_rr = $extval (file_mode rw, "\"r+\"")
macdef file_mode_w = $extval (file_mode w, "\"w\"")
macdef file_mode_ww = $extval (file_mode rw, "\"w+\"")
macdef file_mode_a = $extval (file_mode w, "\"a\"")
macdef file_mode_aa = $extval (file_mode rw, "\"a+\"")

(* ****** ****** *)

macdef EOF = $extval (int, "EOF")
macdef stdin_ref = $extval (FILEref, "stdin")
macdef stdout_ref = $extval (FILEref, "stdout")
macdef stderr_ref = $extval (FILEref, "stderr")

(* ****** ****** *)

fun open_file_exn // exit on failure
  (path: string, mode: file_mode): FILEref = "atslib_fopen_exn"
// end of [open_file_exn]

fun close_file_exn (fil: FILEref): void = "atslib_fclose_exn"

fun reopen_file_exn // exit on failure
  (path: string, mode: file_mode, fil: FILEref): void = "atslib_freopen_exn"
// end of [reopen_file_exp]

(* ****** ****** *)

fun fflush_exn (fil: FILEref): void = "atslib_fflush_exn"

(* ****** ****** *)

fun test_file_eof (fil: FILEref): bool = "atslib_feof"

(* ****** ****** *)

fun test_file_exists // [stat] is called
  (path: string): bool = "atspre_test_file_exists"
// end of [test_file_exists]

(* ****** ****** *)
//
// HX-2011-02-16:
// [stat] is called to obtain the mode of a given file
// for [f] to be applied to it.
//
fun test_file_mode (path: string, f: uint -> bool): Int
//
// HX: [stat] is called // ~1/0/1: error/false/true
//
fun test_file_isblk (path: string): int = "atspre_test_file_isblk"
fun test_file_ischr (path: string): int = "atspre_test_file_ischr"
fun test_file_isdir (path: string): int = "atspre_test_file_isdir"
fun test_file_isfifo (path: string): int = "atspre_test_file_isfifo"
fun test_file_isreg (path: string): int = "atspre_test_file_isreg"
//
// HX: [lstat] is called // ~1/0/1: error/false/true
//
fun test_file_islnk (path: string): int = "atspre_test_file_islnk"

(* ****** ****** *)

(*
** [input_line] reads a sequence of characters ending with a newline
** character or EOF; the string returned by [input_line] does not include
** the newline character it reads; if [input_line] encounters EOF when it
** starts, then stropt_none (a null pointer) is returned.
*)
fun input_line (fil: FILEref): Stropt
fun input_line_vt (fil: FILEref): strptr0

(*
** [output_line] writes to a given file handle a string plus a newline
** character at the end.
*)
fun output_line (fil: FILEref, line: string): void

(* ****** ****** *)
//
// HX: making a lazy char stream out of a file handle
//
fun char_stream_make_file (fil: FILEref):<!laz> stream (char)

// making a lazy line stream out of a file handle
// note that the newline character at the end of each line is dropped
fun line_stream_make_file (fil: FILEref):<!laz> stream (string)

(* ****** ****** *)
//
// HX: making a _linear_ lazy char stream out of a file handle
//
fun char_stream_vt_make_file
  {m:file_mode} {l:addr} (
  pf_mod: file_mode_lte (m, r), pf_fil: FILE m @ l | p_fil: ptr l
) :<!laz> stream_vt (char) // end of [char_stream_vt_make_file]

(* ****** ****** *)
//
// HX:
// making a _linear_ lazy line stream out of a file handle
// note that the newline character at the end of each line is dropped
//
fun line_stream_vt_make_file
  {m:file_mode} {l:addr} (
  pf_mod: file_mode_lte (m, r), pf_fil: FILE m @ l | p_fil: ptr l
) :<!laz> stream_vt (strptr1) // end of [line_stream_vt_make_file]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [filebas.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [filebas.sats] *)
