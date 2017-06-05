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

// author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

(* some commonly used macro definitions *)

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [macrodef.ats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(* macros in short form *)
// [orelse] and [andalso] are not defined in the curried
// form as they are infix operators
macdef orelse (x, y) = if ,(x) then true else ,(y)
macdef andalso (x, y) = if ,(x) then ,(y) else false

(* ****** ****** *)

macdef assertloc (b) = assert_errmsg (,(b), #LOCATION)

(* ****** ****** *)

(* only a macro in long form can be defined recursively *)
macrodef
rec power_mac x(*base:code*) n(*exponent:int*) =
  if n > 1 then `(,(x) * ,(power_mac x (n-1))) else (if n > 0 then x else `(1))
// end of [power_mac]

macdef
square_mac (x) = ,(power_mac x 2) and cube_mac (x)  = ,(power_mac x 3)

(* ****** ****** *)

macdef
print_mac (fprint, x) = let
  val (pf_stdout | ptr_stdout) = stdout_get ()
in
  ,(fprint) (file_mode_lte_w_w | !ptr_stdout, ,(x));
  stdout_view_set (pf_stdout | (*none*))
end // end of [print_mac]

macdef
prerr_mac (fprint, x) = let
  val (pf_stderr | ptr_stderr) = stderr_get ()
in
  ,(fprint) (file_mode_lte_w_w | !ptr_stderr, ,(x));
  stderr_view_set (pf_stderr | (*none*))
end // end of [prerr_mac]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [macrodef.ats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(* end of [macrodef.sats] *)
