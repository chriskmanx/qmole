(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
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
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer 2007
//

(* ****** ****** *)

%{^
#include "libc/CATS/stdio.cats"
#include "libc/CATS/stdlib.cats"
#include "libc/sys/CATS/types.cats"
#include "libc/CATS/unistd.cats"
%} // end of [%{^]

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

fn do_usage (cmd: string): void = begin
  printf ("The usage of %s is:\n", @(cmd));
  printf ("  1. %s <flags> --libats (* generating [libats.a] *)\n", @(cmd));
  printf ("  2. %s <flags> --libats_smlbas (* generating [libats_smlbas.a] *)\n", @(cmd));
  printf ("  3. %s <flags> --libats_lex (* generating [libats_lex.a] *)\n", @(cmd));
  printf ("  4. %s <flags> <infile> (* compiling <infile> and archiving it into [libats.a] *) \n", @(cmd));
end // end of [do_usage]

(* ****** ****** *)

dynload "basics.dats"
dynload "atscc.dats"
dynload "atslib.dats"

(* ****** ****** *)

implement main_prelude () = ()

(* ****** ****** *)

implement
main {n} (argc, argv) = let
  var param_rev: Strlst = STRLSTnil ()
  fun loop {i:nat | i <= n} .<n-i>. (
      argc: int n
    , argv: &(@[string][n])
    , i: int i
    , param_rev: &Strlst
    ) : void =
    if i < argc then let
      val arg = string1_of_string (argv.[i])
      val isempty = string_is_at_end (arg, 0)
      val () = if isempty then begin
        // empty // skip the empty argument
      end else begin case+ arg of
        | "--libats" => libats_make (param_rev)
        | "--libats_mt" => libats_mt_make (param_rev)
        | "--libats_smlbas" => libats_smlbas_make (param_rev)
        | "--libats_lex" => libats_lex_make (param_rev)
        | "-m32" => let
            val () = wordsize_target_set (4(*bytes*)) // 32-bit target machine
          in
            param_rev := STRLSTcons (arg, param_rev)
          end // end of [m64]
        | "-m64" => let
            val () = wordsize_target_set (8(*bytes*)) // 64-bit target machine
          in
            param_rev := STRLSTcons (arg, param_rev)
          end // end of [m64]
        | _ => if arg[0] = '-' then begin
            param_rev := STRLSTcons (arg, param_rev)
          end else let
            val libats_global = libats_global ()
            val () = ccomp_gcc_ar_libfile (param_rev, arg, libats_global)
            val () = ar_s_exn (libats_global) // this is equivalent to [ranlib]
          in
            // empty
          end // end of [_]
      end // end of [val]
    in
      loop (argc, argv, i+1, param_rev)
    end // end of [if]
  // end of [loop]
in
  case+ argc of
  | _ when argc >= 2 => loop (argc, argv, 1, param_rev)
  | _ (* argc = 1 *) => do_usage (argv.[0])
end // end of [main]

(* ****** ****** *)

(* end of [atslib_main.dats] *)
