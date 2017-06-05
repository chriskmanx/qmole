(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS - Unleashing the Power of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
 * Free Software Foundation; either version 2.1, or (at your option)  any
 * later version.
 * 
 * ATS is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
 * for more details.
 * 
 * You  should  have  received  a  copy of the GNU General Public License
 * along  with  ATS;  see the  file COPYING.  If not, please write to the
 * Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 *)

(* ****** ****** *)

// July 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

staload STDIO = "libc/SATS/stdio.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

dynload "intset.dats"
dynload "charset.dats"
dynload "states.dats"

dynload "position.dats"
dynload "token.dats"

dynload "parser.dats"
dynload "lexgen.dats"

(* ****** ****** *)

local

dataviewtype input =
  | {l:addr} INPUTsome of (FILE r @ l | ptr l)
  | INPUTnone of ()

val theInputRef = ref<input> (INPUTnone ())

in // in of [local]

implement the_atslex_input_fin () = let
  val (vbox pf | p) = ref_get_view_ptr (theInputRef)
in
  case+ !p of
  | ~INPUTsome (pf_fil | p_fil) => let
      val () = $STDIO.fclose1_exn (pf_fil | p_fil)
    in
      !p := INPUTnone ()
    end // end of [INPUTsome]
  | INPUTnone () => fold@ !p
end // end of [the_atslex_input_fin]

implement the_atslex_input_set (pf_fil | p_fil) = let
  val (vbox pf | p) = ref_get_view_ptr (theInputRef)
in
  case+ !p of
  | INPUTsome (!pf1_fil_r | !p1_fil_r) => let
      val () = $STDIO.fclose1_exn (!pf1_fil_r | !p1_fil_r)
    in
      !pf1_fil_r := pf_fil; !p1_fil_r := p_fil; fold@ !p
    end // end of [INPUTsome]
  | ~INPUTnone () => (!p := INPUTsome (pf_fil | p_fil))
end // end of [the_atslex_input_set]

implement atslex_getchar () = let
  val (vbox pf | p) = ref_get_view_ptr (theInputRef)
in
  case+ !p of
  | INPUTsome (!pf_fil_r | p_fil) => let
      prval pf_fil = !pf_fil_r
      val c = $STDIO.fgetc1_err (file_mode_lte_r_r | !p_fil)
      prval () = !pf_fil_r := pf_fil
    in
      fold@ !p; c
    end // end of [INPUTsome]
  | INPUTnone () => (fold@ !p; ~1 (*EOF*))
end // end of [atslex_getchar]

end // end of [local]

(* ****** ****** *)

fn atslex_usage (cmd: string): void = begin
  printf ("usage: %s --input <filename> --output <filename>\n", @(cmd));
end // end of [atslex_usage]

(* ****** ****** *)

implement main (argc, argv) = let

val () = let
  val (pf_stdin | p_stdin) = stdin_get ()
in
  the_atslex_input_set (pf_stdin | p_stdin)
end // end of [val]

val () = token_initialization ()

// val () = prerr ("atslex: [lexer_parse] is started.\n")

val lexer = lexer_parse ()
val () = the_atslex_input_fin () // close the input channel

// val () = prerr ("atslex: [lexer_parse] is finished.\n")

val (pf_stdout | ptr_stdout) = stdout_get ()

val () = fprint_string (
  file_mode_lte_w_w | !ptr_stdout, lexer.preamble
) // end of [fprint_string]

// val () = prerr ("atslex: preamble is finished.\n")

val () = fprint_lexfns (
  file_mode_lte_w_w | !ptr_stdout, lexer.redef, lexer.lexfns
) // end of [fprint_lexfns]

// val () = prerr ("atslex: [fprint_lexfns] is finished.\n")

val () = fprint_string (
  file_mode_lte_w_w | !ptr_stdout, lexer.postamble
) // end of [fprint_string]

// val () = prerr ("atslex: postamble is finished.\n")

val () = stdout_view_set (pf_stdout | (*none*))

in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [atslex.dats] *)
