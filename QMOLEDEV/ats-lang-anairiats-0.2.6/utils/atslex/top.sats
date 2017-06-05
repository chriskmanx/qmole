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
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: July 2007
//
(* ****** ****** *)

abstype pos_t (* defined in position.dats *)
abstype charset_t (* defined in [charset.dats] *)
abstype intset_t (* defined in [intset.dats] *)
absviewtype states_t (* defined in [states.dats] *)

datatype
token = // type for tokens
  | TOKchar of char
  | TOKcode of string
  | TOKint of int
  | TOKstring of string
  | TOKword of string
  | TOKlit of char
  | TOKmark of string
  | TOKeof
// end of [token]

datatype regex = // type for regular expressions
  | REGalt of (regex, regex)
  | REGchars of charset_t
  | REGid of string
  | REGnil
  | REGopt of regex
  | REGplus of regex
  | REGrep of (regex, int)
  | REGseq of (regex, regex)
  | REGstar of regex
  | REGstr of string
// end if [regex]

datatype redef =
  | redef_nil | redef_cons of (string (* identifier *), regex, redef)
// end of [redef]

datatype rules =
  | rules_nil | rules_cons of (regex, string (* code for action *), rules)
// end of [rules]

datatype lexfns =
  | lexfns_nil | lexfns_cons of (string (*name*), string (*arg*), rules, lexfns)
// end if [lexfns]

typedef lexer = '{
  preamble= string, redef= redef, lexfns= lexfns, postamble= string
} // end of [lexer]

(* ****** ****** *)

fun the_atslex_input_fin (): void
fun the_atslex_input_set {l:addr} (pf: FILE r @ l | p: ptr l): void

fun atslex_getchar (): int = "atslex_getchar"

(* ****** ****** *)
//
// HX: implemented in [token.dats]
//
fun pos_get_line (): int = "pos_get_line"
fun pos_get_char (): int = "pos_get_char"
fun pos_get_line_prev (): int = "pos_get_line_prev"
fun pos_get_char_prev (): int = "pos_get_char_prev"

fun token_get (): token = "token_get"
fun token_update (): void = "token_update"
fun token_get_update (): token = "token_get_update"

fun tokenize_line_comment (): void
fun tokenize_rest_text (): string
fun tokenize_logue (): string
fun tokenize_funarg (): string

fun print_token (tok: token): void = "print_token"
fun prerr_token (tok: token): void = "prerr_token"

fun token_initialization (): void

(* ****** ****** *)
//
// HX: implemented in [position.dats]
//
fun position_get (): pos_t = "position_get"
fun position_prev_get (): pos_t = "position_prev_get"

fun print_pos (p: pos_t): void = "print_pos"
fun prerr_pos (p: pos_t): void = "prerr_pos"

(* ****** ****** *)
//
// HX: implemented in [charset.sats]
//
val char_max: char

fun add_char_int (c: char, i: int): char
fun sub_char_int (c: char, i: int): char

overload + with add_char_int
overload - with sub_char_int

val charset_all: charset_t // the full charset
val charset_nil: charset_t // the empty charset
val charset_eof: charset_t

fun charset_interval (c1: char, c2: char): charset_t
fun charset_singleton (c: char): charset_t

fun charset_complement (cs: charset_t): charset_t
fun charset_difference (cs1: charset_t, cs2: charset_t): charset_t
fun charset_intersect (cs1: charset_t, cs2: charset_t): charset_t
fun charset_union (cs1: charset_t, cs2: charset_t): charset_t
fun charset_is_member (cs: charset_t, c: char): bool

fun fprint_charset {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, cs: charset_t): void =
  "fprint_charset"

fun print_charset (cs: charset_t): void
fun prerr_charset (cs: charset_t): void

(* ****** ****** *)
//
// HX: implemented in [parser.dats]
//
fun print_regex (reg: regex): void
fun prerr_regex (reg: regex): void

fun lexer_parse (): lexer

(* ****** ****** *)
//
// HX: implemented in [intset.dats]
//
val intset_nil : intset_t
fun intset_is_nil (ns: intset_t): bool

fun intset_singleton (n: int): intset_t

fun eq_intset_intset (ns1: intset_t, ns2: intset_t): bool
overload = with eq_intset_intset

fun compare_intset_intset (ns1: intset_t, ns2: intset_t): Sgn
overload compare with compare_intset_intset

fun union_intset_intset (ns1: intset_t, ns2: intset_t): intset_t
overload + with union_intset_intset

fun fprint_intset {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, ns: intset_t): void =
  "fprint_intset"

fun print_intset (cs: intset_t): void
fun prerr_intset (cs: intset_t): void

fun foreach_intset {v:view}
  (pf: !v | f: !(!v | int) -<cloptr1> void, ns: intset_t): void
// end of [foreach_intset]

(* ****** ****** *)
//
// HX: implemented in [states.dats]
//
fun states_nil (): states_t

fun states_free (sts: states_t): void

fun states_find (sts: !states_t, ns0: intset_t): int
fun states_insert (sts: &states_t, tag0: int, ns0: intset_t): void

fun states_foreach_and_free {v:view}
  (pf: !v | f: !(!v | int, intset_t) -<cloptr1> void, sts: states_t): void
// end of [states_foreach_and_free]

(* ****** ****** *)
//
// HX: implemented in [lexgen.dats]
//
fun fprint_lexfns {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, rds: redef, lfs: lexfns): void
// end of [fprint_lexfns]

(* ****** ****** *)

(* end of top.sats *)
