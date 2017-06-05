(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
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
(*
**
** An implementation of parsing combinators
**
*)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: December, 2008
//
(* ****** ****** *)

fun{t:t@ype}
stream_get_item (tks: &stream t):<> Option_vt (t)

(* ****** ****** *)

abstype parser_t (a:t@ype, t:t@ype)

(* ****** ****** *)

fun lzeta {a:t@ype}{t:t@ype}
  (p: lazy (parser_t (a, t))):<> parser_t (a, t)

(* ****** ****** *)

fun{a:t@ype} any_parser (): parser_t (a, a)
fun{a:t@ype} anyopt_parser (): parser_t (Option a, a)

(* ****** ****** *)

fun{} fail_parser {a:t@ype}{t:t@ype} (): parser_t (a, t)

(* ****** ****** *)
//
// HX: this one does not consume any tokens
//
fun{a:t@ype}{t:t@ype}
neg_parser (p: parser_t (a, t)): parser_t (unit, t)
overload ~ with neg_parser

(* ****** ****** *)

symintr return
fun{a:t@ype}{t:t@ype}
return_parser (x: a):<> parser_t (a, t)
overload return with return_parser

(* ****** ****** *)

// symintr alt // infix
fun{a:t@ype}{t:t@ype}
alt_parser_parser
  (p1: parser_t (a, t), p2: parser_t (a, t)):<> parser_t (a, t)
overload || with alt_parser_parser

(* ****** ****** *)

// symintr seq // infix
fun{a1,a2:t@ype}{t:t@ype}
seq_parser_parser (
  p1: parser_t (a1, t), p2: parser_t (a2, t)
) :<> parser_t (@(a1,a2), t)
overload && with seq_parser_parser

(* ****** ****** *)

symintr wth // infix

fun{a:t@ype;b:t@ype}{t:t@ype}
seq1wth_parser_fun (
  p1: parser_t (a, t), f: (a) -<fun> b
) :<> parser_t (b, t)
overload wth with seq1wth_parser_fun

fun{a:t@ype;b:t@ype}{t:t@ype}
seq1wth_parser_cloref (
  p1: parser_t (a, t), f: (a) -<cloref> b
) :<> parser_t (b, t)
overload wth with seq1wth_parser_cloref

(* ****** ****** *)

fun{a1,a2,b:t@ype}{t:t@ype}
seq2wth_parser_fun (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, f: (a1, a2) -<fun> b
) :<> parser_t (b, t)
// end of [seq2wth_parser_fun]

fun{a1,a2,b:t@ype}{t:t@ype}
seq2wth_parser_cloref (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, f: (a1, a2) -<cloref> b
) :<> parser_t (b, t)
// end of [seq2wth_parser_cloref]

(* ****** ****** *)

fun{a1,a2,a3,b:t@ype}{t:t@ype}
seq3wth_parser_fun (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, f: (a1, a2, a3) -<fun> b
) :<> parser_t (b, t)
// end of [seq3wth_parser_fun]

fun{a1,a2,a3,b:t@ype}{t:t@ype}
seq3wth_parser_cloref (
    p1: parser_t (a1, t)
  , p2: parser_t (a2, t)
  , p3: parser_t (a3, t)  
  , f: (a1, a2, a3) -<cloref> b
  ) :<> parser_t (b, t)
// end of [seq3wth_parser_cloref]

(* ****** ****** *)

fun{a1,a2,a3,a4,b:t@ype}{t:t@ype}
seq4wth_parser_fun (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, f: (a1, a2, a3, a4) -<fun> b
) :<> parser_t (b, t)
// end of [seq4wth_parser_fun]

fun{a1,a2,a3,a4,b:t@ype}{t:t@ype}
seq4wth_parser_cloref (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, f: (a1, a2, a3, a4) -<cloref> b
) :<> parser_t (b, t)
// end of [seq4wth_parser_cloref]

(* ****** ****** *)

fun{a1,a2,a3,a4,a5,b:t@ype}{t:t@ype}
seq5wth_parser_fun (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, p5: parser_t (a5, t)  
, f: (a1, a2, a3, a4, a5) -<fun> b
) :<> parser_t (b, t)
// end of [seq5wth_parser_fun]

fun{a1,a2,a3,a4,a5,b:t@ype}{t:t@ype}
seq5wth_parser_cloref (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, p5: parser_t (a5, t)  
, f: (a1, a2, a3, a4, a5) -<cloref> b
) :<> parser_t (b, t)
// end of [seq5wth_parser_cloref]

(* ****** ****** *)

fun{a1,a2,a3,a4,a5,a6,b:t@ype}{t:t@ype}
seq6wth_parser_fun (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, p5: parser_t (a5, t)
, p6: parser_t (a6, t)
, f: (a1, a2, a3, a4, a5, a6) -<fun> b
) :<> parser_t (b, t)
// end of [seq6wth_parser_fun]

fun{a1,a2,a3,a4,a5,a6,b:t@ype}{t:t@ype}
seq6wth_parser_cloref (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, p5: parser_t (a5, t)
, p6: parser_t (a6, t)
, f: (a1, a2, a3, a4, a5, a6) -<cloref> b
) :<> parser_t (b, t)
// end of [seq6wth_parser_cloref]

(* ****** ****** *)

fun{a1,a2,a3,a4,a5,a6,a7,b:t@ype}{t:t@ype}
seq7wth_parser_fun (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, p5: parser_t (a5, t)
, p6: parser_t (a6, t)
, p7: parser_t (a7, t)
, f: (a1, a2, a3, a4, a5, a6, a7) -<fun> b
) :<> parser_t (b, t)
// end of [seq7wth_parser_fun]

fun{a1,a2,a3,a4,a5,a6,a7,b:t@ype}{t:t@ype}
seq7wth_parser_cloref (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, p5: parser_t (a5, t)
, p6: parser_t (a6, t)
, p7: parser_t (a7, t)
, f: (a1, a2, a3, a4, a5, a6, a7) -<cloref> b
) :<> parser_t (b, t)
// end of [seq7wth_parser_cloref]

(* ****** ****** *)

fun{a1,a2,a3,a4,a5,a6,a7,a8,b:t@ype}{t:t@ype}
seq8wth_parser_fun (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, p5: parser_t (a5, t)
, p6: parser_t (a6, t)
, p7: parser_t (a7, t)
, p8: parser_t (a8, t)
, f: (a1, a2, a3, a4, a5, a6, a7, a8) -<fun> b
) :<> parser_t (b, t)
// end of [seq8wth_parser_fun]

fun{a1,a2,a3,a4,a5,a6,a7,a8,b:t@ype}{t:t@ype}
seq8wth_parser_cloref (
  p1: parser_t (a1, t)
, p2: parser_t (a2, t)
, p3: parser_t (a3, t)  
, p4: parser_t (a4, t)  
, p5: parser_t (a5, t)
, p6: parser_t (a6, t)
, p7: parser_t (a7, t)
, p8: parser_t (a8, t)
, f: (a1, a2, a3, a4, a5, a6, a7, a8) -<cloref> b
) :<> parser_t (b, t)
// end of [seq8wth_parser_cloref]

(* ****** ****** *)

// << (fst) and >> (snd)

fun{a1,a2:t@ype}{t:t@ype}
proj1_parser_parser
  (p1: parser_t (a1, t), p2: parser_t (a2, t)):<> parser_t (a1, t)
overload << with proj1_parser_parser

fun{a1,a2:t@ype}{t:t@ype}
proj2_parser_parser
  (p1: parser_t (a1, t), p2: parser_t (a2, t)):<> parser_t (a2, t)
overload >> with proj2_parser_parser

(* ****** ****** *)

symintr sat // infix

fun{a:t@ype}{t:t@ype} sat_parser_fun
  (p: parser_t (a, t), f: a -<fun> bool):<> parser_t (a, t)
overload sat with sat_parser_fun

fun{a:t@ype}{t:t@ype} sat_parser_cloref
  (p: parser_t (a, t), f: a -<cloref> bool):<> parser_t (a, t)
overload sat with sat_parser_cloref

(* ****** ****** *)

fun{a:t@ype}{t:t@ype}
discard_one_parser (p: parser_t (a, t)):<> parser_t (unit, t)
fun{a:t@ype}{t:t@ype}
discard_many_parser (p: parser_t (a, t)):<> parser_t (unit, t)

(* ****** ****** *)

symintr ^? ^* ^+ // postfix

fun{a:t@ype}{t:t@ype} optional_parser
  (p: parser_t (a, t)):<> parser_t (Option a, t)
overload ^? with optional_parser

fun{a:t@ype}{t:t@ype} repeat0_parser
  (p: parser_t (a, t)):<> parser_t (List a, t)
overload ^* with repeat0_parser

viewtypedef List1 (a: t@ype) = [n:int | n > 0] list (a, n)

fun{a:t@ype}{t:t@ype} repeat1_parser
  (p: parser_t (a, t)):<> parser_t (List1 a, t)
overload ^+ with repeat1_parser

(* ****** ****** *)

fun{a,b:t@ype}{t:t@ype}
repeat0_sep_parser
  (p: parser_t (a, t), sep: parser_t (b, t)):<> parser_t (List a, t)
// end of [repeat0_sep_parser]

fun{a,b:t@ype}{t:t@ype}
repeat1_sep_parser
  (p: parser_t (a, t), sep: parser_t (b, t)):<> parser_t (List1 a, t)
// end of [repeat1_sep_parser]

(* ****** ****** *)

fun{a:t@ype}{t:t@ype}
apply_parser (
  p: parser_t (a, t), tks: &stream t, ncur: &int, nmax: &int
) :<!laz> Option_vt a // end of [apply_parser]

(* ****** ****** *)

(* end of [parcomb.sats] *)
