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
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
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
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: Char (http://www.standardml.org/Basis/char.html)
//

(* ****** ****** *)

staload "libats/smlbas/SATS/general.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/char.sats"

(* ****** ****** *)

implement minChar = '\000'
implement maxChar = char_of_int (UCHAR_MAX)
implement maxOrd = uint_of_int (UCHAR_MAX)

(* ****** ****** *)

implement ord (c) = uint_of_char c

implement chr (i) =
  if i <= maxOrd then char_of_uint (i) else $raise Chr ()
// end of [chr]

(* ****** ****** *)

implement succ (c) = let val u = ord (c) in
  if u < maxOrd then char_of_uint (succ_uint u) else $raise Chr ()
end // end of [succ]

implement pred (c) = let val u = ord (c) in
  if u > 0U(*minOrd*) then char_of_uint (pred_uint u ) else $raise Chr ()
end // end of [pred]

(* ****** ****** *)

implement lt (c1, c2) = lt_char_char (c1, c2)
implement lte (c1, c2) = lte_char_char (c1, c2)

implement gt (c1, c2) = gt_char_char (c1, c2)
implement gte (c1, c2) = gte_char_char (c1, c2)

implement compare (c1, c2) = compare_char_char (c1, c2)

(* ****** ****** *)

implement contains (s, c) = string_contains (s, c)
implement notContains (s, c) = ~string_contains (s, c)

(* ****** ****** *)

implement isAscii (c) = char_isascii (c)
implement isAlpha (c) = char_isalpha (c)
implement isAlphaNum (c) = char_isalnum (c)
implement isCntrl (c) = char_iscntrl (c)
implement isDigit (c) = char_isdigit (c)
implement isGraph (c) = char_isgraph (c)
implement isHexDigit (c) = char_isxdigit (c)
implement isLower (c) = char_islower (c)
implement isPrint (c) = char_isprint (c)
implement isSpace (c) = char_isspace (c)
implement isPunct (c) = char_ispunct (c)
implement isUpper (c) = char_isupper (c)

(* ****** ****** *)

implement toLower (c) = char_tolower (c)
implement toUpper (c) = char_toupper (c)

(* ****** ****** *)

#define p2s string_of_strptr
implement toCString (c) = case+ c of
  | '\\' => "\\\\"
  | '\"' => "\\\""
  | '?' => "\\?"
  | '\'' => "\\'"
  | '\a' => "\\a"
  | '\b' => "\\b"
  | '\t' => "\\t"
  | '\n' => "\\n"
  | '\v' => "\\v"
  | '\f' => "\\f"
  | '\r' => "\\r"
  | _ when char_isprint c => let
      val sbp = string_make_char (1, c) in string1_of_strbuf sbp
    end // end of [_ when ...]
  | _ => let
      val u = ord c
      val u3 = u land 0x7U
      val u = u >> 3
      val u2 = u land 0x7U
      val u = u >> 3
      val u1 = u land 0x7U
    in
      p2s (sprintf ("\\%o%o%o", @(u1,u2,u3)))
    end // end of [_]
// end of [toCString]

(* ****** ****** *)

(* end of [char.dats] *)
