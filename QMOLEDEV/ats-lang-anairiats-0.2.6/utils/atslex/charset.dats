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
 * along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
 * Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
 * 02110-1301, USA.
 *
 *)

(* ****** ****** *)

// July 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

datatype chars =
  | chars_nil | chars_cons of (char, char, chars)

assume charset_t = chars

//

implement fprint_charset
  {m:file_mode} (pf_mod | fil, cs): void = let
  fun loop (fil: &FILE m, cs: chars): void = begin case+ cs of
    | chars_cons (c1, c2, cs) => begin
        if c1 = c2 then begin
          fprintf (pf_mod | fil, " '%c'", @(c1)); loop (fil, cs)
        end else begin
          fprintf (pf_mod | fil, " '%c'-'%c'", @(c1, c2)); loop (fil, cs)
        end
      end // end of [chars_cons]
    | chars_nil () => ()
  end // end of [loop]
in
  case+ cs of
  | chars_cons (c1, c2, cs) => begin
      if c1 = c2 then begin
        fprintf (pf_mod | fil, "[ '%c'", @(c1));
        loop (fil, cs);
        fprint_string (pf_mod | fil, " ]")
      end else begin
        fprintf (pf_mod | fil, "[ '%c'-'%c'", @(c1, c2));
        loop (fil, cs);
        fprint_string (pf_mod | fil, " ]")
      end // end of [if]
    end // end of [chars_cons]
  | chars_nil () => begin
      fprint_string (pf_mod | fil, "[]")
    end // end of [chars_nil]
end // end of [fprint_charset]

implement print_charset (cs) = print_mac (fprint_charset, cs)
implement prerr_charset (cs) = prerr_mac (fprint_charset, cs)

(* ****** ****** *)

#define CHAR_NUL '\000'
#define CHAR_MAX '\177'
#define CHAR_EOF '\377'

implement charset_nil = chars_nil ()
implement charset_all = chars_cons (CHAR_NUL, CHAR_MAX, chars_nil ())

// every char satisfying c <= -1 is treated as CHAR_EOF
implement charset_eof = chars_cons (CHAR_EOF, CHAR_EOF, chars_nil ())

(* ****** ****** *)

implement charset_interval (c1, c2) = begin
  if c1 <= c2 then
    chars_cons (c1, c2, chars_nil ())
  else
    chars_cons (c2, c1, chars_nil ())
  // end of [if]
end // end of [charset_interval]

implement charset_singleton c = chars_cons (c, c, chars_nil ())

//

implement charset_complement (cs) =
  charset_difference (charset_all, cs)

//

implement charset_is_member
  (cs, c0) = loop (cs, c0) where {
  fun loop (cs: chars, c0: char): bool = begin
    case+ cs of
    | chars_cons (c1, c2, cs) => begin
        if c1 <= c0 then
          (if c0 <= c2 then true else loop (cs, c0))
        else loop (cs, c0)
      end // end of [chars_cons]
    | chars_nil () => false
  end // end of [loop]
} // end of [charset_is_member]

//

implement charset_union (cs1, cs2) = let
  fun loop (cs1: chars, cs2: chars)
    : chars = begin case+ (cs1, cs2) of
    | (chars_cons (c11, c12, cs10),
       chars_cons (c21, c22, cs20)) => begin
        if c21 < c11 then begin
          loop (cs2, cs1)
        end else if 1 < c21 - c12 then begin
          chars_cons (c11, c12, loop (cs10, cs2))
        end else if c12 <= c22 then begin
          loop (chars_cons (c11, c22, cs20), cs10)
        end else begin
          loop (cs1, cs20)
        end
      end // end of [chars_cons _, chars_cons _]
    | (chars_nil (), _) => cs2
    | (_, chars_nil ()) => cs1
  end // end of [loop]
in
  loop (cs1, cs2)
end // end of [charset_union]

//

implement charset_intersect (cs1, cs2) = let
  fun loop (cs1: chars, cs2: chars)
    : chars = begin case+ (cs1, cs2) of
    | (chars_cons (c11, c12, cs10),
       chars_cons (c21, c22, cs20)) => begin
        if c21 < c11 then begin
          loop (cs2, cs1)
        end else if c12 < c21 then begin
          loop (cs10, cs2)
        end else if c12 <= c22 then begin
          chars_cons (c21, c12, loop (cs10, cs2))
        end else begin
          chars_cons (c21, c22, loop (cs1, cs20))
        end
      end // end of [chars_cons]
    | (chars_nil (), _) => chars_nil ()
    | (_, chars_nil ()) => chars_nil ()
  end // end of [loop]
in
  loop (cs1, cs2)
end // end of [charset_intersect]

//

implement add_char_int (c, i) = char_of_int (int_of_char c + i)
implement sub_char_int (c, i) = char_of_int (int_of_char c - i)

//

implement charset_difference (cs1, cs2) = let
  fun loop (cs1: chars, cs2: chars)
    : chars = begin case+ (cs1, cs2) of
    | (chars_cons (c11, c12, cs10),
       chars_cons (c21, c22, cs20)) => begin
        if c12 < c21 then begin
          loop (cs10, cs2)
        end else if c22 < c11 then begin
          loop (cs1, cs20)
        end else if c12 <= c22 then begin
          if c11 < c21 then begin
            chars_cons (c11, c21-1, loop (cs10, cs2))
          end else begin
            loop (cs10, cs2)
          end // end of [if]
        end else begin // c22 < c12
          if c11 < c21 then begin chars_cons
            (c11, c21-1, loop (chars_cons (c22+1, c12, cs10), cs20))
          end else begin
            loop (chars_cons (c22+1, c12, cs10), cs20)
          end // end of [if]
        end
      end // end of [chars_cons _, chars_cons _]
    | (chars_nil (), _) => chars_nil ()
    | (_, chars_nil ()) => cs1
  end // end of [loop]
in
   loop (cs1, cs2)
end // end of [charset_difference]

(* ****** ****** *)

(* end of [charset.dats] *)
