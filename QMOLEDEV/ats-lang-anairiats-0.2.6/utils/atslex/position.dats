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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: July 2007
//
(* ****** ****** *)

(* functions for manipulating positions in files *)

//

staload "top.sats"

//

(* ****** ****** *)

typedef pos = '{line= int, char= int}
assume pos_t = pos

(* ****** ****** *)

fun fprint_pos {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, p: pos): void =
  fprintf (pf_mod | fil, "%i.%i", @(p.line, p.char))

implement print_pos (p) = print_mac (fprint_pos, p)
implement prerr_pos (p) = prerr_mac (fprint_pos, p)

(* ****** ****** *)

fn lt_pos_pos (p1: pos, p2: pos): bool =
  if p1.line < p2.line then true
  else if p1.line <= p2.line then p1.char < p2.char
  else false
// end of [lt_pos_pos]

fn lte_pos_pos (p1: pos, p2: pos): bool =
  if p1.line < p2.line then true
  else if p1.line <= p2.line then p1.char <= p2.char
  else false
// end of [lte_pos_pos]

overload < with lt_pos_pos
overload <= with lte_pos_pos

(* ****** ****** *)

fun min_pos_pos (p1: pos, p2: pos): pos =
  if p1 <= p2 then p1 else p2

fun max_pos_pos (p1: pos, p2: pos): pos =
  if p1 <= p2 then p2 else p1

overload min with min_pos_pos
overload max with max_pos_pos

(* ****** ****** *)

implement position_get () = let
  // [pos_line_get] is implemented in [token.dats]
  val line = pos_get_line ()
  // [pos_char_get] is implemented in [token.dats]
  val char = pos_get_char ()
in
  '{line= line, char= char }
end // end of [position_get]

implement position_prev_get () = let
  // [pos_line_prev_get] is implemented in [token.dats]
  val line = pos_get_line_prev ()
  // [pos_char_prev_get] is implemented in [token.dats]
  val char = pos_get_char_prev ()
in
  '{line= line, char= char }
end // end of [position_prev_get]

(* ****** ****** *)

(* end of [position.dats] *)
