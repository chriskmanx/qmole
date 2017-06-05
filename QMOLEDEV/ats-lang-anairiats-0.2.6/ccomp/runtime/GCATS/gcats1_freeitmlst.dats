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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: June 2008
//
(* ****** ****** *)

#include "gcats1.hats"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

#define ATSOPT_NAMESPACE "gcats1_freeitmlst_"

(* ****** ****** *)

implement freeitmlst_length (itms) = let
  fun loop {i,j:nat}
    (itms: freeitmlst i, j: int j): int (i+j) =
    if freeitmlst_is_nil itms then j
    else begin
      loop (freeitmlst_tail_get itms, j+1)
    end
in
  loop (itms, 0)
end // end of [freeitmlst_length]

(* ****** ****** *)

implement the_freeitmlst_array_clear_all () = let
  #define N FREEITMLST_ARRAYSIZE
  fun loop {i:nat | i <= N} .<N-i>. (i: int i): void =
    if i < N then begin
      the_freeitmlst_array_clear_one (i); loop (i+1)
    end
in
  loop (0)
end // end of [the_freeitmlst_array_clear_all]

(* ****** ****** *)

fun freeitmlst_mark_unset (itms: freeitmlst0): void = begin
  if freeitmlst_is_cons (itms) then let
    var ofs: int = 0; val ptr = freeitmlst2ptr (itms)
    val chks = gc_ptr_is_valid (ptr, ofs); val chks = (
      if chunklst_is_cons chks then chks else begin
        prerr "freeitmlst_mark_unset: illegal pointer: ptr = ";
        prerr ptr;
        prerr_newline ();
        exit {chunklst1} (1)
      end
    ) : chunklst1
    val markbits = chunklst_markbits_get (chks)
    val () = // could this really happen?
      if MARK_GET (markbits, ofs) > 0 then begin
        // this could happen only if data is mistreated as a pointer!!!
        MARK_CLEAR (markbits, ofs); chunklst_markcnt_dec (chks)
      end
  in
    freeitmlst_mark_unset (freeitmlst_tail_get itms)
  end
end // end of [freeitmlst_mark_unset]

//

implement the_freeitmlst_array_mark_unset () = let
  #define N FREEITMLST_ARRAYSIZE
  fun loop {i:nat | i <= N} .<N-i>. (i: int i): void =
    if i < N then begin
      freeitmlst_mark_unset (the_freeitmlst_array_get i); loop (i+1)
    end // end of [if]
in
  loop (0)
end // end of [the_freeitmlst_array_mark_unset]

(* ****** ****** *)

(* end of [gcats1_freeitmlst.dats] *)
