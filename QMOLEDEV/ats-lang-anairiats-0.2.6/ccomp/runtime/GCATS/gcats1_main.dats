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

// mainly for the purpose of debugging

(* ****** ****** *)

#include "gcats1.hats"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

implement main (argc, argv) = let
  val () = gc_init () where {
    extern fun gc_init (): void = "gc_init"
  }
// (*
  // val () = gc_markroot_bsz (null + 1024, 401)
  var ofs: int = 0
  val _ = gc_ptr_is_valid (null + 1024 * 1024 * 1024, ofs)
// *)

// (*
  val ptr1 = gc_aut_malloc_bsz (1)
  val () = begin
    prerr "ptr1 = "; prerr ptr1; prerr_newline ()
  end
  val ptr2 = gc_aut_malloc_bsz (2)
  val () = begin
    prerr "ptr2 = "; prerr ptr2; prerr_newline ()
  end
(*
  val () = gc_aut_free (ptr1)
  val () = gc_aut_free (ptr2)

  val _ = gc_aut_malloc_bsz (10)
  val _ = gc_aut_malloc_bsz (16)
  val _ = gc_aut_malloc_bsz (16)
  val _ = gc_aut_malloc_bsz (16)
  val _ = gc_aut_malloc_bsz (16)
  val _ = gc_aut_malloc_bsz (32)
*)
  val () = gc_collect () where {
    extern fun gc_collect (): void = "gc_collect"
  }
// *)

in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [gc_main.dats] *)
