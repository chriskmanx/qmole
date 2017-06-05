(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January, 2010
//
(* ****** ****** *)
//
// API for SDL in ATS
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload "contrib/SDL/SATS/SDL.sats"

(* ****** ****** *)

implement
SDL_SetVideoMode_exn
  (width, height, bpp, flags) = let
  val (pf | sf) = SDL_SetVideoMode (width, height, bpp, flags)
in
  if ref_is_null (sf) then let
    prval () = Video_v_unnull (pf)
    val _null = ref_free_null (sf)
    val () = prerr ("exit(ATS/SDL): [SDL_SetVideoMode] failed.\n")
  in
    exit (1)
  end else (pf | sf) // end of [if]
end // end of [SDL_SetVideoMode_exn]

(* ****** ****** *)

implement
SDL_LoadBMP_exn (filename) = let
  val sf = SDL_LoadBMP (filename) // [sfopt] may be null
in
  if ref_is_null (sf) then let
    val _null = ref_free_null sf
    val () = prerrf (
      "exit(ATS/SDL): [SDL_LoadBMP(%s)] failed.\n", @(filename)
    ) // end of [val]
  in
    exit (1)
  end else sf // end of [if]
end // end of [SDL_LoadBMP_exn]

(* ****** ****** *)

(* end of [SDL.dats] *)
