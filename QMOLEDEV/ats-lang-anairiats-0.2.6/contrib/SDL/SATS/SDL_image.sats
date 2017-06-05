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

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January, 2010

(* ****** ****** *)

%{#
#include "contrib/SDL/CATS/SDL_image.cats"
%} // end of [%{#]

(* ****** ****** *)

staload "contrib/SDL/SATS/SDL.sats"

(* ****** ****** *)

macdef
SDL_IMAGE_MAJOR_VERSION = $extval (int, "SDL_IMAGE_MAJOR_VERSION")
macdef
SDL_IMAGE_MINOR_VERSION = $extval (int, "SDL_IMAGE_MINOR_VERSION")
macdef SDL_IMAGE_PATCHLEVEL = $extval (int, "SDL_IMAGE_PATCHLEVEL")

(* ****** ****** *)
  
fun IMG_Load (filename: string): SDL_Surface_ref0 = "mac#atsctrb_IMG_Load"

(* ****** ****** *)

(* end of [SDL_image.sats] *)
