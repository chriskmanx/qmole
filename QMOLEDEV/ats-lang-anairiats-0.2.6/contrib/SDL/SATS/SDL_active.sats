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

macdef SDL_APPMOUSEFOCUS =
  $extval (Uint8, "SDL_APPMOUSEFOCUS") // The app has mouse coverage
macdef SDL_APPINPUTFOCUS =
  $extval (Uint8, "SDL_APPINPUTFOCUS") // The app has input focus
macdef SDL_APPACTIVE =
  $extval (Uint8, "SDL_APPACTIVE") // The application is active

(* ****** ****** *)

fun SDL_GetAppState (): Uint8 = "mac#atsctrb_SDL_GetAppState"

(* ****** ****** *)

(* end of [SDL_active.sats] *)
