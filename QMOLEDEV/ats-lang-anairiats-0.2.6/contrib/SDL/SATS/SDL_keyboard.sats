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

#include "contrib/SDL/SATS/SDL_keysym.sats"

(* ****** ****** *)

typedef SDL_keysym =
  $extype_struct "SDL_keysym" of {
  scancode= Uint8, sym= SDLKey, mod= SDLMod, unicode= Uint16
} // end of [SDL_keysym]

(* ****** ****** *)

#define SDL_ALL_HOTKEYS 0xFFFFFFFF

(* ****** ****** *)

fun SDL_EnableUNICODE (enable: int): int = "mac#atsctrb_SDL_EnableUNICODE"

(* ****** ****** *)

#define SDL_DEFAULT_REPEAT_DELAY 500
#define SDL_DEFAULT_REPEAT_INTERVAL 30

fun SDL_EnableKeyRepeat
  (delay: int, interval: int): int = "mac#atsctrb_SDL_EnableKeyRepeat"
// end of [SDL_EnableKeyRepeat]

fun SDL_GetKeyRepeat (
  delay: &int? >> int, interval: &int? >> int
) : void
  = "mac#atsctrb_SDL_GetKeyRepeat"
// end of [fun]

(* ****** ****** *)

abstype SDL_KeyStateArr (int)
typedef SDL_KeyStateArr = [n:nat] SDL_KeyStateArr n

fun SDL_GetKeyState
  (numkeys: &int? >> int n): #[n:nat] SDL_KeyStateArr (n)
  = "mac#atsctrb_SDL_GetKeyState"

fun SDL_GetKeyState_null (): [n:nat] SDL_KeyStateArr (n)
  = "atsctrb_SDL_GetKeyState_null" // this is a function!

fun SDL_KeyStateArr_get {n:nat}
  (keyarr: SDL_KeyStateArr n, key: SDLKey):<> int
  = "atsctrb_SDL_KeyStateArr_get" // this is a function!
overload [] with SDL_KeyStateArr_get

(* ****** ****** *)

fun SDL_GetModState (): SDLMod = "mac#atsctrb_SDL_GetModState"
fun SDL_SetModState (state: SDLMod): void = "mac#atsctrb_SDL_SetModState"

(* ****** ****** *)

fun SDL_GetKeyName (key: SDLKey): string = "mac#atsctrb_SDL_GetKeyName"

(* ****** ****** *)

(* end of [SDL_keyboard.sats] *)
