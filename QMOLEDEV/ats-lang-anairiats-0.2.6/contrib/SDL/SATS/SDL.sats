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

%{#
#include "contrib/SDL/CATS/SDL.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

symintr NULL
symintr Sint8 Uint8
symintr Sint16 Uint16
symintr Sint32 Uint32
symintr Sint64 Uint64

symintr ref_null // castfn
symintr ref_free_null // castfn
symintr ref_is_null ref_isnot_null // fun

(* ****** ****** *)

#include "contrib/SDL/SATS/SDL_stdinc.sats"

(* ****** ****** *)

#include "contrib/SDL/SATS/SDL_events.sats"
#include "contrib/SDL/SATS/SDL_thread.sats"
#include "contrib/SDL/SATS/SDL_timer.sats"
#include "contrib/SDL/SATS/SDL_video.sats"

(* ****** ****** *)

(*
#define	SDL_INIT_TIMER		0x00000001
#define SDL_INIT_AUDIO		0x00000010
#define SDL_INIT_VIDEO		0x00000020
#define SDL_INIT_CDROM		0x00000100
#define SDL_INIT_JOYSTICK	0x00000200
#define SDL_INIT_NOPARACHUTE	0x00100000	/**< Don't catch fatal signals */
#define SDL_INIT_EVENTTHREAD	0x01000000	/**< Not supported on all OS's */
#define SDL_INIT_EVERYTHING	0x0000FFFF
*)

macdef SDL_INIT_TIMER = $extval (Uint32, "SDL_INIT_TIMER")
macdef SDL_INIT_AUDIO = $extval (Uint32, "SDL_INIT_AUDIO")
macdef SDL_INIT_VIDEO = $extval (Uint32, "SDL_INIT_VIDEO")
macdef SDL_INIT_CDROM = $extval (Uint32, "SDL_INIT_CDROM")
macdef SDL_INIT_JOYSTICK = $extval (Uint32, "SDL_INIT_JOYSTICK")
macdef SDL_INIT_NOPARACHUTE = $extval (Uint32, "SDL_INIT_NOPARACHUTE")
macdef SDL_INIT_EVENTTHREAD = $extval (Uint32, "SDL_INIT_EVENTTHREAD")
macdef SDL_INIT_EVERYTHING = $extval (Uint32, "SDL_INIT_EVERYTHING")

(* ****** ****** *)

fun SDL_Init
  (flags: Uint32): int = "mac#atsctrb_SDL_Init"
// end of [fun]

castfn
SDL_Quit_Video
  {l:addr} (
  pf: Video_v l | sf: SDL_Surface_ref l
) : ptr // end of [castfn]

fun SDL_Quit (): void = "mac#atsctrb_SDL_Quit"

(* ****** ****** *)

fun SDL_InitSubSystem
  (flags: Uint32): int = "mac#atsctrb_SDL_InitSubSystem"
fun SDL_QuitSubSystem
  (flags: Uint32): void = "mac#atsctrb_SDL_QuitSubSystem"
fun SDL_WasInit (flags: Uint32): Uint32 = "mac#atsctrb_SDL_WasInit"

(* ****** ****** *)

(* end of [SDL.sats] *)
