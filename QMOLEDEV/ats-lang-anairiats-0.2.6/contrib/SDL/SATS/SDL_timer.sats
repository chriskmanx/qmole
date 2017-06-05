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

(*
** This is the OS scheduler timeslice, in milliseconds
*)
macdef SDL_TIMESLICE = 10

(*
** This is the maximum resolution of the SDL timer on all platforms
*)
macdef TIMER_RESOLUTION = 10 (* Experimentally determined *)

fun SDL_GetTicks (): Uint32 = "mac#atsctrb_SDL_GetTicks"

(* ****** ****** *)

fun SDL_Delay (ms: Uint32): void = "mac#atsctrb_SDL_Delay"

(* ****** ****** *)

typedef SDL_TimerCallback = (Uint32(*interval*)) -<fun1> Uint32

fun SDL_SetTimer (
  interval: Uint32, callback: SDL_TimerCallback
) : int
  = "mac#atsctrb_SDL_SetTimer"
// end of [fun]

//
// SDL_CancelTimer () = SDL_SetTimer (0, NULL)
//
fun SDL_CancelTimer
  (): int // cancel the current running timer
  = "mac#atsctrb_SDL_CancelTimer"

(* ****** ****** *)

(* end of [SDL_timer.sats] *)
