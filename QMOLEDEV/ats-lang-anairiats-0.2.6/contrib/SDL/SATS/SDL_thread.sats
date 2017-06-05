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

absviewtype SDL_Thread_ptr // SDL_Thread*

(* ****** ****** *)

fun SDL_ThreadID ():<> Uint32 = "mac#atsctrb_SDL_ThreadID"

(* ****** ****** *)

fun SDL_CreateThread
  {vt:viewtype} (
  threadfun: (vt) -<fun> int, env: vt
) : SDL_Thread_ptr
  = "mac#atsctrb_SDL_CreateThread"
// end of [fun]

fun SDL_KillThread
  (thread: SDL_Thread_ptr):<> void = "mac#atsctrb_SDL_KillThread"
// end of [fun]

fun SDL_WaitThread (
  thread: SDL_Thread_ptr, status: &int? >> int
) :<> void
  = "mac#atsctrb_SDL_WaitThread"
// end of [fun]

(* ****** ****** *)

(* end of [SDL_thread.sats] *)
