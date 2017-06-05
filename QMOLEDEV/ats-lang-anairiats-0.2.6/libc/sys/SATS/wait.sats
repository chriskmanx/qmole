(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)  *)

(* ****** ****** *)

staload "libc/sys/SATS/types.sats"

(* ****** ****** *)

%{#
#include "libc/sys/CATS/wait.cats"
%} // end of [%{#]

(* ****** ****** *)

absprop WIFEXITED_p (s:int, b:bool)

fun WIFEXITED {s:int}
  (status: int s): [b:bool] (WIFEXITED_p (s, b) | bool b)
  = "atslib_WIFEXITED"
// end of [WIFEXITED]

fun WEXITSTATUS {s:int}
  (pf: WIFEXITED_p (s, true) | status: int s): int = "atslib_WEXITSTATUS"
// end of [WEXITSTATUS]

(* ****** ****** *)

absprop WIFSIGNALED_p (s:int, b:bool)

fun WIFSIGNALED {s:int}
  (status: int s): [b:bool] (WIFSIGNALED_p (s, b) | bool b)
  = "atslib_WIFSIGNALED"
// end of [WIFSIGNALED]

fun WTERMSIG {s:int}
  (pf: WIFSIGNALED_p (s, true) | status: int s): int = "atslib_WTERMSIG"
// end of [WTERMSIG]

(* ****** ****** *)

absprop WIFSTOPPED_p (s:int, b:bool)

fun WIFSTOPPED {s:int}
  (status: int s): [b:bool] (WIFSTOPPED_p (s, b) | bool b)
  = "atslib_WIFSTOPPED"
// end of [WIFSTOPPED]

fun WSTOPSIG {s:int}
  (pf: WIFSTOPPED_p (s, true) | status: int s): int
  = "atslib_WSTOPSIG"
// end of [WSTOPSIG]

(* ****** ****** *)

absprop WIFCONTINUED_p (s:int, b:bool)

fun WIFCONTINUED {s:int}
  (status: int s): [b:bool] (WIFCONTINUED_p (s, b) | bool b)
  = "atslib_WIFCONTINUED"
// end of [WIFCONTINUED]

(* ****** ****** *)

absprop WCOREDUMP_p (s:int, b:bool)

fun WCOREDUMP {s:int}
  (status: int s): [b:bool] (WCOREDUMP_p (s, b) | bool b)
// end of [WCOREDUMP]

(* ****** ****** *)

fun wait_null (): pid_t = "atslib_wait_null"
fun wait (status: &int? >> int): pid_t = "mac#atslib_wait"

(* ****** ****** *)

abst@ype waitopt_t = $extype"ats_int_type"
macdef WNOHANG = $extval (waitopt_t, "WNOHANG")
macdef WUNTRACED = $extval (waitopt_t, "WUNTRACED")
macdef WCONTINUED = $extval (waitopt_t, "WCONTINUED")
macdef WNONE = $extval (waitopt_t, "0") // default value for [waitopt_t]

fun lor_waitopt_waitopt
  (opt1: waitopt_t, opt2: waitopt_t): waitopt_t
overload lor with lor_waitopt_waitopt

(* ****** ****** *)

fun waitpid (
    chldpid: pid_t, status: &int? >> int, options: waitopt_t
  ) : pid_t = "mac#atslib_waitpid" // macro!
// end of [waitpid]

(* ****** ****** *)

(* end of [wait.sats] *)
