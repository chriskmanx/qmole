(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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

%{#
#include "libc/sys/CATS/resource.cats"
%} // end of [%{#]

(* ****** ****** *)

staload TIME = "libc/sys/SATS/time.sats"
typedef timeval = $TIME.timeval

(* ****** ****** *)

typedef rlimit_struct =
$extype_struct "ats_rlimit_type" of {
  rlimit_cur= int // soft limit
, rlimit_max= int // hard limit // ceiling for rlimit_cur
} // end of [rlimit_struct]
typedef rlimit = rlimit_struct

(* ****** ****** *)

abst@ype resource_t = int
macdef RLIMIT_CPU = $extval (resource_t, "RLIMIT_CPU")
macdef RLIMIT_FSIZE = $extval (resource_t, "RLIMIT_FSIZE")
macdef RLIMIT_DATA = $extval (resource_t, "RLIMIT_DATA")
macdef RLIMIT_STACK = $extval (resource_t, "RLIMIT_STACK")
macdef RLIMIT_CORE = $extval (resource_t, "RLIMIT_CORE")
macdef RLIMIT_RSS = $extval (resource_t, "RLIMIT_RSS")
macdef RLIMIT_NOFILE = $extval (resource_t, "RLIMIT_NOFILE")
macdef RLIMIT_AS = $extval (resource_t, "RLIMIT_AS")
macdef RLIMIT_NPROC = $extval (resource_t, "RLIMIT_NPROC")
macdef RLIMIT_MEMLOCK = $extval (resource_t, "RLIMIT_MEMLOCK")
macdef RLIMIT_LOCKS = $extval (resource_t, "RLIMIT_LOCKS")
macdef RLIMIT_SIGPENDING = $extval (resource_t, "RLIMIT_SIGPENDING")
macdef RLIMIT_MSGQUEUE = $extval (resource_t, "RLIMIT_MSGQUEUE")
macdef RLIMIT_NICE = $extval (resource_t, "RLIMIT_NICE")
macdef RLIMIT_RTPRIO = $extval (resource_t, "RLIMIT_RTPRIO")
macdef RLIMIT_NLIMITS = $extval (resource_t, "RLIMIT_NLIMITS")
macdef RLIM_NLIMITS = $extval (resource_t, "RLIM_NLIMITS")

(* ****** ****** *)

macdef RLIM_INFINITY = $extval (ullint, "RLIM_INFINITY")
macdef RLIM_SAVED_CUR = $extval (ullint, "RLIM_SAVED_CUR")
macdef RLIM_SAVED_MAX = $extval (ullint, "RLIM_SAVED_MAX")

(* ****** ****** *)

macdef // minimal priority a process can have
PRIO_MIN = $extval (int, "PRIO_MIN")
macdef // maximal priority a process can have
PRIO_MAX = $extval (int, "PRIO_MAX")

(* ****** ****** *)

fun getrlimit ( // 0/-1 : succ/fail/ errno set
  res: resource_t
, lim: &rlimit? >> opt (rlimit, i==0)
) : #[i:int | i <= 0] int i
  = "mac#atslib_getrlimit"
// end of [getrlimit]

fun setrlimit ( // 0/-1 : succ/fail/ errno set
  res: resource_t
, lim: &rlimit(*const*)
) : int
  = "mac#atslib_setrlimit"
// end of [setrlimit]

(* ****** ****** *)

typedef rusage_struct =
$extype_struct "ats_rusage_struct" of {
  ru_utime= timeval // user time used
, ru_stime= timeval // system time used
(*
, ru_maxrss= lint // maximal resident set size
, ru_ixrss= lint // integral shared memory size
, ru_idrss= lint // integral unshared data size
, ru_isrss= lint // integral unshared stack size
, ru_minflt= lint // page reclaims
, ru_majflt= lint // page faults
, ru_nswap= lint // swaps
, ru_inblock= lint // block input operations
, ru_oublock= lint // block output operations
, ru_msgsnd= lint // message sent
, ru_msgrcv= lint // message received
, ru_nsignals= lint // signals received
, ru_nvcsw= lint // voluntary context switch
, ru_nivcsw= lint // involuntary context switch
*)
, _rest = undefined_t // unknown quantity
} // en dof [rusage_struct]
typedef rusage = rusage_struct

fun getrusage ( // 0/-1 : succ/fail/ errno set
  who: int, rusage: &rusage? >> opt (rusage, i==0)
) : #[i:int | i <= 0] int(i) = "mac#atslib_getrusage"
// end of [getrusage]

(* ****** ****** *)

abst@ype priowhich_t = int
castfn int_of_priowhich (x: priowhich_t):<> int
macdef PRIO_PROCESS = $extval (priowhich_t, "PRIO_PROCESS")
macdef PRIO_PGRP = $extval (priowhich_t, "PRIO_PGRP")
macdef PRIO_USER = $extval (priowhich_t, "PRIO_USER")

(* ****** ****** *)
//
// HX: -1 maybe a legitimate return value for getpriority
//
fun getpriority (
  which: priowhich_t, who: int
) : int = "mac#atslib_getpriority"
// end of [getpriority]
//
// HX: 0/-1 : succ/fail // errno set
//
fun setpriority (
  which: priowhich_t, who: int, prio: int
) : int = "mac#atslib_setpriority"
// end of [setpriority]

(* ****** ****** *)

(* end of [resource.sats] *)
