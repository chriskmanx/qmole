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

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010

(* ****** ****** *)

%{#
#include "libc/CATS/sched.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for staload at run-time

(* ****** ****** *)

staload
TYPES = "libc/sys/SATS/types.sats"
typedef pid_t = $TYPES.pid_t

(* ****** ****** *)

typedef sched_param_struct =
$extype_struct "ats_sched_param_type" of {
  sched_priority= int
, _rest = undefined_t // for unknown fields
} // end of [sched_param]
typedef sched_param = sched_param_struct

(* ****** ****** *)

//
// HX-2010-04-02:
// [n] is the size of [cpu_set_t] in byte!
//

abst@ype
cpu_set0_t = $extype"cpu_set_t"
abst@ype cpu_set_t (n:int) = cpu_set0_t

(* ****** ****** *)

prfun cpusetinit
  (x: !cpu_set0_t? >> cpu_set_t n): #[n:nat] void
fun cpusetsize_get
  {n:nat} (cs: &cpu_set_t n):<> size_t n = "atslib_cpusetsize_get"
// end of [cpusetsize_get]

(* ****** ****** *)

//
// HX: pid=0: myself
//

fun sched_getaffinity {n:nat} (
  pid: pid_t, n: size_t n, cs: &cpu_set_t n
) : int(*err*)
  = "atslib_sched_getaffinity"
// end of [fun]

fun sched_setaffinity {n:nat} (
  pid: pid_t, n: size_t n, cs: &cpu_set_t n
) : int(*err*)
  = "atslib_sched_setaffinity"
// end of [fun]

(* ****** ****** *)

fun CPU_ZERO {n:nat} (cpuset: &cpu_set_t n):<> void
  = "mac#atslib_CPU_ZERO"

fun CPU_CLR {n,i:nat | i < 8*n} (cpu: int i, cpuset: &cpu_set_t n):<> void
  = "mac#atslib_CPU_CLR"

fun CPU_SET {n,i:nat | i < 8*n} (cpu: int i, cpuset: &cpu_set_t n):<> void
  = "mac#atslib_CPU_SET"

fun CPU_ISSET {n,i:nat | i < 8*n} (cpu: int i, cpuset: &cpu_set_t n):<> int
  = "mac#atslib_CPU_ISSET"

(* ****** ****** *)

(* end of [sched.sats] *)
