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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

//
// some history:
//
// Rui Shi and Hongwei Xi first did [pthread] in ATS/Proto, on which
// this version is primarily based.
//

(* ****** ****** *)

%{#
#include "libc/CATS/pthread.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for static loading at run-time

(* ****** ****** *)

abst@ype pthread_t = $extype"pthread_t"
castfn int_of_pthread (x: pthread_t):<> int
castfn lint_of_pthread (x: pthread_t):<> lint
fun pthread_self (): pthread_t = "mac#atslib_pthread_self"

(* ****** ****** *)

absviewt@ype pthread_attr_t = $extype"pthread_attr_t"

(* ****** ****** *)

fun pthread_attr_init (
  attr: &pthread_attr_t? >> opt (pthread_attr_t, i == 0)
) : #[i:nat] int (i)
  = "mac#atslib_pthread_attr_init"
// end of [pthread_attr_init]
//
// HX: possible failure: ENOMEM, which is remote
//
fun pthread_attr_init_exn
  (attr: &pthread_attr_t? >> pthread_attr_t): void
  = "atslib_pthread_attr_init_exn"
// end of [pthread_attr_init_exn]

(* ****** ****** *)
//
// HX: this function does not fail?
//
fun pthread_attr_destroy (
  attr: &pthread_attr_t >> opt (pthread_attr_t, i > 0)
) : #[i:nat] int (i)
  = "mac#atslib_pthread_attr_destroy"
// end of [pthread_attr_destroy]

fun pthread_attr_destroy_exn
  (attr: &pthread_attr_t >> pthread_attr_t?): void
  = "atslib_pthread_attr_destroy_exn"
// end of [pthread_attr_destroy_exn]

(* ****** ****** *)

fun pthread_create (
  tid: &pthread_t? >> pthread_t
, attr: &pthread_attr_t
, fthread: ptr -> ptr
, arg: ptr
) : int = "mac#atslib_pthread_create"
// end of [pthread_create]

(* ****** ****** *)

fun pthread_join
  (tid: pthread_t, status: &ptr? >> ptr): int = "mac#atslib_pthread_join"
// end of [pthread_join]

(* ****** ****** *)
//
// HX: this one is implemented in [$ATSHOME/ccomp/runtime/ats_prelude.c]
//
fun pthread_create_detached {vt:viewtype}
  (f: (vt) -<fun1> void, env: !vt >> opt (vt, i > 0)): #[i:nat] int i
  = "ats_pthread_create_detached"
// end of [pthread_create_detached]

//
// HX: [pthread_create_detached_exn] is implemented in
// [$ATSHOME/libc/DATS/pthread.dats]
//
fun pthread_create_detached_exn {vt:viewtype}
  (f: (vt) -<fun1> void, env: vt): void // env is to be processed by f
// end of [pthread_create_detached_exn]

//
// HX: [pthread_create_detached_cloptr] is implemented in
// [$ATSHOME/libc/DATS/pthread.dats]
//
fun pthread_create_detached_cloptr
  (f: () -<lincloptr1> void): void // HX: closure is freed to avoid leak!
// end of [pthread_create_detached_cloptr]

(* ****** ****** *)
//
// HX: [pval] is used for supporting [pthread_join]
//
fun pthread_exit
  (pval: ptr): void = "mac#atslib_pthread_exit" // macro!
// end of [pthread_exit]

(* ****** ****** *)

fun pthread_cancel
  (tid: pthread_t): int = "mac#atslib_pthread_cancel" // macro!
// end of [pthread_cancel]

fun pthread_testcancel (): void = "mac#atslib_pthread_testcancel" // macro!

(* ****** ****** *)

absview pthread_cleanup_v

fun pthread_cleanup_push
  {vt:viewtype} (
  handler: (vt) -> void, arg: vt
) : (
  pthread_cleanup_v | void
) = "mac#atslib_pthread_cleanup_push"
// end of [pthread_cleanup_push]

fun pthread_cleanup_pop
  (pf: pthread_cleanup_v | execute: int): void = "mac#atslib_pthread_cleanup_pop"
// end of [pthread_cleanup_pop]

(* ****** ****** *)

absviewt@ype
pthread_mutex_view_viewt0ype
  (v:view) = $extype"pthread_mutex_t"
// end of [absviewt@ype]
stadef mutex_vt = pthread_mutex_view_viewt0ype

(* ****** ****** *)
//
// HX: this one does initialization and locking
//
fun pthread_mutex_init_locked
  {v:view} (mut: &mutex_vt? >> opt (mutex_vt(v), i==0)): #[i:nat] int i
  = "atslib_pthread_mutex_init_locked"
// end of [pthread_mutex_init_locked]

fun pthread_mutex_init_unlocked {v:view} (
    pf: !v >> option_v (v, i > 0)
  | mut: &mutex_vt? >> opt (mutex_vt(v), i==0)
  ) : #[i:nat] int i = "atslib_pthread_mutex_init_unlocked"
// end of [pthread_mutex_init_unlocked]

(* ****** ****** *)

fun pthread_mutex_create_locked
  {v:view} {l:addr} (
// there is no argument
) : [l:addr] (
  option_v ((free_gc_v l, mutex_vt v @ l), l > null)
| ptr l
) = "atslib_pthread_mutex_create_locked"
// end of [pthread_mutex_create_locked]

fun pthread_mutex_create_unlocked
  {v:view} {l:addr} (
  pf: !v >> option_v (v, l==null) | (*none*)
) : [l:addr] (
  option_v ((free_gc_v l, mutex_vt v @ l), l > null)
| ptr l
) = "atslib_pthread_mutex_create_unlocked"
// end of [pthread_mutex_create_unlocked]

(* ****** ****** *)
//
// HX-2010-03-14:
// it should be called 'uninitialize' or 'clear' in ATS
//
fun pthread_mutex_destroy
  {v:view} {l:addr} (p: &mutex_vt(v) >> opt (mutex_vt(v), i > 0))
  : #[i:nat] (option_v (v, i==0) | int i) = "atslib_pthread_mutex_destroy"
// end of [pthread_mutex_destroy]

(* ****** ****** *)

fun pthread_mutex_lock
  {v:view} (
  mutex: &mutex_vt v
) :<> [i:nat] (option_v (v, i==0) | int i)
  = "mac#atslib_pthread_mutex_lock" // macro!
// end of [pthread_mutex_lock]

fun pthread_mutex_unlock
  {v:view} (
  resource: v | mutex: &mutex_vt v
) :<> [i:nat] (
  option_v (v, i > 0) | int i
) = "mac#atslib_pthread_mutex_unlock" // macro!
// end of [pthread_mutex_unlock]

(* ****** ****** *)

absviewt@ype
pthread_cond_viewt0ype = $extype"pthread_cond_t"
stadef cond_vt = pthread_cond_viewt0ype

(* ****** ****** *)

fun pthread_cond_create (
// there is no arugment
) : [l:addr] (
  option_v ((free_gc_v l, cond_vt @ l), l > null)
| ptr l
) = "atslib_pthread_cond_create"

fun pthread_cond_wait
  {v:view} (
  resource: !v | cond: &cond_vt, p: &mutex_vt v
) :<> int
  = "mac#atslib_pthread_cond_wait"
// end of [pthread_cond_wait]

fun pthread_cond_signal
  (cond: &cond_vt):<> int = "mac#atslib_pthread_cond_signal"
// end of [pthread_cond_signal]

fun pthread_cond_broadcast
  (cond: &cond_vt):<> int = "mac#atslib_pthread_cond_broadcast"
// end of [pthread_cond_broadcast]

(* ****** ****** *)

(* end of [pthread.sats] *)
