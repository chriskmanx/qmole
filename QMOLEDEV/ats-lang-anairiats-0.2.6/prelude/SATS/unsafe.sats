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
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// HX:
// For safety, [unsafe.sats] should not be loaded automatically.
// The unsafe functions declared here must be used with caution!!!
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)
//
// HX: [castvwtp1] : it is mostly used in a situation
// where a linear value is passed as a read-only value;
// for instance, casting [strptr] to [string]
//
castfn cast {to:t@ype} {from:t@ype} (x: from):<> to
castfn castvwtp1 {to:t@ype} {from:viewt@ype} (x: !from):<> to

(* ****** ****** *)

castfn cast2int {a:t@ype} (x: a):<> int
castfn cast2uint {a:t@ype} (x: a):<> uint
castfn cast2lint {a:t@ype} (x: a):<> lint
castfn cast2ulint {a:t@ype} (x: a):<> ulint
castfn cast2size {a:t@ype} (x: a):<> size_t
castfn cast2ssize {a:t@ype} (x: a):<> ssize_t

(* ****** ****** *)
//
// HX: only if you know what you are doing ...
//
fun{a:viewt@ype} ptrget (p: ptr): a
fun{a:viewt@ype} ptrset (p: ptr, x: a): void

(* ****** ****** *)
//
// HX-2011-02-26: virtual takeout
//
absview viewout (v:view) // invariant!
prfun vtakeout {v:view} (pf: !v): viewout (v)
prfun viewout_decode
  {v:view} (pf: viewout (v)): (v, v -<lin,prf> void)
// end of [viewout_decode]

(* ****** ****** *)

(* end of [unsafe.sats] *)
