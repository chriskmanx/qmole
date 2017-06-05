(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [option.sats] starts!\n"

#endif

(* ****** ****** *)

%{#
#include "prelude/CATS/option.cats"
%}

(* ****** ****** *)

(*
// this is defined in [basics_sta.sats"
datatype // t@ype+: covariant
option_t0ype_bool_type (a:t@ype+, bool) =
  | None (a, false) | Some (a, true) of a
// end of [datatype]
stadef option = option_t0ype_bool_type
typedef Option (a:t@ype) = [b:bool] option (a, b)
*)

(* ****** ****** *)

fun option_is_none
  {a:t@ype} {b:bool} (opt: option (a, b)):<> bool (~b)

fun option_is_some
  {a:t@ype} {b:bool} (opt: option (a, b)):<> bool ( b)

(* ****** ****** *)

val{a:type} option_none : option (a, false)
fun{a:type} option_some (x: a):<> option (a, true)
fun{a:type} option_unsome (opt: option (a, true)):<> a

(* ****** ****** *)

fun{a:t@ype} option_app {f:eff}
  (opt: Option a, f: a -<f> void):<f> void

fun{a1,a2:t@ype} option_map {b:bool} {f:eff}
  (opt: option (a1, b), f: a1 -<f> a2):<f> option (a2, b)

(* ****** ****** *)
//
// HX: a casting function implemented in [prelude/DATS/option.dats]
//
castfn option_of_option_vt
  {a:t@ype} {b:bool} (x: option_vt (a, b)):<> option (a, b)
// end of [option_of_option_vt]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [option.sats] finishes!\n"

#endif

(* end of [option.sats] *)
