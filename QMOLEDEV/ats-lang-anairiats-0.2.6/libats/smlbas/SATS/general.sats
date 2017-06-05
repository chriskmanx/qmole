(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
**
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
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: General (http://www.standardml.org/Basis/general.html)
//

(* ****** ****** *)

exception Bind of ()
exception Match of ()
exception Chr of ()
exception Div of ()
exception Domain of () // out-of-domain type of error
exception Empty of ()
exception Fail of string
exception Overflow of ()
exception Range of () // out-of-range type of error
exception Size of ()
exception Span of ()
exception Subscript of ()
exception Undefined of string // for undefined values and functions 

(* ****** ****** *)

fun{a,b,c:t@ype} compose
  (_: b -<cloref1> c, _: a -<cloref1> b): a -<cloref1> c
// end of [compose]

fun{a:t@ype} ignore (x: a): void

(* ****** ****** *)

(* end of [general.sats] *)
