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
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//
(* ****** ****** *)

(* some commonly used _local_ macro definitions *)

(* ****** ****** *)
//
symintr is_nil
symintr is_cons
//
symintr tup_head
symintr tup_tail
//
(* ****** ****** *)

macdef min (x, y) = if ,(x) <= ,(y) then ,(x) else ,(y)
macdef max (x, y) = if ,(x) <= ,(y) then ,(y) else ,(x)

(* ****** ****** *)

macdef println (x) = (print ,(x); print_newline ())
macdef prerrln (x) = (prerr ,(x); prerr_newline ())

(* ****** ****** *)

local

macrodef
rec
printstar_rec args =
  if is_nil args then `() else `(
    print ,(tup_head args) ; ,(printstar_rec (tup_tail args))
  ) // end of [if]
// end of [printstar_rec]

macrodef
rec
prerrstar_rec args =
  if is_nil args then `() else `(
    prerr ,(tup_head args) ; ,(prerrstar_rec (tup_tail args))
  ) // end of [if]
// end of [prerrstar_rec]

in // in of [local]

//
// HX: for instance, we can write something like:
// printstarln @("x+y = ", x+y, " and x*y = ", x*y)
//
macdef printstar args = ,(printstar_rec args)
macdef printstarln args = begin ,(printstar_rec args); print_newline () end

macdef prerrstar args = ,(prerrstar_rec args)
macdef prerrstarln args = begin ,(prerrstar_rec args); prerr_newline () end

end // end of [local]

(* ****** ****** *)

(* end of [lmacrodef.hats] *)
