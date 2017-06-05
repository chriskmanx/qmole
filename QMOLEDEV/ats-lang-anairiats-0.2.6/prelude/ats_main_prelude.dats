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

(* Author: Hongwei Xi ( hwxi AT cs DOT bu DOT edu ) *)

(* ****** ****** *)

// [main_prelude] is called before [main]

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

implement main_prelude () = let
  (* primary *)
  val () = $dynload "prelude/DATS/basics.dats"
//
  val () = $dynload "prelude/DATS/bool.dats"
//
  val () = $dynload "prelude/DATS/filebas.dats"
//
  val () = $dynload "prelude/DATS/float.dats"
//
  val () = $dynload "prelude/DATS/integer.dats"
//
  val () = $dynload "prelude/DATS/pointer.dats"
//
  val () = $dynload "prelude/DATS/printf.dats"
//
  val () = $dynload "prelude/DATS/reference.dats"
//
  val () = $dynload "prelude/DATS/sizetype.dats"
//
  val () = $dynload "prelude/DATS/string.dats"
//
  (* secondary *)
  val () = $dynload "prelude/DATS/arith.dats"
  val () = $dynload "prelude/DATS/array.dats"
  val () = $dynload "prelude/DATS/array0.dats"
  val () = $dynload "prelude/DATS/array_prf.dats"
  val () = $dynload "prelude/DATS/lazy.dats"
  val () = $dynload "prelude/DATS/list.dats"
  val () = $dynload "prelude/DATS/list_vt.dats"
  val () = $dynload "prelude/DATS/matrix.dats"
  val () = $dynload "prelude/DATS/matrix0.dats"
  val () = $dynload "prelude/DATS/option.dats"
  val () = $dynload "prelude/DATS/option0.dats"
  val () = $dynload "prelude/DATS/ptrarr.dats"
(*
  val () = begin
    print ("The prelude library is loaded."); print_newline ()
  end // end of [val]
*)
in
  // empty
end // end of [main_prelude]

(* ****** ****** *)

(* end of [ats_main_prelude.dats] *)
