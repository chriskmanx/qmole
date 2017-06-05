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

(* Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload "prelude/SATS/bool.sats"

(* ****** ****** *)

%{$

ats_void_type
atspre_fprint_bool (
  ats_ptr_type out, ats_bool_type b
) {
  int n ;
  if (b) {
    n = fprintf ((FILE *)out, "true") ;
  } else {
    n = fprintf ((FILE *)out, "false") ;
  } // end of [if]
  if (n < 0) {
    ats_exit_errmsg(n, (ats_ptr_type)"Exit: [fprint_bool] failed.\n") ;
  } // end of [if]
  return ;
} // end of [atspre_fprint_bool]

%} // end of [%$]

(* ****** ****** *)

(* end of [bool.dats] *)
