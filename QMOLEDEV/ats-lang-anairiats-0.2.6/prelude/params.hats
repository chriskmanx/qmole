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
// author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//
(* ****** ****** *)

#define VERBOSE_FIXITY 0 (* used in [prelude/fixity.ats] *)
#define VERBOSE_PRELUDE 0 (* mainly for the purpose of debugging *)

(* ****** ****** *)

#define ATS_MAJOR_VERSION 0
#define ATS_MINOR_VERSION 2
#define ATS_MICRO_VERSION 6

(* ****** ****** *)

#define ATS_VERBOSE_LEVEL 0

#define ATS_CC_VERBOSE_LEVEL 1 // this one is used in the following files
// $ATSHOME/src/ats_ccomp_emit.dats

#define ATS_GC_VERBOSE_LEVEL 0 // this one is used in the following files
// $ATSHOME/ccomp/runtime/GCATS/gc_top.dats

(* ****** ****** *)

#define ATS_PKGCONFIG 1 // this one is used in the following files:
// $ATSHOME/utils/scripts/atscc_main.dats

(* ****** ****** *)

(* end of [params.hats] *)
