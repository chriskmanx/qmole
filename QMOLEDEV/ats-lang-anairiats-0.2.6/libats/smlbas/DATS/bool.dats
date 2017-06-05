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

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009

(* ****** ****** *)

//
// SML Basis Library: Bool (http://www.standardml.org/Basis/bool.html)
//

(* ****** ****** *)

staload "libats/smlbas/SATS/bool.sats"

(* ****** ****** *)

implement not (b) = ~b

implement toString (b) =
  if b then "true" else "false"
// end of [toString]

local

staload "libc/SATS/string.sats"

in // in of [local]

implement fromString (s) = case+ s of
  | _ when strncmp (s, "true", 4) = 0 => option0_some true
  | _ when strncmp (s, "false", 5) = 0 => option0_some false
  | _ => option0_none () 
// end of [fromString]

end // end of [local]

(* ****** ****** *)

(* end of [bool.dats] *)
