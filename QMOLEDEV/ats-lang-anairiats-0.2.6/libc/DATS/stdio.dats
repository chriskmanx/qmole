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

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/stdio.sats"

(* ****** ****** *)

implement
fclose1_loop (pf | p) = let
  val err = fclose1_err (pf | p)
in
  if err = 0 then let
    prval None_v () = pf in 0
  end else let
    prval Some_v pf = pf
    val errno = errno_get ()
  in
    if (errno = EINTR) then
      fclose1_loop (pf | p)
    else let
      prval () = __assert (pf) where {
        extern prfun __assert {v:view} (pf: v): void
      } // end of [prval]
    in
      err
    end // end of [if]
  end // end of [if]
end // end of [if]

(* ****** ****** *)

(* end of [stdio.dats] *)
