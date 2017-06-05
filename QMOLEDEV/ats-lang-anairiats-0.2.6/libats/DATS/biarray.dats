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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** Bidirectional Arrays (arrays moving from left to right and vice versa)
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: September, 2011
**
*)

(* ****** ****** *)

staload "libats/SATS/biarray.sats"

(* ****** ****** *)

assume
biarray_v (
  a:viewt@ype, n:int, lbeg:addr, lend:addr
) = [ofs:int | lend==lbeg+ofs] (
  MUL (n, sizeof(a), ofs), array_v (a, n, lbeg)
) // end of [biarray_v]

(* ****** ****** *)

implement
array_v_of_biarray_v (pf) =
  let prval (pfmul, pfarr) = pf in pfarr end
// end of [array_v_of_biarray_v]

(* ****** ****** *)

implement
biarray_v_of_array_v (pfmul, pfarr) = (pfmul, pfarr)

(* ****** ****** *)

implement
biarray_v_offset
  {a} (pf) = pfmul where {
  prval (pfmul, pfarr) = pf
  prval () = pf := biarray_v_of_array_v {a} (pfmul, pfarr)
} // end of [biarray_v_offset]

(* ****** ****** *)

implement
biarray_v_unnil (pf) = let
  val (pfmul, pfarr) = pf; val () = array_v_unnil (pfarr)
in
  mul_elim (pfmul)
end // end of [biarray_v_unnil]

(* ****** ****** *)

(* end of [biarray.dats] *)
