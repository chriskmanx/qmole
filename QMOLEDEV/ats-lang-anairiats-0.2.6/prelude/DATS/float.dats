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

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

staload "prelude/SATS/float.sats"

(* ****** ****** *)

//
// The following kind of code duplication should really be avoided!
//

implement pow_float_int1 (x, n) = let
  typedef T = float
  fun loop {n:nat} .<n>. (x: T, n: int n, res: T):<> T = begin
    case+ 0 of
    | _ when n >= 2 => let
        val n2 = n / 2; val xx = x * x
      in
        if n > n2 + n2 then loop (xx, n2, x * res) else loop (xx, n2, res)
      end
    | _ when n >= 1 => x * res
    | _ => res
  end // end of [loop]
in
  loop (x, n, 1.0: T)
end // end of [pow_float_int1]


implement pow_double_int1 (x, n) = let
  typedef T = double
  fun loop {n:nat} .<n>. (x: T, n: int n, res: T):<> T = begin
    case+ 0 of
    | _ when n >= 2 => let
        val n2 = n / 2; val xx = x * x
      in
        if n > n2 + n2 then loop (xx, n2, x * res) else loop (xx, n2, res)
      end
    | _ when n >= 1 => x * res
    | _ => res
  end // end of [loop]
in
  loop (x, n, 1.0: T)
end // end of [pow_double_int1]


implement pow_ldouble_int1 (x, n) = let
  typedef T = ldouble
  fun loop {n:nat} .<n>. (x: T, n: int n, res: T):<> T = begin
    case+ 0 of
    | _ when n >= 2 => let
        val n2 = n / 2; val xx = x * x
      in
        if n > n2 + n2 then loop (xx, n2, x * res) else loop (xx, n2, res)
      end
    | _ when n >= 1 => x * res
    | _ => res
  end // end of [loop]
in
  loop (x, n, 1.0: T)
end // end of [pow_ldouble_int1]


(* ****** ****** *)

(* end of [float.dats] *)
