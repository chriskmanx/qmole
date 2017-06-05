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
** A dynamically resizable vector implementation
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Secptember, 2010
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

%{#
#include "libats/CATS/vector.cats"
%} // end of [%{#]

(* ****** ****** *)

absview vector_v (
  a:viewt@ype+, m: int, n: int, l:addr
) // end of [vector_v]

prfun
vector_v_encode
  {a:viewt@ype}
  {n1,n2:int} {l:addr}
  {ofs:int} (
  pfmul: MUL (n1, sizeof a, ofs)
, pfarr1: array_v (a, n1, l), pfarr2: array_v (a?, n2, l+ofs)
) : vector_v (a, n1+n2, n1, l)
// end of [vector_v_encode]

prfun
vector_v_decode
  {a:viewt@ype}
  {m,n:int} {l:addr}
  {ofs:int} (
  pfmul: MUL (n, sizeof a, ofs), pfarr: vector_v (a, m, n, l)
): (array_v (a, n, l), array_v (a?, m-n, l+ofs))
// end of [vector_v_decode]

(* ****** ****** *)

viewtypedef
VSHELL (
  m:int, n:int, l:addr
) = @{
  m= size_t (m)
, n= size_t (n)
, ptr= ptr (l)
, vfree= free_gc_v l
} // end of [VECTOR0]

viewtypedef
VSHELL0 = VSHELL (0, 0, null)

(* ****** ****** *)

absviewt@ype VECTOR // (elt, capacity, size)
  (a:viewt@ype+, m:int, n:int) = VSHELL (m, n, null)
// end of [VECTOR]
viewtypedef VECTOR1 (a:viewt@ype, n:int) = [m:int] VECTOR (a, m, n)

prfun VECTOR_encode {a:viewt@ype} {m,n:int} {l:addr}
  (pf: vector_v (a, m, n, l) | V: &VSHELL (m, n, l) >> VECTOR (a, m, n)):<> void
// end of [VECTOR_encode]

prfun VECTOR_decode {a:viewt@ype} {m,n:int} {l:addr}
  (V: &VECTOR (a, m, n) >> VSHELL (m, n, l)):<> #[l:addr] vector_v (a, m, n, l)
// end of [VECTOR_decode]

(* ****** ****** *)

fun vector_cap
  {a:viewt@ype} {m,n:int} (V: &VECTOR (a, m, n)):<> size_t m
fun vector_size
  {a:viewt@ype} {m,n:int} (V: &VECTOR (a, m, n)):<> size_t n

(* ****** ****** *)

fun{a:viewt@ype}
vector_initialize {m:nat} (
  V: &VSHELL0? >> VECTOR (a, m, 0), m: size_t m
) :<> void // end of [vector_initialize]

(* ****** ****** *)

fun vector_clear {a:t@ype}
  {m,n:int} (V: &VECTOR (a, m, n) >> VECTOR (a, m, 0)):<> void
// end of [vector_clear]

fun{a:viewt@ype}
vector_clear_fun {m,n:int} (
  V: &VECTOR (a, m, n) >> VECTOR (a, m, 0), f: (&a >> a?) -<fun> void
) :<> void // end of [vector_clear_fun]

(* ****** ****** *)

fun{a:t@ype}
vector_uninitialize {m,n:int} (V: &VECTOR (a, m, n) >> VSHELL0?):<> void

fun{a:viewt@ype}
vector_uninitialize_vt {m:int} (V: &VECTOR (a, m, 0) >> VSHELL0?):<> void

(* ****** ****** *)

fun{a:t@ype}
vector_get_elt_at {m,n:int} (V: &VECTOR (a, m, n), i: sizeLt n):<> a
// end of [vector_get_elt_at]

fun{a:t@ype}
vector_set_elt_at {m,n:int} (V: &VECTOR (a, m, n), i: sizeLt n, x: a):<> void
// end of [vector_set_elt_at]

(* ****** ****** *)

fun{a:viewt@ype}
vector_append
  {m,n:int | n < m}
  (V: &VECTOR (a, m, n) >> VECTOR (a, m, n+1), x: a):<> void
// end of [vector_append]

fun{a:viewt@ype}
vector_prepend
  {m,n:int | n < m}
  (V: &VECTOR (a, m, n) >> VECTOR (a, m, n+1), x: a):<> void
// end of [vector_prepend]

(* ****** ****** *)

fun{a:viewt@ype}
vector_insert_at
  {m,n:int | n < m} {i:nat | i <= n}
  (V: &VECTOR (a, m, n) >> VECTOR (a, m, n+1), i: size_t i, x: &a >> a?!):<> void
// end of [vector_insert]

fun{a:viewt@ype}
vector_remove_at
  {m,n:int} {i:nat | i < n}
  (V: &VECTOR (a, m, n) >> VECTOR (a, m, n-1), i: size_t i, x: &a? >> a ):<> void
// end of [vector_remove]

(* ****** ****** *)

fun{a:t@ype}
vector_resize {m,n:int} {m1:int | n <= m1}
  (V: &VECTOR (a, m, n) >> VECTOR (a, m1, n), m1: size_t m1):<> void
// end of [vector_resize]

(* ****** ****** *)

fun vector_foreach_funenv_tsz
  {a:viewt@ype} {v:view} {vt:viewtype} {m,n:int} (
  pf: !v
| V: &VECTOR (a, m, n)
, f: (!v | &a, !vt) -<> void, tsz: sizeof_t a, env: !vt
) :<> void // end of [vector_foreach_funenv_tsz]

fun{a:viewt@ype}
vector_foreach_vclo {v:view} {m,n:int}
  (pf: !v | V: &VECTOR (a, m, n), f: &(!v | &a) -<clo> void) :<> void
// end of [vector_foreach_vclo]

(* ****** ****** *)

fun vector_iforeach_funenv_tsz
  {a:viewt@ype} {v:view} {vt:viewtype} {m,n:int} (
  pf: !v
| V: &VECTOR (a, m, n)
, f: (!v | sizeLt n, &a, !vt) -<> void, tsz: sizeof_t a, env: !vt
) :<> void // end of [vector_iforeach_funenv_tsz]

fun{a:viewt@ype}
vector_iforeach_vclo {v:view} {m,n:int}
  (pf: !v | V: &VECTOR (a, m, n), f: &(!v | sizeLt n, &a) -<clo> void) :<> void
// end of [vector_iforeach_vclo]

(* ****** ****** *)

(* end of [vector.sats] *)
