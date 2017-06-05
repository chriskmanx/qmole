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

(*
**
** A functional array implementation based on Braun trees
** An functional array as such can also be used as a deque
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: April, 2010 // based on a version done in October, 2008
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

abstype array_t0ype_int_type
  (a:t@ype+ (*element*),  n:int (*size*))
stadef array = array_t0ype_int_type

(* ****** ****** *)

//
// HX-2010-04-04:
// this means staloading at run-time is needed!
//
exception SubscriptException of ()

(* ****** ****** *)

fun{}
funarray_make_nil {a:t@ype} ():<(*pure*)> array (a, 0)

(* ****** ****** *)

// compute the size of [A]
fun{a:t@ype} //  O(log^2(n))
funarray_size {n:nat} (A: array (a, n)):<(*pure*)> size_t n

(* ****** ****** *)
//
// obtain the element stored in 'A[i]'
//
fun{a:t@ype} // O(log(n))
funarray_get_elt_at
  {n:nat} (A: array (a, n), i: natLt n):<(*pure*)> a
overload [] with funarray_get_elt_at

//
// update 'A[i]' with 'x'; note that this creates a new array!
//
fun{a:t@ype} // O(log(n))
funarray_set_elt_at
  {n:nat} (A: &array (a, n), i: natLt n, x: a):<(*pure*)> void
overload [] with funarray_set_elt_at
  
(* ****** ****** *)
//
// exchange elements stored in 'A[i]' and 'x'
//
fun{a:t@ype}// O(log(n))
funarray_xch_elt_at
  {n:nat} (A: &array (a, n), i: natLt n, x: a):<(*pure*)> a
// end of [funarray_xch_elt_at]

(* ****** ****** *)

fun{a:t@ype} funarray_get_elt_at_exn
  {n:nat} (A: array (a, n), i: Nat):<!exn> a
fun{a:t@ype} funarray_set_elt_at_exn
  {n:nat} (A: &array (a, n), i: Nat, x: a):<!exn> void

(* ****** ****** *)
//
// insert an element to the start of the array
//
fun{a:t@ype}
funarray_loadd {n:nat} // O(log(n))
  (A: &array (a, n) >> array (a, n+1), x: a):<(*pure*)> void
// end of [funarray_loadd]

//
// remove an element from the start of the array
//
fun{a:t@ype}
funarray_lorem (* O(log(n)) *)
  {n:pos} (A: &array (a, n) >> array (a, n-1)):<(*pure*)> void
// end of [funarray_lorem]

//
// remove an element from the start of the array and obtain it
//
fun{a:t@ype}
funarray_lorem_get (* O(log(n)) *)
  {n:pos} (A: &array (a, n) >> array (a, n-1)):<(*pure*)> a
// end of [funarray_lorem_get]

(* ****** ****** *)
//
// insert an element to the end of the array
//
fun{a:t@ype}
funarray_hiadd {n:nat} // O(log(n))
  (A: &array (a, n) >> array (a, n+1), n: int n, x: a):<(*pure*)> void
// end of [funarray_hiadd]

//
// remove an element from the end of the array
//
fun{a:t@ype}
funarray_hirem // O(log(n))
  {n:pos} (A: &array (a, n) >> array (a, n-1), n: int n):<(*pure*)> void
// end of [funarray_hirem]

//
// remove an element from the end of the array and obtain it
//
fun{a:t@ype}
funarray_hirem_get (* O(log(n)) *)
  {n:pos} (A: &array (a, n) >> array (a, n-1), n: int n):<(*pure*)> a
// end of [funarray_hirem_get]

(* ****** ****** *)

(*
** HX-2009:
** these higher-order functions are probably not particularly useful as
** they can be readily replaced with for-loops. See the implementation.
*)

fun{a:t@ype} funarray_foreach_vclo {v:view} {n:nat}
  (pf: !v | A: array (a, n), n: int n, f: &(!v | a) -<clo> void):<> void

fun{a:t@ype}
funarray_foreach_cloptr {n:nat}
  (A: array (a, n), n: int n, f: !(a) -<cloptr> void):<> void
fun{a:t@ype} funarray_foreach_vcloptr {v:view} {n:nat}
  (pf: !v | A: array (a, n), n: int n, f: !(!v | a) -<cloptr> void):<> void

fun{a:t@ype} funarray_foreach_cloref {n:nat}
  (A: array (a, n), n: int n, f: a -<cloref> void):<!ref> void
  
(* ****** ****** *)

fun{a:t@ype} funarray_iforeach_vclo {v:view} {n:nat}
  (pf: !v | A: array (a, n), n: int n, f: &(!v | natLt n, a) -<clo> void):<> void

fun{a:t@ype}
funarray_iforeach_cloptr {n:nat}
  (A: array (a, n), n: int n, f: !(natLt n, a) -<cloptr> void):<> void
fun{a:t@ype}
funarray_iforeach_vcloptr {v:view} {n:nat}
  (pf: !v | A: array (a, n), n: int n, f: !(!v | natLt n, a) -<cloptr> void):<> void

fun{a:t@ype} funarray_iforeach_cloref {n:nat}
  (A: array (a, n), n: int n, f:  (natLt n, a) -<cloref> void):<!ref> void

(* ****** ****** *)

(* end of [funarray_braun.sats] *)
