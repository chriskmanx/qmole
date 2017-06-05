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

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [list.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

%{#
#include "prelude/CATS/list.cats"
%} // end of [%{#]

(* ****** ****** *)

(*
** Notes (HX-2009-11-8):
**
** if a function f returns a list, then usually the returned list is
** a linear list; if needed, please call the function [list_of_list_vt]
** to turn the linear list into a nonlnear one; as [list_of_list_vt] is
** a cast function (castfn), there is no run-time cost associated with
** calling it.
**
*)

(* ****** ****** *)

(*
// this is defined in [basic_sta.sats]
datatype // t@ype+: covariant
list_t0ype_int_type (a:t@ype+, int) =
  | {n:int | n >= 0}
    list_cons (a, n+1) of (a, list_t0ype_int_type (a, n))
  | list_nil (a, 0)
// end of [datatype]
stadef list = list_t0ype_int_type
typedef List (a:t@ype) = [n:int | n >= 0] list (a, n)
*)

(* ****** ****** *)

prfun list_length_is_nonnegative
  {a:t@ype} {n:int} (xs: list (a, n)): [n>=0] void
// end of [list_length_is_nonnegative]

(* ****** ****** *)

macdef list_sing (x) = list_cons (,(x), list_nil ())

(* ****** ****** *)

exception ListSubscriptException of ()

(* ****** ****** *)
//
// HX: a casting function
// implemented in [prelude/DATS/list.dats]
//
castfn list_of_list_vt
  {a:t@ype} {n:nat} (xs: list_vt (a, n)):<> list (a, n)
// end of [list_of_list_vt]

(* ****** ****** *)
//
// HX: always inlining
//
fun{} list_is_empty
  {a:t@ype} {n:nat} (xs: list (a, n)):<> bool (n==0)
fun{} list_isnot_empty
  {a:t@ype} {n:nat} (xs: list (a, n)):<> bool (n > 0)
overload ~ with list_isnot_empty

fun{a:t@ype} list_is_sing
  {n:nat} (xs: list (a, n)):<> bool (n==1)
// end of [list_is_sing]

(* ****** ****** *)

fun{a:t@ype}
list_app_funenv
  {v:view} {vt:viewtype} {f:eff} (
  pf: !v | xs: List a, f: (!v | a, !vt) -<fun,f> void, env: !vt
) :<f> void // end of [list_app_funenv]

fun{a:t@ype}
list_app_fun {f:eff} (xs: List a, f: a -<fun,f> void):<f> void

fun{a:t@ype}
list_app_vclo {v:view} {f:eff} (
  pf: !v | xs: List a, f: &(!v | a) -<clo,f> void
) :<f> void // end of [list_app_vclo]

fun{a:t@ype}
list_app_cloptr {f:eff} (
  xs: List a, f: !(a) -<cloptr,f> void
) :<f> void // end of [list_app_cloptr]
fun{a:t@ype}
list_app_vcloptr {v:view} {f:eff} (
  pf: !v | xs: List a, f: !(!v | a) -<cloptr,f> void
) :<f> void // end of [list_app_vcloptr]

fun{a:t@ype}
list_app_cloref {f:eff} (xs: List a, f: (a -<cloref,f> void)):<f> void

(*
symintr list_app
overload list_app with list_app_fun
overload list_app with list_app_vclo
overload list_app with list_app_cloptr
overload list_app with list_app_vcloptr
overload list_app with list_app_cloref
*)

(* ****** ****** *)

fun{a1,a2:t@ype}
list_app2_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff} (
  pf: !v
| xs: list (a1, n)
, ys: list (a2, n)
, f: (!v | a1, a2, !vt) -<fun,f> void
, env: !vt
) :<f> void // end of [list_app2_funenv]

fun{a1,a2:t@ype}
list_app2_fun {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: (a1, a2) -<fun,f> void
) :<f> void // end of [list_app2_fun]

fun{a1,a2:t@ype}
list_app2_vclo {v:view} {n:nat} {f:eff} (
  pf: !v | xs: list (a1, n), ys: list (a2, n), f: &(!v | a1, a2) -<clo,f> void
) :<f> void // end of [list_app2_vclo]

fun{a1,a2:t@ype}
list_app2_cloptr {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: !(a1, a2) -<cloptr,f> void
) :<f> void // end of [list_app2_cloptr]
fun{a1,a2:t@ype}
list_app2_vcloptr {v:view} {n:nat} {f:eff} (
  pf: !v | xs: list (a1, n), ys: list (a2, n), f: !(!v | a1, a2) -<cloptr,f> void
) :<f> void // end of [list_app2_vcloptr]

fun{a1,a2:t@ype}
list_app2_cloref {n:nat} {f:eff}
  (xs: list (a1, n), ys: list (a2, n), f: (a1, a2) -<cloref,f> void):<f> void
// end of [list_app2_cloref]

(*
symintr list_app2
overload list_app2 with list_app2_fun
overload list_app2 with list_app2_vclo
overload list_app2 with list_app2_cloptr
overload list_app2 with list_app2_vcloptr
overload list_app2 with list_app2_cloref
*)

(* ****** ****** *)

fun{a:t@ype} list_append {i,j:nat}
  (xs: list (a, i), ys: list (a, j)):<> list (a, i+j)
overload + with list_append

fun{a:t@ype} list_append1_vt {i,j:nat}
  (xs: list_vt (a, i), ys: list (a, j)):<> list (a, i+j)
// end of [list_append1_vt]

fun{a:t@ype} list_append2_vt {i,j:nat}
  (xs: list (a, i), ys: list_vt (a, j)):<> list_vt (a, i+j)
// end of [list_append2_vt]

(* ****** ****** *)

fun{a1,a2:t@ype}
list_assoc_funenv
  {v:view} {vt:viewtype} {eq:eff} (
  pf: !v
| xs: List @(a1, a2), eq: (!v | a1, a1, !vt) -<fun,eq> bool, x: a1, env: !vt
) :<eq> Option_vt a2 // end of [list_assoc_funenv]

fun{a1,a2:t@ype}
list_assoc_fun {eq:eff} (
  xs: List @(a1, a2), eq: (a1, a1) -<fun,eq> bool, x: a1
) :<eq> Option_vt a2 // end of [list_assoc_fun]

fun{a1,a2:t@ype}
list_assoc_vclo {v:view} {eq:eff} (
  pf: !v | xs: List @(a1, a2), eq: &(!v | a1, a1) -<clo,eq> bool, x: a1
) :<eq> Option_vt a2 // end of [list_assoc_vclo]

fun{a1,a2:t@ype}
list_assoc_cloptr {eq:eff} (
  xs: List @(a1, a2), eq: !(a1, a1) -<cloptr,eq> bool, x: a1
) :<eq> Option_vt a2 // end of [list_assoc_cloptr]
fun{a1,a2:t@ype}
list_assoc_vcloptr {v:view} {eq:eff} (
  pf: !v | xs: List @(a1, a2), eq: !(!v | a1, a1) -<cloptr,eq> bool, x: a1
) :<eq> Option_vt a2 // end of [list_assoc_vcloptr]

fun{a1,a2:t@ype}
list_assoc_cloref {eq:eff} (
  xs: List @(a1, a2), eq: (a1, a1) -<cloref,eq> bool, x: a1
) :<eq> Option_vt a2 // end of [list_assoc_cloref]

(* ****** ****** *)

fun{a:t@ype}
list_concat (xs: List (List a)):<> List_vt a

(* ****** ****** *)

fun{a:t@ype}
list_copy {n:nat} (xs: list (a, n)):<> list_vt (a, n)

(* ****** ****** *)

fun{a:t@ype}
list_drop
  {n:int} {i:nat | i <= n}
  (xs: list (a, n), i: int i):<> list (a, n-i)
// end of [list_drop]

fun{a:t@ype}
list_drop_exn {n,i:nat}
  (xs: list (a, n), i: int i):<!exn> [i <= n] list (a, n-i)
// end of [list_drop_exn]

(* ****** ****** *)

fun{a:t@ype}
list_exists_funenv
  {v:view} {vt:viewtype} {p:eff} (
  pf: !v | xs: List a, p: (!v | a, !vt) -<fun,p> bool, env: !vt
) :<p> bool // end of [list_exists_funenv]

fun{a:t@ype}
list_exists_fun {p:eff} (xs: List a, p: a -<p> bool):<p> bool

fun{a:t@ype}
list_exists_vclo {v:view} {p:eff}
  (pf: !v | xs: List a, p: &(!v | a) -<clo,p> bool):<p> bool

fun{a:t@ype}
list_exists_cloptr {p:eff}
  (xs: List a, p: !(a) -<cloptr,p> bool):<p> bool
fun{a:t@ype}
list_exists_vcloptr {v:view} {p:eff}
  (pf: !v | xs: List a, p: !(!v | a) -<cloptr,p> bool):<p> bool

fun{a:t@ype}
list_exists_cloref {p:eff} (xs: List a, p: a -<cloref,p> bool):<p> bool

(*
symintr list_exists
overload list_exists with list_exists_fun
overload list_exists with list_exists_vclo
overload list_exists with list_exists_cloptr
overload list_exists with list_exists_vcloptr
overload list_exists with list_exists_cloref
*)

(* ****** ****** *)

fun{a1,a2:t@ype}
list_exists2_funenv
  {v:view} {vt:viewtype} {n:nat} {p:eff} (
    pf: !v
  | xs1: list (a1, n)
  , xs2: list (a2, n)
  , p: (!v | a1, a2, !vt) -<fun,p> bool
  , env: !vt
  ) :<p> bool
// end of [list_exists2_funenv]

fun{a1,a2:t@ype}
list_exists2_fun {n:nat} {p:eff}
  (xs: list (a1, n), ys: list (a2, n), p: (a1, a2) -<p> bool):<p> bool

fun{a1,a2:t@ype}
list_exists2_vclo {v:view} {n:nat} {p:eff} (
  pf: !v | xs: list (a1, n), ys: list (a2, n), p: &(!v | a1, a2) -<clo,p> bool
) :<p> bool // end of [list_exists2_vclo]

fun{a1,a2:t@ype}
list_exists2_cloptr {n:nat} {p:eff} (
  xs: list (a1, n), ys: list (a2, n), p: !(a1, a2) -<cloptr,p> bool
) :<p> bool // end of [list_exists2_cloptr]
fun{a1,a2:t@ype}
list_exists2_vcloptr {v:view} {n:nat} {p:eff} (
  pf: !v | xs: list (a1, n), ys: list (a2, n), p: !(!v | a1, a2) -<cloptr,p> bool
) :<p> bool // end of [list_exists2_vcloptr]

fun{a1,a2:t@ype}
list_exists2_cloref {n:nat} {p:eff}
  (xs: list (a1, n), ys: list (a2, n), p: (a1, a2) -<cloref,p> bool):<p> bool
// end of [list_exists2_cloref]

(*
symintr list_exists2
overload list_exists2 with list_exists2_fun
overload list_exists2 with list_exists2_vclo
overload list_exists2 with list_exists2_cloptr
overload list_exists2 with list_exists2_vcloptr
overload list_exists2 with list_exists2_cloref
*)

(* ****** ****** *)

fun{a:t@ype} // HX: this function is O(n)-time
list_extend {n:nat} (xs: list (a, n), y: a):<> list_vt (a, n+1)

(* ****** ****** *)

fun{a:t@ype}
list_filter_funenv
  {v:view} {vt:viewtype} {n:nat} {p:eff} (
  pf: !v | xs: list (a, n), p: (!v | a, !vt) -<fun,p> bool, env: !vt
) :<p> [n':nat | n' <= n] list_vt (a, n')

fun{a:t@ype}
list_filter_fun {n:nat} {p:eff}
  (xs: list (a, n), p: a -<fun,p> bool):<p> [n':nat | n' <= n] list_vt (a, n')
// end of [list_filter_fun]

fun{a:t@ype}
list_filter_vclo {v:view} {n:nat} {p:eff} (
  pf: !v | xs: list (a, n), p: &(!v | a) -<clo,p> bool
) :<p> [n':nat | n' <= n] list_vt (a, n')

fun{a:t@ype}
list_filter_cloptr {n:nat} {p:eff} (
  xs: list (a, n), p: !(a) -<cloptr,p> bool
) :<p> [n':nat | n' <= n] list_vt (a, n')
fun{a:t@ype}
list_filter_vcloptr {v:view} {n:nat} {p:eff} (
  pf: !v | xs: list (a, n), p: !(!v | a) -<cloptr,p> bool
) :<p> [n':nat | n' <= n] list_vt (a, n')

fun{a:t@ype}
list_filter_cloref {n:nat} {p:eff}
  (xs: list (a, n), p: a -<cloref,p> bool):<p> [n':nat | n' <= n] list_vt (a, n')
// end of [list_filter_cloref]

(* ****** ****** *)

fun{a:t@ype}
list_find_funenv
  {v:view} {vt:viewtype} {p:eff} (
  pf: !v | xs: List a, p: (!v | a, !vt) -<fun,p> bool, env: !vt
) :<p> Option_vt a // end of [list_find_funenv]

fun{a:t@ype}
list_find_fun {p:eff} (xs: List a, p: a -<fun,p> bool):<p> Option_vt a

fun{a:t@ype}
list_find_vclo {v:view} {p:eff}
  (pf: !v | xs: List a, p: &(!v | a) -<clo,p> bool):<p> Option_vt a
// end of [list_find_vclo]

fun{a:t@ype}
list_find_cloptr {p:eff}
  (xs: List a, p: !(a) -<cloptr,p> bool):<p> Option_vt a
fun{a:t@ype}
list_find_vcloptr {v:view} {p:eff}
  (pf: !v | xs: List a, p: !(!v | a) -<cloptr,p> bool):<p> Option_vt a

fun{a:t@ype}
list_find_cloref {p:eff} (xs: List a, p: a -<cloref,p> bool):<p> Option_vt a

(* ****** ****** *)

fun{init:t@ype}{a:t@ype}
list_fold_left_funenv {v:view} {vt:viewtype} {f:eff} (
  pf: !v
| f: (!v | init, a, !vt) -<fun,f> init, ini: init, xs: List a, env: !vt
) :<f> init // end of [list_fold_left_funenv]

fun{init:t@ype}{a:t@ype}
list_fold_left_fun {f:eff}
  (f: (init, a) -<fun,f> init, ini: init, xs: List a):<f> init
// end of [list_fold_left_fun]

fun{init:t@ype}{a:t@ype}
list_fold_left_vclo {v:view} {f:eff} (
  pf: !v | f: &(!v | init, a) -<clo,f> init, ini: init, xs: List a
) :<f> init // end of [list_fold_left_vclo]

fun{init:t@ype}{a:t@ype}
list_fold_left_cloptr {f:eff} (
  f: !(init, a) -<cloptr,f> init, ini: init, xs: List a
) :<f> init // end of [list_fold_left_cloptr]
fun{init:t@ype}{a:t@ype}
list_fold_left_vcloptr {v:view} {f:eff} (
  pf: !v | f: !(!v | init, a) -<cloptr,f> init, ini: init, xs: List a
) :<f> init // end of [list_fold_left_vcloptr]

fun{init:t@ype}{a:t@ype}
list_fold_left_cloref {f:eff}
  (f: (init, a) -<cloref,f> init, ini: init, xs: List a):<f> init
// end of [list_fold_left_cloref]

(* ****** ****** *)

fun{init:t@ype}{a1,a2:t@ype}
list_fold2_left_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff} (
  pf: !v
| f: (!v | init, a1, a2, !vt) -<fun,f> init
, ini: init
, xs1: list (a1, n)
, xs2: list (a2, n)
, env: !vt
) :<f> init
// end of [list_fold2_left_funenv]

fun{init:t@ype}{a1,a2:t@ype}
list_fold2_left_cloptr
  {v:view} {n:nat} {f:eff} (
  pf: !v
| f: !(!v | init, a1, a2) -<cloptr,f> init
, ini: init, xs1: list (a1, n), xs2: list (a2, n)
):<f> init // end of [list_fold2_left_cloptr]

fun{init:t@ype}{a1,a2:t@ype}
list_fold2_left_cloref {n:nat} {f:eff} (
  f: (init, a1, a2) -<cloref,f> init, ini: init, xs1: list (a1, n), xs2: list (a2, n)
) :<f> init // end of [list_fold2_left_cloref]

(* ****** ****** *)

fun{a:t@ype}{sink:t@ype}
list_fold_right_funenv
  {v:view} {vt:viewtype} {f:eff} (
  pf: !v | f: (!v | a, sink, !vt) -<fun,f> sink, xs: List a, snk: sink, env: !vt
) :<f> sink // end of [list_fold_right_funenv]

fun{a:t@ype}{sink:t@ype}
list_fold_right_fun {f:eff}
  (f: (a, sink) -<fun,f> sink, xs: List a, snk: sink):<f> sink
// end of [list_fold_right_fun]

fun{a:t@ype}{sink:t@ype}
list_fold_right_vclo {v:view} {f:eff}
  (pf: !v | f: &(!v | a, sink) -<clo,f> sink, xs: List a, snk: sink):<f> sink
// end of [list_fold_right_vclo]

fun{a:t@ype}{sink:t@ype}
list_fold_right_cloptr {f:eff}
  (f: !(a, sink) -<cloptr,f> sink, xs: List a, snk: sink):<f> sink
// end of [list_fold_right_cloptr]
fun{a:t@ype}{sink:t@ype}
list_fold_right_vcloptr {v:view} {f:eff}
  (pf: !v | f: !(!v | a, sink) -<cloptr,f> sink, xs: List a, snk: sink):<f> sink
// end of [list_fold_right_vcloptr]

fun{a:t@ype}{sink:t@ype}
list_fold_right_cloref {f:eff}
  (f: (a, sink) -<cloref,f> sink, xs: List a, snk: sink):<f> sink
// end of [list_fold_right_cloref]

(* ****** ****** *)

fun{a1,a2:t@ype}{sink:t@ype}
list_fold2_right_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff} (
  pf: !v
| f: (!v | a1, a2, sink, !vt) -<fun,f> sink
, xs1: list (a1, n)
, xs2: list (a2, n)
, snk: sink
, env: !vt
) :<f> sink
// end of [list_fold2_right_funenv]

(* ****** ****** *)

fun{a:t@ype}
list_forall_funenv {v:view} {vt:viewtype} {p:eff}
  (pf: !v | xs: List a, p: (!v | a, !vt) -<fun,p> bool, env: !vt):<p> bool
// end of [list_forall_funenv]

fun{a:t@ype}
list_forall_fun {p:eff} (xs: List a, p: a -<p> bool):<p> bool

fun{a:t@ype}
list_forall_vclo {v:view} {p:eff}
  (pf: !v | xs: List a, p: &(!v | a) -<clo,p> bool):<p> bool

fun{a:t@ype}
list_forall_cloptr {p:eff}
  (xs: List a, p: !(a) -<cloptr,p> bool):<p> bool
fun{a:t@ype}
list_forall_vcloptr {v:view} {p:eff}
  (pf: !v | xs: List a, p: !(!v | a) -<cloptr,p> bool):<p> bool

fun{a:t@ype}
list_forall_cloref {p:eff} (xs: List a, p: a -<cloref,p> bool):<p> bool

(*
symintr list_forall
overload list_forall with list_forall_fun
overload list_forall with list_forall_vclo
overload list_forall with list_forall_cloptr
overload list_forall with list_forall_vcloptr
overload list_forall with list_forall_cloref
*)

(* ****** ****** *)

fun{a1,a2:t@ype}
list_forall2_funenv
  {v:view} {vt:viewtype} {n:nat} {p:eff} (
  pf: !v
| xs1: list (a1, n)
, xs2: list (a2, n)
, p: (!v | a1, a2, !vt) -<fun,p> bool
, env: !vt
) :<p> bool
// end of [list_forall2_funenv]

fun{a1,a2:t@ype}
list_forall2_fun {n:nat} {p:eff}
  (xs: list (a1, n), ys: list (a2, n), p: (a1, a2) -<p> bool):<p> bool

fun{a1,a2:t@ype}
list_forall2_vclo {v:view} {n:nat} {p:eff} (
  pf: !v| xs: list (a1, n), ys: list (a2, n), p: &(!v | a1, a2) -<clo,p> bool
) :<p> bool // end of [list_forall2_vclo]

fun{a1,a2:t@ype}
list_forall2_cloptr {n:nat} {p:eff} (
  xs: list (a1, n), ys: list (a2, n), p: !(a1, a2) -<cloptr,p> bool
) :<p> bool // end of [list_forall2_cloptr]
fun{a1,a2:t@ype}
list_forall2_vcloptr {v:view} {n:nat} {p:eff} (
  pf: !v
| xs: list (a1, n), ys: list (a2, n), p: !(!v | a1, a2) -<cloptr,p> bool
) :<p> bool // end of [list_forall2_vcloptr]

fun{a1,a2:t@ype}
list_forall2_cloref {n:nat} {p:eff} (
  xs: list (a1, n), ys: list (a2, n), p: (a1, a2) -<cloref,p> bool
) :<p> bool // end of [list_forall2_cloref]

(*
symintr list_forall2
overload list_forall2 with list_forall2_fun
overload list_forall2 with list_forall2_vclo
overload list_forall2 with list_forall2_cloptr
overload list_forall2 with list_forall2_vcloptr
overload list_forall2 with list_forall2_cloref
*)

(* ****** ****** *)

fun{a:t@ype}
list_foreach_funenv
  {v:view} {vt:viewtype} {f:eff} (
  pf: !v | xs: List a, f: (!v | a, !vt) -<fun,f> void, env: !vt
) :<f> void // end of [list_foreach_funenv]

fun{a:t@ype}
list_foreach_fun {f:eff} (xs: List a, f: a -<fun,f> void):<f> void

fun{a:t@ype}
list_foreach_vclo {v:view} {f:eff}
  (pf: !v | xs: List a, f: &(!v | a) -<clo,f> void):<f> void
// end of [list_foreach_vclo]

fun{a:t@ype}
list_foreach_cloptr {f:eff}
  (xs: List a, f: !(a) -<cloptr,f> void):<f> void
// end of [list_foreach_cloptr]
fun{a:t@ype}
list_foreach_vcloptr {v:view} {f:eff}
  (pf: !v | xs: List a, f: !(!v | a) -<cloptr,f> void):<f> void
// end of [list_foreach_vcloptr]

fun{a:t@ype}
list_foreach_cloref {f:eff} (xs: List a, f: (a) -<cloref,f> void):<f> void

(* ****** ****** *)

fun{a1,a2:t@ype}
list_foreach2_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff} (
  pf: !v
| xs: list (a1, n)
, ys: list (a2, n)
, f: (!v | a1, a2, !vt) -<fun,f> void
, env: !vt
) :<f> void // end of [list_foreach2_funenv]

fun{a1,a2:t@ype}
list_foreach2_fun {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: (a1, a2) -<fun,f> void
) :<f> void // end of [list_foreach2_fun]

fun{a1,a2:t@ype}
list_foreach2_vclo
  {v:view} {n:nat} {f:eff} (
  pf: !v
| xs: list (a1, n), ys: list (a2, n), f: &(!v | a1, a2) -<clo,f> void
) :<f> void // end of [list_foreach2_vclo]

fun{a1,a2:t@ype}
list_foreach2_cloptr
  {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: !(a1, a2) -<cloptr,f> void
) :<f> void // end of [list_foreach2_cloptr]
fun{a1,a2:t@ype}
list_foreach2_vcloptr
  {v:view} {n:nat} {f:eff} (
  pf: !v
| xs: list (a1, n), ys: list (a2, n), f: !(!v | a1, a2) -<cloptr,f> void
) :<f> void // end of [list_foreach2_vcloptr]

fun{a1,a2:t@ype}
list_foreach2_cloref {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: (a1, a2) -<cloref,f> void
) :<f> void // end of [list_foreach2_cloref]

(* ****** ****** *)

fun{a:t@ype}
list_iforeach_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff} (
  pf: !v
| xs: list (a, n), f: (!v | natLt n, a, !vt) -<fun,f> void, env: !vt
) :<f> void // end of [list_iforeach_funenv]

fun{a:t@ype}
list_iforeach_fun {n:nat} {f:eff}
  (xs: list (a, n), f: (natLt n, a) -<fun,f> void):<f> void
// end of [list_iforeach_fun]

fun{a:t@ype}
list_iforeach_vclo {v:view} {n:nat} {f:eff}
  (pf: !v | xs: list (a, n), f: &(!v | natLt n, a) -<clo,f> void):<f> void
// end of [list_iforeach_vclo]

fun{a:t@ype}
list_iforeach_cloptr {n:nat} {f:eff}
  (xs: list (a, n), f: !(natLt n, a) -<cloptr,f> void):<f> void
// end of [list_iforeach_cloptr]
fun{a:t@ype}
list_iforeach_vcloptr {v:view} {n:nat} {f:eff}
  (pf: !v | xs: list (a, n), f: !(!v | natLt n, a) -<cloptr,f> void):<f> void
// end of [list_iforeach_vcloptr]

fun{a:t@ype}
list_iforeach_cloref {n:nat} {f:eff}
  (xs: list (a, n), f: (natLt n, a) -<cloref,f> void):<f> void
// end of [list_iforeach_cloref]

(* ****** ****** *)

fun{a1,a2:t@ype}
list_iforeach2_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff} (
    pf: !v
  | xs: list (a1, n)
  , ys: list (a2, n)
  , f: (!v | natLt n, a1, a2, !vt) -<fun,f> void
  , env: !vt
  ) :<f> void
// end of [list_iforeach2_funenv]

fun{a1,a2:t@ype}
list_iforeach2_fun {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: (natLt n, a1, a2) -<fun,f> void
) :<f> void // end of [list_iforeach2_fun]

fun{a1,a2:t@ype}
list_iforeach2_vclo
  {v:view} {n:nat} {f:eff} (
  pf: !v
| xs: list (a1, n), ys: list (a2, n), f: &(!v | natLt n, a1, a2) -<clo,f> void
) :<f> void // end of [list_iforeach2_vclo]

fun{a1,a2:t@ype}
list_iforeach2_cloptr
  {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: !(natLt n, a1, a2) -<cloptr,f> void
) :<f> void // end of [list_iforeach2_cloptr]
fun{a1,a2:t@ype}
list_iforeach2_vcloptr
  {v:view} {n:nat} {f:eff} (
  pf: !v
| xs: list (a1, n), ys: list (a2, n), f: !(!v | natLt n, a1, a2) -<cloptr,f> void
) :<f> void // end of [list_iforeach2_vcloptr]

fun{a1,a2:t@ype}
list_iforeach2_cloref {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: (natLt n, a1, a2) -<cloref,f> void
) :<f> void // end of [list_iforeach2_cloref]

(* ****** ****** *)

fun{a:t@ype}
list_get_elt_at {n:nat} (xs: list (a, n), i: natLt n):<> a
overload [] with list_get_elt_at

fun{a:t@ype}
list_get_elt_at_exn {n:nat} (xs: list (a, n), i: Nat):<!exn> [n>0] a

fun{a:t@ype}
list_get_elt_at_opt (xs: List a, i: Nat):<> Option_vt a

(* ****** ****** *)

fun{a:t@ype} list_head {n:pos} (xs: list (a, n)):<> a
fun{a:t@ype} list_head_exn {n:nat} (xs: list (a, n)):<!exn> [n>0] a

(* ****** ****** *)

fun{a:t@ype} list_last {n:pos} (xs: list (a, n)):<> a
fun{a:t@ype} list_last_exn {n:nat} (xs: list (a, n)):<!exn> [n>0] a
fun{a:t@ype} list_last_opt {n:nat} (xs: list (a, n)):<> Option_vt (a)

(* ****** ****** *)

fun{a:t@ype} list_length {n:nat} (xs: list (a, n)):<> int n
overload length with list_length

(* ****** ****** *)

//
// HX: please try [list_vt_make_elt]
// fun{a:t@ype} list_make_elt {n:nat} (x: a, n: int n):<> list (a, n)
//

(* ****** ****** *)

fun{a:t@ype}{b:viewt@ype}
list_map_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff}
  (pf: !v | xs: list (a, n), f: (!v | a, !vt) -<fun,f> b, env: !vt)
  :<f> list_vt (b, n)
// end of [list_map_funenv]

fun{a:t@ype}{b:viewt@ype}
list_map_fun {n:nat} {f:eff}
  (xs: list (a, n), f: a -<fun,f> b):<f> list_vt (b, n)

fun{a:t@ype}{b:viewt@ype}
list_map_vclo
  {v:view} {n:nat} {f:eff} (
  pf: !v | xs: list (a, n), f: &(!v | a) -<clo,f> b
) :<f> list_vt (b, n) //  end of [list_map_vclo]

fun{a:t@ype}{b:viewt@ype}
list_map_cloptr {n:nat} {f:eff}
  (xs: list (a, n), f: !(a) -<cloptr,f> b):<f> list_vt (b, n)
fun{a:t@ype}{b:viewt@ype}
list_map_vcloptr
  {v:view} {n:nat} {f:eff} (
  pf: !v | xs: list (a, n), f: !(!v | a) -<cloptr,f> b
) :<f> list_vt (b, n) // end of [list_map_vcloptr]

fun{a:t@ype}{b:viewt@ype}
list_map_cloref {n:nat} {f:eff}
  (xs: list (a, n), f: (a -<cloref,f> b)):<f> list_vt (b, n)
// end of [list_map_cloref]

(*
symintr list_map
overload list_map with list_map_fun
overload list_map with list_map_vclo
overload list_map with list_map_cloptr
overload list_map with list_map_vcloptr
overload list_map with list_map_cloref
*)

(* ****** ****** *)

fun{a1,a2:t@ype}{b:viewt@ype}
list_map2_funenv
  {v:view} {vt:viewtype} {n:nat} {f:eff} (
    pf: !v
  | xs: list (a1, n)
  , ys: list (a2, n)
  , f: (!v | a1, a2, !vt) -<fun,f> b
  , env: !vt
  ) :<f> list_vt (b, n)
// end of [list_map2_funenv]

fun{a1,a2:t@ype}{b:viewt@ype}
list_map2_fun {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: (a1, a2) -<fun,f> b
) :<f> list_vt (b, n) // end of [list_map2_fun]

fun{a1,a2:t@ype}{b:viewt@ype}
list_map2_vclo {v:view} {n:nat} {f:eff} (
  pf: !v
| xs: list (a1, n), ys: list (a2, n), f: &(!v | a1, a2) -<clo,f> b
) :<f> list_vt (b, n) // end of [list_map2_vclo]

fun{a1,a2:t@ype}{b:viewt@ype}
list_map2_cloptr {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: !(a1, a2) -<cloptr,f> b
) :<f> list_vt (b, n) // end of [list_map2_cloptr]
fun{a1,a2:t@ype}{b:viewt@ype}
list_map2_vcloptr {v:view} {n:nat} {f:eff} (
  pf: !v
| xs: list (a1, n), ys: list (a2, n), f: !(!v | a1, a2) -<cloptr,f> b
) :<f> list_vt (b, n) // end of [list_map2_vcloptr]

fun{a1,a2:t@ype}{b:viewt@ype}
list_map2_cloref {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: (a1, a2) -<cloref,f> b
) :<f> list_vt (b, n) // end of [list_map2_cloref]

(*
symintr list_map2
overload list_map2 with list_map2_fun
overload list_map2 with list_map2_vclo
overload list_map2 with list_map2_cloptr
overload list_map2 with list_map2_vcloptr
overload list_map2 with list_map2_cloref
*)

(* ****** ****** *)

fun{a:t@ype}
list_nth {n:nat} (xs: list (a, n), i: natLt n):<> a
fun{a:t@ype}
list_nth_exn {n:nat} (xs: list (a, n), i: Nat):<!exn> [n>0] a
fun{a:t@ype}
list_nth_opt (xs: List a, i: Nat):<> Option_vt a

(* ****** ****** *)

fun{a:t@ype}
list_reverse_append {i,j:nat}
  (xs: list (a, i), ys: list (a, j)):<> list (a, i+j)
// end of [list_reverse_append]

(* ****** ****** *)

fun{a:t@ype}
list_reverse_append1_vt {i,j:nat}
  (xs: list_vt (a, i), ys: list (a, j)):<> list (a, i+j)

fun{a:t@ype}
list_reverse_append2_vt {i,j:nat}
  (xs: list (a, i), ys: list_vt (a, j)):<> list_vt (a, i+j)

(* ****** ****** *)

fun{a:t@ype}
list_reverse {n:nat} (xs: list (a, n)):<> list_vt (a, n)

(* ****** ****** *)

fun{a:t@ype}
list_set_elt_at {n:nat}
  (xs: list (a, n), i: natLt n, x: a):<> list (a, n)

fun{a:t@ype}
list_set_elt_at_exn {n:nat}
  (xs: list (a, n), i: Nat, x: a):<!exn> [n>0] list (a, n)

fun{a:t@ype}
list_set_elt_at_opt {n:nat}
  (x: list (a, n), i: Nat, x: a):<> Option_vt (list (a, n))

(* ****** ****** *)

fun{a:t@ype}
list_split_at {n:int} {i:nat | i <= n}
  (xs: list (a, n), i: int i):<> (list_vt (a, i), list (a, n-i))
// end of [list_split_at]

(* ****** ****** *)

fun{a:t@ype}
list_take {n:int} {i:nat | i <= n}
  (xs: list (a, n), i: int i):<> list_vt (a, i)
// end of [list_take]

fun{a:t@ype}
list_take_exn {n,i:nat}
  (xs: list (a, n), i: int i):<!exn> [i <= n] list_vt (a, i)
// end of [list_take_exn]

(* ****** ****** *)
//
// list_tabulate: please try [list_vt_tabulate]
//
(* ****** ****** *)

fun{a:t@ype}
list_tail {n:pos} (xs: list (a, n)):<> list (a, n-1)

fun{a:t@ype}
list_tail_exn {n:nat} (xs: list (a, n)):<!exn> [n>0] list (a, n-1)

(* ****** ****** *)

fun{a,b:t@ype}
list_zip {n:nat}
  (xs: list (a, n), ys: list (b, n)):<> list_vt (@(a, b), n)
// end of [list_zip]

(* ****** ****** *)

fun{a,a2:t@ype}{b:viewt@ype}
list_zipwth_fun {n:nat} {f:eff}
  (xs: list (a, n), ys: list (a2, n), f: (a, a2) -<fun,f> b):<f> list_vt (b, n)
// end of [list_zipwth_fun]

fun{a1,a2:t@ype}{b:viewt@ype}
list_zipwth_vclo {v:view} {n:nat} {f:eff} (
  pf: !v | xs: list (a1, n), ys: list (a2, n), f: &(!v | a1, a2) -<clo,f> b
) :<f> list_vt (b, n) // end of [list_zipwth_vclo]

fun{a1,a2:t@ype}{b:viewt@ype}
list_zipwth_cloptr {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: !(a1, a2) -<cloptr,f> b
) :<f> list_vt (b, n) // end of [list_zipwth_cloptr]
fun{a1,a2:t@ype}{b:viewt@ype}
list_zipwth_vcloptr {v:view} {n:nat} {f:eff} (
  pf: !v | xs: list (a1, n), ys: list (a2, n), f: !(!v | a1, a2) -<cloptr,f> b
) :<f> list_vt (b, n) // end of [list_zipwth_vcloptr]

fun{a1,a2:t@ype}{b:viewt@ype}
list_zipwth_cloref {n:nat} {f:eff} (
  xs: list (a1, n), ys: list (a2, n), f: (a1, a2) -<cloref,f> b
) :<f> list_vt (b, n) // end of [list_zipwth_cloref]

(* ****** ****** *)

fun{a1,a2:t@ype}
list_unzip {n:nat}
  (xys: list (@(a1, a2), n)):<> (list_vt (a1, n), list_vt (a2, n))
// end of [list_unzip]

(* ****** ****** *)

(*
** HX:
** if the list to be sorted is long (say, containing 10K+ elements),
** please try to use [list_vt_mergesort] instead
*)
fun{a:t@ype}
list_mergesort
  {env:viewtype} {n:nat} (
  xs: list (a, n), cmp: (a, a, !env) -<fun> int, env: !env
) :<> list (a, n) // end of [list_mergesort]

(*
** HX:
** this is not a realistic implementation. please try to use
** [list_vt_quicksort] instead
*)
fun{a:t@ype}
list_quicksort {env:viewtype} {n:nat}
  (xs: list (a, n), cmp: (a, a, !env) -<fun> int, env: !env):<> list (a, n)
// end of [list_quicksort]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [list.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [list.sats] *)
