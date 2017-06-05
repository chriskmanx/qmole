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

// some built-in static constants for linear list operations

(* ****** ****** *)

(*
#define ATS_STALOADFLAG 0 // ...
*)
#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/list_vt.sats"

(* ****** ****** *)

implement list_vt_length_is_nonnegative (xs) = begin
  case+ xs of list_vt_cons _ => fold@ xs | list_vt_nil () => fold@ xs
end // end of [list_vt_length_is_nonnegative]

(* ****** ****** *)

fn list_vt_is_cons {a:viewt@ype} {n:nat} (xs: !list_vt (a, n)): bool (n>0) =
  case+ xs of list_vt_cons _ => (fold@ xs; true) | list_vt_nil () => (fold@ xs; false)
// end of [list_vt_is_cons]

(* ****** ****** *)

implement{a}
list_vt_make_array (A, n) = let
//
  viewtypedef list_vt = [a:viewt@ype;n:nat] list_vt (a, n)
//
  fun loop {n:nat} {l1,l2:addr} .<n>. (
      pf1: !array_v (a, n, l1) >> array_v (a?!, n, l1)
    , pf2: !list_vt? @ l2 >> list_vt (a, n) @ l2
    | p_arr: ptr l1, res: ptr l2, n: size_t n
    ) :<> void =
    if n > 0 then let
      prval (pf11, pf12) = array_v_uncons {a} (pf1)
      val () = !res := list_vt_cons {a} {0} (!p_arr, ?)
      val list_vt_cons (_, !res_next) = !res
      val () = loop 
        (pf12, view@ (!res_next) | p_arr+sizeof<a>, res_next, n-1)
      // end of [val]
      prval () = pf1 := array_v_cons {a?!} (pf11, pf12)
    in
      fold@ (!res)
    end else let
      prval () = array_v_unnil {a} (pf1)
      prval () = pf1 := array_v_nil {a?!} ()
    in
      !res := list_vt_nil {a} ()
    end // end of [if]
  // end of [loop]
  var res: list_vt?
  val () = loop (view@ A, view@ res | &A, &res, n)
in
  res
end // end of [list_vt_make_arrsz]

(* ****** ****** *)

(*
//
// HX: this is a different style: looping backward
//
implement{a}
list_vt_make_array (A, n) = let
// the loop goes from the end of an array to its beginning
fun loop {i,j:nat} {l:addr} {ofs:int} .<i>.
  (pf_mul: MUL (i, sizeof a, ofs), pf_arr: array_v (a, i, l) |
   i: size_t i, p: ptr (l+ofs), res: list_vt (a, j))
  :<> (array_v (a?!, i, l) | list_vt (a, i+j)) =
  if i > 0 then let
    prval pf1_mul = mul_add_const {~1} (pf_mul)
    prval (pf1_arr, pf_lst) = array_v_unextend {a} (pf_mul, pf_arr)
    val p1 = p - sizeof<a>
    val x = ptr_get_vt<a> (pf_lst | p1)
    val (pf1_arr | res) = loop (pf1_mul, pf1_arr | i-1, p1, list_vt_cons (x, res))
    prval pf_arr = array_v_extend {a?!} (pf1_mul, pf1_arr, pf_lst)
  in
    (pf_arr | res)    
  end else let
    prval () = array_v_unnil {a} (pf_arr)
  in
    (array_v_nil {a?!} () | res)
  end // end of [if]
// end of [loop]
val (pf_mul | ofs) = mul2_size1_size1 (n, sizeof<a>)
val (pf_arr | res) = loop (pf_mul, view@ A | n, &A+ofs, list_vt_nil ())
//
prval () = view@ A := pf_arr
//
in
  res
end // end of [list_vt_of_arraysize]
*)

(* ****** ****** *)

implement{a}
list_vt_of_arraysize (arrsz) = let
  val (pf_gc, pf_arr | p_arr, asz) = arrsz
  val res = list_vt_make_array (!p_arr, asz)
  val () = array_ptr_free {a} (pf_gc, pf_arr | p_arr)
in
  res
end // end of [list_vt_of_arraysize]

(* ****** ****** *)

local

staload UN = "prelude/SATS/unsafe.sats"

in // in of [local]

implement{a}
list_vt_copy {n} (xs0) = list_copy ($UN.castvwtp1 {list(a,n)} (xs0))

end // end of [local]

(* ****** ****** *)

implement{a}
list_vt_free (xs0) = let
  fun loop {n:nat} .<n>. (xs: list_vt (a, n)):<> void =
    case+ xs of ~list_vt_cons (_, xs) => loop xs | ~list_vt_nil () => ()
  // end of [loop]
in
  loop (xs0)
end // end of [list_vt_free]

implement{a}
list_vt_free_fun (xs0, f) = let
  fun loop {n:nat} .<n>.
    (xs: list_vt (a, n)):<cloref> void =
    case+ xs of
    | list_vt_cons (!p_x, xs1) =>
        (f (!p_x); free@ {a} {0} (xs); loop (xs1))
    | ~list_vt_nil () => ()
  // end of [loop]
in
  loop (xs0)
end // end of [list_vt_free_fun]

(* ****** ****** *)

implement{a}
list_vt_length (xs0) = loop (xs0, 0) where {
  fun loop {i,j:nat} .<i>.
    (xs: !list_vt (a, i), j: int j):<> int (i+j) = begin
    case+ xs of
    | list_vt_cons (_, !p_xs1) =>
        let val n = loop (!p_xs1, j+1) in fold@ xs; n end
    | list_vt_nil () => (fold@ xs; j)
  end // end of [loop]
} // end of [list_vt_length]

(* ****** ****** *)

implement{a}
list_vt_make_elt (x0, n) = let
  fun loop {i,j:nat} .<i>.
    (x0: a, i: int i, res: list_vt (a, j)):<> list_vt (a, i+j) =
    if i > 0 then loop (x0, i-1, list_vt_cons (x0, res)) else res
in
  loop (x0, n, list_vt_nil)
end // end of [list_make_elt]

(* ****** ****** *)

implement{a}
list_vt_append (xs0, ys0) = let
  var xs0 = xs0
  fun{a:viewt@ype} loop {m,n:nat} .<m>.
    (xs0: &list_vt (a, m) >> list_vt (a, m+n), ys0: list_vt (a, n))
    :<> void = begin case+ xs0 of
    | list_vt_cons (_, !p_xs) => (loop (!p_xs, ys0); fold@ xs0)
    | ~list_vt_nil () => (xs0 := ys0)
  end // end of [loop]
in
  loop (xs0, ys0); xs0
end // end of [list_vt_append]

(* ****** ****** *)

implement{a}
list_vt_reverse (xs) =
  list_vt_reverse_append<a> (xs, list_vt_nil)
// end of [list_vt_reverse]

implement{a}
list_vt_reverse_append (xs, ys) = let
  fun revapp {m,n:nat} .<m>. (
    xs: list_vt (a, m), ys: list_vt (a, n)
  ) :<> list_vt (a, m+n) =
    case+ xs of
    | list_vt_cons (_, !p_xs1) => let
        val xs1 = !p_xs1
      in
        !p_xs1 := ys; fold@ xs; revapp (xs1, xs)
      end // end of [val]
    | ~list_vt_nil () => ys
  // end of [revapp]
in
  revapp (xs, ys)
end // end of [list_vt_reverse_append]

(* ****** ****** *)

implement{a}
list_vt_concat (xss) = let
  fun aux {n:nat} .<n>. (
    xs0: List_vt a, xss: list_vt (List_vt a, n)
  ) :<> List_vt a =
    case+ xss of
    | ~list_vt_cons (xs, xss) => list_vt_append (xs0, aux (xs, xss))
    | ~list_vt_nil () => xs0
  // end of [aux]
in
  case+ xss of
  | ~list_vt_cons (xs0, xss) => aux (xs0, xss)
  | ~list_vt_nil () => list_vt_nil ()
end // end of [list_vt_concat]

(* ****** ****** *)

implement{a}
list_vt_tabulate_funenv
  {v} {vt} {n} {f} (pf | f, n, env) = let
  var res: List_vt a // uninitialized
  fun loop {i:nat | i <= n} .<n-i>. (
      pf: !v
    | n: int n, i: int i, f: (!v | natLt n, !vt) -<f> a, env: !vt
    , res: &(List_vt a)? >> list_vt (a, n-i)
    ) :<f> void =
    if i < n then let
      val () = (res := list_vt_cons {a} {0} (f (pf | i, env), ?))
      val+ list_vt_cons (_, !p) = res
    in
      loop (pf | n, i+1, f, env, !p); fold@ res
    end else begin
      res := list_vt_nil ()
    end // end of [if]
  // end of [loop]
in
  loop (pf | n, 0, f, env, res); res
end // end of [list_vt_tabulate_funenv]

implement{a}
list_vt_tabulate_fun {n} {f:eff} (f, n) = let
  val f = coerce (f) where { extern castfn
    coerce (f: natLt n -<f> a):<> (!unit_v | natLt n, !ptr) -<f> a
  } // end of [where]
  prval pf = unit_v ()
  val ans = list_vt_tabulate_funenv<a> {..} {ptr} (pf | f, n, null)
  prval unit_v () = pf
in
  ans
end // end of [list_vt_tabulate_fun]

implement{a}
list_vt_tabulate_vclo
  {v} {n} {f:eff}
  (pf1 | f, n) = ans where {
  typedef clo_t = (!v | natLt n) -<clo,f> a
  stavar l_f: addr; val p_f: ptr l_f = &f
  viewdef V = (v, clo_t @ l_f)
  fn app (
    pf: !V | i: natLt n, p_f: !ptr l_f
  ) :<f> a = let
    prval pfclo = pf.1
    val x = !p_f (pf.0 | i)
    prval () = pf.1 := pfclo
  in
    x
  end // end of [app]
  prval pf = (pf1, view@ f)
  val ans = list_vt_tabulate_funenv<a> {V} {ptr l_f} (pf | app, n, p_f)
  prval () = pf1 := pf.0
  prval () = view@ f := pf.1
} // end of [list_vt_tabulate_vclo]

implement{a}
list_vt_tabulate_cloptr
  {n} {f:eff} (f, n) = res where {
//
  viewtypedef cloptr0_t = (natLt n) -<cloptr,f> a
  viewtypedef cloptr1_t = (!unit_v | natLt n) -<cloptr,f> a
//
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr0_t >> cloptr1_t): void
  } // end of [val]
  prval pfu = unit_v ()
  val res = list_vt_tabulate_vcloptr<a> {unit_v} (pfu | f, n)
  prval unit_v () = pfu
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr1_t >> cloptr0_t): void
  } // end of [val]
} (* end of [list_app_cloptr] *)
implement{a}
list_vt_tabulate_vcloptr
  {v} {n} {f:eff} (pf | f, n) = let
  viewtypedef cloptr_t = (!v | natLt n) -<cloptr,f> a
  fn app (pf: !v | i: natLt n, f: !cloptr_t):<f> a = f (pf | i)
in
  list_vt_tabulate_funenv<a> {v} {cloptr_t} (pf | app, n, f)
end // end of [list_app_vcloptr]

(* ****** ****** *)

implement{a}
list_vt_foreach_funenv
  {v} {vt} {n} {f} (pf | xs0, f, env) = let
  viewtypedef fun_t = (!v | &a, !vt) -<f> void
  fun loop {i:nat} .<i>.
    (pf: !v | xs0: !list_vt (a, i), f: !fun_t, env: !vt):<f> void =
    case+ xs0 of
    | list_vt_cons (!p_x, !p_xs) => begin
        f (pf | !p_x, env); loop (pf | !p_xs, f, env); fold@ xs0
      end // end of [val]
    | list_vt_nil () => (fold@ xs0)
  // end of [loop]
in
  loop (pf | xs0, f, env)
end // end of [list_vt_foreach_funenv]

implement{a}
list_vt_foreach_fun
  {n} {f:eff} (xs, f) = let
//
  typedef fun0_t = (&a) -<fun,f> void
  typedef fun1_t = (!unit_v | &a, !ptr) -<fun,f> void
//
  val f = __cast (f) where { extern castfn __cast (f: fun0_t):<> fun1_t }
  prval pf = unit_v ()
  val () = list_vt_foreach_funenv<a> {unit_v} {ptr} (pf | xs, f, null)
  prval unit_v () = pf
in
  // nothing
end // end of [list_vt_foreach_fun]

implement{a}
list_vt_foreach_vclo
  {v} {n} {f:eff}
  (pf1 | xs, f) = () where {
  typedef clo_t = (!v | &a) -<clo,f> void
  stavar l_f: addr; val p_f: ptr l_f = &f
  viewdef V = (v, clo_t @ l_f)
  fn app (pf: !V | x: &a, p_f: !ptr l_f):<f> void = () where {
    prval (pf1, pf2) = pf
    val () = !p_f (pf1 | x)
    prval () = pf := (pf1, pf2)
  } // end of [val]
  prval pf = (pf1, view@ f)
  val () = list_vt_foreach_funenv<a> {V} {ptr l_f} (pf | xs, app, p_f)
  prval () = pf1 := pf.0 and () = view@ f := pf.1
} // end of [list_vt_foreach_vclo]

implement{a}
list_vt_foreach_cloptr
  {n} {f:eff} (xs, f) = let
//
  viewtypedef cloptr0_t = (&a) -<cloptr,f> void
  viewtypedef cloptr1_t = (!unit_v | &a) -<cloptr,f> void
//
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr0_t >> cloptr1_t): void
  } // end of [val]
  prval pfu = unit_v ()
  val () = list_vt_foreach_vcloptr<a> {unit_v} (pfu | xs, f)
  prval unit_v () = pfu
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr1_t >> cloptr0_t): void
  } // end of [val]
in
  // nothing
end // end of [list_vt_foreach_cloptr]
implement{a}
list_vt_foreach_vcloptr
  {v} {n} {f:eff} (pf | xs, f) = let
  viewtypedef cloptr_t = (!v | &a) -<cloptr,f> void
  fn app (pf: !v | x: &a, f: !cloptr_t):<f> void = f (pf | x)
  val () = list_vt_foreach_funenv<a> {v} {cloptr_t} (pf | xs, app, f)
in
  // empty
end // end of [list_vt_foreach_vcloptr]

(* ****** ****** *)

implement{a}
list_vt_iforeach_funenv
  {v} {vt} {n} {f} (pf | xs0, f, env) = let
  viewtypedef fun_t = (!v | natLt n, &a, !vt) -<f> void
  fun loop {i:nat | i <= n} .<n-i>.
    (pf: !v | i: int i, xs0: !list_vt (a, n-i), f: !fun_t, env: !vt):<f> void =
    case+ xs0 of
    | list_vt_cons (!p_x, !p_xs) => begin
        f (pf | i, !p_x, env); loop (pf | i+1, !p_xs, f, env); fold@ xs0
      end // end of [val]
    | list_vt_nil () => (fold@ xs0)
  // end of [loop]
in
  loop (pf | 0, xs0, f, env)
end // end of [list_vt_iforeach_funenv]

implement{a}
list_vt_iforeach_fun
  {n} {f:eff} (xs, f) = let
//
  typedef fun0_t = (natLt n, &a) -<fun,f> void
  typedef fun1_t = (!unit_v | natLt n, &a, !ptr) -<fun,f> void
//
  val f = __cast (f) where { extern castfn __cast (f: fun0_t):<> fun1_t }
  prval pf = unit_v ()
  val () = list_vt_iforeach_funenv<a> {unit_v} {ptr} (pf | xs, f, null)
  prval unit_v () = pf
in
  // nothing
end // end of [list_vt_iforeach_fun]

implement{a}
list_vt_iforeach_vclo
  {v} {n} {f:eff} (pf1 | xs, f) = let
  typedef clo_t = (!v | natLt n, &a) -<clo,f> void
  stavar l_f: addr; val p_f: ptr l_f = &f
  viewdef V = (v, clo_t @ l_f)
  fn app (pf: !V | i: natLt n, x: &a, p_f: !ptr l_f):<f> void = () where {
    prval (pf1, pf2) = pf
    val () = !p_f (pf1 | i, x)
    prval () = pf := (pf1, pf2)
  } // end of [val]
  prval pf = (pf1, view@ f)
  val () = list_vt_iforeach_funenv<a> {V} {ptr l_f} (pf | xs, app, p_f)
  prval () = pf1 := pf.0 and () = view@ f := pf.1
in
  // empty
end // end of [list_vt_iforeach_vclo]

implement{a}
list_vt_iforeach_cloptr
  {n} {f:eff} (xs, f) = let
//
  viewtypedef cloptr0_t = (natLt n, &a) -<cloptr,f> void
  viewtypedef cloptr1_t = (!unit_v | natLt n, &a) -<cloptr,f> void
//
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr0_t >> cloptr1_t): void
  } // end of [val]
  prval pfu = unit_v ()
  val () = list_vt_iforeach_vcloptr<a> {unit_v} (pfu | xs, f)
  prval unit_v () = pfu
  prval () = __assert(f) where {
    extern prfun __assert (f: !cloptr1_t >> cloptr0_t): void
  } // end of [val]
in
  // nothing
end // end of [list_vt_iforeach_cloptr]
implement{a}
list_vt_iforeach_vcloptr
  {v} {n} {f:eff} (pf | xs, f) = let
  viewtypedef cloptr_t = (!v | natLt n, &a) -<cloptr,f> void
  fn app (pf: !v | i: natLt n, x: &a, f: !cloptr_t):<f> void = f (pf | i, x)
  val () = list_vt_iforeach_funenv<a> {v} {cloptr_t} (pf | xs, app, f)
in
  // empty
end // end of [list_vt_iforeach_vcloptr]

(* ****** ****** *)

local

staload STDLIB = "libc/SATS/stdlib.sats"
typedef cmp (a:viewt@ype) = (&a, &a) -<clo> Sgn

in // in of [local]

implement{a}
list_vt_mergesort
  {n} (xs, cmp) = let
//
fun split {n,n1:nat | n >= n1} .<n1>. (
    xs: &list_vt (a, n) >> list_vt (a, n1)
  , n1: int n1, res: &List_vt a? >> list_vt (a, n-n1)
  ) :<> void =
  if n1 > 0 then let
    val+ list_vt_cons (_, !p_xs1) = xs
    val () = split (!p_xs1, n1-1, res)
  in
    fold@ (xs)
  end else let
    val () = res := xs
    val () = xs := list_vt_nil ()
  in
    // nothing
  end // end of [if]
// end of [split]
fun merge {n1,n2:nat} .<n1+n2>. (
    xs1: list_vt (a, n1)
  , xs2: list_vt (a, n2)
  , cmp: &cmp a
  , res: &List_vt a? >> list_vt (a, n1+n2)
  ) :<> void =
  case+ xs1 of
  | list_vt_cons (!p_x1, !p_xs11) => (
    case+ xs2 of
    | list_vt_cons (!p_x2, !p_xs21) => let
        val sgn = cmp (!p_x1, !p_x2)
      in
        if sgn <= 0 then let
          prval () = fold@ xs2
          val () = merge (!p_xs11, xs2, cmp, !p_xs11)
        in
          fold@ (xs1); res := xs1
        end else let
          prval () = fold@ xs1
          val () = merge (xs1, !p_xs21, cmp, !p_xs21)
        in
          fold@ (xs2); res := xs2
        end // end of [if]
      end // end of [list_vt_cons]
    | ~list_vt_nil () => (fold@ (xs1); res := xs1)
    ) // end of [list_vt_cons]
  | ~list_vt_nil () => (res := xs2)
// end of [merge]
//
val n = list_vt_length<a> (xs)
//
in
//
if n >= 2 then let
  val+ list_vt_cons (_, !p_xs1) = xs
  var res: List_vt a? // uninitialized
  val () = split (!p_xs1, (n-1)/2, res)
  prval () = fold@ (xs)
  val xs1 = list_vt_mergesort<a> (xs, cmp)
  val xs2 = list_vt_mergesort<a> (res, cmp)
  val () = merge (xs1, xs2, cmp, res)
in
  res
end else xs // end of [if]
//
end // end of [list_vt_mergesort]

implement{a}
list_vt_quicksort {n} (xs, cmp) = let
//
// HX-2010:
// I use an array to do quicksorting and then copy the sorted
// array back in to the list. Note that this style of hacking
// should probably not imitated :)
//
  abst@ype a1 = a?
  val _ = __cast (xs) where {
    extern castfn __cast
      (xs: !list_vt (a, n) >> list_vt (a1, n)):<> ptr
  } // end of [val]
  val cmp = __cast (cmp) where {
    extern castfn
      __cast (cmp: (&a, &a) -<fun> Sgn):<> (&a1, &a1) -<fun> Sgn
  } // end of [val]
  val asz = size1_of_int1 (list_vt_length xs)
  val (
    pf_gc, pf_arr | p_arr
  ) = array_ptr_alloc_tsz {a1} (asz, sizeof<a1>)
  val () = array_ptr_initialize_lst<a1> (!p_arr, __cast xs) where {
    extern castfn __cast
      (xs: !list_vt (a1, n) >> list_vt (a1?, n)):<> list (a1, n) // good hacking :)  
  } // end of [val]
  val () = $effmask_all ($STDLIB.qsort (!p_arr, asz, sizeof<a1>, cmp))
  val () = loop (pf_arr | p_arr, xs) where {
    fun loop {n:nat} {l:addr} .<n>. (
        pf_arr: !array_v (a1, n, l) >> array_v (a1?, n, l)
      | p_arr: ptr l, xs: !list_vt (a1?, n) >> list_vt (a1, n)
      ) :<> void =
      case+ xs of
      | list_vt_cons (!p_x1, !p_xs1) => let
          prval (pf1_elt, pf2_arr) = array_v_uncons {a1} (pf_arr)
          val () = !p_x1 := !p_arr
          val () = loop (pf2_arr | p_arr + sizeof<a1>, !p_xs1)
          prval () = pf_arr := array_v_cons {a1?} (pf1_elt, pf2_arr)
        in
          fold@ (xs)
        end // end of [list_vt_cons]
      | list_vt_nil () => let
          prval () = array_v_unnil {a1} (pf_arr)
          prval () = pf_arr := array_v_nil {a1?} ()
        in
          fold@ (xs)
        end // end of [list_vt_nil]
    // end of [loop]
  } // end of [val]
  val () = array_ptr_free {a1} (pf_gc, pf_arr | p_arr)
  val _ = __cast (xs) where {
    extern castfn __cast (xs: !list_vt (a1, n) >> list_vt (a, n)):<> ptr
  } // end of val]
in
  // empty
end // end of [list_vt_quicksort]

end // end of [local]

(* ****** ****** *)

(* end of [list_vt.dats] *)
