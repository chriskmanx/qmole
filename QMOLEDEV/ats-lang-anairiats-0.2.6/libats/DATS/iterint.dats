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

//
// some common functions that iterate over natural numbers;
// The code mainly serves as an example for writing iterative loops
// in ATS
//

(* ****** ****** *)

staload "libats/SATS/iterint.sats"

(* ****** ****** *)

implement foreach_funenv
  {v} {vt} {n} {f} (pf | n, f, env) = let
  typedef fun_t = (!v | natLt n, !vt) -<f> void
  fun aux {i:nat | i <= n} .<n-i>.
    (pf: !v | f: fun_t, n: int n, i: int i, env: !vt):<f> void =
    if i < n then (f (pf | i, env); aux (pf | f, n, i+1, env))
    else ()
  // end of [aux]
in
  aux (pf | f, n, 0, env)
end // end of [foreach_funenv]

//

implement
foreach_fun
  {n} {f:eff} (n, f) = let
  typedef fun0_t = (natLt n) -<f> void
  typedef fun1_t = (!unit_v | natLt n, !ptr) -<f> void
  val f = coerce f where {
    extern fun coerce (f: fun0_t):<> fun1_t = "atspre_fun_coerce"
  } // end of [where]
  prval pfu = unit_v ()
  val () = foreach_funenv {unit_v} {ptr} (pfu | n, f, null)
  prval unit_v () = pfu
in
  // nothing
end // end of [foreach_fun]

//

implement
foreach_vclo
  {v} {n} {f:eff} (pf | n, f) = let
  viewtypedef clo_t = (!v | natLt n) -<clo,f> void
  stavar l_f: addr; val p_f: ptr l_f = &f
  viewdef v1 = @(v, clo_t @ l_f)
  prval pf1 = (pf, view@ f)
  fn app (pf1: !v1 | i: natLt n, p_f: !ptr l_f):<f> void = let
    prval (pf, pf_clo) = pf1
    val () = !p_f (pf | i)
    prval () = pf1 := (pf, pf_clo)
  in
    // empty
  end // end of [app]
  val () = foreach_funenv {v1} {ptr l_f} {n} {f} (pf1 | n, app, p_f)
in
  pf := pf1.0; view@ f := pf1.1
end // end of [foreach_vclo]

//

implement
foreach_cloref
  {n} {f:eff} (n, f) = let
  typedef cloref_t = (natLt n) -<cloref,f> void
  fn app (pf: !unit_v | i: natLt n, f: !cloref_t):<f> void = f (i)
  prval pf = unit_v ()
  val () = foreach_funenv {unit_v} {cloref_t} {n} {f} (pf | n, app, f)
  prval unit_v () = pf
in
  // empty
end // end of [foreach_cloref]

(* ****** ****** *)

implement
foreach2_funenv
  {v} {vt} {m,n} {f}
  (pf | m, n, f, env) = let
  typedef fun_t = (!v | natLt m, natLt n, !vt) -<f> void
  fn* aux1 {i:nat | i <= m} .<m-i,n+1>.
    (pf: !v | f: fun_t, m: int m, n: int n, i: int i, env: !vt):<f> void =
    if i < m then aux2 (pf | f, m, n, i, 0, env) else ()
  and aux2 {i,j:nat | i < m; j <= n} .<m-i,n-j>.
    (pf: !v | f: fun_t, m: int m, n: int n, i: int i, j: int j, env: !vt)
    :<f> void =
    if j < n then begin
      (f (pf | i, j, env); aux2 (pf | f, m, n, i, j+1, env))
    end else begin
      aux1 (pf | f, m, n, i+1, env)
    end
in
   aux1 (pf | f, m, n, 0, env)
end // end of [foreach2_funenv]

implement
foreach2_fun
  {m,n} {f:eff} (m, n, f) = let
  typedef fun0_t = (natLt m, natLt n) -<f> void
  typedef fun1_t = (!unit_v | natLt m, natLt n, !ptr) -<f> void
  val f = coerce f where {
    extern fun coerce (f: fun0_t):<> fun1_t = "atspre_fun_coerce"
  } // end of [where]
  prval pfu = unit_v ()
  val () = foreach2_funenv {unit_v} {ptr} (pfu | m, n, f, null)
  prval unit_v () = pfu
in
  // nothing
end // end of [foreach2_fun]

implement
foreach2_vclo
  {v} {m,n} {f:eff} (pf | m, n, f) = let
  viewtypedef clo_t = (!v | natLt m, natLt n) -<clo,f> void
  stavar l_f: addr; val p_f: ptr l_f = &f
  viewdef v1 = @(v, clo_t @ l_f)
  prval pf1 = (pf, view@ f)
  fn app (pf1: !v1 | i: natLt m, j: natLt n, p_f: !ptr l_f):<f> void = let
    prval (pf, pf_clo) = pf1
    val () = !p_f (pf | i, j)
    prval () = pf1 := (pf, pf_clo)
  in
    // empty
  end // end of [app]
  val () = foreach2_funenv {v1} {ptr l_f} {m,n} {f} (pf1 | m, n, app, p_f)
in
  pf := pf1.0; view@ f := pf1.1
end // end of [foreach2_vclo]

implement
foreach2_cloref
  {m,n} {f:eff} (m, n, f) = let
  typedef cloref_t = (natLt m, natLt n) -<cloref,f> void
  fn app (pf: !unit_v | i: natLt m, j: natLt n, f: !cloref_t):<f> void =
    f (i, j)
  prval pf = unit_v ()
  val () = foreach2_funenv {unit_v} {cloref_t} (pf | m, n, app, f)
  prval unit_v () = pf
in
  // empty
end // end of [foreach2_cloref]

(* ****** ****** *)

implement
repeat_funenv
  {v} {vt} {n} {f}
  (pf | n, f, env) = let
  typedef fun_t = (!v | !vt) -<f> void
  fun aux {i:nat | i <= n} .<i>.
    (pf: !v | f: fun_t, i: int i, env: !vt):<f> void =
    if i > 0 then (f (pf | env); aux (pf | f, i-1, env))
    else ()
  // end of [aux]
in
  aux (pf | f, n, env)
end // end of [repeat_funenv]

implement
repeat_fun
  {n} {f:eff} (n, f) = let
  typedef fun0_t = () -<f> void
  typedef fun1_t = (!unit_v | !ptr) -<f> void
  val f = coerce f where {
    extern fun coerce (f: fun0_t):<> fun1_t = "atspre_fun_coerce"
  } // end of [where]
  prval pfu = unit_v ()
  val () = repeat_funenv {unit_v} {ptr} (pfu | n, f, null)
  prval unit_v () = pfu
in
  // nothing
end // end of [repeat_fun]

implement
repeat_vclo
  {v} {n} {f:eff} (pf | n, f) = let
  viewtypedef clo_t = (!v | (*none*)) -<clo,f> void
  stavar l_f: addr; val p_f: ptr l_f = &f
  viewdef v1 = @(v, clo_t @ l_f)
  prval pf1 = (pf, view@ f)
  fn app (pf1: !v1 | p_f: !ptr l_f):<f> void = let
    prval (pf, pf_clo) = pf1
    val () = !p_f (pf | (*none*))
    prval () = pf1 := (pf, pf_clo)
  in
    // empty
  end // end of [app]
  val () = repeat_funenv {v1} {ptr l_f} {n} {f} (pf1 | n, app, p_f)
in
  pf := pf1.0; view@ f := pf1.1
end // end of [repeat_vclo]

implement
repeat_cloref {n} {f} (n, f) = let
  typedef cloref_t = () -<cloref,f> void
  fn app (pf: !unit_v | f: !cloref_t):<f> void = f ()
  prval pf = unit_v ()
  val () = repeat_funenv {unit_v} {cloref_t} (pf | n, app, f)
  prval unit_v () = pf
in
  // empty
end // end of [repeat_cloref]

(* ****** ****** *)

(* end of [iterint.dats] *)
