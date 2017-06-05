(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time
#define ATS_DYNLOADFLAG 0 // there is no need for dynloading at run-time

(* ****** ****** *)

staload "libats/ngc/SATS/slist.sats"

(* ****** ****** *)

extern
castfn p2p {l:addr} (p: !ptr l):<> ptr l

(* ****** ****** *)

implement
slseg_v_append
  {a} (pf1seg, pf2seg) = let
  prfun append
    {n1,n2:nat}
    {la,lm,lz:addr} .<n1>. (
      pf1seg: slseg_v (a, n1, la, lm)
    , pf2seg: slseg_v (a, n2, lm, lz)
    ) :<prf> slseg_v (a, n1+n2, la, lz) = begin
    case+ pf1seg of
    | slseg_v_cons (pf1nod, pf1seg) =>
        slseg_v_cons (pf1nod, slseg_v_append (pf1seg, pf2seg))
      // end of [slseg_v_cons]
    | slseg_v_nil () => pf2seg
  end // end of [append]
in
  append (pf1seg, pf2seg)
end // end of [slseg_v_append]

(* ****** ****** *)

implement
slseg_v_extend
  {a} (pfseg, pfnod) = let
  prfun extend
    {n:nat} {la,ly,lz:addr} .<n>. (
      pfseg: slseg_v (a, n, la, ly)
    , pfnod: slnode_v (a, ly, lz)
    ) :<prf> slseg_v (a, n+1, la, lz) =
    case+ pfseg of
    | slseg_v_cons (pf1nod, pf1seg) =>
        slseg_v_cons (pf1nod, slseg_v_extend (pf1seg, pfnod))
      // end of [slseg_v_cons]
    | slseg_v_nil () => let
        prval () = slnode_ptr_is_gtz (pfnod)
      in
        slseg_v_cons (pfnod, slseg_v_nil ())
      end // end of [slseg_v_nil]
  // end of [extend]
in
  extend (pfseg, pfnod)
end // end of [slseg_v_extend]

(* ****** ****** *)

implement{a}
slist_split_at
  (pflst | pa, i) =
  if i > 0 then let
    prval slseg_v_cons (pfnod, pf1lst) = pflst
    val pb = slnode_get_next (pfnod | pa)
    val (pfres1, pfres2 | pm) = slist_split_at (pf1lst | pb, i-1)
  in
    (slseg_v_cons (pfnod, pfres1), pfres2 | pm)
  end else (
    slseg_v_nil (), pflst | pa
  ) // end of [if]
// end of [slist_split_at]

(* ****** ****** *)

implement{a}
slist_nil () =
  slist_encode (slseg_v_nil | null)
// end of [slist_nil]

implement{a}
slist_cons (
  pfnod | p_nod, xs
) = let
  prval () = slnode_ptr_is_gtz (pfnod)
  val (pflst | p_xs) = slist_decode (xs)
  val () = slnode_set_next (pfnod | p_nod, p_xs)
  prval pflst = slseg_v_cons (pfnod, pflst)
in
  slist_encode (pflst | p_nod)
end // end of [slist_cons]

(* ****** ****** *)

implement{a}
slist_free (xs) = let
  fun free {n:nat} {la:addr} .<n>. (
    pfseg: slist_v (a, n, la) | p: ptr la
  ) :<> void =
    if slist_ptr_is_cons (pfseg | p) then let
      prval slseg_v_cons (pfnod, pf1seg) = pfseg
      val p1 = slnode_get_next<a> (pfnod | p)
      val () = slnode_free<a> (pfnod | p)
    in
      free (pf1seg | p1)
    end else begin
      let prval slseg_v_nil () = pfseg in () end
    end // end of [if]
  // end of [free]
  val (pfseg | p_xs) = slist_decode (xs)
in
  free (pfseg | p_xs)
end (* end of [slist_free] *)

(* ****** ****** *)

implement{a}
slist_free_funenv
  {v} {vt} (pfv | xs, f, env) = let
  fun free
    {n:nat} {la:addr} .<n>. (
    pfv: !v, pfseg: slist_v (a, n, la)
  | p: ptr la, f: (!v | &a >> a?, !vt) -<fun> void, env: !vt
  ) :<> void =
    if slist_ptr_is_cons (pfseg | p) then let
      prval slseg_v_cons (pfnod, pf1seg) = pfseg
      prval (pfat, fpfnod) = slnode_v_takeout_val {a} (pfnod)
      val () = f (pfv | !p, env)
      prval () = pfnod := fpfnod {a?} (pfat)
      val p1 = slnode_get_next<a?> (pfnod | p)
      val () = slnode_free<a> (pfnod | p)
    in
      free (pfv, pf1seg | p1, f, env)
    end else begin
      let prval slseg_v_nil () = pfseg in () end
    end // end of [if]
  // end of [free]
  val (pfseg | p_xs) = slist_decode (xs)
in
  free (pfv, pfseg | p_xs, f, env)
end (* end of [slist_free_funenv] *)

implement{a}
slist_free_fun
  (xs, f) = let
  val f = coerce (f) where {
    extern castfn coerce
      (f: (&a >> a?) -<fun> void):<> (!unit_v | &a >> a?, !ptr) -<fun> void
  } // end of [where]
  prval pf = unit_v ()
  val () = slist_free_funenv<a> {..} {ptr} (pf | xs, f, null)
  prval unit_v () = pf
in
  ()
end // end of [slist_free_fun]

implement{a}
slist_free_vclo
  {v} (pf1 | xs, f) = let
  viewtypedef clo_t = (!v | &a >> a?) -<clo> void
  stavar l_f: addr; val p_f: ptr l_f = &f
  viewdef V = (v, clo_t @ l_f)
  prval pf = (pf1, view@ f)
  fn app (pf: !V | x: &a >> a?, p_f: !ptr l_f):<> void = let
    prval (pf1, pf2) = pf
    val () = !p_f (pf1 | x)
    prval () = pf := (pf1, pf2)
  in
    // nothing
  end // end of [app]
  val () = slist_free_funenv<a> {V} {ptr l_f} (pf | xs, app, p_f)
  prval () = pf1 := pf.0
  prval () = view@ f := pf.1
in
  ()
end // end of [slist_free_vclo]

(* ****** ****** *)

implement{a}
slist_length (xs) = let
  fun loop
    {la:addr}
    {n,k:nat} .<n>. (
    pfseg: !slist_v (a, n, la)
  | p: !ptr la, k: size_t (k)
  ) :<> size_t (n+k) =
  if slist_ptr_is_cons (pfseg | p) then let
    prval slseg_v_cons (pfnod, pf1seg) = pfseg
    val p1 = slnode_get_next<a> (pfnod | p)
    val res = loop (pf1seg | p1, k + 1)
    prval () = pfseg := slseg_v_cons (pfnod, pf1seg)
  in
    res
  end else k // end of [if]
//
  prval (
    pfseg | ()
  ) = slist_unfold (xs)
  val p_xs = p2p (xs)
  val res = loop (pfseg | p_xs, 0)
  prval () = slist_fold (pfseg | xs)
//
in
  res
end // end of [slist_length]

(* ****** ****** *)

implement{a}
slist_append (xs, ys) = let
//
  fun loop {m,n:nat}
    {la1,lb1:addr} {la2:addr} .<m>. (
    pfnod: !slnode_v (a, la1, lb1) >> slnode_v (a, la1, lb1)
  , pf1lst: slist_v (a, m, lb1)
  , pf2lst: slist_v (a, n, la2)
  | p1: ptr la1, p2: ptr la2
  ) :<> #[lb1:addr] (
    slist_v (a, m+n, lb1) | void
  ) = let
    val p11 = slnode_get_next<a> (pfnod | p1)
  in
    if slist_ptr_is_cons (pf1lst | p11) then let
      prval slseg_v_cons (pf1nod, pf1lst1) = pf1lst
      val (pflst1 | ()) = loop (pf1nod, pf1lst1, pf2lst | p11, p2)
    in
      (slseg_v_cons (pf1nod, pflst1) | ())
    end else let
      prval slseg_v_nil () = pf1lst
      val () = slnode_set_next (pfnod | p1, p2)
    in
      (pf2lst | ())
    end (* end of [if] *)
  end // end of [loop]
//
  val (pf1lst | p_xs) = slist_decode (xs)
  val (pf2lst | p_ys) = slist_decode (ys)
//
in
  if slist_ptr_is_cons (pf1lst | p_xs) then let
    prval slseg_v_cons (pf1nod, pf1lst1) = pf1lst
    val (pflst1 | ()) = loop (pf1nod, pf1lst1, pf2lst | p_xs, p_ys)
    val xs = slist_encode {a} (slseg_v_cons (pf1nod, pflst1) | p_xs)
  in
    xs
  end else let
    prval slseg_v_nil () = pf1lst
    val ys = slist_encode {a} (pf2lst | p_ys)
  in
    ys
  end (* end of [if] *)
end // end of [slist_append]

(* ****** ****** *)

implement{a}
slist_reverse (xs) = let
  fun reverse {i,j:nat} .<i>. (
    xs: slist (a, i), ys: slist (a, j)
  ) :<> slist (a, i+j) = let
    val (pflst | p_xs) = slist_decode (xs)
  in
    if slist_ptr_is_cons (pflst | p_xs) then let
      prval slseg_v_cons (pfnod, pf1lst) = pflst
      val p_xs1 = slnode_get_next<a> (pfnod | p_xs)
      val xs1 = slist_encode {a} (pf1lst | p_xs1)
      val ys = slist_cons<a> (pfnod | p_xs, ys)
    in
      reverse (xs1, ys)
    end else let
      prval slseg_v_nil () = pflst
    in
      ys
    end (* end of [if] *)
  end // end of [reverse]
in
  reverse (xs, slist_nil<a> ())
end // end of [slist_reverse]

(* ****** ****** *)

implement{a}
slist_foreach_funenv
  {v} {vt}
  (pfv | xs, f, env) = let
//
  fun loop {la,lz:addr} {n:nat} .<n>. (
    pfv: !v, pfseg: !slist_v (a, n, la)
  | p: !ptr la, f: (!v | &a, !vt) -<fun> void, env: !vt
  ) :<> void =
    if slist_ptr_is_cons (pfseg | p) then let
      prval slseg_v_cons (pfnod, pf1seg) = pfseg
      prval (pfat, fpfnod) = slnode_v_takeout_val {a} (pfnod)
      val () = f (pfv | !p, env)
      prval () = pfnod := fpfnod (pfat)
      val p1 = slnode_get_next (pfnod | p)
      val () = loop (pfv, pf1seg | p1, f, env)
    in
      pfseg := slseg_v_cons (pfnod, pf1seg)
    end // end of [if]
  (* end of [loop] *)
//
  prval (
    pfseg | p_xs
  ) = slist_unfold (xs)
  val p_xs = p2p (xs)
  val () = loop (pfv, pfseg | p_xs, f, env)
  prval () = slist_fold (pfseg | xs)
//
in
  // nothing
end // end of [slist_foreach_funenv]

(* ****** ****** *)

implement{a}
slist_foreach_fun (xs, f) = let
  val f = coerce (f) where { extern castfn
    coerce (f: (&a) -<> void):<> (!unit_v | &a, !ptr) -<> void
  } // end of [where]
  prval pf = unit_v ()
  val () = slist_foreach_funenv<a> {unit_v} {ptr} (pf | xs, f, null)
  prval unit_v () = pf
in
  // nothing
end // end of [slist_foreach_fun]

(* ****** ****** *)

implement{a}
slist_foreach_vclo
  {v} (pfv | xs, f) = let
//
  stavar l_f: addr
  val p_f: ptr l_f = &f
//
  typedef clo_t = (!v | &a) -<clo> void
  typedef vt = ptr l_f
  viewdef V = (v, clo_t @ l_f)
//
  fn app (
    pf: !V | x: &a, p_f: !vt
  ) :<> void = let
    prval pf1 = pf.1
    val () = !p_f (pf.0 | x)
    prval () = pf.1 := pf1
  in
    // nothing
  end // end of [app]
//
  prval pfV = (pfv, view@ f)
  val () = slist_foreach_funenv<a> {V} {vt} (pfV | xs, app, p_f)
  prval () = pfv := pfV.0
  prval () = view@ f := pfV.1
//
in
  // nothing
end // end of [slist_foreach_vclo]
  
(* ****** ****** *)

(* end of [slist.dats] *)
