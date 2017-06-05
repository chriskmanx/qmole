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

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/array.sats"

(* ****** ****** *)
//
// HX: code implementing prfun functions manipulating array views
//
(* ****** ****** *)

implement
array_v_sing {a}
  (pf) = array_v_cons {a} (pf, array_v_nil ())
// end of [array_v_sing]

implement
array_v_unsing {a}
  (pf) = pf1 where {
  prval (pf1, pf2) = array_v_uncons {a} (pf)
  prval () = array_v_unnil (pf2)
} // end of [array_v_unsing]

(* ****** ****** *)

implement
array_v_split {a}
  (pf_mul, pf_arr) = split (pf_mul, pf_arr) where {
  prfun split
    {n,i:nat | i <= n} {l:addr} {ofs:int} .<i>.
    (pf_mul: MUL (i, sizeof a, ofs), pf_arr: array_v (a, n, l))
    : @(array_v (a, i, l), array_v (a, n-i, l+ofs)) =
    sif i > 0 then let
      prval @(pf1_elt, pf2_arr) = array_v_uncons {a} (pf_arr)
      // pf1_mul : MUL (i-1, sizeof a, ofs - sizeof a)
      prval pf1_mul = mul_add_const {~1} {i, sizeof a} (pf_mul)
      prval @(pf1_arr_res, pf2_arr_res) = split {n-1,i-1} (pf1_mul, pf2_arr)
    in
      @(array_v_cons {a} (pf1_elt, pf1_arr_res), pf2_arr_res)
    end else let
      prval MULbas () = pf_mul
    in
      (array_v_nil {a} {l} (), pf_arr)
    end // end of [sif]
} // end of [array_v_split]

(* ****** ****** *)

implement
array_v_unsplit {a}
  (pf_mul, pf1_arr, pf2_arr) = unsplit (pf_mul, pf1_arr, pf2_arr) where {
//
  extern praxi lemma {n:int} {l:addr} (pf: !array_v (a, n, l)): [n>=0] void
  prval () = lemma (pf1_arr) and () = lemma (pf2_arr) 
//
  prfun unsplit {n1,n2:nat} {l:addr} {ofs:int} .<n1>.
    (pf_mul: MUL (n1, sizeof a, ofs), pf1_arr: array_v (a, n1, l), pf2_arr: array_v (a, n2, l+ofs))
    : array_v (a, n1+n2, l) =
    sif n1 > 0 then let
      prval @(pf11_elt, pf12_arr) = array_v_uncons {a} (pf1_arr)
      // pf1_mul : MUL (n1-1, sizeof a, ofs - sizeof a)
      prval pf1_mul = mul_add_const {~1} {n1, sizeof a} (pf_mul)
      prval pf_arr_res = unsplit (pf1_mul, pf12_arr, pf2_arr)
    in
      array_v_cons {a} (pf11_elt, pf_arr_res)
    end else let
      prval () = array_v_unnil (pf1_arr); prval MULbas () = pf_mul
    in
      pf2_arr
    end // end of [sif]
} // end of [array_v_unsplit]

(* ****** ****** *)

implement
array_v_extend {a} (pf_mul, pf1_arr, pf2_at) = let
  prval pf2_arr = array_v_cons {a} (pf2_at, array_v_nil {a} ())
in
  array_v_unsplit {a} (pf_mul, pf1_arr, pf2_arr)
end // end of [array_v_extend]

implement
array_v_unextend {a}
  (pf_mul, pf_arr) = unextend (pf_mul, pf_arr) where {
  prfun unextend {n:int | n > 0} {l:addr} {ofs:int} .<n>.
    (pf_mul: MUL (n, sizeof a, ofs), pf_arr: array_v (a, n, l))
    : (array_v (a, n-1, l), a @ l+ofs-sizeof a) = let
    prval @(pf1_at, pf2_arr) = array_v_uncons {a} (pf_arr)
  in
    sif n > 1 then let
      prval pf1_mul = mul_add_const {~1} {n, sizeof a} (pf_mul)
      prval @(pf21_arr, pf22_at) = unextend (pf1_mul, pf2_arr)
    in
      @(array_v_cons {a} (pf1_at, pf21_arr), pf22_at)
    end else let
      prval () = array_v_unnil (pf2_arr)
      prval () = mul_elim {1,sizeof a} (pf_mul)
    in
      (array_v_nil (), pf1_at)
    end // end of [sif]
  end // end of [unextend]
} // end of [array_v_unextend]

(* ****** ****** *)

implement
array_v_takeout {a}
  (pf_mul, pf_arr) = takeout (pf_mul, pf_arr) where {
  prfun takeout {n,i:nat | i < n} {l:addr} {ofs:int} .<n>.
    (pf_mul: MUL (i, sizeof a, ofs), pf_arr: array_v (a, n, l))
    : (a @ l+ofs, a @ l+ofs -<lin> array_v (a, n, l)) = let
    prval @(pf1_at, pf2_arr) = array_v_uncons {a} (pf_arr)
  in
    sif i > 0 then let
      prval pf1_mul = mul_add_const {~1} {i, sizeof a} (pf_mul)
      prval (pf_at_res, fpf_res) = takeout {n-1,i-1} (pf1_mul, pf2_arr)
    in
      (pf_at_res, lam pf_at =<lin,prf> array_v_cons {a} (pf1_at, fpf_res pf_at))
    end else let
      prval () = mul_elim {0,sizeof a} (pf_mul)
    in
      (pf1_at, lam pf_at =<lin,prf> array_v_cons {a} (pf_at, pf2_arr))
    end // end of [sif]
  end // end of takeout]
} // end of [array_v_takeout]

implement
array_v_takeout2 {a}
  (pf1_mul, pf2_mul, pf_arr) = takeout2 (pf1_mul, pf2_mul, pf_arr) where {
  prfun takeout2
    {n,i1,i2:nat | i1 < n; i2 < n; i1 <> i2} {l:addr} {ofs1,ofs2:int} .<n>.
    (pf1_mul: MUL (i1, sizeof a, ofs1), pf2_mul: MUL (i2, sizeof a, ofs2), pf_arr: array_v (a, n, l))
    : (a @ l+ofs1, a @ l+ofs2, (a @ l+ofs1, a @ l+ofs2) -<lin> array_v (a, n, l)) = let
    prval @(pf1_at, pf2_arr) = array_v_uncons {a} (pf_arr)
  in
    sif (i1 == 0) then let
      prval () = mul_elim {0,sizeof a} (pf1_mul)
      prval pf21_mul = mul_add_const {~1} {i2, sizeof a} (pf2_mul)
      prval (pf_at_res, fpf_res) = array_v_takeout {a} (pf21_mul, pf2_arr)
    in
      (pf1_at, pf_at_res, llam (_1, _2) =<prf> (array_v_cons {a} (_1, fpf_res (_2))))
    end else sif (i2 == 0) then let
      prval pf11_mul = mul_add_const {~1} {i1, sizeof a} (pf1_mul)
      prval () = mul_elim {0,sizeof a} (pf2_mul)
      prval (pf_at_res, fpf_res) = array_v_takeout {a} (pf11_mul, pf2_arr)
    in
      (pf_at_res, pf1_at, lam (_1, _2) =<lin,prf> (array_v_cons {a} (_2, fpf_res (_1))))
    end else let
      prval pf11_mul = mul_add_const {~1} {i1, sizeof a} (pf1_mul)
      prval pf21_mul = mul_add_const {~1} {i2, sizeof a} (pf2_mul)
      prval (pf1_at_res, pf2_at_res, fpf_res) = takeout2 (pf11_mul, pf21_mul, pf2_arr)
    in
      (pf1_at_res, pf2_at_res, llam (_1, _2) =<prf> (array_v_cons {a} (pf1_at, fpf_res (_1, _2))))
    end // end of [sif]
  end // end of [takeout2]
} // end of [array_v_takeout2]

(* ****** ****** *)

(* end of [array_prf.dats] *)
