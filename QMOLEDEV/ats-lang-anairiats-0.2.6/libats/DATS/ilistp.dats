(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
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
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// HX: reasoning about integer sequences and multisets
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "libats/SATS/ilistp.sats"

(* ****** ****** *)

implement
length_istot
  () = istot () where {
  prfun istot {xs:ilist} .<xs>.
    (): [n:nat] LENGTH (xs, n) =
    scase xs of
    | ilist_cons (x, xs) => LENGTHcons (istot {xs} ())
    | ilist_nil () => LENGTHnil ()
  // end of [prfun]
} // end of [length_istot]

implement
length_isfun (pf1, pf2) = let
  prfun isfun {xs:ilist} {n1,n2:int} .<xs>. (
    pf1: LENGTH (xs, n1), pf2: LENGTH (xs, n2)
  ) : [n1==n2] void =
    scase xs of
    | ilist_cons (x, xs) => let
        prval LENGTHcons (pf1) = pf1 and LENGTHcons (pf2) = pf2
        prval () = isfun {xs} (pf1, pf2)
      in
        // nothing
      end // end of [ilist_cons]
    | ilist_nil () => let
        prval LENGTHnil () = pf1 and LENGTHnil () = pf2 in (*nothing*)
      end // end of [ilist_nil]
  // end of [isfun]
in
  isfun (pf1, pf2)
end // end of [length_isfun]

(* ****** ****** *)

implement length_isnat
  {xs} (pf) = length_isfun (pf, length_istot {xs} ())
// end of [length_isnat]

(* ****** ****** *)

implement
append_length_lemma (pf, pf1len, pf2len) = let
//
prfun lemma
  {xs1,xs2:ilist} {xs:ilist} {n1,n2:int} .<xs1>. (
  pf: APPEND (xs1, xs2, xs), pf1len: LENGTH (xs1, n1), pf2len: LENGTH (xs2, n2)
) : LENGTH (xs, n1+n2) = let
  prval () = length_isnat (pf2len)
in
  scase xs1 of
  | ilist_cons (x1, xs1) => let
      prval APPENDcons (pf) = pf
      prval LENGTHcons (pf1len) = pf1len
      prval pflen = lemma (pf, pf1len, pf2len)
    in
      LENGTHcons (pflen)
    end // end of [ilist_cons]
  | ilist_nil () => let
      prval APPENDnil () = pf
      prval LENGTHnil () = pf1len
    in
      pf2len
    end // end of [ilist_nil]
end // end of [append_length_lemma]
//
in
  lemma (pf, pf1len, pf2len)
end // end of [append_length_lemma]

(* ****** ****** *)

implement
msetcnt_istot
  {x0} {xs} () = let
  prfun istot {xs:ilist} .<xs>.
    (): [n:nat] MSETCNT (x0, xs, n) =
    scase xs of
    | ilist_cons (x, xs) => MSETCNTcons (istot {xs} ())
    | ilist_nil () => MSETCNTnil ()
  // end of [istot]
in
  istot {xs} ()
end // end of [msetcnt_istot]

implement
msetcnt_isfun
  {x0} (pf1, pf2) = let
  prfun isfun {xs:ilist} {n1,n2:int} .<xs>. (
    pf1: MSETCNT (x0, xs, n1), pf2: MSETCNT (x0, xs, n2)
  ) : [n1==n2] void =
    scase xs of
    | ilist_cons (x, xs) => let
        prval MSETCNTcons pf1 = pf1 and MSETCNTcons pf2 = pf2
        prval () = isfun (pf1, pf2)
      in
        // nothing
      end // end of [ilist_cons]
    | ilist_nil () => let
        prval MSETCNTnil () = pf1 and MSETCNTnil () = pf2 in (* nothing *)
      end // end of [ilist_nil]
  // end of [isfun]
in
  isfun (pf1, pf2)
end // end of [msetcnt_isfun]

implement
msetcnt_first {x} {xs} () = let
  prval pf = msetcnt_istot {x} {xs} () in MSETCNTcons (pf)
end // end of [msetcnt_first]

(* ****** ****** *)

implement
nth_msetcnt_lemma {x} (pf) = let
  prfun lemma {xs:ilist} {i:int} .<xs>.
    (pf: NTH (x, xs, i)): [n:pos] MSETCNT (x, xs, n) =
    case+ pf of
    | NTHind (pf) => MSETCNTcons (lemma (pf))
    | NTHbas () => msetcnt_first ()
  // end of [lemma]
in
  lemma (pf)
end // end of [nth_msetcnt_lemma]

implement
msetcnt_nth_lemma {x} (pf) = let
  prfun lemma {xs:ilist} {n:pos} .<xs>.
    (pf: MSETCNT (x, xs, n)): [i:nat] NTH (x, xs, i) = let
    prval MSETCNTcons {..} {x1} {xs1} (pf1) = pf
  in
    sif x == x1 then NTHbas () else NTHind (lemma (pf1))
  end // end of [lemma]
in
  lemma (pf)
end // end of [msetcnt_nth_lemma]


(* ****** ****** *)

implement
insert_length_lemma {x0} (pf1, pf2) = let
  prfun lemma
    {xs:ilist} {i:int} {ys:ilist} {n:nat} .<xs>.
    (pf1: INSERT (x0, xs, i, ys), pf2: LENGTH (xs, n)): LENGTH (ys, n+1) =
    case+ pf1 of
    | INSERTbas () => LENGTHcons (pf2)
    | INSERTind (pf1) => let
        prval LENGTHcons pf2 = pf2 in LENGTHcons (lemma (pf1, pf2))
      end // end of [INSERTind]
  // end of [lemma]
in
  lemma (pf1, pf2)
end // end of [insert_length_lemma]

implement
nth_insert_lemma {x} (pf) = let
  prfun lemma {xs:ilist} {n:int} .<xs>.
    (pf: NTH (x, xs, n)): [ys:ilist] INSERT (x, ys, n, xs) =
    case+ pf of
    | NTHind (pf) => INSERTind (lemma (pf))
    | NTHbas {..} {xs1} () => INSERTbas {x} {xs1} ()
  // end of [lemma]
in
  lemma (pf)
end // end of [nth_insert_lemma]

(* ****** ****** *)

implement permute_refl () = lam pf => pf

implement
permute_symm
  {xs1,xs2} (fpf) =
lam {x0:int} {n:nat} (
  pf: MSETCNT (x0, xs2, n)
) : MSETCNT (x0, xs1, n) =<prf> let
  prval pf1 = msetcnt_istot {x0} {xs1} ()
  prval pf2 = fpf (pf1)
  prval () = msetcnt_isfun (pf, pf2)
in
  pf1
end // end of [permute_symm]

implement
permute_trans (fpf1, fpf2) = lam pf => fpf2 (fpf1 (pf))

(* ****** ****** *)

(*
implement
append_munion_lemma
  (pf) = lemma (pf) where {
  prfun lemma {xs,ys,zs:ilist} .<xs>.
    (pf: APPEND (xs,ys,zs)): MUNION (xs, ys, zs) =
    case+ pf of
    | APPENDcons (pf) => let
        prval fpf = lemma (pf) in
        lam (pf1, pf2) => let
          prval MSETCNTcons pf1 = pf1 in MSETCNTcons (fpf (pf1, pf2))
        end // end of [lam]
      end (* end of [APPENDcons] *)
    | APPENDnil () =>
        lam (pf1, pf2) => let
          prval MSETCNTnil () = pf1 in pf2
        end // end of [lam]
      (* end of [APPENDnil] *)
  // end of [lemma]
} // end of [append_munion_lemma]
*)

(* ****** ****** *)

(* end of [ilistp.dats] *)
