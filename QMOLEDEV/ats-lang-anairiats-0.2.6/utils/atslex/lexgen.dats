(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

// July 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

%{^

#include "prelude/CATS/array.cats"

%}

(* ****** ****** *)

staload "top.sats"

typedef intset = intset_t

// datatype for regular expressions
datatype regex1_node =
  | REG1alt of (regex1, regex1)
  | REG1chars of (int, charset_t)
  | REG1nil
  | REG1end of int
  | REG1opt of regex1
  | REG1plus of regex1
  | REG1seq of (regex1, regex1)
  | REG1star of regex1
  | REG1null

where regex1: type = '{
  node= regex1_node, null= bool, fstpos= intset, lstpos= intset
}

(* ****** ****** *)

exception Fatal

(* ****** ****** *)

datatype CSI = CSI_cs of charset_t | CSI_i of int
dataviewtype CSIlst (int) =
  | CSIlst_nil (0) | {n:nat} CSIlst_cons (n+1) of (CSI, CSIlst n)

extern fun CSI_uncons {n:pos} (lst: &CSIlst n >> CSIlst (n-1)): CSI =
  "CSI_uncons"

implement CSI_uncons (lst) =
  let val+ ~CSIlst_cons (csi, lst_r) = lst in (lst := lst_r; csi) end

viewtypedef T = [n:nat] @{ lst= CSIlst n, len= int n }

(* ****** ****** *)

fn regex1_alt (r1: regex1, r2: regex1): regex1 = let
  val null: bool = if r1.null then true else r2.null
  val fstpos = r1.fstpos + r2.fstpos
  val lstpos = r1.lstpos + r2.lstpos
in '{
  node= REG1alt (r1, r2)
, null= null
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_alt]

//

fn regex1_chars (x: &T, cs: charset_t): regex1 = let
  val x_len = x.len
  val () = x.len := x_len + 1
  val x_lst = x.lst
  val () = x.lst := CSIlst_cons (CSI_cs cs, x_lst)
in '{
  node= REG1chars (x_len, cs)
, null= false
, fstpos = intset_singleton x_len
, lstpos = intset_singleton x_len
} end // end of [regex1_chars]

//

fn regex1_end (x: &T, irule: int): regex1 = let
  val x_len = x.len
  val () = x.len := x_len + 1
  val x_lst = x.lst
  val () = x.lst := CSIlst_cons (CSI_i irule, x_lst)
in '{
  node= REG1end x_len
, null= false
, fstpos = intset_singleton x_len
, lstpos = intset_singleton x_len
} end // end of [regex1_end]

//

fn regex1_nil (): regex1 = '{
  node= REG1nil ()
, null= true
, fstpos = intset_nil
, lstpos = intset_nil
}

//

fn regex1_null (): regex1 = '{
  node= REG1nil ()
, null= false
, fstpos = intset_nil
, lstpos = intset_nil
}

//

fn regex1_seq (r1: regex1, r2: regex1): regex1 = let
  val null: bool = if r1.null then r2.null else false
  val fstpos: intset =
    if r1.null then r1.fstpos + r2.fstpos else r1.fstpos
  val lstpos: intset =
    if r2.null then r1.lstpos + r2.lstpos else r2.lstpos
in '{
  node= REG1seq (r1, r2)
, null= null
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_seq]

//

fn regex1_opt (r: regex1): regex1 = let
  val fstpos = r.fstpos and lstpos = r.lstpos
in '{
  node= REG1opt r
, null= true
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_opt]

fn regex1_plus (r: regex1): regex1 = let
  val null = r.null and fstpos = r.fstpos and lstpos = r.lstpos
in '{
  node= REG1plus r
, null= null
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_plus]

fn regex1_star (r: regex1): regex1 = let
  val fstpos = r.fstpos and lstpos = r.lstpos
in '{
  node= REG1star r
, null= true
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_star]

(* ****** ****** *)

fn redef_find
  (env: redef, id0: string): Option regex = let
  fun loop (env: redef, id0: string): Option regex = begin
    case+ env of
    | redef_cons (id, r, env) =>
        if (id0 = id) then Some {regex} (r) else loop (env, id0)
    | redef_nil () => None {regex} ()
  end // end of [loop]
in
  loop (env, id0)
end // end of [redef_find]

(* ****** ****** *)

extern
fun array_of_CSIlst
  {n:nat} (
  lst: CSIlst n, n: int n
) :<> [l:addr] (
  free_gc_v (CSI, n, l), array_v (CSI, n, l) | ptr l
) = "array_of_CSIlst"

%{

typedef ats_ptr_type CSI ;
ats_ptr_type
array_of_CSIlst (
  ats_ptr_type lst, ats_int_type n
) {
  CSI *p0, *p ;
  p0 = atspre_array_ptr_alloc_tsz(n, sizeof(CSI)) ;
  p = p0 + (n-1) ;
  while (n > 0) {
    *p = CSI_uncons (&lst) ; --n ; --p ;
  }
  return p0 ;
} // end of [array_of_CSIlst]

%}

(* ****** ****** *)

fun regex_mark_str
  {i,l:nat | i <= l} .<l-i>. (
  env: redef, x0: &T, i: size_t i, l: size_t l, s: string l, r1e: regex1
) : regex1 = begin
  if i < l then let
    val cs = charset_singleton s[i]
    val r1e = regex1_seq (r1e, regex1_chars (x0, cs))
  in
    regex_mark_str (env, x0, i+1, l, s, r1e)
  end else r1e
end // end of [regex_mark_str]

(* ****** ****** *)

fun regex_mark (
  env: redef, x0: &T, r0e: regex
) : regex1 = begin case+ r0e of
  | REGalt (r0e1, r0e2) => let
      val r1e1 = regex_mark (env, x0, r0e1)
      val r1e2 = regex_mark (env, x0, r0e2)
    in
      regex1_alt (r1e1, r1e2)
    end
  | REGchars (cs) => regex1_chars (x0, cs)
  | REGnil () => regex1_nil ()
  | REGid id => begin
      case+ redef_find (env, id) of
        | Some r0e0 => regex_mark (env, x0, r0e0)
        | None () => begin
          prerrf ("Undefined identifier: %s\n", @(id));
          $raise Fatal ()
        end
    end (* end of [REGid] *)
  | REGopt (r0e0) => regex1_opt (regex_mark (env, x0, r0e0))
  | REGplus (r0e0) => regex1_plus (regex_mark (env, x0, r0e0))
  | REGrep (r0e0, i) =>
      if i > 0 then let
        val r1e0 = regex_mark (env, x0, r0e0)
      in
        regex_mark_rep (env, x0, i-1, r0e0, r1e0)
      end else regex1_nil ()
    // end of [REGrep]
  | REGseq (r0e1, r0e2) => let
      val r1e1 = regex_mark (env, x0, r0e1)
      val r1e2 = regex_mark (env, x0, r0e2)
    in
      regex1_seq (r1e1, r1e2)
    end // end of [REGseq]
  | REGstar (r0e0) => regex1_star (regex_mark (env, x0, r0e0))
  | REGstr (str) => let
      val str = string1_of_string str
    in
      regex_mark_str (env, x0, 0, string_length str, str, regex1_nil ())
    end // end of [REGstr]
end // end of [regex_mark]

and regex_mark_rep (
  env: redef, x0: &T, i: int, r0e: regex, r1e: regex1
) : regex1 =
  if i > 0 then let
    val r1e' = regex_mark (env, x0, r0e)
  in
    regex_mark_rep (env, x0, i-1, r0e, regex1_seq (r1e, r1e'))
  end else r1e // end of [if]
// end of [regex_mark_rep]

(* ****** ****** *)

fun followpos
  {n:nat} (
  n0: int n, r: regex1
) : [l:addr] (
  free_gc_v (intset?, n, l), array_v (intset, n, l) | ptr l
) = let
  fun aux {l:addr}
    (pf: !array_v (intset, n, l) | A: ptr l, n0: int n, r: regex1): void =
    aux_node (pf | A, n0, r.node)

  and aux_node{l:addr}
    (pf: !array_v (intset, n, l) | A: ptr l, n0: int n, r_node: regex1_node)
    : void =
    case+ r_node of
    | REG1alt (r1, r2) => begin
        aux (pf | A, n0, r1); aux (pf | A, n0, r2)
      end // end of [REG1alt]
    | REG1chars (n, cs) => ()
    | REG1end n => ()
    | REG1nil () => ()
    | REG1null () => ()
    | REG1opt r0 => aux (pf | A, n0, r0)
    | REG1plus r0 => let
        val r0_fstpos = r0.fstpos
        val f = lam (
          pf: !array_v (intset, n, l) | i: int
        ) : void =<cloptr1> let
          val i = int1_of_int i
(*
          val () = prerrf ("followpos: i = %i and n = %i\n", @(i,n))
*)
          val () = assert (0 <= i)
          val () = assert (i < n0)
        in
          A[i] := A[i] + r0_fstpos
        end // end of [f]
        val () = foreach_intset (pf | f, r0.lstpos)
        val () = cloptr_free (f)
      in
        aux (pf | A, n0, r0)
      end // end of [REG1plus]
    | REG1seq (r1, r2) => let
        val r2_fstpos = r2.fstpos
        val f = lam (
          pf: !array_v (intset, n, l) | i: int
        ) : void =<cloptr1> let
          val i = int1_of i
(*
          val () = prerrf ("followpos: i = %i and n = %i\n", @(i,n))
*)
          val () = assert (0 <= i)
          val () = assert (i < n0)
        in
          A[i] := A[i] + r2_fstpos
        end // end of [f]
        val () = foreach_intset (pf | f, r1.lstpos)
        val () = cloptr_free (f)
      in
        aux (pf | A, n0, r1); aux (pf | A, n0, r2);
      end // end of [REG1seq]
    | REG1star r0 => let
        val r0_fstpos = r0.fstpos
        val f = lam (
          pf: !array_v (intset, n, l) | i: int
        ) : void =<cloptr1> let
          val i = int1_of i
(*
          val () = prerrf ("followpos: i = %i and n = %i\n", @(i,n))
*)
          val () = assert (0 <= i)
          val () = assert (i < n0)
        in
          A[i] := A[i] + r0_fstpos
        end // end of [f]
        val () = foreach_intset (pf | f, r0.lstpos)
        val () = cloptr_free (f)
      in
        aux (pf | A, n0, r0)
      end // end of [REG1star]
  // end of [aux] and [aux_node]
  val n0_sz = size1_of_int1 n0
  val tsz = sizeof<intset>
  val (pf_gc, pf_arr | p_arr) =
    array_ptr_alloc_tsz {intset} (n0_sz, tsz)
  val () = begin
    array_ptr_initialize_elt_tsz {intset} (!p_arr, n0_sz, ini, tsz)
  end where {
    var ini: intset = intset_nil
  } // end of [val]
  val () = aux (pf_arr | p_arr, n0, r)
in
  (pf_gc, pf_arr | p_arr)
end // end of [followpos]

(* ****** ****** *)

fn rules_mark (
  env: redef, x0: &T, rls: rules
) : regex1 = let
  fun aux (
    env: redef, x0: &T, irule: int, rls: rules
  ) : regex1 =
    case+ rls of
    | rules_cons (r0e, act, rls) => let
        val r1e1 = regex_mark (env, x0, r0e)
        val r1e2 = regex1_end (x0, irule)
        val r1e_seq = regex1_seq (r1e1, r1e2)
      in
        regex1_alt (r1e_seq, aux (env, x0, irule+1, rls))
      end
    | rules_nil () => regex1_null ()
  // end of [aux]
in
   aux (env, x0, 1, rls) // irule starts from 1!
end (* rules_mark *)

(* ****** ****** *)

dataviewtype
acclst =
  | acclst_nil
  | acclst_cons of (int (*state*), int (*rule*), acclst)
// end of [acclst]

dataviewtype
intlst = intlst_nil | intlst_cons of (int, intlst)

dataviewtype
statelst =
  | statelst_nil | statelst_cons of (intset, statelst)
// end of [statelst]

dataviewtype
translst (int) =
  | translst_nil (0)
  | {n:nat} translst_cons (n+1) of (int, intlst, translst n)
// end of [translst]

viewtypedef Translst = [n:nat] translst n

(* ****** ****** *)

fn transition_char
  {n:nat} {l_csi,l_pos:addr} (
  pf1: !array_v (CSI, n, l_csi)
, pf2: !array_v (intset, n, l_pos)
| A_csi: ptr l_csi
, A_pos: ptr l_pos
, n: int n, st: intset, c: char
) : intset = let
(*
  val () = prerrf ("transition_char: c = %i\n", @(int_of c))
*)
  var st_res: intset = intset_nil
  viewdef V = (
    array_v (CSI, n, l_csi), array_v (intset, n, l_pos), intset @ st_res
  ) // end of [viewdef]
  val f = lam (
    pf: !V | i: int
  ) : void =<cloptr1> let
    prval (pf1, pf2, pf3) = pf
    val i = int1_of_int i
(*
    val () = prerrf ("transition_char: i = %i and n = %i\n", @(i, n))
    val () = (prerr "transition_char: st_res = "; prerr_intset st_res; prerr_newline ())
*)
    val () = assert (0 <= i); val () = assert (i < n)
    val () = (case+ A_csi[i] of
      | CSI_cs cs => begin
          if charset_is_member (cs, c) then st_res := st_res + A_pos[i]
        end // end of [CSI_cs]
      | _ => ()
    ) : void // end of [val]
(*
    val () = (prerr "transition_char: st_res = "; prerr_intset st_res; prerr_newline ())
*)
  in
     pf := (pf1, pf2, pf3)
  end // end of [f]
  prval pf = (pf1, pf2, view@ st_res)
  val () = foreach_intset {V} (pf | f, st)
  prval () = (pf1 := pf.0; pf2 := pf.1; view@ st_res := pf.2)
  val () = cloptr_free (f)
in
  st_res
end // end of [transition_char]

fun transition_one {n:nat} {l_csi,l_pos:addr}
  (pf1: !array_v (CSI, n, l_csi),
   pf2: !array_v (intset, n, l_pos) |
   A_csi: ptr l_csi, A_pos: ptr l_pos, n: int n,
   nst_r: &int, sts: &states_t, stlst: &statelst,
   st0: intset, c: char, ns: intlst): intlst = begin
  if int_of_char c >= ~1 then let
    val st = transition_char (pf1, pf2 | A_csi, A_pos, n, st0, c)
    val nst = states_find (sts, st)
    val nst =
      if nst < 0 then let // not found
        val nst = nst_r
        val () = nst_r := nst+1
        val () = states_insert (sts, nst, st)
        val () = stlst := statelst_cons (st, stlst)
      in
        nst
      end else begin
        nst (* found *)
      end // end of [if]
    val ns = intlst_cons (nst, ns)
  in
    transition_one (
      pf1, pf2 | A_csi, A_pos, n, nst_r, sts, stlst, st0, c-1, ns
    ) // end of [transition_one]
  end else begin
    ns
  end // end of [if]
end (* end of [transition_one] *)

macdef CHAR_MAX = '\177'

fun transition_all {n:nat} {l_csi,l_pos:addr}
  (pf1: !array_v (CSI, n, l_csi),
   pf2: !array_v (intset, n, l_pos) |
   A_csi: ptr l_csi, A_pos: ptr l_pos, n: int n,
   nst_r: &int, sts: &states_t, stlst: statelst,
   ans: Translst): Translst = begin
  case+ stlst of
  | ~statelst_cons (st, stlst) => let
      val nst = states_find (sts, st)
      var stlst = stlst
      val ns = transition_one (
        pf1, pf2
      | A_csi, A_pos, n, nst_r, sts, stlst, st, CHAR_MAX, intlst_nil ()
      ) // transition_one
    in
      transition_all (
        pf1, pf2
      | A_csi, A_pos, n, nst_r, sts, stlst, translst_cons (nst, ns, ans)
      ) // end of [transition_all]
    end
  | ~statelst_nil () => ans
end // end of [transition_all]

(* ****** ****** *)

fun accept_one {n:nat} {l_csi:addr}
  (pf1: !array_v (CSI, n, l_csi) |
   A_csi: ptr l_csi, n: int n, st: intset): int = let
  var irule = (0: int)
  viewdef V = (array_v (CSI, n, l_csi), int @ irule)
  val f = lam
    (pf: !V | nst: int): void =<cloptr1> let
    prval (pf1, pf2) = pf
    val nst = int1_of_int nst
(*
    val () = prerrf ("accept_one: i = %i and n = %i\n", @(i,n))
*)
    val () = assert (0 <= nst)
    val () = assert (nst < n)
    val () = case+ A_csi[nst] of
      | CSI_i i => begin
          if irule > 0 then (if i < irule then irule := i) else (irule := i)
        end // end of [CSI_i]
      | _ => ()
    // end of [val]
  in
    pf := (pf1, pf2)
  end // end of [f]
  prval pf = (pf1, view@ irule)
  val () = foreach_intset {V} (pf | f, st)
  prval () = pf1 := pf.0; prval () = view@ irule := pf.1
  val () = cloptr_free (f)
in
  irule
end // end of [accept_one]

fun accept_all {n:nat} {l_csi:addr} (
    pf1: !array_v (CSI, n, l_csi) | A_csi: ptr l_csi, n: int n, sts: states_t
  ) : acclst = let
  var ans: acclst = acclst_nil ()
  viewdef V = (array_v (CSI, n, l_csi), acclst @ ans)
  val f = lam (
    pf: !V | tag: int, st: intset
  ) : void =<cloptr1> let
    prval (pf1, pf2) = pf
    val irule = accept_one (pf1 | A_csi, n, st)
    val () = if irule > 0 then ans := acclst_cons (tag, irule, ans)
  in
    pf := (pf1, pf2)
  end // end of [f]
  prval pf = (pf1, view@ ans)
  val () = states_foreach_and_free {V} (pf | f, sts)
  prval () = pf1 := pf.0; prval () = view@ ans := pf.1
  val () = cloptr_free (f)
in
  ans
end // end of [accept_all]

(* ****** ****** *)

extern fun acclst_length (lst: !acclst): int = "acclst_length"

implement acclst_length (lst) = let
  fun loop (lst: !acclst, j: int): int =
    case+ lst of
    | acclst_cons (_, _, !lst_r) =>
        let val n = loop (!lst_r, j+1) in fold@ lst; n end
    | acclst_nil () => (fold@ lst; j)
in
  loop (lst, 0)
end // end of [acclst_length]

(* ****** ****** *)

extern fun translst_length {n:nat} (lst: !translst n): int n
  = "translst_length"

implement translst_length (lst) = let
  fun loop {i,j:nat} (lst: !translst i, j: int j): int(i+j) =
    case+ lst of
    | translst_cons (_, _, !lst_r) =>
      let val n = loop (!lst_r, j+1) in fold@ lst; n end
    | translst_nil () => (fold@ lst; j)
in
  loop (lst, 0)
end // end of [translst_length]

(* ****** ****** *)

extern fun translst_uncons {n:pos} (
    lst: &translst n >> translst (n-1), tag: &(int?) >> int, ns: &intlst? >> intlst
  ) : void
  = "translst_uncons"

implement translst_uncons (lst, tag, ns) = let
  val+ ~translst_cons (tag_v, ns_v, lst_v) = lst
in
  tag := tag_v; ns := ns_v; lst := lst_v
end // end of [translst_uncons]

(* ****** ****** *)

extern fun fprint_irule {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, i: int): void
  = "fprint_irule"

extern fun fprint_state {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, n: int): void
  = "fprint_state"

%{

ats_void_type
fprint_irule(ats_ptr_type fil, ats_int_type i) {
  int x, x0, x1, x2 ;

  x = i >> 8 ;
  x2 = '0' + (x & 07) ; x >>= 3 ;
  x1 = '0' + (x & 07) ; x >>= 3 ;
  x0 = '0' + (x & 07) ; x >>= 3 ;
  fputc ('\\', fil); fputc (x0, fil) ; fputc (x1, fil) ; fputc (x2, fil) ;

  if (x != 0) {
    fprintf (
      stderr, "lexgen.dats: fprint_irule: rule number is too large: %i .\n", i
    ) ;
    exit (1) ;
  }

  x = i & 0xff ;
  x2 = '0' + (x & 07) ; x >>= 3 ;
  x1 = '0' + (x & 07) ; x >>= 3 ;
  x0 = '0' + (x & 07) ;
  fputc ('\\', fil); fputc (x0, fil) ; fputc (x1, fil) ; fputc (x2, fil) ;
}

ats_void_type
fprint_state (ats_ptr_type fil, ats_int_type n) {
  int x, x0, x1, x2 ;

  x = n >> 8 ;
  x2 = '0' + (x & 07) ; x >>= 3 ;
  x1 = '0' + (x & 07) ; x >>= 3 ;
  x0 = '0' + (x & 07) ; x >>= 3 ;
  fputc ('\\', fil); fputc (x0, fil) ; fputc (x1, fil) ; fputc (x2, fil) ;

  if (x != 0) {
    fprintf (
      stderr, "lexgen.dats: fprint_state: state number is too large: %i .\n", n
    ) ;
    exit (1) ;
  }

  x = n & 0xff ;
  x2 = '0' + (x & 07) ; x >>= 3 ;
  x1 = '0' + (x & 07) ; x >>= 3 ;
  x0 = '0' + (x & 07) ;
  fputc ('\\', fil); fputc (x0, fil) ; fputc (x1, fil) ; fputc (x2, fil) ;
}

%}

extern fun fprint_intlst {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, ns: intlst): void
   = "fprint_intlst"

// printing as well as freeing
implement fprint_intlst {m} (pf_mod | fil, ns) = let
  fun loop (fil: &FILE m, ns: intlst): void = case+ ns of
    | ~intlst_cons (n, ns) => begin
        fprint_state (pf_mod | fil, n); loop (fil, ns)
      end
    | ~intlst_nil () => ()
in
  loop (fil, ns); fprint_char (pf_mod | fil, '\\')
end // end of [fprint_intlst]

(* ****** ****** *)

extern typedef "intlst" = intlst

// printing as well as freeing
extern fun fprint_translst {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, lst: Translst): void
  = "fprint_translst"

%{

ats_void_type
fprint_translst (ats_ptr_type fil, ats_ptr_type lst) {
  int i, n, tag ; intlst ns, *A;

  n = translst_length (lst) ;

  // this is really just radix sorting
  A = ats_malloc_ngc((n+1)*sizeof(intlst)) ; // A[0] is unused

  for (i = 1; i <= n; ++i) {
    translst_uncons (&lst, &tag, &ns) ;
/*
    fprintf (stderr, "fprint_translst: i = %i and tag = %i\n", i, tag);
*/
    A[tag] = ns ;
  }

  for (i = 1; i <= n; ++i) {
    fprint_intlst (fil, A[i]) ; fprintf ((FILE*)fil, "\n") ;
  }

  ats_free_ngc(A) ;

  return ;
}

%}

// freeing as well
fun fprint_acclst {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, lst: acclst)
  : void = let
  fun loop (fil: &FILE m, lst: acclst): void =
    case+ lst of
    | ~acclst_cons (n, i, lst) => begin
        fprint_state (pf_mod | fil, n);
        fprint_irule (pf_mod | fil, i);
        fprint_char (pf_mod | fil, '\\');
        fprint_newline (pf_mod | fil);
        loop (fil, lst)
      end
    | ~acclst_nil () => ()
in
  loop (fil, lst)
end // end of [fprint_acclst]

(* ****** ****** *)

fun fprint_header {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, id: string, arg: string)
  : void = begin
  fprintf (pf_mod | fil, "implement %s (%s) =\n", @(id, arg));
  fprint_string (pf_mod | fil, "case+ lexing_engine (__");
  fprint_string (pf_mod | fil, id);
  fprint_string (pf_mod | fil, "_transition_table, __");
  fprint_string (pf_mod | fil, id);
  fprint_string (pf_mod | fil, "_accept_table) of");
  fprint_newline (pf_mod | fil)
end // end of [fprint_header]

(* ****** ****** *)

fun fprint_rules {m:file_mode} (
    pf_mod: file_mode_lte (m, w)
  | fil: &FILE m, id: string, arg: string, rls: rules
  ) : void = let
  fun loop (fil: &FILE m, rls: rules, irule: int): void =
    case+ rls of
    | rules_cons (r, code, rls) => begin
        fprintf (pf_mod | fil, "  | %i => ( %s )\n", @(irule, code));
        loop (fil, rls, irule + 1)
      end // end of [rules_cons]
    | rules_nil () => ()
  // end of [loop]
in
  loop (fil, rls, 1);
  fprintf (pf_mod | fil, "  | _ => %s_lexing_error (%s)\n", @(id, arg))
end // end of [fprint_rules]

(* ****** ****** *)

extern fun fprint_DFA {m:file_mode} (
    pf_mod: file_mode_lte (m, w)
  | fil: &FILE m, env: redef, id: string, arg: string, rls: rules
  ) : void

val regex_eof = REGchars charset_eof

implement fprint_DFA (pf_mod | fil, env, id, arg, rls) = let

var x0: T = @{ lst= CSIlst_nil (), len= 0 }
// EOF is pre-defined and cannot be overwritten
val env = redef_cons ("EOF", regex_eof, env)
val root_regex1 = rules_mark (env, x0, rls)
val root_fstpos = root_regex1.fstpos

(*
val () = fprint_string (pf_mod | fil, "root_fstpos = ")
val () = fprint_intset (pf_mod | fil, root_fstpos)
val () = fprint_newline (pf_mod | fil)
*)

val npos = x0.len

val (pf_csi_gc, pf_csi | A_csi) = array_of_CSIlst (x0.lst, npos)
val (pf_pos_gc, pf_pos | A_pos) = followpos (npos, root_regex1)

var nst_r = (0: int)
var sts = states_nil ()

// state 0 is special: it is the error state
val _ = states_insert (sts, 0, intset_nil)
// state 1 is special: it is the start state
val _ = states_insert (sts, 1, root_fstpos)
val () = nst_r := (2: int)

var stlst = statelst_nil ()
val () = stlst := statelst_cons (root_fstpos, stlst)

val dfa_transtbl = transition_all
  (pf_csi, pf_pos | A_csi, A_pos, npos, nst_r, sts, stlst, translst_nil ())

val () = array_ptr_free {intset} (pf_pos_gc, pf_pos | A_pos)

val dfa_acctbl = accept_all (pf_csi | A_csi, npos, sts: states_t)
val dfa_nfinal = acclst_length dfa_acctbl (* number of final states *)

val () = array_ptr_free {CSI} (pf_csi_gc, pf_csi | A_csi)

val dfa_nstate = nst_r - 1

in

// transition table

  fprintf (
    pf_mod | fil,
    "val __%s_transition_table: transition_table_t = __transition_table_make %i \"\\\n",
    @(id, dfa_nstate)
  );
  fprint_translst (pf_mod | fil, dfa_transtbl);
  fprint_string (pf_mod | fil, "\"\n");

// accepting states

  fprintf (
    pf_mod | fil,
    "val __%s_accept_table: accept_table_t = __accept_table_make %i %i \"\\\n",
    @(id, dfa_nstate, dfa_nfinal)
  );
  fprint_acclst (pf_mod | fil, dfa_acctbl);
  fprint_string (pf_mod | fil, "\"\n\n");

// function for lexical analysis

  fprint_header (pf_mod | fil, id, arg);
  fprint_rules (pf_mod | fil, id, arg, rls);
  fprint_newline (pf_mod | fil);

end // end of [fprint_DFA]

(* ****** ****** *)

implement fprint_lexfns {m} (pf_mod | fil, env, lfs) = let
  fun loop (fil: &FILE m, env: redef, lfs: lexfns): void =
    case+ lfs of
    | lexfns_cons (id, arg, rls, lfs) => begin
        fprint_DFA (pf_mod | fil, env, id, arg, rls);
        loop (fil, env, lfs)
      end // end of [lexfns_cons]
    | lexfns_nil () => ()
  // end of [loop]
in
  loop (fil, env, lfs)
end // end of [fprint_lexfns]

(* ****** ****** *)

(* end of [lexgen.dats] *)
