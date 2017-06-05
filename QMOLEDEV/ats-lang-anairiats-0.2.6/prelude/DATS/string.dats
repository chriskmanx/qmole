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

#define ATS_DYNLOADFLAG 0 // loaded by [main_prelude]

(* ****** ****** *)

staload "prelude/SATS/string.sats"

(* ****** ****** *)
//
// HX: declared in [prelude/SATS/string.sats]
//
implement strbuf_vsubr_lemma0 () = vsubr_refl ()

(* ****** ****** *)

implement strbuf_v_split
  (pf_mul, pf_str) = split (pf_mul, pf_str) where {
  prfun split
    {m,n:int}
    {i:nat | i <= n}
    {l:addr}
    {ofs:int} .<n>. (
    pf_mul: MUL (i, sizeof char, ofs), pf_str: strbuf_v (m, n, l)
  ) : (c1hars i @ l, strbuf_v (m-i, n-i, l+ofs)) =
    sif i == 0 then let
      prval () = mul_elim {0, sizeof char} (pf_mul)
    in
      @(array_v_nil {c1har} (), pf_str)
    end else let
      prval (pf1_at, pf2_stropt) = strbuf_v_uncons (pf_str)
      prval strbufopt_v_some pf2_str = pf2_stropt
      prval pf2_mul = mul_add_const {~1} (pf_mul)
      prval (pf1_res, pf2_res) = split {m-1,n-1} (pf2_mul, pf2_str)
    in
      (array_v_cons {c1har} (pf1_at, pf1_res), pf2_res)
    end // end of [sif]
  // end of [split]
} // end of [strbuf_v_split]

implement strbuf_v_unsplit
  (pf_mul, pf_buf, pf_str) = unsplit (pf_mul, pf_buf, pf_str) where {
  prfun unsplit {n1:nat} {m2,n2:nat} {l:addr} {ofs:int} .<n1>. (
      pf_mul: MUL (n1, sizeof char, ofs)
    , pf_buf: c1hars n1 @ l
    , pf_str: strbuf_v (m2, n2, l+ofs)
    ) : strbuf_v (n1+m2, n1+n2, l) =
    sif n1 == 0 then let
      prval () = mul_elim {0, sizeof char} (pf_mul)
      prval () = array_v_unnil {c1har} (pf_buf)
    in
      pf_str
    end else let
      prval pf2_mul = mul_add_const {~1} (pf_mul)
      prval (pf1_at, pf2_buf) = array_v_uncons {c1har} (pf_buf)
      prval pf2_res = unsplit {n1-1} (pf2_mul, pf2_buf, pf_str)
    in
      strbuf_v_cons (pf1_at, pf2_res)
    end // end of [sif]
  // end of [unsplit]
} // end of [strbuf_v_unsplit]

(* ****** ****** *)

implement print_strbuf (buf) = fprint0_strbuf (stdout_ref, buf)
implement prerr_strbuf (buf) = fprint0_strbuf (stderr_ref, buf)

(* ****** ****** *)

%{^
ATSinline()
ats_ptr_type
atspre_string_string_alloc
  (ats_size_type n) {
  char *p ;
  p = ATS_MALLOC(n+1); p[n] = '\000'; return p ;
} // end of [atspre_string_string_alloc]
%} // end of [%{^]

(* ****** ****** *)
//
// implement string_empty = "" // this requires dynamic loading
//
(* ****** ****** *)

#define NUL '\000'
#define i2sz size1_of_int1 

(* ****** ****** *)

implement
string_make_list_int (cs, n) = let
  val (pf_gc, pf_sb | p_sb) =
    string_alloc (i2sz n) where {
    extern fun string_alloc {n:nat} (n: size_t n)
      :<> [l:addr] (freebyte_gc_v (n+1, l), strbuf (n+1, n) @ l | ptr l)
      = "atspre_string_string_alloc"
  } // end of [val]
  val () = loop (!p_sb, n, 0, cs) where {
    fun loop {m,n:nat} {i,j:nat | i + j == n} .<n-i>.
      (buf: &strbuf (m, n), n: int n, i: int i, cs: list (char, j)):<> void =
      if i < n then let
        val+ list_cons (c, cs) = cs
        val [c:char] c = char1_of_char c
//
        val () = $effmask_all (
          if (c <> NUL) then () else let
            val () = prerrf (
              "exit(ATS): a string cannot contain null characters in the middle.\n", @()
            ) // end of [val]
          in
            exit(1)
          end (* end of [if] *)
        ) : [c <> NUL] void // end of [val]
//
        val () = strbuf_set_char_at (buf, i2sz i, c)
      in
        loop (buf, n, i+1, cs)
      end else begin
        // loop exists
      end // end of [if]
  } // end of [val]
in
  #[.. | (pf_gc, pf_sb | p_sb)]
end // end of [string_make_list_int]

implement
string_make_list_rev_int (cs, n) = let
  val (pf_gc, pf_sb | p_sb) =
    string_alloc (i2sz n) where {
    extern fun string_alloc {n:nat} (n: size_t n)
      :<> [l:addr] (freebyte_gc_v (n+1, l), strbuf (n+1, n) @ l | ptr l)
      = "atspre_string_string_alloc"
  } // end of [val]
  val () = loop (!p_sb, n-1, 0, cs) where {
    fun loop {m,n:nat} {i,j:nat | i + j == n} .<n-i>.
      (buf: &strbuf (m, n), n1: int (n-1), i: int i, cs: list (char, j)):<> void =
      if i <= n1 then let
        val+ list_cons (c, cs) = cs
        val [c:char] c = char1_of_char c
//
        val () = $effmask_all (
          if (c <> NUL) then () else let
            val () = prerrf (
              "exit(ATS): a string cannot contain null characters in the middle.\n", @()
            ) // end of [val]
          in
            exit(1)
          end (* end of [if] *)
        ) : [c <> NUL] void // end of [val]
//
        val () = strbuf_set_char_at (buf, i2sz (n1-i), c)
      in
        loop (buf, n1, i+1, cs)
      end else begin
        // loop exists
      end // end of [if]
  } // end of [val]
in
  #[.. | (pf_gc, pf_sb | p_sb)]
end // end of [string_make_list_int]

(* ****** ****** *)

implement
stringlst_concat (ss) = let
  val n0 = aux (ss, i2sz 0) where {
    fun aux {k:nat} .<k>.
      (ss: list (string, k), n: size_t):<> size_t = case+ ss of
      | list_cons (s, ss) => aux (ss, n + string0_length s) | list_nil () => n
    // end of [aux]
  } // end of [val n0]
  val [n0:int] n0 = size1_of_size (n0)
  fun loop1 {m0,n0,i0,n,i:nat | i0 <= n0; i <= n} .<n0-i0>. (
      s0: &strbuf (m0, n0), n0: size_t n0, i0: size_t i0, s: string n, i: size_t i
    ) :<> sizeLte n0 = let
    val c = string_test_char_at (s, i) in
    if c <> NUL then begin
      if i0 < n0 then (s0[i0] := c; loop1 (s0, n0, i0+1, s, i+1)) else i0
    end else i0 // end of [if]
  end (* end of [loop1] *)
  fun loop2 {m0,n0,i0,k:nat | i0 <= n0} .<k>. (
      s0: &strbuf (m0, n0), n0: size_t n0, i0: size_t i0, ss: list (string, k)
    ) :<> void = begin
    case+ ss of
    | list_cons (s, ss) => let
        val s = string1_of_string s; val i0 = loop1 (s0, n0, i0, s, 0)
      in
        loop2 (s0, n0, i0, ss)
      end // end of [list_cons]
    | list_nil () => () // loop exists
  end (* end of [loop2] *)
  val [l:addr] (
    pf_gc, pf_sb | p_sb
  ) = string_alloc (n0) where {
    extern fun string_alloc {n:nat} (n: size_t n)
      :<> [l:addr] (freebyte_gc_v (n+1, l), strbuf (n+1, n) @ l | ptr l)
      = "atspre_string_string_alloc"
  } // end of [val]
  val () = loop2 (!p_sb, n0, 0, ss)
in
  strptr_of_strbuf @(pf_gc, pf_sb | p_sb)
end // end of [stringlst_concat]

(* ****** ****** *)

implement
string_explode (s) = let
  fun loop {n,i:int | 0 <= i; i <= n} .<i+1>. (
      s: string n, i: size_t i, cs: list_vt (char, n-i)
    ) :<> list_vt (char, n) =
    if i >= 1 then let
      val i1 = i - 1
      val c = string_get_char_at (s, i1) in loop (s, i1, list_vt_cons (c, cs))
    end else begin
      cs // loop exists
    end
  // end of [loop]
  val n = string_length s
in
  loop (s, n, list_vt_nil ())
end // end of [string1_explode]

implement
string_implode (cs) =
  string_make_list_int (cs, loop (cs, 0)) where {
  fun loop {i,j:nat} .<i>.
    (cs: list (char, i), j: int j):<> int (i+j) = case+ cs of
    | list_cons (_, cs) => loop (cs, j+1) | list_nil () => j
  // end of [f]
} // end of [string_implode]

(* ****** ****** *)

implement
string_foreach__main {v} {vt} {n} {f:eff}
  (pf | buf, f, env) =  loop (pf | buf, f, env, 0) where {
  fun loop {i:nat | i <= n} .<n-i>. (
      pf: !v
    | str: string n, f: (!v | c1har, !vt) -<f> void, env: !vt, i: size_t i
    ) :<f> void =
    if string_isnot_at_end (str, i) then (f (pf | str[i], env); loop (pf | str, f, env, i+1))
  // end of [loop]
} // end of [strbuf_foreach__main]

(* ****** ****** *)

local

fn string_make_fun {n:nat} (
  s: string n, f: c1har -<> c1har
) :<> [l:addr] strbufptr_gc (n+1, n, l) = let
  val n = string1_length (s)
  val [l:addr] (pf_gc, pf_buf | p_buf) = malloc_gc (n+1)
  val () = loop (pf_buf | p_buf, 0) where {
    fun loop {i:nat | i <= n} {l:addr} .<n-i>.
      (pf: !b0ytes (n-i+1) @ l >> strbuf (n-i+1, n-i) @ l | p: ptr l, i: size_t i)
      :<cloref> void = let
      prval () = eqsize_byte_char ()
      prval (pf1, pf2) = array_v_uncons {byte?} (pf)
      prval pf1 = char_v_of_b0yte_v (pf1)
    in
      if i < n then let
        val c = $effmask_ref (s[i])
        val () = !p := f (c)
        val () = loop (pf2 | p + sizeof<byte>, i + 1)
        prval () = pf := strbuf_v_cons (pf1, pf2)
      in
        // empty
      end else let
        val () = !p := NUL
        prval () = pf := strbuf_v_null (pf1, pf2)
      in
        // empty
      end // end of [if]
    end (* end of [loop] *)
  } // end of [val]
in
  #[l | (pf_gc, pf_buf | p_buf)]
end // end of [string_make_fun]

in // in of [local]

implement string_tolower (s) = let
  extern fun tolower (c: c1har):<> c1har = "atspre_char_tolower"
in
  string_make_fun (s, tolower)
end // end of [string_tolower__bufptr]

implement string_toupper (s) = let
  extern fun toupper (c: c1har):<> c1har = "atspre_char_toupper"
in
  string_make_fun (s, toupper)
end // end of [string_toupper__bufptr]

end // end of [local]

(* ****** ****** *)

implement print_strptr (p) = fprint_strptr (stdout_ref, p)
implement prerr_strptr (p) = fprint_strptr (stderr_ref, p)

implement
strptr_dup (p) = let
  val str = __cast (p) where {
    extern castfn __cast {l:agz} (p: !strptr l):<> String
  } // end of [val]
  val n = string1_length (str)
  val str2 = string_make_substring (str, 0, n)
in
  strptr_of_strbuf (str2)
end // end of [strptr_dup]

(* ****** ****** *)

%{$

#ifndef EXIT_SUCCESS
#define	EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

%}

/* ****** ****** */

%{$
//
// HX: a commonly used simple hash function
//
ats_ulint_type
atspre_string_hash_33 (ats_ptr_type s0) {
  unsigned long int hash_val ; unsigned char *s; int c;
  hash_val = 3141593UL ;
//
  s = (unsigned char*)s0 ;
  while (1) {
    c = *s ;
    if (!c) break ; // the end of string is reached
    hash_val = ((hash_val << 5) + hash_val) + c ; // hash_val = 33 * hash_val + c
    s += 1 ;
  } // end of [while]
//
  return hash_val ;
//
} // end of [atspre_string_hash_33]

%} // end of [%{$]

(* ****** ****** *)

%{$

ats_ptr_type
atspre_string_make_char (
  ats_size_type n, ats_char_type c
) {
  char *p ; 
  if (!c) { ats_exit_errmsg
    (EXIT_FAILURE, "exit(ATS): [string_make_char] failed: null char.\n") ;
  } ; // end of [if]
  p = ATS_MALLOC(n+1) ; memset (p, c, n) ; p[n] = '\000' ;
  return p ;
} // end of [atspre_string_make_char]

/* ****** ****** */

ats_ptr_type
atspre_string_make_substring (
  ats_ptr_type src0, ats_size_type start, ats_size_type len
) {
  char *des, *src ;
  des = ATS_MALLOC(len+1) ;
  src = ((char*)src0) + start ;
  memcpy(des, src, len) ; des[len] = '\000' ;
  return des ;
} // end of [atspre_string_make_substring]

/* ****** ****** */

ats_void_type
atspre_strbuf_tolower
  (ats_ptr_type p0) {
  int c ; char *p = (char*)p0 ;
  while (c = *p) { *p = tolower (c) ; p += 1 ; }
  return ;
} // end of [atspre_strbuf_tolower]

ats_void_type
atspre_strbuf_toupper
  (ats_ptr_type p0) {
  int c ; char *p = (char*)p0 ;
  while (c = *p) { *p = toupper (c) ; p += 1 ; }
  return ;
} // end of [atspre_strbuf_toupper]

/* ****** ****** */

%} // end of [%{$]

(* ****** ****** *)

(* end of [string.dats] *)
