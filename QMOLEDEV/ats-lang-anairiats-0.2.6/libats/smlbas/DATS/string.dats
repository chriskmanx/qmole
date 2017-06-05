(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
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
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: String (http://www.standardml.org/Basis/string.html)
//

(* ****** ****** *)

staload "libats/smlbas/SATS/general.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/string.sats"

(* ****** ****** *)

staload _(*anonynous*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

#define NUL '\000'

(* ****** ****** *)

implement maxSize () = $raise Undefined "string:maxSize"

(* ****** ****** *)

implement size (s) = string1_length (s)

implement sub (s, i) = s[i]

(* ****** ****** *)

implement substring (str, st, ln) = let
  val sbp = string_make_substring (str, st, ln) in string1_of_strbuf sbp
end // end of [substring]

implement extract (str, st, oln) = case+ oln of
  | Some ln => let
      val sbp = string_make_substring (str, st, ln) in string1_of_strbuf sbp
    end // end of [Some]
  | None () => let
      val n = string1_length (str)
      val sbp = string_make_substring (str, st, n-st) in string1_of_strbuf (sbp)
    end // end of [option_none]  
(* end of [extract] *)
  
(* ****** ****** *)

implement ^ (s1, s2) = string1_of_strbuf (s1 + s2)

(* ****** ****** *)

implement str (c: char) = let
  val sbp = string_make_char (1, c) in string1_of_strbuf sbp
end // end of [str]

(* ****** ****** *)

implement concat (ss) = let
  val ss = list1_of_list0 (ss); val sbp = stringlst_concat (ss) in
  string_of_strptr (sbp)
end // end of [concat]

implement concatWith (sep, ss) = let
  val [n:int] ss = list1_of_list0 (ss)
  fun loop {i:nat} .<i>. (
      sep: string
    , s: string, ss: list (string, i)
    , res: &List_vt string? >>
             list_vt (string, i+i+1)
    ) : void = let
    val () = res :=
      list_vt_cons {string} {0} (s, ?)
    val+ list_vt_cons (_, !p_res1) = res
  in
    case+ ss of  
    | list_cons (s1, ss1) => let
        val () = !p_res1 := list_vt_cons {string} {0} (sep, ?)
        val+ list_vt_cons (_, !p_res2) = !p_res1
      in
        loop (sep, s1, ss1, !p_res2); fold@ !p_res1; fold@ res
      end // end of [list_cons]
    | list_nil () => (!p_res1 := list_vt_nil (); fold@ res)
  end // end of [loop]
in
  case+ ss of
  | list_cons (s, ss) => let
      var res: List_vt string?
      val () = loop (sep, s, ss, res)
      val lss_sep = res
      val sbp = stringlst_concat (__cast lss_sep) where {
        extern castfn __cast (_: !List_vt string): List string 
      } // end of [val]
      val () = list_vt_free (lss_sep)
    in
      string_of_strptr (sbp)
    end // end of [list_cons]  
  | list_nil () => ""
end // end of [concatWith]

(* ****** ****** *)

implement implode (cs) = let
  val sbp = string_implode (list1_of_list0 cs) in string1_of_strbuf sbp
end // end of [implode]

implement explode (str) = begin
  list0_of_list_vt (string_explode (string1_of_string str))
end // end of [val]

(* ****** ****** *)

implement map {n} (f, s) = let
  val n = string1_length (s)
  val [l:addr] (pf_gc, pf_buf | p_buf) = malloc_gc (n+1)
  val () = loop (pf_buf | p_buf, 0) where {
    fun loop {i:nat | i <= n} {l:addr} .<n-i>.
      (pf: !b0ytes (n-i+1) @ l >> strbuf (n-i+1, n-i) @ l | p: ptr l, i: size_t i)
      :<cloref1> void = let
      prval () = eqsize_byte_char ()
      prval (pf1, pf2) = array_v_uncons {byte?} (pf)
      prval pf1 = char_v_of_b0yte_v (pf1)
    in
      if i < n then let
        val c = $effmask_ref (s[i])
        val [fc:char] fc = char1_of_char (f c)
        val () = (
          if (fc = NUL) then $raise Range () else ()
        ) : [fc <> NUL] void
        val () = !p := fc
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
  string1_of_strbuf @(pf_gc, pf_buf | p_buf)
end // end of [map]

(* ****** ****** *)

implement translate (f, s) = let
  viewtypedef res_t = List_vt string
  val [n:int] s = string1_of_string (s)
  fun loop {i:nat | i <= n} .<n-i>. (
      f: char -<cloref1> string
    , s: string n, i: size_t i
    , res: &res_t? >> res_t
    ) : void = let
    val c = string_test_char_at (s, i)
  in
    if c <> NUL then let
      val () = res := list_vt_cons {..} {0} (f c, ?); val+ list_vt_cons (_, !p_res1) = res
    in
      loop (f, s, i+1, !p_res1); fold@ res
    end else begin
      res := list_vt_nil ()
    end // end of [if]  
  end (* end of [loop] *)
  var res: res_t; val () = loop (f, s, 0, res)
  val sbp = stringlst_concat (__cast res) where {
    extern castfn __cast (_: !List_vt string): List string
  } // end of [val]
  val () = list_vt_free (res)
in
  string_of_strptr (sbp)
end (* end of [translate] *)

(* ****** ****** *)

implement tokens (isdelim, s) = let
  viewtypedef res_t = List_vt string
  val [n:int] s = string1_of_string (s)
  fn* loop1
    {i:nat | i <= n} .<n-i, 1>. (
      isdelim: char -<cloref1> bool
    , s: string n, i: size_t i
    , res: &res_t? >> res_t
    ) : void = let
    val c = string_test_char_at (s, i)
  in
    if c <> NUL then begin
      if isdelim c then loop1 (isdelim, s, i+1, res) else loop2 (isdelim, s, i, i, res)
    end else begin
      res := list_vt_nil () // loop exits
    end // end of [if]
  end (* end of [loop1 *)

  and loop2
    {i0,i:nat | i0 <=i; i <= n} .<n-i, 0>. (
      isdelim: char -<cloref1> bool
    , s: string n, i0: size_t i0, i: size_t i
    , res: &res_t? >> res_t
    ) : void = let
    val c = string_test_char_at (s, i)
  in
    if c <> NUL then begin
      if isdelim (c) then let
        val () = res := list_vt_cons {..} {0} (substring (s, i0, i-i0), ?)
        val+ list_vt_cons (_, !p_res1) = res
      in
        loop1 (isdelim, s, i+1, !p_res1); fold@ res 
      end else begin
        loop2 (isdelim, s, i0, i+1, res)
      end // end of [if]
    end else begin
      res := list_vt_cons (substring (s, i0, i-i0), list_vt_nil ())
    end // end of [if]
  end (* end of [loop2] *)
  var res: res_t; val () = loop1 (isdelim, s, 0, res)
in
  list0_of_list_vt (res)
end // end of [tokens]

implement fields (isdelim, s) = let
  viewtypedef res_t = List_vt string
  val [n:int] s = string1_of_string (s)
  fun loop
    {i0,i:nat | i0 <=i; i <= n} .<n-i>. (
      isdelim: char -<cloref1> bool
    , s: string n, i0: size_t i0, i: size_t i
    , res: &res_t? >> res_t
    ) : void = let
    val c = string_test_char_at (s, i)
  in
    if c <> NUL then begin
      if isdelim (c) then let
        val () = res := list_vt_cons {..} {0} (substring (s, i0, i-i0), ?)
        val+ list_vt_cons (_, !p_res1) = res
      in
        loop (isdelim, s, i+1, i+1, !p_res1); fold@ res 
      end else begin
        loop (isdelim, s, i0, i+1, res)
      end // end of [if]
    end else begin
      res := list_vt_cons (substring (s, i0, i-i0), list_vt_nil ())
    end // end of [if]
  end (* end of [loop] *)
  var res: res_t; val () = loop (isdelim, s, 0, 0, res)
in
  list0_of_list_vt (res)
end // end of [fields]  

(* ****** ****** *)

implement isPrefix (s1, s2) = let
  val [n1:int] s1 = string1_of_string (s1)
  val [n2:int] s2 = string1_of_string (s2)
  fun loop {i:nat | i <= min(n1,n2)}
    (s1: string n1, s2: string n2, i: size_t i): bool = let
    val c1 = string_test_char_at (s1, i)
  in
    if c1 <> NUL then let
      val c2 = string_test_char_at (s2, i) in
      if c1 = c2 then loop (s1, s2, i+1) else false
    end else true
  end (* loop *)
in
  loop (s1, s2, 0)
end // end of [isPrefix]

implement isSubstring (s1, s2) = let
  val s1 = string1_of_string s1 and s2 = string1_of_string s2
in
  string_index_of_string (s2, s1) >= 0
end // end of [isSubstring]

local

staload "libc/SATS/string.sats"

in // in of [local]

implement isSuffix (s1, s2) = let
  val s1 = string1_of_string s1 and s2 = string1_of_string s2
  val n1 = string1_length s1 and n2 = string1_length s2
in
  if n1 > n2 then false else (substrcmp (s1, 0, s2, n2 - n1) = 0)
end (* end of [isSuffix] *) 

end // end of [local]

(* ****** ****** *)

implement lt (s1, s2) = lt_string_string (s1, s2)
implement lte (s1, s2) = lte_string_string (s1, s2)
implement gt (s1, s2) = gt_string_string (s1, s2)
implement gte (s1, s2) = gte_string_string (s1, s2)

implement eq (s1, s2) = eq_string_string (s1, s2)
implement neq (s1, s2) = neq_string_string (s1, s2)

implement compare (s1, s2) = compare_string_string (s1, s2) 

implement collate (f, s1, s2) = let
  val [n1:int] s1 = string1_of_string (s1)
  val [n2:int] s2 = string1_of_string (s2)
  fun loop {i:nat | i <= min(n1,n2)} (
      f: (char, char) -<cloref1> int, s1: string n1, s2: string n2, i: size_t i
    ) : int = let
    val c1 = string_test_char_at (s1, i)
    val c2 = string_test_char_at (s2, i)
  in
    if c1 <> NUL then begin
      if c2 <> NUL then let
        val sgn = f (c1, c2) in if sgn = 0 then loop (f, s1, s2, i+1) else sgn   
      end else 1 (*s1 > s2*)
    end else begin
      if c2 <> NUL then ~1 (*s1 < s2*) else 0 (*s1 = s2*)
    end // end of [if]  
  end (* loop *)
in
  loop (f, s1, s2, 0)
end // end of [collate]

(* ****** ****** *)

(* end of [string.dats] *)
