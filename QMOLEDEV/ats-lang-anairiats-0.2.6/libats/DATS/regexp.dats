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
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

// some basic IO operations

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

staload "libats/SATS/regexp.sats"

(* ****** ****** *)

#define i2sz size1_of_int1
#define sz2i int1_of_size1

(* ****** ****** *)

%{^

ats_ptr_type
atslib_regexp_compile
  (ats_ptr_type pattern) {
  const char *errptr ;
  int erroffset ; pcre *rc ;
  rc = pcre_compile (
    (char*)pattern
  , 0 /* option bits */
  , &errptr, &erroffset
  , (unsigned char*)0 /* tableptr */
  ) ; // end of [pcre_compile]
  return rc ;
} /* end of [atslib_regexp_compile] */

%} // end of [%{^]

implement
regexp_compile_exn (pattern) = let
  val re0 = regexp_compile (pattern) 
  val p_re0 = ptr_of (re0)
in
  if (p_re0 <> null) then re0
  else let
    val _ = regexp_free_null (re0)
    val () = prerrf (
      "exit(ATS): [pcre_comiple] failed: pattern = %s\n", @(pattern)
    ) // end of [val]
  in
    exit(1)
  end // end of [if]
end // end of [regexp_compile_exn]

(* ****** ****** *)

implement
regexp_match_string (re, str) = let
  val str = string1_of_string (str)
  val len = string_length (str); val len = sz2i (len)
in
  regexp_match_substring (re, str, 0(*ofs*), len)
end // end of [regexp_match_string]

(* ****** ****** *)

%{$

ats_bool_type
atslib_regexp_match_substring (
  ats_ptr_type re, ats_ptr_type str, ats_int_type ofs, ats_int_type len
) {
  int rc ;
//
  rc = pcre_exec (
    (pcre*)re
  , (pcre_extra*)0 /* [re] is not studied */
  , ((char*)str)+ofs, len, 0 // strlenofs
  , 0 /* option bits */
  , (int*)0 /* ovector */
  , 0 /* ovecsize */
  ) ; // end of [pcre_exec]
//
  if (rc >= 0) return ats_true_bool ;
//
  switch (rc) {
  case PCRE_ERROR_NOMATCH: return ats_false_bool ;
  default: fprintf
    (stderr, "exit(ATS): [regexp_match_substring] failed\n"); exit (1);
  } /* end of [switch] */
//
  return ats_false_bool ; /* deadcode */
//
} /* end of [atslib_regexp_match_substring] */

%} // end of [%{$]

(* ****** ****** *)

implement
regexp_match_string_strposlst (re, str) = let
  val len = string_length (str); val len = sz2i (len)
in
  regexp_match_substring_strposlst (re, str, 0(*ofs*), len)
end // end of [regexp_match_string_strpostlst]

(* ****** ****** *)

local

extern
fun strposlst_make_arrptr
  {n:nat} (A: &(@[int][n+n]), n: int n): List_vt @(int, int)
  = "atslib_strposlst_make_arrptr"
// end of [strposlst_make_arrptr]

in // in of [local]

implement
strposlst_make_arrptr
  {n} (A, n) = let
  typedef int2 = (int, int)
  stavar l:addr
  val p = (&A): ptr l
//
  viewdef V1 = array_v (int, n+n, l) and V2 = array_v (int2, n, l)
//
  prval () = __assert (view@ (A)) where {
    extern prfun __assert (pf: !V1 >> V2): void
  } // end of [prval]
  val xs = list_vt_make_array<int2> (A, size1_of_int1 (n))
  prval () = __assert (view@ (A)) where {
    extern prfun __assert (pf: !V2 >> V1): void
  } // end of [prval]
//
in
  xs
end // end of [strposlst_make_arrptr]

end // end of [local]

(* ****** ****** *)

%{$
ats_ptr_type
atslib_regexp_match_substring_strposlst (
  ats_ptr_type re, ats_ptr_type str, ats_int_type ofs, ats_int_type len
) {
  int rc ;
  int ncapture ;
  int ovecsize, *ovector ;
  int err ;
//
  ats_ptr_type res ;
//
  err = pcre_fullinfo (
    (pcre*)re, (pcre_extra*)0, PCRE_INFO_CAPTURECOUNT, &ncapture
  ) ; // end of [pcre_fullinfo]
//
  ovecsize = 3 * (ncapture + 1) ;
  ovector = ATS_MALLOC (ovecsize * sizeof(int)) ;
//
  rc = pcre_exec (
    (pcre*)re
  , (pcre_extra*)0 /* [re] is not studied */
  , ((char*)str)+ofs, len, 0 // strlenofs
  , 0 /* option bits */
  , ovector
  , ovecsize
  ) ; // end of [pcre_exec]
//
  if (rc >= 0) {
    res = atslib_strposlst_make_arrptr (ovector, rc) ;
  } else {
    res = (ats_ptr_type)0 ;
  } // end of [if]
//
  ATS_FREE (ovector) ;
//
  if (rc >= 0) return res ;
//
  switch (rc) { // [rc] is negative
    case PCRE_ERROR_NOMATCH: return res ;
    default: fprintf (
      stderr, "exit(ATS): [regexp_match_substring_strposlst] failed\n"
    ) ; exit (1) ;
  } // end of [switch]
//
  return res ; /* deadcode */
//
} /* end of [atslib_regexp_match_substring_strposlst] */

%} // end of [%{$]

(* ****** ****** *)

%{^

ATSinline()
ats_int_type
atslib_regexp_split_substring_search (
  ats_ptr_type re
, ats_ptr_type str
, ats_int_type ofs
, ats_int_type len
, ats_ptr_type ofsvec
) {
  int rc ;
  rc = pcre_exec (
    (pcre*)re
  , (pcre_extra*)0 /* [re] is not study */
  , ((char*)str)+ofs, len, 0 // strlenofs
  , 0 /* option bits */
  , (int*)ofsvec /* ovector */
  , 3 /* ovecsize */
  ) ; // end of [pcre_exec]
  return rc ;
} /* end of [atslib_regexp_split_substring_search] */

%} // end of [%{^]

(* ****** ****** *)

implement
regexp_split_string_list
  (re, str) = let
//
  val str =
    $UN.castvwtp1 {string} (str)
  // end of [val]
//
  val str = string1_of_string (str)
  val len = string1_length (str)
  val len = int1_of_size1 (len)
in
  regexp_split_substring_list (re, str, 0, len)
end // end of [regexp_split_string_list]

implement
regexp_split_substring_list
  {l0} {n} (re, str, ofs, len) = let
//
  extern fun search
    {i,ln:nat | i + ln <= n} {l:addr} (
      pf_arr: ! @[int?][3] @ l >> @[int][3] @ l
    | re: !REGEXPptr l0, str: string n, ofs: int i, len: int ln, l: ptr l
    ) : int = "atslib_regexp_split_substring_search"
//
  viewtypedef res = List_vt (strptr1)
//
  fun loop {i,ln:nat | i+ln <= n} {l:addr} (
      pf_arr: !(@[int?][3] @ l)
    | re: !REGEXPptr l0
    , str: string n, ofs: int i, len: int ln, p: ptr l, res: &res? >> res
    ) : void = case+ 0 of
    | _ when (len > 0) => let
        val rc = search (pf_arr | re, str, ofs, len, p)
      in
        case+ rc of
        | _ when rc >= 0 => let
            val [i1:int] i1 = int1_of p->[0]
            and [i2:int] i2 = int1_of p->[1]
            prval () = __assert () where {
              extern prfun __assert (): [0 <= i1; i1 <= i2; i2 <= ln] void
            } // end of [prval]
(*
            val () = if (i1 = i2) then let
              val () = prerr (
                "exit(ATS): [regexp_split_substring_list]: nullable separator"
              ) // end of [val]
              val () = prerr_newline ()
            in
              exit (1)
            end // end of [val]
*)
            val sbp = string_make_substring (str, (i2sz)ofs, (i2sz)i1)
            val s = strptr_of_strbuf (sbp) // no-op cast
            val () = res := list_vt_cons {strptr1} {0} (s, ?)
            val+ list_vt_cons (_, !p_res) = res
            val () = loop (pf_arr | re, str, ofs+i2, len-i2, p, !p_res)
            prval () = fold@ (res)
          in
            // nothing
          end // end of [_ when rc >= 0]
        | _ => let
            val sbp = string_make_substring (str, (i2sz)ofs, (i2sz)len)
            val s = strptr_of_strbuf (sbp) // no-op cast
            val () = res := list_vt_cons {strptr1} {0} (s, ?)
            val+ list_vt_cons (_, !p_res) = res
            val () = loop (pf_arr | re, str, ofs+len, 0, p, !p_res)
            prval () = fold@ (res)
          in
            // nothing
          end // end of [_]
      end // end of [_ when (i < n)]
    | _ (* i = n *) => (res := list_vt_nil)
//
  val str =
    $UN.castvwtp1 {string(n)} (str)
  // end of [val]
//
  var !p_arr with pf_arr = @[int][3]()
  var res: res? // uninitialized
  val () = loop (pf_arr | re, str, ofs, len, p_arr, res)
in
  res
end // end of [regexp_split_substring_list]

(* ****** ****** *)

val () = initialize () where {
  extern fun initialize (): void = "atslib_libats_regexp_initialize"
} // end of [val]

(* ****** ****** *)

%{$
//
// HX-2010-02-20: is this really necessary?
//
ats_void_type
atslib_libats_regexp_initialize () {
  pcre_malloc = (void *(*)(size_t))ats_malloc_gc ;
  pcre_free = (void (*)(void*))ats_free_gc ;
  pcre_stack_malloc = (void *(*)(size_t))ats_malloc_gc ;
  pcre_stack_free = (void (*)(void*))ats_free_gc ;
  return ;  
} /* end of [atslib_libats_regexp_initialize] */

%} // end of [%{$]

(* ****** ****** *)

(* end of [regexp.dats] *)
