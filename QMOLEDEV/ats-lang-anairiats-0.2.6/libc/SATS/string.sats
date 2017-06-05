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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)
// Start Time: 2008
//
(* ****** ****** *)

%{#
#include "libc/CATS/string.cats"
%} // end of [%{#]

(* ****** ****** *)

staload ERRNO = "libc/SATS/errno.sats"
typedef errno_t = $ERRNO.errno_t

(* ****** ****** *)

fun strcmp (
  str1: !READ(string), str2: !READ(string)
) : int = "mac#atslib_strcmp" // end of [strcmp]

fun substrcmp
  {n1:int} {i1:nat | i1 <= n1}
  {n2:int} {i2:nat | i2 <= n2} (
  str1: !READ(string n1), i1: size_t i1, str2: !READ(string n2), i2: size_t i2
) : int = "atslib_substrcmp"
// end of [substrcmp]

(* ****** ****** *)

fun strncmp {n:nat} (
  str1: !READ(string), str2: !READ(string), n: size_t n
) :<> int = "mac#atslib_strncmp" // end of [strncmp]

fun substrncmp
  {n1:int}
  {i1:nat | i1 <= n1}
  {n2:int}
  {i2:nat | i2 <= n2}
  {n:nat} (
  str1: !READ(string n1), i1: size_t i1
, str2: !READ(string n2), i2: size_t i2
, n: size_t n
) :<> int
  = "mac#atslib_substrncmp"
// end of [fun]

(* ****** ****** *)

fun strlen {n:nat}
  (str: !READ(string n)):<> size_t n = "mac#atslib_strlen" // !mac
// end of [strlen]

(* ****** ****** *)

(*
char *strchr(const char*, int):
please use [string_index_of_char_from_left] in [prelude/SATS/string.sats]

char *strrchr(const char*, int):
please use [string_index_of_char_from_right] in [prelude/SATS/string.sats]

char *strstr(const char*, const char* ): 
please use [string_index_of_string] in [prelude/SATS/string.sats]
*)

(* ****** ****** *)

fun strspn {n:nat}
  (str: !READ(string n), accept: !READ(string)):<> sizeLte n
  = "mac#atslib_strspn" // macro!
// end of [strspn]

fun strcspn {n:nat}
  (str: !READ(string n), reject: !READ(string)):<> sizeLte n
  = "mac#atslib_strcspn" // macro!
// end of [strcspn]

(* ****** ****** *)

fun strcpy
  {m:int} {n:nat | n < m} {l:addr} {ofs:int} (
  pf_buf: !b0ytes m @ l >> strbuf (m, n) @ l | sbf: ptr l, str: !READ(string n)
) :<> ptr l = "mac#atslib_strcpy" // macro!
// end of [strcpy]

(* ****** ****** *)

fun strcat
  {m:int} {n1,n2:nat | n1 + n2 < m} {l:addr} (
  pf_buf: !strbuf (m, n1) @ l >> strbuf (m, n1+n2) @ l
| sbf: ptr l, str: !READ(string n2)
) :<> ptr l = "mac#atslib_strcat" // macro!
// end of [strcat]

(*
char *strncat(char *dst, const char *src, size_t n):
note that there is really no need for this function given that [strcat] is safe!
*)

(* ****** ****** *)

(*
char *strpbrk(const char *str, const char *accept);
*)

dataprop
strpbrk_p (l:addr, n:int, l_ret:addr) =
  | {i:nat | i < n} {l_ret == l+i} strpbrk_p_some (l, n, l_ret)
  | {l_ret == null} strpbrk_p_none (l, n, l_ret)
// end of [strpbrk_p]

fun strpbrk
  {m,n:nat} {l:addr} (
  pf: !strbuf (m, n) @ l
| p: ptr l, accept: !READ(string)
) :<> [l_ret:addr] (strpbrk_p (l, n, l_ret) | ptr l_ret)
  = "mac#atslib_strpbrk" // macro!
// end of [strpbrk]

(* ****** ****** *)
//
// HX: implemented in [string.dats]
//
fun strdup_gc
  {n:nat} (str: !READ(string n))
  :<> [l:addr] (freebyte_gc_v (n+1, l), strbuf (n+1, n) @ l | ptr l)
  = "atslib_strdup_gc"
// end of [strdup_gc]

(* ****** ****** *)

dataprop
memchr_p (
  l:addr, n:int, addr(*ret*)
) = // [l] should be positive
  | {i:nat | i < n} memchr_p_some (l, n, l+i)
  | memchr_p_none (l, n, null)
// end of [memchr_p]

fun memchr {n:nat}
  {n1:int | n <= n1} {l:addr} (
  pf: !bytes n1 @ l | p: ptr l, chr: int, n: size_t n
) : [l_ret:addr] (memchr_p (l, n, l_ret) | ptr l_ret) = "mac#atslib_memchr"
// end of [memchr]

(* ****** ****** *)

fun memcmp {n:nat}
  {n1,n2:int | n <= n1; n <= n2} (
  buf1: &bytes n1, buf2: &bytes n2, n: size_t n
) :<> int = "mac#atslib_memcmp" // end of [memcmp]

(* ****** ****** *)

fun memcpy {n:nat}
  {n1,n2:int | n <= n1; n <= n2} {l:addr} (
  pf_dst: !bytes n1 @ l | p_dst: ptr l, p_src: &bytes n2, n: size_t n
) :<> ptr l = "mac#atslib_memcpy" // end of [memcpy]

(* ****** ****** *)

fun memset {n:nat}
  {n1:int | n <= n1} {l:addr} (
  pf: !bytes n1 @ l | p: ptr l, chr: int, n: size_t n
) :<> ptr l = "mac#atslib_memset" // end of [memset]

(* ****** ****** *)
//
// HX: [strerror] is not reentrant:
// memory for the returned string is statically allocated
//
fun strerror (
  errno: errno_t
) :<!ref> [l:agz] (strptr l -<lin,prf> void | strptr l)
  = "mac#atslib_strerror" // macro!
// end of [strerror]

(* ****** ****** *)

dataview
strerror_v (
  m:int, l:addr, int(*err*)
) =
  | {n:nat}
    strerror_succ (m, l,  0) of strbuf (m, n) @ l
  | strerror_fail (m, l, ~1) of b0ytes m @ l
// end of [strerror_v]

//
// HX: [strerror_r] is reentrant // this is the POSIX version
//
fun strerror_r
  {m:nat} {l:addr} (
  pf: b0ytes m @ l | errno: errno_t, p_buf: ptr l, m: size_t m
) : [i:int] @(strerror_v (m, l, i) | int i) = "mac#atslib_strerror_r"
// end of [strerror_r]
  
(* ****** ****** *)

(*
//
// HX-2010-10-21:
// this one is moved into libc/SATS/signal.sats:
//
fun strsignal (sgn: signum_t)
  : [l:addr] (strptr(l) -<lin,prf> void | strptr(l)) = "mac#atslib_strsignal"
// end of [strsignal]
*)

(* ****** ****** *)

(* end of [string.sats] *)
