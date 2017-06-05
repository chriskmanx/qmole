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
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
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
// Time: Summer, 2007
//

(* ****** ****** *)

staload STDLIB = "libc/SATS/stdlib.sats"

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

exception Fatal of string

(* ****** ****** *)

local

#include
"prelude/params_system.hats"

in // in of [local]

#if SYSTEM_IS_UNIX_LIKE #then
//
val dirsep: char = '/'
val dirsep_str: string = "/"
//
#endif

end // end of [local]

(* ****** ****** *)

local

val the_wordsize: size_t = sizeof<ptr>

var the_wordsize_target: size_t = the_wordsize
val (pfbox_the_wordsize_target | ()) = begin
  vbox_make_view_ptr {size_t} (view@ the_wordsize_target | &the_wordsize_target)
end // end of [val]

in // in of [local]

implement wordsize_target_get () = let
  prval vbox pf = pfbox_the_wordsize_target in the_wordsize_target
end // end of [wordsize_target_get]

implement
wordsize_target_set (sz) = let
  prval vbox pf = pfbox_the_wordsize_target in the_wordsize_target := sz
end // end of [wordsize_target_get]

end // end of [local]

(* ****** ****** *)

#define sbp2str string1_of_strbuf

implement atsopt_local = "bin/atsopt"
implement precats_local = "prelude/CATS/"
implement runtime_local = "ccomp/runtime/"

implement atslib_local () = let
  val wsz = wordsize_target_get ()
  val wsz = size1_of_size (wsz) // no-op casting
in
  case+ 0 of
  | _ when (wsz = 4(*bytes*)) => "ccomp/lib/"
  | _ when (wsz = 8(*bytes*)) => "ccomp/lib64/"
  | _ => "ccomp/lib/"
end // end of [atslib_local]

implement
atslib_output_local () =
  sbp2str (atslib_local () + "output/")
// end of [atslib_output_local]

implement libats_local () =
  sbp2str (atslib_local () + "libats.a")
// end of [libats_local]

//
// HX: multithreaded
//
implement libats_mt_local () =
  sbp2str (atslib_local () + "libats_mt.a")
// end of [libats_mt_local]

(* ****** ****** *)

local

#define ATSHOME_var "ATSHOME"

in // in of [local]

fn ATSHOME_get (): String = let
  val (fpf_x | x) = $STDLIB.getenv ATSHOME_var
in
  if strptr_isnot_null (x) then let
    val x1 = strptr_dup (x)
    prval () = fpf_x (x)
    val x1 = string_of_strptr (x1)
  in
    string1_of_string (x1)
  end else let
    prval () = fpf_x (x)
  in
    prerr "The environment variable [";
    prerr ATSHOME_var;
    prerr "] is undefined!";
    prerr_newline ();
    $raise (Fatal "ATSHOME_get")
  end (* end of [if] *)
end // end of [ATSHOME_get]

end // end of [local]

(* ****** ****** *)

implement
ATSHOME_dir = let
  val ATSHOME = ATSHOME_get ()
  val n = string_length ATSHOME
in
  if n > 0 then
    if ATSHOME[n-1] = dirsep then ATSHOME else sbp2str (ATSHOME + dirsep_str)
  else begin
    prerr "The variable [ATSHOME] is empty!\n" ;
    $raise (Fatal "ATSHOME")
  end (* end of [if] *)
end // end of [ATSHOME]

implement
ATSHOME_dir_append basename =
  sbp2str (ATSHOME_dir + (string1_of_string basename))
// end of [ATSHOME_dir_append]

(* ****** ****** *)

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

local

#define ATSCCOMP_def "gcc"
#define ATSCCOMP_var "ATSCCOMP"

in // in of [local]

implement ATSCCOMP_gcc = let
  val (fpf_x | x) = $STDLIB.getenv ATSCCOMP_var
in
  if strptr_isnot_null (x) then let
    val x1 = strptr_dup (x)
    prval () = fpf_x (x)
    val x1 = string_of_strptr (x1)
  in
    (string1_of_string)x1
  end else let
    prval () = fpf_x (x) in ATSCCOMP_def
  end (* end of [if] *)
end // end of [ATSCCOMP_gcc]

end // end of [local]

(* ****** ****** *)

implement
basename_of_filename name = let
  val name = string1_of_string name
  val n = string_length name
  val i = string_index_of_char_from_right (name, dirsep)
in
  if (i >= 0) then let
    val i = size1_of_ssize1 (i)
    val () = assert_prerrf_bool1
      (i < n, "[basename_of(%s)] failed.\n", @(name))
    val sbp = string_make_substring (name, i+1, n-i-1)
  in
    sbp2str (sbp)
  end else begin
    name (* [name] containing no [dirsep] *)
  end // end of [if]
end // end of [basename_of_filename]

implement suffix_of_filename name = let
  val name = string1_of_string name
  val i = string_index_of_char_from_right (name, '.')
in
  if i >= 0 then let
    val i = size1_of_ssize1 (i)
    val n = string_length name
    val str = sbp2str (string_make_substring (name, i+1, n-i-1))
  in
    stropt_some (str)
  end else begin
    stropt_none (* [name] containing no [dirsep] *)
  end // end of [if]
end // end of [suffix_of_filename]

implement
filename_is_local name = let
   val name = string1_of_string name
in
   if string1_isnot_empty (name) then let
     val _0 = size1_of_int1 0
   in
     if string_get_char_at (name, _0) <> dirsep then true else false
   end else true
end // end of [filename_is_local]

(* ****** ****** *)

implement atsopt_global = ATSHOME_dir_append atsopt_local
implement precats_global = ATSHOME_dir_append precats_local
implement runtime_global = ATSHOME_dir_append runtime_local

implement atslib_global () = ATSHOME_dir_append (atslib_local ())
implement atslib_output_global () = ATSHOME_dir_append (atslib_output_local ())

implement libats_global () = ATSHOME_dir_append (libats_local ())
implement libats_mt_global () = ATSHOME_dir_append (libats_mt_local ())

(* ****** ****** *)

local

#define nil STRLSTnil
#define :: STRLSTcons

in // in of [local]

implement strlst_nil () = STRLSTnil ()
implement strlst_is_nil (ss) =
  case+ ss of nil () => true | _ :: _ => false

implement strlst_head_get (ss) = let val+ s :: _ = ss in s end
implement strlst_tail_get (ss) = let val+ _ :: ss = ss in ss end

implement
strlst_length {n} (ss) = let
  fun aux {i,j:nat | i+j == n} .<i>.
    (ss: strlst i, res: size_t j): size_t n =
    case+ ss of nil () => res | _ :: ss => aux (ss, res+1)
  // end of [aux]
in
  aux (ss, size1_of_int1 0)
end // end of [strlst_length]

implement
strlst_reverse {n} ss = let
  fun aux {i,j:nat | i+j == n} .<i>.
    (ss: strlst i, res: strlst j): strlst n =
    case+ ss of nil () => res | s :: ss => aux (ss, s :: res)
in
   aux (ss, nil ())
end // end of [strlst_reverse]

end // end of [local]

(* ****** ****** *)

implement
lstrlst_reverse (xs0) = let
  fun revapp {m,n:nat} .<m>.
    (xs: lstrlst m, ys: lstrlst n):<> lstrlst (m+n) =
    case+ xs of
    | LSTRLSTcons (_, !p_xs1) => let
        val xs1 = !p_xs1 in !p_xs1 := ys; fold@ xs; revapp (xs1, xs)
      end // end of [val]
    | ~LSTRLSTnil () => ys
  // end of [revapp]
in
  revapp (xs0, LSTRLSTnil ())
end // end of [lstrlst_reverse]

(* ****** ****** *)

%{^

#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

/* ****** ****** */

#include "libc/CATS/stdlib.cats"

/* ****** ****** */

ats_void_type // also defined in [prelude/DATS/basics.dats]
ats_exit(const ats_int_type status) { exit(status) ; return ; }

ats_void_type // also defined in [prelude/DATS/basics.dats]
ats_exit_errmsg (
  const ats_int_type status, const ats_ptr_type errmsg
) {
  fprintf(stderr, "%s", (char *)errmsg) ; exit(status) ; return ;
} // end of [ats_exit_errmsg]

/* ****** ****** */

ats_void_type // also defined in [prelude/DATS/printf.dats]
atspre_exit_prerrf ( // [status] should be of the type uint8
  const ats_int_type status, const ats_ptr_type fmt, ...
) {
  va_list ap ;
  va_start(ap, fmt) ; vfprintf(stderr, (char *)fmt, ap) ; va_end(ap) ;
/*
  fprintf (stderr, "atspre_exit_prerrf: status = %i\n", status) ;
*/
  exit(status) ;
  return ; // deadcode
} // end of [atspre_exit_prerrf]

//

ats_void_type // also defined in [prelude/DATS/printf.dats]
atspre_assert_prerrf (
  ats_bool_type assertion, ats_ptr_type fmt, ...
) {
  int err ;
  va_list ap ;
//
  if (!assertion) {
    va_start(ap, fmt) ;
    err = vfprintf(stderr, (char *)fmt, ap) ;
    va_end(ap) ;
    if (err < 0) { ats_exit_errmsg
      (err, "exit(ATS): [atspre_assert_prerrf]: prerrf failed\n") ;
    } else { ats_exit_errmsg
      (  1, "exit(ATS): [atspre_assert_prerrf]: assert failed\n") ;
    } /* end of [if] */
  } /* end of [if] */
//
  return ;
} // end of [atspre_assert_prerrf]

//

//
// HX: also defined in [prelude/DATS/printf.dats]
//
ats_ptr_type
atspre_vsprintf_size (
  ats_int_type guess, const ats_ptr_type fmt, va_list ap0
) {
  int n, sz ; char *res ; va_list ap ;
//
  sz = guess ;
//
  while (1) {
    va_copy (ap, ap0) ;
    res = ATS_MALLOC(sz) ;
    n = vsnprintf(res, sz, (char*)fmt, ap) ;
    if (n >= 0) {
      if (n < sz) return res ;
      sz = n+1 ; ATS_FREE(res) ; continue ;
    } else {
      return ((ats_ptr_type)0) ;
    } // end of [if]
  } // end of [while]
//
  return (ats_ptr_type)0 ; // deadcode  
//
} // end of [atspre_vsprintf_size]

//
// HX: also defined in [prelude/DATS/printf.dats]
//
ats_ptr_type
atspre_tostringf_size (
  ats_int_type guess, const ats_ptr_type fmt, ...
) {
  char *res ;
  va_list ap ;
//
  va_start(ap, fmt);
  res = (char*)atspre_vsprintf_size (guess, fmt, ap);
  va_end(ap);
  if (!res) { ats_exit_errmsg
    (1, "exit(ATS): [atspre_tostringf_size] failed.\n") ;
  } // end of [if]
//
  return res ;
} // end of [atspre_tostringf_size]

/* ****** ****** */
//
// HX: also defined in [prelude/DATS/string.dats]
//
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

extern ats_ptr_type atsopt_global ;

ats_bool_type
file_is_exec (
  ats_ptr_type name
) {
  struct stat buf ;
  int ret = stat (name, &buf) ;
//
  if (ret < 0) { atspre_exit_prerrf
    (errno, "exit(ATS): File [%s] does not exist.\n", atsopt_global) ;
  } // end of [if]
//
  return (S_IXUSR & buf.st_mode) ;
} // end of [file_is_exec]

/* ****** ****** */

// int reference operations

typedef ats_ptr_type ats_intref_type ;

ats_intref_type
intref_make (ats_int_type i) {
  int *r ;
  r = ats_malloc_gc(sizeof(ats_int_type)) ; // HX: GC is not needed
  *r = i ;
  return r ;
} // end of [intref_make]

ats_int_type
intref_get (ats_intref_type r) { return *((ats_int_type*)r) ; }
// end of [intref_get]
ats_void_type
intref_set (ats_intref_type r, ats_int_type i) { *((ats_int_type*)r) = i ; return ; }
// end of [intref_set]

/* ****** ****** */

ats_int_type
atslib_fork_exec_and_wait_cloptr_exn
  (ats_ptr_type f_child) {
  pid_t pid ;
  int status ;

  pid = fork () ;
  if (pid < 0) {
    ats_exit_errmsg (errno, "Exit: [fork] failed.\n") ;
  }
  if (pid > 0) {
    wait (&status) ; ATS_FREE (f_child) ; return status ;
  } // end of [if]
  /* this is the child */
  ((ats_void_type (*)(ats_clo_ptr_type))((ats_clo_ptr_type)f_child)->closure_fun)(f_child) ;
  _exit (0) ; /* no need to flush STDIN, STDOUT and STDERR */
  return 0 ; // deadcode
} // end of [atslib_fork_exec_and_wait_cloptr_exn]

/* ****** ****** */

extern ats_bool_type strlst_is_nil(ats_ptr_type) ;
extern ats_ptr_type strlst_head_get(ats_ptr_type) ;
extern ats_ptr_type strlst_tail_get(ats_ptr_type) ;

ats_void_type
strlst_to_strarr (
  ats_sum_ptr_type ss, ats_ptr_type p
) {
  while (1) {
    if (strlst_is_nil(ss)) break ;
    *((ats_ptr_type *)p) = strlst_head_get(ss) ;
    p = ((ats_ptr_type *)p) + 1 ; ss = strlst_tail_get(ss) ;
  } /* end of [while] */
  return ;
} // end of [strlst_to_strarr]

#define BUFSZ 64

ats_ptr_type
string_trans (
  ats_ptr_type s0, ats_clo_ptr_type f
) {
  int i, sz;
  char *buf0, *buf1, *p, c, *s ;
//
  sz = BUFSZ ; buf0 = ats_malloc_gc(sz) ;
//
  i = 0 ; p = buf0 ;
  while (c = *((char *)s0)) {
    s0 = (char *)s0 + 1 ;
    s = ((ats_ptr_type (*)(ats_clo_ptr_type, ats_char_type))f->closure_fun)(f, c) ;
    while (c = *s) {
      ++s ;
      if (i == sz) {
        buf1 = ats_malloc_gc (sz + sz) ;
        memcpy (buf1, buf0, sz) ;
        ats_free_gc (buf0); buf0 = buf1 ;
        p = buf0 + sz ;
        sz = sz + sz ;
      } /* end of [if] */
      *p = c ; ++i ; ++p ;
    } /* end of [while] */
  } /* end of [while] */
//
  if (i == sz) {
    buf1 = ats_malloc_gc(sz+1) ;
    memcpy (buf1, buf0, sz) ;
    ats_free_gc (buf0) ; buf0 = buf1 ;
  } /* end of [if] */
//
  buf0[i] = '\000' ; return buf0 ;
//
} // end of [string_trans]

//

//
// HX: for the purpose of bootstrapping; it is already defined in unistd.dats
//
ats_ptr_type 
atsutil_getcwd0 () {
  char *buf, *res ;
  int sz = 64 ;

  buf = ats_malloc_gc(sz) ;

  while (1) {
    res = getcwd (buf, sz) ;
    if (!res) {
      ats_free_gc (buf) ;
      sz = sz + sz ; buf = ats_malloc_gc (sz) ;
      continue ;
    } /* end of [if] */
    break ;
  } /* end of [while] */
  return buf ;
} // end of [atsutil_getcwd0]

%} // end of [%{^]

(* ****** ****** *)

(* end of [basics.dats] *)
