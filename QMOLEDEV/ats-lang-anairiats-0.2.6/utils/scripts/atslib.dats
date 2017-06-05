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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer 2007
//
(* ****** ****** *)

staload "libc/SATS/stdio.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

extern fun ar_r_exec
  (libfile: string, objfile: string): void = "ar_r_exec"
// end of [ar_r_exec]

// archive with replacement
fn ar_r_err (libfile: string, objfile: string): int = begin
  fork_exec_and_wait_cloptr_exn (lam () => ar_r_exec (libfile, objfile))
end // end of [ar_r_err]

// this is equivalent to [ranlib]
extern fun ar_s_exec (libfile: string): void = "ar_s_exec"

implement
ar_s_exn (libfile) = let
  val status =
    fork_exec_and_wait_cloptr_exn (lam () => ar_s_exec (libfile))
  // end of [val]
in
  if (status <> 0) then
    exit_prerrf {void} (status, "exit(ATS): [ar_s(%s)] failed\n", @(libfile))
  // end of [if]
end // end of [ar_s_exn]

(* ****** ****** *)

extern fun gcc_libfile_exec
  (param_rev: Strlst, infile: string, outfile: string): void = "gcc_libfile_exec"
// end of [gcc_libfile_exec]

implement
gcc_libfile_err (param_rev, infile, outfile) = let
  val cmd = lam (): void =<cloptr1> gcc_libfile_exec (param_rev, infile, outfile)
in
  fork_exec_and_wait_cloptr_exn (cmd)
end // end of [gcc_libfile_err]

(* ****** ****** *)

#define sbp2str string1_of_strbuf

extern
fun tostringf_size {ts:types}
  (guess: size_t, fmt: printf_c ts, arg: ts):<> strptr1 = "atspre_tostringf_size"
// end of [tostringf_size]

fn char_identifize
  (c: char):<cloptr1> String =
  if char_isalnum c then tostring c
  else let
    val i = uint_of_char c
    val c1 = i / 16U and c2 = i mod 16U
    val ptr = tostringf_size (4, "_%x%x", @(c1, c2))
    val str = string_of_strptr (ptr)
  in
    string1_of_string (str)
  end // end of [if]
// end of [char_identifize]

implement
ccomp_gcc_ar_libfile
  (param_rev, infile, libfile) = let
  val sfx = suffix_of_filename infile
  val flag_stadyn = (
    if stropt_is_none sfx then begin
      exit_prerrf {int} (
        1, "exit(ATS): [%s]: no filename extension\n.", @(infile)
      ) // end of [exit_prerrf]
    end else begin case+ stropt_unsome sfx of
      | "sats" => 0
      | "dats" => 1
      | _ => exit_prerrf {int} (
          1, "exit(ATS): [%s]: unsupported filename extension\n.", @(infile)
        ) // end of [exit_prerrf]
    end // end of [if]
  ) : int
//
  val infull = (
    if filename_is_local infile then let
      val cwd = getcwd0 ()
      val str = tostringf_size
        (64, "%s/%s", @(__cast cwd, infile)) where {
        extern castfn __cast {l:agz} (x: !strptr l): string
      } // end of [val]
      val () = strptr_free (cwd)
    in
      string_of_strptr (str)
    end else infile // end of [if]
  ) : string
//
  val outbase = string_trans (infull, char_identifize)
  val outfile =
    sbp2str (atslib_output_global () + outbase)
  val outfile_c = sbp2str (outfile + ".c")
  val status = ccomp_file_to_file_err
    (flag_stadyn, STRLSTnil(*param_ats*), infile, outfile_c)
  val () = if (status <> 0) then exit_prerrf {void}
    (status, "exit(ATS): [ccomp_gcc_ar_libfile(%s)] failed: ccomp\n", @(infile))
  // end of [val]
  val outfile_o = sbp2str (outfile + ".o")
  val status = gcc_libfile_err (param_rev, outfile_c, outfile_o)
  val () = if (status <> 0) then begin exit_prerrf {void}
    (status, "exit(ATS): [ccomp_gcc_ar_libfile(%s)] failed: gcc\n", @(infile))
  end // end of [val]
  val status = ar_r_err (libfile, outfile_o)
  val () = if (status <> 0) then exit_prerrf {void}
    (status, "exit(ATS): [ccomp_gcc_ar_libfile(%s)] failed: ar\n", @(infile))
  // end of [val]
in
  printf ("The file [%s] has been compiled and archived.\n", @(infile))
end // end of [ccomp_gcc_ar_libfile]

(* ****** ****** *)

extern fun fget_line {m:fm}
  (pf: file_mode_lte (m, r) | f: &FILE m): String = "atsutil_fget_line"
// end of [fget_line]

#define i2sz size1_of_int1

fun library_make_loop
  {m:fm} {l_file:addr} (
  param_rev: Strlst, file: &FILE r, dir: String, libfilename: string
) : void = let
//
fn line_is_comment
  (name: String): bool = let
in
  if string_isnot_at_end (name, 0) then
    if name[0] = '#' then true else false
  else true // end of [if]
end // end of [line_is_comment]
//
fun split {n:int}
  {i0,i:nat | i0 <= i; i <= n} .<n-i>. (
  line: string (n), n: size_t n, i0: size_t i0, i: size_t i
) : List (string) =
  if i < n then let
    val c = line[i]
  in
    if c != ' ' then
      split (line, n, i0, i+1)
    else let
      val x = string_make_substring (line, i0, i-i0)
      val x = string_of_strbuf (x)
    in
      list_cons (x, split_skip (line, n, i+1))
    end
  end else let
    val x = string_make_substring (line, i0, n-i0)
    val x = string_of_strbuf (x)
  in
    list_cons (x, list_nil)
  end // end of [if]
and split_skip {n:int}
  {i:nat | i <= n} .<n-i>. (
  line: string (n), n: size_t n, i: size_t i
) : List (string) =
  if i < n then let
    val c = line[i]
  in
    if c != ' ' then
      split (line, n, i, i+1)
    else
      split_skip (line, n, i+1)
    // end of [if]
  end else
    list_nil () // end of [if]
// end of [split_skip]
in
  if feof (file) <> 0 then ()
  else let
    val line =
      fget_line (file_mode_lte_r_r | file)
    // end of [val]
    val () = if
      ~line_is_comment (line) then let
      val n = string_length (line)
      val xs = split (line, n, 0, 0)
    in
      case+ xs of
      | list_cons (x, xs) => let
          val x = string1_of_string x
          val dirx = sbp2str (dir + x)
          val param_rev = loop (xs, param_rev) where {
            fun loop {m,n:nat}
              (xs: list (string, m), ys: strlst n): strlst (m+n) =
              case+ xs of
              | list_cons (x, xs) => loop (xs, STRLSTcons (x, ys))
              | list_nil () => ys
            // end of [loop]
          } // end of [val]
        in
          ccomp_gcc_ar_libfile (param_rev, dirx, libfilename)
        end // end of [list_cons]
      | list_nil () => ()
    end // end of [val]
  in
    library_make_loop (param_rev, file, dir, libfilename)
  end (* end of [if] *)
end // end of [library_make_loop]

(* ****** ****** *)

implement
libats_make (param_rev) = let
  val libfiles_local = ATSHOME_dir_append ".libfiles_local"
  val libats_global = libats_global ()
  val (pf_file | p_file) = fopen_exn (libfiles_local, file_mode_r)
  val () = library_make_loop (param_rev, !p_file, ATSHOME_dir, libats_global)
  val () = fclose_exn (pf_file | p_file)
  val () = ar_s_exn (libats_global)
in
  // nothing
end // end of [libats_make]

implement
libats_mt_make (param_rev) = let
  val libfiles_mt_local = ATSHOME_dir_append ".libfiles_mt_local"
  val libats_mt_global = libats_mt_global ()
  val (pf_file | p_file) = fopen_exn (libfiles_mt_local, file_mode_r)
  val () = library_make_loop (param_rev, !p_file, ATSHOME_dir, libats_mt_global)
  val () = fclose_exn (pf_file | p_file)
  val () = ar_s_exn (libats_mt_global)
in
  // nothing
end // end of [libats_mt_make]

(* ****** ****** *)

implement
libats_lex_make (param_rev) = let
  val dir = ATSHOME_dir_append "libats/lex/"
  val libats_lex_local =
    sbp2str (atslib_local () + "libats_lex.a")
  val libats_lex_global = ATSHOME_dir_append (libats_lex_local)
in
  ccomp_gcc_ar_libfile
    (param_rev, sbp2str (dir + "lexing.sats"), libats_lex_global) ;
  ccomp_gcc_ar_libfile
    (param_rev, sbp2str (dir + "lexing.dats"), libats_lex_global) ;
  ccomp_gcc_ar_libfile
    (param_rev, sbp2str (dir + "tables.dats"), libats_lex_global) ;
  ar_s_exn (libats_lex_global) ;
end // end of [libats_lex_make]

(* ****** ****** *)

implement
libats_smlbas_make (param_rev) = () where {
  val smlbas_libfiles = ATSHOME_dir_append "libats/smlbas/.libfiles"
  val (pf_file | p_file) = fopen_exn (smlbas_libfiles, file_mode_r)
  val libats_smlbas_local = sbp2str (atslib_local () + "libats_smlbas.a")
  val libats_smlbas_global = ATSHOME_dir_append (libats_smlbas_local)
  val () = library_make_loop (param_rev, !p_file, ATSHOME_dir, libats_smlbas_global)
  val () = fclose_exn (pf_file | p_file)
  val () = ar_s_exn (libats_smlbas_global)
} // end of [libats_smlbas_make]

(* ****** ****** *)

%{^

#include <unistd.h>

#include "libc/CATS/stdio.cats"

typedef ats_ptr_type ats_string_type ;

extern ats_string_type ATSHOME_dir ;
extern ats_string_type runtime_global ;

extern ats_bool_type strlst_is_nil (ats_ptr_type) ;
extern ats_ptr_type strlst_head_get (ats_ptr_type) ;
extern ats_ptr_type strlst_tail_get (ats_ptr_type) ;

ats_void_type
gcc_libfile_exec (
  ats_ptr_type param_rev
, ats_string_type input_c
, ats_string_type output_o
) {
  int err ;
  int n, argc ; char **argv, **argv_p, **argv_p1 ;
//
  argc = n = strlst_length (param_rev) ;
  argc += 1 ; // self(*first*)
  argc += 2 ; // -I runtime_global
  argc += 2 ; // -I ATSHOME_dir
  argc += 2 ; // -c input_c
  argc += 2 ; // -o output_o
  argc += 1 ; // nullptr(*last)
  argv = (char**)malloc (argc * sizeof(ats_ptr_type)) ;
  if (!argv) {
    fprintf (stderr, "exit(ATS): gcc_libfile_exec: malloc failed!\n") ;
    exit (1) ;
  } // end of [if]
  argv_p = argv ;
  *argv_p = "gcc" ; argv_p += 1 ;
  *argv_p = "-I" ; argv_p += 1 ;
  *argv_p = runtime_global ; argv_p += 1 ;
  *argv_p = "-I" ; argv_p += 1 ;
  *argv_p = ATSHOME_dir ; argv_p += 1 ;
  argv_p += n ; argv_p1 = argv_p ; while (1) {
    if (strlst_is_nil (param_rev)) break ;
    argv_p1 -= 1 ; *argv_p1 = (char*)strlst_head_get (param_rev) ;
    param_rev = strlst_tail_get (param_rev) ;
  } // end of [while]
  *argv_p = "-c" ; argv_p += 1 ; *argv_p = input_c ; argv_p += 1 ;
  *argv_p = "-o" ; argv_p += 1 ; *argv_p = output_o ; argv_p += 1 ;
  *argv_p = (char*)0 ;
// /*
  fputs (*argv, stderr) ; argv_p = argv + 1 ;
  while (*argv_p) {
    fputc (' ', stderr) ; fputs (*argv_p, stderr) ; argv_p += 1 ;
  }
  fputc ('\n', stderr) ;
// */
  err = execvp("gcc", argv) ;
  if (err < 0) perror ("ccomp_file_to_file_exec: [execvp] failed: ") ;
  exit (1) ;
//
  return ;
} // end of [gcc_libfile_exec]

ats_void_type
ar_r_exec (ats_string_type lib, ats_string_type output_o) {
// /*
  fprintf (stderr, "ar -r %s %s\n", (char*)lib, (char*)output_o) ;
// */
  execlp("ar", "ar", "-r", lib, output_o, (char*)0) ;
  return ;
} // end of [ar_r_exec]

ats_void_type
ar_s_exec (ats_string_type lib) {
// /*
  fprintf (stderr, "ar -s %s\n", (char*)lib) ;
// */
  execlp("ar", "ar", "-s", lib, (char*)0) ;
  return ;
} // end of [ar_s_exec]

#define INCSZ 1024

ats_string_type
atsutil_fget_line
  (ats_ptr_type file) {
  int c;
  int i, sz;
  char *buf0, *buf1, *p;
//
  if (feof((FILE *)file)) {
    ats_exit_errmsg(1, (ats_string_type)"exit(ATS): [fget_line] failed: EOF\n");
  } // end of [if]
//
  sz = INCSZ;
  buf0 = (char*)ats_malloc_gc(sz); p = buf0;
//
  while (1) {
    for (i = 0; i < INCSZ; ++i) {
      c = fgetc ((FILE *)file) ;
      if (c == '\n' || c == EOF) { *p = '\000'; return buf0; }
      *p = c; ++p;
    }
    buf1 = (char*)ats_malloc_gc (sz + INCSZ) ;
    memcpy (buf1, buf0, sz) ;
    ats_free_gc (buf0) ;    
    buf0 = buf1 ; p = buf0 + sz;
    sz = sz + INCSZ ;
  } // end of [while]
//
  return (char*)0 ; // deadcode
//
} // end of [atsutil_fget_line]

%} // end of [%{^]

(* ****** ****** *)

(* end of [atslib.dats] *)
