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

staload "libc/SATS/stdlib.sats"
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/wait.sats"

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

val () = assert_prerrf_bool (
  file_is_exec atsopt_global
, "The file [%s] is not executable.\n"
, @(atsopt_global)
) // end of [val]

fn print_usage_of_ccomp_file (): void =
  print ("Usage: ccomp_file [infile] -output=[outfile]\n")
// end of [print_usage_of_comp_file]

(* ****** ****** *)

extern
fun typecheck_file_exec (
  flag_stadyn: int, param_rev: Strlst, s: string
) : void = "typecheck_file_exec"

implement
typecheck_file
  (flag_stadyn, param_rev, infile) = () where {
  val cmd = lam (): void =<cloptr1>
    typecheck_file_exec (flag_stadyn, param_rev, infile)
  val status = fork_exec_and_wait_cloptr_exn (cmd)
  val status = int1_of_int (status)
  val (pf_ifexited | ifexited) = WIFEXITED (status)
  val () = if ifexited then let
    val code = WEXITSTATUS (pf_ifexited | status) in
    if code <> 0 then exit_prerrf {void}
      (code, "exit(ATS): [typecheck_file(%s)] failed.\n", @(infile))
    // end of [if]
  end // end of [val]
  val () = if ~ifexited then begin
    prerr ("[typecheck_file] is sigtermed\n"); exit(EXIT_FAILURE)
  end // end of [val]
} (* end of [typecheck_file] *)

extern fun ccomp_file_to_file_exec
  (flag_stadyn: int, param_rev: Strlst, s1: string, s2: string): void
  = "ccomp_file_to_file_exec"

implement
ccomp_file_to_file_err
  (flag_stadyn, param_rev, infile, outfile) = let
  val cmd = lam(): void =<cloptr1>
    ccomp_file_to_file_exec (flag_stadyn, param_rev, infile, outfile)
  // end of [val]
in
  fork_exec_and_wait_cloptr_exn (cmd)
end // end of [ccomp_file_to_file_err]

implement
ccomp_file_to_file
  (flag_stadyn, param_rev, infile, outfile) = () where {
  val status = begin
    ccomp_file_to_file_err (flag_stadyn, param_rev, infile, outfile)
  end // end of [val]
  val status = int1_of_int (status)
  val (pf_ifexited | ifexited) = WIFEXITED (status)
  val () = if ifexited then let
    val code = WEXITSTATUS (pf_ifexited | status) in
    if (code <> 0) then exit_prerrf {void} (code,
      "exit(ATS): [ccomp_file_to_file(%s, %s)] failed.\n", @(infile, outfile)
    ) // end of [if]
  end // end of [val]
  val () = if ~ifexited then begin
    prerr ("[compile_file_to_file] is sigtermed\n"); exit(EXIT_FAILURE)
  end // end of [val]
} // end of [ccomp_file_to_file]

(* ****** ****** *)

extern
fun atscc_version_exec (): void = "atscc_version_exec"

implement
atscc_version () = () where { val status =
    fork_exec_and_wait_cloptr_exn (lam () => atscc_version_exec ())
  // end of [val]
  val status = int1_of_int (status)
  val (pf_ifexited | ifexited) = WIFEXITED (status)
  val () = if ifexited then let
    val code = WEXITSTATUS (pf_ifexited | status) in
    if code <> 0 then exit_prerrf {void}
      (code, "exit(ATS): [atscc_version] failed.\n", @())
    // end of [if]
  end // end of [val]
  val () = if ~ifexited then begin
    prerr ("[atscc_version] is sigtermed\n"); exit(EXIT_FAILURE)
  end // end of [val]
} // end of [atscc_version]

(* ****** ****** *)

%{^

#include <errno.h>
#include <unistd.h>
//
extern ats_ptr_type atsopt_global ;
//
extern ats_bool_type strlst_is_nil (ats_ptr_type) ;
extern ats_ptr_type strlst_head_get (ats_ptr_type) ;
extern ats_ptr_type strlst_tail_get (ats_ptr_type) ;
//
ats_void_type
ccomp_file_to_file_exec (
  ats_int_type flag_stadyn
, ats_ptr_type param_rev
, ats_ptr_type infile, ats_ptr_type outfile
) {
  int err ; char *flag_stadyn_str ;
  int n, argc ; char **argv, **argv_p, **argv_p1 ;
//
  switch (flag_stadyn) {
    case 0: flag_stadyn_str = "--static" ; break ;
    case 1: flag_stadyn_str = "--dynamic" ; break ;
    default: ats_exit_errmsg (
      1, "[ccomp_file_to_file_exec] failed: wrong flag.\n"
    ) ; break ; // end of [default]
  } // end of [switch]
//
  argc = n = strlst_length (param_rev) ;
  argc += 1 ; // self(*first*)
  argc += 2 ; // input */
  if (outfile) argc += 2 ; // output
  argc += 1 ; // nullptr(*last*)
//
  argv = (char**)malloc(argc * sizeof(ats_ptr_type)) ;
  if (!argv) {
    fprintf(stderr, "exit(ATS): ccomp_file_to_file_exec: malloc failed!\n") ;
    exit(EXIT_FAILURE) ;
  } // end of [if]
  argv_p = argv ;
  *argv_p = (char*)atsopt_global ; argv_p += 1 ;
//
  // [param_rev] is in the reversed order!!!
  argv_p += n ; argv_p1 = argv_p ; while (1) {
    if (strlst_is_nil(param_rev)) break ;
    argv_p1 -= 1 ; *argv_p1 = (char*)strlst_head_get(param_rev) ;
    param_rev = strlst_tail_get(param_rev) ;
  } // end of [while]
//
  if (outfile) {
    *argv_p = "--output" ; argv_p += 1 ; *argv_p = outfile ; argv_p += 1 ;
  } // end of [if]
//
  *argv_p = flag_stadyn_str ; argv_p += 1 ; *argv_p = infile ; argv_p += 1 ;
  *argv_p = (char*)0 ;

// /*
  fputs(*argv, stderr) ; argv_p = argv + 1 ;
  while (*argv_p) {
    fputc(' ', stderr) ; fputs(*argv_p, stderr) ; argv_p += 1 ;
  } // end of [while]
  fputc('\n', stderr) ;
// */

  err = execv((char*)atsopt_global, argv) ;
  if (err < 0) perror("ccomp_file_to_file_exec: [execv] failed: ") ;
  exit(EXIT_FAILURE) ;
  return ; // deadcode
} /* end of [ccomp_file_to_file_exec] */

ats_void_type
typecheck_file_exec (
  ats_int_type flag_stadyn
, ats_ptr_type param_rev
, ats_ptr_type infile) {
  ccomp_file_to_file_exec
    (flag_stadyn, param_rev, infile, (ats_ptr_type)0/*outfile*/) ;
  return ;
} // end of [typecheck_file_exec]

ats_void_type
atscc_version_exec () {
  int err = execl (
    (char*)atsopt_global, (char*)atsopt_global, "--version", (char*)0
  ) ;
  if (err < 0) perror("atscc_version: [execl] failed: ") ;
  exit(EXIT_FAILURE) ;
} // end of [atscc_version]

%} // end of [%{^]

(* ****** ****** *)

(* end of [atscc.dats] *)
