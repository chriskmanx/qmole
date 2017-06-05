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
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)
// Time: Summer 2007
//
// This is one of the first programs written in ATS/Geizella. The coding
// style is clearly awkward.
//

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

%{^

#include "libc/CATS/stdio.cats"
#include "libc/CATS/stdlib.cats"
#include "libc/sys/CATS/types.cats" // for [pid_t]
#include "libc/CATS/unistd.cats"

typedef ats_ptr_type ats_intref_type ;

%} // end of [%{^]

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

fn do_usage (
  cmd: string
) : void = () where {
  val () = printf ("The usage of %s is:\n", @(cmd))
  val () = printf ("  %s [flag-or-file]*\n", @(cmd))
} // end of [do_usage]

(* ****** ****** *)

#define nil STRLSTnil; #define :: STRLSTcons

fn string_is_flag
  (s: string):<fun0> bool = let
  val s = string1_of_string s in
  if string_is_empty s then false else (s[0] = '-')
end // end of [string_is_flag]

(* ****** ****** *)
//
extern fun DATS_wait_set (): void = "DATS_wait_set"
extern fun DATS_wait_is_set (): bool = "DATS_wait_is_set"
extern fun DATS_wait_clear (): void = "DATS_wait_clear"
extern fun DATS_extract (s: string): Stropt = "DATS_extract"
extern fun flag_is_DATSdef (s: string): bool = "flag_is_DATSdef"
//
extern fun IATS_wait_set (): void = "IATS_wait_set"
extern fun IATS_wait_is_set (): bool = "IATS_wait_is_set"
extern fun IATS_wait_clear (): void = "IATS_wait_clear"
extern fun IATS_extract (s: string): Stropt = "IATS_extract"
extern fun flag_is_IATSdir (s: string): bool = "flag_is_IATSdir"
//
(* ****** ****** *)

fn flag_is_m32 (flag: string): bool =
  case+ flag of | "-m32" => true | _ => false
fn flag_is_m64 (flag: string): bool =
  case+ flag of | "-m64" => true | _ => false

(* ****** ****** *)

fn flag_is_compile_only
  (flag: string):<fun0> Bool =
  case+ flag of "-cc" => true | "--compile" => true | _ => false
val is_compile_only: intref = intref_make 0
extern val "is_compile_only" = is_compile_only

(* ****** ****** *)

fn flag_is_typecheck_only
  (flag: string):<fun0> Bool =
  case+ flag of "-tc" => true | "--typecheck" => true | _ => false
val is_typecheck_only: intref = intref_make 0
extern val "is_typecheck_only" = is_typecheck_only

(* ****** ****** *)

fn flag_is_objcode_only
  (flag: string):<fun0> Bool =
  case+ flag of "-c" => true | _ => false
val is_objcode_only: intref = intref_make 0
extern val "is_objcode_only" = is_objcode_only

(* ****** ****** *)

fn flag_is_debug
  (flag: string):<fun0> Bool =
  case+ flag of "-g" => true | _ => false

fn flag_is_version (flag: string):<fun0> Bool =
  case+ flag of "-v" => true | "--version" => true | _ => false
// end of [flag_is_version]

(* ****** ****** *)

fn flag_is_ATS_GCATS (flag: string): Bool = begin
  case+ flag of "-D_ATS_GCATS" => true | _ => false
end // end of [flag_is_ATS_GCATS]
val is_ATS_GCATS: intref = intref_make 0

fn flag_is_ATS_GCATS2 (flag: string): Bool = begin
  case+ flag of "-D_ATS_GCATS2" => true | _ => false
end // end of [flag_is_ATS_GCATS2]
val is_ATS_GCATS2: intref = intref_make 0

fn flag_is_ATS_GCBDW (flag: string): Bool = begin
  case+ flag of "-D_ATS_GCBDW" => true | _ => false
end // end of [flag_is_ATS_GCBDW]
val is_ATS_GCBDW: intref = intref_make 0

(* ****** ****** *)

fn flag_is_ATS_MULTITHREAD (flag: string): Bool =
  case+ flag of "-D_ATS_MULTITHREAD" => true | _ => false
val is_ATS_MULTITHREAD = intref_make 0
extern val "is_ATS_MULTITHREAD" = is_ATS_MULTITHREAD

(* ****** ****** *)

fn flag_is_ATS_DEBUG (flag: string): Bool = begin
  case+ flag of "-D_ATS_DEBUG" => true | _ => false
end // end of [flag_is_ATS_DEBUG]
val is_ATS_DEBUG: intref = intref_make 0

(* ****** ****** *)

fn flag_is_lats (flag: string): Bool =
  case+ flag of "-lats" => true | _ => false
val is_lats: intref = intref_make 0

fn flag_is_lats_mt (flag: string): Bool =
  case+ flag of "-lats_mt" => true | _ => false
val is_lats_mt: intref = intref_make 0

(* ****** ****** *)

extern
fun atscc_outfile_name_make
  (basename: string): String = "atscc_outfile_name_make"
// end of [atscc_outfile_name_make]

(* ****** ****** *)

#define sbp2str string1_of_strbuf
  
fn atscc_argv_process
  {n:pos} {l:addr} (
  pf: !array_v (String, n, l) | n: int n, p: ptr l
) : Strlst(*param_c*) = let
//
fn* aux {i:nat | i <= n} ( // .<n-i,0>.
    pf: !array_v (String, n, l)
  | param_ats: Strlst, param_c: Strlst, i: int i
  ) :<cloptr1> Strlst(*param_c*) =
  if i < n then let
    val s = p[i] in case+ 0 of
//
    | _ when DATS_wait_is_set () => begin
        DATS_wait_clear (); aux (pf | s :: param_ats, param_c, i+1)
      end // end of [_ when ...]
//
    | _ when IATS_wait_is_set () => begin
        IATS_wait_clear (); aux (pf | s :: param_ats, param_c, i+1)
      end // end of [_ when ...]
//
    | _ when string_is_flag s => begin
        aux_flag (pf | param_ats, param_c, i, s)
      end // end of [_ when ...]
    | _ => begin
        aux_file (pf | param_ats, param_c, i, s)
      end // end of [_]
  end else if intref_get is_objcode_only > 0 then let
    val param_c = strlst_reverse param_c
    val _IATSHOME = sbp2str ("-I" + ATSHOME_dir)
    val param_c = _IATSHOME :: param_c
    val _Iruntime_global = sbp2str ("-I" + runtime_global)
    val param_c = _Iruntime_global :: param_c
  in
    param_c
  end else let
    val param_c = (case+ 0 of
      | _ when intref_get is_ATS_GCATS > 0 => let
          val is_mt = intref_get is_ATS_MULTITHREAD > 0
          val gcobj_local =
            (if is_mt then "GCATS/gc_mt.o" else "GCATS/gc.o"): string
          val gcobj_global = runtime_global + gcobj_local
        in
          gcobj_global :: param_c
        end // end of [ATS_GCATS]
      | _ when intref_get is_ATS_GCATS2 > 0 => let
          val gc_o = sbp2str (runtime_global + "GCATS2/gc.o") in
          gc_o :: param_c
        end // end of [ATS_GCATS2]
      | _ when intref_get is_ATS_GCBDW > 0 => let
(*
          val () = begin
            prerr "is_ATS_GCBDW = 1"; prerr_newline ()
          end // end of [val]
*)
// [ATS_PKGCONFIG] is declared in [prelude/params.hats]
#if (ATS_PKGCONFIG == 1) #then
          #define :: STRLSTcons; #define nil STRLSTnil
          val arglst = "bdw-gc" :: "--libs" :: nil ()
          val toks = atscc_pkgconfig (arglst, 2) where {
            extern fun atscc_pkgconfig {n:nat}
              (arglst: strlst n, narg: int n): List_vt string = "atscc_pkgconfig"
          } // end of [val]
          val param_c = loop (param_c, toks) where {
            fun loop (param_c: Strlst, toks: List_vt string): Strlst =
              case+ toks of
              | ~list_vt_cons (tok, toks) => loop (STRLSTcons (tok, param_c), toks)
              | ~list_vt_nil () => param_c
            // end of [loop]
          } // end of [val param_c]
#else // ATS_PKGCONFIG <> 1
          val gcobj_local_lib = "GCBDW/lib": string
          val gcobj_global_lib = runtime_global + gcobj_local_lib
          val param_c = ("-L" + gcobj_global_lib) :: param_c
          val param_c = "-lgc" :: param_c
#endif // end of [ATS_PKGCONFIG == 1]
        in
          param_c
        end // end of [ATS_GCBDW]
      | _ => param_c
    ) : Strlst // end of [val]
//
    val param_c = (
      if intref_get is_ATS_MULTITHREAD > 0 then
        if intref_get is_lats_mt > 0 then param_c else "-lats_mt" :: param_c
      else param_c
    ) : Strlst // end of [val]
//
    val param_c = (
      if intref_get is_lats > 0 then param_c else "-lats" :: param_c
    ) : Strlst // end of [val]
//
    val param_c = strlst_reverse param_c
    val ats_prelude_c = sbp2str (runtime_global + "ats_prelude.c")
    val param_c = ats_prelude_c :: param_c
    val _Latslib_global = sbp2str ("-L" + atslib_global ())
    val param_c = _Latslib_global :: param_c
    val _Iruntime_global = sbp2str ("-I" + runtime_global)
    val param_c = _Iruntime_global :: param_c
    val _IATSHOME = sbp2str ("-I" + ATSHOME_dir)
    val param_c = _IATSHOME :: param_c
  in
    param_c (* Strlst *)
  end // end of [aux]
//
and aux_flag {i:nat | i < n} // .<n-i-1,1>.
  (pf: !array_v (String, n, l) |
   param_ats: Strlst, param_c: Strlst, i: int i, flag: String)
  :<cloptr1> Strlst = begin case+ flag of
  | _ when flag_is_typecheck_only flag => let
      val () = intref_set (is_typecheck_only, 1)
      val param_ats = "--typecheck" :: param_ats
    in
      aux (pf | param_ats, param_c, i+1)
    end // end of [_ when flag_is_typecheck_only]
  | _ when flag_is_compile_only flag => let
      val () = intref_set (is_compile_only, 1)
    in
      aux (pf | param_ats, param_c, i+1)
    end // end of [_ when flag_is_compile_only]
  | _ when flag_is_objcode_only flag => let
      val () = intref_set (is_objcode_only, 1)
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_objcode_only]
//
  | _ when flag_is_ATS_GCATS flag => let
      val () = intref_set (is_ATS_GCATS, 1)
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_ATS_GCATS]
  | _ when flag_is_ATS_GCATS2 flag => let
      val () = intref_set (is_ATS_GCATS2, 1)
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_ATS_GCATS2]
  | _ when flag_is_ATS_GCBDW flag => let
      val () = intref_set (is_ATS_GCBDW, 1)
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_ATS_GCBDW]
//
  | _ when flag_is_ATS_MULTITHREAD flag => let
      val () = intref_set (is_ATS_MULTITHREAD, 1)
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_ATS_MULTITHREAD]
//
  | _ when flag_is_lats flag => let
      val () = intref_set (is_lats, 1)
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_lats]
  | _ when flag_is_lats_mt flag => let
      val () = intref_set (is_lats_mt, 1)
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_lats_mt]
//
  | _ when flag_is_ATS_DEBUG flag => let
      val param_ats = "--debug=1" :: param_ats
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_ATS_DEBUG]
//
  | _ when flag_is_DATSdef flag => let
      val param_ats = flag :: param_ats
      val flgval = DATS_extract flag; val () = begin
        if stropt_is_some flgval then () else DATS_wait_set ()
      end // end of [if]
    in
      aux (pf | param_ats, param_c, i+1)
    end // end of [_ when ...]
//
  | _ when flag_is_IATSdir flag => let
      val param_ats = flag :: param_ats
      val dir = IATS_extract flag; val () = begin
        if stropt_is_some dir then () else IATS_wait_set ()
      end // end of [if]
    in
      aux (pf | param_ats, param_c, i+1)
    end // end of [_ when ...]
//
  | _ when flag_is_m32 flag => let
      val () = wordsize_target_set (4(*bytes*))
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_m32]
  | _ when flag_is_m64 flag => let
      val () = wordsize_target_set (8(*bytes*))
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_m64]
//
  | _ when flag_is_debug flag => let
      val param_c = flag :: param_c
      val param_ats = "--gline" :: param_ats
    in
      aux (pf | param_ats, param_c, i+1)
    end // end of [_ when flag_is_debug]
//
  | _ when flag_is_version flag => let
      val () = atscc_version ()
    in
      aux (pf | param_ats, flag :: param_c, i+1)
    end // end of [_ when flag_is_version]
//
  | _ => aux (pf | param_ats, flag :: param_c, i+1)
end // end of [aux_flag]
//
and aux_file {i:nat | i < n} // .<n-i-1,1>.
  (pf: !array_v (String, n, l) |
   param_ats: Strlst, param_c: Strlst, i: int i, file: String)
  :<cloptr1> Strlst = let
  val sfx = suffix_of_filename file
  val flag_stadyn = (
    if stropt_is_none sfx then ~1 else begin
      case stropt_unsome sfx of "sats" => 0 | "dats" => 1 | _ => ~1
    end
  ) : int
  val flag_debug = (if intref_get (is_ATS_DEBUG) > 0 then 1 else 0): int
in
  if flag_stadyn >= 0 then
    if intref_get (is_typecheck_only) > 0 then let
      val () = typecheck_file (flag_stadyn, param_ats, file)
    in
      aux (pf | param_ats, param_c, i+1)
    end else let 
      val basename = basename_of_filename file
      val outfile_c = atscc_outfile_name_make basename
      val () = ccomp_file_to_file (flag_stadyn, param_ats, file, outfile_c)
      val param_c = outfile_c :: param_c
    in
      aux (pf | param_ats, param_c, i+1)
    end // end of [if]
  else begin
    aux (pf | param_ats, file :: param_c, i+1)
  end (* end of [if] *)
end // end of [aux_file]
//
in
  aux (pf | nil (*param_ats*), nil (*param_c*), 1)
end // end of [atscc_argv_process]

(* ****** ****** *)

extern val "atscc_argv_process" = atscc_argv_process // for use in C

(* ****** ****** *)

dynload "basics.dats"
dynload "atscc.dats"

(* ****** ****** *)

implement main_prelude () = ()

(* ****** ****** *)

extern
fun atscc_main {n:pos}
  (argc: int n, argv: &(@[string][n])): void = "atscc_main"
// end of [atscc_main]
implement main (argc, argv) = case+ argc of
  | 1 => let val cmd = argv.[0] in do_usage (basename_of_filename cmd) end
  | _ => atscc_main (argc, argv)
// end of [main]

(* ****** ****** *)


%{$

static
int the_DATS_wait = 0 ;
ats_void_type
DATS_wait_set () {
  the_DATS_wait = 1 ; return ;
}
ats_bool_type
DATS_wait_is_set () {
  return (the_DATS_wait ? ats_true_bool : ats_false_bool) ;
}
ats_void_type
DATS_wait_clear () {
  the_DATS_wait = 0 ; return ;
}

ats_bool_type
flag_is_DATSdef (ats_ptr_type s0) { return (
  strncmp((char*)s0, "-DATS", 5)==0 ? ats_true_bool : ats_false_bool
) ; } // end of [flag_is_DATSdef]

ats_ptr_type
DATS_extract (ats_ptr_type s0) {
  int n ; char* s ;
  n = strlen ((char*)s0) - 5 ;
  if (n <= 0) return (ats_ptr_type)0 ;
  s = ats_malloc_gc (n + 1) ;
  memcpy (s, (char*)s0 + 5, n) ; s[n] = '\0' ;
  return s ;
} // end of [DATS_extract]

%} // end of [%{$]

(* ****** ****** *)

%{$

static
int the_IATS_wait = 0 ;
ats_void_type
IATS_wait_set () {
  the_IATS_wait = 1 ; return ;
}
ats_bool_type
IATS_wait_is_set () {
  return (the_IATS_wait ? ats_true_bool : ats_false_bool) ;
}
ats_void_type
IATS_wait_clear () {
  the_IATS_wait = 0 ; return ;
}

ats_bool_type
flag_is_IATSdir (ats_ptr_type s0) { return (
  strncmp((char*)s0, "-IATS", 5)==0 ? ats_true_bool : ats_false_bool
) ; } // end of [flag_is_IATSdir]

ats_ptr_type
IATS_extract (ats_ptr_type s0) {
  int n ; char* s ;
  n = strlen ((char*)s0) - 5 ;
  if (n <= 0) return (ats_ptr_type)0 ;
  s = ats_malloc_gc (n + 1) ;
  memcpy (s, (char*)s0 + 5, n) ; s[n] = '\0' ;
  return s ;
} // end of [IATS_extract]

%} // end of [%{$]

(* ****** ****** *)

%{$

typedef ats_ptr_type ats_stropt_type ;
typedef ats_ptr_type ats_string_type ;
//
extern ats_ptr_type strlst_head_get(ats_ptr_type) ;
extern ats_ptr_type strlst_tail_get(ats_ptr_type) ;
//
extern ats_int_type strlst_length(ats_ptr_type) ;
extern ats_ptr_type strlst_reverse(ats_ptr_type) ;
//
extern ats_intref_type is_compile_only ;
extern ats_intref_type is_typecheck_only ;
extern ats_int_type inref_get(ats_intref_type) ;
//
ats_string_type
atscc_outfile_name_make (
  ats_string_type basename
) {
  int n ; char c, *s ;
  n = strlen((char*)basename) ;
  s = (char*)ats_malloc_gc(n+3) ;
  s[n+2] = '\000' ; s[n+1] = 'c' ; s[n] = '.' ; --n ;
  while (n >= 0) {
    c = ((char*)basename)[n] ;
    if (c == '.') { s[n] = '_' ; --n ; break ; }
    s[n] = c ; --n ;
  }
  while (n >= 0) { s[n] = ((char*)basename)[n] ; --n ; }
  return s ;
} // end of [atscc_outfile_name_make]

ats_void_type
atscc_main (
  ats_int_type argc
, ats_ptr_type argv
) {
  int i, n ;
  ats_sum_ptr_type ss ;
  ats_ptr_type argv_new, p ; pid_t pid ; int status ;

  extern ats_ptr_type ATSCCOMP_gcc ;

  char *gcc = (char*)ATSCCOMP_gcc ;

  ss = ((ats_sum_ptr_type (*)(ats_int_type, ats_ptr_type))atscc_argv_process)(argc, argv) ;

  if (intref_get(is_compile_only) > 0) return ;
  if (intref_get(is_typecheck_only) > 0) return ;

  n = strlst_length(ss) ;
  argv_new = ats_malloc_ngc ((n+1)*sizeof(ats_string_type)+sizeof(ats_stropt_type)) ;
  p = argv_new ;

  // initialization for [argv_new]
  *((ats_string_type*)p) = gcc ;
  p = ((ats_string_type*)p) + 1 ;
  for (i = 0; i < n; ++i) {
    *((ats_string_type*)p) = strlst_head_get(ss) ;
    p = ((ats_string_type*)p) + 1 ; ss = strlst_tail_get(ss) ;
  } /* end of [for] */
  *((ats_stropt_type *)p) = (ats_stropt_type)0 ;

  // printf ("argv_new = ") ;
  for (i = 0; i <= n; ++i) {
    printf ("%s ", (char*)((ats_string_type*)argv_new)[i]) ;
  } /* end of [for] */
  printf ("\n") ;

  pid = fork () ;
  if (pid < 0) {
    ats_exit_errmsg (errno, "Exit: [fork] failed.\n") ;
  } /* end of [if] */
  if (pid == 0) execvp (gcc, argv_new) ; // this is the child
  wait (&status) ; // this is the parent
  if (status) {
    atspre_exit_prerrf (status, "Exit: [%s] failed.\n", gcc) ;
  } /* end of [if] */
  return ;
} // end of [atscc_main]

%} // end of [%{$]

(* ****** ****** *)

(* end of [atscc_main.dats] *)
