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
// Time: Summer, 2008
//
(* ****** ****** *)

//
// HX:
// The command [atspack] is called to make an ATS package for release
//

//
// This is done in ATS (instead of in a scripting language like PERL) largely
// because I want to test some functions declared in [libc/SATS/stdio.sats].
// Also, this exercise should help myself become a bit more familiar with the
// Linux file system in general.
//

(* ****** ****** *)
//
staload "libc/SATS/stdio.sats"
staload "libc/SATS/stdlib.sats"
//
staload "libc/SATS/dirent.sats"
//
staload STAT = "libc/sys/SATS/stat.sats"
macdef chmod_exn = $STAT.chmod_exn
macdef mkdir_exn = $STAT.mkdir_exn
//
staload TYPES = "libc/sys/SATS/types.sats"
typedef mode_t = $TYPES.mode_t
//
(* ****** ****** *)

#define ATSPACKAGE_NAME "ats-lang-anairiats"

(* ****** ****** *)

extern fun dirent_name_get (dir: &DIR): Stropt = "dirent_name_get"

%{^

extern ats_ptr_type
atspre_string_make_substring (
  const ats_ptr_type src0, const ats_int_type start, const ats_int_type len
) ; /* atspre_string_make_substring */

static inline
ats_ptr_type
dirent_name_get(ats_ptr_type dir) {
  struct dirent *ent ;
  ent = readdir((DIR*)dir) ;
  if (ent) { return
    atspre_string_make_substring (ent->d_name, 0, strlen(ent->d_name)) ;
  } else {
    return (char*)0 ;
  } // end of [if]
} /* end of [dirent_name_get] */ 

%} // end of [%{^]

(* ****** ****** *)

fn getenv_exn
  (name: string): String = let
  val (fpf_x | x) = getenv name
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
    prerr "The environment variable ["; prerr name; prerr "] is undefined!\n" ;
    exit (1)
  end (* end of [if] *)
end // end of [getenv_exn]

val ATSHOME = let
  val str = getenv_exn ("ATSHOME")
(*
  val lst = string_length (str) - 1
  val () = if str[lst] = dirsep then str[lst] = '\000' else ()
*)
in
  str // return value
end // end of [val]

(* ****** ****** *)

infixl ++
symintr ++
overload ++ with string0_append

(* ****** ****** *)

val SRCROOT = ATSHOME ++ "/"

fn ATSPACKAGE_VERSION_get (): string = let
  val name = SRCROOT ++ "VERSION.txt"
  val () = begin
    if ~(test_file_exists name) then begin
      prerr "The file ["; prerr name; prerr "] is not available.";
      prerr_newline ();
      exit (1)
    end // end of [if]
  end
  val fil = open_file_exn (name, file_mode_r)
  val version = input_line (fil)
  val () = assert_errmsg (stropt_is_some version, #LOCATION)
  val version = stropt_unsome version
  val () = close_file_exn (fil)
(*
  val () = begin
    prerr "ATSPACKAGE_VERSION_get: version = "; prerr version;
    prerr_newline ()
  end // end of [val]
*)
in
  version
end // end of [ATSPACKAGE_VERSION_get]

val ATSPACKAGE_VERSION: string = ATSPACKAGE_VERSION_get ()

val DSTROOT: string = let
  val sp =
    stringlst_concat '[ATSPACKAGE_NAME, "-", ATSPACKAGE_VERSION, "/"] in
  string_of_strptr (sp)
end // end of [DSTROOT]

(* ****** ****** *)

#define BUFSZ 8192

// there are certainly faster ways to copy files, but this code gives
// an opportunity to test ATS :)
fn fcopy_exn (src: string, dst: string): void = let
(*
  val () = begin
    prerr "fopen_exn: src = "; prerr src; prerr_newline ();
    prerr "fopen_exn: dst = "; prerr dst; prerr_newline ();
  end // end of [val]
*)
  val (pf_src | p_src) = fopen_exn (src, file_mode_r)
  val (pf_dst | p_dst) = fopen_exn (dst, file_mode_w)
(*
  val [l_buf:addr] (pf_gc, pf_buf | p_buf) = malloc_gc (BUFSZ)
*)
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  fun loop (
      pf_buf: !bytes (BUFSZ) @ p_buf | p_buf: ptr p_buf, src: &FILE r, dst: &FILE w
    ) : void =
    if feof (src) <> 0 then () else let
      val nread = fread_byte (file_mode_lte_r_r | !p_buf, BUFSZ, src)
      val () = fwrite_byte_exn (file_mode_lte_w_w | !p_buf, nread, dst)
    in
      loop (pf_buf | p_buf, src, dst)
    end // end of [loop]
  val () = loop (pf_buf | p_buf, !p_src, !p_dst)
(*
  val () = free_gc (pf_gc, pf_buf | p_buf)
*)
in
  fclose_exn (pf_src | p_src); fclose_exn (pf_dst | p_dst)
end // end of [fcopy_exn]

(* ****** ****** *)

val DIRmode: mode_t = begin
  $STAT.S_IRWXU // lor S_IRGRP lor S_IXGRP lor S_IROTH lor S_IXOTH
end // end of [DIRmode]

(* ****** ****** *)

fn file_isexi (path: string): bool = test_file_exists (path)
fn file_isdir (path: string): bool = test_file_isdir (path) > 0

(* ****** ****** *)

fn dir_copy (
  srcdir: string, dstdir: string, test: string -> bool
) : void = let
  val srcdir = string1_of_string srcdir
  and dstdir = string1_of_string dstdir
//
  macdef cp (name) = fcopy_exn
    (srcdir ++ ,(name), dstdir ++ ,(name))
//
  fun loop (
      dir: &DIR
    ) :<cloref1> void = let
    val name = dirent_name_get (dir) in
    case+ 0 of
    | _ when stropt_is_some name => let
        val name = stropt_unsome (name)
        val () = case+ name of
          | _ when test (name) => cp (name) | _ => ()
      in
        loop (dir)
      end // end of [_ when ...]
    | _ => ()
  end // end of [loop]
  val (pf_dir | p_dir) = opendir_exn (srcdir)
  val () = loop (!p_dir)
  val () = closedir_exn (pf_dir | p_dir)
in
  // empty
end // end of [dir_copy]

fn dir_copy_all (
  srcdir: string, dstdir: string
) : void =
  dir_copy (srcdir, dstdir, test) where {
  fun test (path: string) = ~file_isdir (path)
} // end of [dir_copy_all]

(* ****** ****** *)

val SRCROOTccomp = SRCROOT ++ "ccomp/"
val DSTROOTccomp = DSTROOT ++ "ccomp/"

val SRCROOTccomp_lib = SRCROOTccomp ++ "lib/"
val DSTROOTccomp_lib = DSTROOTccomp ++ "lib/"
val SRCROOTccomp_lib_output = SRCROOTccomp_lib ++ "output/"
val DSTROOTccomp_lib_output = DSTROOTccomp_lib ++ "output/"

val SRCROOTccomp_lib64 = SRCROOTccomp ++ "lib64/"
val DSTROOTccomp_lib64 = DSTROOTccomp ++ "lib64/"
val SRCROOTccomp_lib64_output = SRCROOTccomp_lib64 ++ "output/"
val DSTROOTccomp_lib64_output = DSTROOTccomp_lib64 ++ "output/"

val SRCROOTccomp_runtime = SRCROOTccomp ++ "runtime/"
val DSTROOTccomp_runtime = DSTROOTccomp ++ "runtime/"

(*
val SRCROOTccomp_runtime_NGC = SRCROOTccomp_runtime ++ "NGC/"
val DSTROOTccomp_runtime_NGC = DSTROOTccomp_runtime ++ "NGC/"
*)

val SRCROOTccomp_runtime_GCATS = SRCROOTccomp_runtime ++ "GCATS/"
val DSTROOTccomp_runtime_GCATS = DSTROOTccomp_runtime ++ "GCATS/"

(*
val SRCROOTsrc = SRCROOT ++ "src/"; val DSTROOTsrc = DSTROOT ++ "src/"
*)

(* ****** ****** *)

val the_wordsize = sizeof<ptr> : size_t

var the_wordsize_target: size_t = the_wordsize
val (pfbox_the_wordsize_target | ()) = begin
  vbox_make_view_ptr {size_t} (view@ the_wordsize_target | &the_wordsize_target)
end // end of [val]

fn wordsize_target_get (): size_t = let
  prval vbox pf = pfbox_the_wordsize_target in the_wordsize_target
end // end of [wordsize_target_get]

fn wordsize_target_set (sz: size_t): void = let
  prval vbox pf = pfbox_the_wordsize_target in the_wordsize_target := sz
end // end of [wordsize_target_get]

(* ****** ****** *)

datatype packnd =
  | PACKNDsource | PACKNDprecompiled

fn packnd_is_source (knd: packnd): bool =
  case+ knd of PACKNDsource () => true | _ => false
// end of [packnd_is_source]

fn packnd_is_precompiled (knd: packnd): bool =
  case+ knd of PACKNDprecompiled () => true | _ => false
// end of [packnd_is_precompiled]

(* ****** ****** *)

fn Anairiats_bin_dir_copy
  (knd: packnd): void = let
  val SRCROOTbin = SRCROOT ++ "bin/"
  val DSTROOTbin = DSTROOT ++ "bin/"
//
  macdef cp (name) = fcopy_exn
     (SRCROOTbin ++ ,(name), DSTROOTbin ++ ,(name))
  macdef cpx (name) = let
    val src_name = SRCROOTbin ++ ,(name)
    val dst_name = DSTROOTbin ++ ,(name)
    val () = fcopy_exn (src_name, dst_name)
    val () = chmod_exn (dst_name, $STAT.S_IRWXU)
  in
    // empty
  end // end of [cpx]
//
  val () = mkdir_exn (DSTROOTbin, DIRmode)
  // for keeping the directory from being removed
  val () = cp (".keeper")
  val () = begin
    if (packnd_is_precompiled knd) then (cpx "atscc"; cpx "atsopt")
  end // end of [begin]
in
  prerr "The [bin] directory is successfully copied.";
  prerr_newline ()
end // end of [bin_dir_copy]

(* ****** ****** *)

fun name_is_suffix (
  name: string, sfx: string
  ) : bool = let
  val [n1:int] name = string1_of_string name
  val n1 = string1_length (name)
  val [n2:int] sfx = string1_of_string sfx
  val n2 = string1_length (sfx)
in
  if n1 >= n2 then let
    val d = n1 - n2
    var i: natLte n2 = 0
    var res: bool = true
    val () = while (i < n2) (
      if name[d+i] = sfx[i] then i := i+1 else (res := false; break)
    ) // end of [val]
  in
    res
  end else
    false // [sfx] cannot be the suffix of [name]
  // end of [if]
end // end of [name_is_suffix]

fn name_is_c (name: string): bool = name_is_suffix (name, ".c")
fn name_is_cats (name: string): bool = name_is_suffix (name, ".cats")
fn name_is_dats (name: string): bool = name_is_suffix (name, ".dats")
fn name_is_hats (name: string): bool = name_is_suffix (name, ".hats")
fn name_is_sats (name: string): bool = name_is_suffix (name, ".sats")
fn name_is_html (name: string): bool = name_is_suffix (name, ".html")

fn name_is_xats (name: string): bool = let
  val name = string1_of_string name
  val n = string_length (name)
in
  if (n >= 5) then
    if (name[n-5] <> '.') then false
    else if name[n-3] <> 'a' then false
    else if name[n-2] <> 't' then false
    else if name[n-1] <> 's' then false
    else true
  else false
end // end of [name_is_xats]

(* ****** ****** *)

fn Anairiats_bootstrap_dir_copy () = let
  val SRCROOTbootstrap = SRCROOT ++ "bootstrap1/"
  val DSTROOTbootstrap = DSTROOT ++ "bootstrap1/"
//
  fn test
    (name: string): bool = begin
    case+ name of
    | _ when name_is_c (name) => true
    | _ when name_is_cats (name) => true
    | _ => false
  end // end of [test]
//
  macdef cp (name) = fcopy_exn
     (SRCROOTbootstrap ++ ,(name), DSTROOTbootstrap ++ ,(name))
//
  val () = mkdir_exn (DSTROOTbootstrap, DIRmode)
  val () = dir_copy (SRCROOTbootstrap, DSTROOTbootstrap, test)
  val () = cp "ats_grammar_yats.h"
in
  prerr "The [bootstrap] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_bootscrap_dir_copy]

(* ****** ****** *)

fn Anairiats_ccomp_lib_dir_copy
  (wsz: size_t, knd: packnd): void = let
  val wsz = size1_of_size (wsz) // no-op casting
  val () = mkdir_exn (DSTROOTccomp_lib, DIRmode)
  val () = mkdir_exn (DSTROOTccomp_lib64, DIRmode)
  macdef cp32 (name) =
    fcopy_exn (SRCROOTccomp_lib ++ ,(name), DSTROOTccomp_lib ++ ,(name))
  macdef cp64 (name) =
    fcopy_exn (SRCROOTccomp_lib64 ++ ,(name), DSTROOTccomp_lib64 ++ ,(name))
  val () = if
    (packnd_is_precompiled knd) then let
    val () = if
      (wsz = 4(*bytes*)) then let
      val () = cp32 ("libats.a")
      val () = cp32 ("libats_mt.a")
      val () = cp32 ("libats_smlbas.a")
    in
      // nothing
    end // end of [val]
    val () = if
      (wsz = 8(*bytes*)) then let
      val () = cp64 ("libats.a")
      val () = cp64 ("libats_mt.a")
      val () = cp64 ("libats_smlbas.a")
    in
      // nothing
    end // end of [val]
  in
    // nothing
  end // end of [val]
  val () = mkdir_exn (DSTROOTccomp_lib_output, DIRmode)
  val () = mkdir_exn (DSTROOTccomp_lib64_output, DIRmode)
  val () = fcopy_exn // keeping the directory from being removed
     (SRCROOTccomp_lib_output ++ ".keeper", DSTROOTccomp_lib_output ++ ".keeper")
  // end of [val]
  val () = fcopy_exn // keeping the directory from being removed
     (SRCROOTccomp_lib64_output ++ ".keeper", DSTROOTccomp_lib64_output ++ ".keeper")
  // end of [val]
in
  // empty
end // end of [Anairiats_ccomp_lib_dir_copy]

(* ****** ****** *)

(*

fn Anairiats_ccomp_runtime_NGC_dir_copy
  (knd: packnd): void = () where {
  val () = mkdir_exn (DSTROOTccomp_runtime_NGC, DIRmode)
  macdef cp (name) = fcopy_exn (
    SRCROOTccomp_runtime_NGC ++ ,(name), DSTROOTccomp_runtime_NGC ++ ,(name)
  ) // end of [fcopy_exn]
  val () = cp "gc.h"
  val () = prerr "The [ccomp/runtime/NGC] directory is successfully copied.";
  val () = prerr_newline ()
} // end of [Anairiats_ccomp_runtime_NGC_dir_copy]

*)

(* ****** ****** *)

fn Anairiats_ccomp_runtime_GCATS_dir_copy
  (knd: packnd): void = let
  fn test (name: string): bool = begin case+ name of
    | _ when name_is_xats (name) => true | _ => false
  end // end of [filename_test]
//
  macdef cp (name) = fcopy_exn (
    SRCROOTccomp_runtime_GCATS ++ ,(name), DSTROOTccomp_runtime_GCATS ++ ,(name)
  ) // end of [fcopy_exn]
//
  val () = mkdir_exn (DSTROOTccomp_runtime_GCATS, DIRmode)
  val () = dir_copy (
    SRCROOTccomp_runtime_GCATS, DSTROOTccomp_runtime_GCATS, test
  ) // end of [dir_copy]
  val () = begin
    cp "Makefile"; cp "README"; // cp "gc.h"; // no longer used
  end // end of [val]
  val () = begin
    if (packnd_is_precompiled knd) then (cp "gc.o"; cp "gc_mt.o")
  end // end of [val]
in
  prerr "The [ccomp/runtime/GCATS] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_ccomp_runtime_GCATS_dir_copy]

(* ****** ****** *)

fn Anairiats_ccomp_runtime_dir_copy
  (knd: packnd): void = let
  macdef cp (name) = fcopy_exn (
    SRCROOTccomp_runtime ++ ,(name), DSTROOTccomp_runtime ++ ,(name)
  )
  val () = mkdir_exn (DSTROOTccomp_runtime, DIRmode)
in
  cp "ats_config.h";
  cp "ats_basics.h";
  cp "ats_types.h";
  cp "ats_exception.h";
  cp "ats_memory.h";
  cp "ats_bootstrap.c";
  cp "ats_prelude.c";
  cp "ats_prelude_ngc.c";
  cp "ats_prelude_gcats.c";
  cp "ats_prelude_gcbdw.c";
  // ccomp_runtime_NGC_dir_copy (knd); // no longer in use
  Anairiats_ccomp_runtime_GCATS_dir_copy (knd);
  prerr "The [ccomp/runtime] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_ccomp_runtime_dir_copy]

(* ****** ****** *)

fn Anairiats_ccomp_dir_copy
  (knd: packnd): void = let
  val () = mkdir_exn (DSTROOTccomp, DIRmode)
  val () = let
    val wsz = wordsize_target_get () in Anairiats_ccomp_lib_dir_copy (wsz, knd)
  end // end of [val]
  val () = Anairiats_ccomp_runtime_dir_copy (knd)
in
  // empty
end // end of [Anairiats_ccomp_dir_copy]

(* ****** ****** *)

fn Anairiats_doc_dir_copy () = let
  val SRCROOTdoc = SRCROOT ++ "doc/"
  val DSTROOTdoc = DSTROOT ++ "doc/"
  val () = mkdir_exn (DSTROOTdoc, DIRmode)
  val () = fcopy_exn (
    SRCROOTdoc ++ "FAQ.txt", DSTROOTdoc ++ "FAQ.txt"
  ) // end of [fcopy_exn]
//
  val () = () where {
    val SRCROOTdoc_BOOK = SRCROOTdoc ++ "BOOK/"
    val DSTROOTdoc_BOOK = DSTROOTdoc ++ "BOOK/"
    val () = mkdir_exn (DSTROOTdoc_BOOK, DIRmode)
    val SRCROOTdoc_BOOK_manual = SRCROOTdoc_BOOK ++ "manual/"
    val DSTROOTdoc_BOOK_manual = DSTROOTdoc_BOOK ++ "manual/"
    val () = mkdir_exn (DSTROOTdoc_BOOK_manual, DIRmode)
    val () = fcopy_exn (
      SRCROOTdoc_BOOK_manual ++ "manual_main.pdf"
  ,   DSTROOTdoc_BOOK_manual ++ "manual_main.pdf"
    ) // end of [fcopy_exn]
  } // end of [val]
//
  val SRCROOTdoc_EXAMPLE = SRCROOTdoc ++ "EXAMPLE/"
  val DSTROOTdoc_EXAMPLE = DSTROOTdoc ++ "EXAMPLE/"
  val () = mkdir_exn (DSTROOTdoc_EXAMPLE, DIRmode)
//  
  val () = () where {
    val SRCROOTdoc_EXAMPLE_INTRO = SRCROOTdoc_EXAMPLE ++ "INTRO/"
    val DSTROOTdoc_EXAMPLE_INTRO = DSTROOTdoc_EXAMPLE ++ "INTRO/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_INTRO, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_INTRO ++ ,(name), DSTROOTdoc_EXAMPLE_INTRO ++ ,(name)
    )
    val () = cp "Makefile"
    val () = cp "HelloWorld.dats"
    val () = cp "f91.dats"
    val () = cp "fact1.dats"
    val () = cp "fact2.dats"
    val () = cp "fact3.dats"
    val () = cp "fact4.dats"
    val () = cp "fact_in_c.dats"
    val () = cp "fibs.dats"
    val () = cp "revarr.dats"
    val () = cp "revstr.dats"
    val () = cp "tally.dats"
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_MISC = SRCROOTdoc_EXAMPLE ++ "MISC/"
    val DSTROOTdoc_EXAMPLE_MISC = DSTROOTdoc_EXAMPLE ++ "MISC/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_MISC, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_MISC ++ ,(name), DSTROOTdoc_EXAMPLE_MISC ++ ,(name)
    )
    val () = cp "Makefile"
    val () = cp "AutoDiff.dats"
    val () = cp "BlackScholes.dats"
    val () = cp "coin_flip.dats"
    val () = cp "curve.dats"
    val () = cp "fft.dats"
    val () = cp "GarsiaWachs.dats"
    val () = cp "GaussElim.dats"
    val () = cp "gcd_mt.dats"
    val () = cp "hamming_lazy.dats"
    val () = cp "hanoi.dats"
    val () = cp "isqrt.dats"
    val () = cp "kmp.dats"
    val () = cp "longestline.dats"
    val () = cp "montecarlo.dats"
    val () = cp "mycat.dats"
    val () = cp "myfind.dats"
    val () = cp "passwdgen.dats"
    val () = cp "pi_lazy.dats"
    val () = cp "Peano.dats"
    val () = cp "permute.dats"
    val () = cp "permute_vt.dats"
    val () = cp "quicksort_list.dats"
    val () = cp "quicksort1_list.dats"
    val () = cp "quicksort2_list.dats"
    val () = cp "queens.dats"
    val () = cp "queens_appel.dats"
    val () = cp "queens_lazy_vt.dats"
    val () = cp "sieve.dats"
    val () = cp "sieve_lazy.dats"
    val () = cp "sieve_lazy_vt.dats"
    val () = cp "strmat.dats"
    val () = cp "sumup.dats"
    val () = cp "systemf_programming_examples.dats"
    val () = cp "tetrix.dats"
    val () = cp "wc.dats"
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_MISC = SRCROOTdoc_EXAMPLE ++ "MISC/Twentyfour/"
    val DSTROOTdoc_EXAMPLE_MISC = DSTROOTdoc_EXAMPLE ++ "MISC/Twentyfour/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_MISC, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_MISC ++ ,(name), DSTROOTdoc_EXAMPLE_MISC ++ ,(name)
    )
    val () = cp "Makefile"
    val () = cp "rational.sats"
    val () = cp "rational.dats"
    val () = cp "twentyfour.dats"
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_MISC = SRCROOTdoc_EXAMPLE ++ "MISC/HttpServer/"
    val DSTROOTdoc_EXAMPLE_MISC = DSTROOTdoc_EXAMPLE ++ "MISC/HttpServer/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_MISC, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_MISC ++ ,(name), DSTROOTdoc_EXAMPLE_MISC ++ ,(name)
    )
    val () = cp "Makefile"
    val () = cp "server.dats"
    val () = cp "server2.dats"
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_TEST = SRCROOTdoc_EXAMPLE ++ "TEST/"
    val DSTROOTdoc_EXAMPLE_TEST = DSTROOTdoc_EXAMPLE ++ "TEST/"
      val () = mkdir_exn (DSTROOTdoc_EXAMPLE_TEST, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_TEST ++ ,(name), DSTROOTdoc_EXAMPLE_TEST ++ ,(name)
    )
//
    val () = cp "Makefile"
//
    val () = cp "prelude_array.dats"
    val () = cp "prelude_array0.dats"
    val () = cp "prelude_bool.dats"
    val () = cp "prelude_char.dats"
    val () = cp "prelude_float.dats"
    val () = cp "prelude_list.dats"  
    val () = cp "prelude_list_vt.dats"
    val () = cp "prelude_matrix.dats"
    val () = cp "prelude_matrix0.dats"
    val () = cp "prelude_string.dats"
//
    val () = cp "prelude_lazy_vt.dats"
//
    val () = cp "libc_math.dats"
    val () = cp "libc_complex.dats"
    val () = cp "libc_dirent.dats"
    val () = cp "libc_dlfcn.dats"
    val () = cp "libc_gmp.dats"
    val () = cp "libc_printf.dats"
    val () = cp "libc_pwd.dats"
    val () = cp "libc_random.dats"
    val () = cp "libc_sched.dats"
    val () = cp "libc_stdio.dats"
    val () = cp "libc_stdlib.dats"
    val () = cp "libc_time.dats"
    val () = cp "libc_unistd.dats"
//
    val () = cp "libc_gdbm_gdbm.dats"
    val () = cp "libc_gdbm_ndbm.dats"
//
    val () = cp "libc_sys_utsname.dats"
//
    val () = cp "libats_funarray_braun.dats"
    val () = cp "libats_fundeque_fingertree.dats"
    val () = cp "libats_funheap_braun.dats"
    val () = cp "libats_funmap_avltree.dats"
    val () = cp "libats_funmap_rbtree.dats"
    val () = cp "libats_funralist_nested.dats"
    val () = cp "libats_funset_avltree.dats"
    val () = cp "libats_funset_listord.dats"
    val () = cp "libats_genarrays.dats"  
    val () = cp "libats_hashtable_chain.dats"
    val () = cp "libats_hashtable_linprb.dats"
    val () = cp "libats_intinf.dats"
    val () = cp "libats_iterint.dats"
    val () = cp "libats_linbitvec.dats"
    val () = cp "libats_linmap_avltree.dats"
    val () = cp "libats_linmap_rbtree.dats"
    val () = cp "libats_linqueue_arr.dats"
    val () = cp "libats_linqueue_lst.dats"
    val () = cp "libats_linordset_randbst.dats"
    val () = cp "libats_linstack_arr.dats"
    val () = cp "libats_rarray.dats"
    val () = cp "libats_regexp.dats"
    val () = cp "libats_parworkshop.dats"  
    val () = cp "libats_vector.dats"
//
    val () = cp "libats_ngc_slist.dats"
    val () = cp "libats_ngc_dlist.dats"
//
    val () = cp "libats_smlbas_array.dats"
    val () = cp "libats_smlbas_array2.dats"
    val () = cp "libats_smlbas_char.dats"
    val () = cp "libats_smlbas_list.dats"
    val () = cp "libats_smlbas_string.dats"
    val () = cp "libats_smlbas_time.dats"
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_KR =
      SRCROOTdoc_EXAMPLE ++ "KernighanRitchie/"
    val DSTROOTdoc_EXAMPLE_KR =
      DSTROOTdoc_EXAMPLE ++ "KernighanRitchie/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_KR, DIRmode)
    #define NCHAPTER 8
    val () = fcopy_exn (
      SRCROOTdoc_EXAMPLE_KR ++ "Makefile", DSTROOTdoc_EXAMPLE_KR ++ "Makefile"
    ) // end of [val]
    var !p_arr with pf_arr = @[string](
      "Chapter01/", "Chapter02/", "Chapter03/", "Chapter04/"
    , "Chapter05/", "Chapter06/", "Chapter07/", "Chapter08/"
    ) // end of [var]
    val () = loop (!p_arr, 0) where {
      fun loop (
          names: &(@[string][NCHAPTER]), i: natLte NCHAPTER
        ) :<cloref1> void =
        if i < NCHAPTER then let
          val name = names.[i]
          val src = SRCROOTdoc_EXAMPLE_KR ++ name
          val dst = DSTROOTdoc_EXAMPLE_KR ++ name
          val () = mkdir_exn (dst, DIRmode)
          val () = fcopy_exn (src ++ "Makefile", dst ++ "Makefile")
          val () = dir_copy (src, dst, name_is_xats)
        in
          loop (names, i+1)
        end // end of [if]
    } // end of [val]
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_AUP = SRCROOTdoc_EXAMPLE ++ "AUP/"
    val DSTROOTdoc_EXAMPLE_AUP = DSTROOTdoc_EXAMPLE ++ "AUP/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_AUP, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_AUP ++ ,(name), DSTROOTdoc_EXAMPLE_AUP ++ ,(name)
    ) // end of [macdef]
    val () = cp "Makefile"
    val () = cp "README"
    val () = dir_copy
      (SRCROOTdoc_EXAMPLE_AUP, DSTROOTdoc_EXAMPLE_AUP, name_is_xats)
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_AUP ++ "data", DIRmode)
    val () = cp "data/fruits.txt"
    val SRCROOTdoc_EXAMPLE_AUP_utils = SRCROOTdoc_EXAMPLE_AUP ++ "utils/"
    val DSTROOTdoc_EXAMPLE_AUP_utils = DSTROOTdoc_EXAMPLE_AUP ++ "utils/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_AUP_utils, DIRmode)
    val () = dir_copy
      (SRCROOTdoc_EXAMPLE_AUP_utils, DSTROOTdoc_EXAMPLE_AUP_utils, name_is_c)
    val () = dir_copy
      (SRCROOTdoc_EXAMPLE_AUP_utils, DSTROOTdoc_EXAMPLE_AUP_utils, name_is_xats)
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_MULTICORE = SRCROOTdoc_EXAMPLE ++ "MULTICORE/"
    val DSTROOTdoc_EXAMPLE_MULTICORE = DSTROOTdoc_EXAMPLE ++ "MULTICORE/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_MULTICORE, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_MULTICORE ++ ,(name), DSTROOTdoc_EXAMPLE_MULTICORE ++ ,(name)
    ) // end of [macdef]
    val () = cp "Makefile"
    val () = dir_copy
      (SRCROOTdoc_EXAMPLE_MULTICORE, DSTROOTdoc_EXAMPLE_MULTICORE, name_is_xats)
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_cairo = SRCROOTdoc_EXAMPLE ++ "cairo/"
    val DSTROOTdoc_EXAMPLE_cairo = DSTROOTdoc_EXAMPLE ++ "cairo/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_cairo, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_cairo ++ ,(name), DSTROOTdoc_EXAMPLE_cairo ++ ,(name)
    ) // end of [macdef]
    val () = cp "Makefile"
    val () = dir_copy
      (SRCROOTdoc_EXAMPLE_cairo, DSTROOTdoc_EXAMPLE_cairo, name_is_xats)
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_GTK = SRCROOTdoc_EXAMPLE ++ "GTK/"
    val DSTROOTdoc_EXAMPLE_GTK = DSTROOTdoc_EXAMPLE ++ "GTK/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_GTK, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_GTK ++ ,(name), DSTROOTdoc_EXAMPLE_GTK ++ ,(name)
    ) // end of [macdef]
    val () = cp "Makefile"
    val () = dir_copy
      (SRCROOTdoc_EXAMPLE_GTK, DSTROOTdoc_EXAMPLE_GTK, name_is_xats)
  } // end of [val]
//
  val () = () where {
    val SRCROOTdoc_EXAMPLE_OpenGL = SRCROOTdoc_EXAMPLE ++ "OpenGL/"
    val DSTROOTdoc_EXAMPLE_OpenGL = DSTROOTdoc_EXAMPLE ++ "OpenGL/"
    val () = mkdir_exn (DSTROOTdoc_EXAMPLE_OpenGL, DIRmode)
    macdef cp (name) = fcopy_exn (
      SRCROOTdoc_EXAMPLE_OpenGL ++ ,(name), DSTROOTdoc_EXAMPLE_OpenGL ++ ,(name)
    ) // end of [macdef]
    val () = cp "Makefile"
    val () = dir_copy
      (SRCROOTdoc_EXAMPLE_OpenGL, DSTROOTdoc_EXAMPLE_OpenGL, name_is_xats)
  } // end of [val]
//
in
  prerr "The [doc] directory is successfully copied."; prerr_newline ()
end // end of [Anairiats_doc_dir_copy]

(* ****** ****** *)

fn libdir_copy (
  srclibname: string, dstlibname: string
) : void = let
  val srclibname = string1_of_string srclibname
  and dstlibname = string1_of_string dstlibname
//
  val srclibname_SATS = srclibname ++ "SATS/"
  val dstlibname_SATS = dstlibname ++ "SATS/"
  val () = mkdir_exn (dstlibname_SATS, DIRmode)
  val () = dir_copy
    (srclibname_SATS, dstlibname_SATS, name_is_sats)
//
  // DATS is optional
  val srclibname_DATS = srclibname ++ "DATS/"
  val () = if
    file_isexi (srclibname_DATS) then let
    val dstlibname_DATS = dstlibname ++ "DATS/"
    val () = mkdir_exn (dstlibname_DATS, DIRmode)
  in
    dir_copy (srclibname_DATS, dstlibname_DATS, name_is_dats)
  end // end of [val]
//
  // CATS is optional
  val srclibname_CATS = srclibname ++ "CATS/"
  val () = if
    file_isexi (srclibname_CATS) then let
    val dstlibname_CATS = dstlibname ++ "CATS/"
    val () = mkdir_exn (dstlibname_CATS, DIRmode)
  in
    dir_copy (srclibname_CATS, dstlibname_CATS, name_is_cats)
  end // end of [val]
//
  // HATS is optional
  val srclibname_HATS = srclibname ++ "HATS/"
  val () = if
    file_isexi (srclibname_HATS) then let
    val dstlibname_HATS = dstlibname ++ "HATS/"
    val () = mkdir_exn (dstlibname_HATS, DIRmode)
  in
    dir_copy (srclibname_HATS, dstlibname_HATS, name_is_hats)
  end // end of [val]
in
  // empty
end // end of [libdir_copy]

(* ****** ****** *)

fn Anairiats_prelude_dir_copy () = let
  val SRCROOTprelude = SRCROOT ++ "prelude/"
  val DSTROOTprelude = DSTROOT ++ "prelude/"
  macdef cp (name) = fcopy_exn (
    SRCROOTprelude ++ ,(name), DSTROOTprelude ++ ,(name)
  )
  val () = mkdir_exn (DSTROOTprelude, DIRmode)
  val () = libdir_copy (SRCROOTprelude, DSTROOTprelude)
  val () = cp "fixity.ats"
  val () = cp "basics_sta.sats"
  val () = cp "basics_dyn.sats"
  val () = cp "macrodef.sats"
  val () = cp "params.hats"
  val () = cp "params_system.hats"
  val () = cp "sortdef.sats"
  val () = cp "ats_main_prelude.dats"
in
  prerr "The [prelude] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_prelude_dir_copy]

fn Anairiats_libc_dir_copy () = let
  val SRCROOTlibc = SRCROOT ++ "libc/"
  val DSTROOTlibc = DSTROOT ++ "libc/"
  val () = mkdir_exn (DSTROOTlibc, DIRmode)
  val () = libdir_copy (SRCROOTlibc, DSTROOTlibc)
//
  val SRCROOTlibc_sys = SRCROOTlibc ++ "sys/"
  val DSTROOTlibc_sys = DSTROOTlibc ++ "sys/"
  val () = mkdir_exn (DSTROOTlibc_sys, DIRmode)
  val () = libdir_copy (SRCROOTlibc_sys, DSTROOTlibc_sys)
//
  val SRCROOTlibc_arpa = SRCROOTlibc ++ "arpa/"
  val DSTROOTlibc_arpa = DSTROOTlibc ++ "arpa/"
  val () = mkdir_exn (DSTROOTlibc_arpa, DIRmode)
  val () = libdir_copy (SRCROOTlibc_arpa, DSTROOTlibc_arpa)
//
  val SRCROOTlibc_netinet = SRCROOTlibc ++ "netinet/"
  val DSTROOTlibc_netinet = DSTROOTlibc ++ "netinet/"
  val () = mkdir_exn (DSTROOTlibc_netinet, DIRmode)
  val () = libdir_copy (SRCROOTlibc_netinet, DSTROOTlibc_netinet)
//
  val SRCROOTlibc_gdbm = SRCROOTlibc ++ "gdbm/"
  val DSTROOTlibc_gdbm = DSTROOTlibc ++ "gdbm/"
  val () = mkdir_exn (DSTROOTlibc_gdbm, DIRmode)
  val () = libdir_copy (SRCROOTlibc_gdbm, DSTROOTlibc_gdbm)
//
in
  prerr "The [libc] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_libc_dir_copy]

fn Anairiats_libats_dir_copy () = let
  // the code for libats
  val SRCROOTlibats = SRCROOT ++ "libats/"
  val DSTROOTlibats = DSTROOT ++ "libats/"
  val () = mkdir_exn (DSTROOTlibats, DIRmode)
  val () = libdir_copy (SRCROOTlibats, DSTROOTlibats)
  // the code for [libats/ngc]
  val SRCROOTlibats_ngc = SRCROOTlibats ++ "ngc/"
  val DSTROOTlibats_ngc = DSTROOTlibats ++ "ngc/"
  val () = mkdir_exn (DSTROOTlibats_ngc, DIRmode)
  val () = libdir_copy (SRCROOTlibats_ngc, DSTROOTlibats_ngc)
  // the code for sml basis library is in [libats/smlbas]
  val SRCROOTlibats_smlbas = SRCROOTlibats ++ "smlbas/"
  val DSTROOTlibats_smlbas = DSTROOTlibats ++ "smlbas/"
  val () = mkdir_exn (DSTROOTlibats_smlbas, DIRmode)
  val () = fcopy_exn (
    SRCROOTlibats_smlbas++".libfiles", DSTROOTlibats_smlbas++".libfiles"
  ) (* end of [val] *)
  val () = libdir_copy (SRCROOTlibats_smlbas, DSTROOTlibats_smlbas)
  // the code for ATS lexer is in [libats/lex]
  val SRCROOTlibatslex = SRCROOTlibats ++ "lex/"
  val DSTROOTlibatslex = DSTROOTlibats ++ "lex/"
  val () = mkdir_exn (DSTROOTlibatslex, DIRmode)
  val () = dir_copy (SRCROOTlibatslex, DSTROOTlibatslex, name_is_xats)
in
  prerr "The [libats] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_libats_dir_copy]

fn Anairiats_contrib_dir_copy
  (knd: packnd) = let
  val SRCROOTcontrib = SRCROOT ++ "contrib/"
  val DSTROOTcontrib = DSTROOT ++ "contrib/"
  val () = mkdir_exn (DSTROOTcontrib, DIRmode)
//
  val () = () where { // API for parcomb: [contrib/parcomb]
    val SRCROOTcontrib_parcomb = SRCROOTcontrib ++ "parcomb/"
    val DSTROOTcontrib_parcomb = DSTROOTcontrib ++ "parcomb/"
    val () = mkdir_exn (DSTROOTcontrib_parcomb, DIRmode)
//
    val filename = "Makefile"
    val () = fcopy_exn (
      SRCROOTcontrib_parcomb++filename, DSTROOTcontrib_parcomb++filename
    ) // end of [val]
    val filename = "dynloadall.dats"
    val () = fcopy_exn (
      SRCROOTcontrib_parcomb++filename, DSTROOTcontrib_parcomb++filename
    ) // end of [val]
//
    val () = libdir_copy (SRCROOTcontrib_parcomb, DSTROOTcontrib_parcomb)
    val SRCROOTcontrib_parcomb_TEST = SRCROOTcontrib_parcomb ++ "TEST/"
    val DSTROOTcontrib_parcomb_TEST = DSTROOTcontrib_parcomb ++ "TEST/"
    val () = mkdir_exn (DSTROOTcontrib_parcomb_TEST, DIRmode)
    val () = dir_copy_all (SRCROOTcontrib_parcomb_TEST, DSTROOTcontrib_parcomb_TEST)
  } // end of [where]
//
  val () = () where { // API for cblas: [contrib/cblas]
    val SRCROOTcontrib_cblas = SRCROOTcontrib ++ "cblas/"
    val DSTROOTcontrib_cblas = DSTROOTcontrib ++ "cblas/"
    val () = mkdir_exn (DSTROOTcontrib_cblas, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_cblas++"Makefile", DSTROOTcontrib_cblas++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_cblas, DSTROOTcontrib_cblas)
    val SRCROOTcontrib_cblas_TEST = SRCROOTcontrib_cblas ++ "TEST/"
    val DSTROOTcontrib_cblas_TEST = DSTROOTcontrib_cblas ++ "TEST/"
    val () = mkdir_exn (DSTROOTcontrib_cblas_TEST, DIRmode)
    val () = dir_copy_all (SRCROOTcontrib_cblas_TEST, DSTROOTcontrib_cblas_TEST)
  } // end of [where]
//
  val () = () where { // API for clapack: [contrib/clapack]
    val SRCROOTcontrib_clapack = SRCROOTcontrib ++ "clapack/"
    val DSTROOTcontrib_clapack = DSTROOTcontrib ++ "clapack/"
    val () = mkdir_exn (DSTROOTcontrib_clapack, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_clapack++"Makefile", DSTROOTcontrib_clapack++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_clapack, DSTROOTcontrib_clapack)
    val SRCROOTcontrib_clapack_TEST = SRCROOTcontrib_clapack ++ "TEST/"
    val DSTROOTcontrib_clapack_TEST = DSTROOTcontrib_clapack ++ "TEST/"
    val () = mkdir_exn (DSTROOTcontrib_clapack_TEST, DIRmode)
    val () = dir_copy_all (SRCROOTcontrib_clapack_TEST, DSTROOTcontrib_clapack_TEST)
  } // end of [where]
//
  val () = () where { // API for glib: [contrib/glib]
    val SRCROOTcontrib_glib = SRCROOTcontrib ++ "glib/"
    val DSTROOTcontrib_glib = DSTROOTcontrib ++ "glib/"
    val () = mkdir_exn (DSTROOTcontrib_glib, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_glib++"Makefile", DSTROOTcontrib_glib++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_glib, DSTROOTcontrib_glib)
    val DSTROOTcontrib_glib_SATS_glib = DSTROOTcontrib_glib ++ "SATS/glib/"
    val () = mkdir_exn (DSTROOTcontrib_glib_SATS_glib, DIRmode)
    val () = dir_copy (
      SRCROOTcontrib ++ "glib/SATS/glib/", DSTROOTcontrib_glib_SATS_glib, name_is_sats
    ) // end of [val]
    val DSTROOTcontrib_glib_SATS_gobject = DSTROOTcontrib_glib ++ "SATS/gobject/"
    val () = mkdir_exn (DSTROOTcontrib_glib_SATS_gobject, DIRmode)
    val () = dir_copy (
      SRCROOTcontrib ++ "glib/SATS/gobject/", DSTROOTcontrib_glib_SATS_gobject, name_is_sats
    ) // end of [val]
    val DSTROOTcontrib_glib_CATS_glib = DSTROOTcontrib_glib ++ "CATS/glib/"
    val () = mkdir_exn (DSTROOTcontrib_glib_CATS_glib, DIRmode)
    val () = dir_copy (
      SRCROOTcontrib ++ "glib/CATS/glib/", DSTROOTcontrib_glib_CATS_glib, name_is_cats
    ) // end of [val]
    val () = fcopy_exn (
      SRCROOTcontrib ++ "glib/HATS/glibconfig_hats", DSTROOTcontrib ++ "glib/HATS/glibconfig_hats"
    ) // end of [val]
  } // end of [where]
//
  val () = () where { // API for cairo: [contrib/cairo]
    val SRCROOTcontrib_cairo = SRCROOTcontrib ++ "cairo/"
    val DSTROOTcontrib_cairo = DSTROOTcontrib ++ "cairo/"
    val () = mkdir_exn (DSTROOTcontrib_cairo, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_cairo++"Makefile", DSTROOTcontrib_cairo++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_cairo, DSTROOTcontrib_cairo)
  } // end of [where]
//
  val () = () where { // API for pango: [contrib/pango]
    val SRCROOTcontrib_pango = SRCROOTcontrib ++ "pango/"
    val DSTROOTcontrib_pango = DSTROOTcontrib ++ "pango/"
    val () = mkdir_exn (DSTROOTcontrib_pango, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_pango++"Makefile", DSTROOTcontrib_pango++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_pango, DSTROOTcontrib_pango)
    val DSTROOTcontrib_pango_SATS_pango = DSTROOTcontrib_pango ++ "SATS/pango/"
    val () = mkdir_exn (DSTROOTcontrib_pango_SATS_pango, DIRmode)
    val () = dir_copy (
      SRCROOTcontrib ++ "pango/SATS/pango/", DSTROOTcontrib_pango_SATS_pango, name_is_sats
    ) // end of [val]
  } // end of [where]
//
  val () = () where { // API for X11: [contrib/X11]
    val SRCROOTcontrib_X11 = SRCROOTcontrib ++ "X11/"
    val DSTROOTcontrib_X11 = DSTROOTcontrib ++ "X11/"
    val () = mkdir_exn (DSTROOTcontrib_X11, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_X11++"Makefile", DSTROOTcontrib_X11++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_X11, DSTROOTcontrib_X11)
  } // end of [where]
//
  val () = () where { // API for GTK: [contrib/GTK]
    val SRCROOTcontrib_GTK = SRCROOTcontrib ++ "GTK/"
    val DSTROOTcontrib_GTK = DSTROOTcontrib ++ "GTK/"
    val () = mkdir_exn (DSTROOTcontrib_GTK, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_GTK++"Makefile", DSTROOTcontrib_GTK++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_GTK, DSTROOTcontrib_GTK)
    val DSTROOTcontrib_GTK_SATS_gtk = DSTROOTcontrib_GTK ++ "SATS/gtk/"
    val () = mkdir_exn (DSTROOTcontrib_GTK_SATS_gtk, DIRmode)
    val () = dir_copy (
      SRCROOTcontrib ++ "GTK/SATS/gtk/", DSTROOTcontrib_GTK_SATS_gtk, name_is_sats
    ) // end of [val]
    val DSTROOTcontrib_GTK_SATS_gdk = DSTROOTcontrib ++ "GTK/SATS/gdk/"
    val () = mkdir_exn (DSTROOTcontrib_GTK_SATS_gdk, DIRmode)
    val () = dir_copy (
      SRCROOTcontrib ++ "GTK/SATS/gdk/", DSTROOTcontrib_GTK_SATS_gdk, name_is_sats
    ) // end of [val]
  } // end of [where]
//
  val () = () where { // API for GL: [contrib/GL]
    val SRCROOTcontrib_GL = SRCROOTcontrib ++ "GL/"
    val DSTROOTcontrib_GL = DSTROOTcontrib ++ "GL/"
    val () = mkdir_exn (DSTROOTcontrib_GL, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_GL++"Makefile", DSTROOTcontrib_GL++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_GL, DSTROOTcontrib_GL)
  } // end of [where]
//
  val () = () where { // API for GLES2: [contrib/GLES2]
    val SRCROOTcontrib_GLES2 = SRCROOTcontrib ++ "GLES2/"
    val DSTROOTcontrib_GLES2 = DSTROOTcontrib ++ "GLES2/"
    val () = mkdir_exn (DSTROOTcontrib_GLES2, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_GLES2++"Makefile", DSTROOTcontrib_GLES2++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_GLES2, DSTROOTcontrib_GLES2)
  } // end of [where]
//
  val () = () where { // API for SDL: [contrib/SDL]
    val SRCROOTcontrib_SDL = SRCROOTcontrib ++ "SDL/"
    val DSTROOTcontrib_SDL = DSTROOTcontrib ++ "SDL/"
    val () = mkdir_exn (DSTROOTcontrib_SDL, DIRmode)
    val () = fcopy_exn (
      SRCROOTcontrib_SDL++"Makefile", DSTROOTcontrib_SDL++"Makefile"
    ) // end of [val]
    val () = libdir_copy (SRCROOTcontrib_SDL, DSTROOTcontrib_SDL)
  } // end of [where]
//
  val () = () where { // [contrib/testing]
    val SRCROOTcontrib_testing = SRCROOTcontrib ++ "testing/"
    val DSTROOTcontrib_testing = DSTROOTcontrib ++ "testing/"
    val () = mkdir_exn (DSTROOTcontrib_testing, DIRmode)
(*
    val () = fcopy_exn (
      SRCROOTcontrib_testing++"Makefile", DSTROOTcontrib_testing++"Makefile"
    ) // end of [val]
*)
    val () = libdir_copy (SRCROOTcontrib_testing, DSTROOTcontrib_testing)
  } // end of [where]
//
in
  prerr "The [contrib] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_contrib_dir_copy]

(* ****** ****** *)

(*
fn Anairiats_src_dir_copy (): void = let
  fn test (name: string): bool = begin case+ name of
    | _ when name_is_xats (name) => true | _ => false
  end // end of [filename_test]

  macdef cp (name) =
    fcopy_exn (SRCROOTsrc ++ ,(name), DSTROOTsrc ++ ,(name))

  val () = mkdir_exn (DSTROOTsrc, DIRmode)
  val () = dir_copy (SRCROOTsrc, DSTROOTsrc, test)
  val () = cp "Makefile"
  val () = cp "Makefile_bootstrap"
  val () = cp "ats_grammar_yats.c"
  val () = cp "ats_grammar_yats.h"
in
  prerr "The [src] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_src_dir_copy]
*)

(* ****** ****** *)

fn Anairiats_utils_dir_copy () = let
  val SRCROOTutils = SRCROOT ++ "utils/"
  val DSTROOTutils = DSTROOT ++ "utils/"
  val () = mkdir_exn (DSTROOTutils, DIRmode)
//
  val SRCROOTutils_atslex = SRCROOTutils ++ "atslex/"
  val DSTROOTutils_atslex = DSTROOTutils ++ "atslex/"
  val () = mkdir_exn (DSTROOTutils_atslex, DIRmode)
  val () = dir_copy
    (SRCROOTutils_atslex, DSTROOTutils_atslex, name_is_xats)
  val () = fcopy_exn (
    SRCROOTutils_atslex ++ "Makefile", DSTROOTutils_atslex ++ "Makefile"
  ) // end of [fcopy_exn]
  val () = fcopy_exn (
    SRCROOTutils_atslex ++ "README", DSTROOTutils_atslex ++ "README"
  ) // end of [fcopy_exn]
//
  val SRCROOTutils_atslex_EXAMPLE = SRCROOTutils_atslex ++ "EXAMPLE/"
  val DSTROOTutils_atslex_EXAMPLE = DSTROOTutils_atslex ++ "EXAMPLE/"
  val () = mkdir_exn (DSTROOTutils_atslex_EXAMPLE, DIRmode)
  val () = dir_copy
    (SRCROOTutils_atslex_EXAMPLE, DSTROOTutils_atslex_EXAMPLE, name_is_xats)
  val () = fcopy_exn (
    SRCROOTutils_atslex_EXAMPLE ++ "Makefile"
  , DSTROOTutils_atslex_EXAMPLE ++ "Makefile"
  ) // end of [fcopy_exn]
//
  val SRCROOTutils_scripts = SRCROOTutils ++ "scripts/"
  val DSTROOTutils_scripts = DSTROOTutils ++ "scripts/"
  val () = mkdir_exn (DSTROOTutils_scripts, DIRmode)
  val () = dir_copy
    (SRCROOTutils_scripts, DSTROOTutils_scripts, name_is_xats)
  val () = fcopy_exn (
    SRCROOTutils_scripts ++ "Makefile", DSTROOTutils_scripts ++ "Makefile"
  ) // end of [fcopy_exn]
in
  prerr "The [utils] directory is successfully copied.";
  prerr_newline ()
end // end of [Anairiats_utils_dir_copy]

(* ****** ****** *)

extern fun atspack_source_code (): void

implement atspack_source_code () = let
  val () = // run-time checking
    if test_file_exists (DSTROOT) then begin
      prerr "The directory ["; prerr DSTROOT; prerr "] already exists.";
      prerr_newline ();
      exit (1)
    end // end of [if]
  val () = mkdir_exn (DSTROOT, DIRmode)

  macdef cp name =
    fcopy_exn (SRCROOT ++ ,(name), DSTROOT ++ ,(name))
  macdef cp2 name1 name2 =
    fcopy_exn (SRCROOT ++ ,(name1), DSTROOT ++ ,(name2))
  macdef cpx (name) = let
    val src_name = SRCROOT ++ ,(name)
    val dst_name = DSTROOT ++ ,(name)
    val () = fcopy_exn (src_name, dst_name)
    val () = chmod_exn (dst_name, $STAT.S_IRWXU)
  in
    // empty
  end // end of [cpx]
  val () = cp "INSTALL"
  val () = cp "VERSION.txt"
  val () = cp2 "Makefile_dist" "Makefile"
  val () = cp2 "Makefile_macosx" "Makefile_macosx"
  val () = cp2 "Makefile_bootstrap" "Makefile_bootstrap"
//
  val () = cp "ats_env.sh.in"
//
  val () = cp "config.mk.in"
  val () = cp "config.h.in" // in case [autoheader] is not available
//
  val () = cp "configure.ac"
  val () = cpx "configure"  // in case [autoconf] is not available ...
(*
  val () = cpx "install-sh"; val () = cpx "missing"
*)
//
  val () = cp "test.sh.in"
//
  val () = cp ".libfiles"
  val () = cp ".libfiles_mt"
//
  val () = Anairiats_bin_dir_copy (PACKNDsource)
  val () = Anairiats_bootstrap_dir_copy ()
  val () = Anairiats_ccomp_dir_copy (PACKNDsource)
  val () = Anairiats_doc_dir_copy ()
  val () = Anairiats_prelude_dir_copy ()
  val () = Anairiats_libc_dir_copy ()
  val () = Anairiats_libats_dir_copy ()
  val () = Anairiats_contrib_dir_copy (PACKNDsource)
(*
  val () = Anairiats_src_dir_copy () // HX: The source code is no longer distributed
*)
  val () = Anairiats_utils_dir_copy ()
//
in
  prerr "The package [";
  prerr ATSPACKAGE_NAME;
  prerr "-";
  prerr ATSPACKAGE_VERSION;
  prerr "] is successfully built.";
  prerr_newline ()  
end // end of [atspack_source_code]

(* ****** ****** *)

extern fun atspack_precompiled (): void

implement atspack_precompiled () = let
  val () = // run-time checking
    if test_file_exists (DSTROOT) then begin
      prerr "The directory ["; prerr DSTROOT; prerr "] already exists.";
      prerr_newline ();
      exit (1)
    end // end of [if]
  val () = mkdir_exn (DSTROOT, DIRmode)

  macdef cp (name) = fcopy_exn (SRCROOT ++ ,(name), DSTROOT ++ ,(name))
  val () = cp "INSTALL"
  val () = cp "config.h"
  val () = Anairiats_bin_dir_copy (PACKNDprecompiled)
  val () = Anairiats_ccomp_dir_copy (PACKNDprecompiled)
  val () = Anairiats_doc_dir_copy ()
  val () = Anairiats_prelude_dir_copy ()
  val () = Anairiats_libc_dir_copy ()
  val () = Anairiats_libats_dir_copy ()
  val () = Anairiats_contrib_dir_copy (PACKNDprecompiled)
in
  // empty
end // end of [atspack_precompiled]

(* ****** ****** *)

fn do_usage
  (cmd: string): void = () where {
  val () = printf ("%s [flag] [kind]\n", @(cmd))
  val () = printf ("  where flag is -m32 or -m64, and [kind] is --source or --precompiled.\n", @())
} // end of [do_usage]

(* ****** ****** *)

implement
main (argc, argv) = let
//
fun loop {n,i:nat | i <= n} .<n-i>. (
  argc: int n, argv: &(@[string][n]), i: int i, cnt: &int
) : void =
  if i < argc then let
    val x = argv.[i]
    val () = case+ 0 of
      | _ when x = "--source" => (
          cnt := cnt+1; atspack_source_code ()
        )
      | _ when x = "--precompiled" => (
          cnt := cnt+1; atspack_precompiled ()
        )
      | _ when x = "-m32" => wordsize_target_set (4(*bytes*))
      | _ when x = "-m64" => wordsize_target_set (8(*bytes*))
      | _ when x = "--help" => do_usage (argv.[0])
      | _ => let
          val () = prerrf ("[%s]: unrecognized flag: %s\n", @(argv.[0], x))
        in
          // nothing
        end // end of [_]
   in
     if cnt = 0 then loop (argc, argv, i+1, cnt)
   end // end of [if]
  (* end of [loop] *)
//
var cnt: int = 0
val () = loop (argc, argv, 1, cnt)
val () = if cnt = 0 then do_usage (argv.[0])
//
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [atspack.dats] *)
