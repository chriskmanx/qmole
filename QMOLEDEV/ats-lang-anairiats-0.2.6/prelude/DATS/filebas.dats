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
//
// HX-2010-03-14: Here are some basic IO operations which
// are mostly for prototype implementation. If something more
// efficient is needed, please use the functions in declared
// in the following file: libc/SATS/stdio.sats
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no initialization is needed

(* ****** ****** *)

%{^
#include <libc/CATS/stdio.cats>
#include <libc/sys/CATS/stat.cats>
%} // end of [%{#]

(* ****** ****** *)

staload "prelude/SATS/filebas.sats"

(* ****** ****** *)

(*
//
// HX: implemented in [prelude/DATS/basics.dats]
//
implement file_mode_lte_r_r = file_mode_lte_refl {r} ()
implement file_mode_lte_w_w = file_mode_lte_refl {w} ()
implement file_mode_lte_rw_rw = file_mode_lte_refl {rw} ()
*)

(* ****** ****** *)

%{^

typedef struct {
  ats_char_type atslab_0 ; ats_ptr_type atslab_1 ;
} *charlst ; // end of [typedef]

ATSinline()
ats_ptr_type
string_make_charlst_rev
  (ats_int_type sz, ats_ptr_type cs) {
  char *s0, *s ; charlst cs_next ;
  s0 = (char*)ATS_MALLOC (sz + 1) ; s = s0 + sz ;
  *s-- = '\0' ;
  while (cs != (charlst)0) {
    *s-- = ((charlst)cs)->atslab_0 ;
    cs_next = ((charlst)cs)->atslab_1 ;
    ATS_FREE (cs) ; cs = cs_next ;
  } // end of [while]
  return s0 ;
} /* string_make_charlst_rev */

%} // end of [%{^]

(* ****** ****** *)

#define i2c char_of_int
macdef EOF = $extval (int, "EOF")

(* ****** ****** *)

extern
fun feof0 (f: FILEref):<> int = "mac#atslib_feof"

extern
fun fgetc0_err (fil: FILEref):<> int = "mac#atslib_fgetc_err"
// end of [fgetc0_err]

extern
fun fgetc1_err
  {m:file_mode} {l:addr} (
  pf_mod: file_mode_lte (m, r) | fil: &FILE m
) :<> int = "mac#atslib_fgetc_err"

extern
fun fclose0_exn (fil: FILEref):<!exn> void = "atslib_fclose_exn"
// end of [fclose0_exn]

extern
fun fclose1_exn {m:file_mode} {l:addr}
  (pf_fil: FILE m @ l | p_fil: ptr l):<!exn> void = "atslib_fclose_exn"
// end of [fclose1_exn]

(* ****** ****** *)

extern
fun string_make_charlst_rev
  {n:nat} (sz: int n, cs: list_vt (char, n)):<> strptr1
  = "string_make_charlst_rev"
// end of [string_make_charlst_rev]

(* ****** ****** *)
//
// HX: if the last character is '\n', it is dropped
//
implement
input_line_vt (fil) = let
  fun loop {n:nat} (
      fil: FILEref, n: int n, cs: list_vt (char, n)
    ) : strptr0 = let
    val c = fgetc0_err (fil)
  in
    if c <> EOF then let
      val c = i2c c
    in
      if (c <> '\n') then
        loop (fil, n+1, list_vt_cons (c, cs))
      else 
        string_make_charlst_rev (n, cs) // linear string
      // end of [if]
    end else begin
      if n = 0 then let
        val+ ~list_vt_nil () = cs in strptr_null ()
      end else
        string_make_charlst_rev (n, cs) // linear string
      // end of [if]
    end  // end of [if]
  end // end of [loop]
in
  loop (fil, 0, list_vt_nil ())
end // end of [input_line_vt]

implement
input_line (fil) = let
  val str = input_line_vt (fil) in stropt_of_strptr (str)
end // end of [input_line]

(* ****** ****** *)

extern
fun fputc0_exn
  (c: char, fil: FILEref):<!exn> void = "atslib_fputc_exn"
// end of [fputc0_exn]

extern
fun fputs0_exn
  (str: string, fil: FILEref):<!exn> void = "atslib_fputs_exn"
// end of [fputs0_exn]

extern
fun fflush0_exn (fil: FILEref):<!exn> void = "atslib_fflush_exn"
// end of [fflush0_exn]

(* ****** ****** *)
//
// HX: the character '\n' is added at the end
//
implement
output_line (fil, line) = (
  fputs0_exn (line, fil); fputc0_exn ('\n', fil); fflush0_exn (fil)
) // end of [output_line]

(* ****** ****** *)

local
//
staload "libc/sys/SATS/stat.sats"
staload T = "libc/sys/SATS/types.sats"
//
macdef u2m = $T.mode_of_uint
macdef m2u = $T.uint_of_mode
//
in // in of [local]

implement
test_file_exists (path) = let
  var st: stat?
  val ret = stat_err (path, st)
in
  if ret = 0 then let
    prval () = opt_unsome {stat} (st) in true
  end else let
    prval () = opt_unnone {stat} (st) in false
  end (* end of [if] *)
end // end of [test_file_fun]

(* ****** ****** *)

implement
test_file_mode
  (path, f) = let
  var st: stat? ; val ret = stat_err (path, st)
in
  if ret = 0 then let
    prval () = opt_unsome {stat} (st)
  in
    if f (m2u(st.st_mode)) then 1 else 0
  end else let
    prval () = opt_unnone {stat} (st) in ~1 (*error*)
  end (* end of [if] *)
end // end of [test_file_mode]

(* ****** ****** *)

implement test_file_isblk
  (path) = test_file_mode (path, lam x => S_ISBLK (u2m(x)))
// end of [test_file_isblk]

implement test_file_ischr
  (path) = test_file_mode (path, lam x => S_ISCHR (u2m(x)))
// end of [test_file_ischr]

implement test_file_isdir
  (path) = test_file_mode (path, lam x => S_ISDIR (u2m(x)))
// end of [test_file_isdir]

implement test_file_isfifo
  (path) = test_file_mode (path, lam x => S_ISFIFO (u2m(x)))
// end of [test_file_isfifo]

implement test_file_isreg
  (path) = test_file_mode (path, lam x => S_ISREG (u2m(x)))
// end of [test_file_isreg]

(* ****** ****** *)

implement
test_file_islnk (path) = let
  var st: stat? ; val ret = lstat_err (path, st)
in
  if ret = 0 then let
    prval () = opt_unsome {stat} (st)
  in
    if S_ISLNK (st.st_mode) then 1 else 0
  end else let
    prval () = opt_unnone {stat} (st) in ~1 (*error*)
  end (* end of [if] *)
end // end of [test_file_islnk]

end // end of [local]

(* ****** ****** *)

implement
char_stream_make_file
  (fil) = $delay (let
  val c = fgetc0_err (fil)
in
  if c <> EOF then let
    val c = i2c c in
    stream_cons (c, char_stream_make_file fil)
  end else begin
    let val () = fclose0_exn (fil) in stream_nil () end
  end // end of [if]
end : stream_con char
) (* end of [char_stream_make_file] *)

(* ****** ****** *)

implement
line_stream_make_file
  (fil) = $delay (let
  val line = $effmask_ref (input_line fil)
in
  if stropt_is_some line then let
    val line = stropt_unsome line in
    stream_cons (line, line_stream_make_file fil)
  end else let
    val () = fclose0_exn fil in stream_nil ()
  end // end of [if]
end : stream_con string // end of [let]
) (* end of [line_stream_make_file] *)

(* ****** ****** *)

implement
char_stream_vt_make_file
  {m} {l} (
  pf_mod, pf_fil | p_fil
) = $ldelay (
let
  val c = fgetc1_err (pf_mod | !p_fil)
in
  if c >= 0 then let // c <> EOF
    val c = char_of_int (c)
  in
    stream_vt_cons (
      c, char_stream_vt_make_file (pf_mod, pf_fil | p_fil)
    ) // end of [stream_vt_cons]
  end else let
    val () = fclose1_exn (pf_fil | p_fil) in stream_vt_nil ()
  end (* end of [if] *)
end : stream_vt_con char
,
fclose1_exn (pf_fil | p_fil) // HX: for cleanup
) // end of [char_stream_vt_make_file]

(* ****** ****** *)

implement
line_stream_vt_make_file
  {m} {l} (pf_mod, pf_fil | p_fil) = let
//
fun loop {n:nat} (
    pf_fil: FILE m @ l
  | p_fil: ptr l, n: int n, cs: list_vt (char, n)
  ) :<!laz> stream_vt_con (strptr1) = let
  val c = fgetc1_err (pf_mod | !p_fil)
in
  if c >= 0 then let
    val c = char_of_int (c) // c <> EOF
  in
    if c <> '\n' then
      loop (pf_fil | p_fil, n+1, list_vt_cons (c, cs))
    else let
      val line = string_make_charlst_rev (n, cs) in
      stream_vt_cons (
        line, line_stream_vt_make_file (pf_mod, pf_fil | p_fil)
      ) // end of [stream_vt_cons]
    end // end of [if]
  end else let
    val () = fclose1_exn (pf_fil | p_fil)
  in
    if n > 0 then let
      val line = string_make_charlst_rev (n, cs) in
      stream_vt_cons (line, $ldelay stream_vt_nil)
    end else let
      val+ ~list_vt_nil () = cs in stream_vt_nil ()
    end // end of [if]
  end (* end of [if] *)
end (* end of [loop] *)
//
in
//
$ldelay (
loop (
  pf_fil | p_fil, 0, list_vt_nil ()
) // end of [loop]
, // HX: separator
fclose1_exn (pf_fil | p_fil) // HX: for cleanup
) // end of [$ldelay]
//
end // end of [char_stream_vt_make_file]

(* ****** ****** *)

(* end of [filebas.dats] *)
