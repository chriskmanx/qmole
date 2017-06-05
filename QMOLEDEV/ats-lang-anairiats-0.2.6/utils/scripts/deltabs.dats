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
// Time: Summer, 2007
//

(* ****** ****** *)

// replacing tabs with spaces

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

#define i2c char_of_int

(* ****** ****** *)

#define SPACES_PER_TAB 8

extern fun getline {m:file_mode}
  (pf: file_mode_lte (m, r) | out: &FILE m): string

extern fun putline {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, str: string): void

dataviewtype charlst (int) =
  | charlst_nil (0)
  | {n:nat} charlst_cons (n+1) of (char, charlst n)

#define nil charlst_nil
#define cons charlst_cons
#define :: charlst_cons

extern fun charlst_is_nil {n:nat} (cs: !charlst n): bool (n == 0) =
  "charlst_is_nil"

implement charlst_is_nil (cs) = case+ cs of
  | nil _ => (fold@ cs; true) | cons _ => (fold@ cs; false)

extern fun
charlst_uncons {n:pos} (cs: &charlst n >> charlst (n-1)): char =
  "charlst_uncons"

implement charlst_uncons (cs) =
  let val ~(c :: cs_r) = cs in cs := cs_r; c end

extern fun
string_make_charlst_int {n:nat} (cs: charlst n, n: int n): string n =
  "string_make_charlst_int"

%{

ats_ptr_type
string_make_charlst_int (ats_ptr_type cs, const ats_int_type n) {
  char *s0, *s;

  s0 = ats_malloc_gc(n+1) ;

  s = s0 + n ;
  *s = '\0' ; --s ;

  while (!charlst_is_nil(cs)) { *s = charlst_uncons(&cs) ; --s ; }

  return s0 ;
}

%}

implement getline {m} (pf | out) = let

fun loop {n:nat} (out: &FILE m, cs: charlst n, n: int n): string =
  let val i = fgetc_err (pf | out) in
    if i >= 0 then let
      val c = i2c i
    in
      if c <> '\n' then begin
        loop (out, c :: cs, n+1)
      end else begin
        string_make_charlst_int (c :: cs, n+1)
      end
    end else begin
      string_make_charlst_int (cs, n)
    end
  end
in
  loop (out, nil (), 0)
end

(* ****** ****** *)

implement
putline {m} (pf | out, s) = let
//
fun prsp {n:nat} .<n>.
  (out: &FILE m, nsp: int n): void =
  if nsp > 0 then (fputc_exn (pf |' ', out); prsp (out, nsp-1))
//
fun loop{n,i:nat | i <= n} .<n-i>. (
  out: &FILE m, s: string n, n: size_t n, i: size_t i
) : void =
  if i < n then let
    val c = s[i]
  in
    if (c = '\t') then let
      val i = int1_of_size1 (i)
      val nsp = (SPACES_PER_TAB - i nmod SPACES_PER_TAB)
    in
      prsp (out, nsp)
    end else begin
      fputc_exn (pf | c, out)
    end;
    loop (out, s, n, i+1)
  end // end of [if]
//
val s = string1_of_string s
//
in
  loop (out, s, string_length s, 0)
end // end of [putline]

(* ****** ****** *)

fn fdeltabs (
  fil_s: &FILE r, fil_d: &FILE w
) : void = let
  fun loop (fil_s: &FILE r, fil_d: &FILE w): void =
    if feof (fil_s) = 0 then let
      val s = getline (file_mode_lte_r_r | fil_s)
    in
      putline (file_mode_lte_w_w | fil_d, s); loop (fil_s, fil_d)
    end // end of [if]
in
  loop (fil_s, fil_d)
end // end of [fdeltabs]

(* ****** ****** *)

fn deltabs_file_file
  (src: string, dst: string): void = let
  val (pf_s_opt | ptr_s) = fopen_err (src, file_mode_r)
  val () = assert_prerrf_bool1
    (ptr_s > null, "Exit: [fopen_err(\"%s\", \"r\")] failed\n", @(src))
  prval Some_v pf_s = pf_s_opt
  val (pf_d_opt | ptr_d) = fopen_err (dst, file_mode_w)
  val () = assert_prerrf_bool1
    (ptr_d > null, "Exit: [fopen_err(\"%s\", \"w\")] failed\n", @(dst))
  prval Some_v pf_d = pf_d_opt
in
  fdeltabs (!ptr_s, !ptr_d);
  fclose_exn (pf_s | ptr_s);
  fclose_exn (pf_d | ptr_d);
end // end of [deltabs_file_file]

//
// HX: [dst_dir] needs to end with "/"
//
fn deltabs_file_dir (
  src_fil: string, dst_dir: string
) : void = let
  val dst_fil = dst_dir + src_fil
in
  deltabs_file_file (src_fil, dst_fil)
end // end of [deltabs_file_dir]

(* ****** ****** *)

implement
main (argc, argv) = let
//
val cmd = argv.[0]
//
val () = if (argc < 2) then begin
  prerrf ("Usage of [%s]:\n", @(cmd));
  prerrf ("  1. %s [-(stdin)]\n", @(cmd));
  prerrf ("  2. %s [file(src)]\n", @(cmd));
  prerrf ("  3. %s [-(stdin)] [file(dst)]\n", @(cmd));
  prerrf ("  4. %s [file(src)] [file(dst)]\n", @(cmd));
  prerrf ("  5. %s [file(src)] ... [file(src)] [dir(dst)]\n", @(cmd));
  exit (1)
end // end of [val]
//
val () = assert (argc >= 2)
//
in
//
case+ argc of
| 2 when (argv.[1] = "-") => let
    val (pf_s | ptr_s) = stdin_get ()
    val (pf_d | ptr_d) = stdout_get ()
  in
    fdeltabs (!ptr_s, !ptr_d);
    stdin_view_set (pf_s | (*none*));
    stdout_view_set (pf_d | (*none*))
  end
| 2 => let
    val (pf_d | ptr_d) = stdout_get ()
    val (pf_s | ptr_s) = fopen_exn (argv.[1], file_mode_r)
  in
    fdeltabs (!ptr_s, !ptr_d);
    fclose_exn (pf_s | ptr_s);
    stdout_view_set (pf_d | (*none*))
  end
| 3 when (argv.[1] = "-") => let
    val (pf_s | ptr_s) = stdin_get ()
    val (pf_d | ptr_d) = fopen_exn (argv.[2], file_mode_w)
  in
    fdeltabs (!ptr_s, !ptr_d);
    fclose_exn (pf_d | ptr_d);
    stdin_view_set (pf_s | (*none*))
  end
| 3 => deltabs_file_file (argv.[1], argv.[2])
| _ =>> begin
    exit_errmsg (1, "not yet supported!\n")
  end // end of [_]
//
end // end of [main]

(* ****** ****** *)

(* end of [deltabs.dats] *)
