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
// Author: Likai Liu (liulk AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

staload "top.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

%{^

extern int execvp_with_stdout (char **result, char* argv[]) ;

%}

(* ****** ****** *)

extern fun atscc_pkgconfig
  {n:nat} (arglst: strlst n, narg: int n): Lstrlst = "atscc_pkgconfig"
// end of [atscc_pkgconfig]

extern fun shelltok_parse (inp: string): Lstrlst = "shelltok_parse"

(* ****** ****** *)

extern typedef "lstrlst_t" = lstrlst 0
extern typedef "strlst_t" = STRLSTcons_pstruct (string, strlst 0)

%{$

extern void free (void*) ;

ats_ptr_type atscc_pkgconfig (
  ats_ptr_type arglst, ats_int_type narg
) {
  char **argv, *buf ; int i, err ; strlst_t toks ;

  i = 0 ;
  argv = alloca(1/*cmd*/ + narg + 1/*NULL*/) ;
  argv[i] = "pkg-config"; i += 1 ;
  while (arglst != 0) {
    argv[i] = (char*)(((strlst_t)arglst)->atslab_0);
    i += 1; arglst = (((strlst_t)arglst)->atslab_1);
  } /* end of [while] */
  argv[i] = (char*)0 ; /* i == narg + 1 */

  err = execvp_with_stdout(&buf, argv) ;

  if (err != 0) {
    if (buf != 0) free (buf) ; return (ats_ptr_type)0 ;
  } // end of [if]

  toks = (lstrlst_t)shelltok_parse (buf) ; free (buf) ;

  return toks ;
}

%}

(* ****** ****** *)

//
// HX: The implementation is largely adopted from one written by Likai Liu
// in C. I hereby experiment with a programming style involving [alloca] that
// I have been considering for some time.
//

staload STRING = "libc/SATS/string.sats"

implement shelltok_parse (inp) = let
  #define NUL '\0'
  #define SQUOTE '\''
  #define DQUOTE '\"'
  #define SLASH '\\'
  #define BUFSZ_INIT 1024
  fun loop1 {n,i:nat | i <= n}
    {bsz:int | bsz >= 1} .<n-i, 0>. (
      inp: string n
    , i: size_t i, buf: &bytes bsz, bsz: size_t bsz
    , itms: Lstrlst
    ) :<> Lstrlst = let
    val c = string_test_char_at (inp, i)
  in
    if (c <> NUL) then begin case+ 0 of
      | _ when char_isspace (c) => loop1 (inp, i+1, buf, bsz, itms)
      | _ when (c = SQUOTE orelse c = DQUOTE) =>
          loop2 (inp, c(*quote*), 0(*escape*), i+1, buf, bsz, 0, itms)
      | _ => let
          val () = buf.[0] := byte_of_char c in
          loop3 {n,i+1} (inp, NUL(*quote*), 0(*escape*), i+1, buf, bsz, 1, itms)
        end // end of [_]
    end else begin
      itms // loop1 exits
    end (* end of [if] *)
  end (* end of [loop1] *)
  
  and loop2
    {n,i:nat | i <= n} {bsz,k:nat | k < bsz} .<n-i, 0>. (
      inp: string n
    , quotechar: char, escape: int
    , i: size_t i, buf: &bytes bsz, bsz: size_t bsz, k: size_t k
    , itms: Lstrlst
    ) :<> Lstrlst = let
    val c = string_test_char_at (inp, i)
  in
    if (c <> NUL) then begin case+ 0 of
      | _ when quotechar <> NUL => begin
          if c <> quotechar then let
            val () = buf.[k] := byte_of_char c in
            loop3 (inp, quotechar, escape, i+1, buf, bsz, k+1, itms)
          end else begin // this is a closing quote
            loop2 (inp, NUL(*quote*), 0(*escape*), i+1, buf, bsz, k, itms)
          end (* end of [if] *)
        end // end of [_ when ...]
      | _ when escape > 0 => let // quotechar = NUL
          val () = buf.[k] := byte_of_char c
        in
          loop3 (inp, quotechar, escape, i+1, buf, bsz, k+1, itms)
        end // end of [_ when ...]
      | _ when (c = SQUOTE orelse c = DQUOTE) =>
          loop2 (inp, c(*quote*), 0(*escape*), i+1, buf, bsz, k, itms)
        // end of [_ when ...]
      | _ when c = SLASH =>
          loop2 (inp, NUL(*quote*), 1(*escape*), i+1, buf, bsz, k, itms)
        // end of [_ when ...]
      | _ when char_isspace (c) => let
          val itm =
            string_make_substring (__cast buf, 0, k) where {
            extern castfn __cast (buf: &bytes bsz):<> string (k)
          } // end of [val]
          val itm = string1_of_strbuf (itm)
        in
          loop1 (inp, i+1, buf, bsz, LSTRLSTcons (itm, itms))
        end // end of [_ when ...]
      | _ => let
          val () = buf.[k] := byte_of_char c in
          loop3 (inp, NUL(*quote*), 0(*escape*), i+1, buf, bsz, k+1, itms)
        end // end of [_]
    end else let
      val itm =
        string_make_substring (__cast buf, 0, k) where {
        extern castfn __cast (buf: &bytes bsz):<> string (k)
      } // end of [val]
      val itm = string1_of_strbuf (itm)
      //
      // should an unclosed quote be reported?
      //
    in
      LSTRLSTcons (itm, itms) // loop2 exits
    end (* end of [if] *)
  end // end of [loop2]

  and loop3 {n,i:nat | i <= n}
    {bsz,k:nat | 1 <= bsz; k <= bsz} .<n-i, 1>. (
      inp: string n
    , quotechar: char, escape: int
    , i: size_t i, buf: &bytes bsz, bsz: size_t bsz, k: size_t k
    , itms: Lstrlst
    ) :<> Lstrlst =
    if k < bsz then begin
      loop2 (inp, quotechar, escape, i, buf, bsz, k, itms)
    end else let // k = bsz
      val bsz2 = bsz + bsz
      var !p_buf1 with pf_buf1 = @[byte][bsz2]()
      prval () = pf_buf1 := bytes_v_of_b0ytes_v (pf_buf1)
      val _(*p_buf1*) = $STRING.memcpy (pf_buf1 | p_buf1, buf, k)
    in
      loop2 (inp, quotechar, escape, i, !p_buf1, bsz2, k, itms)
    end // end of [if]
  // end of [loop3]
  val inp = string1_of_string (inp)
  var !p_buf with pf_buf = @[byte][BUFSZ_INIT]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  val itms = loop1 (inp, 0, !p_buf, BUFSZ_INIT, LSTRLSTnil ())
in
  lstrlst_reverse (itms)
end (* end of [shelltok_parse] *)

(* ****** ****** *)

%{$

/*
** Author: Likai Liu (liulk AT cs DOT bu DOT edu); some modification by HX
** Time: Summer, 2009
*/

#include <errno.h>      /* EINTR */
#include <stdio.h>      /* perror() */
#include <stdlib.h>     /* malloc(), realloc(), free(), abort() */
#include <sys/wait.h>   /* waitpid() */
#include <unistd.h>     /* pipe(), fork(), execvp() */

#define BUFSZ_INIT 1024

int execvp_with_stdout (char **result, char* argv[])
{
  *result = NULL;

  int filedes[2];
  if (pipe(filedes) != 0)
    return -1;

  pid_t pid = fork();
  if (pid == 0) {
    /* prepare execution of child process. */
    dup2(filedes[1], STDOUT_FILENO);
    close(filedes[0]);
    close(filedes[1]);
    execvp(argv[0], argv); perror(argv[0]); abort();
  } else if (pid == -1) {
    close(filedes[0]);
    close(filedes[1]);
    return -1;
  } // end of [if]

  close(filedes[1]);

  size_t buf_size = BUFSZ_INIT, buf_used = 0;
  char *buf = malloc(buf_size);

  while(1) {
    if (buf_used >= buf_size) {
      buf_size *= 2;
      buf = realloc(buf, buf_size);
    }

    ssize_t res = read(filedes[0], buf + buf_used, buf_size - buf_used);
    if (res == 0)
      break;
    else if (res == -1) {
      if (errno == EINTR)
        continue;
      else
        break;
    } // end of [if]

    buf_used += res;
  } // end of [while]

  buf[buf_used] = '\0';

  close(filedes[0]);

  int status;
  waitpid(pid, &status, 0);
  *result = buf;

  return status;
}

%} (* end of [%{^] *)

(* ****** ****** *)

(* end of [pkgconfig.dats] *)
