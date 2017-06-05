//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 208 - 213
// section 4.2.2: Nonblocking Input
//
(* ****** ****** *)

staload "libc/SATS/errno.sats"
//
staload "libc/SATS/fcntl.sats"
//
staload "libc/SATS/time.sats"
staload "libc/SATS/unistd.sats"
//
(* ****** ****** *)

fun setblock {fd:int} (
    fd: int fd, block: bool
  ) : bool = let
  val f = __getfl (fd) where {
    extern fun __getfl (fd: int): flag_t = "atslib_fcntl_getfl"
  }
  val u = uint_of_flag (f); val i = int_of_uint (u)
in
  if i >= 0 then let
    val f = if block then (f land ~O_NONBLOCK) else (f lor O_NONBLOCK)
    val err = __setfl (fd, f) where {
      extern fun __setfl (fd: int, f: flag_t): int = "atslib_fcntl_setfl"
    } // end of [val]
  in
    if err >= 0 then true else false
  end else false
end // end of [setblock]

(* ****** ****** *)

#define BUFSZ 1024
#define BUFSZ1 %(BUFSZ-1)
#define c2b byte_of_char

fun test_setblock () = let
  var nerr: int = 0
  val tstart = time_get ()
  val () = if (lint_of)tstart < 0L then nerr := nerr + 1
  val res = setblock (STDIN_FILENO, false) // no blocking!
  val () = if ~res then nerr := nerr + 1
//
  val NUL = (c2b)'\000'
//
  val () =
//
if nerr = 0 then let
//
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
//
  val (pf_fd | ()) = stdin_fildes_view_get ()
//
  val () = while (true) let
    val tnow = time_get ()
    val () = if (lint_of)tnow < 0L then nerr := nerr + 1
    val () = if nerr > 0 then break
    val () = printf ("Waiting for input: (%.0f sec.) ...\n", @(difftime (tnow, tstart)))
    val n = read_err (pf_fd | STDIN_FILENO, !p_buf, BUFSZ1)
    val n = int1_of_ssize1 (n)
  in
    case+ 0 of
    | _ when n = 0 => (printf ("EOF\n", @()); break)
    | _ when n >= 1 => let
        val () = if !p_buf.[n-1] = (c2b)'\n' then !p_buf.[n-1] := NUL else !p_buf.[n] := NUL
        val () = printf (
          "Read: \"%s\"\n", @(__cast p_buf)
        ) where {
          extern castfn __cast (x: ptr):<> string
        } // end of [val]
      in
        // nothing
      end // end of [_ when ...]
    | _ (*nread = -1*) => (
        if errno_get () = EAGAIN then
          let val _leftover = sleep(5) in continue end
        else (nerr := nerr + 1; break)
      ) // end of [_]
  end // end of [val]
//
  val () = stdin_fildes_view_set (pf_fd | (*none*))
//
in
  // nothing
end // end of [if]
//
in
  if nerr > 0 then (prerr "test_setblock: failed"; prerr_newline ())
end // end of [test_setblock]

(* ****** ****** *)

#include "prelude/HATS/lmacrodef.hats" // for println

(* ****** ****** *)

implement
main () = () where {
  val () = test_setblock ()
} // end of [main]

(* ****** ****** *)

(* end of [AUP_4_2_2_1.dats] *)

