//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//

(* ****** ****** *)
//
// book: AUP (2nd edition), pages 204 - 208
// section: 4.2: Reading from a Terminal
//
(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload F = "libc/SATS/fcntl.sats"
macdef fildes_read_err = $F.read_err
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

#include "prelude/HATS/lmacrodef.hats" // for println

(* ****** ****** *)

fun getln {n:pos} (
    s: &bytes(n), nmax: size_t n, iseof: &bool
  ) : bool = let
  val (pf_fd | ()) = stdin_fildes_view_get ()
  val nread = fildes_read_err (pf_fd | STDIN_FILENO, s, nmax - 1)
//
  val res = (case+ 0 of
| _ when nread > 0 => true where {
    val () = iseof := false
    val nread = size1_of_ssize1 (nread)
    val nread1 = nread - 1
    macdef NUL = byte_of_char('\0')
    val () = if s.[nread1] = (byte_of_char)'\n'
      then s.[nread1] := NUL else s.[nread] := NUL
    // end of [val]
  } // end of [_ when ...]
| _ when nread >= 0 => true where {
    val () = iseof := true
  } // end of [_ when ...]
| _ (* nread=-1 *) => false
  ) : bool // end of [val]
//
  val () = stdin_fildes_view_set (pf_fd | (*none*))
in
  res
end // end of [getln]

(* ****** ****** *)

%{^
#define atslib_read1(c) read(STDIN_FILENO, c, 1)
%} // end of [%{^]

#define c2b byte_of_char

fun getln2 {nmax:pos} (
    s: &bytes(nmax), nmax: size_t nmax, iseof: &bool 
  ) : bool = let
//
  extern fun read1
    (pf: !fildes_v (STDIN_FILENO) | c: &char): ssize_t = "mac#atslib_read1"
  (* end of [extern] *)
//
  var c: char = '\000'
  val (pf_fd | ()) = stdin_fildes_view_get ()
  var n: natLte nmax = 0
  val () = iseof := false
  var res: bool = false
  val () = while*
    {n:nat | n < nmax} (n: int n) =>
    (true) let
    val nread = read1 (pf_fd | c)
    val nread = int_of_ssize (nread)
  in
    case+ 0 of
    | _ when nread = 0 => let
        val () = s.[n] := (c2b)'\0'
        val () = iseof := true
        val () = res := true
      in
        break
      end // end of [_ when ...]
    | _ when nread >= 1 =>
        if c = '\n' then let
          val () = s.[n] := (c2b)'\0'
          val () = res := true
        in
          break
        end else (
          if n+1 < nmax then
            (s.[n] := c2b(c); n := n+1; continue)
          else
            (s.[n] := c2b('\0'); errno_set (E2BIG); break)
          // end of [if]
        ) // end of [if]
      // end of [_ when ...]
    | _ => break // HX: [read1]  failed
  end // end of [val]
  val () = stdin_fildes_view_set (pf_fd | (*none*))
in
  res
end // end of [getln2]

(* ****** ****** *)

#define BUFSZ 1024

implement
main () = () where {
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  var iseof: bool = false
//
  val () = println "Please input:"
  val () = while (true) let
    val err = getln (!p_buf, BUFSZ, iseof)
    val () = if ~err then break
    val () = if iseof then
      printf ("EOF\n", @())
    else
      printf ("Read: %s\n", @(__cast p_buf)) where {
        extern castfn __cast (x: ptr): string // cutting a corner
      } // end of [where]
    // end of [val]
    val () = if iseof then break
  in
    // nothing
  end // end of [val]
//
  val () = println "Please input:"
  val () = while (true) let
    val err = getln2 (!p_buf, BUFSZ, iseof)
    val () = if ~err then break
    val () = if iseof then
      printf ("EOF\n", @())
    else
      printf ("Read: %s\n", @(__cast p_buf)) where {
        extern castfn __cast (x: ptr): string // cutting a corner
      } // end of [where]
    // end of [val]
    val () = if iseof then break
  in
    // nothing
  end // end of [val]
//
} // end of [main]

(* ****** ****** *)

(* end of [AUP_4_2_1.dats] *)
