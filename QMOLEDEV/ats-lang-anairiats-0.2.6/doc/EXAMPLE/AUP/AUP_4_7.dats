//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 248 - 249
// section 4.7: Terminal-Identification System Calls
//
(* ****** ****** *)

staload "libc/SATS/unistd.sats"

(* ****** ****** *)

fun print_ttyname
  (): int = status where {
  prval () = STDIN_FILENO_gtez ()
  val (fpf_ctty | ctty) =
    ttyname (STDIN_FILENO)
  val status = (case+ 0 of
    | _ when strptr_isnot_null(ctty) => let
        val () = (print ctty; print_newline ())
        prval () = fpf_ctty (ctty)
      in
        0
      end // end of [_ when ...]
    | _ => let
        val () = (print "not a tty"; print_newline ())
        prval () = fpf_ctty (ctty)
      in
        1
      end // end of [_]
  ) : int // end of [val]
} // end of [print_ttyname]

(* ****** ****** *)

fun print_ttyname_r
  (): int = status where {
  prval () = STDIN_FILENO_gtez ()
  #define BUFSZ 1024 // big enough?  
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  val (pfopt | err) =
    ttyname_r (pf_buf | STDIN_FILENO, p_buf, BUFSZ)
  val status = if err = 0 then let
    prval ttyname_v_succ (pf) = pfopt
    val () = print
      (__cast p_buf) where {
      extern castfn __cast (x: ptr): string
    } // end of [val]
    val () = print_newline ()
    prval () = pf_buf := bytes_v_of_strbuf_v (pf)
  in
    0 // success
  end else let // err < 0
    prval ttyname_v_fail (pf) = pfopt
    val () = (print "not a tty?"; print_newline ())
    prval () = pf_buf := bytes_v_of_b0ytes_v (pf)
  in
    1 // failure
  end : int // end of [if]
} // end of [print_ttyname]

(* ****** ****** *)

implement
main () = () where {
  val status = print_ttyname ()
  val status = print_ttyname_r ()
} // end of [main]

(* ****** ****** *)

(* end of [AUP_4_7.dats] *)
