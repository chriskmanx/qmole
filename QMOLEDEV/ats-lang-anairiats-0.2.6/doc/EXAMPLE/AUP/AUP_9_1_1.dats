//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 601 - 603
// section 9.1.1: Introduction to Signals
//
(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/pointer.dats"

(* ****** ****** *)

staload "libc/SATS/fcntl.sats"
staload "libc/SATS/signal.sats"
staload "libc/SATS/stdio.sats" // perror
staload "libc/SATS/stdlib.sats" // _Exit
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

fun handler
  (sgn: signum_t): void = () where {
  val (pf_stdout | ()) = stdout_fildes_view_get ()
  val msg = "Got signal\n"
  val nmsg = string_length (msg)
  val (pf, fpf | p_msg) = __cast (msg) where {
    extern castfn __cast {n:nat}
      (x: string n):<> [l:addr] (bytes(n) @ l, bytes(n) @ l -<lin,prf> void | ptr l)
    // end of [extern]
  } // end of [val]
  val _err = write_err // HX: [printf] is unsafe!!!
    (pf_stdout | STDOUT_FILENO, !p_msg, nmsg)
  prval () = fpf (pf)
  val () = stdout_fildes_view_set (pf_stdout | (*none*))
  val () = _Exit (EXIT_FAILURE)
} // end of [val]

(* ****** ****** *)

implement
main () = () where {
  var act: sigaction
  val () = ptr_zero<sigaction>
    (__assert () | act) where {
    extern prfun __assert (): NULLABLE (sigaction)
  } // end of [val]
  val () = act.sa_handler := (sighandler)handler
  val err = sigaction_null (SIGINT, act)
  val () = if err < 0 then (perror "sigaction"; exit (EXIT_FAILURE))
  var i: int
  val () = for
    (i := 1; ; i := i+1) let
    val _leftover = sleep (1); val () = printf ("%d\n", @(i))
  in
    // nothing
  end // end of [val]
} // end of [main]

(* ****** ****** *)

(* end of [AUP_9_1_1.dats] *)
