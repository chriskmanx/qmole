//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 615 - 618
// section 9.1.7: Signal Handlers
//
(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/pointer.dats"

(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/signal.sats"
staload "libc/SATS/stdio.sats" // perror
staload "libc/SATS/stdlib.sats" // _Exit
staload "libc/SATS/time.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

%{^
volatile sig_atomic_t theSignum ;
ATSinline()
ats_int_type
theSignum_get() { return theSignum ; }
ATSinline()
ats_void_type
theSignum_set(int x) { theSignum = x ; return ; }
%} // end of [%{^]
extern
fun theSignum_get (): int = "theSignum_get"
extern
fun theSignum_set (x: int): void = "theSignum_set"

fun handler
  (sgn: signum_t): void = () where {
  val () = theSignum_set ($UNSAFE.cast2int(sgn))
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
  val () = act.sa_flags := SA_RESTART // HX: even this cannot affect [sleep]!
  val err = sigaction_null (SIGINT, act)
  val () = if err < 0 then (perror "sigaction"; exit (EXIT_FAILURE))
  val () = printf ("Type CTRL-C in the next 10 seconds.\n", @())
  val time0 = time_get ()
  val () = assertloc ($UNSAFE.cast2lint(time0) >= 0L)
  val _leftover = sleep (10)
  val time1 = time_get ()
  val () = assertloc ($UNSAFE.cast2lint(time1) >= 0L)
  val () = printf ("Slept for %ld seconds.\n", @($UNSAFE.cast2lint(difftime(time1,time0))))
  val sgn = theSignum_get ()
  val () = if (sgn > 0) then
    printf ("Got signal(%d).\n", @(sgn))
  else
    printf ("Got no signal.\n", @())
  // end of [if]
} // end of [main]

(* ****** ****** *)

(* end of [AUP_9_1_7.dats] *)
