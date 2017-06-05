//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 653 - 656
// section 9.7.2: sleep System Call
//
(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/pointer.dats"

(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"
macdef int_of_pid = $T.int_of_pid
staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/signal.sats"
staload "libc/SATS/stdio.sats"
staload "libc/SATS/stdlib.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)
//
// HX: splitting the code into [mysleep0] and [mysleep1] makes
// the logic considerably easy to follow  
//
(* ****** ****** *)

fun mysleep0 {n:nat}
  (nsec: uint n): uInt = let
//
  var set: sigset_t and set2: sigset_t
  val err = sigemptyset (set)
  val () = assertloc (err = 0)
  prval () = opt_unsome {sigset_t} (set)
  val err = sigaddset (set, SIGALRM)
  val () = assertloc (err >= 0)
  val err = sigprocmask (SIG_BLOCK, set, set2) // SIGALRM is blocked
  val () = assertloc (err >= 0)
  prval () = opt_unsome {sigset_t} (set2)
//
  var act: sigaction and act2: sigaction
  val () = ptr_zero<sigaction>
    (__assert () | act) where {
    extern prfun __assert (): NULLABLE (sigaction)
  } // end of [val]
  // HX: note that SIGUSR1 kills the process if there is no handler for it
  val () = act.sa_handler := sighandler(lam (sgn:signum_t): void => ())
  val err = sigaction (SIGALRM, act, act2)
  val () = assertloc (err = 0)
  prval () = opt_unsome {sigaction} (act2)
//
  val (pf_alarm | _) = alarm_set (nsec)
//
  val () = set := set2
  val err = sigdelset (set, SIGALRM)
  val () = assertloc (err = 0)
  val rtn = sigsuspend (set) // there is really no need for error-checking!
//
  val err = sigaction_null (SIGALRM, act2)
  val () = assertloc (err = 0)
  val err = sigprocmask_null (SIG_SETMASK, set2)
  val () = assertloc (err = 0)
//
in
  alarm_cancel (pf_alarm | (*none*))
end // end [mysleep0]

(* ****** ****** *)

fun mysleep1 {n:nat}
  (nsec: uint n): uInt = let
  val (pf0 | leftover0) = alarm_set (0U)
  prval () = alarm_v_elim (pf0)
in
  if leftover0 = 0U then mysleep0 (nsec)
  else if leftover0 <= nsec then let
    val diff = nsec - leftover0
    val leftover1 = mysleep0 (leftover0)
    val () = if leftover1 > 0U then let
      val (pf_alarm | _) = alarm_set (leftover1)
      prval () = __assert (pf_alarm) where {
        extern prfun __assert {i:nat} (pf: alarm_v i): void
      } // end of [prval]
    in
      // nothing
    end // end of [val]
  in
    diff + leftover1
  end else let
    val diff = leftover0 - nsec
    val leftover1 = mysleep0 (nsec)
    val (pf_alarm | _) = alarm_set (diff + leftover1)
    prval () = __assert (pf_alarm) where {
      extern prfun __assert {i:nat} (pf: alarm_v i): void
    } // end of [prval]
  in
    leftover1
  end // end of [if]
end // end of [mysleep1]

(* ****** ****** *)

implement
main () = () where {
  val leftover = mysleep1 (1U)
  val () = (print "leftover(0) = "; print leftover; print_newline ())
//
  val (pf_alarm | _) = alarm_set (4U)
//
  val leftover = mysleep1 (2U)
  val () = (print "leftover(0) = "; print leftover; print_newline ())
  val leftover = mysleep1 (3U)
  val () = (print "leftover(1) = "; print leftover; print_newline ())
//
  val _ = alarm_cancel (pf_alarm | (*none*))
} // end of [main]

(* ****** ****** *)

(* end of [AUP_9_7_2.dats] *)
