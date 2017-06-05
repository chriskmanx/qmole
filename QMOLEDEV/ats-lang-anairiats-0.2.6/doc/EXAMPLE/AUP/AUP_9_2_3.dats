//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 628 - 634
// section 9.2.3: sigsuspend System Call
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
// HX: any order printing order is possible: child/parent or parent/child
//
fun try1 () = let
  val pid = fork_err ()
  val ipid = int_of_pid (pid)
in
  case+ 0 of
  | _ when ipid = 0 => let
      val cpid = getpid ()
      val () = printf ("try1: child(%ld)\n", @($UNSAFE.cast2lint(cpid)))
    in
      exit (EXIT_SUCCESS)
    end // end of [_]
  | _ when ipid > 0 => let
      val () = printf ("parent(%d)\n", @(ipid))
    in
      // nothing
    end // end of [ipid > 0]
  | _ (*ipid=-1*) => let
      val () = perror ("fork")
    in
      exit (EXIT_SUCCESS)
    end // end of [_]
end // end of [try1]

(* ****** ****** *)
//
// this one uses [sigsuspend]
//
fun try4 () = let
  typedef T = sigset_t
  var set: T
  val err = sigemptyset (set)
  val () = assertloc (err = 0)
  prval () = opt_unsome{T} (set)
  val err = sigaddset (set, SIGUSR1) // SIGUSR1 is blocked
  val () = assertloc (err = 0)
  val err = sigprocmask_null (SIG_SETMASK, set)
  val pid = fork_err ()
  val ipid = int_of_pid (pid)
in
  case+ 0 of
  | _ when ipid = 0 => let // child
//
      var act: sigaction
      val () = ptr_zero<sigaction>
        (__assert () | act) where {
        extern prfun __assert (): NULLABLE (sigaction)
      }
      // HX: note that SIGUSR1 kills the process if there is no handler for it
      val () = act.sa_handler := sighandler(lam (sgn:signum_t): void => ())
      val err = sigaction_null (SIGUSR1, act)
      val () = assertloc (err = 0)
//
      var suspendset: sigset_t
      val err = sigfillset (suspendset)
      val () = assertloc (err = 0)
      prval () = opt_unsome{T} (suspendset)
      val err = sigdelset (suspendset, SIGUSR1) // only [SIGUSR1] is allowed
      val () = assertloc (err = 0)
      val err = sigsuspend (suspendset)
      val () = assertloc (err < 0)
      val () = if (errno_get() = EINTR) then let
        val cpid = getpid ()
        val () = printf ("try4: child(%ld)\n", @($UNSAFE.cast2lint(cpid)))
        val () = exit (EXIT_SUCCESS)
      in
        // nothing
      end // end of [val]
      val () = exit (EXIT_FAILURE)
    in
      // nothing
    end // end of [val]
  | _ when ipid > 0 => let // parent
      val () = printf ("parent(%d)\n", @(ipid))
      val err = kill (pid, SIGUSR1)
      val () = assertloc (err = 0)
    in
      // nothing
    end // end of [ipid > 0]
  | _ (*ipid=-1*) => let
      val () = perror "fork" in (*nothing*)
    end // end of [_]
end // end of [try4]

(* ****** ****** *)
//
// this one uses [sigwait]
//
fun try5 () = let
  var set: sigset_t
  val err = sigemptyset (set)
  val () = assertloc (err = 0)
  prval () = opt_unsome{sigset_t} (set)
  val err = sigaddset (set, SIGUSR1)
  val () = assertloc (err = 0)
  val err = sigprocmask_null (SIG_SETMASK, set)
  val () = assertloc (err = 0)
  val pid = fork_err ()
  val ipid = int_of_pid (pid)
in
  case+ 0 of
  | _ when ipid = 0 => let // child
      var sgn: signum_t
      val err = sigwait (set, sgn) // wait for SIGUSR1
      val () = assertloc (err = 0)
      prval () = opt_unsome{signum_t} (sgn)      
      val cpid = getpid ()
      val () = printf ("try5: child(%ld)\n", @($UNSAFE.cast2lint(cpid)))
    in
      exit (EXIT_SUCCESS)
    end // end of [val]
  | _ when ipid > 0 => let // parent
      val () = printf ("parent(%d)\n", @(ipid))
      val err = kill (pid, SIGUSR1)
      val () = assertloc (err = 0)
    in
      // nothing
    end // end of [ipid > 0]
  | _ (*ipid=-1*) => let
      val () = perror "fork" in (*nothing*)
    end // end of [_]
end // end of [try5]

(* ****** ****** *)
//
// this one uses [pipe]
//
fun try6 () = let
  var fd1: int and fd2: int
  val (pfopt | err) = pipe (fd1, fd2)
  val () = assertloc (err >= 0)
  prval Some_v @(pfd1, pfd2) = pfopt
  val pid = fork_err ()
  val ipid = int_of_pid (pid)
in
  case+ 0 of
  | _ when ipid = 0 => let // child
      val () = close_exn (pfd2 | fd2)
      var c: byte
      prval pfc = array_v_sing (view@ c)
      val nread = read_err (pfd1 | fd1, c, 1)
      prval () = view@ c := array_v_unsing (pfc)
      val () = close_exn (pfd1 | fd1)
      val cpid = getpid ()
      val () = printf ("try6: child(%ld)\n", @($UNSAFE.cast2lint(cpid)))
    in
      exit (EXIT_SUCCESS)
    end // end of [ipid = 0]
  | _ when ipid > 0 => let // parent
      val () = printf ("parent(%d)\n", @(ipid))
      val () = close_exn (pfd1 | fd1)
      val () = close_exn (pfd2 | fd2)
    in
      // nothing
    end // end of [ipid > 0]
  | _ (*ipid=-1*) => let // failure
      val () = close_exn (pfd1 | fd1)
      val () = close_exn (pfd2 | fd2)
      val () = perror ("fork")
    in
      exit (EXIT_SUCCESS)
    end // end of [_]
end // end of [try6]

(* ****** ****** *)

implement
main () = () where {
  val () = try1 ()
  val () = try4 ()
  val () = try5 ()
  val () = try6 ()
  val _leftover = sleep (1)
} // end of [main]

(* ****** ****** *)

(* end of [AUP_9_2_3.dats] *)
