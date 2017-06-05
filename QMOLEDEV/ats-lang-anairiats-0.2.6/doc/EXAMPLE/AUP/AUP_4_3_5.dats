//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 230 - 232
// section 4.3.5: Session-related System Calls
//
(* ****** ****** *)
//
staload TYPES = "libc/sys/SATS/types.sats"
macdef pid(x) = $TYPES.pid_of_int ,(x)
macdef long(x) = $TYPES.lint_of_pid ,(x)
//
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/signal.sats"
staload "libc/SATS/termios.sats"
staload "libc/SATS/unistd.sats"
//
(* ****** ****** *)

typedef strcst = string

(* ****** ****** *)

extern
fun showpginfo (msg: strcst): void = "showpginfo"
implement
showpginfo
  (msg) = () where {
  var err: int = 0
  val () = printf ("%s\n", @(msg))
  val () = printf (
    "\tprocess ID = %ld; process-group ID = %ld\n", @(long(getsid((pid)0)), long(getpgid((pid)0)))
  ) // end of [val]
  val (pfopt | fd) = open_flag_err ("/dev/tty", O_RDWR)
  val () = assert_errmsg (fd >= 0, #LOCATION)
  prval open_v_succ (pf) = pfopt
  val () = printf (
    "\tcontrolling terminal's foreground progress-group ID = %ld\n", @(long(tcgetpgrp(fd)))
  ) // end of [val]
  val () = printf ("\tcontrolling-terminal's session ID = %ld\n", @(long(tcgetsid(fd))))
  val () = close_exn (pf | fd)
} // end of [showpginfo]

(* ****** ****** *)

extern
fun maininit (): void = "maininit"
implement
main () = () where {
  val () = maininit ()
  val () = showpginfo ("initial call")
  val () = while (true) let
    val _leftover = sleep (1000000) in (*nothing*)
  end // end of [val]
} // end of [main]

(* ****** ****** *)

%{$

static
void catchsig (int signo) {
  if (signo == SIGCONT) showpginfo ("SIGCONT caught") ;
  return ;
} // end of [catchsig]

void
maininit () {
  int err ;
  struct sigaction act ;
  memset (&act, 0, sizeof(act)) ;
  act.sa_handler = catchsig ;
  err = sigaction (SIGCONT, &act, NULL) ;
  if (err == -1) exit (1) ;
  return ;
} // end of [main1]

%} // end of [%{$]

(* ****** ****** *)

(* end of [AUP_4_3_5.dats] *)
