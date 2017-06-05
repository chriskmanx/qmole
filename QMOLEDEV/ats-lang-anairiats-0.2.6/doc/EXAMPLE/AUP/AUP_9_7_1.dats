//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 651 - 653
// section 9.7.1: alarm System Call
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
// HX: this is really an overkill ...
//
fun fprint_bytes_size
  {n1,n2:nat | n2 <= n1} 
  (out: FILEref, buf: &(@[byte][n1]), n2: size_t n2): void = let
  val p_buf = &buf
  prval () = eqsize_byte_one () // sizeof byte == 1
  prval pfmul = mul_make {n2,sizeof(byte)} ()
  prval () = mul_elim {n2,1} (pfmul)
  prval (pf1, pf2) = array_v_split {byte} {n1} {n2} (pfmul, view@ (buf))
  prval pfu = unit_v ()
  typedef env = FILEref
  val () = array_ptr_foreach_funenv_tsz {byte} {unit_v} {env}
    (pfu | !p_buf, lam (pf | x, out) =<> $effmask_ref (fprint_byte (out, x)), n2, sizeof<byte>, out)
  prval unit_v () = pfu
  prval () = view@ (buf) := array_v_unsplit {byte} {n2,n1-n2} (pfmul, pf1, pf2)
in
  // nothing
end // end of [print_buf_size]

fun print_bytes_size
  {n1,n2:nat | n2 <= n1} 
  (buf: &(@[byte][n1]), n2: size_t n2): void = fprint_bytes_size (stdout_ref, buf, n2)
// end of [print_bytes_size]

(* ****** ****** *)

implement
main () = () where {
  var act: sigaction
  val () = ptr_zero<sigaction>
    (__assert () | act) where {
    extern prfun __assert (): NULLABLE (sigaction)
  } // end of [val]
  val () = act.sa_handler := sighandler_of_fun(lam (sgn) => ())
  val err = sigaction_null (SIGALRM, act)
  val () = assertloc (err = 0)
  macdef NSEC = 5U
  val () = printf
    ("You've got %u seconds for input:\n", @(NSEC))
  val (pf_alarm | _) = alarm_set (NSEC)
  val (pf_stdin | ()) = stdin_fildes_view_get ()
  #define BUFSZ 128
  var nerr: int = 0
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  val nread = read_err (pf_stdin  | STDIN_FILENO, !p_buf, BUFSZ)
  val _leftover = alarm_cancel (pf_alarm | (*none*))
  val () = if (nread < 0) then (
    if (errno_get() = EINTR) then
      printf ("Timed out! Please type faster next time.\n", @())
    else (nerr := nerr + 1) // end of [if]
  ) // end of [val]
  val () = stdin_fildes_view_set (pf_stdin | (*none*))
  val () =
if nread >= 0 then let
  val nread = size1_of_ssize1 (nread)
  val () = if nread > 0 then let
    val () = print "Got: "
    val () = print_bytes_size (!p_buf, nread)
  in
    // nothing
  end else
    printf ("Got: EOF\n", @())
  // end of [if]
in
  // nothing
end // end of [if]
  val () = if (nerr > 0) then exit (EXIT_FAILURE)
} // end of [main]

(* ****** ****** *)

(* end of [AUP_9_7_1.dats] *)
