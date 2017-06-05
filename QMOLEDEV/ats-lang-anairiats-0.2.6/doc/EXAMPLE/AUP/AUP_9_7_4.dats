//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 657 - 660
// section 9.7.4: Baisc Interval-Timer System Calls
//
(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/pointer.dats"

(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"
typedef time_t = $T.time_t
typedef suseconds_t = $T.suseconds_t
staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/signal.sats"
staload "libc/SATS/stdio.sats"
staload "libc/SATS/stdlib.sats"
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/time.sats"

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

val theMsg = "Hello for the timer!\n"
val theMsgLen = string_length (theMsg)

fun test_timer (): void = let
  var act: sigaction?
  val () = ptr_zero<sigaction>
    (__assert () | act) where {
    extern prfun __assert (): NULLABLE (sigaction)
  } // end of [val]
  fun handler (sgn: signum_t): void = let
    val (pf_stdout | ()) = stdout_fildes_view_get ()
    val _err = write_substring_err (pf_stdout | STDOUT_FILENO, theMsg, 0, theMsgLen)
    val () = stdout_fildes_view_set (pf_stdout | (*none*))
  in
  end // end of [handler]  
  val () = act.sa_flags := SA_RESTART // HX: allowing [read] to restart
  val () = act.sa_handler := sighandler(handler)
  val err = sigaction_null (SIGALRM, act)
  val () = assertloc (err = 0)
//
  var itv: itimerval?
  val _2 = $UNSAFE.cast{time_t} (2)
  val () = ptr_zero<itimerval>
    (__assert () | itv) where {
    extern prfun __assert (): NULLABLE (itimerval)
  } // end of [val]
  val () = itv.it_interval.tv_sec := _2; val () = itv.it_value.tv_sec := _2
  val err = setitimer_null (ITIMER_REAL, itv)
  val () = assertloc (err = 0)
//
  #define BUFSZ 128
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  var nerr: int = 0
  val () = while (true) let
    val (pf_stdin | ()) = stdin_fildes_view_get ()
    val nread = read_err (pf_stdin | STDIN_FILENO, !p_buf, BUFSZ)
    val () = stdin_fildes_view_set (pf_stdin | (*none*))
  in
    if nread >= 0 then let
      val nread = size1_of_ssize1 (nread)
    in
      if nread > 0 then let
        val () = print_bytes_size (!p_buf, nread)        
      in
        continue
      end else (
        printf ("EOF\n", @()); break
      ) // end of [if]
    end else let
      val () = perror "read" in nerr := nerr + 1; break
    end // end of [if]
  end // end of [val]
  val () = if (nerr > 0) then exit (EXIT_FAILURE)
in
  // nothing
end // end of [test_timer]

(* ****** ****** *)

implement
main () = () where {
  val () = test_timer ()
} // end of [main]

(* ****** ****** *)

(* end of [AUP_9_7_4.dats] *)
