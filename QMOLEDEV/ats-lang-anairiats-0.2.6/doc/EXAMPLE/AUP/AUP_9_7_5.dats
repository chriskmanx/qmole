//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 660 - 663
// section 9.7.5: Realtime Clocks
//
(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/pointer.dats"

(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"
typedef time_t = $T.time_t
staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/signal.sats"
staload "libc/SATS/stdio.sats"
staload "libc/SATS/stdlib.sats"
staload "libc/SATS/time.sats"
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/time.sats"

(* ****** ****** *)

macdef _2lint(x) = $UNSAFE.cast2lint ,(x)

fun test_clocks
  (): void = () where {
  var tm: time_t
  val yn = time_get_and_set (tm)
  val () = assertloc (yn) 
  prval () = opt_unsome {time_t} (tm)
  val () = printf ("time() = %ld secs.\n", @(_2lint(tm)))
  var ts: timespec
  val () = printf ("CLOCK_REALTIME:\n", @())
  val err = clock_gettime (CLOCK_REALTIME, ts)
  val () = assertloc (err >= 0)
  prval () = opt_unsome {timespec} (ts)
  val () = printf ("gettime() = %ld.%09ld secs.\n", @(_2lint(ts.tv_sec), _2lint(ts.tv_nsec)))
  val err = clock_getres (CLOCK_REALTIME, ts)
  val () = assertloc (err >= 0)
  prval () = opt_unsome {timespec} (ts)
  val () = printf ("getres()  = %ld.%09ld secs.\n", @(_2lint(ts.tv_sec), _2lint(ts.tv_nsec)))
} // end of [clocks]

(* ****** ****** *)

implement
main () = () where {
  val () = test_clocks ()
} // end of [main]

(* ****** ****** *)

(* end of [AUP_9_7_5.dats] *)
