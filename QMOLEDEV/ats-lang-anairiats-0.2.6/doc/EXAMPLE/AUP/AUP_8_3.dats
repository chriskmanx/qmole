//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 544 - 549
// section 8.3: Socket Options
//
(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/string.sats"
staload "libc/sys/SATS/sockaddr.sats"
staload "libc/sys/SATS/sockopt.sats"
staload "libc/sys/SATS/socket.sats"
staload "libc/sys/SATS/time.sats"

(* ****** ****** *)

datatype OPTKIND = OPTKINDint | OPTKINDlinger | OPTKINDtimeval

fun show {fd:nat} (
    skt: int fd
  , level: int, option: int, name: string
  , knd: OPTKIND
  ) : void =
  case+ knd of
  | OPTKINDint () => let
      var n: int?
      val err = getsockopt_err{int} (skt, level, option, n, sizeof<int>)
    in
      if err >= 0 then let
        prval () = opt_unsome {int} (n) in printf ("%s = %d\n", @(name, n))
      end else let
        prval () = opt_unnone {int} (n)
        val (fpf_str | str) = strerror (errno_get())
        val () = printf ("%s FAILED (%s)\n", @(name, $UNSAFE.castvwtp1{string} (str)))
        prval () = fpf_str (str)
      in
        // nothing
      end // end of [if]
    end (* end of [OPTKINDint] *)
  | OPTKINDlinger () => let
      var lg: linger?
      val err = getsockopt_err{linger} (skt, level, option, lg, sizeof<linger>)
    in
      if err >= 0 then let
        prval () = opt_unsome {linger} (lg)
      in
        printf ("%s = %d(on/off) / %d sec(linger)\n", @(name, lg.l_onoff, lg.l_linger))
      end else let
        prval () = opt_unnone {linger} (lg)
        val (fpf_str | str) = strerror (errno_get())
        val () = printf ("%s FAILED (%s)\n", @(name, $UNSAFE.castvwtp1{string} (str)))
        prval () = fpf_str (str)
      in
        // nothing
      end // end of [if]
    end (* end of [OPTKINDlinger] *)
  | OPTKINDtimeval () => let
      var tv: timeval?
      val err = getsockopt_err{timeval} (skt, level, option, tv, sizeof<timeval>)
    in
      if err >= 0 then let
        prval () = opt_unsome {timeval} (tv)
      in
        printf ("%s = %ld sec / %ld usec\n",
          @(name, $UNSAFE.cast2lint(tv.tv_sec), $UNSAFE.cast2lint(tv.tv_usec))
        ) // end of [printf]
      end else let
        prval () = opt_unnone {timeval} (tv)
        val (fpf_str | str) = strerror (errno_get())
        val () = printf ("%s FAILED (%s)\n", @(name, $UNSAFE.castvwtp1{string} (str)))
        prval () = fpf_str (str)
      in
        // nothing
      end // end of [if]
    end (* end of [OPTKINDtimeval] *)
// end of [show]

(* ****** ****** *)

fun showall {fd:nat}
  (skt: int fd, caption: string): void = () where {
  val () = printf ("\n%s\n", @(caption))
//
  val () = show (skt, SOL_SOCKET, SO_ACCEPTCONN, "SO_ACCEPTCONN", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_BROADCAST, "SO_BROADCAST", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_DEBUG, "SO_DEBUG", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_DONTROUTE, "SO_DONTROUTE", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_ERROR, "SO_ERROR", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_KEEPALIVE, "SO_KEEPALIVE", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_LINGER, "SO_LINGER", OPTKINDlinger)
  val () = show (skt, SOL_SOCKET, SO_OOBINLINE, "SO_OOBINLINE", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_RCVBUF, "SO_RCVBUF", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_RCVLOWAT, "SO_RCVLOWAT", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_RCVTIMEO, "SO_RCVTIMEO", OPTKINDtimeval)
  val () = show (skt, SOL_SOCKET, SO_REUSEADDR, "SO_REUSEADDR", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_SNDBUF, "SO_SNDBUF", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_SNDLOWAT, "SO_SNDLOWAT", OPTKINDint)
  val () = show (skt, SOL_SOCKET, SO_SNDTIMEO, "SO_SNDTIMEO", OPTKINDtimeval)
  val () = show (skt, SOL_SOCKET, SO_TYPE, "SO_TYPE", OPTKINDint)
//
} // end of [showall]

(* ****** ****** *)

implement
main () = () where {
//
  val (pfskt | skt) = socket_family_type_exn (AF_INET, SOCK_STREAM)
  val () = showall (skt, "AF_INET/SOCKET_STREAM")
  val () = socket_close_exn (pfskt | skt)
//
  val (pfskt | skt) = socket_family_type_exn (AF_INET, SOCK_DGRAM)
  val () = showall (skt, "AF_INET/SOCKET_DGRAM")
  val () = socket_close_exn (pfskt | skt)
//
} // end of [main]

(* ****** ****** *)

(* end of [AUP_8_3.dats] *)
