//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 535 - 537
// section 8.2.6: getaddrinfo
//
(* ****** ****** *)

staload "prelude/DATS/pointer.dats"

(* ****** ****** *)

staload "libc/SATS/netdb.sats"
staload "libc/sys/SATS/sockaddr.sats"
staload "libc/sys/SATS/socket.sats"
staload "libc/arpa/SATS/inet.sats"

(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

implement
main () = () where {
  typedef AI = addrinfo(0) 
  var hint: AI
  val () = ptr_zero<AI>
    (__assert () | hint) where {
    extern prfun __assert (): NULLABLE (AI)
  } // end of [val]
  val () = hint.ai_family := AF_INET
  val () = hint.ai_socktype := SOCK_STREAM
  var infop: addrinfoptr
  val err = getaddrinfo ("www.yahoo.com", "80", hint, infop)
  val () = if err < 0 then let
    val (fpf_str | str) = gai_strerror (err)
    val () = (print "GAI ERROR: "; print str; print_newline ())
    prval () = fpf_str (str)
  in
    // nothing
  end // end of [val]
  val () = assertloc (err = 0)
  prval () = opt_unsome {addrinfoptr} (infop)
  val () = loop (infop) where {
    fun loop {l:addr}
      (infop: !addrinfoptr l): void =
      if addrinfoptr_isnot_null (infop) then let
        val prot = addrinfoptr_get_protocol (infop)
        val prot = $UNSAFE.cast {int} (prot)
        val (pf, fpf | p) = addrinfoptr_get_addr_in (infop)
        val nport = ntohs (uint16_of_in_port_nbo(p->sin_port))
        val (fpf_str | str) = inet_ntoa (p->sin_addr)
        prval () = minus_addback (fpf, pf | infop)
        val () = (
          print str; print " port: "; print nport; print " protocol: "; print prot; print_newline ()
        ) // end of [val]
        prval () = fpf_str (str)
        val (fpf1 | infop1) = addrinfoptr_get_next (infop)
        val () = loop (infop1)
        prval () = minus_addback (fpf1, infop1 | infop)
      in
        // nothing
      end else () // end of [if]
    // end of [loop]
  } // end of [val]
  val () = freeaddrinfo (infop)
} // end of [main]

(* ****** ****** *)

(* end of [AUP_8_2_6.dats] *)
