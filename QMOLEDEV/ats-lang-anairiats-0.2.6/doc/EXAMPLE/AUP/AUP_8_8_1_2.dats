//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 578 - 585
// section 8.8.1: Host Functions
//
(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"
staload _(*UNSAFE*) = "prelude/DATS/unsafe.dats"

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/netdb.sats"
staload "libc/SATS/string.sats"
staload "libc/sys/SATS/sockaddr.sats"
staload "libc/netinet/SATS/in.sats"
staload "libc/sys/SATS/socket_in.sats"
staload "libc/arpa/SATS/inet.sats"

(* ****** ****** *)

fun display_hostent
  (h: &hostent): void = let
  val (fpf_name | name) = hostent_get_name (h)
  val () = printf (
    "name: %s; type: %d; len: %d\n"
  , @($UNSAFE.castvwtp1{string} (name), h.h_addrtype, h.h_length)
  ) // end of [printf]
  prval () = fpf_name (name)
//
  val (pf, fpf | p) = hostent_get_aliases (h)
  val n = ptrarr_size (!p)
  prval (pf1 , fpf1) = ptrarr_takeout{string} (pf)
  val () = loop (!p, n, 0) where {
    fun loop {n,i:nat | i <= n} .<n-i>.
      (A: &(@[string][n]), n: size_t n, i: size_t i): void =
      if i < n then let
        val () = printf ("\t%s\n", @(A.[i])) in loop (A, n, i+1)
      end else () // end of [loop]
    // end of [loop]
  } // end of [val]
  prval () = pf := fpf1 (pf1)
  prval () = fpf (pf)
//
  val () = if
    (h.h_addrtype = $UNSAFE.cast2int(AF_INET)) then let
//
  val (pf, fpf | p) = hostent_get_addr_list (h)
  val n = ptrarr_size (!p)
  prval (pf1 , fpf1) = ptrarr_takeout{Ptr1} (pf)
  val () = loop (!p, n, 0) where {
    fun loop {n,i:nat | i <= n} .<n-i>.
      (A: &(@[Ptr1][n]), n: size_t n, i: size_t i): void =
      if i < n then let
        val (fpf_addr | addr) = inet_ntoa
          ($UNSAFE.ptrget<in_addr_struct>(A.[i]))
        val () = printf ("\t%s\n", @($UNSAFE.castvwtp1{string}(addr)))
        prval () = fpf_addr (addr)
      in
        loop (A, n, i+1)
      end else () // end of [loop]
    // end of [loop]
  } // end of [val]
  prval () = pf := fpf1 (pf1)
  prval () = fpf (pf)    
//
  in
    // nothing
  end // end of [val]
//
in
  // nothing
end // end of [display_hostent]

(* ****** ****** *)

fun test_gethostbyname
  (): void = () where {
  val NAME = "www.yahoo.com"
  val (pfopt | p) = gethostbyname (NAME)
  val () = if (p > null) then let
    prval Some_v @(pf, fpf) = pfopt
    val () = display_hostent (!p)
    prval () = fpf (pf)
  in
    // nothing
  end else let
    prval None_v () = pfopt
    val () = printf ("The host (%s) is not found!\n", @(NAME))
  in
    (*nothing*)
  end // end of [val]
} // end of [test_gethostbyname]

(* ****** ****** *)

fun test_gethostbyaddr () = () where {
  val ADDR = "67.195.160.76"
  var a: in_addr_nbo_t = inet_addr (ADDR)
  val () = assertloc ($UNSAFE.cast2int (a) >= 0)
  val (pfopt | p) = gethostbyaddr (a, sizeof<in_addr_nbo_t>, AF_INET)
  val () = if (p > null) then let
    prval Some_v @(pf, fpf) = pfopt
    val () = display_hostent (!p)
    prval () = fpf (pf)
  in
    // nothing
  end else let
    prval None_v () = pfopt
    val () = printf ("The host (%s) is not found!\n", @(ADDR))
  in
    (*nothing*)
  end // end of [if]
} // end of [test_gethostbyaddr]

(* ****** ****** *)

staload STDIO = "libc/SATS/stdio.sats"
fun fstrnprint {n:nat}
  (out: FILEref, buf: &bytes(n), n: size_t n): int = let
  var nerr: int = 0
  var i: sizeLte n
  val _0 = size1_of_int1 (0)
  val () = for
    (i := _0; i < n; i := i+1) let
    val c = char_of_byte (buf.[i])
  in
    if c = '\000' then break else
      (if ($STDIO.fputc_err (c, out) < 0) then (nerr := nerr + 1; break))
  end // end of [val]
in
  nerr
end // end of [fstrnprint]

(* ****** ****** *)

fun test_getnameinfo () = let
  var sa: sockaddr_in
  val ADDR = "209.191.122.70"
  val s_addr = inet_addr (ADDR)
  val () = assertloc ($UNSAFE.cast2int(s_addr) >= 0)
  val nport = in_port_nbo_of_int (80)
  val () = sockaddr_in_init (sa, AF_INET, s_addr, nport)
  #define NODELEN 256
  #define SERVLEN 256
  var !p_nodename = @[byte][NODELEN]()
  var !p_servname = @[byte][SERVLEN]()
  val err = getnameinfo{sockaddr_in}
    (sa, sizeof<sockaddr_in>, !p_nodename, NODELEN, !p_servname, SERVLEN, NI_ZERO)
  val () = if err >= 0 then let
    val () = fprint (stdout_ref, "node: ")
    val _err = fstrnprint (stdout_ref, !p_nodename, NODELEN)
    val () = fprint (stdout_ref, "; service: ")
    val _err = fstrnprint (stdout_ref, !p_servname, SERVLEN)
    val () = fprint_newline (stdout_ref)
  in
    // nothing
  end else let
    val (fpf_str | str) = gai_strerror (err)
    val () = fprint (stdout_ref, "[getnameinfo] failed: ")
    val () = fprint_strptr (stdout_ref, str)
    prval () = fpf_str (str)
    val () = fprint_newline (stdout_ref)
  in
    // nothing
  end // end of [val]
in
  // nothing
end // end of [test_getnameinfo]

(* ****** ****** *)

implement
main () = () where {
//
  val () = test_gethostbyname ()
  val () = test_gethostbyaddr ()
//
  val () = test_getnameinfo () // thist a modern version
//
} // end of [main]

(* ****** ****** *)

(* end of [AUP_8_8_1_2.dats] *)
