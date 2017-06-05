//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 535 - 537
// section 8.2.3: AF_INET Socket Addresses
//
(* ****** ****** *)

staload "libc/SATS/fcntl.sats"
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/socket.sats"
staload "libc/sys/SATS/sockaddr.sats"
staload "libc/netinet/SATS/in.sats"
staload "libc/sys/SATS/socket_in.sats"
staload "libc/arpa/SATS/inet.sats"

(* ****** ****** *)

#define REQUEST "GET / HTTP/1.0\r\n\r\n"

implement
main () = () where {
//
  var sa: sockaddr_in
//
  val () = sa.sin_family := AF_INET
  val () = sa.sin_port := in_port_nbo_of_int (80)
  val () = sa.sin_addr.s_addr := inet_addr ("209.191.122.70")
//
  val (pfskt | fd) = socket_family_type_exn (AF_INET, SOCK_STREAM)
  val () = connect_in_exn (pfskt  | fd, sa)
  val len = string_length (REQUEST)
  val () = socket_write_substring (pfskt | fd, REQUEST, 0, len)
  #define BUFSZ 1024 // HX: hopefully, this is large enough
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  val nread = socket_read_err (pfskt | fd, !p_buf, BUFSZ)
  val () = assertloc (nread >= 1)
  val nread = size1_of_ssize1 (nread)
  val (pfout | ()) = stdout_fildes_view_get ()
  val _err = write_err (pfout | STDOUT_FILENO, !p_buf, nread-1)
  val () = stdout_fildes_view_set (pfout | (*none*))
  val () = socket_close_exn (pfskt | fd)
} // end of [main]

(* ****** ****** *)

(* end of [AUP_8_2_3.dats] *)
