//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 520 - 524
// section 8.1.1: How Sockets Work
//
(* ****** ****** *)

#define SOCKETNAME "MySocket"

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/stdio.sats"
staload "libc/SATS/stdlib.sats"
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/types.sats"

(* ****** ****** *)

staload "libc/sys/SATS/sockaddr.sats"
staload "libc/sys/SATS/socket.sats"
staload "libc/sys/SATS/un.sats"
staload "libc/sys/SATS/socket_un.sats"

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
//
  var sa: sockaddr_un
  val _err = unlink (SOCKETNAME)
  val () = sockaddr_un_init (sa, AF_UNIX, SOCKETNAME)
//
  val pid = fork_err ()
  val ipid = int_of_pid (pid)
  val () = case+ 0 of
    | _ when ipid = 0 => let // child
        var !p_buf with pf_buf = @[byte][64]()
        val [fd:int] (pfskt | fd) = socket_family_type_exn (AF_UNIX, SOCK_STREAM)
        val () = loop (pfskt | fd, sa) where {
          fun loop (
              pfskt: !socket_v (fd, init) >> socket_v (fd, conn) | fd: int fd, sa: &sockaddr_un
            ) : void = let
            prval () = sockaddr_un_trans (view@ sa)
            val (pfopt | err) = connect_err (pfskt | fd, sa, socklen_un)
            prval () = sockaddr_trans_un (view@ sa)
          in
            if err >= 0 then let
              prval connect_v_succ pf = pfopt; prval () = pfskt := pf
            in
              // nothing
            end else let
              prval connect_v_fail pf = pfopt; prval () = pfskt := pf
              val errno = errno_get ()
            in
              if (errno = ENOENT) then let
                val _leftover = sleep (1) in loop (pfskt | fd, sa)
              end else let
                val () = exit (EXIT_FAILURE) in loop (pfskt | fd, sa)
              end // end of [if]
            end // end of [if]
          end // end of [loop]
        } // end of [val]
        val () = socket_write_substring (pfskt | fd, "Hello!", 0, 6)
        var !p_buf with pf_buf = @[byte][64]()
        prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
        val nread = socket_read_exn (pfskt | fd, !p_buf, 64)        
        val () = (print "Client got: "; print_bytes_size (!p_buf, nread); print_newline ())
        val () = socket_close_exn (pfskt | fd)
      in
        exit (EXIT_SUCCESS)
      end // end of [ipid = 0]
    | _ when ipid > 0 => let // parent
        val [fd:int] (pfskt | fd) = socket_family_type_exn (AF_UNIX, SOCK_STREAM)
        val () = bind_un_exn (pfskt | fd, sa)
        val () = listen_exn (pfskt | fd, SOMAXCONN)
        val (pfskt_c | fd_c) = accept_null_exn (pfskt | fd)
        var !p_buf with pf_buf = @[byte][64]()
        prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
        val nread = socket_read_exn (pfskt_c | fd_c, !p_buf, 64)
        val () = (print "Server got: "; print_bytes_size (!p_buf, nread); print_newline ())
        val () = socket_write_substring (pfskt_c | fd_c, "Goodbye!", 0, 8)
        val () = socket_close_exn (pfskt | fd)
        val () = socket_close_exn (pfskt_c | fd_c)
      in
        exit (EXIT_SUCCESS)
      end // end of [ipid > 0]
    | _ => let // [fork] failed
        val () = perror ("fork") in exit (EXIT_FAILURE)
      end // end of [val]
} // end of [main]

(* ****** ****** *)

(* end of [AUP_8_1_1.dats] *)
