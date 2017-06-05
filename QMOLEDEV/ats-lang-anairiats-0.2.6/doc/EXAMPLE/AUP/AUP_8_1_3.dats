//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 527 - 530
// section 8.1.3: Handling Multiple Clients
//
(* ****** ****** *)

#define SOCKETNAME "MySocket"

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/printf.sats"
staload "libc/SATS/stdio.sats"
staload "libc/SATS/stdlib.sats"
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/select.sats"
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

fun run_client
  (sa: &sockaddr_un): bool = let
  val pid = fork_err ()
  val ipid = int_of_pid (pid)
in
//
case+ 0 of
| _ when ipid = 0 => let // child
  var !p_buf with pf_buf = @[byte][64]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
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
//
  val cpid = getpid ()
  val cpid = (lint_of_pid)cpid
//
  val _n = snprintf (pf_buf | p_buf, 64, "Hello from %ld!", @(cpid))
  val n = strbuf_length (!p_buf)
  val () = socket_write_substring
    (pfskt | fd, __cast !p_buf, 0, n) where {
    extern castfn __cast {m,n:nat} (x: &strbuf (m,n)):<> string n
  } // end of [val]
  prval () = pf_buf := bytes_v_of_strbuf_v (pf_buf)
  val nread = socket_read_exn (pfskt | fd, !p_buf, 64)        
  val () = (print "Client got: "; print_bytes_size (!p_buf, nread); print_newline ())
  val () = socket_close_exn (pfskt | fd)
in
  exit (EXIT_SUCCESS)
end // end of [ipid = 0]
//
| _ when ipid > 0 => true
//
| _ (* ipid=-1 *) => false
// end of [case]
end // end of [run_client]

(* ****** ****** *)

fun lower_hwm {fd:nat}
  (set: &fd_set, fd: int fd): int =
  if fd > 0 then (
    if ~FD_ISSET (fd, set) then lower_hwm (set, fd - 1) else fd
  ) else 0 // end of [if]
// end of [lower_hwm]

(* ****** ****** *)

fun run_server
  (sa: &sockaddr_un): bool = let
//
  #define BUFSZ 128
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
//
  var set: fd_set
  var read_set: fd_set?
  val () = FD_ZERO (set)
//
  var fd_hwm: int = ~1
//
  val [fd:int] (pfopt | sfd) = socket_family_type_err (AF_UNIX, SOCK_STREAM)
in
//
if sfd >= 0 then let
  prval Some_v (pfskt) = pfopt
  val () = bind_un_exn (pfskt | sfd, sa)
  val () = listen_exn (pfskt | sfd, SOMAXCONN)
  prval pfskt = pfskt
  val () = if (sfd > fd_hwm) then fd_hwm := sfd
//
  extern prfun __attach
    {fd:int} (pf: socket_v (fd, conn) | fd: int fd):<> void
  extern prfun __detach {fd:int} (fd: int fd):<> socket_v (fd, conn)
//
  var nerr: int = 0
  val () = FD_SET (sfd, set)
  val () = while (true) let
    val () = read_set := set
    val rtn = select0 (fd_hwm+1, read_set, null, null, null) where {
      extern fun select0
        (n: int, set: &fd_set, _: ptr, _: ptr, _: ptr): int = "mac#atslib_select"
      // end of [extern]
    } // end of [val]
(*
    val () = (print "select: rtn = "; print rtn; print_newline ())
*)
    val () = if (rtn < 0) then let
      val () = (perror "select"; nerr := nerr + 1; break) in (*nothing*)
    end // end of [val]
    var fd: Nat // uninitialized
    val () = for* (read_set: fd_set) =>
      (fd := 0; fd <= fd_hwm; fd := fd + 1)
      if FD_ISSET (fd, read_set) then (
        if (fd = sfd) then let
          val (pfopt | cfd) = accept_null_err (pfskt | sfd)
        in
          if (cfd >= 0) then let
            prval Some_v (pfconn) = pfopt
            prval () = __attach (pfconn | cfd)
            val () = FD_SET (cfd, set)
          in
            if (cfd > fd_hwm) then fd_hwm := cfd
          end else let
            prval None_v () = pfopt
          in
            (perror "accept"; nerr := nerr + 1; break)
          end // end of [val]
        end else let
          prval pfconn = __detach (fd)
          val nread = socket_read_err (pfconn | fd, !p_buf, BUFSZ)
        in
          if nread >= 0 then let
            val nread = size1_of_ssize1 (nread)
          in
            if nread = 0 then let
              val () = FD_CLR (fd, set)
              val () = socket_close_exn (pfconn | fd)
              val () = if (fd_hwm = fd) then fd_hwm := lower_hwm (set, fd)
            in
              // nothing
            end else let
              val () = (
                print "Server got: "; print_bytes_size (!p_buf, nread); print_newline ()
              ) // end of [val]
              val () = socket_write_substring (pfconn | fd, "Goodbye!", 0, 8)
              prval () = __attach (pfconn | fd)
            in
              // nothing
            end // end of [if]
          end else let
            prval () = __attach (pfconn | fd)
          in
            perror "socket read"; nerr := nerr + 1; break
          end // end of [if]
        end // end of [if]
      ) // end of [if]
    // end of [for] // end of [val]
  in
    if nerr > 0 then break
  end // end of [val]
  val () = socket_close_exn (pfskt | sfd)
in
  if nerr = 0 then true else false
end else let
  prval None_v () = pfopt
  val () = perror "socket"
in
  false
end // end of [if]
//
end // end of [run_server]

(* ****** ****** *)

implement
main () = () where {
//
  var sa: sockaddr_un
  val _err = unlink (SOCKETNAME)
  val () = sockaddr_un_init (sa, AF_UNIX, SOCKETNAME)
//
  var nerr: int = 0
//
  var i: int
  val () = for
    (i := 1; i <= 4; i := i+1) let
    val rtn = run_client (sa)
  in
    if ~rtn then (nerr := nerr + 1; break)
  end // end of [val]
  val () = if (nerr = 0) then
    (print "All the clients have been started"; print_newline ())
//
  val () = if (nerr = 0) then let
    val rtn = run_server (sa) in if ~rtn then (nerr := nerr + 1)
  end // end of [val]
  val () = if (nerr = 0) then
    (print "The server have been started"; print_newline ())
//
  val () = if nerr > 0 then exit (EXIT_FAILURE)
//
  val () = exit (EXIT_SUCCESS)
//
} // end of [main]

(* ****** ****** *)

(* end of [AUP_8_1_3.dats] *)
