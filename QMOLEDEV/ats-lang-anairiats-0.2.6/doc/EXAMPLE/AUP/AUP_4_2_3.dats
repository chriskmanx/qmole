//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 213 - 218
// section 4.2.3: select System Call
//
(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/select.sats"

(* ****** ****** *)

%{^
#define atslib_read1(fd, c) read(fd, c, 1)
%} // end of [%{^]

fun readany2 {n:nat} (
    fds: &(@[int][n]), n: int n, which: &int
  ) : int = let
//
  var fdset_rd: fd_set
  val () = FD_ZERO (fdset_rd)
//
  var i: natLte n
  var maxfd: intGte 0 = 0
//
  val () = for
    (i := 0; i < n; i := i+1) {
    val fd = fds.[i]
    val fd = int1_of_int (fd)
    val () = assert (fd >= 0)
    val () = FD_SET (fd, fdset_rd)
    val () = if fd > maxfd then maxfd := fd
  } // end of [val]
//
  var nerr: int = 0
//
  val nfd = select (
    maxfd + 1, fdset_rd, null, null, null
  ) where {
    extern fun select (
      _: intGte 1, fdset: &fd_set, _: ptr, _: ptr, _: ptr
    ) : int = "mac#atslib_select"
  } // end of [val]
  val () = if nfd < 0 then nerr := nerr + 1
//
  var c: char = '\0'
//
  val () = if nerr = 0 then (
    for (i := 0; i < n; i := i+1) let
      val fd = fds.[i]
      val fd = int1_of_int (fd)
      val () = assert (fd >= 0)
    in
      if FD_ISSET (fd, fdset_rd) then let
        val () = c := '\0'
//
        extern fun read1
          (fd: int, c: &char): ssize_t = "mac#atslib_read1"
//
        val nread = read1 (fd, c)
        val nread = int_of_ssize (nread)
        val nread = int1_of_int (nread)
        val () = which := i
        val () = if (nread < 0) then (nerr := nerr + 1)
      in
        break
      end // end of [if]
    end // end of [for]
  ) // end of [if]
//
in
  if nerr > 0 then ~1 else (int_of)c
end // end of [readany2]

(* ****** ****** *)

fun test_readany2 () = let
  var !p_fds = @[int](~1, ~1)
  var nerr: int = 0
  extern prfun __leak {v:view} (pf: v): void
  val (pf | fd) = open_flag_err ("/dev/tty", O_RDWR)
  val () = if fd < 0 then let
    val () = prerr "test_readany: open: 0\n" in nerr := nerr + 1
  end // end of [val]
  prval () = __leak (pf)
  val () = p_fds->[0] := fd
  val path = "/dev/pts/5" // HX: change may be needed
  val (pf | fd) = open_flag_err (path, O_RDWR)
  val () = if fd < 0 then let
    val () = prerr "test_readany: open: 1\n" in nerr := nerr + 1
  end // end of [val]
  prval () = __leak (pf)
  val () = p_fds->[1] := fd  
//
  val () = if (nerr = 0) then let
    var which: int = ~1
    val () = while (true) let
      val c = readany2 (!p_fds, 2, which)
      val () = if c > 0 then let
        val c = char_of_int(c)
        val c = (if char_isprint (c) then c else '?'): char
        val () = printf ("Got %c from terminal %d\n", @(c, which))
      in
        continue
      end // end of [val]
      val () = if (c = 0) then
        printf ("EOF from terminal %d\n", @(which)) else nerr := nerr+1
      // end of [val]
    in
      break
    end // end of [val]
  in
    // nothing
  end // end of [val]
//
  var i: natLte 2
  val () = for
    (i := 0; i < 2; i := i+1) let
    extern fun __close (fd: int): void = "atslib_close_exn"
    val fd = p_fds->[i]
  in
    if fd >= 0 then __close (fd)
  end // end of [val]
//
  val () = if (nerr > 0) then prerrf ("test_readany: failed.\n", @())
//
in
  // nothing
end // end of [test_readany2]

(* ****** ****** *)

implement
main () = () where {
  val () = test_readany2 ()
} // end of [main]

(* ****** ****** *)

(* end of [AUP_4_2_3.dats] *)
