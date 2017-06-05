//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 208 - 213
// section 4.2.2: Nonblocking Input
//
(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/time.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

fun setblock {fd:int} (
    fd: int fd, block: bool
  ) : bool = let
  val f = __getfl (fd) where {
    extern fun __getfl (fd: int): flag_t = "atslib_fcntl_getfl"
  }
  val i = int_of_uint(uint_of_flag (f))
in
  if i >= 0 then let
    val f = if block then (f land ~O_NONBLOCK) else (f lor O_NONBLOCK)
    val err = __setfl (fd, f) where {
      extern fun __setfl (fd: int, f: flag_t): int = "atslib_fcntl_setfl"
    } // end of [val]
  in
    if err >= 0 then true else false
  end else false
end // end of [setblock]

(* ****** ****** *)

fun msetblock {n:nat}
  (fds: &(@[int][n]), n: int n): int = let
  var nerr: int = 0
  var i: natLte n
  val () = for
    (i := 0; i < n; i := i+1) let
    val fd = fds.[i]
    val fd = int1_of (fd)
    val res = setblock (fd, false) // inefficient
  in
    if ~res then nerr := nerr + 1
  end // end of [val]
in
  nerr (* 0/neg : succ/fail *)
end // end of [msetblock]

%{^
#define atslib_read1(fd, c) read(fd, c, 1)
%} // end of [%{^]

fun readany {n:nat} (
    fds: &(@[int][n]), n: int n, which: &int
  ) : int = let
//
  extern fun read1 (fd: int, c: &char): ssize_t = "mac#atslib_read1"
//
  var nerr: int = 0
  var i: natLte n = 0
  var c:char = '\0'
  val () = while (true) let
    val () = case+ 0 of
      | _ when i < n => let
          val () = c := '\0'
          val nread = read1 (fds.[i], c)
          val nread = int_of_ssize (nread)
          val nread = int1_of_int (nread)
        in
          case+ 0 of
          | _ when nread >= 0 => (which := i; break)
          | _ (*nread = -1*) => (
              if (errno_get () = EAGAIN) then (i := i+1; continue) else (nerr := nerr+1; break)
            ) // end of [_]
        end // end of [_ when ...]
      | _ => let
          val _leftover = sleep (1) in i := 0
        end // end of [_]
    // end of [val]
  in
    // nothing
  end // end of [val]
in
  if nerr > 0 then ~1 else (int_of)c
end // end of [readany]

(* ****** ****** *)

fun test_readany () = let
  var !p_fds = @[int](~1, ~1)
  var nerr: int = 0
  extern prfun __leak {v:view} (pf: v): void
  val (pf | fd) = open_flag_err ("/dev/tty", O_RDWR)
  val () = if fd < 0 then let
    val () = prerr "test_readany: open: 0\n" in nerr := nerr + 1
  end // end of [val]
  prval () = __leak (pf)
  val () = p_fds->[0] := fd
  val path = "/dev/pts/3" // HX: change may be needed
  val (pf | fd) = open_flag_err (path, O_RDWR)
  val () = if fd < 0 then let
    val () = prerr "test_readany: open: 1\n" in nerr := nerr + 1
  end // end of [val]
  prval () = __leak (pf)
  val () = p_fds->[1] := fd  
//
  val _err = msetblock (!p_fds, 2)
//
  val () = if (nerr = 0) then let
    var which: int = ~1
    val () = while (true) let
      val c = readany (!p_fds, 2, which)
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
end // end of [test_readany]

(* ****** ****** *)

implement
main () = () where {
  val () = test_readany ()
} // end of [main]

(* ****** ****** *)

(* end of [AUP_4_2_2_2.dats] *)
