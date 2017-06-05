//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 106 - 107
// section 2.13: lseek system call
//
(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"
staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

staload S = "libats/SATS/linstack_arr.sats"
stadef STACK = $S.STACK
stadef STACK0 = $S.STACK0

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"
staload _(*anon*) = "libats/DATS/linstack_arr.dats"
staload _(*anon*) = "libats/ngc/DATS/deque_arr.dats"

(* ****** ****** *)

extern fun backward (path: string): void

(* ****** ****** *)

#define BUFSZ 256
implement backward (path) = let
//
  typedef itm = char
  var S: STACK0 (itm)
//
  viewtypedef T (i:int) = STACK (itm, BUFSZ, i)
  viewtypedef T0 = [i:nat] T (i)
//
  fun print_stack {i:nat} .<i>.
    (S: &T i >> T 0): void = let
    val i = $S.stack_size (S)
  in
    if i > 0 then let
      val c = $S.stack_remove<itm> (S) in print c; print_stack (S)
    end // end of [if]
  end // end of [print_stack]
//
  val () = $S.stack_initialize<itm> (S, BUFSZ)
//
  val (pf_fd | fd) = open_flag_err (path, O_RDONLY)
//
  val () = assert_errmsg (fd >= 0, #LOCATION)
  prval open_v_succ (pf_fd) = pf_fd
//
  val _pos1 = ($T.off_of_lint)1L
  val off = lseek_err (pf_fd | fd, _pos1, $T.SEEK_END)
  val off = $T.lint_of_off(off)
  val () = assert_errmsg (off <> ~1L, #LOCATION)
//
  val _neg2 = ($T.off_of_lint)(~2L)
  val () = while*
    (S: T0) => (true) let
    var c: char
    val off = lseek_err (pf_fd | fd, _neg2, $T.SEEK_CUR)
    val off = $T.lint_of_off (off)
(*
    val () = (print "while: off = "; print off; print_newline ())
*)
    val () = assert_errmsg (off <> ~1L, #LOCATION)
    val n = read (fd, c, (size_of_int1)1) where {
      extern fun read (_:int, _: &char? >> char, _:size_t): size_t = "atslib_fildes_read_exn"
    } // end of [val]
(*
    val () = print c
*)
//
    val () = if :(S: T0) => (c = '\n') then print_stack (S)
    val nitm = $S.stack_size (S)
    val () = if :(S: T0) => (nitm < BUFSZ)
      then $S.stack_insert<itm> (S, c) else errno_set (E2BIG) // line too long
    // end of [val]
//
  in
    if off <= 0L then break
  end // end of [val]
//
  val () = print_stack (S)
  val () = $S.stack_uninitialize_vt {itm} (S)
//
  val () = close_exn (pf_fd | fd)
//
in
  // nothing
end // end of [backward]

(* ****** ****** *)

implement main
  (argc, argv) = () where {
  val () = assertloc (argc >= 2)
  val () = backward (argv.[1])
} // end of [val]

(* ****** ****** *)

(* end of [AUP_2_13.dats] *)
