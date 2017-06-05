//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 314 - 315
// section 5.10: Implementing a Shell (Version 3)
//
(* ****** ****** *)

staload "libc/sys/SATS/types.sats"
staload "libc/sys/SATS/wait.sats"
staload "libc/SATS/stdlib.sats" // for getenv
staload "libc/SATS/unistd.sats" // for environ_get_arrsz

(* ****** ****** *)

typedef ptrarr0 (n:int) = @[ptr?][n]

dataview
getargs_v (n0:int, l:addr, int) =
  | {n:nat | n < n0}
    getargs_v_succ (n0, l, n) of (
      ptrarr (n) @ l, ptrarr (n) @ l -<lin,prf> ptrarr0 (n0) @ l
    ) // end of [getargs_v_succ]
  | getargs_v_fail (n0, l, ~1) of (ptrarr0 (n0) @ l)
// end of [getargs_v]

extern
fun getargs {n0:nat} {l:addr} (
  pfargv: ptrarr0 (n0) @ l | pargv: ptr l, n0: int n0, iseof: &bool? >> bool
) : [n:int] (getargs_v (n0, l, n) | int n) = "mac#getargs"
// end of [getargs]

(* ****** ****** *)

fun printenv {n:pos} (
  argc: int n, argv: &(@[string][n])
) : void = let
  var m: size_t // uninitialized
  val (pf, fpf | p) = environ_get_arrsz (m)
  stavar m: int
  val m: size_t m = m
  var i: sizeLte m
  val _0 = size1_of_int1 (0)
  val () = for
    (i := _0; i < m; i := i+1) (printf("%s\n", @(!p.[i])))
  // end of [val]
  prval () = fpf (pf)
in
  // nothing
end // end of [printenv]

(* ****** ****** *)

fun assgnenv {n:pos}
  (argc: int n, argv: &(@[string][n])): void = let
  var nerr: int = 0
  val () = while (true) let
    val () = (
      if argc <= 2 then (nerr := 1; break; assertfalse())
    ) : [n >= 3] void
    val () = if (setenv (argv.[1], argv.[2], 1(*overwritten*)) < 0) then
      nerr := 2
    // end of [val]
  in
    break
  end // end of [val]
  val () = (case+ nerr of
    | 1 => printf ("Incorrect command format for <assgn>\n", @())
    | 2 => printf ("[setenv] failed\n", @())
    | _ => ()
  ) : void // end of [val]
in
  // nothing
end // end of [assgnenv]

(* ****** ****** *)

fun quit {n:pos}
  (argc: int n, argv: &(@[string][n])): void = exit(0)
// end of [quit]

(* ****** ****** *)

extern
fun display_status (pid: int, status: int): void
implement display_status (pid, status) = let
  val () = printf ("Process(%i) finished with the status(%i)\n", @(pid, status))
in
  // nothing
end // end of [display_status]

(* ****** ****** *)

extern
fun execute3 {n:pos} (argc: int n, argv: &ptrarr(n)): void
// end of [execute3]

implement
execute3
  (argc, argv) = let
  val pid = fork_err ()
  val pid = int_of_pid (pid)
in
  case+ 0 of
  | _ when pid = 0 => let // child
      prval (pf1, fpf1) = ptrarr_takeout{string} (view@(argv))
      val arg0 = argv.[0]
      prval () = view@(argv) := fpf1 (pf1)
      val _err = execvp (arg0, argv)
      val () = if _err < 0 then prerr "execute3: child: [execvp] failed\n"
    in
      _exit (EXIT_FAILURE)
    end // end of [_ when ...]
  | _ when pid > 0 => let // parent
      var status: int // uinitialized
      val _err = int_of_pid (wait (status))
      val () = if _err >= 0 then let
        val () = display_status (pid, status)
      in
        // nothing
      end else let
        val () = prerr "execute3: parent: [wait] failed\n" in (*none*)
      end // end of [if]
    in
      // nothing
    end // end of [_ when ...]
  | _ (*pid = -1*) => let
      val () = prerr "execute3: [fork] failed"
    in
      // nothing
    end // end of [_]
end // end of [execute3]

(* ****** ****** *)

implement
main () =
while (true) let
  #define MAXARG 32
  var !pargv with pfargv = @[ptr?][MAXARG]()
  val () = printf ("@ ", @())
  var iseof: bool // uninitialized
  val [n:int] (pfargs | argc) = getargs (pfargv | pargv, MAXARG, iseof)
//
  val () = if argc >= 0 then let
    var leftover: bool = false
    prval getargs_v_succ (pf, fpf) = pfargs
    prval (pf1, fpf1) = ptrarr_takeout{string} (pf)
    val () = if (argc > 0) then let
      val arg0 = pargv->[0] in case+ 0 of
      | _ when arg0 = "quit" => quit (argc, !pargv)
      | _ when arg0 = "print" => printenv (argc, !pargv)
      | _ when arg0 = "assgn" => assgnenv (argc, !pargv)
      | _ => leftover := true
    end // end of [val]
    prval () = pf := fpf1 (pf1)
    val () = if argc > 0 then
      if leftover then execute3 (argc, !pargv)
    // end of [val]
    prval () = pfargv := fpf (pf)
  in
    // nothing
  end else let
    prval getargs_v_fail (pf) = pfargs
    prval () = pfargv := pf
  in
    // nothing
  end // end of [val]
//
in
  if iseof then exit (EXIT_SUCCESS)
end // end of [main]

(* ****** ****** *)

(* end of [AUP_5_10.dats] *)
