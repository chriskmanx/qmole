//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 371 - 375
// section 6.3: dup and dup2 System Calls
//
(* ****** ****** *)

staload "libc/sys/SATS/types.sats"
staload "libc/sys/SATS/wait.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/stdlib.sats" // EXIT_FAILURE
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

staload "utils/errinfo.sats"

(* ****** ****** *)

fun errinfo_report_wloc
  (loc: string): void = let
  var ei: errinfo_t
  val () = errinfo_set_wloc (ei, loc)
  val () = fprint_errinfo (stderr_ref, ei)
  val () = errinfo_clear (ei)
in
  // nothing
end // end of [errinfo_report]

(* ****** ****** *)

fun who2wc (): void = let
//
  exception ERROR of (int)
  macdef errptexit (status) = let
    val () = errinfo_report_wloc (#LOCATION) in $raise ERROR (,(status))
  end // end of [errptrexit]
//
fun who2wc_main (): void = let
  var fd1: int and fd2: int
  val (pfopt | err) = pipe (fd1, fd2)
(*
  val () = (print "who2wc: fd1 = "; print fd1; print_newline ())
  val () = (print "who2wc: fd2 = "; print fd2; print_newline ())
*)
in
//
// HX-2010-10-09:
// excessive error-checking makes the code difficult to understand!
//
if err = 0 then let
  prval Some_v @(pf1, pf2) = pfopt
  val pid1 = fork_err ()
  val ipid = int_of_pid (pid1)
  val () = (case+ 0 of
    | _ when ipid > 0 => let
        val pid2 = fork_err ()
        val ipid = int_of_pid (pid2)
        val () = (case+ 0 of
          | _ when ipid > 0 => let
              val () = close_exn (pf1 | fd1)
              val () = close_exn (pf2 | fd2)
              var status: int?
            in 
              if int_of_pid(waitpid (pid2, status, WNONE)) < 0 then errptexit (EXIT_FAILURE)
            end // end of [pid > 0]
          | _ when ipid = 0 => let // child 2
              prval () = STDIN_FILENO_gtez ()
              val (pf1_ | ()) = stdin_fildes_view_get ()
              val [i:int] (pfopt | err) = dup2 (pf1, pf1_ | fd1, STDIN_FILENO)
              val () = assertloc (err >= 0)
              prval Some_v (pf1_) = pfopt
              val () = stdin_fildes_view_set (pf1_ | (*none*))
              val () = (if (err < 0) then errptexit (EXIT_FAILURE) else ()): void
              val () = close_exn (pf1 | fd1)
              val () = close_exn (pf2 | fd2)
              val _ = execlp ("wc", "wc", "-l", null) where {
                extern fun execlp
                  (_: string, _: string, _: string, _: ptr null): int = "mac#atslib_execlp"
                // end of [execl]
              } // end of [val]
              val () = errptexit (EXIT_FAILURE)
            in
              // nothing
            end // end of [pid = 0]
          | _ (*ipid=-1*) => let
              val () = close_exn (pf1 | fd1)
              val () = close_exn (pf2 | fd2)
            in
              errptexit (EXIT_FAILURE)
            end // end of [_] 
        ) : void // end of [val]
        var status: int?
      in
        if int_of_pid(waitpid (pid1, status, WNONE)) < 0 then errptexit (EXIT_FAILURE)
      end // end of [pid > 0]
  | _ when ipid = 0 => let // child 1
      prval () = STDOUT_FILENO_gtez ()
      val (pf2_ | ()) = stdout_fildes_view_get ()
      val [i:int] (pfopt | err) = dup2 (pf2, pf2_ | fd2, STDOUT_FILENO)
      val () = assertloc (err >= 0)
      prval Some_v (pf2_) = pfopt
      val () = stdout_fildes_view_set (pf2_ | (*none*))
      val () = (if (err < 0) then errptexit (EXIT_FAILURE) else ()): void
      val () = close_exn (pf1 | fd1)
      val () = close_exn (pf2 | fd2)
      val _ = execlp ("who", "who", null) where {
        extern fun execlp
          (_: string, _: string, _: ptr null): int = "mac#atslib_execlp"
        } // end of [val]
      val () = errptexit (EXIT_FAILURE)
    in
      // nothing
    end // end of [pid = 0]
  | _ (*ipid=-1*) => let
      val () = close_exn (pf1 | fd1)
      val () = close_exn (pf2 | fd2)
    in
      errptexit (EXIT_FAILURE)
    end // end of [_] 
  ) : void // end of [val]
in
  // nothing
end else let
  prval None_v () = pfopt in errptexit (EXIT_FAILURE)
end // end of [if]
//
end // end of [who2wc_main]
//
in
//
try who2wc_main () with ~ERROR (status) => exit (status)
//
end // end of [who2wc]

(* ****** ****** *)

implement
main () = () where {
  val () = who2wc ()
  val () = printf ("who2wc is finished.\n", @())
} // end of [main]

(* ****** ****** *)

(* end of [AUP_6_3_1.dats] *)
