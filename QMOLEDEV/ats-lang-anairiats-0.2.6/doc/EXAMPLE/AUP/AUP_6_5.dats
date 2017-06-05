//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 371 - 375
// section 6.5: Two-Way Communication with Unidirectional Pipes
//
(* ****** ****** *)

staload "libc/sys/SATS/types.sats"
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
//
// HX-2010-10-10:
// error-handling in the following code really caused my head to spin. However,
// I do not have a significantly better approach at this moment :(
//
(* ****** ****** *)

fun fsort () = let
//
  exception ERROR of (int)
  macdef errptexit (status) = let
    val () = errinfo_report_wloc (#LOCATION) in $raise ERROR (,(status))
  end // end of [errptrexit]
//
fun fsort_main (): void = let
  var fdin1: int and fdin2: int
  val [i:int] (pfopt | err) = pipe (fdin1, fdin2)
  val () = (
//
if (err < 0) then let
  prval None_v () = pfopt in errptexit (EXIT_FAILURE)
end else let
  prval Some_v @(pfin1, pfin2) = pfopt
  var fdout1: int and fdout2: int
  val [i:int] (pfopt | err) = pipe (fdout1, fdout2)
  val () = (
if (err < 0) then let
  val () = close_exn (pfin1 | fdin1)
  val () = close_exn (pfin2 | fdin2)
  prval None_v () = pfopt in errptexit (EXIT_FAILURE)
end else let
  prval Some_v @(pfout1, pfout2) = pfopt
  val () = fsort_main2 (pfin1, pfin2, pfout1, pfout2 | fdin1, fdin2, fdout1, fdout2)
in
  // nothing
end
  ) : void // end of [val]
//
in
  // nothing
end
  ) : void // end of [val]
//
in
  // nothing
end // end of [fsort_main]
//
and fsort_main2
  {fdin1,fdin2:int} {fdout1,fdout2:int} (
  pfin1: fildes_v fdin1, pfin2: fildes_v fdin2
, pfout1: fildes_v fdout1, pfout2: fildes_v fdout2
| fdin1: int fdin1, fdin2: int fdin2
, fdout1: int fdout1, fdout2: int fdout2
) : void = let
  prval () = STDIN_FILENO_gtez ()
  prval () = STDOUT_FILENO_gtez ()
  val pid = fork_err ()
  val ipid = (int_of_pid)pid
in
  case+ 0 of
  | _ when ipid = 0 => let // child
//
      val (pf_ | ()) = stdin_fildes_view_get ()
      val [i:int] (pfopt | err) = dup2 (pfout1, pf_ | fdout1, STDIN_FILENO)
      val () = assertloc (err >= 0)
      prval Some_v (pf_) = pfopt
      val () = stdin_fildes_view_set (pf_ | (*none*))
      val () = close_exn (pfout1 | fdout1)
      val () = close_exn (pfout2 | fdout2)
      val () = (if (err < 0) then errptexit (EXIT_FAILURE) else ()): void
//
      val (pf_ | ()) = stdout_fildes_view_get ()
      val [i:int] (pfopt | err) = dup2 (pfin2, pf_ | fdin2, STDOUT_FILENO)
      val () = assertloc (err >= 0)
      prval Some_v (pf_) = pfopt      
      val () = stdout_fildes_view_set (pf_ | (*none*))
      val () = close_exn (pfin1 | fdin1)
      val () = close_exn (pfin2 | fdin2)
      val () = (if (err < 0) then errptexit (EXIT_FAILURE) else ()): void
      val _int = execlp ("sort", "sort", null) where {
        extern fun execlp (_: string, _: string, _: ptr null): int = "mac#atslib_execlp"
      } // end of [val]
//
    in
      errptexit (EXIT_FAILURE) // HX: if execution reaches here, [execlp] has failed
    end // end of [ipid = 0]
  | _ when ipid > 0 => let // parent
      #define BUFSZ 512
      var !p_buf with pf_buf = @[byte][BUFSZ]()
      prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
      val () = close_exn (pfin2 | fdin2)
      val () = close_exn (pfout1 | fdout1)
      val (pfopt | fd) = open_flag_err ("data/fruits.txt", O_RDONLY)
      var nerr: int = 0
      val () = if (fd < 0) then let
        prval open_v_fail () = pfopt in nerr := nerr + 1
      end else let
        prval open_v_succ (pf) = pfopt
        val () = while (true) let
          val nread = read_err (pf | fd, !p_buf, BUFSZ)
        in
          if nread >= 0 then let
            val nread = size1_of_ssize1 (nread)
            val () = if nread = 0 then break
            val nwrite = write_err (pfout2 | fdout2, !p_buf, nread)
            val () = if nwrite < 0 then (nerr := nerr + 1; break)
          in
            // nothing
          end else let
            val () = nerr := nerr + 1 in break
          end // end of [if]
        end // end of [while]
        val () = close_exn (pf | fd)
      in
        // nothing
      end // end of [val]
      val () = close_exn (pfout2 | fdout2)
      val () = (
if (nerr > 0) then let
  val () = close_exn (pfin1 | fdin1)
in
  errptexit (EXIT_FAILURE) // HX: open ("datafile") failed
end else let
  var nerr: int = 0
  val (pf_stdout | ()) = stdout_fildes_view_get ()
  val () = while (true) let
    val nread = read_err (pfin1 | fdin1, !p_buf, BUFSZ)
  in
    if nread >= 0 then let
      val nread = size1_of_ssize1 (nread)
      val () = if nread = 0 then break
      val nwrite = write_err (pf_stdout | STDOUT_FILENO, !p_buf, nread)
    in
      if nwrite < 0 then (nerr := nerr + 1; break)
    end else let
      val () = nerr := nerr + 1 in break
    end // end of [if]
  end // end of [val]
  val () = stdout_fildes_view_set (pf_stdout | (*none*))
  val () = close_exn (pfin1 | fdin1)
  val () = if nerr > 0 then errptexit (EXIT_FAILURE)
in
  // nothing
end // end of [if]
      ) : void // end of [val]
    in
      // nothing
    end // end of [ipid > 0]
  | _ => let
      val () = close_exn (pfin1 | fdin1)
      val () = close_exn (pfin2 | fdin2)
      val () = close_exn (pfout1 | fdout1)
      val () = close_exn (pfout2 | fdout2)
    in
      errptexit (EXIT_FAILURE) // [fork] failed
    end // end of [_]
end // end of [fsort_main2]
in
  try fsort_main () with ~ERROR (status) => exit (status)
end // end of [fsort]

(* ****** ****** *)

implement main () = fsort ()

(* ****** ****** *)

(* end of [AUP_6_5.dats] *)
