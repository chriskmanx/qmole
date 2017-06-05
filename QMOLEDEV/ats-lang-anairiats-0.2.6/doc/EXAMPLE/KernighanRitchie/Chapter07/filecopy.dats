//
// K&R, 2nd edition, pages 163
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload UNSAFE = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

extern fun filecopy {m1,m2:file_mode} (
    pf1: file_mode_lte (m1, r)
  , pf2: file_mode_lte (m2, w)
  | ifp: &FILE m1, ofp: &FILE m2
  ) : void

implement filecopy {m1,m2} 
  (pf1, pf2 | ifp, ofp) = loop (ifp, ofp) where {
  fun loop (ifp: &FILE m1, ofp: &FILE m2): void = let
    val c = fgetc_err (pf1 | ifp)
  in
    if (c >= 0) then begin // c <> EOF
      let val _ = fputc_err (pf2 | char_of_int1 c, ofp) in loop (ifp, ofp) end
    end // end of [if]
  end // end of [loop]
} // end of [filecopy]

(* ****** ****** *)

implement main {n} (argc, argv) = let
  val () = case+ argc of
  | 1 => let
      val (pf_stdin | p_stdin) = stdin_get ()
      val (pf_stdout | p_stdout) = stdout_get ()
      val () = filecopy
        (file_mode_lte_r_r, file_mode_lte_w_w | !p_stdin, !p_stdout)
      val () = stdout_view_set (pf_stdout | (*none*))
      val () = stdin_view_set (pf_stdin | (*none*))
    in
      // empty
    end // end of [1]
  | _ (*argc >= 2*) => loop (argc, argv, 1) where {
      fun loop {i:nat | i <= n}
        (argc: int n, argv: &(@[string][n]), i: int i): void =
        if i < argc then let
          val name = argv.[i]
          val (pfopt | p_ifp) = fopen_err (name, file_mode_r)
        in
          if p_ifp > null then let
            prval Some_v (pf) = pfopt
            val (pf_stdout | p_stdout) = stdout_get ()
            val () = filecopy
              (file_mode_lte_r_r, file_mode_lte_w_w | !p_ifp, !p_stdout)
            val () = stdout_view_set (pf_stdout | (*none*))
            val () = fclose_exn (pf | p_ifp)
          in
            loop (argc, argv, i+1)
          end else let
            prval None_v () = pfopt
            val () = prerrf ("%s: can't open [%s]\n", @(argv.[0], name))
          in
            exit {void} (1)
          end // end of [if]
        end // end of [if]
    } // end of [_]
  // end of [val]
  val (pf_stdout | p_stdout) = stdout_get ()
  val err = ferror (!p_stdout)
  val () = stdout_view_set (pf_stdout | (*none*))
in
  if (err <> 0) then begin
    prerrf ("%s: error writing stdout\n", @(argv.[0])); exit {void} (2)
  end else begin
    exit {void} (0) // exit normally
  end // end of [if]
end // end of [main]

(* ****** ****** *)

(* end of [filecopy.dats] *)

