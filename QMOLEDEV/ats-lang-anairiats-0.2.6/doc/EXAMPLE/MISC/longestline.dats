(*
// print out the longest line in a file; this example demonstrates
// an interesting example of linear lazy evaluation
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: January, 2009
//

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

// staload "prelude/SATS/list_vt.sats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"

viewtypedef cstream_vt = stream_vt (char)

// tail-recursion; no persistent heap allocation
fn* longestline_loop1 {n:nat}
  (cs: cstream_vt, cur: list_vt (char, n), n: int n): List_vt (char) =
  case+ !cs of
  | ~stream_vt_cons (c, cs) => begin
      if (c <> '\n') then begin
        longestline_loop1 (cs, list_vt_cons (c, cur), n+1)
      end else begin
        longestline_loop2 (cs, list_vt_nil (), 0, cur, n)
      end // end of [if]
    end // end of [stream_vt_cons]
  | ~stream_vt_nil () => cur
// end of [longestline_loop1]

and longestline_loop2 {m,n:nat | n <= m} (
    cs: cstream_vt
  , cur: list_vt (char, n), n: int n
  , max: list_vt (char, m), m: int m
  ) : List_vt (char) = begin case+ !cs of
  | ~stream_vt_cons (c, cs) => begin
      if (c <> '\n') then let
        val cur = list_vt_cons (c, cur)
      in
        if (n < m) then
          longestline_loop2 (cs, cur, n + 1, max, m)
        else let // m = n
          val () = list_vt_free (max)
        in
          longestline_loop1 (cs, cur, n + 1)
        end // end of [if]
      end else let // c = '\n'
        val () = list_vt_free (cur)
      in
        longestline_loop2 (cs, list_vt_nil (), 0, max, m)
      end // end of [if]
    end // end of [stream_vt_cons]
  | ~stream_vt_nil () => let
      val () = list_vt_free (cur) in max
    end // end of [stream_vt_nil]
end // end of [longestline_loop2]

implement main () = let
  val (pf_stdin | p_stdin) = stdin_get ()
  val cs = char_stream_vt_make_file
    (file_mode_lte_r_r, pf_stdin | p_stdin)
  val longestline =
    longestline_loop1 {0} (cs, list_vt_nil (), 0)
  val longestline = list_vt_reverse (longestline)
  val () = loop (longestline) where {
    fun loop {n:nat} .<n>.
      (cs: list_vt (char, n)): void = case+ cs of
      | ~list_vt_cons (c, cs) => (print c; loop (cs))
      | ~list_vt_nil () => ()
    // end of [loop]
  }
  val () = print_newline ()
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [longestline.dats] *)
