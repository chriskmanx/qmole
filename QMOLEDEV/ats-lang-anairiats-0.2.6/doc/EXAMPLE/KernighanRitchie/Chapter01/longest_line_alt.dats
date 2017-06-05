//
// K&R, 2nd edition, page 29
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(*
** Handling C strings (byte sequences ending with the null byte)
** in ATS is a constant challenge. This implementation employs byte
** arrays instead of C strings, and it is significantly cleaner than
** the implementation in [longest_line.dats], which uses C strings.
*)

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(****** ****** *)
 
#define MAXLINE 1000
typedef b0ytes (n: int) = @[byte?][n]

(* ****** ****** *)

fun getline {m:nat}
  (buf: &bytes m, m: int m): natLte m = loop (buf, m, 0) where {
  fun loop {i:nat | i <= m} .<m-i>.
    (buf: &bytes m, m: int m, i: int i): natLte m =
    if i < m then let
      val c = getchar ()
    in
      if c = EOF then i else let
        val c = byte_of_int (c); val () = buf.[i] := c
      in
        if (c = byte_of_char '\n') then (i+1) else loop (buf, m, i+1)
      end // end of [if]
    end else begin
      m // loop exits
    end // end of [if]
} // end of [getline]

(* ****** ****** *)

fn copy {m1,m2:int} {n:nat | n <= m1; n <= m2} (
    buf_dst: &bytes m1, buf_src: &bytes m2, n: int n
  ) : void = loop (buf_dst, buf_src, n, 0) where {
  fun loop {i:nat | i <= n} .<n-i>.
    (buf_dst: &bytes m1, buf_src: &bytes m2, n: int n, i: int i): void =
    if i < n then begin
      buf_dst.[i] := buf_src.[i]; loop (buf_dst, buf_src, n, i+1)
    end // end of [if]
} // end of [copy]

(* ****** ****** *)

(*
** It is formally verified in the type system of ATS that there is
** *no* possibility of buffer overlow in this implementation. Period.
*)


implement main () = let
  #define M MAXLINE
  var !p_line with pf_line = @[byte][M]() // allocation on stack
  val () = pf_line := bytes_v_of_b0ytes_v (pf_line)
  var !p_longest with pf_longest = @[byte][M]() // allocation on stack
  val () = pf_longest := bytes_v_of_b0ytes_v (pf_longest)
  val max = loop (!p_line, !p_longest, 0) where {
    fun loop {max:nat | max <= M}
      (line: &bytes M, longest: &bytes M, max: int max)
      : natLte M = let
      val n = getline (line, M)
    in
      if n > 0 then begin
        if max < n then begin
          copy (longest, line, n); loop (line, longest, n)
        end else begin
          loop (line, longest, max)
        end // end of [if]
      end else begin
        max // loop exits
      end // end of [if]
    end // end of [loop]
  }
  val (pf_stdout | p_stdout) = stdout_get ()
  val () = if (max > 0) then let
    val max_sz = size1_of_int1 max
  in
    fwrite_byte_exn (file_mode_lte_w_w | !p_longest, max_sz, !p_stdout)
  end // end of [val]
  val () = stdout_view_set (pf_stdout | (*none*))
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [longest_line_alt.dats] *)
