//
// K&R, 2nd edition, page 64
//

(*

/*
** itoa: convert [n] to characters
*/
void itoa (int n, char s[]) {
  int i, sgn ;
  if ((sgn = n) < 0) n = -n ;
  i = 0 ;
  do { /* generate digits in reverse order */
    s[i++] = n % 10 + '0' ;
  } while ((n /= 10) > 0) ;
  if (sgn < 0) s[i++] = '-' ;
  s[i] = '\0' ;
  reverse (s) ;
  return ;
} /* end of [iota] */

*)

(* ****** ****** *)

dataview
itoa_v (bsz:int, l:addr, int) =
  | itoa_v_succ (bsz, l, 0) of strbuf bsz @ l
  | itoa_v_fail (bsz, l, ~1) of b0ytes bsz @ l
// end of [itoa_v]

// [itoa_err] reports an error if the buffer is not long enough
extern fun itoa_err {bsz:pos} {l:addr}
  (pf_buf: b0ytes bsz @ l | n: int, p_buf: ptr l, bsz: int bsz)
  :<> [p:int] (itoa_v (bsz, l, p) | int p)
// end of [itoa_err]

(* ****** ****** *)

typedef chars (n:int) = @[char][n]

(* ****** ****** *)

%{^
ats_char_type
char_of_digit (ats_int_type i) { return (i + '0') ; }
%}
extern fun char_of_digit (d: natLt 10):<> char = "char_of_digit"

(* ****** ****** *)

%{^
ats_void_type
strbuf_reverse (ats_ptr_type s0) {
  char *s = (char*)s0 ; int c, i, j ;
  for (i = 0, j = strlen(s) - 1; i < j; i++, j--) {
    c = s[i]; s[i] = s[j]; s[j] = c;
  }
  return ;
} /* end of [strbuf_reverse] */
%}
extern
fun strbuf_reverse {m,n:nat | n < m}
  (s: &strbuf (m, n)):<> void = "strbuf_reverse"
// end of [strbuf_reverse]

(* ****** ****** *)
  
implement
itoa_err {bsz} {l}
  (pf_buf | n, p_buf, bsz) = let
  fun loop
    (n: Nat, buf: &chars bsz, bsz: int bsz, i: &Nat): void =
    if i < bsz then begin
      if n > 0 then let
        val d = n nmod 10
        val () = buf.[i] := char_of_digit (d)
        val () = i := i + 1
      in
        loop (n / 10, buf, bsz, i)
      end else begin
        // loop exits normally
      end // end of [if]
    end // end of [if]
  // end of [loop]
  var i: Nat = 0
  prval pf1_buf = chars_v_of_b0ytes_v (pf_buf)
  val n = int1_of_int n
  val n_abs = (if n >= 0 then n else ~n): Nat
  val () = $effmask_all (loop (n_abs, !p_buf, bsz, i))
  val () = if i = 0 then (p_buf->[0] := '0'; i := 1)
  val () = if n < 0 then begin
    if i < bsz then (p_buf->[i] := '-'; i := i + 1)
  end // end of [val]
in
  if i < bsz then let
    prval pf_buf = bytes_v_of_chars_v (pf1_buf)
    val () = bytes_strbuf_trans (pf_buf | p_buf, size1_of_int1 i)
    val () = strbuf_reverse (!p_buf)
  in
    (itoa_v_succ (pf_buf) | 0)
  end else let
    prval pf_buf = bytes_v_of_chars_v (pf1_buf)
  in
    (itoa_v_fail (pf_buf) | ~1)
  end // end of [if]
end // end of [itoa_err]

(* ****** ****** *)

implement main (argc, argv) = let
  val () = assert (argc >= 2)
  val n = int_of_string (argv.[1])
  #define bsz 16
  var !p_buf with pf_buf = @[byte][bsz]()
  val (pf_itoa | err) = itoa_err (pf_buf | n, p_buf, bsz)
in
  if :(pf_buf: b0ytes bsz @ p_buf) => err >= 0 then let
    prval itoa_v_succ pf1_buf = pf_itoa
    val () = print (__cast p_buf) where {
      extern castfn __cast (p: ptr): string 
    }
    val () = print_newline ()
    prval () = pf_buf := bytes_v_of_strbuf_v (pf1_buf)
  in
    // empty
  end else let // err < 0
    prval itoa_v_fail pf1_buf = pf_itoa
    val () = (print "?"; print_newline ())
    prval () = pf_buf := pf1_buf
  in
    // empty
  end // end of [if]
end // end of [main]

(* ****** ****** *)

(* end of [itoa.dats] *)
