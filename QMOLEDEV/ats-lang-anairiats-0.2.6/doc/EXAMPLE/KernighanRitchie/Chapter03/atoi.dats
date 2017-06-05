//
// K&R, 2nd edition, page 61
//

(*

int atoi (char s[]) {
  int i, n, sgn ;
  n = 0 ;
  for (i = 0; isspace(s[i]); ++i) ; /* skip white space */
  sgn = (s[i] == '-') ? -1 : 1 ;
  if (s[i] == '+' || s[i] == '-') ++i ;
  for (i = 0; s[i] >= '0' && s[i] <= '9'; ++i)
    n = 10 * n + (s[i] - '0') ;
  return sgn * n ;
} /* end of [atoi] */

*)

(* ****** ****** *)

extern fun digit_val_get (c: char):<> intBtw (~1, 10)

implement digit_val_get (c) = let
  val c = char1_of_char (c)
in
  if c >= '0' then (if c <= '9' then c - '0' else ~1) else ~1
end // end of [digit_val_get]

(* ****** ****** *)

extern fun atoi {m,n:nat | n < m} (buf: &strbuf (m, n)):<> int

implement atoi {m,n} (buf) = let
  typedef buf_t = strbuf (m, n)
  var i: sizeLte n = loop (buf, 0) where { // skip white space
    fun loop {i:nat | i <= n} .<n-i>.
      (buf: &buf_t, i: size_t i):<> sizeLte n =
      if strbuf_is_at_end (buf, i) then i else begin
        if char_isspace buf[i] then loop (buf, i+1) else i
      end // end of [if]
    // end of [loop]
  } // end of [val]
in
  if strbuf_is_at_end (buf, i) then 0 else let
    val c = buf[i]
    val sgn = (if c = '-' then ~1 else 1): int; val () = begin
      if c = '+' then i := i + 1 else if c = '-' then i := i + 1 else ()
    end : void
    val n = loop (buf, i, 0) where {
      fun loop {i:nat | i <= n} .<n-i>.
        (buf: &buf_t, i: size_t i, res: int):<> int =
        if strbuf_is_at_end (buf, i) then res else let
          val d = digit_val_get buf[i]
        in
          if d >= 0 then loop (buf, i+1, 10 * res + d) else res
        end // end of [if]
      // end of [loop]
    } // end of [val]
  in
    sgn * n
  end // end of [if]
end // end of [atoi]

(* ****** ****** *)

implement main (argc, argv) = let
  val () = assert (argc >= 2)
  val str = string1_of_string (argv.[1])
  val int = atoi (!p_buf) where {
    val (vbox pf_buf | p_buf) = strbuf_of_string1 str
  } // end of [val]
in
  printf ("str = %s and int = %i\n", @(str, int))
end // end of [main]

(* ****** ****** *)

(* end of [atoi.dats] *)
