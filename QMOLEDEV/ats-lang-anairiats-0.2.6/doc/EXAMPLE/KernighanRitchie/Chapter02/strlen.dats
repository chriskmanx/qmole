//
// K&R, 2nd edition, page 39
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

(*
int strlen (char s[]) {
  int i ;
  i = 0 ;
  while (s[i] != '\0') ++i ;
  return i ;
} /* end of [strlen] */
*)

extern
fun strlen {m,n:nat} (buf: &strbuf (m, n)):<> size_t n

implement strlen {m,n} (buf) = loop (buf, 0) where {
  fun loop {i:nat | i <= n} .<n-i>.
    (buf: &strbuf (m, n), i: size_t i):<> size_t n =
    if strbuf_is_at_end (buf, i) then i else loop (buf, i+1)
} // end of [strlen]

(* ****** ****** *)

implement
main (argc, argv) = let
  val () = assertloc (argc >= 2)
  val str = string1_of_string (argv.[1])
  val len = strlen (!p_buf) where {
    val (vbox pf_buf | p_buf) = strbuf_of_string1 str
  } // end of [val]
in
  print "str = "; print str; print " and len = "; print len; print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [strlen.dats] *)
