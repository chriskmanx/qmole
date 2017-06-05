//
// A simple example for illustrating some benefits of dependent types
//

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Spring, 2009
//

(* ****** ****** *)
//
// How to compile:
//   atscc -o revstr revstr.dats
// How to test:
//   ./revstr <string>
//
(* ****** ****** *)

extern fun revstr {m,n:nat} {l:addr}
  (pf: !strbuf (m, n) @ l | p: ptr l):<> void
// end of [revstr]

implement
revstr {m,n} {l} (pf | p) =
  loop (pf | p, 0, n) where {
  val n = strbuf_length (!p)
  fun loop {i,j:nat | i <= n; i + j == n} .<n-i>.
    (pf: !strbuf (m, n) @ l | p: ptr l, i: size_t i, j: size_t j)
    :<> void =
    if i < j then let
      val j1 = j-1
      val ci = strbuf_get_char_at (!p, i)
      val cj = strbuf_get_char_at (!p, j1)
      val () = strbuf_set_char_at (!p, j1, ci)
      val () = strbuf_set_char_at (!p, i, cj)
    in
      loop (pf | p, i+1, j-1)
    end // end of [if]
} // end of [revstr]

(* ****** ****** *)

implement
main (argc, argv) = let
  val () = assert (argc >= 2)
  val str = argv.[1]
  val str = string1_of_string (str)
  val () = begin
    print "str = "; print str; print_newline ()
  end // end of [val]
  val (pfbox | p) = strbuf_of_string1 (str)
  val () = let
    prval vbox pf = pfbox in revstr (pf | p)
  end // end of [val]
  val () = begin
    print "str = "; print str; print_newline ()
  end // end of [val]
in
  // empty
end // end of [main]  

(* ****** ****** *)

(* end of [revstr.dats] *)
