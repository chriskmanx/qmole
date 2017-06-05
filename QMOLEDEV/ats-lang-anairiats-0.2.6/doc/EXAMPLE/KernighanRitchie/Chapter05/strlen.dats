//
// K&R, 2nd edition, page 103
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

/*

int strlen (char *s) {
  char *p = s ;
  while (*p != '\000') ++p ;
  return (p - s) ;
}

*/

(*

// this is the version in Chapter2:
implement strlen {m,n} (s) = loop (s, 0) where {
  fun loop {i:nat | i <= n} .<n-i>.
    (s: &strbuf (m, n), i: int i):<> int n =
    if strbuf_is_at_end (s, i) then i else loop (s, i+1)
  // end of [loop]
} // end of [strlen]

*)

(* ****** ****** *)

extern fun strlen {m,n:nat} (s: &strbuf (m, n)):<> size_t n

(* ****** ****** *)

#define NUL '\000'

implement strlen {m,n} (s) = let
  stadef bsz = sizeof(byte)
  macdef bsz = sizeof<byte>
  fun loop {m,n:nat} {l:addr} {ofs:int} .<m>. (
      pf: !strbuf (m, n) @ l
    , pf_mul: MUL (n, bsz, ofs)
    | p: ptr l
    ) :<> ptr (l + ofs) = let
    prval (pf1, pf2) = strbuf_v_uncons (pf)
    val c = !p
  in
    if (c = NUL) then let
      prval strbufopt_v_none pf2 = pf2
      prval MULbas () = pf_mul
    in
      pf := strbuf_v_null (pf1, pf2); p
    end else let
      prval () = eqsize_byte_char ()
      prval strbufopt_v_some pf2 = pf2
      prval pf1_mul = mul_add_const {~1} (pf_mul)
      val p_end = loop (pf2, pf1_mul | p+bsz)
    in
      pf := strbuf_v_cons (pf1, pf2); p_end
    end // end of [if]
  end // end of [loop]
  val p_beg = &s
  prval pf_mul = mul_istot {n,bsz} ()
  val p_end = loop (view@ s, pf_mul | p_beg)
  prval () = eqsize_byte_one () where {
    extern praxi eqsize_byte_one (): [sizeof byte == 1] void
  } // end of [val]
  prval () = mul_elim {n,1} (pf_mul)
in
  size1_of_ptrdiff1 (p_end - p_beg)
end // end of [strlen]

(* ****** ****** *)

implement main (argc, argv) = let
  val () = assert (argc >= 2)
  val str = argv.[1]
  val int = strlen (!p_buf) where {
    val str = string1_of_string str
    val (vbox pf_buf | p_buf) = strbuf_of_string1 str
  } // end of [val]
in
  printf ("strlen (%s) = ", @(str)); print_size int; print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [strlen.dats] *)
