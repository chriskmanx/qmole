//
// K&R, 2nd edition, page 105
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

extern fun strcpy
  {m1:nat} {m2,n2:nat | n2 < m1} {l1:addr} (
    pf1: !b0ytes m1 @ l1 >> strbuf (m1, n2) @ l1
  | p1: ptr l1 , s2: &strbuf (m2, n2)
  ) :<> void

(* ****** ****** *)

(*

// the following is declared in [prelude/SATS/string.sats]

viewdef strbuf_v (m: int, n: int, l:addr) = strbuf (m, n) @ l

praxi strbuf_v_null {n:nat} {l:addr}
  (pf1: char NUL @ l, pf2: b0ytes (n) @ l + 1): strbuf_v (n+1, 0, l)

praxi strbuf_v_cons {c: char | c <> NUL} {m,n:nat} {l:addr}
  (pf1: char c @ l, pf2: strbuf_v (m, n, l+1)): strbuf_v (m+1, n+1, l)

dataview strbufopt_v (int, int, addr, char) =
  | {m:nat} {l:addr}
    strbufopt_v_none (m, ~1, l, NUL) of b0ytes m @ l
  | {m,n:nat} {l:addr} {c:char | c <> NUL}
    strbufopt_v_some (m, n, l, c) of strbuf_v (m, n, l)

praxi strbuf_v_uncons
  {m,n:nat | n < m} {l:addr} (pf: strbuf_v (m, n, l))
  :<prf> [c:char] @(char c @ l, strbufopt_v (m-1, n-1, l+1, c))

*)

(* ****** ****** *)

#define NUL '\000'

implement strcpy (pf1 | p1, s2) =
  loop (pf1, view@ s2 | p1, &s2) where {
  fun loop
    {m1:nat} {m2,n2:nat | n2 < m1} {l1,l2:addr} .<m1>. (
      pf1: !b0ytes m1 @ l1 >> strbuf_v (m1, n2, l1)
    , pf2: !strbuf_v (m2, n2, l2)
    | p1: ptr l1, p2: ptr l2
    ) :<> void = let
    prval (pf21, pf22) = strbuf_v_uncons (pf2)
    val c = !p2
    prval (pf11, pf12) = array_v_uncons {byte?} (pf1)
    prval () = pf11 := char_v_of_b0yte_v (pf11)
    val () = !p1 := c
  in
    if c = NUL then let
      prval () = eqsize_byte_char ()
      prval () = pf1 := strbuf_v_null (pf11, pf12)
      prval strbufopt_v_none (pf22) = pf22
      prval () = pf2 := strbuf_v_null (pf21, pf22)
    in
      // empty
    end else let
      prval () = eqsize_byte_char ()
      prval strbufopt_v_some (pf22) = pf22
      val () = loop (pf12, pf22 | p1+sizeof<byte>, p2+sizeof<byte>)
      prval () = pf1 := strbuf_v_cons (pf11, pf12)
      prval () = pf2 := strbuf_v_cons (pf21, pf22)
    in
      // empty
    end // end of [if]
  end // end of [loop]
} // end of [strcpy]

(* ****** ****** *)

implement main (argc, argv) = let
  val () = assert (argc >= 2)
  val str = string1_of_string (argv.[1])
  val len = string_length (str)
  var !p_buf_new with pf_buf_new = @[byte][len+1]()
  val () = let
    val (vbox pf_buf | p_buf) = strbuf_of_string1 str
  in
     strcpy (pf_buf_new | p_buf_new, !p_buf)
  end // end of [val]
  val () = printf ("strcpy (%s) = ", @(str))
  val () = print (__cast p_buf_new) where {
    extern castfn __cast (p: ptr): string 
  } // end of [val]
  val () = print_newline ()
  prval () = pf_buf_new := bytes_v_of_strbuf_v (pf_buf_new)
in
  // empty  
end // end of [main]

(* ****** ****** *)

(* end of [strcpy.dats] *)
