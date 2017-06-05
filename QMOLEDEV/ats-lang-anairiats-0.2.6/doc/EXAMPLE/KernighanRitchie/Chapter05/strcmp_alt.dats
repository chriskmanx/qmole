//
// K&R, 2nd edition, page 106
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

extern fun strcmp
  {m1,n1:nat} {m2,n2:nat} (
    s1: &strbuf (m1, n1), s2: &strbuf (m2, n2)
  ) :<> Sgn

(* ****** ****** *)

(*

//
// This is the pointer version of [strcmp]. Handling C-strings
// in such a style requires a great deal of effort being spent on
// proof manipulation. However, the generated C code from this
// implementation is just as efficient as a corresponding version
// manually written in C.
//

// the following is declared in [prelude/SATS/string.sats]

viewdef strbuf_v (m: int, n: int, l:addr) = strbuf (m, n) @ l

praxi strbuf_v_null {n:nat} {l:addr}
  (pf1: char NUL @ l, pf2: b0ytes (n) @ l + sizeof(byte))
  : strbuf_v (n+1, 0, l)

praxi strbuf_v_cons
  {c: char | c <> NUL} {m,n:nat} {l:addr}
  (pf1: char c @ l, pf2: strbuf_v (m, n, l + sizeof(byte)))
  :<prf> strbuf_v (m+1, n+1, l)

dataview strbufopt_v (int, int, addr, char) =
  | {m:nat} {l:addr}
    strbufopt_v_none (m, ~1, l, NUL) of b0ytes m @ l
  | {m,n:nat} {l:addr} {c:char | c <> NUL}
    strbufopt_v_some (m, n, l, c) of strbuf_v (m, n, l)

praxi strbuf_v_uncons
  {m,n:nat} {l:addr} (pf: strbuf_v (m, n, l))
  :<prf> [c:char] @(
     char c @ l, strbufopt_v (m-1, n-1, l + sizeof(byte), c)
   )

*)

(* ****** ****** *)

#define NUL '\000'

implement strcmp (s1, s2) =
  loop (view@ s1, view@ s2 | &s1, &s2) where {
  fun loop
    {m1,n1:nat} {m2,n2:nat}
    {l1,l2:addr} .<m1>. (
      pf1: !strbuf_v (m1, n1, l1)
    , pf2: !strbuf_v (m2, n2, l2)
    | p1: ptr l1, p2: ptr l2
    ) :<> Sgn = let
    prval (pf11, pf12) = strbuf_v_uncons (pf1)
    prval (pf21, pf22) = strbuf_v_uncons (pf2)
    val c1 = !p1 and c2 = !p2
  in
    if c1 = NUL then begin
      if c2 = NUL then let
        prval strbufopt_v_none (pf12) = pf12
        prval () = pf1 := strbuf_v_null (pf11, pf12)
        prval strbufopt_v_none (pf22) = pf22
        prval () = pf2 := strbuf_v_null (pf21, pf22)
      in
        0 // loop exits
      end else let
        prval strbufopt_v_none (pf12) = pf12
        prval () = pf1 := strbuf_v_null (pf11, pf12)
        prval strbufopt_v_some (pf22) = pf22
        prval () = pf2 := strbuf_v_cons (pf21, pf22)
      in
        ~1 // loop exits
      end // end of [if]
    end else begin // c1 <> NUL
      if c2 = NUL then let
        prval strbufopt_v_some (pf12) = pf12
        prval () = pf1 := strbuf_v_cons (pf11, pf12)
        prval strbufopt_v_none (pf22) = pf22
        prval () = pf2 := strbuf_v_null (pf21, pf22)
      in
        1 // loop exits
      end else let // c2 <> NUL
        prval strbufopt_v_some (pf12) = pf12
        prval strbufopt_v_some (pf22) = pf22
        val sgn = compare (c1, c2)
      in
        if sgn = 0 then let // c1 = c2
          val sgn = loop (pf12, pf22 | p1+sizeof<char>, p2+sizeof<char>)
          prval () = pf1 := strbuf_v_cons (pf11, pf12)
          prval () = pf2 := strbuf_v_cons (pf21, pf22)
        in
          sgn
        end else let // c1 <> c2
          prval () = pf1 := strbuf_v_cons (pf11, pf12)
          prval () = pf2 := strbuf_v_cons (pf21, pf22)
        in
          sgn // loop exits
        end (* end of [if] *)
      end // end of [if]
    end (* end of [if] *)
  end // end of [loop]
} (* end of [strcmp] *)

(* ****** ****** *)

implement main (argc, argv) = let
  val () = assert (argc >= 3)
  val str1 = string1_of_string (argv.[1])
  and str2 = string1_of_string (argv.[2])
  val sgn = let
    val (vbox pf1_buf | p1_buf) = strbuf_of_string1 str1 in
    $effmask_all let
      val (vbox pf2_buf | p2_buf) = strbuf_of_string1 str2 in
      strcmp (!p1_buf, !p2_buf)
    end // end of [let]
  end // end of [val]
in
  printf ("strcmp (%s, %s) = %i\n", @(str1, str2, sgn))
end // end of [main]

(* ****** ****** *)

(* end of [strcmp_alt.dats] *)
