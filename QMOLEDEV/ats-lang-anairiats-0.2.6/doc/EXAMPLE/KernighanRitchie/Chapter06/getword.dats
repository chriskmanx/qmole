//
// K&R, 2nd edition, pages 136
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

extern fun getword {lim:int | lim >= 2} {l:addr}
  (pf: !b0ytes lim @ l >> strbuf lim @ l | word: ptr l, lim: int lim): void

(* ****** ****** *)

implement getword {lim} {l} (pf | word, lim) = let
  var c: int // uninitialized
  val () = loop (c) where {
    fun loop (c: &int? >> int): void = let
      val () = c := getchar1 ()
    in
      if (c >= 0) then begin
        if char_isspace (char_of_int1 c) then loop (c) else ()
      end // end of [if]
    end // end of [loop]
  } // end of [val]
  prval () = pf := bytes_v_of_b0ytes_v (pf)
  var i: natLt lim = 0
  val () = if c >= 0 then begin
    if char_isalpha (char_of_int c) then let
      prval (pf1, pf2) = array_v_uncons {byte} (pf)
      val () = (!word := byte_of_int c)
      val nLeft = loop
        (pf2 | word + sizeof<byte>, lim - 2) where {
        fun loop {n:nat} {l:addr} .<n>.
          (pf: !bytes (n+1) @ l | p: ptr l, n: int n): natLte n =
          if n = 0 then 0 (* loop exits *) else let
            val c = getchar1 ()
          in
            if c < 0 then n (* loop exits *) else begin
              if char_isalnum (char_of_int c) then let
                prval (pf1, pf2) = array_v_uncons {byte} (pf)
                val () = (!p := byte_of_int c)
                val nLeft = loop (pf2 | p + sizeof<byte>, n-1)
                val () = pf := array_v_cons {byte} (pf1, pf2)
              in
                nLeft
              end else begin
                n // loop exits
              end // end of [if]
            end // end of [if]
          end // end of [if]
      } // end of [val]
      prval () = pf := array_v_cons {byte} (pf1, pf2)
    in
      i := 1 + nLeft      
    end // end of [if]
  end // end of [val]
in
  bytes_strbuf_trans (pf | word, size1_of_int1 i)
end // end of [getword]

(* ****** ****** *)

#define BUFSZ 16

implement main (argc, argv) = let
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  val () = getword (pf_buf | p_buf, BUFSZ)
  val () = print_string (__cast p_buf) where {
    extern castfn __cast (p: ptr): string 
  } // end of [val]
  val () = print_newline ()
  prval () = pf_buf := bytes_v_of_strbuf_v (pf_buf)
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [getword.dats] *)
