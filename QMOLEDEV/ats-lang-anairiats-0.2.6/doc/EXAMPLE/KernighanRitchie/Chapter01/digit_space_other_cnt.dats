//
// K&R, 2nd edition, page 22
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

extern fun digit_val_get {c:char} (c: char c): intBtw (~1, 10)

implement digit_val_get (c) = begin
  if c >= '0' then (if c <= '9' then c - '0' else ~1) else ~1
end // end of [digit_val_get]

implement main () = let
  var nwhite: int = 0 and nother = 0
  var !p_ndigit = @[int][10](0) // array allocated on stack
  val () = loop (!p_ndigit, nwhite, nother) where {
    fun loop (ndigit: &(@[int][10]), nwhite: &int, nother: &int): void = let
      val c = getchar ()
    in
      if (c <> EOF) then let
        val c = char1_of_int (c)
        val i = digit_val_get (c)
        val () = case+ 0 of
          | _ when i >= 0 => ndigit.[i] := ndigit.[i] + 1
          | _ when c = ' ' => nwhite := nwhite + 1
          | _ when c = '\n' => nwhite := nwhite + 1
          | _ when c = '\t' => nwhite := nwhite + 1
          | _ => nother := nother + 1
      in
        loop (ndigit, nwhite, nother)
      end else begin
        // loop exits
      end // end of [if]
    end // end of [loop]
  }
  val () = print "digits ="
  val () = pr (!p_ndigit, 0) where {
    fun pr (A: &(@[int][10]), i: natLte 10): void =
      if i < 10 then (printf (" %d", @(A.[i])); pr (A, i+1)) else ()
  }
in
  printf (", white space = %d, other = %d\n", @(nwhite, nother))
end // end of [main]

(* ****** ****** *)

(* end of [digit_space_other_cnt.dats] *)
