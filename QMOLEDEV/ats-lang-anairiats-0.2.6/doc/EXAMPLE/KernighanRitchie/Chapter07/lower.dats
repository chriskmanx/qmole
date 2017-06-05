//
// K&R, 2nd edition, pages 153
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

implement main () = let
  var c: int?
in
  while (true) let
    val () = c := getchar1 ()
  in
    if c >= 0 then let // c <> EOF
      val _ = putchar (char_tolower (char_of_int1 c))
    in
      // empty
    end else begin
      break ;
    end // end of [if]
  end // end of [while]
end // end of [main]

(* ****** ****** *)

(* end of [lower.dats] *)
