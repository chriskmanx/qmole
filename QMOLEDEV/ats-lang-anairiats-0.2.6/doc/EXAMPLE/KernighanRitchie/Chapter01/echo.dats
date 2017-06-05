//
// K&R, 2nd edition, page 17
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

staload "libc/SATS/stdio.sats"

(*

/* copy input to output */
int main () {
  int c ;
  while ((c = getchar()) != EOF) putchar(c) ;
}

*)

implement main () = loop () where {
  fun loop () = let
    val c = getchar ()
  in
    if (c <> EOF) then begin
      let val _ = putchar (char_of_int c) in loop () end
    end // end of [if]
  end // end of [loop]
} // end of [main]

(* ****** ****** *)

(* end of [echo.dats] *)
