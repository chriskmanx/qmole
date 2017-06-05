//
// K&R, 2nd edition, page 19
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

staload "libc/SATS/stdio.sats"

(*

#include <stdio.h>

int main () {
  int c ; int nl ;
  while ((c = getchar()) != EOF) if (c == '\n') ++nl ;
  printf ("%d\n", nl) ;
} /* end of [main] */

*)

implement main () = let
  val nl = loop (0) where {
    fun loop (nl: int): int = let
      val c = getchar ()
    in
      if  c <> EOF then begin
        if c = int_of_char '\n' then loop (nl + 1) else loop (nl)
      end else begin
        nl // loop exits
      end // end of [if]
    end // end of [loop]
  }
in
  printf ("%d\n", @(nl))
end // end of [main]

(* ****** ****** *)

(* end of [linecnt.dats] *)
