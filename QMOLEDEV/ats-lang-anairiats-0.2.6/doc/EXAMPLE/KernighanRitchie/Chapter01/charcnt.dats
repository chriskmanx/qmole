//
// K&R, 2nd edition, page 18
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

staload "libc/SATS/stdio.sats"

(*

#include <stdio.h>

int main () {
  double nc ;
  for (nc = 0; getchar () != EOF; nc++) ;
  printf ("%.0f\n", nc) ;
} /* end of main */

*)

implement main () = let
  val nc = loop (0.0) where {
    fun loop (nc: double): double =
      if getchar () <> EOF then loop (nc + 1.0) else nc
  } // end of [where]
in
  printf ("%.0f\n", @(nc))
end // end of [main]

(* ****** ****** *)

(* end of [charcnt.dats] *)
