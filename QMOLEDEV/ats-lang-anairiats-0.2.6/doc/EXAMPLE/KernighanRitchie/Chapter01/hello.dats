//
// K&R, 2nd edition, page 7
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(*

#include <stdio.h>

main () {
  printf ("hello, world\n") ;
}

*)

implement main () = begin
  printf ("hello, world\n", @()); // semicolon is optional
end // end of [main]

(*

// here is a variant in ATS:
implement main () = begin
  printf ("%s, %s\n", @("hello", "world"));
end // end of [main]

*)

(* ****** ****** *)

(* end of [hello.dats] *)
