//
// K&R, 2nd edition, page 24
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

(* ****** ****** *)

(*

#include <stdio.h>

extern int power (int base, int n) ;

int main () {
  int i ;
  for (i = 0; i < 10; ++i) {
    printf ("%d %d %d\n", i, power(2, i), power(-3, i)) ;
  }
  return 0 ;
} /* end of [main] */

/* ****** ****** */

int power (int base, int n) {
  int i, p ;
  p = 1 ;
  for (i = 1; i <= n; ++i) p = p * base ;
  return p ;
}

*)

extern fun power (base: int, n: int): int

implement
power (base, n) = loop (1, 1) where {
  // note that [loop] is a closure
  fun loop (i: int, p: int):<cloref1> int =
    if i <= n then loop (i+1, p * base) else p
} // end of [power]

(* ****** ****** *)

implement main () = let
  var i: int // uninitialized
in
  for (i := 0; i < 10; i := i + 1)
    printf ("%d %d %d\n", @(i, power(2, i), power(~3, i)))
end // end of [main]

(* ****** ****** *)

(* end of [power.c] *)
