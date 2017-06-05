//
// K&R, 2nd edition, page 15
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

#define LOWER 0
#define UPPER 300
#define STEP 20

(*

#define LOWER 0
#define UPPER 300
#define STEP 20

main () {
  int fahr;
  for (fahr = LOWER; fahr <= UPPER; fahr += STEP) {
    printf ("%3d %6.1f\n", fahr, (5.0/9.0) * (fahr - 32)) ;
  } // end of [for]

} /* end of [main] */

*)

implement main () = loop (LOWER) where {
  fun loop (fahr: int): void =
    if fahr <= UPPER then let
      val () = printf ("%3d %6.1f\n", @(fahr, (5.0/9.0) * double_of (fahr - 32)))
    in
      loop (fahr + STEP)
    end // end of [loop]
} // end of [main]

(*

// here is a variant
implement main () = let
  var fahr: int = LOWER
in
  while (fahr <= UPPER) begin
    printf ("%3d %6.1f\n", @(fahr, (5.0/9.0) * double_of (fahr - 32)));
    fahr := fahr + STEP
  end // end of [while]
end // end of [main]

*)

(*

// here is another variant
implement main () = let
  var fahr: int // uninitialized
in
  for (fahr := LOWER; fahr <= UPPER; fahr := fahr + STEP) begin
    printf ("%3d %6.1f\n", @(fahr, (5.0/9.0) * double_of (fahr - 32)));
  end // end of [for]
end // end of [main]

*)

(* ****** ****** *)

(* end of [fahrenheit_celsius.dats] *)
