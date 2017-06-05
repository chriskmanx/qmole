//
// A naive implementation of the factorial function (2)
// Author: Hongwei Xi (August 2007)
//

(* ****** ****** *)
//
// How to compile:
//   atscc -o fact2 fact2.dats
// How to test:
//   ./fact2
//
(* ****** ****** *)

typedef Nat = [n:int | n >= 0] int n // type for natural numbers
fun fact2 {n:nat} (x: int n): Nat = if x > 0 then x nmul fact2 (x-1) else 1

// [fn] declares a non-recursive function
// [@(...)] is used in ATS to group arguments for functions of variable arguments
fn fact2_usage (cmd: string): void =
  prerrf ("Usage: %s [integer]\n", @(cmd)) // print an error message

(* ****** ****** *)

implement main (argc, argv) =
  if argc >= 2 then let
    val n = int1_of argv.[1] // turning string into integer
    val () = assert_errmsg_bool1
      (n >= 0, "The integer argument needs to be nonnegative.\n")
    val res = fact2 (n)
  in
    printf ("factorial of %i = %i\n", @(n, res))
  end else begin
    fact2_usage (argv.[0]); exit (1)
  end // end of [if]
// end of [main]

(* ****** ****** *)

(* end of [fact2.dats] *)
