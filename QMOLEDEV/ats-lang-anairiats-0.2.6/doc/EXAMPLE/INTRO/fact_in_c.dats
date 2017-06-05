//
//
// This example shows how C code can be included in ATS programs
// Author: Hongwei Xi (August 2007)
//
//

(* ****** ****** *)
//
// How to compile:
//   atscc -o fact_in_c -O3 fact_in_c.dats
// How to test:
//   ./fact_in_c
//
(* ****** ****** *)

%{
ats_int_type
fact_in_c (ats_int_type n) {
  int res = 1;
  while (n > 0) res *= n-- ;
  return res ;
}
%}

extern fun fact {n:nat} (n: int n): Nat = "fact_in_c"

// [fn] declares a non-recursive function
// [@(...)] is used in ATS to group arguments for functions of variable arguments
fn fact_usage (cmd: string): void =
  prerrf ("Usage: %s [integer]\n", @(cmd)) // print an error message

implement main (argc, argv) = begin
  if argc >= 2 then let
    val n = int1_of argv.[1] // turning string into integer
    val () = assert_errmsg
      (n >= 0, "The integer argument needs to be nonnegative.\n")
    val res = fact n
  in
    printf ("factorial of %i = %i\n", @(n, res))
  end else begin
    fact_usage (argv.[0]); exit (1)
  end
end // end of [main]

(* ****** ****** *)

(* end of [fact_in_c.dats] *)
