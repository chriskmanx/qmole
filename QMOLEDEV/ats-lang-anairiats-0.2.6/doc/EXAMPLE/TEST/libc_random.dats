(*
** some testing code for functions declared in
** libc/SATS/stdio.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//

(* ****** ****** *)

staload "libc/SATS/random.sats"

(* ****** ****** *)

fun test_randint () = let
  #define _1M 1000000
  var cnt: int = 0
  var i: int // uninitialize
  val () = for
    (i := 0; i < _1M; i := i+1)
    if randint (2) = 0 then cnt := cnt + 1 else ()
  val ratio = (1.0 * cnt / _1M)
in
  printf ("test_randint: ratio = %.6f\n", @(ratio))
end // end of [test_randint]

(* ****** ****** *)

fun test_randint_r () = let
  #define _1M 1000000
  var cnt: int = 0
  var res: int?
  var buf: drand48_data
  val _0 = srand48_r (0L, buf)
  var i: int // uninitialize
  val () = for
    (i := 0; i < _1M; i := i+1) let
    val () = randint_r (buf, 2, res)
  in
    if res = 1 then cnt := cnt + 1 else ()
  end // end of [val]
  val ratio = (1.0 * cnt / _1M)
in
  printf ("test_randint_r: ratio = %.6f\n", @(ratio))
end // end of [test_randint_r]

(* ****** ****** *)

implement
main (argc, argv) = let
  val () = test_randint ()
  val () = test_randint_r ()
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [libc_stdio.dats] *)
