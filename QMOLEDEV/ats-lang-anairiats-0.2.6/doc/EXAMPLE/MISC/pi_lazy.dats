//
//
// using Euler's transform to compute the constant pi 
//
// author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//
//

(* ****** ****** *)

staload "prelude/DATS/lazy.dats"

(* ****** ****** *)

#define nil stream_nil
#define cons stream_cons
#define :: stream_cons

(* ****** ****** *)

stadef strcon = stream_con

// pi/4 = 1/1 - 1/3 + 1/5 - 1/7 + ...

fun pi_stream_con
  (c: double, sum: double, n: Pos):<!laz> strcon double =
  sum :: pi_stream (~c, sum + (c / double_of n), n+2)

and pi_stream (c: double, sum: double, n: Pos):<!laz> stream double =
  $delay (pi_stream_con (c, sum, n))

fun euler_trans_con
  (xs0: stream double):<!laz> strcon double = let
  val- x0 :: xs1 = !xs0
  val- x1 :: xs2 = !xs1
  val- x2 :: xs3 = !xs2
  val x01 = x0 - x1 and x21 = x2 - x1
in
  (x2 - x21 * x21 / (x21 + x01)) :: euler_trans xs1
end // end of [euler_trans_con]

and euler_trans (xs0: stream double):<!laz> stream double =
  $delay (euler_trans_con xs0)

fn pi_compute {n:nat} (n: int n): double = let
  fun loop {i:nat| i <= n}
    (n: int n, i: int i, xs: stream double): double =
    if i < n then loop (n, i+1, euler_trans xs) else stream_nth (xs, 0)
in
  loop (n, 0, pi_stream (4.0, 0.0, 1))
end // end of [pi_compute]

(* ****** ****** *)

implement main (argc, argv) = let
  val pi = pi_compute (8) // pi_compute (10) gives nan
in
  printf ("pi = %.13f\n", @(pi)) ;
end // end of [main]

(* ****** ****** *)

(* end of [pi_lazy.dats] *)
