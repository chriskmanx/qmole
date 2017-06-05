(*
**
** Given a line segment BC. Find the locus of the point A such that there
** exists a unique point D for A satisfying: 1) BC bisects AD and 2) ABCD
** are cyclic

** Assume |BC| = K + K

** r = sqrt (K^2 + t^2)
** x^2 + y^2 = r^2; y = r - t

** This leads to:

** 4(x^2+y^2)y^2 = (y^2 + k^2)^2
**
*)

(* ****** ****** *)

// Author: Hongwei Xi (* hwxi AT cs DOT bu DOT edu *)
// 28 December 2008:

(* ****** ****** *)

staload "libc/SATS/math.sats"

(*

#define K 20.0
#define K2 K * K

fn printrow (y: double): void = let
  val y2 = y * y
  val y2K2 = (y2 + K2)
  val rhs = y2K2 * y2K2
  val x2 = rhs / (4.0 * y2) - y2
  val x = sqrt (x2)
  var i: double // uninitialized
in
  for (i := 0.0; i <= x; i := i + 1.0) print '*';
  print '\n'
end // end of [printrow]

fn printrows (Y: double): void = let
  var y: double // uninitialized
in
  for (y := Y; 2.0 <= y; y := y - 1.0) printrow y;
  print_newline ()
end // end of [printrows]

implement main (argc, argv) = printrows (K)

*)

(* ****** ****** *)

#define K 20.0
#define K2 K * K

fn printbyte
  (X: double, x: double, i: double): void = let
  var b: byte = byte_of_int 0
  var j: double
  val () = for (j := 0.0; j < 8.0; j := j + 1.0) let
    val ij = i + j
    val () = if ij >= X then break
    val () = b := b << 1
  in
    if ij < x then (b := b + byte_of_int 1)
  end // end of [val]
in
  print b
end // end of [printbyte]

fn printrow (X: double, y: double): void = let
  val y2 = y * y
  val y2K2 = (y2 + K2)
  val rhs = y2K2 * y2K2
  val x2 = rhs / (4.0 * y2) - y2
  val x = sqrt (x2)
  var i: double // uninitialized
in
  for (i := 0.0; i < X; i := i + 8.0) printbyte (X, x, i);
end // end of [printrow]

fn printrows (X:double, Y: double): void = let
  var y: double // uninitialized
in
  for (y := Y; y >= 1.0; y := y - 1.0) printrow (X, y);
end // end of [printrows]

implement main (argc, argv) = let
  val I =
    if! (argc >= 2, int_of_string argv.[1], 0): int
  // end of [val]
  val k = (if I > 0 then double_of_int I else K): double
  val X = 8 * k and Y = k
  val () = begin
    printf ("P4\n%i %i\n", @(int_of X, int_of Y));
    printrows (X, Y);
  end // end of [val]
in
  // empty
end // end of [main]

(* end of [curve.dats] *)
