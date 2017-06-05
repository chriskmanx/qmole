(*
** some testing code for functions declared in
** prelude/SATS/float.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: August, 2010
//

(* ****** ****** *)

implement
main () = let
//
  val () = assert (float_of (123456789) = float_of "123456789")
  val () = assert (double_of (123456789) = double_of "123456789")
//
  val () = assert (12345.0f * 6789 = float_of (12345 * 6789))
  val () = assert (12345.0f * 6789 <= float_of (12345 * 6789))
  val () = assert (12345.0f * 6789 >= float_of (12345 * 6789))
//
  val () = assert (12345.0 * 6789 = double_of (12345 * 6789))
  val () = assert (12345.0 * 6789 <= double_of (12345 * 6789))
  val () = assert (12345.0 * 6789 >= double_of (12345 * 6789))
//
  val x = 12345.6789 and y = 23456.7891
  val () = assert (max (x, y) + min (x, y) = x + y)
  val () = assert (max (x, y) - min (x, y) = abs (x - y))
  val () = assert (max (x, y) * min (x, y) = x * y)
//
in
  print "[prelude_float.dats] testing passes!\n"
end // end of [main]

(* ****** ****** *)

(* end of [prelude_float.dats] *)
