(*
** some testing code for functions declared in
** prelude/SATS/bool.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: August, 2010
//

(* ****** ****** *)
//
// HX: testing the recently added support for boolean patterns
//
fn not1 {b:bool}
  (b: bool b):<> bool (~b) =
  case+ b of | true => false | false => true
// end of [not1]

(* ****** ****** *)

implement
main () = let
//
  val b1 = true and b2 = false
//
  val () = println! ("b1(true) = ", b1)
  val () = println! ("b2(false) = ", b2)
//
  val () = assert (not1 b1 = ~b1)
  val () = assert (not1 b2 = ~b2)
//
  val () = assert (~(b1 || b2) = (~b1 && ~b2))
  val () = assert (~(b1 && b2) = (~b1 || ~b2))
//
  val b1 = false and b2 = true
  val () = assert (~(b1 || b2) = (~b1 && ~b2))
  val () = assert (~(b1 && b2) = (~b1 || ~b2))
//
in
  print "[prelude_bool.dats] testing passes!\n"
end // end of [main]

(* ****** ****** *)

(* end of [prelude_bool.dats] *)
