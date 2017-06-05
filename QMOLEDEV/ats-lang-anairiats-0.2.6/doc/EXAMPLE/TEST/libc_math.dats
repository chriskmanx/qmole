(*
** some testing code for functions declared in
** libc/SATS/math.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2009
//

(* ****** ****** *)

staload "libc/SATS/math.sats"

(* ****** ****** *)

local #include "libc/HATS/math.hats" in (*empty*) end

(* ****** ****** *)

implement main
  (argc, argv) = let
//
val DEG = M_PI / 180.0
//
  val sin30 = sintmp (30 * DEG)
  val () = println! ("sin(30) = ", sin30)
  val cos30 = costmp (30 * DEG)
  val () = println! ("cos(30) = ", cos30)
  val tan30 = tantmp (30 * DEG)
  val () = println! ("tan(30) = ", tan30)
//
  val sin60 = sintmp (60 * DEG)
  val () = println! ("sin(60) = ", sin60)
  val cos60 = costmp (60 * DEG)
  val () = println! ("cos(60) = ", cos60)
  val tan60 = tantmp (60 * DEG)
  val () = println! ("tan(60) = ", tan60)
//
  val () = println! ("tan(30) * tan(60) = ", tan30 * tan60)
//
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [libc_math.dats] *)
