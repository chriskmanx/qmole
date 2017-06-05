(*
**
** An interface for ATS to interact with BLAS
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

staload "contrib/clapack/SATS/f2c.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynloading at run-time

(* ****** ****** *)

implement
fprint_integer (out, i) = let
  val i = lint_of_integer i // no-op casting
in
  fprint_lint (out, i)
end // end of [fprint_integer]

implement print_integer (i) = fprint_integer (stdout_ref, i)
implement prerr_integer (i) = fprint_integer (stderr_ref, i)

(* ****** ****** *)

implement
fprint_uinteger (out, u) = let
  val u = ulint_of_uinteger u // no-op casting
in
  fprint_ulint (out, u)
end // end of [fprint_uinteger]

implement print_uinteger (u) = fprint_uinteger (stdout_ref, u)
implement prerr_uinteger (u) = fprint_uinteger (stderr_ref, u)

(* ****** ****** *)

(* end of [f2c.dats] *)
