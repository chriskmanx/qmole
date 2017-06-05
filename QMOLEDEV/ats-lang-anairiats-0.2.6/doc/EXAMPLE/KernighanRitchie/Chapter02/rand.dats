//
// K&R, 2nd edition, page 46
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

(*

unsigned long int next = 1 ;

/* rand: return pseudo-randon integer on 0..32767 */
int rand (void) {
  next = next * 1103515245L + 12345 ;
  return (unsigned int)(next / 65536) % 32768 ;
} /* end of [rand] */

int srand (unsigned int seed) { next = seed ; return ; }

*)

(* ****** ****** *)

staload TIME = "libc/SATS/time.sats"

(* ****** ****** *)

extern fun rand (): int
extern fun srand (seed: ulint): void

(* ****** ****** *)

local

var next: ulint = 1UL // HX: avoid heap-allocation
val next = ref_make_view_ptr {ulint} (view@ (next) | &next)

in

implement rand () = let
  val n = !next * 1103515245UL + 12345UL
  val () = !next := n
in
  uint_of_ulint (n / 65536UL) uimod 32768
end // end of [rand]

implement srand (seed) = !next := seed

end // end of [local]

(* ****** ****** *)

implement main () = let
  val t = $TIME.time_get ()
  val t = $TIME.lint_of_time t
  val () = srand (ulint_of_lint t)
  val r0 = rand ()
  val () = (print "r0 = "; print r0; print_newline ())
  val r1 = rand ()
  val () = (print "r1 = "; print r1; print_newline ())
  val r2 = rand ()
  val () = (print "r2 = "; print r2; print_newline ())
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [rand.dats] *)
