(*
** some testing code for functions declared in
** libc/SATS/printf.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//

(* ****** ****** *)

staload "libc/SATS/printf.sats"

(* ****** ****** *)

implement
main () = () where {
//
  val ntot = printf ("%s", @("abcdefghijklmnopqrstuvwxyz\n"))
  val () = assertloc (ntot = 26+1)
  val () = (print "ntot = "; print ntot; print_newline ())
//
  val (pfout | pout) = stdout_get ()
  val ntot = fprintf (file_mode_lte_w_w | !pout, "%s", @("ABCDEFGHIJKLMNOPQRSTUVWXYZ\n"))
  val () = assertloc (ntot = 26+1)
  val () = (print "ntot = "; print ntot; print_newline ())
  val () = stdout_view_set (pfout | (*none*))
//
} // end of [main]

(* ****** ****** *)

(* end of [libc_printf.dats] *)


