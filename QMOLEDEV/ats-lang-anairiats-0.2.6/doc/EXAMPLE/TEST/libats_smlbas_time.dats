(*
** some testing code for functions declared in
** libats/smlbas/SATS/array.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

staload "libats/smlbas/SATS/time.sats"

(* ****** ****** *)

dynload "libats/smlbas/DATS/real.dats"
dynload "libats/smlbas/DATS/time.dats"

(* ****** ****** *)

implement main () = () where {
  val t0 = now ()
  val () = begin
    print "current time = "; fprint (stdout_ref, t0); print_newline ()
  end // end of [val]     
} // end of [main]

(* ****** ****** *)

(* end of [libats_smlbas_time.dats] *)
