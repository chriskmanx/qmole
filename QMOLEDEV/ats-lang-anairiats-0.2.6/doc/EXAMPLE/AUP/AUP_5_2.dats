//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 248 - 249
// section 5.2: Environment
//
(* ****** ****** *)

staload "libc/SATS/stdlib.sats" // for getenv

(* ****** ****** *)

(*
extern
fun getenv (name: string)
  : [l:addr] (strptr l -<lin,prf> void | strptr l) = "mac#atslib_getenv"
// end of [atslib_getenv]
*)

implement
main () = () where {
  val [l:addr] (fpf_x | x) = getenv ("LOGNAME")
  prval () = addr_is_gtez {l} ()
  val () = if strptr_is_null (x) then let
    val () = printf ("The variable LOGNAME not found\n", @())
  in
    // nothing
  end else let
    val () = printf (
      "The value of LOGNAME is \"%s\"\n", @(__cast x)
    ) where {
      extern castfn __cast {l:agz} (x: !strptr l):<> string
    } // end of [val]
  in
  end // end of [if]
  prval () = fpf_x (x)
} // end of [main]

(* ****** ****** *)

(* end of [AUP_5_2.dats] *)
