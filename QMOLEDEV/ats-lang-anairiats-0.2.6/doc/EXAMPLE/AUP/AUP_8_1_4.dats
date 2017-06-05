//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 527 - 530
// section 8.1.4: Byte Order
//
(* ****** ****** *)

staload "libc/arpa/SATS/inet.sats"

(* ****** ****** *)

fun pr (b: byte) = printf ("%x", @(uint_of_byte(b)))

implement
main () = () where {
//
  var nhost = uint16_of_int (0xD04C)
  val (pf_x, fpf_x | x) = __cast (nhost) where {
    extern castfn __cast (n: &uint16)
      : [l:addr] (bytes(2) @ l, bytes(2) @ l -<lin,prf> void | ptr l)
    // end of [extern]
  } // end of [val]
  val () = (pr x->[0]; print ' '; pr x->[1]; print_newline ())
  prval () = fpf_x (pf_x)
//
  var nnetwork = htons (nhost)
  val (pf_x, fpf_x | x) = __cast (nnetwork) where {
    extern castfn __cast (n: &uint16_nbo)
      : [l:addr] (bytes(2) @ l, bytes(2) @ l -<lin,prf> void | ptr l)
    // end of [extern]
  } // end of [val]
  val () = (pr x->[0]; print ' '; pr x->[1]; print_newline ())
  prval () = fpf_x (pf_x)
//  
} // end of [main]

(* ****** ****** *)

(* end of [AUP_8_1_4.dats] *)