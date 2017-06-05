(*
** some testing code for functions declared in
** libc/SATS/complex.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2009
//

(* ****** ****** *)

staload "libc/SATS/complex.sats"

(* ****** ****** *)

implement main
  (argc, argv) = let
//
  val _1 = ccmplx_of(1)
  val _i = ccmplx_imag_unit
  val _1_i = _1 + _i
  val () = (print "_1_i = "; print _1_i; print_newline ())
  val _2_2i = _1_i + _1_i
  val () = (print "_2_2i = "; print _2_2i; print_newline ())
  val _0 = _1_i - _1_i
  val () = (print "_0 = "; print _0; print_newline ())
  val _2i = _1_i * _1_i
  val () = (print "_2i = "; print _2i; print_newline ())
  val _1 = _1_i / _1_i
  val () = (print "_1 = "; print _1; print_newline ())
//
  val x = abs (_1_i)
  val () = (print "|_1_i| = "; print x; print_newline ())
  val c = sqrt (_1_i)
  val () = (print "sqrt(_1_i) = "; print c; print_newline ())
  val _1_i = c * c
  val () = (print "_1_i = "; print _1_i; print_newline ())
  val _1_i = pow (c, 2.0f)
  val () = (print "_1_i = "; print _1_i; print_newline ())
//
  val _1 = zcmplx_of(1)
  val _i = zcmplx_imag_unit
  val _1_i = _1 + _i
  val () = (print "_1_i = "; print _1_i; print_newline ())
  val _2_2i = _1_i + _1_i
  val () = (print "_2_2i = "; print _2_2i; print_newline ())
  val _0 = _1_i - _1_i
  val () = (print "_0 = "; print _0; print_newline ())
  val _2i = _1_i * _1_i
  val () = (print "_2i = "; print _2i; print_newline ())
  val _1 = _1_i / _1_i
  val () = (print "_1 = "; print _1; print_newline ())
//
  val x = abs (_1_i)
  val () = (print "|_1_i| = "; print x; print_newline ())
  val z = sqrt (_1_i)
  val () = (print "sqrt(_1_i) = "; print z; print_newline ())
  val _1_i = z * z
  val () = (print "_1_i = "; print _1_i; print_newline ())
  val _1_i = pow (z, 2.0)
  val () = (print "_1_i = "; print _1_i; print_newline ())
//
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [libc_complex.dats] *)
