(*
** some testing code for functions declared in
** libc/SATS/gmp.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Author: Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
// Time: August, 2010
//

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/pointer.dats"

(* ****** ****** *)

staload "libc/SATS/gmp.sats"

(* ****** ****** *)

fun mpz_test
  (): void = () where {
  macdef PI = 31415927U
  macdef SQRT2 = 141421UL
//
  var x: mpz_vt
  val () = mpz_init (x)
  val () = mpz_set (x, PI)
  val () = assert (mpz_sgn (x) = 1)
  val () = assert (mpz_get_uint (x) = PI)
//
  val str = mpz_get_str (10, x)
  val () = (print "str = "; print str; print_newline ())
  val () = strptr_free (str)
//
  val () = mpz_set_str_exn (x, "31415927", 10)
  val () = assert (mpz_cmp (x, PI) = 0)
//
  val () = mpz_neg (x)
  val () = assert (mpz_sgn (x) = ~1)
  val () = assert (mpz_cmpabs (x, PI) = 0)
  val () = mpz_clear (x)
//
  val (pfx_gc, pfx | px) = ptr_alloc<mpz_vt> ()
  val () = mpz_init2 (!px, 64UL); val () = mpz_set (!px, SQRT2)
  val () = assert (mpz_get_ulint (!px) = SQRT2)
  val () = mpz_clear (!px)
  val () = ptr_free {mpz_vt} (pfx_gc, pfx | px)
//
  var fac10: mpz_vt
  val () = mpz_init (fac10)
  val () = mpz_fac_ui (fac10, 10UL)
  val () = (print "fac10 = "; print fac10; print_newline ())
  val () = mpz_clear (fac10)
//
  var fib10: mpz_vt
  val () = mpz_init (fib10)
  val () = mpz_fib_ui (fib10, 10UL)
  var fib11: mpz_vt and fib12: mpz_vt
  val () = mpz_init (fib11) and () = mpz_init (fib12)
  val () = mpz_fib2_ui (fib11, fib12, 12UL)
  val () = mpz_swap (fib11, fib12)
  val () = assert (mpz_get_int fib10 + mpz_get_int fib11 = mpz_get_int fib12)
//
  var nprime: mpz_vt
  val () = mpz_init (nprime)
  val () = mpz_nextprime2 (nprime, fib12)
  val () = (print "nprime = "; print nprime; print_newline ())
//
  val nbit = mpz_size (nprime)
  val () = (print "nbit = "; print nbit; print_newline ())
  val ndigit = mpz_sizeinbase (nprime, 10)
  val () = (print "ndigit = "; print ndigit; print_newline ())
//
  val () = mpz_clear (fib10)
  val () = mpz_clear (fib11) and () = mpz_clear (fib12)
  val () = mpz_clear (nprime)
//
  var x: mpz_vt; val () = mpz_init_set (x, 100UL)
  var f: mpz_vt; val () = mpz_init_set (f, 2UL)
  val () = mpz_remove (x, f) // removing from x every factor of 2 
  val () = (print "x(25) = "; print x; print_newline ())
  val () = mpz_clear (x)
  val () = mpz_clear (f)
//
} // end of [mpz_test]

(* ****** ****** *)
//
// e = 1/0! + 1/1! + 1/2! + 1/3! + ...
//
fn compec1
  (N: ulint): double = e where {
  var res: mpq_vt; val () = mpq_init (res)
  var nfac: mpz_vt; val () = mpz_init (nfac)
  var n: ulint
  var nfacinv: mpq_vt; val () = mpq_init (nfacinv)
  val () = mpq_set_ui (nfacinv, 1UL, 1UL)
  val () = for
    (n := 0UL; n < N; n := n + 1UL) let
    val () = mpz_fac_ui (nfac, n)
    val () = mpq_set_den (nfacinv, nfac)
    val () = mpq_add (res, nfacinv)
  in
    // nothing
  end // end of [val]
  val e = mpq_get_d (res)
  val () = mpq_clear (res)
  val () = mpz_clear (nfac)
  val () = mpq_clear (nfacinv)
} // end of [compec1]

(* ****** ****** *)
//
// e = 1/lim_{n->oo} (1-1/n)^n
//
fn compec2
  (n: ulint): double = e where {
  var x: mpq_vt; val () = mpq_init (x)
  val () = mpq_set_ui (x, 1UL, 1UL)
  val () = mpq_decby (x, 1UL, n)
  val () = mpq_pow_ui (x, n)
  val () = mpq_inv (x)
  val e = mpq_get_d (x)
  val () = mpq_clear (x)
} // end of [compec2]

(* ****** ****** *)

fun mpq_test
  (): void = () where {
  var x: mpq_vt and y: mpq_vt
  val () = mpq_init (x) and () = mpq_init (y)
  val () = mpq_set_si (x, 6L, 23UL)
  val () = mpq_inv (y, x)
  val () = mpq_add (x, y)
  val () = mpq_sub (x, y)
//
  val (pf_num, fpf_num | p_num) = mpq_numref (x)
  val () = (print "num = "; print (!p_num); print_newline ())
  prval () = fpf_num (pf_num)
  val (pf_den, fpf_den | p_den) = mpq_denref (x)
  val () = (print "den = "; print (!p_den); print_newline ())
  prval () = fpf_den (pf_den)
  val () = (print "x(num/den) = "; print x; print_newline ())
  val str = mpq_get_str (10, x)
  val () = (print "x(num/den) = "; print str; print_newline ())
  val () = strptr_free (str)
//
  val () = mpq_set (x, 3.1415926535898)
  val () = (print "x(num/den) = "; print x; print_newline ())
  val () = printf ("x = %10.8f\n", @(mpq_get_d x))
//
  val () = mpq_clear (x) and () = mpq_clear (y)
//
  val e = compec1 (16UL) // quite accurate
  val () = printf ("Euler's constant (e) = %.10f\n", @(e))
//
  val e = compec2 (1024UL) // still not quite accurate
  val () = printf ("Euler's constant (e) = %.10f\n", @(e))
//
} // end of [mpq_test]

(* ****** ****** *)

fun mpf_test
  () : void = () where {
//
  #define fPI 3.1415926535898
//
  var x : mpf_vt and y : mpf_vt
  var z : mpf_vt
//
  val () = mpf_set_default_prec (128UL)
  val () = mpf_init (x)
  val () = assert (mpf_get_prec (x) = 128UL)
  val () = mpf_set_prec (x, 128UL)
//
  val () = mpf_set_str_exn (x, "3.1415926535898", 10)
  val fx = mpf_get_d (x)
  // val () = assert (fx <> fPI) // HX: yes, they are different
  val () = assert (abs (fx - fPI) < 1E-8)
//
  var state: gmp_randstate_vt
  val () = gmp_randinit_default (state)
  val () = mpf_urandomb (x, state, 100UL)
  val () = (print "x = "; print_mpf (x, 16); print_newline ())
  val () = gmp_randclear (state)
//
  var a: mpz_vt
  val () = mpz_init_set (a, 141421)
  val () = gmp_randinit_lc_2exp (state, a, 29UL(*c*), 5UL(*m2exp*))
  val () = mpf_urandomb (x, state, 100UL)
  val () = (print "x = "; print_mpf (x, 16); print_newline ())
  val () = mpz_clear (a)
  val () = gmp_randclear (state)
//
  val _err = gmp_randinit_lc_2exp_size (state, 100UL)
  val () = mpf_urandomb (x, state, 100UL)
  val () = (print "x = "; print_mpf (x, 16); print_newline ())
  val () = gmp_randclear (state)
//
  val () = mpf_random2 (x, (mp_size_t)100, (mp_exp_t)10)
  val () = (print "x = "; print_mpf (x, 16); print_newline ())
  val () = mpf_sqrt (x)
  val () = mpf_pow_ui (x, 2UL)
  val () = (print "x = "; print_mpf (x, 16); print_newline ())
  var exp: mp_exp_t
  val str = mpf_get_str (exp, 10, 16, x)
  val () = (print "str = "; print str; print_newline ())
  val () = strptr_free (str)
  val () = mpf_random2 (x, (mp_size_t)~100, (mp_exp_t)10)
  val () = (print "x = "; print_mpf (x, 16); print_newline ())
  val str = mpf_get_str (exp, 10, 00, x)
  val () = (print "str = "; print str; print_newline ())
  val () = strptr_free (str)
//
  val () = mpf_set_d (x, fPI)
  val () = assert (mpf_sgn (x) = 1)
  val () = assert (mpf_get_d (x) = fPI)
//
  val () = mpf_neg (x)
  val () = assert (mpf_sgn (x) = ~1)
  val () = assert (mpf_get_d (x) = ~fPI)
  val () = mpf_abs (x)
  val () = assert (mpf_get_d (x) = fPI)
//
  val () = mpf_init2 (y, 256UL)
  val () = mpf_set_d (y, 4*fPI)
  val () = assert (mpf_cmp_si (y, 12L) > 0)
  val () = assert (mpf_cmp_ui (y, 13UL) < 0)
  val () = assert (mpf_cmp_d (y, 12.56) > 0)
  val () = assert (mpf_get_d (y) = 4*fPI)
//
  val () = mpf_init_set (z, y)
  val () = mpf_add (z, 123456789UL)
  val () = mpf_sub (z, 123456789UL)
  val () = assert (mpf_get_d (y) = mpf_get_d (z))
  val () = mpf_ui_sub2 (z, 123456789UL)
  val () = mpf_ui_sub2 (z, 123456789UL)
  val () = assert (mpf_get_d (y) = mpf_get_d (z))
//
  val () = mpf_add (z, x, y)
  val () = mpf_mul (x, z, y)
  val () = mpf_div (z, x, y)
  val () = mpf_sqrt (y, x)
(*
  val () = printf (
    "x = %g, y = %g, z = %g\n", @(mpf_get_d (x), mpf_get_d (y), mpf_get_d (z))
  ) // end of [val]
*)
  val () = mpf_clear (x)
  val () = mpf_clear (y)
  val () = mpf_clear (z)
} // end of [mpf_test]

(* ****** ****** *)

implement
main () = () where {
//
  val () = mpz_test ()
//
  val () = mpq_test ()
//
  val () = mpf_test ()
//
  val () = print "[libc_gmp.dats] testing passes!\n"
} // end of [main]

(* ****** ****** *)

(* end of [libc_gmp.dats] *)
