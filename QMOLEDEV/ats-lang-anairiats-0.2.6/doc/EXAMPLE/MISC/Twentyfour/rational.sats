(*
//
// interface for some operations on rational numbers
//
// author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//
*)

(* ****** ****** *)
//
// assume int_t: t@ype
// assume rat_t: t@ype
// the header file for rational
//
(* ****** ****** *)

abst@ype int_t = int
abst@ype rat_t = @{ numer= int_t, denom= int_t }

(* ****** ****** *)
//
// HX: for testing, please do not change 'val' to 'fun'
//
(* ****** ****** *)

val int_make_int0 : int -<> int_t

val rat_make_int_int : (int_t, int_t) -<!exn> rat_t
val rat_make_int : int_t -<> rat_t

val rat_zero : rat_t and rat_one : rat_t

val rat_numerator: rat_t -<> int_t
val rat_denominator : rat_t -<> int_t

(* ****** ****** *)

exception DivisionByZeroException of ()

(* ****** ****** *)

val abs_rat : rat_t -<> rat_t
overload abs with abs_rat

val neg_rat : rat_t -<> rat_t
overload ~ with neg_rat

val add_rat_rat : (rat_t, rat_t) -<> rat_t
overload + with add_rat_rat

val sub_rat_rat : (rat_t, rat_t) -<> rat_t
overload - with sub_rat_rat

val mul_rat_rat : (rat_t, rat_t) -<> rat_t
overload * with mul_rat_rat

val div_rat_rat : (rat_t, rat_t) -<!exn> rat_t
overload / with div_rat_rat

val recip_rat : rat_t -<!exn> rat_t

(* ****** ****** *)

val square_rat: rat_t -<> rat_t
val cube_rat: rat_t -<> rat_t
val power_rat: (rat_t, Nat) -<> rat_t

(* ****** ****** *)

val is_zero : rat_t -<> bool
val is_not_zero : rat_t -<> bool

val is_negative : rat_t -<> bool
val is_not_negative : rat_t -<> bool

val is_positive : rat_t -<> bool
val is_not_positive : rat_t -<> bool

(* ****** ****** *)

val lt_rat_rat : (rat_t, rat_t) -<> bool
val lte_rat_rat : (rat_t, rat_t) -<> bool
val gt_rat_rat : (rat_t, rat_t) -<> bool
val gte_rat_rat : (rat_t, rat_t) -<> bool

overload < with lt_rat_rat
overload <= with lte_rat_rat
overload > with gt_rat_rat
overload >= with gte_rat_rat

val eq_rat_rat : (rat_t, rat_t) -<> bool
val neq_rat_rat : (rat_t, rat_t) -<> bool

overload = with eq_rat_rat
overload <> with neq_rat_rat

val compare_rat_rat: (rat_t, rat_t) -<> Sgn
overload compare with compare_rat_rat

val max_rat_rat : (rat_t, rat_t) -<> rat_t
and min_rat_rat : (rat_t, rat_t) -<> rat_t

overload max with max_rat_rat
overload min with min_rat_rat

(* ****** ****** *)

val tostring_rat : rat_t -<> string
overload tostring with tostring_rat

val fprint_rat: fprint_t0ype_type rat_t
overload fprint with fprint_rat

val print_rat : rat_t -<!exnref> void
val prerr_rat : rat_t -<!exnref> void

overload print with print_rat
overload prerr with prerr_rat

(* ****** ****** *)

(* end of [rational.sats] *)
