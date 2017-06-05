(*

// Implementing some operations on rational numbers
//
// author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

*)

// require int_t: t@ype and rat_t: t@ype

(*
staload "rational.sats" with { int_t= int_t }
*)

staload "rational.sats"

assume int_t = int
assume rat_t = @{ numer= int_t, denom= int_t }

implement int_make_int0 (i) = i

implement rat_make_int n = @{ numer= n, denom= 1 }

implement rat_make_int_int (m, n) =
  if n > 0 then
    let val r = gcd (m, n) in @{ numer= m/r, denom= n/r } end
  else if n < 0 then
    let val r = gcd (m, n) in @{ numer= ~m/r, denom= ~n/r } end
  else begin
    $raise DivisionByZeroException ()
  end // end of [if]
// end of [rat_make_int_int]

implement rat_zero = @{ numer=0, denom=1 }
implement rat_one = @{ numer=1, denom=1 }

implement rat_numerator r = r.numer
implement rat_denominator r = r.denom

(* ****** ****** *)

implement add_rat_rat (r1, r2) = begin
  $effmask_exn (rat_make_int_int
    (r1.numer * r2.denom + r2.numer * r1.denom, r1.denom * r2.denom)
  ) // end of [$effmask_exn]
end // end of [add_rat_rat]

implement sub_rat_rat (r1, r2) = begin
  $effmask_exn (rat_make_int_int
    (r1.numer * r2.denom - r2.numer * r1.denom, r1.denom * r2.denom)
  ) // end of [$effmask_exn]
end // end of [sub_rat_rat]

implement mul_rat_rat (r1, r2) = begin
  $effmask_exn (rat_make_int_int (r1.numer * r2.numer, r1.denom * r2.denom))
end // end of [mul_rat_rat]

implement div_rat_rat (r1, r2) = begin
  rat_make_int_int (r1.numer * r2.denom, r1.denom * r2.numer)
end // end of [div_rat_rat]

implement recip_rat r = begin
  if r.numer <> 0 then @{ numer= r.denom, denom= r.numer }
  else $raise DivisionByZeroException
end // end of [recip_rat]

(* ****** ****** *)

implement square_rat r = @{
  numer = square r.numer, denom= square r.denom
}

implement power_rat (r0, n0) = let
  var r = r0 and n = n0 and res = rat_one
  val () = while*
    {n:nat} .<n>. (n:int n, r: rat_t, res: rat_t) => (true) (
    case+ n of
    | 0 => break
    | 1 => (res := mul_rat_rat (r, res); break)
    | _ =>> begin
        if n mod 2 > 0 then begin
          n := n / 2; res := mul_rat_rat (r, res);
          r := square_rat r; continue
        end else begin
          n := n / 2; r := square_rat r; continue
        end
      end
  ) // end of [while*]
in
  res
end // end of [power_rat]

(* ****** ****** *)

implement lt_rat_rat (r1, r2) =
  r1.numer * r2.denom < r2.numer * r1.denom

implement lte_rat_rat (r1, r2) =
  r1.numer * r2.denom <= r2.numer * r1.denom

implement gt_rat_rat (r1, r2) =
  r1.numer * r2.denom > r2.numer * r1.denom

implement gte_rat_rat (r1, r2) =
  r1.numer * r2.denom >= r2.numer * r1.denom

implement eq_rat_rat (r1, r2) =
  if r1.numer = r2.numer then r1.denom = r2.denom else false

implement neq_rat_rat (r1, r2) =
  if r1.numer <> r2.numer then true else r1.denom <> r2.denom

(* ****** ****** *)

implement compare_rat_rat (r1, r2) =
  compare (r1.numer * r2.denom, r2.numer * r1.denom)

implement max_rat_rat (r1, r2) =
  if r1.numer * r2.denom >= r2.numer * r1.denom then r1 else r2

implement min_rat_rat (r1, r2) =
  if r1.numer * r2.denom <= r2.numer * r1.denom then r1 else r2

(* ****** ****** *)

implement is_zero r = r.numer = 0
implement is_not_zero r = r.numer <> 0

implement is_negative r = r.numer < 0
implement is_not_negative r = r.numer >= 0

implement is_positive r = r.numer > 0
implement is_not_positive r = r.numer <= 0

(* ****** ****** *)

implement fprint_rat (pf_mode | out, r) =
  if r.denom > 1 then begin
    fprintf (pf_mode | out, "%i/%i", @(r.numer, r.denom))
  end else fprint (pf_mode | out, r.numer)

implement print_rat r = print_mac (fprint_rat, r)
implement prerr_rat r = prerr_mac (fprint_rat, r)

implement tostring_rat r =
  if r.denom <> 1 then
    string_of_strptr (tostringf ("%i/%i", @(r.numer, r.denom)))
  else tostring r.numer
// end of [tostring_rat]

(* ****** ****** *)

(* end of rational.dats *)
