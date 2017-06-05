//
// K&R, 2nd edition, page 111
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

staload "prelude/DATS/matrix.dats"

#define NMONTH 12
#define NMONTH1 NMONTH + 1
val daytab =
  matrix_make_arrsz {int} (2, 13, $arrsz (
  0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
, 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
)) // end of [val]

fn isleap (year: int): natLt 2 =
  if year mod 4 = 0 then
    if year mod 100 = 0 then
      (if year mod 400 = 0 then 1 else 0)
    else 1 // end of [if]
  else 0
// end of [isleap]

extern fun month_day
  (year: int, yday: int, month: &int? >> int, day: &int? >> int): void

implement month_day (year, yday, month, day) = let
  var i: intGt 0 // uninitialized
  var year: int = year and yday: int = yday
  val leap = isleap (year)
  val () = for (i := 1; i < NMONTH; i := i + 1) let
    val mday = daytab[leap, NMONTH1, i]
  in
    if yday <= mday then break else (yday := yday - mday)
  end // end of [val]
in
  month := i ; day := yday ;
end // end of [month_day]

(* ****** ****** *)

implement main () = let
  var m: int and d: int
  val () = month_day (1988, 60, m, d) 
in
  printf ("m(2) = %i and d(29) = %i\n", @(m, d))
end // end of [main]

(* ****** ****** *)

(* end of [month_day.dats] *)
