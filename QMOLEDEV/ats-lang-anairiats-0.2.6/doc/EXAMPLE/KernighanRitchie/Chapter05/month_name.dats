//
// K&R, 2nd edition, page 113
//

// Translated to ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)

staload "prelude/DATS/array.dats"

#define NMONTH 12
extern fun month_name (nmonth: intBtw (1, NMONTH+1)): string

local

val month_name_arr = array_make_arrsz {string} $arrsz(
  "January"
, "February"
, "March"
, "April"
, "May"
, "June"
, "July"
, "August"
, "September"
, "October"
, "November"
, "December"
) // end of [val]

in

implement month_name (nmonth) = month_name_arr[nmonth-1]

end // end of [local]

(* ****** ****** *)

implement main () = let
  var i: intGt 0 // uninitialized
  val () = for (i := 1; i <= NMONTH; i := i + 1) begin
    printf ("month_name[%i] = %s\n", @(i, month_name (i))) ;
  end // end of [val]
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [month_name.dats] *)
