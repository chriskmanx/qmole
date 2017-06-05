//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

(* ****** ****** *)

// imperative style
fun tally1 (n: int): int = let
  var i: int // uninitialized
  var res: int = 0
in
  for (i := 1; i <= n; i := i + 1) res := res + i;
  res
end // end of [tally]

// functional style
fun tally2
  (n: Nat): int = loop (0, n, 1) where {
  fun loop (res: int, n: int, i: int): int =
    if i <= n then loop (res + i, n, i + 1) else res
} // end of [tally2]

implement main () = let
  val ans1 = tally1 (100)
  val ans2 = tally2 (100)
//
// HX: functions defined through fixed-point expressions
//
  val ans3 = tally3 (100) where {
    val tally3 = fix f(n: Nat): int => if n > 0 then n + f(n-1) else 0
  } // end of [val]
//
  val ans4 = !p_clo(100) where {
    var !p_clo = @fix f
      {n:nat} (n: int n): intGte(0) =<clo1> if n > 0 then n + f(n-1) else 0
  } // end of [val]
in
  printf ("tally(100) = %i\n", @(ans1));
  printf ("tally(100) = %i\n", @(ans2));
  printf ("tally(100) = %i\n", @(ans3));
  printf ("tally(100) = %i\n", @(ans4));
end // end of [main]

(* ****** ****** *)

(* end of [tally.dats] *)
