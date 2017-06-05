//
// K&R, 2nd edition, page 87
//

(* ****** ****** *)

fn swap {n:nat} {i,j:nat | i < n; j < n}
  (v: &(@[int][n]), i: int i, j: int j):<> void = let
  val tmp = v.[i]
in
  v.[i] := v.[j]; v.[j] := tmp
end // end of [swap]

(* ****** ****** *)

fun qsort {n:nat} {left,right:int}
  {0 <= left; left <= right+1; right+1 <= n}
  .<right+1-left>. (
    v: &(@[int][n]), left: int left, right: int right
  ) :<> void =
  if (left >= right) then () else let
    val () = swap (v, left, (left + right) / 2)
    val piv = v.[left]; val left1 = left + 1
    val last = loop (v, left1, left1) where {
      fun loop {last,i:nat}
        {left < last; last <= i; i <= right+1}
        .<right+1-i>.
        (v: &(@[int][n]), last: int last, i: int i)
        :<cloref> intBtw (left, right+1) =
        if i <= right then (
          if v.[i] < piv then begin
            swap (v, last, i); loop (v,  last+1, i+1)
          end else begin
            loop (v, last, i+1)
          end // end of [if]
        ) else (
          last - 1 // loop exits
        ) // end of [if]
    } // end of [val]
  in
    swap (v, left, last); qsort (v, left, last-1); qsort (v, last+1, right)
  end // end of [if]
// end of [qsort]

(* ****** ****** *)

implement main () = let
  fn pr {n:nat}
    (v: &(@[int][n]), n: int n): void = loop (v, n, 0) where {
    fun loop {i:nat | i <= n} .<n-i>.
      (v: &(@[int][n]), n: int n, i: int i): void =
      if i < n then
        (if i > 0 then print ", "; print v.[i]; loop (v, n, i+1))
      // end of [if]
  }
  var !p_arr with pf_arr = @[int](8, 7, 1, 3, 9, 4, 2, 0, 6, 5)
  val () = (print "bef: "; pr (!p_arr, 10); print_newline ())
  val () = qsort (!p_arr, 0, 9)
  val () = (print "aft: "; pr (!p_arr, 10); print_newline ())
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [qsort.dats] *)
