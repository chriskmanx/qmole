//
// A simple example for illustrating some benefits of dependent types
//

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Spring, 2009
//

(* ****** ****** *)
//
// How to compile:
//   atscc -o revarr revarr.dats
// How to test:
//   ./revarr
//
(* ****** ****** *)

(*

// this one does not use dependent types:

staload _(*anonymous*) = "prelude/DATS/array0.dats"

fun{a:t@ype} revarr
  (A: array0 a): void = loop (A, 0, n - 1) where {
  val n: size_t = array0_size A
  val n: int = int_of_size (n)
  fun loop  (A: array0 a, i: int, j: int): void =
    if i < j then let
      val tmp = A[i] in
      A[i] := A[j]; A[j] := tmp; loop (A, i + 1, j - 1)
    end // end of [if]
  // end of [loop]
} // end of [revarr]

implement main () = let
  fun pr (A: array0 int): void = loop (A, n, 0) where {
    val n = array0_size A; val n = int_of_size (n)
    fun loop (A: array0 int, n: int, i: int): void =
      if i < n then begin
        if i > 0 then print ", "; print A[i]; loop (A, n, i+1)
      end else begin
        // loop exits
      end // end of [if]
  } // end of [pr]
  val A = array0_make_arrsz $arrsz {int} (0, 1, 2, 3, 4, 5)
  val () = pr A
  val () = print_newline ()
  val () = revarr<int> (A)
  val () = pr A
  val () = print_newline ()
in
  // empty
end // end of [main]

*)

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"

fun{a:t@ype} revarr {n:nat}
  (A: array (a, n), n: int n): void = loop (A, 0, n - 1) where {
  fun loop  {i:nat;j:int | i <= j+1; i + j == n-1}
    (A: array (a, n), i: int i, j: int j): void =
    if i < j then let
      val tmp = A[i] in
      A[i] := A[j]; A[j] := tmp; loop (A, i + 1, j - 1)
    end // end of [if]
} // end of [revarr]

(* ****** ****** *)

implement main () = let
  fun pr {n:nat}
    (A: array (int, n), n: int n): void = loop (A, n, 0) where {
    fun loop {i:nat | i <= n} .<n-i>.
      (A: array (int, n), n: int n, i: int i): void =
      if i < n then begin
        if i > 0 then print ", "; print A[i]; loop (A, n, i+1)
      end else begin
        // loop exits
      end // end of [if]
  } // end of [pr]
  val N = 10
  val A = array_make_arrsz {int} $arrsz(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  val () = pr (A, N)
  val () = print_newline ()
  val () = revarr<int> (A, N)
  val () = pr (A, N)
  val () = print_newline ()
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [revarr.dats] *)
