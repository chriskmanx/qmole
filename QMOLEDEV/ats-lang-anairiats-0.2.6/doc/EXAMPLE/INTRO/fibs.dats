//
//
// Some implementations of the Fibonacci function
// Author: Hongwei Xi (September, 2007)
//

(* ****** ****** *)
//
// How to compile:
//   atscc -o fibs fibs.dats -lats -lgmp
// How to test:
//   ./fibs
//
(* ****** ****** *)

fun fib1 (x: int): int =
  if x > 1 then fib1 (x-1) + fib1 (x-2) else x
// end of [fib1]

(* ****** ****** *)

fun fib2 (x: Nat): Nat =
  if x > 1 then fib2 (x-1) + fib2 (x-2) else x
// end of [fib2]

(* ****** ****** *)

fun fib3 (x: Nat): Nat =
  loop (x, 0, 1) where {
  fun loop (x: Nat, a0: Nat, a1: Nat): Nat =
    if x > 0 then loop (x-1, a1, a0 + a1) else a0
} // end of [where]

(* ****** ****** *)

dataprop FIB (int, int) =
  | FIB_bas_0 (0, 0)
  | FIB_bas_1 (1, 1)
  | {i:nat} {r0,r1:int}
    FIB_ind (i+2, r0+r1) of (FIB (i, r0), FIB (i+1, r1))
// end of [FIB]

fun fib4 {n:nat}
  (x: int n): [r:int] (FIB (n, r) | int r) = let
  fun loop {i,j:nat | i+j == n} {r0,r1:int}
    (pf0: FIB (j, r0), pf1: FIB (j+1, r1) | x: int i, a0: int r0, a1: int r1)
    : [r:int] (FIB (n, r) | int r) =
    if x > 0 then loop (pf1, FIB_ind (pf0, pf1) | x-1, a1, a0 + a1)
    else (pf0 | a0)
in
  loop (FIB_bas_0 (), FIB_bas_1 () | x, 0, 1)
end // end of [fib4]

(* ****** ****** *)

staload "libats/SATS/intinf.sats"
dynload "libats/DATS/intinf.dats"

(* ****** ****** *)

fun fib5 {n:nat} (x: int n)
  : [r:int] (FIB (n, r) | intinfptr_gc r) = let
  fun loop {i,j:nat | i+j==n} {r0,r1:int} (
      pf0: FIB (j, r0), pf1: FIB (j+1, r1)
    | x: int i, a0: intinfptr_gc r0, a1: intinfptr_gc r1
    ) : [r:int] (FIB (n, r) | intinfptr_gc r) =
    if x > 0 then let
      val (pf0_gc, pf0_at | p_a0) = a0
      val (pf1_gc, pf1_at | p_a1) = a1
      val a2 = !p_a0 + !p_a1
      val () = intinfptr_free @(pf0_gc, pf0_at | p_a0)
      val a1 = (pf1_gc, pf1_at | p_a1)
    in
      loop (pf1, FIB_ind (pf0, pf1) | x-1, a1, a2)
    end else let
      val (pf1_gc, pf1_at | p_a1) = a1
    in
      intinfptr_free @(pf1_gc, pf1_at | p_a1); (pf0 | a0)
    end // end of [if]
  val intinf_0 = intinf_make 0 and intinf_1 = intinf_make 1
in
  loop (FIB_bas_0 (), FIB_bas_1 () | x, intinf_0, intinf_1)
end // end of [fib5]

(* ****** ****** *)

implement main (argc, argv) = let
//
val () =
  if argc < 2 then begin
    prerrf ("Usage: %s [integer]\n", @(argv.[0]));
    exit 1
  end
val () = assert (argc >= 2)
//
val n = int1_of (argv.[1]); val () =
  if n < 0 then begin
    prerrf ("The argument = %i is illegal.\n", @(n)); exit 1
  end // end of [val]
val () = assert (n >= 0)
//
val fib1_n = fib1 n
val () = begin
  printf ("fib1(%i) = ", @(n)); print fib1_n; print_newline ()
end // end of [val]
//
val fib2_n = fib2 n
val () = begin
  printf ("fib2(%i) = ", @(n)); print fib2_n; print_newline ()
end // end of [val]
//
val fib3_n = fib3 n
val () = begin
  printf ("fib3(%i) = ", @(n)); print fib3_n; print_newline ()
end // end of [val]
//
val (_ | fib4_n) = fib4 n
val () = begin
  printf ("fib4(%i) = ", @(n)); print fib4_n; print_newline ()
end // end of [val]
//
val (_ | fib5_n) = fib5 n
val () = let
  val (pf_gc, pf_at | p) = fib5_n
  val () = begin
    printf ("fib5(%i) = ", @(n)); print !p; print_newline ()
  end // end of [val]
in
  intinfptr_free @(pf_gc, pf_at | p)
end // end of [let]
//
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [fibs.dats] *)
