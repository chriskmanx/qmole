(*

// Implementing Erathosthene's sieve in lazy style

// author: Hongwei Xi (September, 2007)

*)

(* ****** ****** *)

staload "prelude/DATS/lazy.dats"

(* ****** ****** *)

#define nil stream_nil
#define :: stream_cons

(* ****** ****** *)

fun from {n:int} (n: int n):<!laz> stream (intGte n) =
  $delay (n :: from (n+1))

//

typedef Nat2 = intGte 2

fun sieve (ns: stream Nat2):<!laz> stream (Nat2) = $delay (let
  val- n :: ns = !ns
in
  n :: sieve (stream_filter_cloref<Nat2> (ns, lam x => x nmod n > 0))
end : stream_con Nat2) // end of [sieve]

//

val primes: stream Nat2 = sieve (from 2)
fun prime_get (n: Nat): Nat = stream_nth (primes, n)

//

implement main (argc, argv) = begin

(*
printf ("prime 1 = %i\n", @(prime_get 1)) ;
printf ("prime 10 = %i\n", @(prime_get 10)) ;
printf ("prime 100 = %i\n", @(prime_get 100)) ;
printf ("prime 1000 = %i\n", @(prime_get 1000)) ;
printf ("prime 999 = %i\n", @(prime_get 999)) ;
printf ("prime 5000 = %i\n", @(prime_get 5000)) ; // = 48619
printf ("prime 10000 = %i\n", @(prime_get 10000)) ; // = 104743
*)

printf ("prime 10000 = %i\n", @(prime_get 10000)) ; // = 104743

end // end of [main]

(* ****** ****** *)

(* end of [sieve_lazy.dats] *)
