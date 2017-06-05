(*
// Implementing Erathosthene's sieve in linear-lazy style
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2008)
//
*)

(* ****** ****** *)

staload "prelude/DATS/lazy_vt.dats"

(* ****** ****** *)

#define nil stream_vt_nil
#define :: stream_vt_cons

(* ****** ****** *)

fun{a:t@ype}
  stream_vt_nth (xs0: stream_vt a, i: Nat): a = let
(*
  val () = begin
    print "stream_vt_nth: before: i = "; print i; print_newline ()
  end
*)
  val xs0_con = !xs0
in
  case+ xs0_con of
  | ~(x :: xs) => begin
      if i = 0 then (~xs; x) else stream_vt_nth (xs, i-1)
    end // end of [::]
  | ~nil () => $raise StreamSubscriptException ()
end // end of [stream_vt_nth]

(* ****** ****** *)

fun from_con {n:int} (n: int n)
  :<!laz> stream_vt_con (intGte n) = n :: from (n+1)
and from {n:int} (n: int n)
  :<!laz> stream_vt (intGte n) = $ldelay (from_con n)

//

typedef Nat2 = intGte 2

fun sieve_con
  (ns: stream_vt Nat2)
  :<!laz> stream_vt_con (Nat2) = let
(*
     val () = begin
       print "sieve_con: enter"; print_newline ()
     end
*)
     val ns_con = !ns
     val- n :: !p_ns = ns_con
(*
     val () = begin
       print "sieve_con: n = "; print n; print_newline ()
     end // end of [val]
*)
     val ns = !p_ns
     val () = (!p_ns := sieve (stream_vt_filter_cloptr<Nat2> (ns, lam x => x nmod1 n > 0)))
  in
     fold@ ns_con; ns_con
  end
// end of [sieve_con]

and sieve (ns: stream_vt Nat2)
  :<!laz> stream_vt (Nat2) = $ldelay (sieve_con ns, ~ns)
// end of [sieve]

//

fn primes (): stream_vt Nat2 = sieve (from 2)
fn prime (n: Nat): Nat = stream_vt_nth (primes (), n)

//

implement
main (argc, argv) = begin
//printf ("prime 1000 = %i\n", @(prime 1000)) ; // 7927
//printf ("prime 5000 = %i\n", @(prime 5000)) ; // 48619
printf ("prime 10000 = %i\n", @(prime 10000)) ; // 104743
//printf ("prime 20000 = %i\n", @(prime 20000)) ; // 224743
//printf ("prime 30000 = %i\n", @(prime 30000)) ; // = 350381 (2 min.)
//printf ("prime 50000 = %i\n", @(prime 50000)) ; // = 611957 (6 min.)
end // end of [main]

(* ****** ****** *)

(* end of [sieve_lazy_vt.dats] *)
