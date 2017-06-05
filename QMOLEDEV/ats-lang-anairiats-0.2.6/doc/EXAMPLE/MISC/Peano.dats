(*
** An implementation of natural numbers in ATS
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: November, 2009
*)

(* ****** ****** *)

(*
**
** How to compile:
**   atscc -o Peano Peano.dats -DATS_GATS
** How to test:
**   ./Peano
*)

(* ****** ****** *)

datatype NAT (int) =
  | NATzero (0) | {n:nat} NATsucc (n+1) of NAT n
// end of [NAT]

(* ****** ****** *)

// tail-recursive
fun add_NAT_NAT {m,n:nat} .<m>.
  (m: NAT m, n: NAT n):<> NAT (m+n) = case+ m of
  | NATzero () => n | NATsucc (m1) => add_NAT_NAT (m1, NATsucc n)
// end of [add_NAT_NAT]

overload + with add_NAT_NAT

(* ****** ****** *)

// tail-recursive
fun sub_NAT_NAT {m,n:nat} .<m>.
  (m: NAT m, n: NAT n):<> NAT (m nsub n) = case+ m of
  | NATzero () => NATzero ()
  | NATsucc m1 => begin case+ n of
    | NATzero () => m | NATsucc n1 => sub_NAT_NAT (m1, n1)
    end // end of [NATsucc]
// end of [sub_NAT_NAT]

overload - with sub_NAT_NAT

(* ****** ****** *)

// tail-recursive
fun mul_NAT_NAT {m,n:nat} .<m>.
  (m: NAT m, n: NAT n):<> [mn:nat] (MUL (m, n, mn) | NAT mn) = let
  fun loop {m:nat} {mn:int} {r:nat} .<m>.
    (pf: MUL (m, n, mn) | m: NAT m, n: NAT n, res: NAT r):<> NAT (mn+r) =
    case+ m of
    | NATzero () => let
        prval MULbas () = pf in res
      end // end of [NATzero]
    | NATsucc (m1) => let
        prval MULind (pf1) = pf
      in
        loop (pf1 | m1, n, add_NAT_NAT (res, n))
      end // end of [NATsucc]
  // end of [loop]
  prval [mn:int] pf = mul_istot {m,n} () // pf: MUL (m, n, mn)
  prval () = mul_nat_nat_nat (pf) // prove that [mn >= 0] holds
in
  (pf | loop (pf | m, n, NATzero))
end // end of [mul_NAT_NAT]

overload * with mul_NAT_NAT

(* ****** ****** *)

fn ofint_NAT {n:nat}
  (n: int n):<> NAT n = let
  fun loop {i:nat | i <= n} .<i>.
    (i: int i, res: NAT (n-i)):<> NAT n =
    if i > 0 then loop (i-1, NATsucc res) else res
in
  loop (n, NATzero)
end // end of [ofint_NAT]

fn toint_NAT {n:nat}
  (n: NAT n):<> int n = loop (n, 0) where {
  fun loop {i:nat | i <= n} .<i>.
    (i: NAT i, res: int (n-i)):<> int n =
    case+ i of
    | NATzero () => res | NATsucc i1 => loop (i1, res+1)
  // end of [loop]
} // end of [toint_NAT]

(* ****** ****** *)

staload "libc/SATS/random.sats"

(* ****** ****** *)

#define MAX 100

implement main () = let
  val () = srand48_with_time ()
  val m = randint (MAX)
  val n = randint (MAX)
  val _m = ofint_NAT m and _n = ofint_NAT n
  val add_m_n = _m + _n
  val () = (
    printf ("%i + %i = %i\n", @(m, n, toint_NAT add_m_n))
  )
  val sub_m_n = _m - _n
  val () = (
    printf ("%i - %i = %i\n", @(m, n, toint_NAT sub_m_n))
  )
  val (pf_mn | mul_m_n) = _m * _n
  val () = (
    printf ("%i * %i = %i\n", @(m, n, toint_NAT mul_m_n))
  )
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [Peano.dats] *)
