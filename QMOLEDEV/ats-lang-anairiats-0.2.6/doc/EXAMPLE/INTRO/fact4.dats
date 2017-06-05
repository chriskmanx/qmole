//
//
// A Verified Implementation of the Factorial Function
//
// Author: Hongwei Xi (December, 2009)
//
//

(*

The mathematical definition of factorials:

fact (0) = 1
fact (n) = n * fact (n-1) ; if n > 0

*)

(* ****** ****** *)

extern
praxi mul_equal
  {m1,m2:int | m1==m2}
  {n1,n2:int | n1==n2} (): [m1*n1==m2*n2] void
// end of [mul_equal]
extern
praxi mul_assoc {p,q,r:int} (): [(p*q)*r==p*(q*r)] void
// end of [mul_assoc]

(* ****** ****** *)

sta fact : int -> int

extern
praxi fact_equal
  {n1,n2:int | n1==n2} (): [fact(n1)==fact(n2)] void
// end of [mul_equal]

extern
praxi fact_bas (): [fact(0) == 1] void
extern
praxi fact_ind {n:pos} (): [fact(n) == n * fact(n-1)] void

(* ****** ****** *)
//
// HX: [fact] implements the factorial function
//
fn fact {n:nat} // in a tail-recursive style
  (n: int n): int (fact n) = loop (n, 1) where {
  // [loop] is tail-recusive
  fun loop {n:nat; x:int} .< n >.
    (n: int n, x: int x) : int (x * fact n) =
    if n > 0 then let
      val [xn:int] (pf_mul_x_n_xn | xn) = x imul2 n
      prval () = mul_elim (pf_mul_x_n_xn)
      stadef res1 = fact(n-1)
      val res = loop (n-1, xn) // res: int (xn*res1)
      prval () = mul_equal {xn,x*n} {res1,res1} ()
      prval () = mul_assoc {x,n,res1} () // x*n*res1=x*(n*res1)
      prval () = fact_ind {n} () // fact n = n*res1
      prval () = mul_equal {x,x} {fact n,n*res1} ()
    in
      res
    end else let
      prval () = fact_bas ()
      prval () = fact_equal {n,0} ()
      prval () = mul_equal {x,x} {fact(n),1} ()
    in
      x
    end // end of [val]
} // end of [fact]

(* ****** ****** *)
//
// HX: for a simple test: are there any dounts :)
//
implement main (argc, argv) = case+ argc of
  | 2 => begin
      let
        val n = int1_of argv.[1]
        val () = assert_prerrf_bool1 (
          n >= 0, "Exit: negative argument: %i\n", @(n)
        )
        val res2 = fact n
      in
        printf ("The value of fact(%i) is %i.\n", @(n, res2))
      end
    end // end of [2]
  | _ => let
      val cmd = argv.[0]
    in
      printf ("Usage: %s [integer]\n", @(cmd))
    end // end of [_]
// end of [main]

(* ****** ****** *)

(* end of [fact4.dats] *)
