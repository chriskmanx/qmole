//
//
// One of the early examples first done in ATS/Geizella
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: circa May 2007
//

(* ****** ****** *)

//
// HX:
// The *awkward* style should be not be changed so as to preserve
// a bit history about the development of ATS
//

(* ****** ****** *)

staload Rand = "libc/SATS/random.sats"
staload Time = "libc/SATS/time.sats"

(* ****** ****** *)

extern
fun array_int_ptr_make
  : {n:nat} int n -<!ref> [l:addr] (@[Nat][n] @ l | ptr l)
  = "ats_array_int_ptr_make"

extern
fun array_int_ptr_free
  : {n:nat} {l:addr} (@[Nat?][n] @ l | ptr l) -<!ref> void
  = "ats_array_int_ptr_free"

extern
fun array_int_ptr_get
  : {n:nat} {l:addr} (! @[Nat][n] @ l | ptr l, natLt n) -<> Nat
  = "ats_array_int_ptr_get"

extern
fun array_int_ptr_set
  : {n:nat} {l:addr} (! @[Nat][n] @ l | ptr l, natLt n, Nat) -<> void
  = "ats_array_int_ptr_set"

%{^

ats_ptr_type ats_array_int_ptr_make (ats_int_type n) {
  return calloc (n, sizeof(int)) ;  
}

ats_void_type ats_array_int_ptr_free (ats_ptr_type A) {
  free (A) ; return ;
}

ats_int_type ats_array_int_ptr_get (ats_ptr_type A, ats_int_type i) {
  return ((ats_int_type *)A)[i] ;
}

ats_void_type ats_array_int_ptr_set (ats_ptr_type A, ats_int_type i, ats_int_type x) {
  ((ats_int_type *)A)[i] = x ; return ;
}

%} // end of [%{^]

(* ****** ****** *)

fn heads_one ():<!ref> bool = $Rand.drand48 () < 0.5

fn heads_many {n:nat}
  (n: int n):<!ref> natLte n = aux (n, 0) where {
  fun aux {i,s:nat | i + s <= n} .<i>.
       (i: int i, s: int s):<!ref> natLte n =
      if i > 0 then
        if heads_one () then aux (i-1, s+1) else aux (i-1, s)
      else s
} // end of [heads_many]

fn test_one {n:nat} {l:addr}
  (pf: ! @[Nat][n+1] @ l | A: ptr l, n: int n):<!ref> void = let
  val cnt = heads_many (n)
in
  array_int_ptr_set (pf | A, cnt, array_int_ptr_get (pf | A, cnt) + 1)
end // end of [test_one]

fun test_many {m,n:nat} {l:addr} .<m>.
  (pf: ! @[Nat][n+1] @ l | A: ptr l, m: int m, n: int n):<!ref> void =
  if m > 0 then
    (test_one (pf | A, n); test_many (pf | A, m-1, n))
  else ()
// end of [test_many]

#define INC 16

fn test_show_one {l:addr} (times: Nat):<!ref> void = let
  fun aux {t,i:nat} .<t nsub i>. (t: int t, i: int i):<!ref> void =
    if i < t then (print '*'; aux (t, i+INC)) else print_newline ()
in
  if times > 0 then aux (times, 0) else print ".\n"
end // end of [test_show_one]

fun test_show_all {n,i:nat | i <= n+1} {l:addr} .<n+1-i>.
  (pf: ! @[Nat][n+1] @ l | A: ptr l, n: int n, i: int i):<!ref> void =
  if i <= n then
    (test_show_one (array_int_ptr_get (pf | A, i)); test_show_all (pf | A, n, i+1))
  else ()
// end of [test_show_all]

//

macdef double = $Time.double_of_clock

#define M 4096
#define N 32

implement main () = () where {
  val (pf | A) = array_int_ptr_make (N+1)
  val clock_sta = double ($Time.clock ())
  val () = begin
    $Rand.srand48_with_time ();
    test_many (pf | A, M, N);
    test_show_all (pf | A, N, 1);
    array_int_ptr_free (pf | A)
  end
  val clock_fin = double ($Time.clock ())
  val time_spent =
     (clock_fin - clock_sta) / (double)$Time.CLOCKS_PER_SEC
  // end of [val]
  val () = printf ("time spent = %.10f\n", @(time_spent))
} // end of [main]

(* ****** ****** *)

(* end of [coin_flip.dats] *)

