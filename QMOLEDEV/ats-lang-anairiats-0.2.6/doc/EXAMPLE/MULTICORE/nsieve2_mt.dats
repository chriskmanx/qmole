//
// Time March 2010
// Author: Hongwei Xi (hwxi @ cs.bu.edu)
//

//
// Erastothene's sieve
// This is an excellent example showing the issue of memory bottleneck!!!
//

(*

command: time nsieve2_mt <integer> <ncore>

output:
Primes up to 40960000  2488465

*)

(* ****** ****** *)

staload M = "libc/SATS/math.sats"

(* ****** ****** *)

staload "libc/SATS/pthread.sats"

(* ****** ****** *)

staload "libats/SATS/parworkshop.sats"
staload _ = "libats/DATS/parworkshop.dats"

(* ****** ****** *)

macdef tt = int8_of_int (1)
macdef ff = int8_of_int (0)

(* ****** ****** *)

typedef two = int8

(* ****** ****** *)

%{^

ATSinline()
ats_int_type
ceil2 (ats_int_type k, ats_int_type i) {
  int r = k % i ;
  if (r == 0) {
    return k ;
  } else {
    return (k + i - r) ;
  } // end of [if]
} // end of [ceil2]

%} // end of [%{^]

extern
fun ceil2 {k,i:nat | i > 0} (k: int k, i: int i): intGte k = "ceil2"

(* ****** ****** *)

extern fun sieve_once
  {m,limit:nat | limit <= m} {i,j:nat} {l:addr}
  (pf: !array_v (two, m, l) | A: ptr l, limit: int limit, i: int i, j: int j)
  : void = "sieve_once_safe"
// end of [sieve_once]

implement sieve_once (pf | A, limit, i, j) = begin
  if (j < limit) then begin
    (if A[j] <> ff then A[j] := ff; sieve_once (pf | A, limit, i, j+i))
  end // end of [if]
end // end of [sieve_once]

extern fun sieve_many
  {m,m1,m2:nat | m1 <= m; m2 <= m} {l:addr}
  (pf: !array_v (two, m, l) | A: ptr l, m1: int m1, m2: int m2): void

implement sieve_many (pf | p_A, m1, m2) = () where {
  var i: intGte 2 = 2
  val () = while (i < m1) let
    val () = if p_A->[i] = tt then sieve_once (pf | p_A, m2, i, i+i)
  in
    i := i + 1
  end // end of [while]
} // end of [sieve_many]

(* ****** ****** *)

extern fun sieve_many_seg
  {m,m1,m2,m3:nat | m1 <= m; m2 <= m; m3 <= m} {l:addr}
  (pf: !array_v (two, m, l) | A: ptr l, m1: int m1, m2: int m2, m3: int m3): void
  = "sieve_many_seg_safe"

extern fun sieve_many_seg_unsafe
  {m1,m2,m3:nat} {l:addr} (A: ptr l, m1: int m1, m2: int m2, m3: int m3): void
  = "sieve_many_seg_safe"

implement sieve_many_seg (pf | p_A, m1, m2, m3) = () where {
(*
  val () = (print "sieve_many_seg: m2 = "; print m2; print_newline ())
  val () = (print "sieve_many_seg: m3 = "; print m3; print_newline ())
*)
  var i: intGte 2 = 2
  val () = while (i < m1) let
    val () = if p_A->[i] = tt then let
      val j = ceil2 (m2, i) in sieve_once (pf | p_A, m3, i, j)
    end // end of [val]
  in
    i := i + 1
  end // end of [val]
} // end of [sieve_seg_many]

(* ****** ****** *)

fn sqrt_int {m:nat} (m: int m): Nat = let
  val m_sqrt = int_of_double ($M.sqrt (double_of m + 0.5))
  val m_sqrt = int1_of_int m_sqrt
  val () = assert (m_sqrt >= 0) // redundant at run-time
in
  m_sqrt
end // end of [sqrt_int]

(* ****** ****** *)

viewtypedef work = () -<lincloptr1> void
viewtypedef WSptr (l:addr) = WORKSHOPptr (work, l)

(* ****** ****** *)

fun fwork {lws:agz}
  (ws: !WSptr lws, wk: &work >> work?): int = let
  val wk = wk
  val pfun = __cast (wk) where {
    extern castfn __cast
      (wk: !work >> opt (work, i >= 2)): #[i:nat] uintptr i
  } // end of [val]
in
  if pfun >= (uintptr1_of_uint1)2U then let
    prval () = opt_unsome {work} (wk)
    val () = wk ()
    val () = cloptr_free (wk)
  in
    1 // the worker is to continue
  end else let
    val u = uint1_of_uintptr1 (pfun)
    val i = int_of_uint (u)
    prval () = opt_unnone {work} (wk)
    prval () = cleanup_top {work} (wk)
  in
    ~i // the worker is to pause or quit
  end // end of [if]
end // end of [fwork]

(* ****** ****** *)

fn nsieve_mt {lws:agz}
  {m:int | m >= 2} (ws: !WSptr lws, m: int m): void = let
//
  val () = assert_prerrf_bool1 (
    m >= 2, "nsieve_mt(%i): argument is illegal; it must be positive.\n", @(m)
  ) // end of [val]
//
  val msz = size1_of_int1 (m)
  val [la:addr] (pf_gc, pf | p_A) =
    array_ptr_alloc_tsz {two} (msz, sizeof<two>)
  var x0 = tt
  val () = array_ptr_initialize_elt_tsz {two} (!p_A, msz, x0, sizeof<two>)
  val m1 = sqrt_int (m)
  val [m1:int] m1 = (if m1 < m then m1 + 1 else m): natLte m
  val () = sieve_many (pf | p_A, m1, m1) // find the all the primes upto [m1]
//
  #define INC %(1 << 16)
//
  val () = split (pf | ws, p_A, m1, m, m1) where {
//
  fun split {m1,m:nat | m1 <= m} {j:nat} (
      pf: !array_v (two, m, la)
    | ws: !WSptr lws
    , p_A: ptr la
    , m1: int m1
    , m: int m
    , j: int j
    ) : void = let
    val j1 = j + INC
  in
    if j1 < m then let
      val () = workshop_insert_work (ws, f) where {
        val f = lam (): void =<lincloptr1> sieve_many_seg_unsafe (p_A, m1, j, j1)
      } // end of [val]
(*
      val () = sieve_many_seg_unsafe (p_A, m1, j, j1)
*)
    in
      split (pf | ws, p_A, m1, m, j1)
    end else let
      val () = workshop_insert_work (ws, f) where {
        val f = lam (): void =<lincloptr1> sieve_many_seg_unsafe (p_A, m1, j, m)
      } // end of [val]
(*
      val () = sieve_many_seg_unsafe (p_A, m1, j, m)
*)
    in
      // nothing
    end // end of [if]
  end // end of [split]
//
  }
//
  val () = workshop_wait_blocked_all (ws)  
  val nworker = workshop_get_nworker (ws)
  var i: Nat = 0
  val () = while (i < nworker) let
    val _quit = $extval (work, "(void*)0")
    val () = workshop_insert_work (ws, _quit) in i := i + 1
  end // end of [val]
  val () = workshop_wait_quit_all (ws)  
//
  val count = loop (pf | 2, 0) where {
    fun loop {i:nat}
      (pf: !array_v (two, m, la) | i: int i, c: int):<cloref1> int =
      if i < m then begin
        if p_A->[i] = tt then loop (pf | i+1, c+1) else loop (pf | i+1, c)
      end else begin
        c // return value
      end
    // end of [loop]
  } // end of [where]
//
  val () = array_ptr_free {two} (pf_gc, pf | p_A)
in
  printf ("The number of primes < %8i is %8i\n", @(m, count))
end // end of [nsieve_mt]

(* ****** ****** *)

// dynload "libats/DATS/parworkshop.dats" // unnecessary

(* ****** ****** *)

#define QSZ 1024 // HX: arbitrarily choosen; should be >= 1
#define NWORKER 1

implement main (argc, argv) = let
  var nworker: int = NWORKER
  val () = assert_errmsg_bool1 (
    argc >= 2, ": command format: nsieve2_mt <integer> <ncore>"
  )
  val i = int1_of argv.[1]
  val () = assert_errmsg_bool1 (
    i >= 2, "The input integer needs to be at least 2.\n"
  ) // end of [val]
  val () = if argc >= 3 then (nworker := int_of argv.[2])
//
  val nworker = int1_of_int (nworker)
  val () = assert_errmsg (nworker > 0, #LOCATION)
//
  val ws = workshop_make<work> (QSZ, fwork)
  val _err = workshop_add_nworker (ws, nworker)
  val () = assert_errmsg (_err = 0, #LOCATION)
//
  val () = nsieve_mt (ws, i)
//
  val () = workshop_free_vt_exn (ws)
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [nsieve2_mt.dats] *)
