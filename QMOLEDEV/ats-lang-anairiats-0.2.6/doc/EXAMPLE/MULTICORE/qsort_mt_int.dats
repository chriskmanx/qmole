//
// qsort_mt_int:
// A plain multithreaded quicksort implementation
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: March, 2010
//

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/pointer.dats"

(* ****** ****** *)

staload "libats/SATS/parworkshop.sats"
staload _ = "libats/DATS/parworkshop.dats"

(* ****** ****** *)

absviewtype work_vt
viewtypedef WSptr (l:addr) = WORKSHOPptr (work_vt, l)
viewtypedef work = {l:agz} (!WSptr l) -<lincloptr1> void
extern castfn work_encode (x: work): work_vt

(* ****** ****** *)

typedef fwork_type
  (a:viewt@ype) = {l:agz} (!WSptr l, &a >> a?) -> int
extern fun fwork : fwork_type (work)

implement fwork (ws, wk) = let
  val wk = wk
  val pfun = __cast (wk) where {
    extern castfn __cast
      (wk: !work >> opt (work, i >= 2)): #[i:nat] uintptr i
  } // end of [val]
in
  if pfun >= (uintptr1_of_uint1)2U then let
    prval () = opt_unsome {work} (wk)
    val () = wk (ws)
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

#define ARG_QUICKSORT_MT_DATS 1

(*

absviewt@ype T
extern fun lte_T_T (x: !T, y: !T):<> bool
extern fun compare_T_T (x: !T, y: !T):<> Sgn

overload compare with compare_T_T
overload <= with lte_T_T

*)

typedef T = int

(* ****** ****** *)

#include "qsort_mt.dats"

(* ****** ****** *)

#define CUTOFF %(1 << 20)

fun qsort_splt {lws:agz}
  {n:nat} {A:addr} (ws: !WSptr lws, A: ptr A, n: int n): void = let
  prval (pf, fpf) = __assert () where {
    extern prfun __assert (): (array_v (T, n, A), array_v (T, n, A) -<prf> void)
  } // end of [prval]
in
  if n > CUTOFF then let
    val i_pivot = partition (pf | A, n)
    val (pf_mul | ofs) = (size)i_pivot szmul2 sizeof<T>
    prval (pf1, pf2) = array_v_split {T} (pf_mul, pf)
    prval (pf21, pf22) = array_v_uncons {T} (pf2)
    prval pf1_mul = mul_add_const {1} (pf_mul)
    val () = workshop_insert_work (ws, f) where {
      val f = lam {l:agz} (ws: !WSptr l)
        : void =<lincloptr1> qsort_splt (ws, A, i_pivot)
      val f = work_encode (f)
    } // end of [val]
    val () = workshop_insert_work (ws, f) where {
      val f = lam {l:agz} (ws: !WSptr l)
        : void =<lincloptr1> qsort_splt (ws, A+ofs+sizeof<T>, n-i_pivot-1)
      val f = work_encode (f)
    } // end of [val]
    prval () = pf2 := array_v_cons {T} (pf21, pf22)
    prval () = pf := array_v_unsplit {T} (pf_mul, pf1, pf2)
    prval () = fpf (pf)
  in
    // empty
  end else let
    val () = qsort (pf | A, n)
    prval () = fpf (pf)
  in
    // empty
  end (* end of [if] *)
end // end of [qsort_splt]

(* ****** ****** *)

staload RAND = "libc/SATS/random.sats"

(* ****** ****** *)

fn array_ptr_print
  {n:nat} {l:addr} (
  pf_arr: !array_v (T, n, l) | A: ptr l, n: size_t n
) : void = let
  var !p_f = @lam
    (pf: !unit_v | i: sizeLt n, x: &T): void =<clo> begin
    $effmask_all (if i > 0 then print ", "; printf ("%i", @(x)))
  end // end of [var]
  prval pfu = unit_v ()
  val () = array_ptr_iforeach_vclo<T> (pfu | !A, !p_f, n)
  prval unit_v () = pfu
in
  // nothing
end // end of [array_ptr_print]

(* ****** ****** *)

#define N 100000000

fn random_array_ptr_gen
  {n:nat} (n: size_t n):<>
  [l:addr | l <> null] (
  free_gc_v (T?, n, l), array_v (T, n, l) | ptr l
) = let
  val (
    pfgc, pfarr | parr
  ) = array_ptr_alloc<T> (n)
  val () = array_ptr_initialize_fun<T> (
    !parr, n, lam (i, x) =<fun> x := $effmask_ref ($RAND.randint (N))
  ) // end of [array_ptr_make_fun_tsz_cloptr]
//
in
  (pfgc, pfarr | parr)
end // end of [random_array_ptr_gen]

(* ****** ****** *)

fn prerr_usage (cmd: string): void = begin
  prerr ("Usage:\n");
  prerrf ("  single core: %s [integer]\n", @(cmd));
  prerrf ("  multiple core: %s [integer(arg)] [integer(core)]\n", @(cmd));
end // end of [prerr_usage]

(* ****** ****** *)

#define QSZ %(1 << 16)
#define NWORKER 1

implement
main (argc, argv) = let
  val () = assert_errmsg_bool1
    (argc >= 2, "command format: qsort_mt_int <int> <ncore>")
  val n = int_of argv.[1]
  val n = int1_of_int (n)
  val () = assert_errmsg (n >= 0, #LOCATION)
  val fwork = __cast (fwork) where {
    extern castfn __cast (x: fwork_type work):<> fwork_type (work_vt)
  } // end of [val]
  val ws = workshop_make<work_vt> (QSZ, fwork)
  val nworker =
    (if (argc >= 3) then int_of argv.[2] else NWORKER): int
  val nworker = int1_of_int (nworker)
  val () = assert_errmsg (nworker > 0, #LOCATION)
  val _err = workshop_add_nworker (ws, nworker)
  val () = assert_errmsg (_err = 0, #LOCATION)
//
  val nsz = size1_of_int1 (n)
  val (pfgc, pfarr | A) = random_array_ptr_gen (nsz)
  val t = qsort_splt (ws, A, n)
//
  val () = workshop_wait_blocked_all (ws)
  val () = if (n <= 100) then
    (array_ptr_print (pfarr | A, nsz); print_newline ())
  val () = array_ptr_free {T} (pfgc, pfarr | A)
//
  var i: Nat = 0
  val () = while (i < nworker) let
    val _quit = $extval (work_vt, "(void*)0")
    val () = workshop_insert_work (ws, _quit) in i := i + 1
  end // end of [val]
  val () = workshop_wait_quit_all (ws)
  val () = (print "sorting is finished"; print_newline ())
  val () = workshop_free_vt_exn (ws)
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [qsort_mt_int.dats] *)
