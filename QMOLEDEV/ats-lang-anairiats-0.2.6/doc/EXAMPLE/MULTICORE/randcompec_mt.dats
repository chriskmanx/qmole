//
// randcompec_mt.dats: computing the Euler's constant via randomization
//
// Author: Chris Double (chris DOT double AT double DOT co DOT nz)
//   with minor tweaking by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: August, 2010
//

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"

staload "libc/SATS/random.sats"
staload "libc/SATS/unistd.sats"
staload "libats/SATS/parworkshop.sats"
staload _(*anon*) = "libats/DATS/parworkshop.dats"

(* ****** ****** *)

fn random_double (buf: &drand48_data): double = let
  var r: double
  val _ = drand48_r(buf, r)
in
  r
end

fn attempts (buf: &drand48_data): uint = let 
  fun loop (buf: &drand48_data, sum: double, count: uint): uint = 
    if sum <= 1.0 then loop(buf, sum + random_double(buf), count + 1U) else count
in
  loop(buf, 0.0, 0U)
end

fn n_attempts (n:int): ulint = let
  var buf: drand48_data
  val _ = srand48_r(0L, buf)
  fun loop (n:int, count: ulint, buf: &drand48_data): ulint =
    if n = 0 then count else loop(n-1, count + ulint_of(attempts(buf)), buf)
in
  loop(n, 0UL, buf)
end

dataviewtype command = 
  | {l:addr} Compute of (ulint @ l, ulint @ l -<lin,prf> void | ptr l, int)
  | Quit of ()
// end of [command]

viewtypedef work = command

fun fwork {l:addr}
  (ws: !WORKSHOPptr(work,l), x: &work >> work?): int = 
  case+ x of
  | ~Compute (pf_p, fpf_p | p, iterations) => let 
       val () = !p := n_attempts (iterations)
       prval () = fpf_p (pf_p) // the view is returned to the scheduler
     in 1 end
  | ~Quit () => 0
// end of [fwork]

fun insert_all
  {l,l2:agz} {n0:pos} (
    pf_arr: !array_v(ulint, n0, l2)
  | ws: !WORKSHOPptr(work, l)
  , p_arr: ptr l2, n0: int n0, iterations0: int
  ) : void = let
  typedef T = ulint
  fun loop {l,l2:agz} {n:nat} .<n>. (
      pf: !array_v(ulint, n, l2)
    | ws: !WORKSHOPptr(work, l), p: ptr l2, n: int n, iterations: int
    ) : void =
    if n > 0 then let
      prval (pf1, pf2) = array_v_uncons{T}(pf)
      prval (pf_p, fpf_p)  = __borrow(pf1) where {
        extern prfun __borrow {l:addr} (pf: !T @ l): (T @ l, T @ l -<lin,prf> void)
      } // end of [prval]
      val () = workshop_insert_work(ws, Compute (pf_p, fpf_p | p, iterations))
      val () = loop(pf2 | ws, p + sizeof<T>, n - 1, iterations)
      prval () = pf := array_v_cons{T}(pf1, pf2)
    in
      // nothing
    end // end of [if]
  // end of [loop]
in
  loop(pf_arr | ws, p_arr, n0, iterations0 / n0)
end // end of [insert_all]

(* ****** ****** *)

#define NCPU0 1
#define ITER0 1000000

implement
main (argc, argv) = let 
//
  val () = if (argc = 1) then (
    prerr ("The command format: randcompec_mt <integer> <ncore>\n"); exit(1)
  ) // end of [val]
//
  var ITER: int = ITER0 // default
  val () = if (argc >= 2) then ITER := int_of (argv.[1])
  val ITER = int1_of_int (ITER)
//
  var NCPU: int = NCPU0 // default
  val () = if (argc >= 3) then NCPU := int_of (argv.[2])
  val NCPU = int1_of_int (NCPU)
//
  val () = assert_prerrf_bool1 (NCPU >= 1, "%s: NCPU = %i\n", @(#LOCATION, NCPU))
  val () = assert_prerrf_bool1 (ITER >= NCPU, "%s: NCPU = %i\n", @(#LOCATION, ITER))
//
  val NCPUsz = size1_of_int1 (NCPU)
  val ws = workshop_make<work>(NCPUsz, fwork)
  var ncpu: int = 0
  val () = while (ncpu < NCPU) let
    val _err = workshop_add_worker(ws)
    val () = assert_prerrf_bool
      (_err = 0, "%s: [workshop_add_worker] failed\n", @(#LOCATION))
    // end of [val]
  in
    ncpu := ncpu + 1
  end // end of [val]
// 
  var !p_arr with pf_arr = @[ulint][NCPU](0UL)
//
  val () = insert_all(pf_arr | ws, p_arr, NCPU, ITER)
//
  val () = workshop_wait_blocked_all(ws)
//
  var j: Nat = 0
  val nworker = workshop_get_nworker(ws) // = NCPU
  val () = while (j < nworker) let
    val () = workshop_insert_work(ws, Quit ()) in j := j + 1
  end // end of [val]
  val () = workshop_wait_quit_all(ws)
  val () = workshop_free_vt_exn(ws)
//
  var k: Nat = 0
  var total: ulint = 0UL
  val () = for (k := 0; k < NCPU; k := k + 1) total := total + p_arr->[k]
  val avg = double_of (total) / double_of (NCPU * (ITER / NCPU))
  val () = (print "total = "; print total; print_newline ())
  val () = print(avg)
  val () = print_newline ()
//
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [randomcompec_mt.dats] *)
