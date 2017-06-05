//
// randcompec_mt.dats: computing the Euler's constant via randomization
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//   modifying code by Chris Double (chris DOT double AT double DOT co DOT nz)
// Time: the 6th of October, 2010
//

(* ****** ****** *)
//
// HX-2010-10-06:
// Per Shiv's request, I modified Chris Double's code to present
// a simple method for passing proofs of views between a parent thread
// and it single child.
//
(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"

staload "libc/SATS/pthread.sats"
staload "libc/SATS/pthread_uplock.sats"
staload "libc/SATS/random.sats"
staload "libc/SATS/unistd.sats"
staload "libats/SATS/parworkshop.sats"
staload _(*anon*) = "libats/DATS/parworkshop.dats"

(* ****** ****** *)

fn random_double
  (buf: &drand48_data): double = let
  var r: double; val _ = drand48_r(buf, r)
in
  r
end // end of [random_double]

fn attempts (buf: &drand48_data): uint = let 
  fun loop (buf: &drand48_data, sum: double, count: uint): uint = 
    if sum <= 1.0 then loop(buf, sum + random_double(buf), count + 1U) else count
in
  loop(buf, 0.0, 0U)
end // end of [attempts]

fn n_attempts (n:int): ulint = let
  var buf: drand48_data
  val _ = srand48_r(0L, buf)
  fun loop (n:int, count: ulint, buf: &drand48_data): ulint =
    if n = 0 then count else loop(n-1, count + ulint_of(attempts(buf)), buf)
in
  loop(n, 0UL, buf)
end // end of [n_attempts]

dataviewtype command = 
  | {l:addr} Compute of (ulint @ l | ptr l, int, upticket (ulint @ l))
  | Quit of ()
// end of [command]

viewtypedef work = command

fun fwork {l:addr}
  (ws: !WORKSHOPptr(work,l), x: &work >> work?): int = 
  case+ x of
  | ~Compute (
       pf | p, iterations, ticket
    ) => let 
       val () = !p := n_attempts (iterations)
       val () = pthread_upticket_upload_and_destroy (pf | ticket)
     in 1 end
  | ~Quit () => 0
// end of [fwork]

(* ****** ****** *)

fun insert_all_and_wait
  {l,l2:agz} {n0:pos} (
    pf_arr: !array_v(ulint, n0, l2)
  | ws: !WORKSHOPptr(work, l)
  , p_arr: ptr l2, n0: int n0, iterations0: int
  ) : void = let
  typedef T = ulint
  fun aux {l,l2:agz} {n:nat} .<n>. ( // HX: not tail-recursive
      pf: !array_v(ulint, n, l2)
    | ws: !WORKSHOPptr(work, l), p: ptr l2, n: int n, iterations: int
    ) : void =
    if n > 0 then let
      viewdef V = ulint @ l2
      prval (pf1, pf2) = array_v_uncons{T}(pf)
      val lock = pthread_uplock_create ()
      val ticket = pthread_upticket_create {V} (lock)
      val () = workshop_insert_work(ws, Compute (pf1 | p, iterations, ticket))
      val () = aux (pf2 | ws, p + sizeof<T>, n - 1, iterations)
      val (pf1 | ()) = pthread_uplock_download {V} (lock)
      val () = pthread_uplock_destroy (lock)
      prval () = pf := array_v_cons{T}(pf1, pf2)
    in
      // nothing
    end // end of [if]
  // end of [aux]
in
  aux (pf_arr | ws, p_arr, n0, iterations0 / n0)
end // end of [insert_all_and_wait]

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
  val () = insert_all_and_wait (pf_arr | ws, p_arr, NCPU, ITER)
//
  var k: Nat = 0
  var total: ulint = 0UL
  val () = for (k := 0; k < NCPU; k := k + 1) total := total + p_arr->[k]
  val avg = double_of (total) / double_of (NCPU * (ITER / NCPU))
  val () = (print "total = "; print total; print_newline ())
  val () = print(avg)
  val () = print_newline ()
//
// HX-2010-10-06: the following code is just for cleanup
//
  val () = workshop_wait_blocked_all(ws)
//
  var j: Nat = 0
  val nworker = workshop_get_nworker(ws)
  val () = while (j < nworker) let
    val () = workshop_insert_work(ws, Quit ()) in j := j + 1
  end // end of [val]
  val () = workshop_wait_quit_all(ws)
  val () = workshop_free_vt_exn(ws)
//
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [randomcompec2_mt.dats] *)
