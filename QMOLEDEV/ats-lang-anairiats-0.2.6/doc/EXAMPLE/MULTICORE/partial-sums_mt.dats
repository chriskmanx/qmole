//
// partial-sums.dats:
//   computing partial sums of a power series
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: March, 2010
//

(* ****** ****** *)

staload M = "libc/SATS/math.sats"

(* ****** ****** *)

staload "libc/SATS/pthread.sats"

(* ****** ****** *)

staload "libats/SATS/parworkshop.sats"
staload _ = "libats/DATS/parworkshop.dats"

(* ****** ****** *)

fun loop
  (n: int, i: int, sum: &double): void =
  if i < n then let
    val () = sum := sum + $M.pow (2.0 / 3.0, double_of i)
  in
    loop (n, i+1, sum)
  end else ()
// end of [loop]

(* ****** ****** *)

dataviewtype ans =
  | D of (ans, ans) | S of double

fun finalize (t: ans): double =
  case+ t of
  | ~D (t1, t2) => finalize t1 + finalize t2 | ~S sum => sum
// end of [finalize]

(* ****** ****** *)

viewtypedef work = () -<lincloptr1> void
viewtypedef WSptr (l:addr) = WORKSHOPptr (work, l)

(* ****** ****** *)

fun fwork {l:agz}
  (ws: !WSptr l, wk: &work >> work?): int = let
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

fun loop_split {l:agz}
  (NSPLIT: int, ws: !WSptr l, n: int, i: int): ans = let
  val ni = n - i
in
  if ni > NSPLIT then let
    val ni2 = i + (ni / 2)
    val ans1 = loop_split (NSPLIT, ws, n, ni2)
    and ans2 = loop_split (NSPLIT, ws, ni2, i)
  in
    D (ans1, ans2)
  end else let
    val res = S (?)
    val S (!p) = res
    val () = !p := 0.0
    extern prfun __ref
      {l:addr} (pf: !double @ l): double @ l
    prval pf = __ref (view@ !p)
    extern prfun __unref {l:addr} (pf: double @ l): void
    val f = lam (): void =<lincloptr1>
      let val () = loop (n, i, !p); prval () =__unref (pf) in (*empty*) end
    // val () = f ()
    val () = workshop_insert_work (ws, f)
  in
    fold@ res; res
  end // end of [val]
end // end of [loop_split]

(* ****** ****** *)

// dynload "libats/DATS/parworkshop.dats" // this no longer necessary

(* ****** ****** *)

#define QSZ 256
#define NWORKER 1

implement
main (argc, argv) = let
  val () = assert_errmsg_bool1
    (argc >= 2, "exit: wrong command format!\n")
  val n = int_of argv.[1]
  val ws = workshop_make<work> (QSZ, fwork)
  val nworker =
    (if (argc >= 3) then int_of argv.[2] else NWORKER): int
  val nworker = int1_of_int (nworker)
  val () = assert_errmsg (nworker > 0, #LOCATION)
  val _err = workshop_add_nworker (ws, nworker)
  val () = assert_errmsg (_err = 0, #LOCATION)
  val N = max (1024, n / 1024) // threshold for splitting
  val t = loop_split (N, ws, n, 0)
  // val () = (print "spliting is done"; print_newline ())
  val () = workshop_wait_blocked_all (ws)
  val sum = finalize (t)
  var i: Nat = 0
  val () = while (i < nworker) let
    val _quit = $extval (work, "(void*)0")
    val () = workshop_insert_work (ws, _quit) in i := i + 1
  end // end of [val]
  val () = workshop_wait_quit_all (ws)
  val () = workshop_free_vt_exn (ws)
in
  printf ("%.9f\t(2/3)^k", @(sum)); print_newline ();
end // end of [main]

(* ****** ****** *)

(* end of [partial-sums_mt.dats] *)
