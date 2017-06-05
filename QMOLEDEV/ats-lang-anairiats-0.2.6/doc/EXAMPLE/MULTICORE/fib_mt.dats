//
// fib_mt.dats: computing Fibonacci numbers
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: March, 2010
//

(* ****** ****** *)

staload "libats/SATS/parworkshop.sats"
staload _ = "libats/DATS/parworkshop.dats"

(* ****** ****** *)

macdef int64 = int64_of_int

fun fib
  (n: int, sum: &int64): void =
  if n >= 2 then let
    val () = fib (n-1, sum); val () = fib (n-2, sum)
  in
    // nothing
  end else
    sum := sum + (int64)n
  // end of [if
// end of [fib]

(* ****** ****** *)

dataviewtype cont =
  | D of (cont, cont) | S of int64
// end of [cont]

fun finalize (t: cont): int64 =
  case+ t of
  | ~D (t1, t2) => finalize t1 + finalize t2
  | ~S sum => sum
// end of [finalize]

(* ****** ****** *)

viewtypedef work = () -<lincloptr1> void
viewtypedef WSptr (l:addr) = WORKSHOPptr (work, l)

(* ****** ****** *)

fun fwork {l:addr}
  (ws: !WSptr l, wk: &work >> work?): int = let
  val wk = wk
  val pfun = __cast (wk) where {
    extern castfn __cast
      (wk: !work >> opt (work, i >= 1)): #[i:nat] uintptr i
  } // end of [val]
in
  if pfun >= (uintptr1_of_uint1)1U then let
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

fun fib_split {l:agz}
  (N: int, ws: !WSptr l, n: int): cont = let
in
  if n > N then let
    val cont1 = fib_split (N, ws, n-1)
    and cont2 = fib_split (N, ws, n-2)
  in
    D (cont1, cont2)
  end else let
    val res = S (?)
    val S (!p) = res
    val () = !p := (int64)0
    extern prfun __ref
      {l:addr} (pf: !int64 @ l): int64 @ l
    prval pf = __ref (view@ !p)
    extern prfun __unref {l:addr} (pf: int64 @ l): void
    val f = lam (): void =<lincloptr1>
      let val () = fib (n, !p); prval () =__unref (pf) in (*empty*) end
    // val () = f ()
    val () = workshop_insert_work (ws, f)
  in
    fold@ res; res
  end // end of [val]
end // end of [fib_split]

(* ****** ****** *)

// dynload "libats/DATS/parworkshop.dats" // unnecessary

(* ****** ****** *)

#define QSZ 1024
#define NWORKER 1

implement
main (argc, argv) = let
  val () = assert_errmsg_bool1
    (argc >= 2, "command format: fib_mt <int> <ncore>")
  val n = int_of argv.[1]
  val N = max (10, n - 16)
  val nworker =
    (if (argc >= 3) then int_of argv.[2] else NWORKER): int
  val nworker = int1_of_int (nworker)
  val () = assert_errmsg (nworker > 0, #LOCATION)
  val ws = workshop_make<work> (QSZ, fwork)
  val _err = workshop_add_nworker (ws, nworker)
  val () = assert_errmsg (_err = 0, #LOCATION)
  val t = fib_split (N, ws, n)
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
  print "fib("; print n; print ") = "; print sum; print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [fib_mt.dats] *)
