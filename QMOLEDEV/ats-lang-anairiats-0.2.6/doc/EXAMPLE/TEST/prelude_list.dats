(*
** some testing code for functions declared in
** prelude/SATS/list.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Spring, 2009
//

(* ****** ****** *)

staload Rand = "libc/SATS/random.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

#define MAXELT 100
(*
fun random_list_gen {n:nat}
  (n: int n): list (natLt MAXELT, n) = loop (n, list_nil) where {
  typedef T = natLt MAXELT
  fun loop {i,j:nat} .<i>.
    (i: int i, xs: list (T, j)):<!ref> list (T, i+j) =
    if i = 0 then xs else loop (i-1, list_cons ($Rand.randint MAXELT, xs))
  // end of [loop]
} // end of [random]
*)

fun random_list_gen {n:nat}
  (n: int n): list (natLt MAXELT, n) = xs where {
  val xs = list_vt_tabulate_fun (lam _ =<!ref> $Rand.randint MAXELT, n)
  val xs = list_of_list_vt (xs)
} // end of [val]

(* ****** ****** *)

#define l2l list_of_list_vt 

implement
main (argc, argv) = let
//
// for printing out an integer list
// also for testing [list_iforeach]
//
  fn lstpr (xs: List int): void = () where {
    val () = list_iforeach_fun (xs, f) where {
      fn f (i: int, x: int)
        : void = (if i > 0 then print (", "); print x)
      // end of [f]
    } // end of [val]
  } // end of [lstpr]
  fun lsteq (xs1: List int, xs2: List int): bool =
    case+ (xs1, xs2) of
    | (list_nil _, list_nil _) => true
    | (list_cons (x1, xs1), list_cons (x2, xs2)) when x1 = x2 => lsteq (xs1, xs2)
    | (_, _) => false
  // end of [lsteq]
//
  val () = $Rand.srand48_with_time () // a new seed is generated
//
  #define N 20
  #define N2 10
  #assert (N == 2 * N2)
  val xs = random_list_gen (N) // for testing [list_vt_tabulate]
  val () = () where {
    val () = print "xs (randomly generated) = "
    val () = lstpr (xs)
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    val () = print "testing [list_take] and [list_drop]: starts\n"
    val xs1 = l2l (list_take (xs, N2))
    val xs2 = list_drop (xs, N2)
    val xs12 = xs1 + xs2 // testing list_append
    val () = assert (lsteq (xs, xs12))
    val () = print "testing [list_take] and [list_drop]: finishes\n"
  } // end of [val]
//
  val () = () where {
    val () = print "testing [list_reverse]: starts\n"
    val () = assert (lsteq (xs, l2l (list_reverse (l2l (list_reverse xs)))))
    val () = print "testing [list_reverse]: finishes\n"
  } // end of [val]
//
  val () = () where {
    val () = print "testing [list_nth]: starts\n"
    val x1 = list_nth<int> (xs, N/3)
    val x2 = list_nth<int> (l2l (list_reverse<int> xs), N-N/3-1)
    val () = assert (x1 = x2)
    val () = print "testing [list_nth]: finishes\n"
  } // end of [val]
//
  val () = () where {
    val () = print "filter (xs, evn) = "
    // for testing [list_filter]
    val xs = list_filter_fun<int> (xs, lam x =<0> x mod 2 = 0)
    val xs = list_of_list_vt (xs)
    val () = lstpr (xs)
    val () = print_newline ()
  } // end of [val]
  val () = () where {
    val () = print "filter (xs, odd) = "
    // for testing [list_filter] again
    val xs = list_filter_fun<int> (xs, lam x =<0> x mod 2 > 0)
    val xs = list_of_list_vt (xs)
    val () = lstpr (xs)
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    val () = print "list_app: "
    var i: int = 0
    fn f (pf_i: !int @ i | x: int, p_i: !ptr i): void = () where {
      val ()  = if !p_i > 0 then print ", "
      val () = !p_i := !p_i + 1
      val () = print x
    } // end of [f]
    val () = list_app_funenv {int @ i} {ptr i} (view@ i | xs, f, &i)
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    val () = print "map (xs, double) = "
    // for testing [list_map]
    val () = lstpr (list_of_list_vt xs) where {
      val xs = list_map_fun<int><int> (xs, lam x =<0> 2 * x)
    }
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    val () = print "testing [list_head] and [list_last]: starts\n"
    val () = assert (list_head xs = list_last (l2l (list_reverse<int> xs)))
    val () = assert (list_last xs = list_head (l2l (list_reverse<int> xs)))
    val () = print "testing [list_head] and [list_last]: finishes\n"
  } // end of [val]
//
  val () = () where {
    val () = print "testing [list_mergesort] and [list_quicksort]: starts\n"
    val xs_ms = list_mergesort<int> {ptr} (xs, lam (x1, x2, env) =<0> compare (x1, x2), null)
    val xs_qs = list_quicksort<int> {ptr} (xs, lam (x1, x2, env) =<0> compare (x1, x2), null)
    val () = lstpr (xs_ms)
    val () = print_newline ()
    val () = lstpr (xs_qs)
    val () = print_newline ()
    val () = assert (lsteq (xs_ms, xs_qs))
    val () = print "testing [list_mergesort] and [list_quicksort]: finishes\n"
  } // end of [val]
//
in
  print "The run of [prelude_list.dats] is done successfully!\n"
end // end of [main]

(* ****** ****** *)

(* end of [prelude_list.dats] *)
