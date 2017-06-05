(*
// some testing code for functions declared in
// prelude/SATS/list_vt.sats
*)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Spring, 2009

(* ****** ****** *)

staload RAND = "libc/SATS/random.sats"
staload STDLIB = "libc/SATS/stdlib.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

#define MAXELT 100

(*
fun random_list_gen {n: nat}
  (n: int n): list_vt (natLt MAXELT, n) = loop (n, list_vt_nil) where {
  typedef T = natLt MAXELT
  fun loop {i,j:nat} .<i>.
    (i: int i, xs: list_vt (T, j)):<!ref> list_vt (T, i+j) =
    if i = 0 then xs else loop (i-1, list_vt_cons ($RAND.randint MAXELT, xs))
  // end of [loop]
} // end of [random]
*)

fun random_list_gen {n:nat}
  (n: int n): list_vt (natLt MAXELT, n) =
  list_vt_tabulate_fun (lam _ =<!ref> $RAND.randint MAXELT, n)
// end of [random_list_gen]

// end of [val]

(* ****** ****** *)

implement main (argc, argv) = let
  fn lstpr {n:nat}
    (xs: !list_vt (int, n)): void = () where {
    val xs = __cast (xs) where {
      extern castfn __cast (xs: !list_vt (int, n)): list (int, n)
    } // end of [val]
    val () = list_iforeach_fun (xs, f) where {
      fn f (i: int, x: int)
        : void = (if i > 0 then print (", "); print x)
      // end of [f]
    } // end of [val]
  } // end of [lstpr]
//
  val xs = list_vt_of_arraysize<int> $arrsz(1, 2, 3, 4, 5, 6, 7, 8, 9)
  val () = list_vt_free (xs)
//
  val () = $RAND.srand48_with_time () // a new seed is generated
  val xs = random_list_gen (10)
//
  val () = () where {
    val () = print "xs (randomly generated) = "
    val () = lstpr (xs)
    val () = print_newline ()
  } // end of [val]
//
  val ys = list_vt_of_arraysize<int> $arrsz(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
//
  val () = () where {
    val () = print "ys (0-9) = " // for testing [list_vt_of_arraysize]
    val () = lstpr (ys)
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    val () = print "length (xs) = "
    val () = print (list_vt_length xs) // for testing [list_vt_length]
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    val xs1 = list_vt_copy (xs)
    val () = print "copy (xs) = ";
    val () = lstpr (xs1) // for testing [list_vt_copy]
    val () = list_vt_free (xs1)
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    val xs1 = list_vt_copy (xs)
    val ys1 = list_vt_copy (ys)
    val xs1ys1 = list_vt_append (xs1, ys1)
    val () = print "append (xs, ys) = ";
    val () = lstpr (xs1ys1) // for testing [list_vt_append]
    val () = list_vt_free (xs1ys1)
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    val xs1 = list_vt_copy (xs)
    val xs1 = list_vt_reverse (xs1)
    val () = print "reverse (xs) = ";
    val () = lstpr (xs1) // for testing [list_vt_reverse]
    val () = list_vt_free (xs1)
    val () = print_newline ()
  } // end of [val]
//
  val () = () where {
    // [list_vt_quicksort] is implemented on top of [qsort] in stdlib.h
    val () = list_vt_quicksort<int> (xs, cmp) where {
      fn cmp (x1: &int, x2: &int):<> Sgn = compare (x1, x2)
    }
    val () = print "list_vt_quicksort (xs) = "
    val () = lstpr (xs)
    val () = print_newline ()
  } // end of [val]
//
  val () = list_vt_free (xs)
  val () = list_vt_free (ys)
in
  print "The run of [prelude_list.dats] is done successfully!\n"
end // end of [main]

(* ****** ****** *)

(* end of [prelude_list_vt.dats] *)
