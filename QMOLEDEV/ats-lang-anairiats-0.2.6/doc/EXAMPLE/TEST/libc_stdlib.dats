(*
** some testing code for functions declared in
** libc/SATS/stdlib.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2009
//

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload F = "libc/SATS/fcntl.sats"
staload R = "libc/SATS/random.sats"
staload STR = "libc/SATS/string.sats"
staload UNI = "libc/SATS/unistd.sats"

(* ****** ****** *)

staload "libc/SATS/stdlib.sats"

(* ****** ****** *)

fun{a:t@ype}
print_array {n:nat} (
  A: &(@[a][n]), n: size_t n, pr: (a) -> void
) : void = () where {
  var i: sizeLte n
  val _0 = size1_of_int1 (0)
  val () = for (i := _0; i < n; i := i+1) let
    val () = if i > 0 then print ", "; val () = pr (A.[i])
  in
    // nothing
  end // end of [val]
} // end of [print_array]

(* ****** ****** *)

implement
main (argc, argv) = let
//
  val (fpf_x | _x) = getenv ("ATSHOME0")
  val () = (print "${ATSHOME0} = "; print _x; print_newline ())
  prval () = fpf_x (_x)
  val (fpf_nameval | nameval) = (string_takeout_ptr)"ATSHOME0=ATSHOME0"
  val _err = putenv (nameval)
  prval () = fpf_nameval (nameval)
  val (fpf_x | _x) = getenv ("ATSHOME0")
  val () = (print "${ATSHOME0} = "; print _x; print_newline ())
  prval () = fpf_x (_x)
//
  val tmp = $STR.strdup_gc ("foo-XXXXXX")
  prval pfstr = tmp.1
  val (pfopt | i) = mkstemp !(tmp.2) // create it!
  val () = assertloc (i > 0)
  val () = printf ("mkstemp: %s\n", @($UN.cast (tmp.2)))
  prval () = tmp.1 := pfstr
  prval $F.open_v_succ (pffil) = pfopt
  val () = $F.close_exn (pffil | i)
  val () = assertloc ($UNI.unlink ($UN.cast (tmp.2)) = 0) // remove it!
  val () = strbufptr_free (tmp)
//
  val tmp = $STR.strdup_gc ("bar-XXXXXX")
  val p = mkdtemp (tmp.1 | tmp.2) // create it!
  val () = assertloc (p > null)
  val () = printf ("mkdtemp: %s\n", @($UN.cast (tmp.2)))
  val () = assertloc ($UNI.rmdir ($UN.cast (tmp.2)) = 0) // remove it!
  val () = strbufptr_free (tmp)
//
  val () = $R.srand48_with_time ()
//
  #define ASZ 10
  var !p_arr = @[double][ASZ](0.0)
//
  var i: natLte ASZ
//
  val () = for
    (i := 0; i < ASZ; i := i+1) let
    val () = p_arr->[i] := $R.drand48 ()
  in
    // nothing
  end // end of [val]
//
  val () = print_array<double> (!p_arr, ASZ, lam x => printf ("%.2f", @(x)))
  val () = print_newline ()
  val () = qsort {double} (!p_arr, ASZ, sizeof<double>, lam (x1, x2) => compare (x1, x2))
  val () = print_array<double> (!p_arr, ASZ, lam x => printf ("%.2f", @(x)))
  val () = print_newline ()
//
  val _err = atexit (lam () => printf ("Bye, bye!\n", @()))
  val () = assert_errmsg (_err = 0, #LOCATION)
//
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [libc_stdlib.dats] *)
