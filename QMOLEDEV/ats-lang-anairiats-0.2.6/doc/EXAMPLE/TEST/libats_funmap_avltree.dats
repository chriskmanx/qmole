(*
** some testing code for functions declared in
** libats/SATS/funmap_avltree.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: March, 2010
//

(* ****** ****** *)

staload RAND = "libc/SATS/random.sats"

(* ****** ****** *)

staload M = "libats/SATS/funmap_avltree.sats"
staload _(*anon*) = "libats/DATS/funmap_avltree.dats"

(* ****** ****** *)

implement
$M.compare_key_key<int> (x1, x2, cmp) =
  if x1 < x2 then ~1 else if x1 > x2 then 1 else 0
// end of [compare_key_key]

(* ****** ****** *)

implement main (argc, argv) = let
  val () = gc_chunk_count_limit_max_set (~1) // infinite
  var n: int = 0
  val () = begin
    if argc >= 2 then n := int_of_string (argv.[1])
  end
  val [n:int] n = int1_of n
  val () = assert (n > 0)
  val () = $RAND.srand48 (0L)
(*
  val () = $RAND.srand48_with_time ()
*)
//
  typedef key = int and itm = string
  fn cmp (x1: key, x2: key):<cloref> Sgn = compare (x1, x2)
//
  var map: $M.map (key, itm) = $M.funmap_make_nil ()
  var i: int; val () = for (i := 0; i < n; i := i+1) let
    val key = i
    // val key = $RAND.randint n
    val itm = tostring key // val itm = sprintf ("%i", @(key))
    // val () = printf ("key = %i and itm = %s\n", @(key, itm))
    val _(*exists*) = $M.funmap_insert<int,string> (map, key, itm, cmp)
  in
    // nothing
  end // end [for]
//
  val size = $M.funmap_size (map)
  val () = begin
    print "size = "; print size; print_newline ()
  end // end of [size]
//
  val height = $M.funmap_height (map)
  val () = begin
    print "height = "; print height; print_newline ()
  end // end of [height]
//
  val () = if n < 100 then let
    prval pf = unit_v (); val () =
      $M.funmap_foreach_vclo<int,string> {unit_v} (pf | map, !p_clo) where {
      var !p_clo = @lam (pf: !unit_v | k: key, i: itm): void =<clo> $effmask_all
        (printf ("%i\t->\t%s\n", @(k, i)))
    } // end of [val]
    prval unit_v () = pf
  in
    // empty
  end // end of [val]
//
  var res: itm? // uninitialized
//
  fn find (
      map: $M.map (key, itm), k: int, res: &itm?
    ) : void = () where {
    val () = printf ("%i\t->\t", @(k))
    val b = $M.funmap_search (map, k, cmp, res)
    val () = if b then let
      prval () = opt_unsome {itm} (res)
    in
      print "Some("; print res; print ")"
    end else let
      prval () = opt_unnone {itm} (res) in print "None()"
    end // end of [val]
    val () = print_newline ()
  } // end of [find]
//
  val () = find (map, 0, res)
  val () = find (map, 1, res)
  val () = find (map, 10, res)
  val () = find (map, 100, res)
  val () = find (map, 1000, res)
  val () = find (map, 10000, res)
//
  var i: int; val () = for (i := 0; i < n; i := i+1) let
    val key = i
    val _(*removed*) = $M.funmap_remove<int,string> (map, key, cmp)
  in
    // nothing
  end // end [for]
//
  val size = $M.funmap_size (map)
  val () = begin
    print "size = "; print size; print_newline ()
  end // end of [size]
//
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [libats_funmap_avltree.dats] *)
