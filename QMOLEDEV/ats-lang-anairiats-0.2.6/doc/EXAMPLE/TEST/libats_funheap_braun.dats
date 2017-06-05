(*

// some testing code for [libats/funheap_braun]

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010

*)

(* ****** ****** *)

staload H = "libats/SATS/funheap_braun.sats"
staload _(*anon*) = "libats/DATS/funheap_braun.dats"

typedef elt = int
typedef heap_t = $H.heap (elt)

(* ****** ****** *)

implement main (argc, argv) = () where {
  val () = gc_chunk_count_limit_max_set (~1) // infinite
  var n: int = 100 // default
  val () = begin
    if argc >= 2 then n := int_of_string (argv.[1])
  end // end of [va]
  val [n:int] n = int1_of_int n
  val () = assert (n > 0)
  val cmp = lam (x1: elt, x2: elt): Sgn =<cloref> compare_int_int (x1, x2)
  var heap: heap_t = $H.funheap_make_nil ()
  var i: Nat // uninitialized
  val () = for (i := n; i > 0; i := i-1) let
    val elt = i
    val () = $H.funheap_insert<elt> (heap, elt, cmp)
  in
    // nothing
  end // end of [val]
//
  val sz = $H.funheap_size (heap)
  val () = (print "funheap_size (heap) = "; print sz; print_newline ())
  val ht = $H.funheap_height (heap)
  val () = (print "funheap_size (heap) = "; print ht; print_newline ())
//
  val () = loop (sz, heap) where {
    val sz = size1_of_size (sz)
    fun loop {n:nat} .<n>. (
        sz: size_t n, heap: &heap_t
      ) :<cloref1> void = let
      var x: elt? // uninitialized
    in
      if sz > 0 then let
        val removed = $H.funheap_delmin (heap, x, cmp)
        val () = assert_errmsg (removed, #LOCATION)
        prval () = opt_unsome {elt} (x)
        // val () = (print x; print_newline ())
      in
        loop (sz-1, heap)
      end // end of [if]
    end // end of [loop]
  } // end of [loop]
} // end of [main]

(* ****** ****** *)

(* end of [libats_funheap_braun.dats] *)
