(*
//
// Author: Hongwei Xi (September, 2010)
//
*)

(* ****** ****** *)

staload V = "libats/SATS/vector.sats"
stadef VSHELL = $V.VSHELL
stadef VSHELL0 = $V.VSHELL0
stadef VECTOR = $V.VECTOR

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"
staload _(*anon*) = "libats/DATS/vector.dats"

(* ****** ****** *)

macdef size1 (x) = size1_of_int1 ,(x)

implement main () = () where {
  #define N 10
  #define M %(N+N)
  typedef T = double
  var V: VSHELL0 // unintialized
  val () = $V.vector_initialize<double> (V, N)
  val () = $V.vector_resize (V, M)
//
  val () = loop (V, 0) where {
    fun loop {m,n:int}
      {i:nat | i <= N; n+N <= m+i} .<N-i>. (
        V: &VECTOR (T, m, n) >> VECTOR (T, m, n+N-i), i: size_t i
      ) :<> void =
      if i < N then let
        val () = $V.vector_append (V, (double_of)i) in loop (V, i+1)
      end // end of [if]
    // end of [loop]
  } // end of [val]
//
  var i: natLte (N)
  val () = for*
    (V: VECTOR (T, M, N)) => (i := 0; i < N; i := i+1)
    (printf ("V[%i] = %.1f\n", @(i, $V.vector_get_elt_at (V, (size1)i))))
//
  val () = loop (V, 0) where {
    fun loop {m,n:int}
      {i:nat | i <= N; N <= n+i} .<N-i>. (
        V: &VECTOR (T, m, n) >> VECTOR (T, m, n-N+i), i: size_t i
      ) :<> void =
      if i < N then let
        var x: T // uninitialized
        val () = $V.vector_remove_at<T> (V, 0, x) in loop (V, i+1)
      end // end of [if]
    // end of [loop]
  } // end of [val]
//
  val () = loop (V, 0) where {
    fun loop {m,n:int}
      {i:nat | i <= N; n+N <= m+i} .<N-i>. (
        V: &VECTOR (T, m, n) >> VECTOR (T, m, n+N-i), i: size_t i
      ) :<> void =
      if i < N then let
        val () = $V.vector_prepend (V, (double_of)i) in loop (V, i+1)
      end // end of [if]
    // end of [loop]
  } // end of [val]
//
  viewdef V = unit_v
  prval pfu = unit_v
  val () = $V.vector_iforeach_vclo<T>
    {V} (pfu | V, !p_clo) where {
    var !p_clo = @lam (pf: !V | i: sizeLt N, x: &T)
      : void =<> $effmask_all let
        val i = int1_of_size1(i) in printf ("V[%i] = %.1f\n", @(i,x))
      end // end of [@lam]
  } // end of [val]
  prval unit_v () = pfu
//
  val () = $V.vector_uninitialize (V)
//
} // end of [main]

(* ****** ****** *)

(* end of [libats_vector.dats] *)
