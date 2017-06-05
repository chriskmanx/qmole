(*
**
** An implementation of functional arrays based on Braun trees.
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: October, 2008
**
*)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt
//

(* ****** ****** *)

// How to compile:
//   atscc -o test test.dats funarray.dats

(* ****** ****** *)

staload FA = "libats/SATS/funarray_braun.sats"
staload _(*anon*) = "libats/DATS/funarray_braun.dats"

(* ****** ****** *)

typedef farrstr (n:int) = $FA.array (string, n)

implement main (argc, argv) = let
  fn prarr {n:nat} (A: farrstr n, n: int n): void = let
    val () = $FA.funarray_iforeach_cloref (A, n,
      lam (i, x) =<cloref> $effmask_all (printf ("%i\t |->\t %s\n", @(i, x)))
    )
  in
    // empty
  end // end of [prarr]
//
  var A = $FA.funarray_make_nil {string} ()
//
  val () = loop (argc, argv, 0, A) where {
    fun loop {n,i:nat | i <= n} (
        n: int n, ss: &(@[string][n]), i: int i, A: &farrstr i >> farrstr n
      ) : void =
      if i < n then let
        val () = $FA.funarray_hiadd (A, i, ss.[i]) in loop (n, ss, i+1, A)
      end else ()
    // end of [loop]
  } // end of [val]
  val () = prarr (A, argc)
//
  val () = loop (argc, 0, A) where {
    fun loop {n,i:nat | i <= n}
      (n: int n, i: int i, A: &farrstr (n-i) >> farrstr 0): void =
      if i < n then let
        val () = $FA.funarray_lorem A in loop (n, i+1, A)
      end else ()
    // end of [loop]
  } // end of [val]
//
  val () = loop (argc, argv, 0, A) where {
    fun loop {n,i:nat | i <= n}
      (n: int n, ss: &(@[string][n]), i: int i, A: &farrstr i >> farrstr n)
      : void = begin
      if i < n then let
        val () = $FA.funarray_loadd (A, ss.[i]) in loop (n, ss, i+1, A)
      end else ()
    end // end of [loop]
  } // end of [val]
  val () = prarr (A, argc)
in
 // empty
end // end of [main]

(* ****** ****** *)

(* end of [libats_funarray_braun.dats] *)
