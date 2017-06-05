(*
** some testing code for functions declared in
** libats/SATS/intinf.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Spring, 2009
//

(* ****** ****** *)

staload "libats/SATS/intinf.sats"

(* ****** ****** *)

fun fact (x: int): Intinfptr_gc =
  loop (pf_gc, pf_at | x, res) where {
  val x = int1_of_int x
  val (pf_gc, pf_at | res) = intinf_make (1)
  fun loop {i:int} {res:addr} (
      pf_gc: free_gc_v (intinf0?, res)
    , pf_at: Intinf @ res
    | x: int i, res: ptr res
    ) : Intinfptr_gc =
    if x > 0 then let
      val (pf_mul | (pf1_gc, pf1_at | res1)) = !res * x
      val () = intinfptr_free @(pf_gc, pf_at | res)
    in
      loop (pf1_gc, pf1_at | x - 1, res1)
    end else begin
      #[.. | #[.. | (pf_gc, pf_at | res)]] // loop exits
    end // end of [if]
  // end of [loop]
} // end of [fact]

fn prerr_usage (cmd: string): void =
  prerrf ("Usage: %s <integer>\n", @(cmd))
// end of [prerr_usage]

(* ****** ****** *)

dynload "libats/DATS/intinf.dats"

implement main (argc, argv) = let
  val () = if (argc <> 2) then prerr_usage (argv.[0])
  val () = assert (argc = 2)
  val n = int1_of_string (argv.[1])
  val (pf_gc, pf_at | res) = fact (n)
  val () = begin
    printf ("fact (%i) = ", @(n)); print !res; print_newline ()
  end // end of [val]
  val () = intinfptr_free @(pf_gc, pf_at | res)
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [libats_intinf.dats] *)
