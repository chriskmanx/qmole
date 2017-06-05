//
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: August 2007
//
//
// This is an example of programming with theorem proving: A verified
// implmentation of the factioral function is given.
//

(* ****** ****** *)
//
// How to compile:
//   atscc -o fact3 fact3.dats -lgmp
// How to test:
//   ./fact3 100
//
(* ****** ****** *)

//
#include "prelude/HATS/lmacrodef.hats" // for prerrstarln
//

(* ****** ****** *)

staload "libats/SATS/intinf.sats"
dynload "libats/DATS/intinf.dats"

(* ****** ****** *)

// The following dataprop encodes a specification of the factorial function
dataprop FACT (int, int) =
  | FACTzero (0, 1)
  | {n,r,r1:int | n > 0} FACTsucc (n, r) of (FACT (n-1, r1), MUL (n, r1, r))
// end of [FACT]

fun fact3 {n:nat} .<n>. (n: int n): [r:int] (FACT (n, r) | intinfptr_gc r) =
  if n > 0 then let
    val n1 = pred n
    val (pf1 | (pf1_gc, pf1_at | p1)) = fact3 (n1)
    val (pf_mul | r) = n * !p1
    val () = intinfptr_free @(pf1_gc, pf1_at | p1)
  in
    (FACTsucc (pf1, pf_mul) | r)
  end else begin
    (FACTzero () | intinf_make 1)
  end // end of [if]
// end of [fact3]

(* ****** ****** *)

// [fn] declares a non-recursive function
// [@(...)] is used in ATS to group arguments for functions of variable arguments
fn fact3_usage (cmd: string): void =
  prerrstarln @("Usage: ", cmd, " [integer]") // print an error message

(* ****** ****** *)
//
// Is there still any doubt :)
//
implement
main (argc, argv) =
  if argc >= 2 then let
    val n0 = int1_of argv.[1] // turning string into integer
    val () = assert_errmsg
      (n0 >= 0, "The integer argument needs to be nonnegative.\n")
    val (pf | (pf_gc, pf_at | p_res)) = fact3 (n0)
    val () = begin
      print "The factorial of "; print n0; print " = "; print !p_res; print_newline ()
    end // end of [val]
  in
    intinfptr_free @(pf_gc, pf_at | p_res)
  end else begin
    fact3_usage (argv.[0]); exit (1)
  end // end of [if]
// end of [main]

(*

The factorial of 100 =
93326215443944152681699238856266700490715968264381\
62146859296389521759999322991560894146397615651828\
62536979208272237582511852109168640000000000000000\
00000000

*)

(* ****** ****** *)

(* end of [fact3.dats] *)
