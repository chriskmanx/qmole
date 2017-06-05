(*
** some testing code for functions declared in
** libats/SATS/regexp.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

staload "prelude/DATS/list_vt.dats"

(* ****** ****** *)

staload "libats/SATS/regexp.sats"

(* ****** ****** *)

fn prerr_usage (cmd: string): void =
  prerrf ("Usage: %s <string>\n", @(cmd))
// end of [prerr_usage]

(* ****** ****** *)

dynload "libats/DATS/regexp.dats"

(* ****** ****** *)

fun print_strposlst
  {n:nat} (xs: !strposlst n): void =
  case+ xs of
  | list_vt_cons (pp, !p_xs) => {
      val () = printf ("%i-%i\n", @(pp.0, pp.1))
      val () = print_strposlst {n} (!p_xs)
      prval () = fold@ (xs)
    } // end of [list_vt_cons]
  | _ => ()
// end of [print_strposlst]

(* ****** ****** *)

implement
main (argc, argv) = let
  stavar n: int
  val intstr = "0123456789": string (n)
  val intpat = "^([1-9])([0-9]*)$"
  val re = regexp_compile_exn intpat
//
  val ans = regexp_match_string (re, intstr)
  val () = if ans then begin
    printf ("The string [%s] represents a valid integer.\n", @(intstr))
  end else begin
    printf ("The string [%s] does not represent a valid integer.\n", @(intstr))  
  end // end of [if]
//
  val ans = regexp_match_substring_strposlst (re, intstr, 1, 9)
  val () = print_strposlst {n} (ans)
//
  val () = list_vt_free (ans)
//
  val () = regexp_free (re)
//
in
  // nothing
end (* end of [main] *)

(* ****** ****** *)

(* end of [libats_regexp.dats] *)
