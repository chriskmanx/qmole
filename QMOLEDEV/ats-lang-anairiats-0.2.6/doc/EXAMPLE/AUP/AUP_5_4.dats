//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
(* ****** ****** *)
//
// book: AUP (2nd edition), pages 292 - 296
// section 5.4: Implementing a Shell (Version 1)
//
(* ****** ****** *)

staload "libc/SATS/stdlib.sats" // for getenv
staload "libc/SATS/unistd.sats" // for environ_get_arrsz

(* ****** ****** *)

typedef ptrarr0 (n:int) = @[ptr?][n]

dataview
getargs_v (n0:int, l:addr, int) =
  | {n:nat | n < n0}
    getargs_v_succ (n0, l, n) of (
      ptrarr (n) @ l, ptrarr (n) @ l -<lin,prf> ptrarr0 (n0) @ l
    ) // end of [getargs_v_succ]
  | getargs_v_fail (n0, l, ~1) of (ptrarr0 (n0) @ l)
// end of [getargs_v]

extern
fun getargs {n0:nat} {l:addr} (
  pfargv: ptrarr0 (n0) @ l | pargv: ptr l, n0: int n0, iseof: &bool? >> bool
) : [n:int] (getargs_v (n0, l, n) | int n) = "mac#getargs"
// end of [getargs]

(* ****** ****** *)

fun printenv {n:pos} (
  argc: int n, argv: &(@[string][n])
) : void = let
  var m: size_t // uninitialized
  val (pf, fpf | p) = environ_get_arrsz (m)
  stavar m: int
  val m: size_t m = m
  var i: sizeLte m
  val _0 = size1_of_int1 (0)
  val () = for
    (i := _0; i < m; i := i+1) (printf("%s\n", @(!p.[i])))
  // end of [val]
  prval () = fpf (pf)
in
  // nothing
end // end of [printenv]

(* ****** ****** *)

fun assgnenv {n:pos}
  (argc: int n, argv: &(@[string][n])): void = let
  var nerr: int = 0
  val () = while (true) let
    val () = (
      if argc <= 2 then (nerr := 1; break; assertfalse())
    ) : [n >= 3] void
    val () = if (setenv (argv.[1], argv.[2], 1(*overwritten*)) < 0) then
      nerr := 2
    // end of [val]
  in
    break
  end // end of [val]
  val () = (case+ nerr of
    | 1 => printf ("Incorrect command format for <assgn>\n", @())
    | 2 => printf ("[setenv] failed\n", @())
    | _ => ()
  ) : void // end of [val]
in
  // nothing
end // end of [assgnenv]

(* ****** ****** *)

%{^
void execute (
  int argc, char *argv[]
) {
  execvp (argv[0], &argv[0]) ;
  printf ("Can't execute\n") ;
  return ;
}
%} // end of [%{^]
extern
fun execute {n:pos}
  (argc: int n, argv: &(@[string][n])): void = "mac#execute"
// end of [execute]

implement
main () =
while (true) let
  #define MAXARG 32
  var !pargv with pfargv = @[ptr?][MAXARG]()
  val () = printf ("@ ", @())
  var iseof: bool // uninitialized
  val (pfargs | argc) = getargs (pfargv | pargv, MAXARG, iseof)
//
  val () = if argc >= 0 then let
    prval getargs_v_succ (pf, fpf) = pfargs
    prval (pf1, fpf1) = ptrarr_takeout{string} (pf)
    val () = if (argc > 0) then let
      val arg0 = pargv->[0] in case+ 0 of
      | _ when arg0 = "print" => printenv (argc, !pargv)
      | _ when arg0 = "assgn" => assgnenv (argc, !pargv)
      | _ => execute (argc, !pargv)
    end // end of [val]
    prval () = pf := fpf1 (pf1)
    prval () = pfargv := fpf (pf)
  in
    // nothing
  end else let
    prval getargs_v_fail (pf) = pfargs
    prval () = pfargv := pf
  in
    // nothing
  end // end of [val]
//
in
  if iseof then exit (EXIT_SUCCESS)
end // end of [main]

(* ****** ****** *)

(* end of [AUP_5_4.dats] *)
