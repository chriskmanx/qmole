(*
** This example is taken from Appel's book:
** Modern Compiler Design and Implementation in ML
*)

(* A program to solve the 8-queens problem *)

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

#define N 8
#define N1 (N - 1)

(* ****** ****** *)

var NSOL: Nat = 0
val (pfbox_NSOL | ()) =
  vbox_make_view_ptr {Nat} (view@ NSOL | &NSOL)
// end of [val]

(* ****** ****** *)

val row = array_make_elt<int> (N, 0)
val col = array_make_elt<int> (N, 0)
val diag1 = array_make_elt<int> (size1_of_int1 N+N1, 0)
val diag2 = array_make_elt<int> (size1_of_int1 N+N1, 0)

(* ****** ****** *)

%{^

#include <stdio.h>

// for efficiency
ats_void_type
  print_string (ats_ptr_type s) {
  fputs ((char*)s, stdout) ; return ;
}

%} // end of [%{^]

extern fun print_string (str: string): void = "print_string"

fn printboard (): void = let
  var i: natLte N and j: natLte N
  val () = for* (j: int?) =>
    (i := 0; i < N; i := i + 1) let
    val () = for* (i: natLt N) =>
      (j := 0; j < N; j := j + 1) begin print_string
      (if :(j: natLt N) => (col[i] = j) then " Q" else " .")
    end // end of [val]
  in
    print_string ("\n")
  end // end of [for]
  val () = print_string ("\n")
in
  // empty
end (* end of [printboard] *)

(* ****** ****** *)

fun _try (c: natLte N): void =
  if (c = N) then let
    val () = let
      prval vbox pf = pfbox_NSOL in NSOL := NSOL + 1
    end // end of [val]
  in
    printboard ()
  end else let
    var r: natLte N // unitialized
    val () = for (r := 0; r < N; r := r+1) (
      if :(r: natLt N) => (row[r] = 0) then begin
        if :(r: natLt N) => (diag1[r+c] = 0) then begin
          if :(r: natLt N) => (diag2[r+N1-c] = 0) then begin
            row[r] := 1; diag1[r+c] := 1; diag2[r+N1-c] := 1;
            col[c] := r; _try (c+1);
            row[r] := 0; diag1[r+c] := 0; diag2[r+N1-c] := 0;
          end (* end of [if] *)
        end (* end of [if] *)
      end (* end of [if] *)
    ) // end of [val]
  in
    // empty
  end // end of [if]
// end of [_try]

(* ****** ****** *)
	
implement main () = let
  val () = _try (0)
  val n = NSOL where {
    prval vbox pf = pfbox_NSOL
  }
in
  printf ("The total number of solutions is [%i].\n",@(n))
end // end of [main]

(* ****** ****** *)

(* end of [queens_appel.dats] *)
