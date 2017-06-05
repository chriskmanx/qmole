//
// K&R, 2nd edition, page 20
//

//
// Translated into ATS by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//

(*

#include <stdio.h>

#define IN  1 /* inside a word */
#define OUT 0 /* outside a word */

/* count lines, words, and chars in input */
int main () {
  int c, nl, nw, nc, state ;
  state = OUT ;
  nl = nw = nc = 0 ;
  while ((c = getchar()) != EOF) {
    ++nc ;
    if (c == '\n') ++nl ;
    if (c == ' ' || c == '\n' || c == '\t')
      state = OUT ;
    else if (state == OUT) {
      state = IN ; ++nw ;
    }
  } // end of [while]
  printf ("%d %d %d\n", nl, nw, nc) ;
} /* end of [main] */

*)

staload "libc/SATS/stdio.sats"

#define IN 0; #define OUT 1

implement main () = let
  fun loop (nl: &int, nw: &int, nc: &int, state: &int): void = let
    val c = getchar ()
  in
    if (c <> EOF) then let
      val () = nc := nc + 1
      val c = char_of_int (c)
      val () = if (c = '\n') then (nl := nl + 1)
      val () = case+ c of
        | ' ' => state := OUT
        | '\n' => state := OUT
        | '\t' => state := OUT
        | _ => begin
            if (state = OUT) then (state := IN; nw := nw + 1)
          end
    in
      loop (nl, nw, nc, state)
    end else begin
      // loop exits
    end // end of [if]
  end // end of [loop]
  var nl: int = 0 and nw: int = 0 and nc: int = 0 and state: int = OUT
  val () = loop (nl, nw, nc, state)
in
  printf ("%d %d %d\n", @(nl, nw, nc))
end // end of [main]

(* ****** ****** *)

(* end of [wc.dats] *)
