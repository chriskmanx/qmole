//
// K&R, 2nd edition, page 87
//

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

extern fun printd (n: int): void

implement printd (n) = let
  var n: int = n
  val () = if (n < 0) then begin
    let val _ = putchar '-' in n := ~n end
  end // end of [val]
  val () = if (n / 10 > 0) then printd (n / 10)
  val _ = putchar (char_of_int (n mod 10 + int_of_char '0'))
in
  // empty
end // end of [printd]

(* ****** ****** *)

implement main (argc, argv) = let
  val () = assert (argc >= 2)
  val n = int_of_string (argv.[1])
in
  printd (n); print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [printd.dats] *)
