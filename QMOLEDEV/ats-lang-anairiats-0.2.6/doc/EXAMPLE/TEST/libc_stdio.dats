(*
** some testing code for functions declared in
** libc/SATS/stdio.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2010
//

(* ****** ****** *)

staload "libc/SATS/unistd.sats"

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

(* ****** ****** *)

#define c2i int_of_char
#define i2c char_of_int
#define _1M 1000000

implement
main (argc, argv) = let
//
   val _err =
     setvbuf_null (stdout_ref, _IOLBF)
   // end of [val]
//
   var i: int
   val () = for
     (i := 0; i < 26; i := i+1) let
     val c = i2c(c2i('A')+i); val () = print c
     val _err = usleep (_1M / 26)
   in
     // nothing
   end // end of [val]
   val () = print_newline ()
//
   val () = setbuf_null (stdout_ref)
//
   val () = for
     (i := 0; i < 26; i := i+1) let
     val c = i2c(c2i('A')+i); val () = print c
     val _err = usleep (_1M / 26)
   in
     // nothing
   end // end of [val]
   val () = print_newline ()
//
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [libc_stdio.dats] *)
