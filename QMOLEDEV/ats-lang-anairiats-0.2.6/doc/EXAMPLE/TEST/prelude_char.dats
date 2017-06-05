(*
** some testing code for functions declared in
** prelude/SATS/char.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: August, 2010
//

(* ****** ****** *)

implement
main () = let
  val () = assert (char_isalpha 'a')
  val () = assert (~char_isalpha '0')
//
  val () = assert (char_isalnum 'a')
  val () = assert (char_isalnum '0')
  val () = assert (~char_isalnum '\n')
//
  val () = assert (char_isprint 'a')
  val () = assert (~char_isprint '\001')
//
  val () = assert (char_isnull '\000')
  val () = assert (~char_isnull '\001')
//
  val () = assert (char_ispunct ',')
  val () = assert (char_ispunct '.')
  val () = assert (char_ispunct ':')
  val () = assert (char_ispunct '?')
  val () = assert (~char_ispunct 'a')
  val () = assert (~char_ispunct 'A')
//
  val () = assert (char_isspace ' ')
  val () = assert (char_isspace '\n')
  val () = assert (char_isspace '\t')
  val () = assert (~char_isspace '.')
//
  val () = assert (char_isdigit '0')
  val () = assert (~char_isdigit 'a')
//
  val () = assert (char_isxdigit 'a')
  val () = assert (char_isxdigit 'A')
  val () = assert (char_isxdigit '0')
  val () = assert (~char_isxdigit 'X')
//
  val () = assert (char_islower 'a')
  val () = assert (~char_islower 'A')
  val () = assert (~char_islower '0')
//
  val () = assert (char_isupper 'A')
  val () = assert (~char_isupper 'a')
  val () = assert (~char_isupper '0')
//
  val () = assert ('a' < 'z')
  val () = assert ('a' <= 'a')
  val () = assert ('A' <= 'a')
//
  val () = assert ('z' > 'a')
  val () = assert ('a' >= 'a')
  val () = assert ('a' >= 'A')
//
  val () = assert ('a' = 'a')
  val () = assert ('a' <> 'b')
//
  val () = assert ('9' - '0' = 9)
  val () = assert ('z' - 'a' = 25)
  val () = assert ('Z' - 'A' = 25)
//
in
  print "[prelude_char.dats] testing passes!\n"
end // end of [main]

(* ****** ****** *)

(* end of [prelude_char.dats] *)
