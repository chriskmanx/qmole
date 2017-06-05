(*
** some testing code for functions declared in
** libats/smlbas/SATS/array.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

staload "libats/smlbas/SATS/char.sats"

(* ****** ****** *)

(*
fun ord (c: char): uint
fun chr (i: uint): char
fun succ (c: char): char
fun pred (c: char): char
*)

fn test__ord (): void = () where {
  val () = assert_errmsg (ord '0' = 0x30U, #LOCATION)
  val () = assert_errmsg (ord 'a' = uint_of_char 'a', #LOCATION)
  val () = assert_errmsg (ord 'A' = uint_of_char 'A', #LOCATION)
} // end of [test__ord]

fn test__chr (): void = () where {
  val () = assert_errmsg (chr 0x30U = '0', #LOCATION)
  val () = assert_errmsg (chr (uint_of_char 'a') = 'a', #LOCATION)
  val () = assert_errmsg (chr (uint_of_char 'A') = 'A', #LOCATION)
} // end of [test__chr]

fn test__succ (): void = () where {
  val () = assert_errmsg (succ '0' = '1', #LOCATION)
  val () = assert_errmsg (succ 'a' = 'b', #LOCATION)
  val () = assert_errmsg (succ 'A' = 'B', #LOCATION)
} // end of [test__succ]

fn test__pred (): void = () where {
  val () = assert_errmsg (pred '1' = '0', #LOCATION)
  val () = assert_errmsg (pred 'b' = 'a', #LOCATION)
  val () = assert_errmsg (pred 'B' = 'A', #LOCATION)
} // end of [test__pred]

(* ****** ****** *)

(*
fun contains (s: string, c: char): bool
fun notContains (s: string, c: char): bool
*)

fn test__contains (): void = () where {
  val () = assert_errmsg (contains ("abcdefghijklmnopqrstuvwxyz", 'a'), #LOCATION)
  val () = assert_errmsg (contains ("abcdefghijklmnopqrstuvwxyz", 'm'), #LOCATION)
  val () = assert_errmsg (contains ("abcdefghijklmnopqrstuvwxyz", 'n'), #LOCATION)
  val () = assert_errmsg (contains ("abcdefghijklmnopqrstuvwxyz", 'z'), #LOCATION)
} // end of [test__contains]

fn test__notContains (): void = () where {
  val () = assert_errmsg (notContains ("abcdefghijklmnopqrstuvwxyz", 'A'), #LOCATION)
  val () = assert_errmsg (notContains ("abcdefghijklmnopqrstuvwxyz", 'M'), #LOCATION)
  val () = assert_errmsg (notContains ("abcdefghijklmnopqrstuvwxyz", 'N'), #LOCATION)
  val () = assert_errmsg (notContains ("abcdefghijklmnopqrstuvwxyz", 'Z'), #LOCATION)
} // end of [test__notContains]

(* ****** ****** *)

fn test__toLower (): void = () where {
  val () = assert_errmsg (toLower '0' = '0', #LOCATION)
  val () = assert_errmsg (toLower 'A' = 'a', #LOCATION)
} // end of [test__toLower]

fn test__toUpper (): void = () where {
  val () = assert_errmsg (toUpper '0' = '0', #LOCATION)
  val () = assert_errmsg (toUpper 'a' = 'A', #LOCATION)
} // end of [test__toUpper]

(* ****** ****** *)

fn test__toCString (): void = () where {
  val () = assert_errmsg (toCString '\n' = "\\n", #LOCATION)
  val () = assert_errmsg (toCString '\101' = "A", #LOCATION) 
  val () = assert_errmsg (toCString '\177' = "\\177", #LOCATION) 
} // end of [test__toCString]

(* ****** ****** *)

(*
fun fromCString (s: string): option0 char
*)
fn test__fromCString () = () where {
(*
   val oc = fromCString "A"
   val- option0_some c = oc
   val () = assert_errmsg (c = 'A', #LOCATION)
   val oc = fromCString "\\n"
   val- option0_some c = oc
   val () = assert_errmsg (c = '\n', #LOCATION)
   val oc = fromCString "AB"
   val- option0_none () = oc
*)
} // end of [test__fromCString]

(* ****** ****** *)

dynload "libats/smlbas/DATS/char.dats"

(* ****** ****** *)

implement main () = () where {
  val () = test__ord ()
  val () = test__chr ()
  val () = test__succ ()
  val () = test__pred ()
//
  val () = test__contains ()
  val () = test__notContains ()
//
  val () = test__toLower ()
  val () = test__toUpper ()
//
  val () = test__toCString ()
  val () = test__fromCString ()
//
  val () = print "[libats_smlbas_char.dats] testing passes!\n"
} // end of [main]

(* ****** ****** *)

(* end of [libats_smlbas_array.dats] *)
