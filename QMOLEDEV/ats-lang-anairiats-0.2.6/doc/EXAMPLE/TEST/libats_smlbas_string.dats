(*
** some testing code for functions declared in
** libats/smlbas/SATS/array.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

staload Char = "libats/smlbas/SATS/char.sats"
staload List = "libats/smlbas/SATS/list.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/string.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"
staload _(*anonymous*) = "prelude/DATS/list0.dats"

staload _(*anonymous*) = "libats/smlbas/DATS/list.dats"

(* ****** ****** *)

dynload "libats/smlbas/DATS/char.dats"
dynload "libats/smlbas/DATS/list.dats"
dynload "libats/smlbas/DATS/string.dats"

(* ****** ****** *)

implement main () = () where {
  val abs = "abcdefghijklmnopqrstuvwxyz"
//
  val () = assert (isPrefix ("", "abcde"))
  val () = assert (isPrefix ("abcde", "abcde"))
  val () = assert (~isPrefix ("abcdef", "abcde"))
  val () = assert (~isPrefix ("abcdeg", "abcdefg"))
  val () = assert (isSubstring ("", "abcde"))
  val () = assert (isSubstring ("abcde", "abcde"))
  val () = assert (isSubstring ("bcd", "abcde"))
  val () = assert (~isSubstring ("bcdefgh", "abcdefg"))
  val () = assert (isSuffix ("", "abcde"))
  val () = assert (isSuffix ("abcde", "abcde"))
  val () = assert (isSuffix ("cde", "abcde"))
  val () = assert (~isSuffix ("def", "abcde"))
//
  val ABs = map (lam c => $Char.toUpper c, abs)
  val () = print (ABs)
  val () = print_newline ()
  val abs = map (lam c => $Char.toLower c, ABs)
  val () = print (abs)
  val () = print_newline ()
//
  val ABs = translate (lam c => str ($Char.succ c), ABs)
  val () = print (ABs)
  val () = print_newline ()
//
  val words = tokens (lam (c) => c = '|', "|abc||efgh||ijk|")
  val () = assert (list0_length words = 3)
  val () = $List.app<string> (lam s => (print "tokens="; print s; print_newline ()), words)
  val words = fields (lam (c) => c = '|', "|abc||efgh||ijk|")
  val () = assert (list0_length words = 7)
  val () = $List.app<string> (lam s => (print "field="; print s; print_newline ()), words)
//
  val abs = explode abs
  val abss = $List.map<char><string> (lam c => str c, abs) 
  val abs_sep = concatWith (sep, abss) where { val sep = ", " }
  val () = print (abs_sep)
  val () = print_newline ()
//  
} // end of [main]

(* ****** ****** *)

(* end of [libats_smlbas_string.dats] *)
