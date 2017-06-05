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
staload "libats/smlbas/SATS/list.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/list.dats"
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"
staload _(*anonymous*) = "prelude/DATS/list0.dats"

staload _(*anonymous*) = "libats/smlbas/DATS/list.dats"

(* ****** ****** *)

dynload "libats/smlbas/DATS/char.dats"
dynload "libats/smlbas/DATS/list.dats"

(* ****** ****** *)

implement main () = () where {
  val abs =
    list_of_list_vt (string_explode "abcdefghijklmnopqrstuvwxyz")
  // end of [val]  
  val abs = list0_of_list1 (abs)
//
  val () = app<char> (lam (c) => print c, abs)
  val () = print_newline ()
//
  val _(*int*) = foldl<char,int>
    (lam (c, i) => (if i > 0 then print ", "; print c; i + 1), 0, abs) 
  val () = print_newline ()
//
  val _(*int*) = foldr<char,int>
    (lam (c, i) => (if i > 0 then print ", "; print c; i + 1), 0, abs) 
  val () = print_newline ()
//
  val ABs = map<char><char> (lam x => toUpper x, abs)
  val () = app<char> (lam (c) => print c, ABs)
  val () = print_newline ()
//
  val () = app<char> (lam (c) => print c, append (abs, ABs))
  val () = print_newline ()
//
  val () = app<char> (lam (c) => print c, rev (abs))
  val () = print_newline ()
//
  val abs1 = tabulate
    (length abs, lam (i) => chr (ord 'a' + uint_of_int i))
  // end of [val]
  val () = app<char> (lam (c) => print c, abs1)
  val () = print_newline ()
//
  val () = app<char> (lam (c) => print c, take (ABs, 13))
  val () = print_newline ()
  val () = app<char> (lam (c) => print c, drop (ABs, 13))
  val () = print_newline ()
//
  val () = assert (all<char> (lam (x) => x \gte 'a', abs))
  val () = assert (~exists<char> (lam (x) => x \lt 'a', abs))
  val () = assert (all<char> (lam (x) => x \lte 'z', abs))
  val () = assert (~exists<char> (lam (x) => x \gt 'z', abs))
//
  macdef list0_sing (x) = list0_cons (,(x), list0_nil ())
  val abss = map<char><list0 char> (lam x => list0_sing x, abs) 
  val () = assert
    (collate<char> (lam (x, y) => compare (x, y), abs, abs1) = 0)
  // end of [val]
  val () = assert
    (collate<char> (lam (x, y) => compare (x, y), abs, concat abss) = 0)
  // end of [val]
} // end of [main]

(* ****** ****** *)

(* end of [libats_smlbas_list.dats] *)
