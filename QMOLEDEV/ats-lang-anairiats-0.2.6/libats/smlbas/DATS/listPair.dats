(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: ListPair (http://www.standardml.org/Basis/list-pair.html)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // there is no need for dynamic loading

(* ****** ****** *)

staload "libats/smlbas/SATS/general.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/listPair.sats"

(* ****** ****** *)

implement{a,b}
zip (xs, ys) = res where {
  viewtypedef res_t = List_vt @(a, b)
  fun loop {m,n:nat} .<m>. (
      xs: list (a, m), ys: list (b, n), res: &res_t? >> res_t
    ) :<> void = case+ (xs, ys) of
    | (list_cons (x, xs), list_cons (y, ys)) => let
        val () = res := list_vt_cons {..} {0} (@(x, y),  ?)
        val+ list_vt_cons (_, !p_res1) = res
      in
        loop (xs, ys, !p_res1); fold@ res
      end // end of [list_cons, list_cons]
    | (_, _) =>> (res := list_vt_nil ())
  var res: res_t
  val xs = list1_of_list0 (xs) and ys = list1_of_list0 (ys)
  val () = loop (xs, ys, res)
  val res = list0_of_list_vt (res)
} // end of [zip]

implement{a,b}
zipEq (xs, ys) = let
  viewtypedef res_t = List_vt @(a, b)
  fun loop {m,n:nat} .<m>. (
      xs: list (a, m), ys: list (b, n), res: &res_t? >> res_t, err: &int
    ) :<> void = case+ (xs, ys) of
    | (list_cons (x, xs), list_cons (y, ys)) => let
        val () = res := list_vt_cons {..} {0} (@(x, y),  ?)
        val+ list_vt_cons (_, !p_res1) = res
      in
        loop (xs, ys, !p_res1, err); fold@ res
      end // end of [list_cons, list_cons]
    | (list_nil _, list_nil _) => (res := list_vt_nil ())
    | (_, _) =>> (err := 1; res := list_vt_nil ())
  // end of [loop]  
  var res: res_t?; var err: int = 0 
  val xs = list1_of_list0 (xs) and ys = list1_of_list0 (ys)
  val () = loop (xs, ys, res, err)
in
  if err = 0 then
    list0_of_list_vt (res)
  else begin
    list_vt_free (res); $raise UnequalLengths ()
  end // end of [if]  
end (* end of [zipEq] *)

(* ****** ****** *)

implement{a,b}
unzip (xys) = (xs, ys) where {
  val xys = list1_of_list0 (xys)
  val (xs, ys) = list_unzip<a,b> (xys)
  val xs = list0_of_list_vt xs and ys = list0_of_list_vt ys
} (* end of [unzip] *)

(* ****** ****** *)

local

fun{a,b:t@ype}
loop (
  f: (a, b) -<cloref1> void, xs: list0 a, ys: list0 b
) : int(*err*) =
  case+ (xs, ys) of
  | (list0_cons (x, xs), list0_cons (y, ys)) => (f (x, y); loop (f, xs, ys))
  | (list0_nil (), list0_nil ()) => 0
  | (_, _) => 1
// end of [loop]

in // in of [local]

implement{a,b}
app (f, xs, ys) =
  let val _(*int*) = loop<a,b> (f, xs, ys) in () end
// end of [app]

implement{a,b}
appEq (f, xs, ys) = let
  val err = loop<a,b> (f, xs, ys) in
  if err > 0 then $raise UnequalLengths () else ()
end // end of [zipEq]

end // end of [local]

(* ****** ****** *)

local

fun{a,b:t@ype}{c:t@ype}
loop (
  f: (a, b) -<cloref1> c
, xs: list0 a, ys: list0 b, res: &List_vt c? >> List_vt c
, err: &int
) : void =
  case+ (xs, ys) of
  | (list0_cons (x, xs), list0_cons (y, ys)) => let
      val () = res := list_vt_cons {..} {0} (f (x, y), ?); val+ list_vt_cons (_, !p_res1) = res
    in
      loop (f, xs, ys, !p_res1, err); fold@ res
    end // end of [list0_cons, list0_cons]
  | (list0_nil (), list0_nil ()) => res := list_vt_nil ()
  | (_, _) => (err := 1; res := list_vt_nil ())
// end of [app_err]

in // in of [local]

implement{a,b}{c}
map (f, xs, ys) = let
  var res: List_vt c?; var err: int = 0
  val () = loop<a,b><c> (f, xs, ys, res, err)
in
  list0_of_list_vt (res)
end (* end of [mapEq] *)

implement{a,b}{c}
mapEq (f, xs, ys) = let
  var res: List_vt c?; var err: int = 0
  val () = loop<a,b><c> (f, xs, ys, res, err)
in
  if err = 0 then 
    list0_of_list_vt (res)
  else begin
    list_vt_free (res); $raise UnequalLengths ()
  end // end of [if]  
end (* end of [mapEq] *)

end // end of [local]

(* ****** ****** *)

implement{a,b}{c}
foldl (f, ini, xs, ys) = loop (ini, xs, ys) where {
  fun loop (ini: c, xs: list0 a, ys: list0 b): c = case+ (xs, ys) of
    | (list0_cons (x, xs), list0_cons (y, ys)) => loop (f (x, y, ini), xs, ys)
    | (_, _) => ini
  // end of [loop]
} // end of [foldl]

implement{a,b}{c}
foldlEq (f, ini, xs, ys) = loop (ini, xs, ys) where {
  fun loop (ini: c, xs: list0 a, ys: list0 b): c = case+ (xs, ys) of
    | (list0_cons (x, xs), list0_cons (y, ys)) => loop (f (x, y, ini), xs, ys)
    | (list0_nil (), list0_nil ()) => ini 
    | (_, _) => $raise UnequalLengths ()
  // end of [loop]
} // end of [foldlEq]

(* ****** ****** *)

implement{a,b}{c}
foldr (f, snk, xs, ys) = aux (snk, xs, ys) where {
  fun aux (snk: c, xs: list0 a, ys: list0 b): c = case+ (xs, ys) of
    | (list0_cons (x, xs), list0_cons (y, ys)) => f (x, y, aux(snk, xs, ys))
    | (_, _) => snk
  // end of [aux]
} // end of [foldr]

implement{a,b}{c}
foldrEq (f, snk, xs, ys) = aux (snk, xs, ys) where {
  fun aux (snk: c, xs: list0 a, ys: list0 b): c = case+ (xs, ys) of
    | (list0_cons (x, xs), list0_cons (y, ys)) => f (x, y, aux (snk, xs, ys))
    | (list0_nil (), list0_nil ()) => snk
    | (_, _) => $raise UnequalLengths ()
  // end of [aux]
} // end of [foldrEq]

(* ****** ****** *)

implement{a,b}
all (f, xs, ys) = loop (f, xs, ys) where {
  fun loop (f: (a, b) -<cloref1> bool, xs: list0 a, ys: list0 b): bool =
    case+ (xs, ys) of
    | (list0_cons (x, xs), list0_cons (y, ys)) => f (x, y) andalso loop (f, xs, ys)
    | (_, _) => true
  // end of [loop]
} // end of [all]

implement{a,b}
allEq (f, xs, ys) = loop (f, xs, ys) where {
  fun loop (f: (a, b) -<cloref1> bool, xs: list0 a, ys: list0 b): bool =
    case+ (xs, ys) of
    | (list0_cons (x, xs), list0_cons (y, ys)) => f (x, y) andalso loop (f, xs, ys)
    | (list0_nil (), list0_nil ()) => true
    | (_, _) => false
  // end of [loop]
} // end of [all]

(* ****** ****** *)

implement{a,b}
exists (f, xs, ys) = loop (f, xs, ys) where {
  fun loop (f: (a, b) -<cloref1> bool, xs: list0 a, ys: list0 b): bool =
    case+ (xs, ys) of
    | (list0_cons (x, xs), list0_cons (y, ys)) => f (x, y) orelse loop (f, xs, ys)
    | (_, _) => false
  // end of [loop]
} // end of [all]

(* ****** ****** *)

(* end of [listPair.dats] *)
