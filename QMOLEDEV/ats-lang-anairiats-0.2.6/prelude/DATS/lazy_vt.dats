(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/lazy_vt.sats"

(* ****** ****** *)

(*

local // for call-by-need lazy evaluation

assume lazy_viewt0ype_viewtype (a:viewt@ype) = thunkvalue_vt a

in

implement{a}
lazy_vt_force_crypt
  (v_lazy) = begin
  case+ $decrypt (v_lazy) of
  | ~thunkvalue_vt_thunk (xf) => let
      stavar T: t@ype
      val x = $effmask_ref((xf: () -<lin,cloptr1> a) ())
      val (pf_gc, pf_at | p) = cloptr_get_view_ptr {T} (xf)
    in
      ptr_free (pf_gc, pf_at | p); x
    end // end of [thunkvalue_vt_thunk]
  | ~thunkvalue_vt_value (x) => x
end // end of [lazy_vt_force_crypt]

end // end of [local]

*)

implement{a} lazy_vt_force (r) = !r

(* ****** ****** *)

extern castfn
list_vt_cons_unfold_of_stream_vt_cons_unfold {l1,l2:addr}
  (x: stream_vt_cons_unfold (l1, l2)):<> list_vt_cons_unfold (l1, l2)
// casting one data constructor to another

implement{a}
list_vt_of_stream_vt (xs) = let
  fun loop {n0:nat} (
    xs: stream_vt a, n: &int n0 >> int (n + n0)
  ) :<!laz> #[n:nat] list_vt (a, n) = let
    val xs_con = !xs
  in
    case+ xs_con of
    | stream_vt_cons (!p_x, !p_xs1) => let
        val () = n := n + 1
        val xs1 = !p_xs1
        prval pf_xs1 = list_vt_of_lazy_vt (view@ !p_xs1) where {
          extern prfun list_vt_of_lazy_vt
            {l:addr} (pf: stream_vt a? @ l): List_vt a? @ l
        } // end of [prval]
        val () = !p_xs1 := loop (xs1, n)
        val xs_cons = begin
          list_vt_cons_unfold_of_stream_vt_cons_unfold (xs_con)
        end // end of [val]
      in
        fold@ xs_cons; xs_cons
      end // end of [stream_cons]
    | ~stream_vt_nil () => list_vt_nil ()
  end (* end of [loop] *)
  var n = 0; val res = loop (xs, n)
in
  (n, res)
end // end of [list_vt_of_stream_vt]

(* ****** ****** *)

implement{a}
stream_vt_free (xs) = case+ !xs of
  | ~stream_vt_cons (_, xs) => stream_vt_free xs
  | ~stream_vt_nil () => ()
// end of [stream_vt_free]

(* ****** ****** *)

local

fun{a:t@ype}
stream_vt_filter_cloptr_con
  (xs: stream_vt a, pred: (&a) -<cloptr,!laz> bool)
  :<!laz> stream_vt_con a = let
  val xs_con = !xs
in
  case+ xs_con of
  | stream_vt_cons (!p_x, !p_xs1) => begin
      if pred (!p_x) then let
        val xs1 = !p_xs1
        val () = !p_xs1 := stream_vt_filter_cloptr (xs1, pred)
      in
        fold@ {a} (xs_con); xs_con
      end else let
        val xs1 = !p_xs1
        val () = free@ {a} (xs_con)
      in
        stream_vt_filter_cloptr_con (xs1, pred)
      end // end of [if]
    end (* end of [stream_vt_cons] *)
  | stream_vt_nil () => begin
      fold@ xs_con; cloptr_free pred; xs_con
    end // end of [stream_vt_nil]
end // end of [stream_vt_filter_con]

in // in of [local]

implement{a}
stream_vt_filter_fun (xs, pred) =
  $ldelay (stream_vt_filter_cloptr_con<a> (xs, lam x => pred x), ~xs)
// end of [stream_vt_filter_fun]

implement{a}
stream_vt_filter_cloptr (xs, pred) = $ldelay (
  stream_vt_filter_cloptr_con<a> (xs, pred), (cloptr_free pred; ~xs)
) // end of [stream_vt_filter_cloptr]

end // end of [local]

(* ****** ****** *)

local

fun
{a:viewt@ype}
{b:viewt@ype}
stream_vt_map_cloptr_con (
  xs: stream_vt a, f: (&a >> a?) -<cloptr,!laz> b
) :<!laz> stream_vt_con (b) = let
  val xs_con = !xs
in
  case+ xs_con of
  | stream_vt_cons (!p_x, xs1) => let
      val y = f (!p_x)
      val () = free@ {a} (xs_con)
      val ys = $ldelay (
        stream_vt_map_cloptr_con<a><b> (xs1, f)
      , (~xs1; cloptr_free (f))
      ) // end of [$ldelay
    in
      stream_vt_cons (y, ys)
    end // end of [stream_vt_cons]
  | ~stream_vt_nil () => (cloptr_free (f); stream_vt_nil)
end // end of [stream_vt_map_cloptr_con]

in // in of [local]

implement{a}{b}
stream_vt_map_fun (xs, f) = $ldelay (
  stream_vt_map_cloptr_con<a><b> (xs, lam (x) => f (x)), ~xs
) // end of [stream_vt_map_fun]

implement{a}{b}
stream_vt_map_cloptr (xs, f) = $ldelay (
  stream_vt_map_cloptr_con<a><b> (xs, f), (~xs; cloptr_free (f))
) // end of [stream_vt_map_cloptr]

end // end of [local]

(* ****** ****** *)

local

#define nil stream_vt_nil
#define :: stream_vt_cons

fun
{a1,a2:t@ype}
{b:viewt@ype}
stream_vt_map2_cloptr_con (
  xs1: stream_vt a1
, xs2: stream_vt a2
, f: (a1, a2) -<cloptr,!laz> b
) :<!laz> stream_vt_con b = begin
  case !xs1 of
  | ~(x1 :: xs1) => begin case+ !xs2 of
    | ~(x2 :: xs2) => y :: ys where {
        val y = f (x1, x2)
        val ys = $ldelay (
          stream_vt_map2_cloptr_con<a1,a2><b> (xs1, xs2, f)
        , (~xs1; ~xs2; cloptr_free f)
        ) // end of [$ldelay]
      } (* end of [::] *)
    | ~nil () => (~xs1; cloptr_free f; nil ())
    end // end of [::]
  | ~nil () => (~xs2; cloptr_free f; nil ())
end // end of [stream_map2_con]

in // in of [local]

implement{a1,a2}{b}
stream_vt_map2_fun (xs1, xs2, f) = $ldelay (
  stream_vt_map2_cloptr_con<a1,a2><b> (xs1, xs2, lam (x1, x2) => f (x1, x2))
, (~xs1; ~xs2)
) // end of [stream_map2_fun]

implement{a1,a2}{b}
stream_vt_map2_cloptr (xs1, xs2, f) = $ldelay (
  stream_vt_map2_cloptr_con<a1,a2><b> (xs1, xs2, f), (~xs1; ~xs2; cloptr_free f)
) // end of [stream_vt_map2_cloptr]

end // end of [local]

(* ****** ****** *)

local

#define free list_vt_free
staload UN = "prelude/SATS/unsafe.sats"

fun{a1,a2:t@ype}
stream_vt_of_lstlstprod_con {n1,n2:nat} (
  xs1: list_vt (a1, n1), xs2: list_vt (a2, n2)
) :<!laz> stream_vt_con @(a1, a2) = let
  typedef lst2_t = list (a2, n2)
  viewtypedef res_vt = stream_vt_con @(a1, a2)
//
  fun aux1 {n1:nat} (
    xs1: list_vt (a1, n1), xs2: list_vt (a2, n2)
  ) :<!laz> res_vt =
    case+ xs1 of
    | ~list_vt_cons (x1, xs1) =>
        aux2 (x1, $UN.castvwtp1 {lst2_t} (xs2), xs1, xs2)
    | ~list_vt_nil () => (free (xs2); stream_vt_nil)
  // end of [aux1]
//
  and aux2 {n1:nat} {k:nat | k <= n2} (
    y1: a1, ys2: list (a2, k)
  , xs1: list_vt (a1, n1), xs2: list_vt (a2, n2)
  ) :<!laz> res_vt =
    case+ ys2 of
    | list_cons (y2, ys2) => let
        val res = $ldelay (
          aux2 (y1, ys2, xs1, xs2), (free (xs1); free (xs2))
        ) // end of [val]
      in
        stream_vt_cons ((y1, y2), res)
      end // end of [list_cons]
    | list_nil () => aux1 (xs1, xs2)
  // end of [aux2]
//
in
  aux1 (xs1, xs2)
end // end of [stream_vt_of_lstlstprod_con]

in // in of [local]

implement{a1,a2}
stream_vt_of_lstlstprod (xs1, xs2) = $ldelay (
  stream_vt_of_lstlstprod_con<a1,a2> (xs1, xs2), (free (xs1); free (xs2))
) // end of [stream_vt_of_lstlstprod]

end // end of [local]

(* ****** ****** *)

local

#define free list_vt_free
staload UN = "prelude/SATS/unsafe.sats"

fun{a1,a2:t@ype}
stream_vt_of_strmlstprod_con {n:nat} (
  xs1: stream_vt (a1), xs2: list_vt (a2, n)
) :<!laz> stream_vt_con @(a1, a2) = let
  typedef lst2_t = list (a2, n)
  viewtypedef res_vt = stream_vt_con @(a1, a2)
//
  fun aux1 (
    xs1: stream_vt (a1), xs2: list_vt (a2, n)
  ) :<!laz> res_vt =
    case+ !xs1 of
    | ~stream_vt_cons (x1, xs1) =>
        aux2 (x1, $UN.castvwtp1 {lst2_t} (xs2), xs1, xs2)
    | ~stream_vt_nil () => (free (xs2); stream_vt_nil)
  // end of [aux1]
//
  and aux2 {k:nat | k <= n} (
    y1: a1, ys2: list (a2, k)
  , xs1: stream_vt (a1), xs2: list_vt (a2, n)
  ) :<!laz> res_vt =
    case+ ys2 of
    | list_cons (y2, ys2) => let
        val res = $ldelay (
          aux2 (y1, ys2, xs1, xs2), (~xs1; free (xs2))
        ) // end of [val]
      in
        stream_vt_cons ((y1, y2), res)
      end // end of [list_cons]
    | list_nil () => aux1 (xs1, xs2)
  // end of [aux2]
//
in
  aux1 (xs1, xs2)
end // end of [stream_vt_of_strmlstprod_con]

in // in of [local]

implement{a1,a2}
stream_vt_of_strmlstprod (xs1, xs2) = $ldelay (
  stream_vt_of_strmlstprod_con<a1,a2> (xs1, xs2), (~xs1; free (xs2))
) // end of [stream_vt_of_strmlstprod]

end // end of [local]

(* ****** ****** *)

(* end of [lazy_vt.dats] *)
