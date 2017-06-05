(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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

// author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/lazy.sats"

(* ****** ****** *)

(*

local // for call-by-need lazy evaluation

assume lazy_t0ype_type (a:t@ype) = ref (thunkvalue a)

in

implement{a}
lazy_force_crypt (r) = $effmask_ref let
  val (vbox pf | p) = begin
    ref_get_view_ptr ($decrypt r) // this effect is ignored!
  end // end of [val]
in
  case+ !p of
  | ~thunkvalue_thunk (xf) => let
      val x = $effmask_ref ((xf: () -<cloref1> a) ())
    in
      !p := thunkvalue_value x; x
    end // end of [thunkvalue_thunk]
  | thunkvalue_value (x) => let val () = fold@ (!p) in x end
    // end of [thunkvalue_value]
end // end of [lazy_force_crypt]

*)

implement{a} lazy_force (r) = !r

(* ****** ****** *)

#define nil stream_nil
#define cons stream_cons
#define :: stream_cons

(* ****** ****** *)

fun{a:t@ype}
stream_filter_fun_con
  (xs: stream a, p: a -<fun,!laz> bool)
  :<!laz> stream_con a = begin case+ !xs of
  | stream_cons (x, xs) => begin
      if p x then stream_cons (x, stream_filter_fun<a> (xs, p))
      else stream_filter_fun_con (xs, p)
    end // end of [stream_cons]
  | stream_nil () => stream_nil ()
end // end of [stream_filter_fun_con]

implement{a}
stream_filter_fun (xs, p) =
  $delay (stream_filter_fun_con<a> (xs, p))
// end of [stream_filter_fun]
  
fun{a:t@ype}
stream_filter_cloref_con
  (xs: stream a, p: a -<cloref,!laz> bool)
  :<!laz> stream_con a = begin case+ !xs of
  | stream_cons (x, xs) => begin
      if p x then stream_cons (x, stream_filter_cloref<a> (xs, p))
      else stream_filter_cloref_con (xs, p)
    end // end of [stream_cons]
  | stream_nil () => stream_nil ()
end // end of [stream_filter_cloref_con]

implement{a}
stream_filter_cloref (xs, p) =
  $delay (stream_filter_cloref_con<a> (xs, p))
// end of [stream_filter_cloref]

(* ****** ****** *)

implement{a}{b}
stream_map_fun (xs, f) = $delay (begin
  case+ !xs of
  | x :: xs => cons (f x, stream_map_fun<a><b> (xs, f)) | nil () => nil ()
end : stream_con b) // end of [stream_map_fun]

implement{a}{b}
stream_map_cloref (xs, f) = $delay (begin
  case+ !xs of
  | x :: xs => cons (f x, stream_map_cloref<a><b> (xs, f)) | nil () => nil ()
end : stream_con b) // end of [stream_map_cloref]

(* ****** ****** *)

implement{a1,a2}{b}
stream_map2_fun (xs1, xs2, f) = $delay (begin
  case+ !xs1 of
  | x1 :: xs1 => begin case+ !xs2 of
    | x2 :: xs2 => f (x1, x2) :: stream_map2_fun<a1,a2><b> (xs1, xs2, f)
    | nil () => nil ()
    end // end of [::]
  | nil () => nil ()
end : stream_con b) // end of [stream_map2_fun]

implement{a1,a2}{b}
stream_map2_cloref (xs1, xs2, f) = $delay (begin
  case+ !xs1 of
  | x1 :: xs1 => begin case+ !xs2 of
    | x2 :: xs2 => f (x1, x2) :: stream_map2_cloref<a1,a2><b> (xs1, xs2, f)
    | nil () => nil ()
    end // end of [::]
  | nil () => nil ()
end : stream_con b) // end of [stream_map2_cloref]

(* ****** ****** *)

implement{a}
stream_ordmerge_fun
  (xs10, xs20, lte) = $delay (begin
  case+ !xs10 of
  | x1 :: xs1 => begin case+ !xs20 of
    | x2 :: xs2 => begin
        if lte (x1, x2) then begin
          x1 :: stream_ordmerge_fun (xs1, xs20, lte)
        end else begin
          x2 :: stream_ordmerge_fun (xs10, xs2, lte)
        end // end of [if]
      end (* end of [::] *)
    | nil () => x1 :: xs1
    end (* end of [::] *)
  | nil () => !xs20
end : stream_con a) // end of [stream_ordmerge_fun]

implement{a}
stream_ordmerge_cloref
  (xs10, xs20, lte) = $delay (begin
  case+ !xs10 of
  | x1 :: xs1 => begin case+ !xs20 of
    | x2 :: xs2 => begin
        if lte (x1, x2) then begin
          x1 :: stream_ordmerge_cloref (xs1, xs20, lte)
        end else begin
          x2 :: stream_ordmerge_cloref (xs10, xs2, lte)
        end // end of [if]
      end (* end of [::] *)
    | nil () => x1 :: xs1
    end (* end of [::] *)
  | nil () => !xs20
end : stream_con a) // end of [stream_ordmerge_cloref]

(* ****** ****** *)

implement{a}
stream_nth (xs, n) = begin case+ !xs of
  | x :: xs => if n = 0 then x else stream_nth<a> (xs, n-1)
  | nil () => $raise StreamSubscriptException ()
end // end of [stream_nth]

(* ****** ****** *)

implement{a}
stream_take (xs, n) = let
  fun loop {n:nat} .<n>. (
      xs: stream a
    , n: int n
    , res: &List_vt a? >> list_vt (a, k)
    ) :<> #[k:nat | k <= n] void =
    if n > 0 then (case+ !xs of
      | stream_cons (x, xs) => let
          val () =
            res := list_vt_cons {a} {0} (x, ?)
          // end of [val]
          val list_vt_cons (_, !p_res_nxt) = res
          val () = loop (xs, n-1, !p_res_nxt)
        in
          fold@ (res)
        end // end of [stream_cons]
      | stream_nil () => (res := list_vt_nil ())
    ) else (
      res := list_vt_nil ()
    ) // end of [if]
  var res: List_vt a // uninitialized
  val () = loop (xs, n, res)
in
  res
end // end of [stream_take]

(* ****** ****** *)

(* end of [lazy.dats] *)
