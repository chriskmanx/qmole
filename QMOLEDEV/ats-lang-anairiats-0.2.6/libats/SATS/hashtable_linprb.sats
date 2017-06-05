(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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

(*
**
** A hashtable implementation based on linear probing
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2010
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

sortdef t0p = t@ype and vt0p = viewt@ype

(* ****** ****** *)

typedef hash (key:t0p) = (key) -<cloref> ulint
typedef eqfn (key:t0p) = (key, key) -<cloref> bool

absviewtype
HASHTBLptr (key:t@ype, itm:viewt@ype+, l:addr)
viewtypedef HASHTBLptr0
  (key:t0p, itm:vt0p) = [l:agez] HASHTBLptr (key, itm, l)
// end of [HASHTBLptr0]
viewtypedef HASHTBLptr1
  (key:t0p, itm:vt0p) = [l:addr | l > null] HASHTBLptr (key, itm, l)
// end of [HASHTBLptr1]

castfn ptr_of_HASHTBLptr
  {key:t0p} {itm:vt0p} {l:addr} (x: !HASHTBLptr (key, itm, l)):<> ptr l
overload ptr_of with ptr_of_HASHTBLptr

(* ****** ****** *)

abstype
HASHTBLref (key:t@ype, itm:viewt@ype)

castfn
HASHTBLref_make_ptr
  {key:t0p;itm:vt0p}
  {l:agz} (
  x: HASHTBLptr (key, itm, l)
) :<> HASHTBLref (key, itm)
// end of [castfn]

//
// HX: unsafe but convenient ...
//
castfn
HASHTBLref_takeout_ptr
{key:t0p;itm:vt0p} (
  x: HASHTBLref (key, itm)
) :<!ref> [l:agz] (
  HASHTBLptr (key, itm, l) -<lin,prf> void | HASHTBLptr (key, itm, l)
) // end of [HASHTBLref_takeout_ptr]

(* ****** ****** *)

fun{key:t0p}
hash_key (x: key, hash: hash key):<> ulint
// end of [hash_key]

fun{key:t0p}
equal_key_key (x1: key, x2: key, eqfn: eqfn key):<> bool
// end of [equal_key_key]

(* ****** ****** *)

absviewt@ype Opt (a:viewt@ype) = a
prfun Opt_none {keyitm:vt0p} (x: !keyitm? >> Opt keyitm):<> void
prfun Opt_some {keyitm:vt0p} (x: !(keyitm) >> Opt keyitm):<> void
prfun Opt_encode
  {keyitm:vt0p} {b:bool} (x: !opt (keyitm, b) >> Opt keyitm):<> void
// end of [Opt_encode]

fun{keyitm:vt0p}
keyitem_nullify (x: &keyitm? >> Opt keyitm):<> void

fun{keyitm:vt0p}
keyitem_isnot_null (x: &Opt keyitm >> opt (keyitm, b)):<> #[b:bool] bool b

(* ****** ****** *)

fun hashtbl_size // the (array) size of the hashtable
  {key:t0p;itm:vt0p} {l:agz} (p: !HASHTBLptr (key, itm, l)):<> size_t
// end of [hashtbl_size]

fun hashtbl_total // the total number of elements present in the hashtable
  {key:t0p;itm:vt0p} {l:agz} (tbl: !HASHTBLptr (key, itm, l)):<> size_t
// end of [hashtbl_total]

fun{key:t0p;itm:t0p} // clear the hashtable
hashtbl_clear {l:agz} (ptbl: !HASHTBLptr (key, itm, l)):<> void
// end of [hashtbl_clear]

(* ****** ****** *)

//
// HX-2010-03-20:
// if the returned pointer is used, it must be done before the hashtable
// is changed!
//
fun{key:t0p;itm:vt0p} // unsafe but ...
hashtbl_search_ref {l:agz} (ptbl: !HASHTBLptr (key, itm, l), k0: key): Ptr
// end of [hashtbl_search_ptr]

//
// HX-2010-03-20:
// this one is a safe version, but it can only handle non-linear items
//
fun{key:t0p;itm:t0p}
hashtbl_search {l:agz} (
  ptbl: !HASHTBLptr (key, itm, l), k0: key, res: &itm? >> opt (itm, b)
) :<> #[b:bool] bool b
// end of [hashtbl_search]

(* ****** ****** *)

//
// HX-2010-04-03:
// if [k] is already in the table, [i] replaces the original one
//
fun{key:t0p;itm:vt0p}
hashtbl_insert {l:agz} (
  ptbl: !HASHTBLptr (key, itm, l)
, k: key, i: itm, res: &itm? >> opt (itm, b)
) :<> #[b:bool] bool b
// end of [hashtbl_insert]

//
// HX-2010-04-03:
// removal seems to be quite efficient as well
//
fun{key:t0p;itm:vt0p}
hashtbl_remove {l:agz} (
  ptbl: !HASHTBLptr (key, itm, l)
, k0: key, res: &itm? >> opt (itm, b)
) :<> #[b:bool] bool b
// end of [hashtbl_remove]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
hashtbl_foreach_vclo
  {v:view} {l:agz} (
  pf: !v | ptbl: !HASHTBLptr (key, itm, l), f: &(!v | key, &itm) -<clo> void
) :<> void // end of [hashtbl_foreach_vclo]

fun{key:t0p;itm:vt0p}
hashtbl_foreach_cloref {l:agz}
  (ptbl: !HASHTBLptr (key, itm, l), f: !(key, &itm) -<cloref> void):<> void
// end of [hashtbl_foreach_cloref]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
hashtbl_make
  (hash: hash key, eqfn: eqfn key):<> HASHTBLptr1 (key, itm)
// end of [hashtbl_make]

(*
// some prime numbers
53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741
*)

fun{key:t0p;itm:vt0p}
hashtbl_make_hint (
  hash: hash key, eqfn: eqfn key, hint: size_t
) :<> HASHTBLptr1 (key, itm)
  = "atslib_hashtbl_make_hint__linprb"
// end of [hashtbl_make_hint]

(* ****** ****** *)

fun hashtbl_free
  {key:t0p;itm:t0p}
  {l:agz} (
  tbl: HASHTBLptr (key, itm, l)
) :<> void
  = "atslib_hashtbl_free__linprb"
// end of [hashtbl_free]

fun hashtbl_free_vt
  {key:t0p;itm:vt0p}
  {l:agz} (
  tbl: !HASHTBLptr (key, itm, l) >> opt (HASHTBLptr (key, itm, l), b)
) :<> #[b:bool] bool b(*~freed*) = "atslib_hashtbl_free_vt__linprb"
// end of [hashtbl_free_vt]

(* ****** ****** *)

//
// HX-2010-07-01: it can be readily implemented based on [foreach]
//
fun{key:t0p;itm:t0p}
hashtbl_listize {l:agz}
  (tbl: !HASHTBLptr (key, itm, l)):<> List_vt @(key, itm)
fun{key:t0p;itm:vt0p}
hashtbl_listize_free
  {l:agz} (tbl: HASHTBLptr (key, itm, l)):<> List_vt @(key, itm)

(* ****** ****** *)

(* end of [hashtable_linprb.sats] *)
