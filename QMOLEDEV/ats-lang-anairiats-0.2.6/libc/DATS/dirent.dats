(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

staload "libc/SATS/dirent.sats"

(* ****** ****** *)

implement
dirent_stream_vt_make_DIR
  {l_dir:addr} (pf_dir | p_dir) = $ldelay (
  res where {
    var res: stream_vt_con dirent ; val () = f (pf_dir | p_dir, res)
  } // end of [where]
, $effmask_exn (closedir_exn (pf_dir | p_dir))
) where {
  extern fun readdir_r
    (_: &DIR, _: &dirent? >> dirent, _: &ptr? >> Ptr):<> int
    = "mac#atslib_readdir_r"
  fn f (
    pf_dir: DIR @ l_dir
  | p_dir: ptr l_dir, res: &stream_vt_con dirent? >> stream_vt_con dirent
  ) :<!laz> void = let
    var ret: ptr // uninitialized
    val () = (res := stream_vt_cons {dirent} (?, ?))
    val+ stream_vt_cons (!p_x, !p_xs) = res
    val err = readdir_r (!p_dir, !p_x, ret)
    val islast = if (err <> 0) then true else (ret = null)
  in
    if islast then let
      val () = $effmask_exn (closedir_exn (pf_dir | p_dir))
    in
      free@ {dirent} res; res := stream_vt_nil ()
    end else let
      val () = !p_xs := dirent_stream_vt_make_DIR (pf_dir | p_dir)
    in
      fold@ res
    end // end of [if]
  end // end of [f]
} // end of [dirent_stream_vt_make_DIR]

(* ****** ****** *)

implement
direntptr_stream_vt_make_DIR
  {l_dir:addr} (pf_dir | p_dir) = $ldelay (
  res where {
    var res: stream_vt_con dirent ; val () = f (pf_dir | p_dir, res)
  } // end of [where]
, $effmask_exn (closedir_exn (pf_dir | p_dir))
) where {
  extern fun readdir_r
    (_: &DIR, _: &dirent? >> dirent, _: &ptr? >> Ptr):<> int
    = "mac#atslib_readdir_r"
  fn f (
      pf_dir: DIR @ l_dir
    | p_dir: ptr l_dir
    , res: &stream_vt_con direntptr_gc? >> stream_vt_con direntptr_gc
    ) :<!laz> void = let
    var ret: ptr // uninitialized
    val (pf_ent_gc, pf_ent | p_ent) = ptr_alloc_tsz {dirent} (sizeof<dirent>)
    val err = readdir_r (!p_dir, !p_ent, ret)
    val islast = if (err <> 0) then true else (ret = null)
  in
    if islast then let
      val () = ptr_free {dirent} (pf_ent_gc, pf_ent | p_ent)
      val () = $effmask_exn (closedir_exn (pf_dir | p_dir))
    in
      res := stream_vt_nil ()
    end else let
      val x = (pf_ent_gc, pf_ent | p_ent): direntptr_gc
      val () = (res := stream_vt_cons {direntptr_gc} (x, ?))
      val+ stream_vt_cons (!p_x, !p_xs) = res
      val () = !p_xs := direntptr_stream_vt_make_DIR (pf_dir | p_dir)
    in
      fold@ res
    end // end of [if]
  end (* end of [f] *)
} // end of [direntptr_stream_vt_make_DIR]

(* ****** ****** *)

(* end of [dirent.dats] *)
