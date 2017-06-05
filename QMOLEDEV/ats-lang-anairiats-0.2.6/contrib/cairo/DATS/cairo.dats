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
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: December, 2009

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload STDIO = "libc/SATS/stdio.sats"

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

implement
cairo_create0 (sf) = cr where {
  val cr = cairo_create (sf); val () = cairo_surface_destroy (sf)
} // end of [cairo_create0]

(* ****** ****** *)

implement
cairo_get1_target (cr) = sf1 where {
  val (fpf_sf | sf) = cairo_get_target (cr)
  val sf1 = cairo_surface_reference (sf) // increase refcount by 1
  prval () = minus_addback (fpf_sf, sf | cr) // return borrowed resource
} // end of [cairo_get_target1]

implement
cairo_get1_group_target (cr) = sf1 where {
  val (fpf_sf | sf) = cairo_get_group_target (cr)
  val sf1 = cairo_surface_reference (sf) // increase refcount by 1
  prval () = minus_addback (fpf_sf, sf | cr) // return borrowed resource
} // end of [cairo_get_group_target1]

(* ****** ****** *)

(*

//
// implementing [cairo_surface_write_to_png] in terms of
// [cairo_surface_write_to_png_stream]
//

// fun cairo_surface_write_to_png
//  (sf: !cairo_surface_ref, filename: string): cairo_status_t

implement cairo_surface_write_to_png
  (sf, filename) = let
  val [l:addr] (pfopt_fil | p_fil) =
    $STDIO.fopen_err (filename, file_mode_w)
in
  if (p_fil <> null) then let
    prval Some_v pf_fil = pfopt_fil
    viewdef V = FILE w @ l; viewtypedef VT = ptr l
    fn f (pf: !V | env: !VT, data: string, n0: uint)
      : cairo_status_t = let
      extern fun fwrite (data: string, n: size_t, fil: &FILE w): size_t
        = "atslib_fwrite_byte"
      val n0 = size_of_uint (n0)
      val n1 = fwrite (data, n0, !env)
    in
      if n1 < n0 then CAIRO_STATUS_WRITE_ERROR else CAIRO_STATUS_SUCCESS
    end // end of [fn f]
    val status =
      cairo_surface_write_to_png_stream {V} {VT} (pf_fil | sf, f, p_fil)
    val () = $STDIO.fclose1_exn (pf_fil | p_fil)
  in
    status
  end else let
    prval None_v () = pfopt_fil in CAIRO_STATUS_WRITE_ERROR 
  end // end of [if]
end // end of [cairo_surface_write_to_png]

*)

(* ****** ****** *)

(* end of [cairo.dats] *)
