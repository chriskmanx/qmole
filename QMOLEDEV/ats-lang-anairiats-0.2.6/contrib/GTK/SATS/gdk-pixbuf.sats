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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2010
//
(* ****** ****** *)

%{#
#include "contrib/GTK/CATS/gdk-pixbuf.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for staload at run-time

(* ****** ****** *)

staload GLIB = "contrib/glib/SATS/glib.sats"
stadef gint = $GLIB.gint
stadef guint = $GLIB.guint
stadef gint8 = $GLIB.gint8
stadef guint8 = $GLIB.guint8
stadef guint16 = $GLIB.guint16
stadef guint32 = $GLIB.guint32

stadef gfloat = $GLIB.gfloat
stadef gdouble = $GLIB.gdouble

stadef gpointer = $GLIB.gpointer

(* ****** ****** *)

staload GOBJ = "contrib/glib/SATS/glib-object.sats"
stadef gboolean = $GOBJ.gboolean
stadef gobjref = $GOBJ.gobjref

(* ****** ****** *)

staload "gdkclassdec.sats"

(* ****** ****** *)

#include "gdk-pixbuf/gdk-pixbuf-core.sats"

(* ****** ****** *)

(* end of [gdk-pixbuf.sats] *)

////

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf-features.h>
#include <glib-object.h>

#include <gdk-pixbuf/gdk-pixbuf-core.h>
#include <gdk-pixbuf/gdk-pixbuf-transform.h>
#include <gdk-pixbuf/gdk-pixbuf-animation.h>
#include <gdk-pixbuf/gdk-pixbuf-simple-anim.h>
#include <gdk-pixbuf/gdk-pixbuf-io.h>
#include <gdk-pixbuf/gdk-pixbuf-loader.h>
#include <gdk-pixbuf/gdk-pixbuf-enum-types.h>
