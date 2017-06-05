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
// Time: April, 2010
//
(* ****** ****** *)

%{#
#include "contrib/GTK/CATS/gdk.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for staload at run-time

(* ****** ****** *)

staload GLIB = "contrib/glib/SATS/glib.sats"
//
stadef gboolean (b:bool) = $GLIB.gboolean b
stadef gboolean = $GLIB.gboolean
//
stadef gchar = $GLIB.gchar
//
stadef gint = $GLIB.gint
stadef gint8 = $GLIB.gint8
stadef gint16 = $GLIB.gint16
stadef gint32 = $GLIB.gint32
//
stadef guint = $GLIB.guint
stadef guint8 = $GLIB.guint8
stadef guint16 = $GLIB.guint16
stadef guint32 = $GLIB.guint32
//
stadef gfloat = $GLIB.gfloat
stadef gdouble = $GLIB.gdouble
//
stadef gpointer = $GLIB.gpointer
//
stadef gstring = $GLIB.gstring
stadef gstring0 = $GLIB.gstring0
stadef gstring1 = $GLIB.gstring1
//
(* ****** ****** *)

staload GOBJ = "contrib/glib/SATS/glib-object.sats"
stadef gobjref = $GOBJ.gobjref

(* ****** ****** *)

absview GdkFree_v (l:addr) // for free GDK resources

(* ****** ****** *)

staload "contrib/GTK/SATS/gdkclassdec.sats"

(* ****** ****** *)

viewtypedef GdkColormap_ref (l:addr) = gobjref (GdkColormap, l)
viewtypedef GdkColormap_ref0 = [l:agez] GdkColormap_ref l
viewtypedef GdkColormap_ref1 = [l:addr | l > null] GdkColormap_ref l

viewtypedef GdkPixbuf_ref (l:addr) = gobjref (GdkPixbuf, l)
viewtypedef GdkPixbuf_ref0 = [l:agez] GdkPixbuf_ref l
viewtypedef GdkPixbuf_ref1 = [l:addr | l > null] GdkPixbuf_ref l

viewtypedef GdkPixmap_ref (l:addr) = gobjref (GdkPixmap, l)
viewtypedef GdkPixmap_ref0 = [l:agez] GdkPixmap_ref l
viewtypedef GdkPixmap_ref1 = [l:addr | l > null] GdkPixmap_ref l

viewtypedef GdkWindow_ref (l:addr) = gobjref (GdkWindow, l)
viewtypedef GdkWindow_ref0 = [l:agez] GdkWindow_ref l
viewtypedef GdkWindow_ref1 = [l:addr | l > null] GdkWindow_ref l

(* ****** ****** *)

#include "contrib/GTK/SATS/gdk/gdktypes.sats"

(* ****** ****** *)

#include "contrib/GTK/SATS/gdk/gdkcairo.sats"
#include "contrib/GTK/SATS/gdk/gdkcolor.sats"
#include "contrib/GTK/SATS/gdk/gdkevents.sats"
#include "contrib/GTK/SATS/gdk/gdkkeys.sats"
#include "contrib/GTK/SATS/gdk/gdkpixbuf.sats"
#include "contrib/GTK/SATS/gdk/gdkpixmap.sats"
#include "contrib/GTK/SATS/gdk/gdkrgb.sats"
#include "contrib/GTK/SATS/gdk/gdkselection.sats"
#include "contrib/GTK/SATS/gdk/gdkwindow.sats"

(* ****** ****** *)

(* end of [gdk.sats] *)
