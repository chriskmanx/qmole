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
//
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: May, 2010
//
(* ****** ****** *)
//
// API for pango in ATS
//
(* ****** ****** *)

%{#
#include "contrib/pango/CATS/pango.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)
//
staload GLIB = "contrib/glib/SATS/glib.sats"
//
stadef gboolean (b:bool) = $GLIB.gboolean b
stadef gboolean = $GLIB.gboolean
//
stadef gchar = $GLIB.gchar
//
stadef gint (n:int) = $GLIB.gint (n)
stadef gint = $GLIB.gint
stadef gint8 = $GLIB.gint8
stadef gint16 = $GLIB.gint16
stadef gint32 = $GLIB.gint32
//
stadef guint (n:int) = $GLIB.guint (n)
stadef guint = $GLIB.guint
stadef guint8 = $GLIB.guint8
stadef guint16 = $GLIB.guint16
stadef guint32 = $GLIB.guint32
//
stadef gdouble = $GLIB.gdouble
stadef gfloat = $GLIB.gfloat
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

absview PangoFree_v (l:addr) // for free Pango resources

(* ****** ****** *)

staload "contrib/pango/SATS/pangoclassdec.sats"

viewtypedef PangoContext_ref (l:addr) = gobjref (PangoContext, l)
viewtypedef PangoContext_ref0 = [l:agez] PangoContext_ref l
viewtypedef PangoContext_ref1 = [l:addr | l > null] PangoContext_ref l

viewtypedef PangoLayout_ref (l:addr) = gobjref (PangoLayout, l)
viewtypedef PangoLayout_ref0 = [l:agez] PangoLayout_ref l
viewtypedef PangoLayout_ref1 = [l:addr | l > null] PangoLayout_ref l

(* ****** ****** *)

(*
#include <pango/pango-attributes.h>
#include <pango/pango-break.h>
#include <pango/pango-context.h>
#include <pango/pango-coverage.h>
#include <pango/pango-engine.h>
#include <pango/pango-enum-types.h>
#include <pango/pango-font.h>
#include <pango/pango-fontmap.h>
#include <pango/pango-glyph.h>
#include <pango/pango-item.h>
#include <pango/pango-layout.h>
#include <pango/pango-renderer.h>
#include <pango/pango-script.h>
#include <pango/pango-types.h>
*)

(* ****** ****** *)

#include "contrib/pango/SATS/pango/pango-attributes.sats"
#include "contrib/pango/SATS/pango/pango-context.sats"
#include "contrib/pango/SATS/pango/pango-font.sats"
#include "contrib/pango/SATS/pango/pango-layout.sats"

(* ****** ****** *)

(* end of [pango.sats] *)
