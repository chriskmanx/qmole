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

(*

struct _GtkStyle
{
  GObject parent_instance;

  /*< public >*/

  GdkColor fg[5];
  GdkColor bg[5];
  GdkColor light[5];
  GdkColor dark[5];
  GdkColor mid[5];
  GdkColor text[5];
  GdkColor base[5];
  GdkColor text_aa[5];		/* Halfway between text/base */

  GdkColor black;
  GdkColor white;
  PangoFontDescription *font_desc;

  gint xthickness;
  gint ythickness;

  GdkGC *fg_gc[5];
  GdkGC *bg_gc[5];
  GdkGC *light_gc[5];
  GdkGC *dark_gc[5];
  GdkGC *mid_gc[5];
  GdkGC *text_gc[5];
  GdkGC *base_gc[5];
  GdkGC *text_aa_gc[5];
  GdkGC *black_gc;
  GdkGC *white_gc;

  GdkPixmap *bg_pixmap[5];

  /*< private >*/

  gint attach_count;

  gint depth;
  GdkColormap *colormap;
  GdkFont *private_font;
  PangoFontDescription *private_font_desc; /* Font description for style->private_font or %NULL */

  /* the RcStyle from which this style was created */
  GtkRcStyle	 *rc_style;

  GSList	 *styles;	  /* of type GtkStyle* */
  GArray	 *property_cache;
  GSList         *icon_factories; /* of type GtkIconFactory* */
};

*)

(* ****** ****** *)

fun gtk_style_get_text_aa
  {c:cls | c <= GtkStyle} {l:agz}
  (x: !gobjref (c, l)): [l1:addr] (
    minus (gobjref (c, l), array_v (GdkColor, 5, l1)), array_v (GdkColor, 5, l1) | ptr l1
  ) = "atsctrb_gtk_style_get_text_aa" // a function
// end of [gtk_style_get_text_aa]

fun gtk_style_get_font_desc
  {c:cls | c <= GtkStyle} {l:agz}
  (x: !gobjref (c, l)): [l1:addr] (
    minus (gobjref (c, l), PangoFontDescription_ptr l1) |  PangoFontDescription_ptr l1
  ) = "atsctrb_gtk_style_get_font_desc" // a function
// end of [gtk_style_get_font_desc]

(* ****** ****** *)

fun gtk_style_new
  (): GtkStyle_ref1 = "mac#atsctrb_gtk_style_new"
// end of [gtk_style_new]

fun gtk_style_copy
  {c:cls | c <= GtkStyle}
  {l:agz} (
  x: !gobjref (c, l)
) : GtkStyle_ref1
  = "mac#atsctrb_gtk_style_copy"
// end of [gtk_style_copy]

(* ****** ****** *)

fun gtk_style_attach
  {c1,c2:cls | c1 <= GtkStyle; c2 <= GdkWindow}
  {l1,l2:agz} (
  x: gobjref (c1, l1), win: !gobjref (GdkWindow, l2)
) : GtkStyle_ref1
  = "mac#atsctrb_gtk_style_attach"
// end of [gtk_style_attach]

fun gtk_style_detach
  {c:cls | c <= GtkStyle}
  {l:agz} (
  x: !gobjref (c, l)
) : void
  = "mac#atsctrb_gtk_style_detach"
// end of [gtk_style_detach]

(* ****** ****** *)

(* end of [gtkstyle.sats] *)
