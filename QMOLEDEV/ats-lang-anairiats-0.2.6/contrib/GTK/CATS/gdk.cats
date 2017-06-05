/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Power of Types!
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
*/

/* ****** ****** */
//
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: April, 2010
//
/* ****** ****** */

#ifndef ATSCTRB_GTK_GDK_CATS
#define ATSCTRB_GTK_GDK_CATS

/* ****** ****** */

#include <gdk/gdk.h>

/* ****** ****** */

//
// source: gdkcairo.h
//

#define atsctrb_gdk_cairo_create gdk_cairo_create
#define atsctrb_gdk_cairo_set_source_color gdk_cairo_set_source_color
#define atsctrb_gdk_cairo_set_source_pixbuf gdk_cairo_set_source_pixbuf
#define atsctrb_gdk_cairo_set_source_pixmap gdk_cairo_set_source_pixmap
#define atsctrb_gdk_cairo_rectangle gdk_cairo_rectangle
#define atsctrb_gdk_cairo_region gdk_cairo_region
#define atsctrb_gdk_cairo_reset_clip gdk_cairo_reset_clip

/* ****** ****** */

//
// source: gdkcolor.h
//

ATSinline()
ats_void_type
atsctrb_gdk_color3_set (
  ats_ptr_type p_color
, ats_uint_type r, ats_uint_type b, ats_uint_type g
) {
  ((GdkColor*)p_color)->red = (guint16)r ;
  ((GdkColor*)p_color)->blue = (guint16)b ;
  ((GdkColor*)p_color)->green = (guint16)g ;
  return ;
} // end of [atsctrb_gdk_color3_set]

ATSinline()
ats_void_type
atsctrb_gdk_color4_set (
  ats_ptr_type p_color
, ats_uint_type pix
, ats_uint_type r, ats_uint_type b, ats_uint_type g
) {
  ((GdkColor*)p_color)->pixel = (guint32)pix ;
  ((GdkColor*)p_color)->red = (guint16)r ;
  ((GdkColor*)p_color)->blue = (guint16)b ;
  ((GdkColor*)p_color)->green = (guint16)g ;
  return ;
} // end of [atsctrb_gdk_color4_set]

#define atsctrb_gdk_color_copy gdk_color_copy
#define atsctrb_gdk_color_free gdk_color_free

#define atsctrb_gdk_color_parse gdk_color_parse

/* ****** ****** */

//
// source: gdkevent.h
//

ATSinline()
ats_bool_type
atsctrb_eq_GdkEventType_GdkEventType
  (GdkEventType x1, GdkEventType x2) { return (x1 == x2) ; }
// end of [atsctrb_eq_GdkEventType_GdkEventType]

ATSinline()
GdkEventMask
atsctrb_lor_GdkEventMask_GdkEventMask
  (GdkEventMask x1, GdkEventMask x2) { return (x1 | x2) ; }
// end of [atsctrb_lor_GdkEventMask_GdkEventMask]

ATSinline()
ats_bool_type
atsctrb_eq_GdkVisibilityState_GdkVisibilityState
  (GdkVisibilityState x1, GdkVisibilityState x2) { return (x1 == x2) ; }
// end of [atsctrb_eq_GdkVisibilityState_GdkVisibilityState]

/* ****** ****** */

//
// source: gdkpixbuf.h
//

#define atsctrb_gdk_pixbuf_render_threshold_alpha \
  gdk_pixbuf_render_threshold_alpha

/* ****** ****** */

//
// source: gdkpixmap.h
//

#define atsctrb_gdk_pixmap_new gdk_pixmap_new

/* ****** ****** */

//
// source: gdkrgb.h
//

#define atsctrb_gdk_rgb_find_color gdk_rgb_find_color

/* ****** ****** */

//
// source: gdkwindow.h
//

#define atsctrb_gdk_window_set_background gdk_window_set_background

#define atsctrb_gdk_window_invalidate_rect gdk_window_invalidate_rect

#define atsctrb_gdk_window_process_updates gdk_window_process_updates
#define atsctrb_gdk_window_process_all_updates gdk_window_process_all_updates

/* ****** ****** */

#endif // end of [ATSCTRB_GTK_GDK_CATS]
