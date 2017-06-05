/* $Id: screens.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 ****************************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 * mwm - "LessTif Window Manager"
 ***********************************************************************/

#include <LTconfig.h>

#include <stdio.h>

#include <X11/keysym.h>

#include <Xm/XmP.h>
#include <Xm/MwmUtil.h>
#include <XmI/XmI.h>

#include "mwm.h"


/* amai: Unfortunately X11 forgot about non-case-sensitive filesystems which
   may have problems to store both "X11/bitmaps/Stipple" and
   "X11/bitmaps/stipple".
   So we inline the few lines here instead. */
#if 0
#include <X11/bitmaps/stipple>
#else
#define stipple_width 16
#define stipple_height 4
static char stipple_bits[] = {
   0x55, 0x55, 0xee, 0xee, 0x55, 0x55, 0xba, 0xbb};
#endif /* #if 0 */


/*
 * create the GC's for the menus
 */
static void
create_menu_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth |
	GCForeground | GCBackground | GCFont;
    gcv.line_width = 0;
    gcv.plane_mask = AllPlanes;
    gcv.function = GXcopy;
    if (scr->components[MWM_MENU].font_list == NULL)
	scr->components[MWM_MENU].font_list = _XmFontListCreateDefault(dpy);
   _XmFontListGetDefaultFont(scr->components[MWM_MENU].font_list,
	                     &scr->components[MWM_MENU].font);
    gcv.font = scr->components[MWM_MENU].font->fid;
    if (scr->components[MWM_MENU].font)
    {
	scr->components[MWM_MENU].f_height =
	    scr->components[MWM_MENU].font->ascent +
	    scr->components[MWM_MENU].font->descent;
	scr->components[MWM_MENU].f_y =
	    scr->components[MWM_MENU].font->ascent;
    }

    /*
     * Prevent GraphicsExpose and NoExpose events.  We'd only get NoExpose
     * events anyway;  they cause BadWindow errors from XGetWindowAttributes
     * call in FindScreenInfo (events.c) (since drawable is a pixmap).
     */
    gcv.graphics_exposures = False;

    gcv.foreground = scr->components[MWM_MENU].foreground;
    gcv.background = scr->components[MWM_MENU].background;
    scr->components[MWM_MENU].normal_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    /* FIXME - not a very good grayed out color */
    scr->components[MWM_MENU].grayed_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    {
        Pixmap stipple = XCreateBitmapFromData(dpy, scr->root_win, stipple_bits,
                                     stipple_width, stipple_height);
        if (stipple) {
            XSetFillStyle(dpy, scr->components[MWM_MENU].grayed_GC,
                          FillOpaqueStippled);
            XSetStipple(dpy, scr->components[MWM_MENU].grayed_GC,
                        stipple);
        }
    }

    if (scr->components[MWM_MENU].top_shadow_pixmap != None &&
	scr->components[MWM_MENU].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MENU].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MENU].top_shadow_color;
	gcv.background = scr->components[MWM_MENU].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MENU].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MENU].bottom_shadow_pixmap != None &&
	scr->components[MWM_MENU].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MENU].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MENU].bottom_shadow_color;
	gcv.background = scr->components[MWM_MENU].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MENU].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * create the GC's for the feedback
 */
static void
create_feedback_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth |
	GCForeground | GCBackground | GCFont;
    gcv.line_width = 0;
    gcv.function = GXcopy;
    gcv.plane_mask = AllPlanes;
    if (scr->components[MWM_FEEDBACK].font_list == NULL)
	scr->components[MWM_FEEDBACK].font_list = _XmFontListCreateDefault(dpy);
    _XmFontListGetDefaultFont(scr->components[MWM_FEEDBACK].font_list,
                              &scr->components[MWM_FEEDBACK].font);
    gcv.font = scr->components[MWM_FEEDBACK].font->fid;
    if (scr->components[MWM_FEEDBACK].font)
    {
	scr->components[MWM_FEEDBACK].f_height =
	    scr->components[MWM_FEEDBACK].font->ascent +
	    scr->components[MWM_FEEDBACK].font->descent;
	scr->components[MWM_FEEDBACK].f_y =
	    scr->components[MWM_FEEDBACK].font->ascent;
    }

    /*
     * Prevent GraphicsExpose and NoExpose events.  We'd only get NoExpose
     * events anyway;  they cause BadWindow errors from XGetWindowAttributes
     * call in FindScreenInfo (events.c) (since drawable is a pixmap).
     */
    gcv.graphics_exposures = False;

    gcv.foreground = scr->components[MWM_FEEDBACK].foreground;
    gcv.background = scr->components[MWM_FEEDBACK].background;

    scr->components[MWM_FEEDBACK].normal_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    scr->components[MWM_FEEDBACK].grayed_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
    XSetFillStyle(dpy, scr->components[MWM_FEEDBACK].grayed_GC, FillOpaqueStippled);

    if (scr->components[MWM_FEEDBACK].top_shadow_pixmap != None &&
	scr->components[MWM_FEEDBACK].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_FEEDBACK].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_FEEDBACK].top_shadow_color;
	gcv.background = scr->components[MWM_FEEDBACK].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_FEEDBACK].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_FEEDBACK].bottom_shadow_pixmap != None &&
    scr->components[MWM_FEEDBACK].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_FEEDBACK].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_FEEDBACK].bottom_shadow_color;
	gcv.background = scr->components[MWM_FEEDBACK].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_FEEDBACK].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

static void
create_pager_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth |
	GCForeground | GCBackground | GCFont;
    gcv.line_width = 0;
    gcv.function = GXcopy;
    gcv.plane_mask = AllPlanes;
    if (scr->components[MWM_PAGER].font_list == NULL)
	scr->components[MWM_PAGER].font_list = _XmFontListCreateDefault(dpy);
    _XmFontListGetDefaultFont(scr->components[MWM_PAGER].font_list,
	            &scr->components[MWM_PAGER].font);
    gcv.font = scr->components[MWM_PAGER].font->fid;
    if (scr->components[MWM_PAGER].font)
    {
	scr->components[MWM_PAGER].f_height =
	    scr->components[MWM_PAGER].font->ascent +
	    scr->components[MWM_PAGER].font->descent;
	scr->components[MWM_PAGER].f_y =
	    scr->components[MWM_PAGER].font->ascent;
    }

    /*
     * Prevent GraphicsExpose and NoExpose events.  We'd only get NoExpose
     * events anyway;  they cause BadWindow errors from XGetWindowAttributes
     * call in FindScreenInfo (events.c) (since drawable is a pixmap).
     */
    gcv.graphics_exposures = False;

    gcv.foreground = scr->components[MWM_PAGER].foreground;
    gcv.background = scr->components[MWM_PAGER].background;
    scr->components[MWM_PAGER].normal_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
    scr->components[MWM_PAGER].grayed_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
    XSetFillStyle(dpy, scr->components[MWM_PAGER].grayed_GC, FillOpaqueStippled);
}

/*
 * create the GC's for the icons
 */
static void
create_icon_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth |
	GCForeground | GCBackground | GCFont;
    gcv.line_width = 0;
    gcv.function = GXcopy;
    gcv.plane_mask = AllPlanes;
    if (scr->components[MWM_ICON].font_list == NULL)
	scr->components[MWM_ICON].font_list = _XmFontListCreateDefault(dpy);
    _XmFontListGetDefaultFont(scr->components[MWM_ICON].font_list,
	            &scr->components[MWM_ICON].font);
    gcv.font = scr->components[MWM_ICON].font->fid;
    if (scr->components[MWM_ICON].font)
    {
	scr->components[MWM_ICON].f_height =
	    scr->components[MWM_ICON].font->ascent +
	    scr->components[MWM_ICON].font->descent;
	scr->components[MWM_ICON].f_y =
	    scr->components[MWM_ICON].font->ascent;
    }

    /*
     * Prevent GraphicsExpose and NoExpose events.  We'd only get NoExpose
     * events anyway;  they cause BadWindow errors from XGetWindowAttributes
     * call in FindScreenInfo (events.c) (since drawable is a pixmap).
     */
    gcv.graphics_exposures = False;

    gcv.foreground = scr->components[MWM_ICON].foreground;
    gcv.background = scr->components[MWM_ICON].background;

    scr->components[MWM_ICON].normal_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
    scr->components[MWM_ICON].grayed_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
    XSetFillStyle(dpy, scr->components[MWM_ICON].grayed_GC, FillStippled);

    if (scr->components[MWM_ICON].top_shadow_pixmap != None &&
	scr->components[MWM_ICON].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_ICON].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_ICON].top_shadow_color;
	gcv.background = scr->components[MWM_ICON].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_ICON].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_ICON].bottom_shadow_pixmap != None &&
	scr->components[MWM_ICON].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_ICON].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_ICON].bottom_shadow_color;
	gcv.background = scr->components[MWM_ICON].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_ICON].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth |
	GCForeground | GCBackground | GCFont;
    gcv.line_width = 0;
    gcv.function = GXcopy;
    gcv.plane_mask = AllPlanes;
    gcv.font = scr->components[MWM_ICON].font->fid;

    /*
     * Prevent GraphicsExpose and NoExpose events.  We'd only get NoExpose
     * events anyway;  they cause BadWindow errors from XGetWindowAttributes
     * call in FindScreenInfo (events.c) (since drawable is a pixmap).
     */
    gcv.graphics_exposures = False;

    gcv.foreground = scr->components[MWM_ICON].active_foreground;
    gcv.background = scr->components[MWM_ICON].active_background;

    scr->components[MWM_ICON].active_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_ICON].active_top_shadow_pixmap != None &&
    scr->components[MWM_ICON].active_top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_ICON].active_top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_ICON].active_top_shadow_color;
	gcv.background = scr->components[MWM_ICON].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_ICON].active_top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_ICON].active_bottom_shadow_pixmap != None &&
	scr->components[MWM_ICON].active_bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_ICON].active_bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_ICON].active_bottom_shadow_color;
	gcv.background = scr->components[MWM_ICON].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_ICON].active_bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * create the GC's for the title_area
 */
static void
create_title_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth |
	GCForeground | GCBackground | GCFont;
    gcv.line_width = 0;
    gcv.function = GXcopy;
    gcv.plane_mask = AllPlanes;
    if (scr->components[MWM_TITLE_A].font_list == NULL)
	scr->components[MWM_TITLE_A].font_list = _XmFontListCreateDefault(dpy);
    _XmFontListGetDefaultFont(scr->components[MWM_TITLE_A].font_list,
                    &scr->components[MWM_TITLE_A].font);
    gcv.font = scr->components[MWM_TITLE_A].font->fid;
    if (scr->components[MWM_TITLE_A].font)
    {
	scr->components[MWM_TITLE_A].f_height =
	    scr->components[MWM_TITLE_A].font->ascent +
	    scr->components[MWM_TITLE_A].font->descent;
	scr->components[MWM_TITLE_A].f_y =
	    scr->components[MWM_TITLE_A].font->ascent;
    }

    /*
     * Prevent GraphicsExpose and NoExpose events.  We'd only get NoExpose
     * events anyway;  they cause BadWindow errors from XGetWindowAttributes
     * call in FindScreenInfo (events.c) (since drawable is a pixmap).
     */
    gcv.graphics_exposures = False;

    gcv.foreground = scr->components[MWM_TITLE_A].foreground;
    gcv.background = scr->components[MWM_TITLE_A].background;

    scr->components[MWM_TITLE_A].normal_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
    scr->components[MWM_TITLE_A].grayed_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
    XSetFillStyle(dpy, scr->components[MWM_TITLE_A].grayed_GC, FillOpaqueStippled);

    if (scr->components[MWM_TITLE_A].top_shadow_pixmap != None &&
	scr->components[MWM_TITLE_A].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_TITLE_A].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_TITLE_A].top_shadow_color;
	gcv.background = scr->components[MWM_TITLE_A].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_TITLE_A].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_TITLE_A].bottom_shadow_pixmap != None &&
     scr->components[MWM_TITLE_A].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_TITLE_A].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_TITLE_A].bottom_shadow_color;
	gcv.background = scr->components[MWM_TITLE_A].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_TITLE_A].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    gcm = GCFunction | GCPlaneMask | GCGraphicsExposures | GCLineWidth |
	GCForeground | GCBackground | GCFont;
    gcv.line_width = 0;
    gcv.function = GXcopy;
    gcv.plane_mask = AllPlanes;
    gcv.font = scr->components[MWM_TITLE_A].font->fid;

    /*
     * Prevent GraphicsExpose and NoExpose events.  We'd only get NoExpose
     * events anyway;  they cause BadWindow errors from XGetWindowAttributes
     * call in FindScreenInfo (events.c) (since drawable is a pixmap).
     */
    gcv.graphics_exposures = False;

    gcv.foreground = scr->components[MWM_TITLE_A].active_foreground;
    gcv.background = scr->components[MWM_TITLE_A].active_background;

    scr->components[MWM_TITLE_A].active_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_TITLE_A].active_top_shadow_pixmap != None &&
	scr->components[MWM_TITLE_A].active_top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_TITLE_A].active_top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_TITLE_A].active_top_shadow_color;
	gcv.background = scr->components[MWM_TITLE_A].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_TITLE_A].active_top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_TITLE_A].active_bottom_shadow_pixmap != None &&
	scr->components[MWM_TITLE_A].active_bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_TITLE_A].active_bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_TITLE_A].active_bottom_shadow_color;
	gcv.background = scr->components[MWM_TITLE_A].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_TITLE_A].active_bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * create the GC's for the resize handles
 */
static void
create_resize_h_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    if (scr->components[MWM_RESIZE_H].top_shadow_pixmap != None &&
	scr->components[MWM_RESIZE_H].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_RESIZE_H].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_RESIZE_H].top_shadow_color;
	gcv.background = scr->components[MWM_RESIZE_H].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_RESIZE_H].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_RESIZE_H].bottom_shadow_pixmap != None &&
    scr->components[MWM_RESIZE_H].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_RESIZE_H].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_RESIZE_H].bottom_shadow_color;
	gcv.background = scr->components[MWM_RESIZE_H].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_RESIZE_H].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_RESIZE_H].active_top_shadow_pixmap != None &&
	scr->components[MWM_RESIZE_H].active_top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_RESIZE_H].active_top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_RESIZE_H].active_top_shadow_color;
	gcv.background = scr->components[MWM_RESIZE_H].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_RESIZE_H].active_top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_RESIZE_H].active_bottom_shadow_pixmap != None &&
	scr->components[MWM_RESIZE_H].active_bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_RESIZE_H].active_bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_RESIZE_H].active_bottom_shadow_color;
	gcv.background = scr->components[MWM_RESIZE_H].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_RESIZE_H].active_bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * create the GC's for the border
 */
static void
create_border_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    if (scr->components[MWM_BORDER].top_shadow_pixmap != None &&
	scr->components[MWM_BORDER].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_BORDER].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_BORDER].top_shadow_color;
	gcv.background = scr->components[MWM_BORDER].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_BORDER].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_BORDER].bottom_shadow_pixmap != None &&
      scr->components[MWM_BORDER].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_BORDER].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_BORDER].bottom_shadow_color;
	gcv.background = scr->components[MWM_BORDER].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_BORDER].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_BORDER].active_top_shadow_pixmap != None &&
	scr->components[MWM_BORDER].active_top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_BORDER].active_top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_BORDER].active_top_shadow_color;
	gcv.background = scr->components[MWM_BORDER].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_BORDER].active_top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_BORDER].active_bottom_shadow_pixmap != None &&
	scr->components[MWM_BORDER].active_bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_BORDER].active_bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_BORDER].active_bottom_shadow_color;
	gcv.background = scr->components[MWM_BORDER].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_BORDER].active_bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * create the GC's for the maximize button
 */
static void
create_maximize_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    if (scr->components[MWM_MAXIMIZE_B].top_shadow_pixmap != None &&
     scr->components[MWM_MAXIMIZE_B].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MAXIMIZE_B].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MAXIMIZE_B].top_shadow_color;
	gcv.background = scr->components[MWM_MAXIMIZE_B].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MAXIMIZE_B].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MAXIMIZE_B].bottom_shadow_pixmap != None &&
	scr->components[MWM_MAXIMIZE_B].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MAXIMIZE_B].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MAXIMIZE_B].bottom_shadow_color;
	gcv.background = scr->components[MWM_MAXIMIZE_B].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MAXIMIZE_B].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MAXIMIZE_B].active_top_shadow_pixmap != None &&
	scr->components[MWM_MAXIMIZE_B].active_top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MAXIMIZE_B].active_top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MAXIMIZE_B].active_top_shadow_color;
	gcv.background = scr->components[MWM_MAXIMIZE_B].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MAXIMIZE_B].active_top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MAXIMIZE_B].active_bottom_shadow_pixmap != None &&
	scr->components[MWM_MAXIMIZE_B].active_bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MAXIMIZE_B].active_bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MAXIMIZE_B].active_bottom_shadow_color;
	gcv.background = scr->components[MWM_MAXIMIZE_B].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MAXIMIZE_B].active_bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * create the GC's for the maximize button
 */
static void
create_minimize_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    if (scr->components[MWM_MINIMIZE_B].top_shadow_pixmap != None &&
     scr->components[MWM_MINIMIZE_B].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MINIMIZE_B].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MINIMIZE_B].top_shadow_color;
	gcv.background = scr->components[MWM_MINIMIZE_B].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MINIMIZE_B].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MINIMIZE_B].bottom_shadow_pixmap != None &&
	scr->components[MWM_MINIMIZE_B].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MINIMIZE_B].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MINIMIZE_B].bottom_shadow_color;
	gcv.background = scr->components[MWM_MINIMIZE_B].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MINIMIZE_B].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MINIMIZE_B].active_top_shadow_pixmap != None &&
	scr->components[MWM_MINIMIZE_B].active_top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MINIMIZE_B].active_top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MINIMIZE_B].active_top_shadow_color;
	gcv.background = scr->components[MWM_MINIMIZE_B].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MINIMIZE_B].active_top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MINIMIZE_B].active_bottom_shadow_pixmap != None &&
	scr->components[MWM_MINIMIZE_B].active_bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MINIMIZE_B].active_bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MINIMIZE_B].active_bottom_shadow_color;
	gcv.background = scr->components[MWM_MINIMIZE_B].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MINIMIZE_B].active_bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * create the GC's for the maximize button
 */
static void
create_menu_b_gcs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    if (scr->components[MWM_MENU_B].top_shadow_pixmap != None &&
	scr->components[MWM_MENU_B].top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MENU_B].top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MENU_B].top_shadow_color;
	gcv.background = scr->components[MWM_MENU_B].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MENU_B].top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MENU_B].bottom_shadow_pixmap != None &&
      scr->components[MWM_MENU_B].bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MENU_B].bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MENU_B].bottom_shadow_color;
	gcv.background = scr->components[MWM_MENU_B].background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MENU_B].bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MENU_B].active_top_shadow_pixmap != None &&
	scr->components[MWM_MENU_B].active_top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MENU_B].active_top_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MENU_B].active_top_shadow_color;
	gcv.background = scr->components[MWM_MENU_B].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MENU_B].active_top_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);

    if (scr->components[MWM_MENU_B].active_bottom_shadow_pixmap != None &&
	scr->components[MWM_MENU_B].active_bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
    {
	gcm = GCTile | GCFillStyle;

	gcv.tile = scr->components[MWM_MENU_B].active_bottom_shadow_pixmap;
	gcv.fill_style = FillTiled;
    }
    else
    {
	gcm = GCForeground | GCBackground;

	gcv.foreground = scr->components[MWM_MENU_B].active_bottom_shadow_color;
	gcv.background = scr->components[MWM_MENU_B].active_background;
    }
    gcm |= GCLineWidth | GCLineStyle | GCCapStyle |
	GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;
    scr->components[MWM_MENU_B].active_bot_GC =
	XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * open fonts and create all the needed GC's
 *
 */
static void
create_GCs(ScreenInfo *scr)
{
    XGCValues gcv;
    unsigned long gcm;

    /* create the decoration GC's */
    create_menu_gcs(scr);
    create_feedback_gcs(scr);
    create_pager_gcs(scr);
    create_icon_gcs(scr);
    create_title_gcs(scr);
    create_resize_h_gcs(scr);
    create_border_gcs(scr);
    create_maximize_gcs(scr);
    create_minimize_gcs(scr);
    create_menu_b_gcs(scr);

    /* create the resize GC */
    gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
    gcv.function = GXxor;
    gcv.line_width = 0;
    gcv.foreground = (((unsigned long)1) << scr->d_depth) - 1;

    gcv.subwindow_mode = IncludeInferiors;
    scr->resize_GC = XCreateGC(dpy, scr->root_win, gcm, &gcv);

    /* create the matte top shadow GC */
    gcm = GCLineWidth | GCLineStyle | GCCapStyle | GCGraphicsExposures;
    gcv.line_width = 0;
    gcv.line_style = LineSolid;
    gcv.cap_style = CapButt;
    gcv.graphics_exposures = False;

    scr->matte_ts_GC = XCreateGC(dpy, scr->root_win, gcm, &gcv);
    scr->matte_bs_GC = XCreateGC(dpy, scr->root_win, gcm, &gcv);
}

/*
 * Find out what the Alt key is.  This code came from libXm/VirtKeys.
 */
static int
get_modifier_mapping(Display *Dsp)
{
    XModifierKeymap *mk;
    KeySym ModifierKeysym;
    int ModifierSet, SetIndex, SetSize;
    int AltMask = Mod1Mask;

    /*
     * Ask the server for the current modifier mapping and parse it for
     * the META and ALT keysyms. If one of them is bound to a modifier,
     * then remember the mask of that modifier.
     */
    mk = XGetModifierMapping(Dsp);
    SetSize = mk->max_keypermod;

    for (ModifierSet = 0; ModifierSet < 8; ModifierSet++)
    {
	for (SetIndex = 0; SetIndex < SetSize; SetIndex++)
	{
	    ModifierKeysym =
		XKeycodeToKeysym(Dsp,
			     mk->modifiermap[SetIndex + ModifierSet * SetSize],
				 0);
	    switch (ModifierKeysym)
	    {
		/*
		 * If we've found one of the well known modifier keysyms,
		 * we'll remember the modifier - but only if we havn't yet
		 * seen another one.
		 */
	    case XK_Meta_L:
	    case XK_Meta_R:
		AltMask = 1L << ModifierSet;
		goto bailout;
	    case XK_Alt_L:
	    case XK_Alt_R:
		AltMask = 1L << ModifierSet;
		goto bailout;
	    }
	}
    }

  bailout:
    /*
     * And don't forget to clean up, we don't want to waste memory here --
     * because we're not the Closed Software Foundation! And then a last
     * paranoic checking for missing ALT modifiers...
     */
    XFreeModifiermap(mk);

    return AltMask;
}

/*
 * initialize one screen.  returns a boolean indicating whether or not the
 * DESKTOP property was found
 */
Boolean
SCREEN_Initialize(ScreenInfo *scr)
{
    XSetWindowAttributes attributes;
    unsigned long valuemask;
    Boolean restart = False;
    int ssw;

    PROP_SetPriorityColors(scr);

    RES_GetScreenDefaults(scr);
    RES_GetComponentDefaults(scr);

    MWM_SetErrorHandler(REDIRECT);

    XSelectInput(dpy, scr->root_win,
		 LeaveWindowMask | EnterWindowMask | PropertyChangeMask |
		 SubstructureRedirectMask | KeyPressMask |
		 ButtonPressMask | ButtonReleaseMask);

    XSync(dpy, 0);

    MWM_SetErrorHandler(GENERAL);

    scr->d_width = DisplayWidth(dpy, scr->screen);
    scr->d_height = DisplayHeight(dpy, scr->screen);
    scr->d_depth = DefaultDepth(dpy, scr->screen);

    CURS_Initialize(scr);

    valuemask = CWCursor | CWDontPropagate;
    attributes.cursor = scr->cursors[HOURGLASS_CURS];
    attributes.do_not_propagate_mask = -1;

    scr->mwm_root.w = scr->root_win;
    scr->mwm_root.next = NULL;
    XGetWindowAttributes(dpy, scr->root_win, &scr->mwm_root.attr);
    scr->root_pushes = 0;
    scr->mwm_pushed = &scr->mwm_root;
    scr->mwm_root.number_cmap_windows = 0;
    scr->event_context = C_NO_CONTEXT;	/* current button press context */


    scr->mwm_highlight = NULL;
    scr->mwm_focus = NULL;
    scr->mwm_grabbing = NULL;

    scr->virt_scale = 32;
    scr->virt_x = scr->virt_y = 0;

    restart = PROP_CheckDesktop(scr);

    scr->ScrollResistance = scr->MoveResistance = 250;
    scr->OpaqueSize = 5;

    /* set major operating modes */
    scr->flags = 0;
    scr->num_icon_boxes = 1;
    scr->icon_boxes[0][0] = 0;
    scr->icon_boxes[0][1] = scr->d_height - 75;
    scr->icon_boxes[0][2] = scr->d_width;
    scr->icon_boxes[0][3] = scr->d_height;

    scr->buttons2grab = 7;

    scr->mwm_pager = NULL;
    scr->pager_win = None;
    scr->pager_child_win = None;
    scr->pressed_win = None;
    scr->restart_win = None;
    scr->quit_win = None;
    scr->toggle_win = None;

    /* initialize some lists */
    scr->buttons = NULL;
    scr->keys = NULL;
    scr->DefaultIcon = NULL;

    scr->IconPath = Mwm.bitmap_directory;
    scr->PixmapPath = Mwm.bitmap_directory;

    /* create graphics contexts */
    create_GCs(scr);

    XSync(dpy, 0);

    /* find our alt key */
    scr->alt_mask = get_modifier_mapping(dpy);

    PARSE_mwmrc(scr);

    MENU_LinkUp(scr);

    MENU_RealizeMenus(scr);

    if (Mwm.edge_scroll_x == XmUNSPECIFIED)
	scr->edge_scroll_x = scr->d_width;
    else
	scr->edge_scroll_x = Mwm.edge_scroll_x;

    if (Mwm.edge_scroll_y == XmUNSPECIFIED)
	scr->edge_scroll_y = scr->d_height;
    else
	scr->edge_scroll_x = Mwm.edge_scroll_x;

    if (Mwm.pager_x == XmUNSPECIFIED)
	scr->pager_x = 0;
    else
	scr->pager_x = Mwm.pager_x;

    if (Mwm.pager_y == XmUNSPECIFIED)
	Mwm.pager_y = 0;
    else
	scr->pager_y = Mwm.pager_y;

    scr->virt_x_max = Mwm.virtual_x * scr->d_width - scr->d_width;
    scr->virt_y_max = Mwm.virtual_y * scr->d_height - scr->d_height;
    if (scr->virt_x_max < 0)
	scr->virt_x_max = 0;
    if (scr->virt_y_max < 0)
	scr->virt_y_max = 0;

    if (Mwm.use_pager)
	PAGER_Initialize(scr, scr->pager_x, scr->pager_y);

    scr->smart_placement = Mwm.smart_placement;

    XSync(dpy, 0);

    XGrabServer(dpy);

    attributes.event_mask = KeyPressMask | ButtonPressMask;
    attributes.cursor = scr->cursors[HOURGLASS_CURS];
    attributes.override_redirect = True;
    scr->shield_win = XCreateWindow(dpy, scr->root_win, 0, 0,
				    scr->d_width, scr->d_height, 0, 0,
				    InputOnly, CopyFromParent,
				    CWEventMask | CWOverrideRedirect |
				    CWCursor,
				    &attributes);

    XMapRaised(dpy, scr->shield_win);

    /* create a window which will accept the keyboard focus when no other 
       windows have it */
    attributes.event_mask = KeyPressMask | FocusChangeMask;
    attributes.override_redirect = True;
    scr->no_focus_win = XCreateWindow(dpy, scr->root_win, -10, -10, 10, 10, 0, 0,
				      InputOnly, CopyFromParent,
				      CWEventMask | CWOverrideRedirect,
				      &attributes);
    XStoreName(dpy, scr->no_focus_win, "NoFocusWin");
    XMapWindow(dpy, scr->no_focus_win);

    XSetInputFocus(dpy, scr->no_focus_win, RevertToParent, CurrentTime);

    XSync(dpy, 0);
    if (debugging)
	XSynchronize(dpy, 1);

    ssw = XTextWidth(scr->components[MWM_FEEDBACK].font,
		     " +8888 x +8888 ", 15);
    attributes.border_pixel = scr->components[MWM_FEEDBACK].foreground;
    attributes.background_pixel = scr->components[MWM_FEEDBACK].background;
    attributes.bit_gravity = NorthWestGravity;
    valuemask = (CWBorderPixel | CWBackPixel | CWBitGravity);
    scr->size_win =
	XCreateWindow(dpy, scr->root_win,
		      (scr->d_width - (ssw + SIZE_HINDENT * 2)) / 2,
		      (scr->d_height - (scr->components[MWM_FEEDBACK].f_height +
					SIZE_VINDENT * 2)) / 2,
		      (unsigned int)(ssw + SIZE_HINDENT * 2),
		      (unsigned int)(scr->components[MWM_FEEDBACK].f_height +
				     SIZE_VINDENT * 2),
		      (unsigned int)0, 0,
		      (unsigned int)CopyFromParent,
		      (Visual *)CopyFromParent,
		      valuemask, &attributes);

    PAN_Initialize(scr);

    WIN_CaptureWindows(scr);

    MISC_FixupTransients(scr);

    PAN_CheckBounds(scr);

    XUnmapWindow(dpy, scr->shield_win);

    XUngrabServer(dpy);

    PAGER_UpdateViewPort(scr);

    valuemask = CWCursor;
    attributes.cursor = scr->cursors[SYS_MODAL_CURS];
    XChangeWindowAttributes(dpy, scr->shield_win, valuemask, &attributes);

    return restart;
}

/*
 * from an event, figure out the screen structure
 */
ScreenInfo *
SCREEN_EventToStruct(XEvent *event)
{
    XAnyEvent *ev = (XAnyEvent *)event;
    int i;

    for (i = 0; i < Mwm.number_of_screens; i++)
    {
	if (ev->window == Mwm.screen_info[i]->root_win)
	    return Mwm.screen_info[i];
    }
    XGetGeometry(dpy, ev->window, &JunkRoot, &JunkX, &JunkY,
		 &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
    for (i = 0; i < Mwm.number_of_screens; i++)
    {
	if (JunkRoot == Mwm.screen_info[i]->root_win)
	    return Mwm.screen_info[i];
    }
    fprintf(stderr, "Event has no screen!\n");
    return NULL;
}
