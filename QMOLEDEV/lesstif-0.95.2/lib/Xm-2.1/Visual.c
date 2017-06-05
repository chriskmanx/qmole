/**
 *
 * $Id: Visual.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Id: Visual.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <limits.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/GadgetP.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShellP.h>
#include <Xm/PrimitiveP.h>

#include <Xm/VendorSEP.h>

#include <XmI/DebugUtil.h>


#define	COLOR_CACHE_SIZE		5

#define	PCT_BRIGHTNESS			(6 * 0xffff / 100)

/* How much lighter/darker to make things in default routine */

#define	PCT_DARK_BOTTOM		70	/* lighter (less dark, actually) */
#define	PCT_DARK_TOP		50	/* lighter */
#define	PCT_LIGHT_BOTTOM	55	/* darker */
#define	PCT_LIGHT_TOP		80	/* darker */
#define	PCT_MEDIUM_BOTTOM_BASE	40	/* darker */
#define	PCT_MEDIUM_BOTTOM_RANGE	25
#define	PCT_MEDIUM_TOP_BASE	60	/* lighter */
#define	PCT_MEDIUM_TOP_RANGE	-30
#define	PCT_SELECT		85	/* darker (lighter if "dark") */

/* The "brightness" of an RGB color.  The "right" way seems to use
 * empirical values like the default thresholds below, but it boils down
 * to red is twice as bright as blue and green is thrice blue.
 */

#define	BRIGHTNESS(xc)		((xc).red + (xc).red + (xc).green + \
				 (xc).green + (xc).green + (xc).blue)

static Pixel black_or_white(XmColorData *cd, unsigned char which);
static void call_color_proc(XmColorData *cd);
static void _XmColorProcDefaultProc(XColor *bg_color, XColor *fg_color,
				    XColor *sel_color, XColor *ts_color,
				    XColor *bs_color);

static XContext background_color_context, background_spec_context;
static XmColorProc _color_proc = _XmColorProcDefaultProc;
static XmColorData color_cache[COLOR_CACHE_SIZE];

/*
 *  Note: 
 *  
 * At one time there was no cache in the color routines. When displaying
 * remotely, profile data indicated that the vast majority of the time
 * spent in _XtCreateWidget was spent getting colors from these routines.
 * In Aug 1997, Jon Christopher (jac8792@tamu.edu) added a cache to each
 * of these routines so that it will remember the data from the last call
 * and avoid server round trips if they are called again under the same
 * conditions.  These changes speeded up _XtCreateWidget by 10 times.
 *
 *  More recently (Sept 1998), Jamie Gritton (jamie@gritton.org) improved
 *  the cache by using the proper _Xm* routines as defined in XmP.h.
 *  Jamie has also implemented routines which use better default color
 *  calculations based on Motif-like empirical brightness calculations.
 *  Thanks, Jamie!  */


static void color_fail_warn(void)
{
  static int warned = False;
  
  if (!warned) {
    warned=True;
    _XmWarning(NULL,
	       "Cannot allocate colormap entry, some colors may be incorrect");
  }
}

XmColorProc
XmSetColorCalculation(XmColorProc proc)
{
    XmColorProc ret_val;

    ret_val = _color_proc;

    if (proc == NULL)
    {
	_color_proc = _XmColorProcDefaultProc;
    }
    else
    {
	_color_proc = proc;
    }

    _XmInvalidateColorCache( False);

    return ret_val;
}

XmColorProc
XmGetColorCalculation(void)
{
    return _color_proc;
}

void
_XmInvalidateColorCache(Boolean default_only)
{
    XmColorData *cc;

    if (!default_only || _color_proc == _XmColorProcDefaultProc)
	for (cc = color_cache; cc < color_cache + COLOR_CACHE_SIZE; cc++)
	    cc->allocated = 0;
}

/* Get a color cache entry */

XmColorData *
_XmGetColors(Screen *screen, Colormap color_map, Pixel background)
{
    XmColorData cd;
    XmColorData *cc;

    /* Construct a new blank record to possibly add */

    cd.allocated = 0;
    cd.screen = screen;
    cd.color_map = color_map;
    cd.background.pixel = background;
    cd.foreground.pixel =
	cd.top_shadow.pixel =
	cd.bottom_shadow.pixel =
	cd.select.pixel = 0;

    /* Get it from the cache, putting it in if necessary */

    cc = _XmAddToColorCache(&cd);

    /* Call the color proc if the entry is new */

    if (!(cc->allocated & XmBACKGROUND))
    {
	cc->allocated |= XmBACKGROUND;
	XQueryColor(DisplayOfScreen(screen),
		    color_map,
		    &cc->background);
	call_color_proc(cc);
    }

    return cc;
}

/* Get the colors for a widget with a default background */

XmColorData *
_XmGetDefaultColors(Screen *screen, Colormap color_map)
{
    Widget sw = XmGetXmScreen(screen);
    Colormap screen_color_map;
    XrmValue val;

    /* Get the background pixel */

    screen_color_map = CoreColormap(sw);
    CoreColormap(sw) = color_map;
    _XmBackgroundColorDefault(sw, 0, &val);
    CoreColormap(sw) = screen_color_map;

    /* _XmBackgroundColorDefault always leaves its mark
     * on the front of the color cache.
     */

    return color_cache;
}

/* Search the color cache for a matching entry */

Boolean
_XmSearchColorCache(unsigned int which, XmColorData *values,
		    XmColorData **ret)
{
    XmColorData *cc;

    for (cc = color_cache; cc < color_cache + COLOR_CACHE_SIZE; cc++)
	if (cc->allocated &&
	    (!(which & XmLOOK_AT_SCREEN) ||
	     cc->screen == values->screen) &&
	    (!(which & XmLOOK_AT_CMAP) ||
	     cc->color_map == values->color_map) &&
	    (!(which & XmLOOK_AT_BACKGROUND) ||
	     cc->background.pixel == values->background.pixel) &&
	    (!(which & XmLOOK_AT_FOREGROUND) ||
	     ((cc->allocated & XmFOREGROUND) &&
	      cc->foreground.pixel == values->foreground.pixel)) &&
	    (!(which & XmLOOK_AT_TOP_SHADOW) ||
	     ((cc->allocated & XmTOP_SHADOW) &&
	      cc->top_shadow.pixel == values->top_shadow.pixel)) &&
	    (!(which & XmLOOK_AT_BOTTOM_SHADOW) ||
	     ((cc->allocated & XmBOTTOM_SHADOW) &&
	      cc->bottom_shadow.pixel == values->bottom_shadow.pixel)) &&
	    (!(which & XmLOOK_AT_SELECT) ||
	     ((cc->allocated & XmSELECT) &&
	      cc->select.pixel == values->select.pixel)))
	{
	    /* Found the entry.  Bump it to the front of the cache
	     * to find it quicker next time, and to make this an LRU cache.
	     * Kind of expensive perhaps, but for a small cache that's fine.
	     */

	    if (cc > color_cache)
	    {
		XmColorData temp_cd = *cc;
		memmove(color_cache + 1, color_cache,
			(char *)cc - (char *)color_cache);
		*color_cache = temp_cd;
	    }
	    *ret = color_cache;
	    return True;
	}

    *ret = NULL;
    return False;
}

/* Parse/Allocate a background color and add it to the color cache */

void
_XmAddBackgroundToColorCache(Screen *screen, Colormap color_map,
			     String background_spec,
			     int rgb_fallback, XrmValue *val)
{
    XmColorData	cd;
    XmColorData *cc;
    static Pixel background;

    val->size = sizeof background;
    val->addr = (XPointer)&background;

    /* Parse the color.  If that fails, use the fallback RGB values */

    if (!XParseColor(DisplayOfScreen(screen), color_map, background_spec,
		     &cd.background))
    {
	_XmWarning(NULL, "Couldn't parse default background color - using fallback RGB values");
	cd.background.red =
	    (rgb_fallback >> 16 & 0xff) | (rgb_fallback >> 8 & 0xff00);
	cd.background.green =
	    (rgb_fallback >> 8 & 0xff) | (rgb_fallback & 0xff00);
	cd.background.blue =
	    (rgb_fallback & 0xff) | (rgb_fallback << 8 & 0xff00);
    }

    /* See if the color is in the cache.  This is like _XmSearchColorCache
     * except that it looks for background RGB values instead of pixels.
     */

    for (cc = color_cache; cc < color_cache + COLOR_CACHE_SIZE; cc++)
	if ((cc->allocated & XmBACKGROUND) &&
	    cc->screen == screen &&
	    cc->color_map == color_map &&
	    cc->background.red == cd.background.red &&
	    cc->background.green == cd.background.green &&
	    cc->background.blue == cd.background.blue)
	{
	    /* Found the entry.  Put in front like _XmSearchColorCache. */

	    if (cc > color_cache)
	    {
		cd = *cc;
		memmove(color_cache + 1, color_cache,
			(char *)cc - (char *)color_cache);
		*color_cache = cd;
	    }

	    /* It's already in the cache, so we're done */

	    background = color_cache->background.pixel;
	    return;
	}

    /* It's not in the cache.
     * Allocate it and put it in there (a la _XmGetColors).
     */

    if (!XAllocColor(DisplayOfScreen(screen), color_map, &cd.background))
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmAddBackgroundToColorCache(%d) - Could not allocate color\n",
			   __FILE__,__LINE__));
	color_fail_warn();
	cd.background.pixel =
	    _XmWhitePixel(screen, color_map, cd.background);
	cd.background.red = cd.background.green = cd.background.blue =
	    0xffff;
    }
    cd.screen = screen;
    cd.color_map = color_map;
    cd.allocated = XmBACKGROUND;
    cd.foreground.pixel =
	cd.top_shadow.pixel =
	cd.bottom_shadow.pixel =
	cd.select.pixel = 0;
    call_color_proc(_XmAddToColorCache(&cd));
    background = cd.background.pixel;
}

/* Add an entry to the color cache, or augment an existing entry */

XmColorData *
_XmAddToColorCache(XmColorData *new_rec)
{
    XmColorData *cc;

    if (_XmSearchColorCache(XmLOOK_AT_SCREEN | XmLOOK_AT_CMAP |
			    XmLOOK_AT_BACKGROUND, new_rec, &cc))
    {
	/* Entry exists - add new data */

	cc->allocated |= new_rec->allocated;
	if (new_rec->allocated & XmBACKGROUND)
	{
	    cc->background = new_rec->background;
	}
	if (new_rec->allocated & XmFOREGROUND)
	{
	    cc->foreground = new_rec->foreground;
	}
	if (new_rec->allocated & XmTOP_SHADOW)
	{
	    cc->top_shadow = new_rec->top_shadow;
	}
	if (new_rec->allocated & XmBOTTOM_SHADOW)
	{
	    cc->bottom_shadow = new_rec->bottom_shadow;
	}
	if (new_rec->allocated & XmSELECT)
	{
	    cc->select = new_rec->select;
	}
	return cc;
    }
    else
    {
	/* Key doesn't exist - add the entire entry */

	memmove(color_cache + 1, color_cache,
		(COLOR_CACHE_SIZE - 1) * sizeof( XmColorData));
	color_cache[0] = *new_rec;
	return color_cache;
    }
}

/* Get a field from a color cache entry, allocating it if necessary */

Pixel
_XmAccessColorData(XmColorData *cd, unsigned char which)
{
    switch(which)
    {
    case XmBACKGROUND:
	return cd->background.pixel;
    case XmFOREGROUND:
	if (!(cd->allocated & XmFOREGROUND))
	{
	    cd->allocated |= XmFOREGROUND;
	    if (!XAllocColor(DisplayOfScreen(cd->screen),
			     cd->color_map,
			     &cd->foreground))
	    {
		DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmAccessColorData(%d) - Could not allocate XmFOREGROUND\n",
				   __FILE__,__LINE__));
		color_fail_warn();
		cd->foreground.pixel = black_or_white(cd, XmFOREGROUND);
	    }
	}
	return cd->foreground.pixel;
    case XmTOP_SHADOW:
	if (!(cd->allocated & XmTOP_SHADOW))
	{
	    cd->allocated |= XmTOP_SHADOW;
	    if (!XAllocColor(DisplayOfScreen(cd->screen),
			     cd->color_map,
			     &cd->top_shadow))
	    {
		DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmAccessColorData(%d) - Could not allocate XmTOP_SHADOW\n",
				   __FILE__,__LINE__));
		color_fail_warn();
		cd->top_shadow.pixel = black_or_white(cd, XmTOP_SHADOW);
	    }
	}
	return cd->top_shadow.pixel;
    case XmBOTTOM_SHADOW:
	if (!(cd->allocated & XmBOTTOM_SHADOW))
	{
	    cd->allocated |= XmBOTTOM_SHADOW;
	    if (!XAllocColor(DisplayOfScreen(cd->screen),
			     cd->color_map,
			     &cd->bottom_shadow))
	    {
		DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmAccessColorData(%d) - Could not allocate XmBOTTOM_SHADOW\n",
				   __FILE__,__LINE__));
		color_fail_warn();
		cd->bottom_shadow.pixel = black_or_white(cd, XmBOTTOM_SHADOW);
	    }
	}
	return cd->bottom_shadow.pixel;
    case XmSELECT:
	if (!(cd->allocated & XmSELECT))
	{
	    cd->allocated |= XmSELECT;
	    if (!XAllocColor(DisplayOfScreen(cd->screen),
			     cd->color_map,
			     &cd->select))
	    {
		DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmAccessColorData(%d) - Could not allocate XmSELECT\n",
				   __FILE__,__LINE__));
		color_fail_warn();
		cd->select.pixel = black_or_white(cd, XmSELECT);
	    }
	}
	return cd->select.pixel;
    }

    _XmWarning(NULL, "Invalid color specification in _XmAccessColorData");
    return 0;
}

/* If a color is unavailable, choose black or white */

static Pixel
black_or_white(XmColorData *cd, unsigned char which)
{
    int threshold = 0;
    long brightness;
    Widget sw;

    brightness = BRIGHTNESS( cd->background);
    sw = XmGetXmScreen(cd->screen);

    switch (which)
    {
    case XmFOREGROUND:
	threshold = Screen_ForegroundThreshold(sw)
	    ? Screen_ForegroundThreshold(sw) : XmDEFAULT_FOREGROUND_THRESHOLD;
	break;
    case XmTOP_SHADOW:
    case XmSELECT:
	threshold = Screen_LightThreshold(sw)
	    ? Screen_LightThreshold(sw) : XmDEFAULT_LIGHT_THRESHOLD;
	break;
    case XmBOTTOM_SHADOW:
	threshold = Screen_DarkThreshold(sw)
	    ? Screen_DarkThreshold(sw) : XmDEFAULT_DARK_THRESHOLD;
    }
    return brightness > threshold * PCT_BRIGHTNESS
	? _XmBlackPixel(cd->screen, cd->color_map, cd->select)
	: _XmWhitePixel(cd->screen, cd->color_map, cd->select);
}

/* Call the color proc to fill in color data */

static void call_color_proc(XmColorData *cd)
{
    if (DefaultDepthOfScreen(cd->screen) == 1)
    {
	/* Don't call the color proc for B/W screens.
	 * Just do an easy "bw proc".
	 */

	cd->allocated |=
	    XmFOREGROUND | XmTOP_SHADOW | XmBOTTOM_SHADOW | XmSELECT;
	if (cd->background.pixel == WhitePixelOfScreen(cd->screen))
	{
	    cd->foreground.red = cd->foreground.green =
		cd->foreground.blue = cd->bottom_shadow.red =
		cd->bottom_shadow.green = cd->bottom_shadow.blue =
		cd->select.red = cd->select.green = cd->select.blue = 0;
	    cd->foreground.pixel = cd->bottom_shadow.pixel =
		cd->select.pixel = BlackPixelOfScreen(cd->screen);
	    cd->top_shadow = cd->background;
	}
	else
	{
	    cd->foreground.red = cd->foreground.green =
		cd->foreground.blue = cd->top_shadow.red =
		cd->top_shadow.green = cd->top_shadow.blue =
		cd->select.red = cd->select.green = cd->select.blue =
		USHRT_MAX;
	    cd->foreground.pixel = cd->top_shadow.pixel =
		cd->select.pixel = WhitePixelOfScreen(cd->screen);
	    cd->bottom_shadow = cd->background;
	}
    }
    else
	_color_proc(&cd->background,
		    &cd->foreground,
		    &cd->select,
		    &cd->top_shadow,
		    &cd->bottom_shadow);
}

/* External entry point - get requested colors for a widget */

void
XmGetColors(Screen *screen,
	    Colormap color_map,
	    Pixel background,
	    Pixel *foreground_ret,
	    Pixel *top_shadow_ret,
	    Pixel *bottom_shadow_ret,
	    Pixel *select_ret)
{
    XmColorData *cd;

    cd = _XmGetColors( screen, color_map, background);

    if (foreground_ret)
	*foreground_ret = _XmAccessColorData( cd, XmFOREGROUND);
    if (top_shadow_ret)
	*top_shadow_ret = _XmAccessColorData( cd, XmTOP_SHADOW);
    if (bottom_shadow_ret)
	*bottom_shadow_ret = _XmAccessColorData( cd, XmBOTTOM_SHADOW);
    if (select_ret)
	*select_ret = _XmAccessColorData( cd, XmSELECT);
}

void
XmChangeColor(Widget widget,
	      Pixel background)
{
    Pixel foreground;
    Pixel top_shadow;
    Pixel bottom_shadow;
    Pixel select;

    XmGetColors(XtScreenOfObject(widget),
		ColormapOfObject(widget),
		background,
		&foreground,
		&top_shadow,
		&bottom_shadow,
		&select);

    XtVaSetValues(widget,
		  XmNbackground, background,
		  XmNforeground, foreground,
		  XmNhighlightColor, foreground,
		  XmNtopShadowColor, top_shadow,
		  XmNbottomShadowColor, bottom_shadow,
		  XmNarmColor, select,
		  XmNselectColor, select,
		  XmNtroughColor, select,
		  NULL);
}

/* the default color calculation */
static void
_XmColorProcDefaultProc(XColor *bg_color,
			XColor *fg_color,
			XColor *sel_color,
			XColor *ts_color,
			XColor *bs_color)
{
    int foreground_threshold, dark_threshold, light_threshold;
    long brightness, brightadj;
    XColor _widgetBackground;

    /* Find the thresholds for the different color models.
     * This is cheating - for these thresholds we need the Screen object,
     * which wasn't passed to this function.  Here's a hack:
     * If the background pointer is inside the color cache,
     * use that cache entry's screen.
     */

    if ((char *)bg_color > (char *)color_cache &&
	(char *)bg_color < (char *)(color_cache + COLOR_CACHE_SIZE))
    {
	Widget sw = XmGetXmScreen(color_cache[((char *)bg_color -
			(char *)color_cache) / sizeof(XmColorData)].screen);
	foreground_threshold = Screen_ForegroundThreshold(sw)
	    ? Screen_ForegroundThreshold(sw) : XmDEFAULT_FOREGROUND_THRESHOLD;
	dark_threshold = Screen_DarkThreshold(sw)
	    ? Screen_DarkThreshold(sw) : XmDEFAULT_DARK_THRESHOLD;
	light_threshold = Screen_LightThreshold(sw)
	    ? Screen_LightThreshold(sw) : XmDEFAULT_LIGHT_THRESHOLD;
    }
    else
    {
	foreground_threshold = XmDEFAULT_FOREGROUND_THRESHOLD;
	dark_threshold = XmDEFAULT_DARK_THRESHOLD;
	light_threshold = XmDEFAULT_LIGHT_THRESHOLD;
    }

    _widgetBackground = *bg_color;
    brightness = BRIGHTNESS(_widgetBackground);

    if (fg_color)
	fg_color->red = fg_color->green = fg_color->blue =
	    brightness > foreground_threshold * PCT_BRIGHTNESS ? 0 : 0xffff;

    /* For "dark" backgrounds, make everything a fixed %age lighter */

    if (brightness < dark_threshold * PCT_BRIGHTNESS)
    {
	if (sel_color)
	{
	    sel_color->red = 0xffff -
		((0xffff - _widgetBackground.red) * PCT_SELECT + 50) / 100;
	    sel_color->green = 0xffff -
		((0xffff - _widgetBackground.green) * PCT_SELECT + 50) / 100;
	    sel_color->blue = 0xffff -
		((0xffff - _widgetBackground.blue) * PCT_SELECT + 50) / 100;
	}
	if (ts_color)
	{
	    ts_color->red = 0xffff -
		((0xffff - _widgetBackground.red) * PCT_DARK_TOP + 50) / 100;
	    ts_color->green = 0xffff -
		((0xffff - _widgetBackground.green) * PCT_DARK_TOP + 50) / 100;
	    ts_color->blue = 0xffff -
		((0xffff - _widgetBackground.blue) * PCT_DARK_TOP + 50) / 100;
	}
	if (bs_color)
	{
	    bs_color->red = 0xffff -
		((0xffff - _widgetBackground.red) * PCT_DARK_BOTTOM) / 100;
	    bs_color->green = 0xffff - ((0xffff - _widgetBackground.green) *
					PCT_DARK_BOTTOM + 50) / 100;
	    bs_color->blue = 0xffff - ((0xffff - _widgetBackground.blue) *
				       PCT_DARK_BOTTOM + 50) / 100;
	}
    }

    /* For "light" background, make everything a fixed %age darker */

    else if (brightness > light_threshold * PCT_BRIGHTNESS)
    {
	if (sel_color)
	{
	    sel_color->red = (_widgetBackground.red * PCT_SELECT + 50) / 100;
	    sel_color->green =
		(_widgetBackground.green * PCT_SELECT + 50) / 100;
	    sel_color->blue = (_widgetBackground.blue * PCT_SELECT + 50) / 100;
	}
	if (ts_color)
	{
	    ts_color->red = (_widgetBackground.red * PCT_LIGHT_TOP + 50) / 100;
	    ts_color->green =
		(_widgetBackground.green * PCT_LIGHT_TOP + 50) / 100;
	    ts_color->blue =
		(_widgetBackground.blue * PCT_LIGHT_TOP + 50) / 100;
	}
	if (bs_color)
	{
	    bs_color->red =
		(_widgetBackground.red * PCT_LIGHT_BOTTOM + 50) / 100;
	    bs_color->green =
		(_widgetBackground.green * PCT_LIGHT_BOTTOM + 50) / 100;
	    bs_color->blue =
		(_widgetBackground.blue * PCT_LIGHT_BOTTOM + 50) / 100;
	}
    }

    /* For "medium" background, select is a fixed %age darker;
     * top (lighter) and bottom (darker) are a variable %age
     * based on the background's brightness
     */

    else
    {
	brightness = (brightness + (PCT_BRIGHTNESS >> 1)) / PCT_BRIGHTNESS;
	if (sel_color)
	{
	    sel_color->red = (_widgetBackground.red * PCT_SELECT + 50) / 100;
	    sel_color->green =
		(_widgetBackground.green * PCT_SELECT + 50) / 100;
	    sel_color->blue =
		(_widgetBackground.blue * PCT_SELECT + 50) / 100;
	}
	if (ts_color)
	{
	    brightadj = PCT_MEDIUM_TOP_BASE +
		(brightness * PCT_MEDIUM_TOP_RANGE + 50) / 100;
	    ts_color->red = 0xffff -
		((0xffff - _widgetBackground.red) * brightadj + 50) / 100;
	    ts_color->green = 0xffff -
		((0xffff - _widgetBackground.green) * brightadj + 50) / 100;
	    ts_color->blue = 0xffff -
		((0xffff - _widgetBackground.blue) * brightadj + 50) / 100;
	}
	if (bs_color)
	{
	    brightadj = PCT_MEDIUM_BOTTOM_BASE +
		(brightness * PCT_MEDIUM_BOTTOM_RANGE + 50) / 100;
	    bs_color->red = (_widgetBackground.red * brightadj + 50) / 100;
	    bs_color->green = (_widgetBackground.green * brightadj + 50) / 100;
	    bs_color->blue = (_widgetBackground.blue * brightadj + 50) / 100;
	}
    }
}

void
_XmForegroundColorDefault(Widget w,
			  int offset,
			  XrmValue *val)
{
    static Pixel foreground;

    val->addr = (XPointer)&foreground;
    val->size = sizeof foreground;
    foreground = _XmAccessColorData(_XmGetColors(XtScreenOfObject(w),
						 ColormapOfObject(w),
						 XmIsGadget(w) ?
						 XmParentBackground(w) :
						 XtBackground(w)),
				    XmFOREGROUND);
}

void
_XmHighlightColorDefault(Widget w,
			 int offset,
			 XrmValue *val)
{
    static Pixel highlight;

    val->addr = (XPointer)&highlight;
    val->size = sizeof highlight;
    highlight = _XmAccessColorData(_XmGetColors(XtScreenOfObject(w),
						ColormapOfObject(w),
						XmIsGadget(w) ?
						XmParentBackground(w) :
						XtBackground(w)),
				   XmFOREGROUND);
}

void
_XmBackgroundColorDefault(Widget w,
			  int offset,
			  XrmValue *val)
{
    XColor *xc;
    XmColorData *cc;
    XmColorData cd;
    static Pixel background;

    cd.screen = XtScreenOfObject(w);
    if (DefaultDepthOfScreen(cd.screen) == 1)
    {
	/* The default background color of B/W screen in always white. */

	val->size = sizeof background;
	val->addr = (XPointer)&background;
	background = WhitePixelOfScreen(cd.screen);
	return;
    }
    cd.color_map = ColormapOfObject(w);

    /* Look for an XColor associated with the screen */

    if (!background_color_context)
	background_color_context = XUniqueContext();
    if (XFindContext(DisplayOfScreen(cd.screen),
		     RootWindowOfScreen(cd.screen),
		     background_color_context,
		     (XPointer *)&xc) == XCSUCCESS)
    {
	/* Found it.  Make sure it's in the cache (a la _XmGetColors) */

	cd.allocated = 0;
	cd.background = *xc;
	cd.foreground.pixel =
	    cd.top_shadow.pixel =
	    cd.bottom_shadow.pixel =
	    cd.select.pixel = 0;
	cc = _XmAddToColorCache( &cd);
	if (!(cc->allocated & XmBACKGROUND))
	{
	    cc->allocated |= XmBACKGROUND;
	    call_color_proc(cc);
	}
	val->size = sizeof background;
	val->addr = (XPointer)&background;
	background = cd.background.pixel;
    }
    else
    {
	/* Put the color in the cache */

	_XmAddBackgroundToColorCache(cd.screen,
				   cd.color_map,
				   _XmGetDefaultBackgroundColorSpec(cd.screen),
#if XmVERSION == 1
				   0x729fff,
#else
				   0xc4c4c4,
#endif
				   val);

	/* Save the XColor (which is now in the front of the color cache). */

	xc = XtNew(XColor);
	*xc = color_cache->background;
	XSaveContext(DisplayOfScreen(cd.screen),
		     RootWindowOfScreen(cd.screen),
		     background_color_context,
		     (XPointer)xc);
    }
}


void
_XmTopShadowColorDefault(Widget w,
			 int offset,
			 XrmValue *val)
{
    static Pixel top_shadow;

    val->addr = (XPointer)&top_shadow;
    val->size = sizeof top_shadow;
    top_shadow = _XmAccessColorData(_XmGetColors(XtScreenOfObject(w),
						 ColormapOfObject(w),
						 XmIsGadget(w) ?
						 XmParentBackground(w) :
						 XtBackground(w)),
				    XmTOP_SHADOW);
}


void
_XmBottomShadowColorDefault(Widget w,
			    int offset,
			    XrmValue *val)
{
    static Pixel bottom_shadow;

    val->addr = (XPointer)&bottom_shadow;
    val->size = sizeof bottom_shadow;
    bottom_shadow = _XmAccessColorData(_XmGetColors(XtScreenOfObject(w),
						    ColormapOfObject(w),
						    XmIsGadget(w) ?
						    XmParentBackground(w) :
						    XtBackground(w)),
				       XmBOTTOM_SHADOW);
}

void
_XmSelectColorDefault(Widget w,
		      int offset,
		      XrmValue *val)
{
    static Pixel select;

    val->addr = (XPointer)&select;
    val->size = sizeof select;
    select = _XmAccessColorData(_XmGetColors(XtScreenOfObject(w),
					     ColormapOfObject(w),
					     XmIsGadget(w) ?
					     XmParentBackground(w) :
					     XtBackground(w)),
				XmSELECT);
}

/* Get the name of the default background color for a widget on this screen */

String
_XmGetDefaultBackgroundColorSpec(Screen *screen)
{
    String background_spec;
    XrmValue val;
    String str_type;

    /* It would have made so much sense to make this a Screen resource.
     * Instead, associate a background color with a screen in the same way
     * the screen object is associated.
     */

    if (!background_spec_context)
	background_spec_context = XUniqueContext();
    else if (XFindContext(DisplayOfScreen(screen),
			  RootWindowOfScreen(screen),
			  background_spec_context,
			  &background_spec) == XCSUCCESS)
	return background_spec;

    /* The "default default" is a top-level "background" resource.
     * But we want the name, not the pixel value, so look in the
     * resource database directly.  Failing that, the "default default default"
     * is that classic blue.
     */

    background_spec = XrmGetResource(XtScreenDatabase(screen), XmNbackground,
	XmNbackground, &str_type, &val) && !strcmp( str_type, XmRString)
	? val.addr
#if XmVERSION == 1
	: "rgb:72/9f/ff";
#else
	: "rgb:c4/c4/c4";
#endif

    XSaveContext(DisplayOfScreen(screen),
		 RootWindowOfScreen(screen),
		 background_spec_context,
		 XtNewString(background_spec));
    return background_spec;
}

/* Set a new default background color for this screen */

void
_XmSetDefaultBackgroundColorSpec(Screen *screen, String new_color_spec)
{
    String background;

    /* Set the color string to get later */

    if (!background_spec_context)
	background_spec_context = XUniqueContext();
    else if (XFindContext(DisplayOfScreen(screen),
		     RootWindowOfScreen(screen),
		     background_spec_context,
		     &background) == XCSUCCESS)
    {
	XtFree(background);
	XDeleteContext(DisplayOfScreen(screen),
		       RootWindowOfScreen(screen),
		       background_spec_context);
    }
    XSaveContext(DisplayOfScreen(screen),
		 RootWindowOfScreen(screen),
		 background_spec_context,
		 XtNewString(new_color_spec));

    /* Clear any saved XColor */

    if (background_color_context)
	XDeleteContext(DisplayOfScreen(screen),
		       RootWindowOfScreen(screen),
		       background_color_context);
}

/* According to code given to me by segg@reseau.com, this does work I
 * already do in XmColorProcDefaultProc: it makes the threshold resources
 * for the Screen object available for color proc use.  I don't see a reason
 * I shouldn't continue making them available where I do, so I'm putting
 * an empty function here to signify it does nothing not already done.   - JHG
 */

void
_XmGetDefaultThresholdsForScreen(Screen *screen)
{
}

/*
 * conversion args for the pixmap converters.  Thanks to Gilles Seguin
 * for pointing out the lack.
 */
#define POFFSET(field) \
    (XtPointer)XtOffsetOf(XmPrimitiveRec, field)
#define MOFFSET(field) \
    (XtPointer)XtOffsetOf(XmManagerRec, field)

/* background for everyone */
static XtConvertArgRec backgroundArgs[] =
{
	{
		XtBaseOffset,
		(XtPointer)XtOffsetOf(CoreRec, core.self),
		sizeof(WidgetRec *)
	},
	{
		XtImmediate,
		(XtPointer)XmNbackgroundPixmap,
		sizeof(String)
	},
};

static XtConvertArgRec pixmapArgs[] =
{
	{
		XtBaseOffset,
		(XtPointer)XtOffsetOf(CoreRec, core.self),
		sizeof(WidgetRec *)
	},
	{
		XtImmediate,
		(XtPointer)XmNiconPixmap,
		sizeof(String)
	},
};

/* primitive foreground */
static XtConvertArgRec PrimForegroundPixmapArgs[] =
{
    {
	XtBaseOffset, POFFSET(core.screen), sizeof(Screen *)
    },
    {
	XtBaseOffset, POFFSET(primitive.foreground), sizeof(Pixel)
    },
    {
	XtBaseOffset, POFFSET(core.background_pixel), sizeof(Pixel)
    },
    {
	XtBaseOffset, POFFSET(core.depth), sizeof(Cardinal)
    }
};

/* primitive highlight */
static XtConvertArgRec PrimHighlightPixmapArgs[] =
{
    {
	XtBaseOffset, POFFSET(core.screen), sizeof(Screen *)
    },
    {
	XtBaseOffset, POFFSET(primitive.highlight_color), sizeof(Pixel)
    },
    {
	XtBaseOffset, POFFSET(core.background_pixel), sizeof(Pixel)
    },
    {
	XtBaseOffset, POFFSET(core.depth), sizeof(Cardinal)
    }
};

/* primitive topShadow */
static XtConvertArgRec PrimTopShadowPixmapArgs[] =
{
    {
	XtBaseOffset, POFFSET(core.screen), sizeof(Screen *)
    },
    {
	XtBaseOffset, POFFSET(primitive.highlight_color), sizeof(Pixel)
    },
    {
	XtBaseOffset, POFFSET(core.background_pixel), sizeof(Pixel)
    },
    {
	XtBaseOffset, POFFSET(core.depth), sizeof(Cardinal)
    }
};

/* primitive bottomShadow */
static XtConvertArgRec PrimBottomShadowPixmapArgs[] =
{
    {
	XtBaseOffset, POFFSET(core.screen), sizeof(Screen *)
    },
    {
	XtBaseOffset, POFFSET(primitive.bottom_shadow_color), sizeof(Pixel)
    },
    {
	XtBaseOffset, POFFSET(core.background_pixel), sizeof(Pixel)
    },
    {
	XtBaseOffset, POFFSET(core.depth), sizeof(Cardinal)
    }
};

/* manager foreground */
static XtConvertArgRec ManForegroundPixmapArgs[] =
{
    {
	XtWidgetBaseOffset, MOFFSET(core.screen), sizeof(Screen *)
    },
    {
	XtWidgetBaseOffset, MOFFSET(manager.foreground), sizeof(Pixel)
    },
    {
	XtWidgetBaseOffset, MOFFSET(core.background_pixel), sizeof(Pixel)
    },
    {
	XtWidgetBaseOffset, MOFFSET(core.depth), sizeof(Cardinal)
    }
};

/* manager highlight */
static XtConvertArgRec ManHighlightPixmapArgs[] =
{
    {
	XtWidgetBaseOffset, MOFFSET(core.screen), sizeof(Screen *)
    },
    {
	XtWidgetBaseOffset, MOFFSET(manager.highlight_color), sizeof(Pixel)
    },
    {
	XtWidgetBaseOffset, MOFFSET(core.background_pixel), sizeof(Pixel)
    },
    {
	XtBaseOffset, MOFFSET(core.depth), sizeof(Cardinal)
    }
};

/* manager topShadow */
static XtConvertArgRec ManTopShadowPixmapArgs[] =
{
    {
	XtWidgetBaseOffset, MOFFSET(core.screen), sizeof(Screen *)
    },
    {
	XtWidgetBaseOffset, MOFFSET(manager.top_shadow_color), sizeof(Pixel)
    },
    {
	XtWidgetBaseOffset, MOFFSET(core.background_pixel), sizeof(Pixel)
    },
    {
	XtBaseOffset, MOFFSET(core.depth), sizeof(Cardinal)
    }
};

/* manager bottomShadow */
static XtConvertArgRec ManBottomShadowPixmapArgs[] =
{
    {
	XtWidgetBaseOffset, MOFFSET(core.screen), sizeof(Screen *)
    },
    {
	XtWidgetBaseOffset, MOFFSET(manager.bottom_shadow_color), sizeof(Pixel)
    },
    {
	XtWidgetBaseOffset, MOFFSET(core.background_pixel), sizeof(Pixel)
    },
    {
	XtBaseOffset, MOFFSET(core.depth), sizeof(Cardinal)
    }
};

/* gadget pixmap */
static XtConvertArgRec gadgetPixmapArgs[] =
{
    {
	XtBaseOffset, (XtPointer)XtOffsetOf(XmGadgetRec, object.parent),
	sizeof(WidgetRec *)
    }
};

static char *background_pixmap_name = NULL;

char *
_XmGetBGPixmapName(void)
{
    return background_pixmap_name;
}

void
_XmClearBGPixmap(void)
{
    background_pixmap_name = NULL;
}

static struct __x {
	char	*rn, *s;
	Widget	w;
} *x = NULL;
static int	nx = 0,
		ax = 0;

extern void 
_XmPickupUnspecifiedPixmaps(Display *dpy)
{
	int	i, last;
	Pixmap	p;

	for (i=0; i<nx; i++)
		if (x[i].w && XtDisplay(x[i].w) == dpy) {
			DEBUGOUT(_LtDebug(__FILE__, x[i].w, "_XmPickupUnspecifiedPixmaps(%s,%s)\n",
				x[i].rn, x[i].s));
			p = XmGetPixmap(XtScreen(x[i].w),
				x[i].s,
				WhitePixelOfScreen(XtScreen(x[i].w)),
				BlackPixelOfScreen(XtScreen(x[i].w)));

			/* Using XtSetValues here doesn't seem to work. */
			if (XtIsSubclass(x[i].w, vendorShellWidgetClass)) {
				Vendor_IconPixmap(x[i].w) = p;
			} else if (XtIsSubclass(x[i].w, xmMenuShellWidgetClass)) {
/* I don't know what I was thinking...
				MS_DefaultFontList(x[i].w) = p;
				MS_ButtonFontList(x[i].w) = p;
				MS_LabelFontList(x[i].w) = p;
 */
			}
			x[i].w = NULL;
		}

	/* Clean up */
	for (last=-1,i=0; i<nx; i++)
		if (x[i].w) {
			last = i;
		}
	nx = last+1;
	if (nx == 0) {
		XtFree((char *)x);
		x = NULL;
	}
}

static Boolean
CvtStringToBackgroundPixmap(Display *display,
				XrmValuePtr args, Cardinal *num_args,
				XrmValuePtr fromVal,
				XrmValuePtr toVal,
				XtPointer *converter_data)
{
	static Pixmap	buf;
	Widget		w = (Widget)args[0].addr;
	String		rn = *(String *)args[1].addr,
			s = (String)fromVal->addr;

	DEBUGOUT(_LtDebug(__FILE__, w, "CvtStringToBackgroundPixmap(%s): %s\n",
		rn, s));

	if (XtIsShell(w)) {
		/* Allocation */
		if (x == 0) {
			x = (struct __x *)XtCalloc(32, sizeof(struct __x));
			ax = 16;
		} else if (nx == ax) {
			ax += 16;
			x = (struct __x *)XtRealloc((char *)x, sizeof(struct __x) * ax);
		}
		/*
		 * In a shell we need to be careful to avoid infinite recursion.
		 * Therefore we now store the parameters that we require later
		 * to do the conversion properly.
		 */
		x[nx].w = w;
		x[nx].s = s;
		x[nx].rn = rn;
		nx++;
	} else {
		background_pixmap_name = s;
	}

	if (toVal->addr != NULL) {
		if( toVal->size < sizeof(Pixmap)) {
			toVal->size = sizeof(Pixmap);
			return False;
		} else {
			*((Pixmap *)(toVal->addr)) = XmUNSPECIFIED_PIXMAP;
			toVal->size = sizeof(Pixmap);
			return True;
		}
	} else {
		buf = XmUNSPECIFIED_PIXMAP;
		toVal->addr = (XPointer)&buf;
		toVal->size = sizeof(Pixmap);
		return True;
	}
}

static void
BuildPixmap(Cardinal ncargs, char *which,
	    XrmValuePtr args, Cardinal *num_args,
	    XrmValuePtr fromVal, XrmValuePtr toVal)
{
    static Pixmap pixmap;
    Screen *screen;
    Pixel foreground;
    Pixel background;
    Cardinal depth;
    char *name = fromVal->addr;

    pixmap = XmUNSPECIFIED_PIXMAP;

    toVal->addr = (char *)&pixmap;
    toVal->size = sizeof(Pixmap);

    if (*num_args != ncargs)
    {
	XtWarningMsg("wrongParameters", which, "XmToolkitError",
		     "String to Pixmap conversion needs four argument",
		     (String *)NULL, (Cardinal *)NULL);

	return;
    }

    if (strcmp(name, "unspecified_pixmap") == 0)
    {
	return;
    }

    screen = *((Screen **)args[0].addr);
    foreground = *((Pixel *)args[1].addr);
    background = *((Pixel *)args[2].addr);
    depth = *((Cardinal *)args[3].addr);

    pixmap = XmGetPixmapByDepth(screen, name,
				foreground, background, depth);
}

static void
_XmCvtStringToPrimForegroundPixmap(XrmValuePtr args, Cardinal *num_args,
				   XrmValuePtr fromVal, XrmValuePtr toVal)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToPrimForegroundPixmap\n"));

    BuildPixmap(XtNumber(PrimForegroundPixmapArgs),
		"_XmCvtStringToPrimForegroundPixmap",
		args, num_args, fromVal, toVal);
}

static void
_XmCvtStringToPrimHighlightPixmap(XrmValuePtr args, Cardinal *num_args,
				  XrmValuePtr fromVal, XrmValuePtr toVal)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToPrimHighlightPixmap\n"));

    BuildPixmap(XtNumber(PrimHighlightPixmapArgs),
		"_XmCvtStringToPrimHighlightPixmap",
		args, num_args, fromVal, toVal);
}

static void
_XmCvtStringToPrimTopShadowPixmap(XrmValuePtr args, Cardinal *num_args,
				  XrmValuePtr fromVal, XrmValuePtr toVal)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToPrimTopShadowPixmap\n"));

    BuildPixmap(XtNumber(PrimTopShadowPixmapArgs),
		"_XmCvtStringToPrimTopShadowPixmap",
		args, num_args, fromVal, toVal);
}

static void
_XmCvtStringToPrimBottomShadowPixmap(XrmValuePtr args, Cardinal *num_args,
				     XrmValuePtr fromVal, XrmValuePtr toVal)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToPrimBottomShadowPixmap\n"));

    BuildPixmap(XtNumber(PrimBottomShadowPixmapArgs),
		"_XmCvtStringToPrimBottomShadowPixmap",
		args, num_args, fromVal, toVal);
}

static void
_XmCvtStringToManForegroundPixmap(XrmValuePtr args, Cardinal *num_args,
				  XrmValuePtr fromVal, XrmValuePtr toVal)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToManForegroundPixmap\n"));

    BuildPixmap(XtNumber(ManForegroundPixmapArgs),
		"_XmCvtStringToManForegroundPixmap",
		args, num_args, fromVal, toVal);
}

static void
_XmCvtStringToManHighlightPixmap(XrmValuePtr args, Cardinal *num_args,
				 XrmValuePtr fromVal, XrmValuePtr toVal)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToManHighlightPixmap\n"));

    BuildPixmap(XtNumber(ManHighlightPixmapArgs),
		"_XmCvtStringToManHighlightPixmap",
		args, num_args, fromVal, toVal);
}

static void
_XmCvtStringToManTopShadowPixmap(XrmValuePtr args, Cardinal *num_args,
				 XrmValuePtr fromVal, XrmValuePtr toVal)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToManTopShadowPixmap\n"));

    BuildPixmap(XtNumber(ManTopShadowPixmapArgs),
		"_XmCvtStringToManTopShadowPixmap",
		args, num_args, fromVal, toVal);
}

static void
_XmCvtStringToManBottomShadowPixmap(XrmValuePtr args, Cardinal *num_args,
				    XrmValuePtr fromVal, XrmValuePtr toVal)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToManBottomShadowPixmap\n"));

    BuildPixmap(XtNumber(ManBottomShadowPixmapArgs),
		"_XmCvtStringToManBottomShadowPixmap",
		args, num_args, fromVal, toVal);
}

static void
_XmCvtStringToGadgetPixmap(XrmValuePtr args, Cardinal *num_args,
			   XrmValuePtr fromVal, XrmValuePtr toVal)
{
    XrmValue pargs[4];
    Cardinal pnargs = 4;
    Widget parent;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToGadgetPixmap\n"));

    parent = *((Widget *)args[0].addr);

    pargs[0].addr = (XPointer)&CoreScreen(parent);
    pargs[1].addr = (XPointer)&MGR_Foreground(parent);
    pargs[2].addr = (XPointer)&CoreBackground(parent);
    pargs[3].addr = (XPointer)&CoreDepth(parent);

    BuildPixmap(pnargs, "_XmCvtStringToGadgetPixmap",
		pargs, &pnargs, fromVal, toVal);
}

void
_XmRegisterPixmapConverters(void)
{
    static Boolean inited = False;
  
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmRegisterPixmapConverters(%d)\n",
	__FILE__, __LINE__));

    if (inited)
    {
	return;
    }

    inited = True;

	XtSetTypeConverter(XtRString, XmRXmBackgroundPixmap,
		CvtStringToBackgroundPixmap, backgroundArgs,
		XtNumber(backgroundArgs), XtCacheNone, NULL);
	XtSetTypeConverter(XtRString, XmRPixmap,
		CvtStringToBackgroundPixmap, pixmapArgs,
		XtNumber(pixmapArgs), XtCacheNone, NULL);

    XtAddConverter(XtRString, XmRPrimForegroundPixmap,
                   _XmCvtStringToPrimForegroundPixmap, PrimForegroundPixmapArgs,
		   XtNumber(PrimForegroundPixmapArgs));
    XtAddConverter(XtRString,  XmRHighlightPixmap,
                   _XmCvtStringToPrimHighlightPixmap, PrimHighlightPixmapArgs,
		   XtNumber(PrimHighlightPixmapArgs));
    XtAddConverter(XtRString, XmRTopShadowPixmap,
                   _XmCvtStringToPrimTopShadowPixmap, PrimTopShadowPixmapArgs,
		   XtNumber(PrimTopShadowPixmapArgs));
    XtAddConverter(XtRString,  XmRBottomShadowPixmap,
                   _XmCvtStringToPrimBottomShadowPixmap,
		   PrimBottomShadowPixmapArgs,
		   XtNumber(PrimBottomShadowPixmapArgs));

    XtAddConverter(XtRString,  XmRManForegroundPixmap,
                   _XmCvtStringToManForegroundPixmap, ManForegroundPixmapArgs,
		   XtNumber(ManForegroundPixmapArgs));
    XtAddConverter(XtRString,  XmRManHighlightPixmap,
                   _XmCvtStringToManHighlightPixmap, ManHighlightPixmapArgs,
		   XtNumber(ManHighlightPixmapArgs));
    XtAddConverter(XtRString,  XmRManTopShadowPixmap,
                   _XmCvtStringToManTopShadowPixmap, ManTopShadowPixmapArgs,
		   XtNumber(ManTopShadowPixmapArgs));
    XtAddConverter(XtRString,  XmRManBottomShadowPixmap,
                   _XmCvtStringToManBottomShadowPixmap,
		   ManBottomShadowPixmapArgs,
		   XtNumber(ManBottomShadowPixmapArgs));

    XtAddConverter(XtRString,  XmRGadgetPixmap,
                   _XmCvtStringToGadgetPixmap, gadgetPixmapArgs,
		   XtNumber(gadgetPixmapArgs));

#if 0
    XtSetTypeConverter(XtRString,  XmRAnimationMask,
		       CvtStringToAnimationMask,
                       backgroundArgs, XtNumber(backgroundArgs),
		       XtCacheNone, NULL);

    XtSetTypeConverter(XtRString, XmRAnimationPixmap,
		       CvtStringToAnimationPixmap,
                       backgroundArgs, XtNumber(backgroundArgs),
		       XtCacheNone, NULL);
#endif
}

/*
 * These functions (_XmBlackPixel and _XmWhitePixel) appear in
 * libXm:Obso2_0.o in the Motif 2 libraries.  The prototypes are in
 * Xm/XmP.h but there were no implementations.
 *
 * The final XColor parameter makes no sense to me at all but the Motif
 * headers on HP-UX 9.05 specify a raw XColor (as opposed to the more
 * sensible XColor*) too so the signatures are correct.
 *
 * I don't know the proper behavior of these functions so I'm guessing.
 *
 *	--mu@echo-on.net, 1998.03.15
 */
Pixel
_XmWhitePixel(Screen *screen, Colormap colormap, XColor whitecolor)
{
    static Screen *last_screen;
    static Colormap last_colormap;
    static Pixel last_pixel;

    if (colormap == DefaultColormapOfScreen(screen))
	return WhitePixelOfScreen(screen);
    if (screen == last_screen && colormap == last_colormap)
	return last_pixel;

    last_screen = screen;
    last_colormap = colormap;
    whitecolor.red   =
    whitecolor.green =
    whitecolor.blue  = USHRT_MAX;
    if (XAllocColor(DisplayOfScreen(screen), colormap, &whitecolor))
    {
	last_pixel = whitecolor.pixel;
    }
    else
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmWhitePixel(%d) - Could not allocate white color\n",
			   __FILE__,__LINE__));
	color_fail_warn();
	last_pixel = WhitePixelOfScreen(screen);
    }
    return last_pixel;
}

Pixel
_XmBlackPixel(Screen *screen, Colormap colormap, XColor blackcolor)
{
    static Screen *last_screen;
    static Colormap last_colormap;
    static Pixel last_pixel;

    if (colormap == DefaultColormapOfScreen(screen))
	return BlackPixelOfScreen(screen);
    if (screen == last_screen && colormap == last_colormap)
	return last_pixel;

    last_screen = screen;
    last_colormap = colormap;
    blackcolor.red   =
    blackcolor.green =
    blackcolor.blue  = 0;
    if (XAllocColor(DisplayOfScreen(screen), colormap, &blackcolor))
    {
	last_pixel = blackcolor.pixel;
    }
    else
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmBlackPixel(%d) - Could not allocate black color\n",
			   __FILE__,__LINE__));
	color_fail_warn();
	last_pixel = BlackPixelOfScreen(screen);
    }
    return last_pixel;
}

/*
 * More convenient forms of _XmWhitePixel and _XmBlackPixel.
 * These can't be macros because the stupid last parameter is a raw structure
 * rather than a pointer.
 */
Pixel
_XmWhitePixelOfObject(Widget w)
{
    XColor unused;
    return _XmWhitePixel(XtScreenOfObject(w), ColormapOfObject(w), unused);
}

Pixel
_XmBlackPixelOfObject(Widget w)
{
    XColor unused;
    return _XmBlackPixel(XtScreenOfObject(w), ColormapOfObject(w), unused);
}
