/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ImageCache.c,v 1.6 2005/05/06 16:47:02 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ImageCache.c,v 1.6 2005/05/06 16:47:02 dannybackx Exp $";

/*
 * --aldi 97/01/11: changed to use the generic hashtable module.
 */
#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <XmI/XmI.h>
#include <XmI/HashI.h>
#include <XmI/ImageCacheI.h>
#include <Xm/XmosP.h>
#include <Xm/XmP.h>
#include <Xm/ScreenP.h>

#include <Xm/XpmP.h>

#include <XmI/DebugUtil.h>

/*
 * The actual caches for images and for pixmaps... we now use the generic
 * hash table module in Hash.c. We don't need to re-invent the wheel every
 * day -- once in a week is just enough so we don't get rusty.
 * Note: there is only ONE image cache but TWO pixmap caches. This is based
 * on the assumption that there are only few images present that will act
 * as sources for many more pixmaps. So we don't want to waste memory and
 * don't use a backward association cache for images. But as pixmaps are
 * created and destroyed more frequently it makes sense to speed up deletion
 * using a backward association cache. With the help of this cache we can
 * easily lookup a pixmap's description entry having only the pixmap id and
 * the screen id ready at hand.
 */
static LTHashTable ImageCache = NULL;
static LTHashTable PixmapCache = NULL;
static LTHashTable RevPixmapCache = NULL;

/*
 * internal functions
 */
static LTImageValue LTGetImageFromCache(char *image_name);
static void LTSetupImageCache(void);
static void LTSetupPixmapCache(void);


/*
 * Various paths used when searching for pixmaps in XmGetPixmap below
 */
static char *_search_path = NULL;

static char *XAPPLRESDIR_set_pattern = "\
%%B:\
%s/%%L/%%T/%%N/%%B:\
%s/%%l/%%T/%%N/%%B:\
%s/%%T/%%N/%%B:\
%s/%%L/%%T/%%B:\
%s/%%l/%%T/%%B:\
%s/%%T/%%B:\
%s/%%T/%%B:\
%s/%%B:\
/usr/lib/X11/%%L/%%T/%%N/%%B:\
/usr/lib/X11/%%l/%%T/%%N/%%B:\
/usr/lib/X11/%%T/%%N/%%B:\
/usr/lib/X11/%%L/%%T/%%B:\
/usr/lib/X11/%%l/%%T/%%B:\
/usr/lib/X11/%%T/%%B:\
/usr/include/X11/%%T/%%B";

static char *nothing_set_pattern = "\
%%B:\
%s/%%L/%%T/%%N/%%B:\
%s/%%l/%%T/%%N/%%B:\
%s/%%T/%%N/%%B:\
%s/%%L/%%T/%%B:\
%s/%%l/%%T/%%B:\
%s/%%T/%%B:\
%s/%%B:\
/usr/lib/X11/%%L/%%T/%%N/%%B:\
/usr/lib/X11/%%l/%%T/%%N/%%B:\
/usr/lib/X11/%%T/%%N/%%B:\
/usr/lib/X11/%%L/%%T/%%B:\
/usr/lib/X11/%%l/%%T/%%B:\
/usr/lib/X11/%%T/%%B:\
/usr/include/X11/%%T/%%B";


/* --------------------------------------------------------------------------
 * Public Image Cache Functions...
 */
/*
 * A pixmap caching function that adds an image to the image cache. This
 * function simply stands on the shoulders of _XmInstallImage() which knows
 * of hot spot coordinates for images.
 */
Boolean
XmInstallImage(XImage *image, char *image_name)
{
    return _XmInstallImage(image, image_name, 0, 0);
}

/*
 * Almost the same as above, but this sucker can deal with hot spots. This
 * is spooky, isn't it...?!
 */
Boolean
_XmInstallImage(XImage *image, char *image_name, int hot_x, int hot_y)
{
    LTImageValue ImageValue;

    /*
     * First check for bad parameters, then make sure that there's an
     * image cache waiting for our image...
     */
    if ((image == NULL) || (image_name) == NULL)
    {
	return False;
    }

    if (ImageCache == (LTHashTable) NULL)
    {
	LTSetupImageCache();
    }

    /*
     * Fill in a description record and let the generic hash table module
     * do the dirty work...
     */
    ImageValue = (LTImageValue) XtMalloc(sizeof(LTImageValueRec));
    ImageValue->image = image;
    ImageValue->hot_x = hot_x;
    ImageValue->hot_y = hot_y;
    ImageValue->undeletable = False;
    if (!_LTHashTableAddItem(ImageCache,
			    (LTHashItemID) image_name,
			    (LTHashItemValue) ImageValue))
    {
	/*
	 * Oops, there's already such an image name used in the cache.
	 * Because the image in the cache hasn't been touched, we now
	 * must free our image value description (the value of an item
	 * within the hash table).
	 */
	XtFree((char *)ImageValue);

	return False;
    }

    return True;
}

/*
 * This "foreach" iterator is necessary to find an image by its XImage id.
 * The iterator will free the image value description for the ximage, if
 * it finds one and the image is deletable (i.e. not a pre-defined one).
 */
static LTHashForEachIteratorResult
LTImageIterator(LTHashTable ht,
		LTHashItemID id, LTHashItemValue value, XtPointer image)
{
    if (((LTImageValue) value)->image == (XImage *)image)
    {
	if (((LTImageValue) value)->undeletable)
	{
	    return LTHASH_BREAK;
	}

	_LTHashTableRemoveItem(ht, id, NULL);

	XtFree((char *)value);

	return LTHASH_COUNTANDBREAK;
    }

    return LTHASH_CONT;
}

/*
 * Removes an image from the image cache. This implementation is notorously
 * slow if there are *many* images in the cache. Fortunately, the uninstall
 * operation is rarely needed, thus there is (almost) no reason to use a
 * second hash table for image lookup by image pointer. This would only
 * increase memory usage and would have rarely any impact. So we stick with
 * only one hash table and bit the bullet if someone really wants to
 * uninstall an image.
 */
Boolean
XmUninstallImage(XImage *image)
{
    if ((ImageCache == NULL) || (image == NULL))
    {
	return False;
    }

    return _LTHashTableForEachItem(ImageCache, LTImageIterator,
				  (XtPointer)image) != 0 ? True : False;
}

/*

 */
Boolean
_XmGetImage(Screen *screen, char *image_name, XImage **image)
{
    return False;
}

/* --------------------------------------------------------------------------
 * Public Pixmap Cache Functions...
 */
/*
 * A pixmap caching function that generates a pixmap, stores it in a pixmap
 * cache and returns the pixmap.
 */
extern Pixmap
XmGetPixmap(Screen *screen, char *image_name,
	    Pixel foreground, Pixel background)
{
    Pixmap p;

    p = XmGetPixmapByDepth(screen, image_name, foreground, background,
			   DefaultDepthOfScreen(screen));

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "XmGetPixmap(%s, %d, %d) => 0x%x (depth %d)\n",
		      image_name, foreground, background, p,
		      DefaultDepthOfScreen(screen)));

    return p;
}

/*
 * Almost the same as XmGetPixmap() above, but this time the programmer can
 * even control the depth of the pixmap she/he wants.
 */
extern Pixmap
XmGetPixmapByDepth(Screen *screen, char *image_name, Pixel foreground, Pixel background, int depth)
{
    LTPixmapDescRec PixmapDesc;
    LTPixmapDesc PixmapValue;
    LTImageValue ImageDesc;

    int image_depth;

    char *pathname_to_pixmap;
    unsigned int bitmap_width, bitmap_height;
    int x_hot, y_hot;
    XImage *image;
    Pixmap new_pix, tmp, mask;
    GC gc;
    XGCValues values;
#ifdef NONSTANDARD_CONVERTERS
    XpmColorSymbol xpm_colour_symbols[3];
    XpmAttributes xpm_attrib;
#endif /* NONSTANDARD_CONVERTERS */

    values.foreground = foreground;
    values.background = background;

    if (PixmapCache == NULL)
    {
	LTSetupPixmapCache();
    }

    if (image_name == NULL)
    {
	return XmUNSPECIFIED_PIXMAP;
    }

    /*
     * First, we check the pixmap cache to see if such a pixmap is already
     * there. Therefore we fill in a pixmap description but only use the
     * first set of fields in the description structure. The second set will
     * contain information about the pixmap, if a pixmap, as described by the
     * first set, is already in the cache.
     */
    PixmapDesc.image_name = image_name;
    PixmapDesc.screen = screen;
    PixmapDesc.foreground = foreground;
    PixmapDesc.background = background;
    PixmapDesc.depth = depth;

    if (_LTHashTableLookupItem(PixmapCache, (LTHashItemID) & PixmapDesc,
			      (LTHashItemValue *) & PixmapValue))
    {
	/*
	 * There's a pixmap in the cache so we use it and increment its
	 * reference count.
	 */
	(PixmapValue->ref_count)++;

#if 0
	DEBUGOUT(_LtDebug0(__FILE__, NULL,
		"XmGetPixmapByDepth(scr %p, %s, dpy %p, fg %p, bg %p, d %d) -> %p, refcnt %d\n",
		screen, image_name, DisplayOfScreen(screen), foreground, background, depth,
		PixmapValue->pixmap, PixmapValue->ref_count));
#endif
	return PixmapValue->pixmap;
    }

    /*
     * Okay, we had no luck. Now check the image cache.
     */
    ImageDesc = LTGetImageFromCache(image_name);

    if (ImageDesc != NULL)
    {
	/*
	 * A match from the image cache. Now create the pixmap, add it to
	 * the cache, and return it.
	 */
	image = ImageDesc->image;

	if (image->format == XYBitmap)
	{
	    image_depth = 1;
	}
	else
	{
	    image_depth = image->depth;
	}

	/* create the pixmap */
	new_pix = _XmAllocScratchPixmap((XmScreen)XmGetXmScreen(screen),
					depth, image->width, image->height);

	tmp = _XmAllocScratchPixmap((XmScreen)XmGetXmScreen(screen),
				    depth, image->width, image->height);

	gc = XCreateGC(DisplayOfScreen(screen), tmp, 0, NULL);

	/* move the image information into a temporary pixmap */
	XPutImage(DisplayOfScreen(screen),
		  tmp,
		  gc,
		  image,
		  0, 0,
		  0, 0,
		  image->width, image->height);

	XFreeGC(DisplayOfScreen(screen), gc);

	values.foreground = background;
	values.background = foreground;

	gc = XCreateGC(DisplayOfScreen(screen), new_pix,
		       GCForeground | GCBackground, &values);

	if (image_depth == 1)
	{
	    XCopyPlane(DisplayOfScreen(screen),
		       tmp,
		       new_pix,
		       gc,
		       0, 0,
		       image->width, image->height,
		       0, 0,
		       1);
	}
	else
	{
	    XCopyArea(DisplayOfScreen(screen),
		      tmp,
		      new_pix,
		      gc,
		      0, 0,
		      image->width, image->height,
		      0, 0);
	}

	XFreeGC(DisplayOfScreen(screen), gc);

	/* add the pixmap to the cache */
	_LTAddPixmapToCache(image_name, new_pix, screen,
			   foreground, background, depth,
			   image->width, image->height,
			   ImageDesc->hot_x, ImageDesc->hot_y);

DEBUGOUT(_LtDebug0(__FILE__, NULL,
"XmGetPixmapByDepth(scr %p, %s, dpy %p, fg %p, bg %p, d %d) -> %p new\n",
screen, image_name, DisplayOfScreen(screen), foreground, background, depth,
new_pix));
	return new_pix;
    }

    /* since it wasn't in either cache, we search for the file */
    /* make sure the search path is there */
    if (!_search_path)
    {
	_LTCreateSearchPath();
    }

    if (image_name[0] == '/')	/* an absolute pathname */
    {
	pathname_to_pixmap = XtNewString(image_name);
    }
    else
    {
	SubstitutionRec subs[1];

	subs[0].match = 'B';
	subs[0].substitution = XtNewString(image_name);

	pathname_to_pixmap = XtResolvePathname(DisplayOfScreen(screen),
					       "bitmaps",
					       NULL,
					       NULL,
					       _search_path,
					       subs,
					       1,
					       NULL);
	XtFree(subs[0].substitution);
    }

    /* this breaks if X isn't where it's supposed to be. Don't dump core,
     * just return */
    if (pathname_to_pixmap == NULL || strlen(pathname_to_pixmap) == 0)
    {
	return XmUNSPECIFIED_PIXMAP;
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "pathname found is %s\n", pathname_to_pixmap));


    /* 
     * For 1.2 converting XPMs to pixmaps is an extension (standard
     * Motif dosen't read XPMs until version 2) so only do it if
     * NONSTANDARD_CONVERTERS is defined
     */
#ifdef NONSTANDARD_CONVERTERS
    /* Try for an XPM file, then a bitmap */
    /* Set up symbolic names for foreground & background colours */
    xpm_colour_symbols[0].name = "background";
    xpm_colour_symbols[0].value = 0;
    xpm_colour_symbols[0].pixel = background ;
    xpm_colour_symbols[1].name = "foreground";
    xpm_colour_symbols[1].value = 0;
    xpm_colour_symbols[1].pixel = foreground ;    
    xpm_colour_symbols[2].name = NULL;
    xpm_colour_symbols[2].value = "None";
    xpm_colour_symbols[2].pixel = background ;

    xpm_attrib.colorsymbols = xpm_colour_symbols;
    xpm_attrib.numsymbols = XtNumber(xpm_colour_symbols);
    xpm_attrib.depth = depth;
    xpm_attrib.closeness = 40000;
    xpm_attrib.valuemask = ( XpmSize | XpmReturnPixels | XpmColorSymbols
			     | XpmCloseness | XpmDepth );

    if (XpmReadFileToPixmap(DisplayOfScreen(screen),
                               RootWindowOfScreen(screen),
                               pathname_to_pixmap,
			       &new_pix, &mask,
			       &xpm_attrib) == XpmSuccess)
    {
	/* add the pixmap to the cache */
	_LTAddPixmapToCache(image_name, new_pix, screen,
			   foreground, background, depth,
			   xpm_attrib.width, xpm_attrib.height,
			   xpm_attrib.x_hotspot, xpm_attrib.y_hotspot);
    }
	else
#endif /* NONSTANDARD_CONVERTERS */

    if (XReadBitmapFile(DisplayOfScreen(screen),
			RootWindowOfScreen(screen),
			pathname_to_pixmap,
			&bitmap_width, &bitmap_height,
			&tmp, &x_hot, &y_hot) == BitmapSuccess)
    {
	new_pix = _XmAllocScratchPixmap((XmScreen)XmGetXmScreen(screen),
					depth, bitmap_width, bitmap_height);

	gc = XCreateGC(DisplayOfScreen(screen), new_pix,
		       GCForeground | GCBackground, &values);

	XCopyPlane(DisplayOfScreen(screen),
		   tmp,
		   new_pix,
		   gc,
		   0, 0,
		   bitmap_width, bitmap_height,
		   0, 0,
		   1);

	XFreeGC(DisplayOfScreen(screen), gc);

	/* add the pixmap to the cache */
	_LTAddPixmapToCache(image_name, new_pix, screen,
			   foreground, background, depth,
			   bitmap_width, bitmap_height,
			   0, 0 /* FIX ME! hot_x, hot_y! */ );

    }
    else
    {
	_XmWarning(NULL, "Couldn't load the pixmap %s.\n", pathname_to_pixmap);
	new_pix = XmUNSPECIFIED_PIXMAP;
    }

    XtFree(pathname_to_pixmap);
DEBUGOUT(_LtDebug0(__FILE__, NULL,
"XmGetPixmapByDepth(scr %p, %s, dpy %p, fg %p, bg %p, d %d) -> %p new2\n",
screen, image_name, DisplayOfScreen(screen), foreground, background, depth,
new_pix));
    return new_pix;
}


/*
 * Removes a pixmap from the pixmap cache.
 */
extern Boolean
XmDestroyPixmap(Screen *screen, Pixmap pixmap)
{
    LTPixmapDescRec PixmapDesc;
    LTPixmapDesc CachedPixmap;
    char *ImageName;

    if (PixmapCache == NULL)
    {
	LTSetupPixmapCache();
    }

    PixmapDesc.screen = screen;
    PixmapDesc.pixmap = pixmap;

    if (!_LTHashTableLookupItem(RevPixmapCache, (LTHashItemID) & PixmapDesc,
			       (LTHashItemValue *) & CachedPixmap))
    {
	return False;
    }

    if (--(CachedPixmap->ref_count) > 0)
    {
	return True;
    }

    if (_LTHashTableRemoveItem(RevPixmapCache, (LTHashItemID) & PixmapDesc,
			      (LTHashItemValue *) & CachedPixmap))
    {
	ImageName = CachedPixmap->image_name;

	_LTHashTableRemoveItem(PixmapCache, (LTHashItemID) CachedPixmap,
			      NULL);
	XtFree(ImageName);

	return True;
    }

    return False;
}


/*
 * This one is going to be obsolete in Motif 2.0...
 */
Pixmap
_XmGetPixmap(Screen *screen, char *image_name, int depth,
	     Pixel foreground, Pixel background)
{
    return XmGetPixmapByDepth(screen, image_name,
			      foreground, background, depth);
}

/*
 * Adds a pixmap with the name given to the pixmap cache.
 */
Boolean
_XmInstallPixmap(Pixmap pixmap, Screen *screen, char *image_name,
		 Pixel foreground, Pixel background)
{
    /*
     * Let _LTAddPixmapToCache() do the work. Also let it calculate the
     * pixmap's depth and geometry.
     */
    return _LTAddPixmapToCache(image_name, pixmap, screen,
			      foreground, background,
			      0, 0, 0, 0, 0);
}

/*
 * Find pixmap by (screen, pixmap); return True if it's in the cache.
 *
 * Found description of this sucker in McMinds & Whitty, "Writing Your
 * Own OSF/Motif Widgets" HP Professional Books, ISBN 0-13-104191-6,
 * kindly donated to the LessTif project by Linux International.
 */
Boolean
_XmGetPixmapData(Screen *screen, Pixmap pixmap,
		 char **image_name,
		 int *depth, Pixel *foreground, Pixel *background,
		 int *hot_x, int *hot_y,
		 unsigned int *width, unsigned int *height)
{
    LTPixmapDescRec PixmapDesc;
    LTPixmapDesc CachedPixmap;

    if (PixmapCache == NULL)
    {
	LTSetupPixmapCache();
    }

    PixmapDesc.screen = screen;
    PixmapDesc.pixmap = pixmap;
    if (!_LTHashTableLookupItem(RevPixmapCache, (LTHashItemID) & PixmapDesc,
			       (LTHashItemValue *) & CachedPixmap))
    {
	return False;
    }

    *image_name = CachedPixmap->image_name;
    *depth = CachedPixmap->depth;
    *foreground = CachedPixmap->foreground;
    *background = CachedPixmap->background;
    *hot_x = CachedPixmap->hot_x;
    *hot_y = CachedPixmap->hot_y;
    *width = CachedPixmap->width;
    *height = CachedPixmap->height;

    return True;
}

/* --------------------------------------------------------------------------
 * --------------------------------------------------------------------------
 * private LessTif functions
 */
/*
 * The pre-defined images live here. They are added to the cache as
 * soon as this sucker gets initialized. Thus there is no need any
 * more to add the default images whenever a XmDisplay gets created.
 */
#define background_width 16
#define background_height 16
static unsigned char background_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define f25_width 16
#define f25_height 16
static unsigned char f25_bits[] =
{
    0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22,
    0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22,
    0x88, 0x88, 0x22, 0x22, 0x88, 0x88, 0x22, 0x22};

#define f50_width 16
#define f50_height 16
static unsigned char f50_bits[] =
{
    0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
    0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
    0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa};

#define f75_width 16
#define f75_height 16
static unsigned char f75_bits[] =
{
    0x55, 0x55, 0xff, 0xff, 0xaa, 0xaa, 0xff, 0xff, 0x55, 0x55, 0xff, 0xff,
    0xaa, 0xaa, 0xff, 0xff, 0x55, 0x55, 0xff, 0xff, 0xaa, 0xaa, 0xff, 0xff,
    0x55, 0x55, 0xff, 0xff, 0xaa, 0xaa, 0xff, 0xff};

#define horizontal_width 16
#define horizontal_height 16
static unsigned char horizontal_bits[] =
{
    0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00,
    0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00,
    0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00};

#define vertical_width 16
#define vertical_height 16
static unsigned char vertical_bits[] =
{
    0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
    0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
    0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55};

#define slant_right_width 16
#define slant_right_height 16
static unsigned char slant_right_bits[] =
{
    0x77, 0x77, 0xbb, 0xbb, 0xdd, 0xdd, 0xee, 0xee, 0x77, 0x77, 0xbb, 0xbb,
    0xdd, 0xdd, 0xee, 0xee, 0x77, 0x77, 0xbb, 0xbb, 0xdd, 0xdd, 0xee, 0xee,
    0x77, 0x77, 0xbb, 0xbb, 0xdd, 0xdd, 0xee, 0xee};

#define slant_left_width 16
#define slant_left_height 16
static unsigned char slant_left_bits[] =
{
    0xee, 0xee, 0xdd, 0xdd, 0xbb, 0xbb, 0x77, 0x77, 0xee, 0xee, 0xdd, 0xdd,
    0xbb, 0xbb, 0x77, 0x77, 0xee, 0xee, 0xdd, 0xdd, 0xbb, 0xbb, 0x77, 0x77,
    0xee, 0xee, 0xdd, 0xdd, 0xbb, 0xbb, 0x77, 0x77};

#define menu_cascade_width 16
#define menu_cascade_height 16
static unsigned char menu_cascade_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x03, 0x00, 0x07, 0x00, 0x0f,
    0xff, 0x1f, 0xff, 0x3f, 0xff, 0x3f, 0xff, 0x1f, 0x00, 0x0f, 0x00, 0x07,
    0x00, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00};

#define menu_cascade_rtol_width 16
#define menu_cascade_rtol_height 16
static unsigned char menu_cascade_rtol_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0xc0, 0x00, 0xe0, 0x00, 0xf0, 0x00,
    0xf8, 0xff, 0xfc, 0xff, 0xfc, 0xff, 0xf8, 0xff, 0xf0, 0x00, 0xe0, 0x00,
    0xc0, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00};

#define menu_checkmark_width 16
#define menu_checkmark_height 16
static unsigned char menu_checkmark_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x70, 0x00, 0x38,
    0x00, 0x1c, 0x00, 0x0e, 0x18, 0x07, 0xb8, 0x03, 0xf8, 0x03, 0xf0, 0x01,
    0xe0, 0x01, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00};

static struct
{
    String ImageName;
    unsigned char *ImageBits;
}
DefaultImages[] =
{
    {
	"background", background_bits
    }
    ,
    {
	"25_foreground", f25_bits
    }
    ,
    {
	"50_foreground", f50_bits
    }
    ,
    {
	"75_foreground", f75_bits
    }
    ,
    {
	"horizontal_tile", horizontal_bits
    }
    ,
    {
	"vertical_tile", vertical_bits
    }
    ,
    {
	"slant_right", slant_right_bits
    }
    ,
    {
	"slant_left", slant_left_bits
    }
    ,
    {
	"menu_cascade", menu_cascade_bits
    }
    ,
    {
	"menu_cascade_rtol", menu_cascade_rtol_bits
    }
    ,
    {
	"menu_checkmark", menu_checkmark_bits
    }
    ,
    {
	NULL, NULL
    }
};

/*
 * Initializes the image and pixmap caches. In addition, it puts the default
 * images into the cache.
 */
extern void _XInitImageFuncPtrs(XImage *);

static void
LTSetupImageCache(void)
{
    int i;
    XImage *Image;
    LTImageValue ImageValue;

    /*
     * The image cache associates an image name with an XImage pointer and
     * hot spot coordinates. Using the generic hash tables for this makes
     * this really easy.
     */
    ImageCache = _LTHashTableCreate(NULL, NULL, LTHASH_ID_STRING);

    /*
     * Now we put the default images into the cache. I'm not using
     * _XmCreateImage() here as this insists on a display pointer. Instead
     * I'm using a self-made XCreateImage() so I don't need a display
     * pointer at all.
     */
    for (i = 0; DefaultImages[i].ImageName; i++)
    {
	Image = (XImage *)XtCalloc(1, (unsigned)sizeof(XImage));
	Image->width = 16;
	Image->height = 16;
	Image->format = XYBitmap;
	Image->red_mask = 0;
	Image->green_mask = 0;
	Image->blue_mask = 0;
	Image->xoffset = 0;
	Image->bitmap_pad = 8;
	Image->depth = 1;
	Image->data = (char *)DefaultImages[i].ImageBits;
	Image->bytes_per_line = (16 + 7) >> 3;
	Image->bits_per_pixel = 1;

	Image->byte_order = LSBFirst;
	Image->bitmap_unit = 8;
	Image->bitmap_bit_order = LSBFirst;
	_XInitImageFuncPtrs(Image);

	ImageValue = (LTImageValue) XtMalloc(sizeof(LTImageValueRec));
	ImageValue->image = Image;
	ImageValue->hot_x = 0;
	ImageValue->hot_y = 0;
	ImageValue->undeletable = True;
	_LTHashTableAddItem(ImageCache,
			   (LTHashItemID) DefaultImages[i].ImageName,
			   (LTHashItemValue) ImageValue);
    }
}


/*
 * The following two functions are needed for the Pixmap cache. They
 * calculate the hash value for an entry and compare two entries. This
 * is done on the tuple (image name, screen, foreground, background, depth).
 */
static unsigned long
LTPixmapGetHash(LTHashItemID id)
{
    LTPixmapDesc p = (LTPixmapDesc) id;
    unsigned long hash;
    char *pStr, ch;

    hash = 0L;
    pStr = p->image_name;
    while ((ch = *pStr++) != 0)
    {
	hash = (hash << 3) + ch;
    }

    hash += (unsigned long)p->screen;
    hash += ((unsigned long)p->foreground) * 71;
    hash += (unsigned long)p->background;
    hash += (unsigned long)p->depth;

    return hash;
}

static Boolean
LTPixmapCompare(LTHashItemID id1, LTHashItemID id2)
{
    LTPixmapDesc p1 = (LTPixmapDesc) id1;
    LTPixmapDesc p2 = (LTPixmapDesc) id2;

    return (strcmp(p1->image_name, p2->image_name) == 0) &&
	(p1->screen == p2->screen) &&
	(p1->depth == p2->depth) &&
	(p1->foreground == p2->foreground) &&
	(p1->background == p2->background);
}

/*
 * The following two functions are needed for the RevPixmap cache. This
 * is merely a hash table that is used to lookup the information about a
 * particular pixmap using the pixmap and screen ids.
 */
static unsigned long
LTRevPixmapGetHash(LTHashItemID id)
{
    LTPixmapDesc p = (LTPixmapDesc) id;

    return ((unsigned long)p->pixmap) +
	((unsigned long)p->screen);
}

static Boolean
LTRevPixmapCompare(LTHashItemID id1, LTHashItemID id2)
{
    LTPixmapDesc p1 = (LTPixmapDesc) id1;
    LTPixmapDesc p2 = (LTPixmapDesc) id2;

    return (p1->pixmap == p2->pixmap) && (p1->screen == p2->screen);
}

/*
 * Setup the pixmap cache. For reasons of speed and to increase memory
 * usage, we use two hash tables for forward and backward associations.
 * The backward association looks like: (screen, pixmap) ----> (info)
 * We use the same data structure for both parts of the association as
 * they belong together and it makes no sense to allocate them separately.
 * If you look at the code below you should notice that the first hash
 * table takes care of allocating and copying the description data structures
 * into the hash table. So we don't need to take care of allocating and
 * freeing them. For this reason the second (backward) hash table must not
 * do any memory management on the association data (LTHASH_ID_NOCOPY).
 */
static void
LTSetupPixmapCache(void)
{
    PixmapCache = _LTHashTableCreate(LTPixmapGetHash, LTPixmapCompare,
				    sizeof(LTPixmapDescRec));

    RevPixmapCache = _LTHashTableCreate(LTRevPixmapGetHash, LTRevPixmapCompare,
				       LTHASH_ID_NOCOPY);
    DEBUGOUT(_LtDebug(__FILE__, NULL, "LTSetupPixmapCache() PixmapCache %p RevPixmapCache %p\n",
			    PixmapCache, RevPixmapCache));
}

/*
 * Retrieves an image from the cache, given its name. If the image can't be
 * found in the cache, the function simply returns NULL. Otherwise you we'll
 * get back a pointer to the description record of the image within the cache.
 */
static LTImageValue
LTGetImageFromCache(char *image_name)
{
    LTImageValue Image;

    if (ImageCache == NULL)
    {
	LTSetupImageCache();
    }

    if (_LTHashTableLookupItem(ImageCache,
			      image_name, (LTHashItemValue *) & Image))
    {
	return Image;
    }

    return NULL;
}

/*
 * Adds a named pixmap for particular screen with a bunch of other information
 * to the pixmap cache. There is an important thing to note: this function is
 * ordinarily used to add a pixmap to the cache which is not already in the
 * cache. The reference count for this pixmap then will be "1". If there should
 * be already such a pixmap in the cache, then the old pixmap will be silently
 * lost.
 */
Boolean
_LTAddPixmapToCache(const char *pixmap_name, Pixmap pixmap, Screen *screen,
		   Pixel foreground, Pixel background,
		   int depth,
		   int width, int height, int hot_x, int hot_y)
{
    Window RootWindow;
    int x, y;
    unsigned int w, h, bw, d;
    LTPixmapDesc PixmapDesc, OldPixmapDesc = NULL;

    if (pixmap_name == NULL)
    {
	return False;
    }

    if (PixmapCache == NULL)
    {
	LTSetupPixmapCache();
    }

    /*
     * For those who always don't now everything, we provide some help...
     */
    if ((width == 0) || (height == 0) || (depth == 0))
    {
	XGetGeometry(DisplayOfScreen(screen), pixmap,
		     &RootWindow, &x, &y, &w, &h, &bw, &d);
	depth = d;
	width = w;
	height = h;
    }

    PixmapDesc = (LTPixmapDesc) XtMalloc(sizeof(LTPixmapDescRec));
    PixmapDesc->image_name = XtNewString(pixmap_name);
    PixmapDesc->screen = screen;
    PixmapDesc->foreground = foreground;
    PixmapDesc->background = background;
    PixmapDesc->depth = depth;
    PixmapDesc->pixmap = pixmap;
    PixmapDesc->width = width;
    PixmapDesc->height = height;
    PixmapDesc->hot_x = hot_x;
    PixmapDesc->hot_y = hot_y;

    PixmapDesc->ref_count = 1;

    /*
     * If there's already such an entry in the cache, then we'll silently
     * overwrite it (compatibility with _XmInstallPixmap()). Sigh.
     */
    if (_LTHashTableReplaceItemAndID(PixmapCache,
				    (LTHashItemID) PixmapDesc,
				    (LTHashItemValue) PixmapDesc,
				    (LTHashItemID *) OldPixmapDesc,
				    NULL))
    {
	XtFree((char *)OldPixmapDesc->image_name);
	XtFree((char *)OldPixmapDesc);
    }

    _LTHashTableReplaceItem(RevPixmapCache, (LTHashItemID) PixmapDesc,
			   (LTHashItemValue) PixmapDesc, NULL);

    return True;
}


extern void
_LTCreateSearchPath(void)
{
    const char *XBMLANGPATH = getenv("XBMLANGPATH");
    const char *XAPPLRESDIR = getenv("XAPPLRESDIR");
    const char *HOME = _XmOSGetHomeDirName();

    
    if (HOME == NULL) {
	HOME = "";
    }
    if (XBMLANGPATH)
    {
      _search_path = XtNewString(XBMLANGPATH);
    }
    else if (XAPPLRESDIR)
    {

	_search_path = (char *)XtMalloc(strlen(XAPPLRESDIR_set_pattern)
					+ strlen(XAPPLRESDIR) * 6
					+ strlen(HOME) * 2 + 1);

	sprintf(_search_path, XAPPLRESDIR_set_pattern,
		XAPPLRESDIR, XAPPLRESDIR, XAPPLRESDIR,
		XAPPLRESDIR, XAPPLRESDIR, XAPPLRESDIR,
		HOME, HOME);

    }
    else
    {				/* neither of them was set... */
	_search_path = (char *)XtMalloc(strlen(nothing_set_pattern)
					+ strlen(HOME) * 7 + 1);

	sprintf(_search_path, nothing_set_pattern,
		HOME, HOME, HOME, HOME, HOME, HOME, HOME);
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "Using %s for search path\n", _search_path));
}

/*
 * Get a scaled pixmap.
 *
 * If scaling_ratio == 0 then use print shell resolution,
 * if scaling_ratio == 1 then don't scale.
 */
extern XtEnum 
XmGetScaledPixmap(Widget widget,
                  String image_name,
                  Pixel foreground,
                  Pixel background,
                  int depth,
                  double scaling_ratio)
{
   if (!widget || !image_name)
      return 0;

   _XmWarning(NULL, "XmGetScaledPixmap() is not yet implemented!");
   return 0;
}

/*
 * The two functions below are an experimental attempt to clear the
 * pixmap cache of all entries related to some screen when that screen
 * is closed.
 *
 * static LTHashForEachIteratorResult
 * LTImageIterator(LTHashTable ht,
 * 		LTHashItemID id, LTHashItemValue value, XtPointer image)
 * {
 *     if (((LTImageValue) value)->image == (XImage *)image)
 *     {
 * 	if (((LTImageValue) value)->undeletable)
 * 	{
 * 	    return LTHASH_BREAK;
 * 	}
 * 
 * 	_LTHashTableRemoveItem(ht, id, NULL);
 * 
 * 	XtFree((char *)value);
 * 
 * 	return LTHASH_COUNTANDBREAK;
 *     }
 * 
 *     return LTHASH_CONT;
 * }
 */
 /* We don't care about the count */
static LTHashForEachIteratorResult
YowIter(LTHashTable ht, LTHashItemID id, LTHashItemValue v, XtPointer client)
{
	Screen		*s = (Screen *)client;
	LTPixmapDescRec	*PixmapDesc = (LTPixmapDescRec *)v;

#if 0
	fprintf(stderr, "YowIter(%p,%p,%p,%p)\n", ht, id, v, client);
#endif

	if (v == NULL)
#if 1
		return LTHASH_COUNT;
#else
		return LTHASH_BREAK;
#endif
	/*
	 * This extra check appears to help the Eclipse termination a bit
	 */
	if (id == NULL)
		return LTHASH_BREAK;

	if (s == PixmapDesc->screen)
		(void)_LTHashTableRemoveItem(ht, id, NULL);
	return LTHASH_COUNT;
}

void 
_LtImageCacheScreenDestroy(Screen *s)
{
#if 0
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "_LtImageCacheScreenDestroy %p - DISABLED !\n", s));
#else
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "_LtImageCacheScreenDestroy %p\n", s));
	if (PixmapCache != NULL) {
		(void) _LTHashTableForEachItem(PixmapCache, YowIter, (XtPointer)s);
	}
#endif
}
