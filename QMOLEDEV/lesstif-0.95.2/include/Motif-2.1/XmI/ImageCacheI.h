/**
 *
 * $Id: ImageCacheI.h,v 1.1 2004/08/28 19:23:29 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2003 LessTif Development Team
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

#ifndef _XMI_IMAGECACHEI_H
#define _XMI_IMAGECACHEI_H

/*
 * The image cache associates:
 *   image name <----> (XImage, hot_x, hot_y, flag)
 * The undeletable flag indicates that this particular image is a predefined
 * one that can't be uninstalled.
 */
typedef struct _LTImageValueRec {
    XImage  *image;
    int      hot_x;
    int      hot_y;
    Boolean  undeletable;
} LTImageValueRec, *LTImageValue;

/*
 * The pixmap cache associates:
 *   (image name, screen, foreground, background, depth) <---->
 *           (pixmap, ref_count, hot_x, hot_y)
 */
typedef struct _LTPixmapDescRec {
    String  image_name;
    Screen *screen;
    Pixel   foreground;
    Pixel   background;
    int     depth;
    /* Here starts the second part, that is the (pixmap, .... ) assoc stuff */
    Pixmap  pixmap;
    int     ref_count;
    int     width;
    int     height;
    int     hot_x;
    int     hot_y;
} LTPixmapDescRec, *LTPixmapDesc;


Boolean _LTAddPixmapToCache(const char *pixmap_name, Pixmap pixmap, Screen *screen,
			   Pixel foreground, Pixel background,
			   int depth, int width, int height,
			   int hot_x, int hot_y);

			   void _LTCreateSearchPath(void);

void _LtImageCacheScreenDestroy(Screen *s);

#endif /* _XMI_IMAGECACHEI_H */
