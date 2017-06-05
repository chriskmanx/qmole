/* $Id: icons.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
/****************************************************************************
 * This module is mostly all new
 * by Rob Nation
 * A little of it is borrowed from ctwm.
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/
/***********************************************************************
 *
 * mwm icon code
 *
 ***********************************************************************/

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <X11/Intrinsic.h>
#include <X11/extensions/shape.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#if XmVERSION >= 2
#include <Xm/XpmP.h>
#else
#include <XmI/XmXpm.h>
#endif

#include "mwm.h"


#define def_bitmap_width 50
#define def_bitmap_height 50
static unsigned char def_bitmap_bits[] =
{
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0x01, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x02, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x01, 0x00, 0x80,
    0x00, 0x00, 0x20, 0x02, 0x01, 0x00, 0xc0, 0x00, 0x00, 0x30, 0x02, 0xe1,
    0xff, 0xdf, 0xf0, 0xff, 0x37, 0x02, 0xe1, 0xff, 0xdf, 0xb0, 0xaa, 0x36,
    0x02, 0xe1, 0xff, 0xdf, 0x50, 0x55, 0x35, 0x02, 0xe1, 0xff, 0xdf, 0xb0,
    0xaa, 0x36, 0x02, 0xe1, 0xff, 0xdf, 0x50, 0x55, 0x35, 0x02, 0xe1, 0xff,
    0xdf, 0xb0, 0xaa, 0x36, 0x02, 0xe1, 0xff, 0xdf, 0x50, 0x55, 0x35, 0x02,
    0xe1, 0xff, 0xdf, 0xb0, 0xaa, 0x36, 0x02, 0xe1, 0xff, 0xdf, 0x50, 0x55,
    0x35, 0x02, 0xe1, 0xff, 0xdf, 0xb0, 0xaa, 0x36, 0x02, 0xe1, 0xff, 0xdf,
    0x50, 0x55, 0x35, 0x02, 0xe1, 0xff, 0xdf, 0xb0, 0xaa, 0x36, 0x02, 0xe1,
    0xff, 0xdf, 0x50, 0x55, 0x35, 0x02, 0xe1, 0xff, 0xdf, 0xf0, 0xff, 0x37,
    0x02, 0x01, 0x00, 0x80, 0x00, 0x00, 0x20, 0x02, 0xf1, 0xff, 0x3f, 0xf8,
    0xff, 0x0f, 0x02, 0xf9, 0xff, 0x7f, 0xfc, 0xff, 0x1f, 0x02, 0x01, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x02, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x01, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x02, 0x01, 0x00, 0x80, 0x00, 0x00, 0x20, 0x02, 0x01, 0x00, 0xc0,
    0x00, 0x00, 0x30, 0x02, 0xe1, 0xff, 0xdf, 0xf0, 0xff, 0x37, 0x02, 0x61,
    0x55, 0xd5, 0xf0, 0xff, 0x37, 0x02, 0xa1, 0xaa, 0xda, 0xf0, 0xff, 0x37,
    0x02, 0x61, 0x55, 0xd5, 0xf0, 0xff, 0x37, 0x02, 0xa1, 0xaa, 0xda, 0xf0,
    0xff, 0x37, 0x02, 0x61, 0x55, 0xd5, 0xf0, 0xff, 0x37, 0x02, 0xa1, 0xaa,
    0xda, 0xf0, 0xff, 0x37, 0x02, 0x61, 0x55, 0xd5, 0xf0, 0xff, 0x37, 0x02,
    0xa1, 0xaa, 0xda, 0xf0, 0xff, 0x37, 0x02, 0x61, 0x55, 0xd5, 0xf0, 0xff,
    0x37, 0x02, 0xa1, 0xaa, 0xda, 0xf0, 0xff, 0x37, 0x02, 0x61, 0x55, 0xd5,
    0xf0, 0xff, 0x37, 0x02, 0xa1, 0xaa, 0xda, 0xf0, 0xff, 0x37, 0x02, 0x61,
    0x55, 0xd5, 0xf0, 0xff, 0x37, 0x02, 0xe1, 0xff, 0xdf, 0xf0, 0xff, 0x37,
    0x02, 0x01, 0x00, 0x80, 0x00, 0x00, 0x20, 0x02, 0xf1, 0xff, 0x3f, 0xf8,
    0xff, 0x0f, 0x02, 0xf9, 0xff, 0x7f, 0xfc, 0xff, 0x1f, 0x02, 0x01, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x02, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0x03};


#ifdef __EMX__
 /* dos-like */
#define cPATHELEMENTSEP ';'
#else
  /* un*x-like */
#define cPATHELEMENTSEP ':'
#endif

/*
 * Find the specified icon file somewhere along the given path.
 *
 * There is a possible race condition here:  We check the file and later
 * do something with it.  By then, the file might not be accessible.
 * Oh well.
 */
static char *
find_icon_file(const char *icon, const char *pathlist, int type)
{
    char *path;
    char *dir_end;
    int l1, l2;

    if (icon != NULL)
	l1 = strlen(icon);
    else
	l1 = 0;

    if (pathlist != NULL)
	l2 = strlen(pathlist);
    else
	l2 = 0;

    path = XtMalloc(l1 + l2 + 10);
    *path = '\0';
    if (*icon == '/')
    {
	/* No search if icon begins with a slash */
	strcpy(path, icon);
	return path;
    }

    if ((pathlist == NULL) || (*pathlist == '\0'))
    {
	/* No search if pathlist is empty */
	strcpy(path, icon);
	return path;
    }

    /* Search each element of the pathlist for the icon file */
    while ((pathlist) && (*pathlist))
    {
	dir_end = strchr(pathlist, cPATHELEMENTSEP);
	if (dir_end != NULL)
	{
	    strncpy(path, pathlist, dir_end - pathlist);
	    path[dir_end - pathlist] = 0;
	}
	else
	    strcpy(path, pathlist);

	strcat(path, "/");
	strcat(path, icon);
	if (access(path, type) == 0)
	    return path;
	strcat(path, ".gz");
	if (access(path, type) == 0)
	    return path;

	/* Point to next element of the path */
	if (dir_end == NULL)
	    pathlist = NULL;
	else
	    pathlist = dir_end + 1;
    }
    /* Hmm, couldn't find the file.  Return NULL */
    XtFree(path);
    return NULL;
}

/*
 * grab needed buttons for the icon window
 */
static void
grab_icon_buttons(ScreenInfo *scr, MwmWindow *tmp_win, Window w)
{
    MouseButton *MouseEntry;

    MouseEntry = scr->buttons;
    while (MouseEntry != (MouseButton *)0)
    {
	if ((MouseEntry->func != (int)0) && (MouseEntry->context & C_ICON))
	{
	    if (MouseEntry->button > 0)
		XGrabButton(dpy, MouseEntry->button, MouseEntry->modifier, w,
			    True, ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync, None,
			    scr->cursors[DEFAULT_CURS]);
	    else
	    {
		XGrabButton(dpy, 1, MouseEntry->modifier, w,
			    True, ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync, None,
			    scr->cursors[DEFAULT_CURS]);
		XGrabButton(dpy, 2, MouseEntry->modifier, w,
			    True, ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync, None,
			    scr->cursors[DEFAULT_CURS]);
		XGrabButton(dpy, 3, MouseEntry->modifier, w,
			    True, ButtonPressMask | ButtonReleaseMask,
			    GrabModeAsync, GrabModeAsync, None,
			    scr->cursors[DEFAULT_CURS]);
	    }
	}

	MouseEntry = MouseEntry->next;
    }
}

/*
 * grab needed keys for the icon window
 */
static void
grab_icon_keys(ScreenInfo *scr, MwmWindow *tmp_win, Window w)
{
    FuncKey *tmp;

    for (tmp = scr->keys; tmp != NULL; tmp = tmp->next)
    {
	if (tmp->cont & C_ICON)
	    XGrabKey(dpy, tmp->keycode, tmp->mods, w, True,
		     GrabModeAsync, GrabModeAsync);
    }
}

/*
 * Looks for a monochrome icon bitmap file
 */
static void
get_bitmap_file(ScreenInfo *scr, MwmWindow *tmp_win)
{
    char *path = NULL;
    int HotX, HotY;

    path = find_icon_file(tmp_win->icon_image, scr->IconPath, R_OK);

    if (path == NULL)
	return;
    if (XReadBitmapFile(dpy, scr->root_win, path,
			(unsigned int *)&tmp_win->icon_p_width,
			(unsigned int *)&tmp_win->icon_p_height,
			&tmp_win->icon_pixmap,
			&HotX, &HotY) != BitmapSuccess)
    {
	tmp_win->icon_p_width = 0;
	tmp_win->icon_p_height = 0;
    }

    XtFree(path);
}

/*
 * Looks for a color XPM icon file
 */
static void
get_xpm_file(ScreenInfo *scr, MwmWindow *tmp_win)
{
    XWindowAttributes root_attr;
#if XmVERSION >= 2
    XpmAttributes xpm_attributes;
#else
    _LtXpmAttributes xpm_attributes;
#endif
    XImage *im, *imshape;
    GC gc;
    char *path = NULL;

    path = find_icon_file(tmp_win->icon_image, scr->PixmapPath, R_OK);
    if (path == NULL)
	return;

    XGetWindowAttributes(dpy, scr->root_win, &root_attr);
    xpm_attributes.colormap = root_attr.colormap;
    xpm_attributes.closeness = 40000;	/* Allow for "similar" colors */
#if XmVERSION >= 2
    xpm_attributes.valuemask = XpmSize | XpmReturnPixels | XpmColormap |
	XpmCloseness;
#else
    xpm_attributes.valuemask = _LtXpmSize | _LtXpmReturnPixels | _LtXpmColormap |
	_LtXpmCloseness;
#endif

#ifdef NONSTANDARD_CONVERTERS
#if XmVERSION >= 2
    if (XpmReadFileToImage(dpy, path, &im, &imshape, &xpm_attributes) == XpmSuccess)
#else
    if (_LtXpmReadFileToImage(dpy, path, &im, &imshape, &xpm_attributes) == _LtXpmSuccess)
#endif
    {

	tmp_win->icon_pixmap = XCreatePixmap(dpy, scr->root_win,
					     im->width, im->height,
					     scr->d_depth);

	if (imshape)
	    tmp_win->icon_mask_pixmap = XCreatePixmap(dpy, scr->root_win,
						  imshape->width,
						  imshape->height,
						  1);

	gc = XCreateGC(dpy, tmp_win->icon_pixmap, 0, NULL);

	XPutImage(dpy, tmp_win->icon_pixmap, gc, im, 0, 0, 0, 0,
		  im->width, im->height);

	XFreeGC(dpy, gc);

	gc = XCreateGC(dpy, tmp_win->icon_mask_pixmap, 0, NULL);

	if (imshape)
	    XPutImage(dpy, tmp_win->icon_mask_pixmap, gc, imshape, 0, 0, 0, 0,
		  imshape->width, imshape->height);

	XFreeGC(dpy, gc);

	if (tmp_win->icon_mask_pixmap)
	    tmp_win->flags |= SHAPED_ICON;
	tmp_win->icon_p_width = xpm_attributes.width;
	tmp_win->icon_p_height = xpm_attributes.height;
	tmp_win->flags |= XPM_FLAG;
	tmp_win->flags |= PIXMAP_OURS;
	tmp_win->icon_depth = scr->d_depth;
    }
#endif

    XtFree(path);
}

/*
 * Looks for an application supplied icon window
 */
static void
get_icon_window(ScreenInfo *scr, MwmWindow *tmp_win)
{
    /* We are guaranteed that wmhints is non-null when calling this
     * routine */
    if (XGetGeometry(dpy, tmp_win->wmhints->icon_window, &JunkRoot,
		     &JunkX, &JunkY, (unsigned int *)&tmp_win->icon_p_width,
		     (unsigned int *)&tmp_win->icon_p_height,
		     &JunkBW, &JunkDepth) == 0)
    {
	fprintf(stderr, "Help! Bad Icon Window!\n");
    }
    tmp_win->icon_p_width += JunkBW << 1;
    tmp_win->icon_p_height += JunkBW << 1;
    /*
     * Now make the new window the icon window for this window,
     * and set it up to work as such (select for key presses
     * and button presses/releases, set up the contexts for it,
     * and define the cursor for it).
     */
    tmp_win->icon_pixmap_w = tmp_win->wmhints->icon_window;
    if (tmp_win->wmhints->flags & IconMaskHint)
    {
	tmp_win->flags |= SHAPED_ICON;
	tmp_win->icon_mask_pixmap = tmp_win->wmhints->icon_mask;
    }
    tmp_win->flags &= ~ICON_OURS;
}

/*
 * Looks for an application supplied bitmap or pixmap
 */
static void
get_icon_bitmap(MwmWindow *tmp_win)
{
    /* We are guaranteed that wmhints is non-null when calling this
     * routine */
    XGetGeometry(dpy, tmp_win->wmhints->icon_pixmap, &JunkRoot, &JunkX, &JunkY,
		 (unsigned int *)&tmp_win->icon_p_width,
		 (unsigned int *)&tmp_win->icon_p_height, &JunkBW, &JunkDepth);
    tmp_win->icon_pixmap = tmp_win->wmhints->icon_pixmap;
    tmp_win->icon_depth = JunkDepth;
    if (tmp_win->wmhints->flags & IconMaskHint)
    {
	tmp_win->flags |= SHAPED_ICON;
	tmp_win->icon_mask_pixmap = tmp_win->wmhints->icon_mask;
    }
}

#if 0
/*
 * draw icon border windows
 */
static void
draw_icon_border(MwmWindow *t, Window win, int x, int y, int w, int h,
		 GC ReliefGC, GC ShadowGC)
{
    XSegment seg[4];
    int i;

    /* top */
    if (win == t->icon_borders[0])
    {
	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y;

	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = x;
	seg[i++].y2 = h + y - 1;

	XDrawSegments(dpy, win, ReliefGC, seg, i);

	i = 0;
	seg[i].x1 = x + t->icon_border_width;
	seg[i].y1 = y + h - 1;
	seg[i].x2 = w + x - 1 - t->icon_border_width;
	seg[i++].y2 = y + h - 1;

	seg[i].x1 = x + w - 1;
	seg[i].y1 = y;
	seg[i].x2 = x + w - 1;
	seg[i++].y2 = y + h - 1;

	XDrawSegments(dpy, win, ShadowGC, seg, i);
    }
    /* right */
    else if (win == t->icon_borders[1])
    {
	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y;

	seg[i].x1 = x;
	seg[i].y1 = y + t->icon_border_width - 1;
	seg[i].x2 = x;
	seg[i++].y2 = h + y - 1 - t->icon_border_width;

	XDrawSegments(dpy, win, ReliefGC, seg, i);

	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y + h - 1;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y + h - 1;

	seg[i].x1 = x + w - 1;
	seg[i].y1 = y;
	seg[i].x2 = x + w - 1;
	seg[i++].y2 = y + h - 1;

	XDrawSegments(dpy, win, ShadowGC, seg, i);
    }
    /* bottom */
    else if (win == t->icon_borders[2])
    {
	i = 0;
	seg[i].x1 = x + t->icon_border_width;
	seg[i].y1 = y;
	seg[i].x2 = w + x - t->icon_border_width;
	seg[i++].y2 = y;

	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = x;
	seg[i++].y2 = h + y - 1;

	XDrawSegments(dpy, win, ReliefGC, seg, i);

	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y + h - 1;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y + h - 1;

	seg[i].x1 = x + w - 1;
	seg[i].y1 = y;
	seg[i].x2 = x + w - 1;
	seg[i++].y2 = y + h - 1;

	XDrawSegments(dpy, win, ShadowGC, seg, i);
    }
    /* left */
    else
    {
	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y;

	seg[i].x1 = x;
	seg[i].y1 = y;
	seg[i].x2 = x;
	seg[i++].y2 = h + y - 1;

	XDrawSegments(dpy, win, ReliefGC, seg, i);

	i = 0;
	seg[i].x1 = x;
	seg[i].y1 = y + h - 1;
	seg[i].x2 = w + x - 1;
	seg[i++].y2 = y + h - 1;

	seg[i].x1 = x + w - 1;
	seg[i].y1 = y + t->icon_border_width - 1;
	seg[i].x2 = x + w - 1;
	seg[i++].y2 = y + h - t->icon_border_width;

	XDrawSegments(dpy, win, ShadowGC, seg, i);
    }
}

#endif

/*
 * Creates an icon window as needed
 */
void
ICON_CreateWindow(ScreenInfo *scr, MwmWindow *tmp_win, int def_x, int def_y)
{
    int final_x, final_y;
    unsigned long valuemask;	/* mask for create windows */
    XSetWindowAttributes attributes;	/* attributes for create windows */

    tmp_win->flags |= ICON_OURS;
    tmp_win->flags &= ~XPM_FLAG;
    tmp_win->flags &= ~PIXMAP_OURS;
    tmp_win->flags &= ~SHAPED_ICON;
    tmp_win->icon_pixmap_w = None;
    tmp_win->icon_pixmap = None;
    tmp_win->icon_depth = 0;

    /* First, see if it was specified in the .mwmrc */
    tmp_win->icon_p_height = 0;
    tmp_win->icon_p_width = 0;

    if ((scr->icon_decoration & XmICON_IMAGE) && tmp_win->use_client_icon)
    {
	/* First, See if the app supplies its own icon window */
	if ((tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0) &&
	    (tmp_win->wmhints) && (tmp_win->wmhints->flags & IconWindowHint))
	    get_icon_window(scr, tmp_win);

	/* Next, try to get icon bitmap from the application */
	if ((tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0) &&
	    (tmp_win->wmhints) && (tmp_win->wmhints->flags & IconPixmapHint))
	    get_icon_bitmap(tmp_win);

	/* Next, check for a color pixmap */
	if (tmp_win->icon_image != NULL &&
	    (tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0))
	    get_xpm_file(scr, tmp_win);

	/* Finally, check for a monochrome bitmap */
	if (tmp_win->icon_image != NULL &&
	    (tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0))
	    get_bitmap_file(scr, tmp_win);
    }
    else if (scr->icon_decoration & XmICON_IMAGE)
    {
	/* First, check for a color pixmap */
	if (tmp_win->icon_image != NULL &&
	    (tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0))
	    get_xpm_file(scr, tmp_win);

	/* Next, check for a monochrome bitmap */
	if (tmp_win->icon_image != NULL &&
	    (tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0))
	    get_bitmap_file(scr, tmp_win);

	/* Next, See if the app supplies its own icon window */
	if ((tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0) &&
	    (tmp_win->wmhints) && (tmp_win->wmhints->flags & IconWindowHint))
	    get_icon_window(scr, tmp_win);

	/* Finally, try to get icon bitmap from the application */
	if ((tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0) &&
	    (tmp_win->wmhints) && (tmp_win->wmhints->flags & IconPixmapHint))
	    get_icon_bitmap(tmp_win);
    }

    /* If all that failed, use the default */
    if ((scr->icon_decoration & XmICON_IMAGE) &&
	(tmp_win->icon_p_height == 0) && (tmp_win->icon_p_width == 0))
    {
	tmp_win->icon_pixmap = XCreateBitmapFromData(dpy,
					scr->root_win, (char *)def_bitmap_bits,
					  def_bitmap_width, def_bitmap_height);
	tmp_win->icon_p_width = def_bitmap_width;
	tmp_win->icon_p_height = def_bitmap_height;
	tmp_win->flags |= PIXMAP_OURS;
    }

    /* compute the icon border width, if applicable */
    if (tmp_win->flags & SHAPED_ICON)
	tmp_win->icon_border_width = 0;
    else
	tmp_win->icon_border_width = scr->frame_border_width;

    /* figure out the icon label size */
    tmp_win->icon_t_width =
	XTextWidth(scr->components[MWM_ICON].font,
		   tmp_win->icon_label, strlen(tmp_win->icon_label));
    tmp_win->icon_w_height = scr->components[MWM_ICON].f_height + 6;

    if ((tmp_win->flags & ICON_OURS) && (tmp_win->icon_p_height > 0))
    {
	tmp_win->icon_p_width += 4;
	tmp_win->icon_p_height += 4;
    }

    if (tmp_win->icon_p_width == 0)
	tmp_win->icon_p_width = tmp_win->icon_t_width + 6;

    tmp_win->icon_w_width = tmp_win->icon_p_width;

    /* clip to fit on screen */
    final_x = def_x;
    final_y = def_y;

    if (final_x < 0)
	final_x = 0;
    if (final_y < 0)
	final_y = 0;

    if (final_x + tmp_win->icon_w_width >= scr->d_width)
	final_x = scr->d_width - tmp_win->icon_w_width - 1;
    if (final_y + tmp_win->icon_w_height >= scr->d_height)
	final_y = scr->d_height - tmp_win->icon_w_height - 1;

    tmp_win->icon_x_loc = final_x;
    tmp_win->icon_xl_loc = final_x;
    tmp_win->icon_y_loc = final_y;

    attributes.background_pixel = tmp_win->icon_image_background;
    valuemask = CWBorderPixel | CWCursor | CWEventMask | CWBackPixel;
    attributes.border_pixel = tmp_win->icon_image_foreground;
    attributes.cursor = scr->cursors[DEFAULT_CURS];
    attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
			     VisibilityChangeMask |
			     ExposureMask | KeyPressMask | EnterWindowMask |
			     FocusChangeMask);

    if (scr->icon_decoration & (XmICON_LABEL | XmICON_ACTIVELABEL))
    {
	tmp_win->icon_w =
	    XCreateWindow(dpy, scr->root_win,
			  final_x, final_y + tmp_win->icon_p_height,
			  tmp_win->icon_w_width, tmp_win->icon_w_height, 0,
			  CopyFromParent, CopyFromParent, CopyFromParent,
			  valuemask, &attributes);
    }
    else
	tmp_win->icon_w = None;

    if (scr->icon_decoration & XmICON_IMAGE)
    {
        /* int i; */

#if 0
	attributes.event_mask = KeyPressMask | ButtonPressMask;
	attributes.override_redirect = True;
	tmp_win->icon_frame = XCreateWindow(dpy, scr->root_win,
					    final_x, final_y,
					    tmp_win->icon_p_width +
					    2 * tmp_win->icon_border_width,
					    tmp_win->icon_p_height +
					    2 * tmp_win->icon_border_width,
					    0, CopyFromParent,
					    CopyFromParent, CopyFromParent,
#if 0
					    CWEventMask | CWOverrideRedirect,
#else
					    valuemask,
#endif
					    &attributes);
#endif

#if 0
	valuemask = CWBorderPixel | CWCursor | CWEventMask | CWBackPixel;
	attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
				 VisibilityChangeMask |
				 ExposureMask | KeyPressMask | EnterWindowMask |
				 FocusChangeMask);
#endif

	if ((tmp_win->flags & ICON_OURS) && (tmp_win->icon_p_width > 0) &&
	    (tmp_win->icon_p_height > 0))
	{
	    tmp_win->icon_pixmap_w =
		XCreateWindow(dpy,
#if 0
			      tmp_win->icon_frame,
#else
			      scr->root_win,
#endif
			      final_x + tmp_win->icon_border_width,
			      final_y + tmp_win->icon_border_width,
			      tmp_win->icon_p_width,
			      tmp_win->icon_p_height,
			      0, CopyFromParent,
			      CopyFromParent, CopyFromParent,
			      valuemask, &attributes);
	}
	else
	{
	    attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
				     VisibilityChangeMask |
				     KeyPressMask | EnterWindowMask |
				     FocusChangeMask | LeaveWindowMask);

	    valuemask = CWEventMask;
	    XChangeWindowAttributes(dpy, tmp_win->icon_pixmap_w,
				    valuemask, &attributes);

#if 0
	    /* This used to say:
	     *   Make sure that the window is a child of the root window!
	     *   Olwais screws this up, maybe others do too!
	     * Now, make sure that *we're* the parent of the pixmap window
	     */
	    XReparentWindow(dpy, tmp_win->icon_pixmap_w, tmp_win->icon_frame,
			    tmp_win->icon_border_width,
			    tmp_win->icon_border_width);
#else
	    XReparentWindow(dpy, tmp_win->icon_pixmap_w, scr->root_win,
			    0, 0);
#endif
	}

#if 0
	if (!(tmp_win->flags & SHAPED_ICON))
	{
	    for (i = 0; i < 4; i++)
	    {
		tmp_win->icon_borders[i] =
		    XCreateWindow(dpy, tmp_win->icon_frame, 0, 0,
				  tmp_win->icon_border_width,
				  tmp_win->icon_border_width,
				  0, CopyFromParent,
				  CopyFromParent, CopyFromParent,
				  valuemask, &attributes);

		XSaveContext(dpy, tmp_win->icon_borders[i],
			     MwmContext, (XPointer)tmp_win);
	    }
	}
	else
	{
	    for (i = 0; i < 4; i++)
		tmp_win->icon_borders[i] = None;
	}
	XMapWindow(dpy, tmp_win->icon_frame);
	XMapSubwindows(dpy, tmp_win->icon_frame);
	XRaiseWindow(dpy, tmp_win->icon_pixmap_w);
#endif
    }
    else
	tmp_win->icon_pixmap_w = None;


    if (tmp_win->flags & SHAPED_ICON)
    {
	XShapeCombineMask(dpy, tmp_win->icon_pixmap_w, ShapeBounding, 2, 2,
			  tmp_win->icon_mask_pixmap, ShapeSet);
    }

    if (tmp_win->icon_w != None)
    {
	XSaveContext(dpy, tmp_win->icon_w, MwmContext, (XPointer)tmp_win);
	XDefineCursor(dpy, tmp_win->icon_w, scr->cursors[DEFAULT_CURS]);
	grab_icon_buttons(scr, tmp_win, tmp_win->icon_w);
	grab_icon_keys(scr, tmp_win, tmp_win->icon_w);
    }
    if (tmp_win->icon_pixmap_w != None)
    {
	XSaveContext(dpy, tmp_win->icon_pixmap_w, MwmContext, (XPointer)tmp_win);
	XDefineCursor(dpy, tmp_win->icon_pixmap_w, scr->cursors[DEFAULT_CURS]);
	grab_icon_buttons(scr, tmp_win, tmp_win->icon_pixmap_w);
	grab_icon_keys(scr, tmp_win, tmp_win->icon_pixmap_w);
    }
}

/*
 * Draws the icon window
 */
void
ICON_DrawWindow(ScreenInfo *scr, MwmWindow *tmp)
{
    GC Shadow, Relief;
    Pixel TextColor, BackColor;
    int x;
    char *label;

    if (tmp->icon_w != None)
	MISC_FlushExpose(tmp->icon_w);
    if (tmp->icon_pixmap_w != None)
	MISC_FlushExpose(tmp->icon_pixmap_w);

    if (tmp == scr->mwm_highlight && (scr->icon_decoration & XmICON_ACTIVELABEL)
 && tmp->icon_active_label != NoName && tmp->icon_active_label != NULL
      && tmp->icon_active_label[0] != '\0')
	label = tmp->icon_active_label;
    else
	label = tmp->icon_label;

    if (label == NULL)
	label = "";

    tmp->icon_t_width = XTextWidth(scr->components[MWM_ICON].font,
				   label, strlen(label));

    if (scr->mwm_highlight == tmp)
    {
	Relief = scr->components[MWM_ICON].active_top_GC;
	Shadow = scr->components[MWM_ICON].active_bot_GC;
	/* resize the icon name window */
	if (tmp->icon_w != None)
	{
	    tmp->icon_w_width = tmp->icon_t_width + 6;
	    if (tmp->icon_w_width < tmp->icon_p_width)
		tmp->icon_w_width = tmp->icon_p_width;
	    tmp->icon_xl_loc = tmp->icon_x_loc -
		(tmp->icon_w_width - tmp->icon_p_width) / 2;
	}
	TextColor = scr->components[MWM_ICON].active_foreground;
	BackColor = scr->components[MWM_ICON].active_background;
    }
    else
    {
	Relief = scr->components[MWM_ICON].top_GC;
	Shadow = scr->components[MWM_ICON].bot_GC;

	/* resize the icon name window */
	if (tmp->icon_w != None)
	{
	    tmp->icon_w_width = tmp->icon_p_width;
	    tmp->icon_xl_loc = tmp->icon_x_loc;
	}
	TextColor = scr->components[MWM_ICON].foreground;
	BackColor = scr->components[MWM_ICON].background;

    }

#if 1
    if ((tmp->flags & ICON_OURS) && (tmp->icon_pixmap_w != None))
	XSetWindowBackground(dpy, tmp->icon_pixmap_w,
			     BackColor);
#else
    if (!(tmp->flags & SHAPED_ICON))
    {
	int i;
	XSetWindowAttributes attributes;
	unsigned long valuemask;

	for (i = 0; i < 4; i++)
	{
	    XWindowChanges xwc;
	    unsigned int xwcm = CWWidth | CWHeight | CWX | CWY;

	    if (i == 0)
	    {
		xwc.x = 0;
		xwc.y = 0;
		xwc.height = tmp->icon_border_width;
		xwc.width = tmp->icon_p_width + 2 * tmp->icon_border_width;
	    }
	    else if (i == 1)
	    {
		xwc.x = tmp->icon_border_width + tmp->icon_p_width;
		xwc.y = 0;
		xwc.width = tmp->icon_border_width;
		xwc.height = tmp->icon_p_height + 2 * tmp->icon_border_width;
	    }
	    else if (i == 2)
	    {
		xwc.x = 0;
		xwc.y = tmp->icon_border_width + tmp->icon_p_height;
		xwc.height = tmp->icon_border_width;
		xwc.width = tmp->icon_p_width + 2 * tmp->icon_border_width;
	    }
	    else
	    {
		xwc.x = 0;
		xwc.y = 0;
		xwc.width = tmp->icon_border_width;
		xwc.height = tmp->icon_p_height + 2 * tmp->icon_border_width;
	    }
	    XConfigureWindow(dpy, tmp->icon_borders[i], xwcm, &xwc);
	}

	valuemask = CWBackPixel;
	if (scr->mwm_highlight == tmp)
	    attributes.background_pixel = scr->components[MWM_ICON].active_background;
	else
	    attributes.background_pixel = scr->components[MWM_ICON].background;

	for (i = 0; i < 4; i++)
	{
	    XChangeWindowAttributes(dpy, tmp->icon_borders[i],
				    valuemask, &attributes);
	    XClearWindow(dpy, tmp->icon_borders[i]);
	    draw_icon_border(tmp, tmp->icon_borders[i], 0, 0,
			     ((i % 2)
			      ? tmp->icon_border_width
			      : tmp->icon_p_width + 2 * tmp->icon_border_width),
			     ((i % 2)
			      ? tmp->icon_p_height + 2 * tmp->icon_border_width
			      : tmp->icon_border_width),
			     Relief, Shadow);
	}

	for (i = 0; i < 4; i++)
	    if (tmp->icon_borders[i] != None)
		MISC_FlushExpose(tmp->icon_borders[i]);
    }

#endif

    if (tmp->icon_w != None)
	XSetWindowBackground(dpy, tmp->icon_w, BackColor);

    if (tmp->icon_pixmap_w != None)
    {
#if 0
	XMoveWindow(dpy, tmp->icon_frame, tmp->icon_x_loc, tmp->icon_y_loc);
#else
	XMoveWindow(dpy, tmp->icon_pixmap_w, tmp->icon_x_loc,
		    tmp->icon_y_loc);
#endif
    }

    if (tmp->icon_w != None)
    {
	XMoveResizeWindow(dpy, tmp->icon_w, tmp->icon_xl_loc,
			  tmp->icon_y_loc + tmp->icon_p_height,
			  tmp->icon_w_width,
			  scr->components[MWM_ICON].f_height + 6);

	XClearWindow(dpy, tmp->icon_w);
    }

    if ((tmp->icon_pixmap != None) && (!(tmp->flags & SHAPED_ICON)))
	DEC_DrawShadows(tmp, tmp->icon_pixmap_w, 0, 0,
			tmp->icon_p_width, tmp->icon_p_height,
			Relief, Shadow);

    /* need to locate the icon pixmap */
    if (tmp->icon_pixmap != None)
    {
	if (tmp->icon_depth == scr->d_depth)
	{
	    XCopyArea(dpy, tmp->icon_pixmap, tmp->icon_pixmap_w,
		      (scr->mwm_highlight == tmp)
		      ? scr->components[MWM_ICON].active_GC
		      : scr->components[MWM_ICON].normal_GC,
		      0, 0,
		      tmp->icon_p_width - 4, tmp->icon_p_height - 4, 2, 2);
	}
	else
	    XCopyPlane(dpy, tmp->icon_pixmap, tmp->icon_pixmap_w,
		       (scr->mwm_highlight == tmp)
		       ? scr->components[MWM_ICON].active_GC
		       : scr->components[MWM_ICON].normal_GC,
		       0, 0,
		       tmp->icon_p_width - 4, tmp->icon_p_height - 4, 2, 2, 1);
    }

    if (tmp->icon_w != None)
    {
	char *label;

	if (tmp == scr->mwm_highlight &&
	    (scr->icon_decoration & XmICON_ACTIVELABEL) &&
	    tmp->icon_active_label != NoName &&
	    tmp->icon_active_label != NULL &&
	    tmp->icon_active_label[0] != '\0')
	{
	    label = tmp->icon_active_label;
	}
	else
	{
	    label = tmp->icon_label;
	}

	if (label == NULL)
	    label = "";

	/* text position */
	x = (tmp->icon_w_width - tmp->icon_t_width) / 2;
	if (x < 3)
	    x = 3;

	XDrawString(dpy, tmp->icon_w,
		    (scr->mwm_highlight == tmp)
		    ? scr->components[MWM_ICON].active_GC
		    : scr->components[MWM_ICON].normal_GC,
		    x, tmp->icon_w_height -
		    scr->components[MWM_ICON].f_height +
		    scr->components[MWM_ICON].f_y - 3,
		    label, strlen(label));
	DEC_DrawShadows(tmp, tmp->icon_w,
			0, 0,
			tmp->icon_w_width,
			scr->components[MWM_ICON].f_height + 6,
			Relief, Shadow);
    }
}

/*
 * procedure to re-position the icon window and name
 */
void
ICON_UpdateWindow(ScreenInfo *scr, MwmWindow *tmp, Boolean force)
{
    char *label;

    if (scr->components[MWM_PAGER].f_height > 0)
    {
	XClearWindow(dpy, tmp->pager_view);
	PAGER_Clear(scr);
    }

    if (tmp->icon_w == None && tmp->icon_pixmap_w == None)
	return;

    if ((force || (tmp == scr->mwm_highlight &&
	 (scr->icon_decoration & XmICON_ACTIVELABEL))) &&
	tmp->icon_active_label != NoName &&
	tmp->icon_active_label != NULL &&
	tmp->icon_active_label[0] != '\0')
    {
	label = tmp->icon_active_label;
    }
    else
    {
	label = tmp->icon_label;
    }

    if (label == NULL)
	label = "";

    tmp->icon_t_width = XTextWidth(scr->components[MWM_ICON].font,
				   label, strlen(label));

    /* clear the icon window, and trigger a re-draw via an expose event */
    if (tmp->flags & ICONIFIED)
	XClearArea(dpy, tmp->icon_w, 0, 0, 0, 0, True);
}

/*
 * Find a home for an icon
 */
void
ICON_AutoPlace(ScreenInfo *scr, MwmWindow *t)
{
    int test_x = 0, test_y = 0, tw, th, tx, ty, i, temp_h, temp_w;
    int base_x, base_y;
    int width, height;
    MwmWindow *test_window;
    Bool loc_ok;
    int real_x = 10, real_y = 10;

    /* New! Put icon in same page as the center of the window */
    if ((t->flags & STICKY))
    {
	base_x = 0;
	base_y = 0;
    }
    else
    {
	base_x = ((t->frame_x + scr->virt_x + (t->frame_width >> 1)) /
		  scr->d_width) * scr->d_width - scr->virt_x;
	base_y = ((t->frame_y + scr->virt_y + (t->frame_height >> 1)) /
		  scr->d_height) * scr->d_height - scr->virt_y;
    }
    if (t->flags & ICON_MOVED)
    {
	/* just make sure the icon is on this screen */
	t->icon_x_loc = t->icon_x_loc % scr->d_width + base_x;
	t->icon_y_loc = t->icon_y_loc % scr->d_height + base_y;
	if (t->icon_x_loc < 0)
	    t->icon_x_loc += scr->d_width;
	if (t->icon_y_loc < 0)
	    t->icon_y_loc += scr->d_height;
    }
    else if (t->wmhints && t->wmhints->flags & IconPositionHint)
    {
	t->icon_x_loc = t->wmhints->icon_x;
	t->icon_y_loc = t->wmhints->icon_y;
    }
    else
    {
	width = t->icon_p_width;
	height = t->icon_w_height + t->icon_p_height;
	loc_ok = False;

	/* check all boxes in order */
	i = 0;
	while ((i < scr->num_icon_boxes) && (!loc_ok))
	{
	    /* In each IconBox, start at the upper left, travel right, then
	     * down */
	    test_y = scr->icon_boxes[i][1] + base_y;

	    temp_h = height;
	    temp_w = width;

	    /* OK second try at this.
	     * If the window is taller than the icon box, ignore the icon height
	     * when figuring where to put it. Same goes for the width */
	    /* This should permit reasonably graceful handling of big icons. */
	    if (width >= (scr->icon_boxes[i][2] - scr->icon_boxes[i][0]))
		temp_w = 0;
	    if (height >= (scr->icon_boxes[i][3] - scr->icon_boxes[i][1]))
		temp_h = 0;

	    while (((test_y + temp_h) < (scr->icon_boxes[i][3] + base_y)) &&
		   (!loc_ok))
	    {

		test_x = scr->icon_boxes[i][0] + base_x;
		while (((test_x + temp_w) < (scr->icon_boxes[i][2] + base_x)) &&
		       (!loc_ok))
		{
		    real_x = test_x;
		    real_y = test_y;

		    if (test_x + width > (scr->d_width - 2 + base_x))
			real_x = scr->d_width - width - 2 + base_x;
		    if (test_y + height > (scr->d_height - 2 + base_y))
			real_y = scr->d_height - height - 2 + base_y;
		    if (test_x < base_x)
			real_x = base_x;
		    if (test_y < base_y)
			real_y = base_y;
		    loc_ok = True;
		    test_window = scr->mwm_root.next;
		    while ((test_window != (MwmWindow *)0) && (loc_ok == True))
		    {
			if (test_window->Desk == t->Desk)
			{
			    if ((test_window->flags & ICONIFIED) &&
				(test_window->icon_w ||
				 test_window->icon_pixmap_w) &&
				(test_window != t))
			    {
				tw = test_window->icon_p_width;
				th = test_window->icon_p_height +
				    test_window->icon_w_height;
				tx = test_window->icon_x_loc;
				ty = test_window->icon_y_loc;

				if ((tx < (real_x + width + 3)) &&
				    ((tx + tw + 3) > real_x) &&
				    (ty < (real_y + height + 3)) &&
				    ((ty + th + 3) > real_y))
				{
				    loc_ok = False;
				}
			    }
			    if (scr->flags & StubbornIconPlacement)
			    {
				if (!(test_window->flags & ICONIFIED) &&
				    (test_window != t))
				{
				    tw = test_window->frame_width;
				    th = test_window->frame_height;
				    tx = test_window->frame_x;
				    ty = test_window->frame_y;

				    if ((tx < (real_x + width + 3)) &&
					((tx + tw + 3) > real_x) &&
					(ty < (real_y + height + 3)) &&
					((ty + th + 3) > real_y))
				    {
					loc_ok = False;
				    }
				}
			    }
			}
			test_window = test_window->next;
		    }
		    test_x += 3;
		}
		test_y += 3;
	    }
	    i++;
	}
	if (loc_ok == False)
	    return;
	t->icon_x_loc = real_x;
	t->icon_y_loc = real_y;
    }

    if (t->icon_pixmap_w)
	XMoveWindow(dpy, t->icon_pixmap_w, t->icon_x_loc, t->icon_y_loc);

    t->icon_w_width = t->icon_p_width;
    t->icon_xl_loc = t->icon_x_loc;

    if (t->icon_w != None)
	XMoveResizeWindow(dpy, t->icon_w, t->icon_xl_loc,
			  t->icon_y_loc + t->icon_p_height,
			  t->icon_w_width,
			  scr->components[MWM_ICON].f_height + 6);
}

/*
 * Iconifies the selected window
 */
void
ICON_Iconify(ScreenInfo *scr, MwmWindow *tmp_win, int def_x, int def_y)
{
    MwmWindow *t;
    XWindowAttributes winattrs;
    unsigned long eventMask;

    XGetWindowAttributes(dpy, tmp_win->w, &winattrs);
    eventMask = winattrs.your_event_mask;

    if ((tmp_win) && (tmp_win == scr->mwm_highlight) &&
	(Mwm.keyboard_focus_policy == XmEXPLICIT) && (tmp_win->next))
    {
	WIN_SetFocusInTree(tmp_win->next);
	WIN_SetFocus(scr, tmp_win->next->w, tmp_win->next);
	MISC_SetFocusSequence(scr);
    }

    /* iconify transients first */
    for (t = scr->mwm_root.next; t != NULL; t = t->next)
    {
	if ((t == tmp_win) ||
	    ((t->flags & TRANSIENT) && (t->transientfor == tmp_win->w)))
	{
	    /*
	     * Prevent the receipt of an UnmapNotify, since that would
	     * cause a transition to the Withdrawn state.
	     */
	    t->flags &= ~MAPPED;
	    XSelectInput(dpy, t->w, eventMask & ~StructureNotifyMask);
	    XUnmapWindow(dpy, t->w);
	    XSelectInput(dpy, t->w, eventMask);
	    XUnmapWindow(dpy, t->frame);
	    t->DeIconifyDesk = t->Desk;
	    if (t->icon_w)
		XUnmapWindow(dpy, t->icon_w);
	    if (t->icon_pixmap_w)
		XUnmapWindow(dpy, t->icon_pixmap_w);

	    PROP_SetState(t, IconicState);
	    DEC_DrawDecorations(scr, t, False, False, False, None);
	    if (t != tmp_win)
		t->flags |= ICONIFIED | ICON_UNMAPPED;
	    if (t != tmp_win)
		PAGER_UpdateView(scr, t);
	}
    }
    if (tmp_win->icon_w == None)
    {
	if (tmp_win->flags & ICON_MOVED)
	    ICON_CreateWindow(scr, tmp_win,
			      tmp_win->icon_x_loc, tmp_win->icon_y_loc);
	else
	    ICON_CreateWindow(scr, tmp_win, def_x, def_y);
    }
    if (Mwm.icon_auto_place)
	ICON_AutoPlace(scr, tmp_win);
    tmp_win->flags |= ICONIFIED;
    tmp_win->flags &= ~ICON_UNMAPPED;
    WIN_Lower(scr, tmp_win);
    if (tmp_win->Desk == scr->current_desk)
    {
	if (tmp_win->icon_w != None)
	    XMapWindow(dpy, tmp_win->icon_w);

	if (tmp_win->icon_pixmap_w != None)
	    XMapWindow(dpy, tmp_win->icon_pixmap_w);
	PAGER_UpdateView(scr, tmp_win);
    }
    if ((Mwm.keyboard_focus_policy == XmEXPLICIT) ||
	(Mwm.keyboard_focus_policy == XmPOINTER))
    {
	if ((tmp_win) && (tmp_win == scr->mwm_focus))
	{
	    if (scr->mwm_last_focus == scr->mwm_focus)
		scr->mwm_last_focus = NULL;
	    if ((Mwm.keyboard_focus_policy == XmEXPLICIT) && (tmp_win->next))
	    {
		WIN_SetFocusInTree(tmp_win->next);
		WIN_SetFocus(scr, tmp_win->next->w, tmp_win->next);
	    }
	    else
		WIN_SetFocus(scr, scr->no_focus_win, NULL);

	    MISC_SetFocusSequence(scr);
	}
    }
    if (Mwm.lower_on_iconify)
	WIN_Lower(scr, tmp_win);
}

/*
 * DeIconify a window
 */
void
ICON_DeIconify(ScreenInfo *scr, MwmWindow *tmp_win)
{
    MwmWindow *t, *tmp;
    int new_x, new_y, w2, h2;

    /* now de-iconify transients */
    for (t = scr->mwm_root.next; t != NULL; t = t->next)
    {
	if ((t == tmp_win) ||
	    ((t->flags & TRANSIENT) && (t->transientfor == tmp_win->w)))
	{
	    t->flags |= MAPPED;
	    if (scr->mwm_highlight == t)
		DEC_DrawDecorations(scr, t, False, True, True, None);
	    /* make sure that the window is on this screen */
	    if ((t->frame_x < 0) || (t->frame_y < 0) ||
		(t->frame_x >= scr->d_width) ||
		(t->frame_y >= scr->d_height))
	    {
		/* try to put at least half the window
		 * in the current screen, if the current desktop
		 * is the windows desktop */
		if (scr->flags & StubbornIcons)
		    t->Desk = t->DeIconifyDesk;
		else
		    t->Desk = scr->current_desk;

		if (t->Desk == scr->current_desk)
		{
		    new_x = t->frame_x;
		    new_y = t->frame_y;
		    w2 = (t->frame_width >> 1);
		    h2 = (t->frame_height >> 1);
		    if (!(scr->flags & StubbornIcons))
		    {
			if ((new_x < -w2) || (new_x > (scr->d_width - w2)))
			{
			    new_x = new_x % scr->d_width;
			    if (new_x < -w2)
				new_x += scr->d_width;
			}
			if ((new_y < -h2) || (new_y > (scr->d_height - h2)))
			{
			    new_y = new_y % scr->d_height;
			    if (new_y < -h2)
				new_y += scr->d_height;
			}
		    }
		    DEC_ConfigureDecorations(scr, t, new_x, new_y,
				       t->frame_width, t->frame_height, False);
		}
	    }
	    XMapWindow(dpy, t->w);
	    if (t->Desk == scr->current_desk)
	    {
		XMapWindow(dpy, t->frame);
		t->flags |= MAP_PENDING;
	    }
	    XMapWindow(dpy, t->parent);
	    PROP_SetState(t, NormalState);
	    t->flags &= ~ICONIFIED;
	    t->flags &= ~ICON_UNMAPPED;
	    /* Need to make sure the border is colored correctly,
	     * in case it was stuck or unstuck while iconified. */
	    tmp = scr->mwm_highlight;
	    scr->mwm_highlight = t;
	    DEC_DrawDecorations(scr, t, False, True, True, None);
	    scr->mwm_highlight = tmp;
	    XRaiseWindow(dpy, t->w);
	    if (t->icon_w)
		XUnmapWindow(dpy, t->icon_w);
	    if (t->icon_pixmap_w)
		XUnmapWindow(dpy, t->icon_pixmap_w);
	}
    }
    WIN_Raise(scr, tmp_win);

    if ((scr->flags & StubbornIcons) ||
	(Mwm.keyboard_focus_policy == XmEXPLICIT && Mwm.deiconify_key_focus))
	WIN_ChangeFocus(scr, tmp_win, 1);

    PAGER_UpdateView(scr, tmp_win);
}
