/* $Id: colormaps.c,v 1.1 2004/08/28 19:25:45 dannybackx Exp $ */
/****************************************************************************
 * This module is all new
 * by Rob Nation
 *
 * This code handles colormaps for mwm.
 *
 * Copyright 1994 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved. No guarantees or
 * warrantees of any sort whatsoever are given or implied or anything.
 ****************************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 * mwm - "LessTif Window Manager"
 ***********************************************************************/

#include <LTconfig.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include "mwm.h"

/*
 * install the colormaps for one mwm window
 */
void
COLOR_InstallWindowColorMap(ScreenInfo *scr, MwmWindow *tmp)
{
    int i;
    XWindowAttributes attributes;
    Window w;
    Bool ThisWinInstalled = False;


    /* If no window, then install root colormap */
    if (!tmp)
	tmp = &scr->mwm_root;

    scr->mwm_colormap = tmp;

    /*
     * Save the colormap to be loaded for when force loading of
     * root colormap(s) ends.
     */
    scr->mwm_pushed = tmp;

    /*
     * Don't load any new colormap if root colormap(s) has been
     * force loaded.
     */
    if (scr->root_pushes)
	return;

    if (tmp->number_cmap_windows > 0)
    {
	for (i = tmp->number_cmap_windows - 1; i >= 0; i--)
	{
	    w = tmp->cmap_windows[i];
	    if (w == tmp->w)
		ThisWinInstalled = True;
	    XGetWindowAttributes(dpy, w, &attributes);

	    if (scr->last_cmap != attributes.colormap)
	    {
		scr->last_cmap = attributes.colormap;
		XInstallColormap(dpy, attributes.colormap);
	    }
	}
    }

    if (!ThisWinInstalled)
    {
	if (scr->last_cmap != tmp->attr.colormap)
	{
	    scr->last_cmap = tmp->attr.colormap;
	    XInstallColormap(dpy, tmp->attr.colormap);
	}
    }
}

/*
 * The next two matching routines provide a mechanism to insure
 * that the root colormap(s) is installed during operations like
 * rubber banding or menu display that require colors from
 * that colormap.  Calls may be nested arbitrarily deeply,
 * as long as there is one UninstallRootColormap call per
 * COLOR_InstallRootColormap call.
 *
 * The final UninstallRootColormap will cause the colormap list
 * which would otherwise have be loaded to be loaded, unless
 * Enter or Leave Notify events are queued, indicating some
 * other colormap list would potentially be loaded anyway.
 */

/*
 * force (un)loads root colormap(s)
 */
void
COLOR_PushRootColorMap(ScreenInfo *scr)
{
    MwmWindow *tmp;

    if (scr->root_pushes == 0)
    {
	tmp = scr->mwm_pushed;
	COLOR_InstallWindowColorMap(scr, &scr->mwm_root);
	scr->mwm_pushed = tmp;
    }
    scr->root_pushes++;
}

/*
 * Unstacks one layer of root colormap pushing 
 * If we peel off the last layer, re-install the application colormap
 */
void
COLOR_PopRootColorMap(ScreenInfo *scr)
{
    if (scr->root_pushes)
	scr->root_pushes--;

    if (!scr->root_pushes)
	COLOR_InstallWindowColorMap(scr, scr->mwm_pushed);
}
