/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/RCUtils.c,v 1.5 2005/06/25 09:33:42 dannybackx Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright © 1996-2001, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/RCUtils.c,v 1.5 2005/06/25 09:33:42 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/DrawnBP.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
#include <Xm/RCUtilsP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScreenP.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/TearOffBP.h>
#include <Xm/ToggleBP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/TransltnsP.h>

#include <XmI/DebugUtil.h>

/*
 * interesting prototypes
 */
static void PreferredSizeNone(Widget rc, XtWidgetGeometry *rcg,
			      int *max_width, int *max_height);

static void PreferredSizeHT(Widget rc, XtWidgetGeometry *rcg,
			    int *max_width, int *max_height);

static void PreferredSizeHC(Widget rc, XtWidgetGeometry *rcg,
			    int *max_width, int *max_height);

static void DoLayoutHT(Widget rc, Widget instig,
		       XtWidgetGeometry *instig_request,
		       int max_width, int max_height);

static void DoLayoutHC(Widget rc, Widget instig,
		       XtWidgetGeometry *instig_request,
		       int max_width, int max_height);

static void PreferredSizeHT(Widget rc, XtWidgetGeometry *rcg,
			    int *max_width, int *max_height);

static void PreferredSizeHC(Widget rc, XtWidgetGeometry *rcg,
			    int *max_width, int *max_height);

static void DoLayoutVT(Widget rc, Widget instig,
		       XtWidgetGeometry *instig_request,
		       int max_width, int max_height);

static void DoLayoutVC(Widget rc, Widget instig,
		       XtWidgetGeometry *instig_request,
		       int max_width, int max_height);

/* For the prototype police */
extern XmRCKidGeometry _XmRCGetKidGeo(Widget w, Widget instig,
				      XtWidgetGeometry *instig_request,
				      int uniform_border, Dimension border,
				      int uniform_width_margins,
				      int uniform_height_margins, Widget help,
				      Widget toc, int geoType);

#if 0
static void
dump_boxes(WidgetList wl, XmRCKidGeometry boxes, int cnt)
{
    int i;

    for (i = 0; i < cnt; i++)
    {
	printf("Child: %-20s box: %-20s : ptrs %p %p\n",
	       XtName(wl[i]), XtName(boxes[i].kid),
	       wl[i], boxes[i].kid);

	printf("       x/y: %3d %3d  w/h: %3d %3d\n",
	       XtX(wl[i]), XtY(wl[i]), XtWidth(wl[i]), XtHeight(wl[i]));

	printf("   box x/y: %3d %3d  w/h: %3d %3d mt/mb: %3d %3d\n",
	       boxes[i].box.x, boxes[i].box.y,
	       boxes[i].box.width, boxes[i].box.height,
	       boxes[i].margin_top, boxes[i].margin_bottom);
    }
}
#endif

/*
 * This function computes the preferred size when the packing is PACK_NONE
 */
static void
PreferredSizeNone(Widget rc, XtWidgetGeometry *rcg,
		  int *max_width, int *max_height)
{
    int i;
    Dimension curw, curh;

    *max_width = 0;
    *max_height = 0;
    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry = &RC_Boxes(rc)[i];

	if (!XtIsManaged(RC_Boxes(rc)[i].kid))
	{
	    continue;
	}

	curw = kid_geometry->box.x + kid_geometry->box.width +
	    2 * kid_geometry->box.border_width;
	curh = kid_geometry->box.y + kid_geometry->box.height +
	    2 * kid_geometry->box.border_width;
	if (curw > *max_width)
	{
	    *max_width = curw;
	}
	if (curh > *max_height)
	{
	    *max_height = curh;
	}
    }

    rcg->request_mode = CWWidth | CWHeight | CWBorderWidth;
    rcg->width = *max_width;
    rcg->height = *max_height;
    rcg->border_width = XtBorderWidth(rc);
}

/*
 * This function treats the three following cases :
 *      XmWORK_AREA & XmHORIZONTAL & XmPACK_TIGHT
 *      XmMENU_BAR
 * ... which kind of explains what MENU_BAR means.
 *
 * Finally, it also covers the case where PreferredSizeHC is called but
 *   no number of columns is specified.
 *
 * According to the manual page, XmNnumColumns only matters in XmPACK_COLUMN.
 * I guess this means this function shouldn't think about XmNnumColumns.
 */
static void
PreferredSizeHT(Widget rc, XtWidgetGeometry *rcg,
		int *max_width, int *max_height)
{
    int i;
    Dimension current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
    Dimension curh = RC_MarginH(rc) + MGR_ShadowThickness(rc);

    DEBUGOUT(_LtDebug(__FILE__, rc, "PreferredSizeHT("));
    DEBUGOUT(_LtDebug0(__FILE__, rc, "%s",
			   RC_FromResize(rc) ? "" : "ParentResize "));
    DEBUGOUT(_LtDebug0(__FILE__, rc, ")\n"));
    DEBUGOUT(_LtDebug0(__FILE__, rc, "requesting %s\n",
		_LtDebugWidgetGeometry2String(rcg)));

    *max_height = 0, *max_width = 0;

    /* Calculate the max_height */
    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry = &RC_Boxes(rc)[i];
	Widget kid = kid_geometry->kid;

	if (!XtIsManaged(kid))
	{
	    continue;
	}

	DEBUGOUT(_LtDebug2(__FILE__, rc, kid,
			   "PreferredSizeHT: kid geo %d %d %dx%d\n",
			   kid_geometry->box.x,
			   kid_geometry->box.y,
			   kid_geometry->box.width,
			   kid_geometry->box.height));

	/*
	 * If we don't have enough space in this row, start another one.
	 */
	if ((RC_FromResize(rc) || (rcg->request_mode & CWWidth)) &&
	    current_x + kid_geometry->box.width > ((rcg->request_mode & CWWidth) ? rcg->width : XtWidth(rc)))
	{
	    if (RC_Type(rc) != XmMENU_OPTION) /* rowcolumn/test58 */
	    {
		current_y += *max_height + RC_Spacing(rc) + RC_MarginW(rc) + MGR_ShadowThickness(rc);
		current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
	    }
	}

	current_x += kid_geometry->box.width;

	if (kid_geometry->box.height + current_y > curh)
	{
	    curh = kid_geometry->box.height + current_y;
	}

	if (kid_geometry->box.height > *max_height)
	{
	    *max_height = kid_geometry->box.height;
	}

	/* Don't add spacing after last child */
	if (i != MGR_NumChildren(rc) - 1)
	{
	    current_x += RC_Spacing(rc);
	}

	if (current_x > *max_width)
	{
	    *max_width = current_x;
	}
    }

    rcg->request_mode = CWWidth | CWHeight | CWBorderWidth;
    if (*max_width == 0)
    {
	rcg->width = 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc));
    }
    else
    {
	rcg->width = *max_width + (RC_MarginW(rc) + MGR_ShadowThickness(rc));
    }
    rcg->height = curh + RC_MarginH(rc) + MGR_ShadowThickness(rc);
    rcg->border_width = XtBorderWidth(rc);
}

/*
 * this function actually lays things out in HT, according to
 * max_width, max_height
 */
static void
DoLayoutHT(Widget rc, Widget instig, XtWidgetGeometry *instig_request,
	   int max_width, int max_height)
{
    int i, j, lmh;
    int first_in_row = 0;
    Dimension current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
    XmRCKidGeometry help_kid_geometry = NULL;
    Dimension help_kid_width = 0;

    DEBUGOUT(_LtDebug2(__FILE__, rc, instig,
		"DoLayoutHT: req %s, max w %d h %d\n",
		_LtDebugWidgetGeometry2String(instig_request),
		max_width, max_height));

    if (RC_HelpPb(rc))
    {
	help_kid_width = XtWidth(RC_HelpPb(rc));
    }
    /* Start layout */
    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry = &RC_Boxes(rc)[i];
	Widget kid = kid_geometry->kid;

	if (!XtIsManaged(kid))
	{
	    continue;
	}

	/* don't lay the help button out until the end. */
	if (kid == RC_HelpPb(rc))
	{
	    help_kid_geometry = kid_geometry;

	    continue;
	}

	/* Height treatment */
	if (!(RC_Type(rc) == XmMENU_OPTION && XmIsCascadeButtonGadget(kid)))
	    kid_geometry->box.height = max_height;

	/*
	 * check for row adjustment
	 */
	if (RC_Type(rc) != XmMENU_OPTION) /* rowcolumn/test58 */
	{
	    if (current_x + kid_geometry->box.width > XtWidth(rc) &&
		current_x > RC_MarginW(rc) + MGR_ShadowThickness(rc))
	    {
		/* compute the local max_height */
		lmh = 0;
		for (j = first_in_row; j < i; j++)
		{
		    if (XtIsManaged(RC_Boxes(rc)[j].kid) &&
			RC_Boxes(rc)[j].box.height > lmh)
			lmh = RC_Boxes(rc)[j].box.height;
		}
		/* set the local max_height */
		for (j = first_in_row; j < i; j++)
		{
		    RC_Boxes(rc)[j].box.height = lmh;
		}

		/* printf("lmh %d spacing %d %d %d\n", lmh, RC_Spacing(rc),
		   RC_MarginW(rc), MGR_ShadowThickness(rc)); */
		current_y += lmh + RC_MarginW(rc) + MGR_ShadowThickness(rc); /* mainw/test14 */
		current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
		first_in_row = i;
	    }
	}

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;

	if (kid == instig && instig_request != NULL)
	{
	    *instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	}

	current_x += RC_Spacing(rc) + kid_geometry->box.width;
    }
    if (RC_HelpPb(rc) && XtIsManaged(RC_HelpPb(rc)) && help_kid_geometry)
    {
    	/* treat the help button exactly the same as all of the other
    	   kids. (ie clone the contents of the above loop)
    	 */
	XmRCKidGeometry kid_geometry = help_kid_geometry;
	Widget kid = kid_geometry->kid;

	/* Height treatment */
	kid_geometry->box.height = max_height;

	/*
	 * check for row adjustment
	 */
	if (current_x + kid_geometry->box.width > XtWidth(rc) &&
	    current_x > RC_MarginW(rc) + MGR_ShadowThickness(rc))
	{
	    /* compute the local max_height */
	    lmh = 0;
	    for (j = first_in_row; j < i; j++)
	    {
		if (XtIsManaged(RC_Boxes(rc)[j].kid) &&
		    RC_Boxes(rc)[j].box.height > lmh)
		    lmh = RC_Boxes(rc)[j].box.height;
	    }
	    /* set the local max_height */
	    for (j = first_in_row; j < i; j++)
	    {
		RC_Boxes(rc)[j].box.height = lmh;
	    }

	    current_y += lmh + RC_MarginW(rc) + MGR_ShadowThickness(rc);
	    current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
	    first_in_row = i;
	}

	kid_geometry->box.x = (XtWidth(rc)
				    - RC_MarginW(rc)
				    - MGR_ShadowThickness(rc)
				    - help_kid_geometry->box.width);
	kid_geometry->box.y = current_y;

	if (kid == instig && instig_request != NULL)
	{
	    *instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	}

	current_x += RC_Spacing(rc) + kid_geometry->box.width;
    }

    /* compute the local max_height */
    lmh = 0;
    for (j = first_in_row; j < i; j++)
    {
	if (XtIsManaged(RC_Boxes(rc)[j].kid) &&
	    RC_Boxes(rc)[j].box.height > lmh)
	{
	    lmh = RC_Boxes(rc)[j].box.height;
	}
    }

    /* set the local max_height */
    for (j = first_in_row; j < i; j++)
    {
	if (!(RC_Type(rc) == XmMENU_OPTION &&
	      XmIsCascadeButtonGadget(RC_Boxes(rc)[j].kid)))
	    RC_Boxes(rc)[j].box.height = lmh;
    }

    /* now we fill in the last row, so it takes up the remaining height, 
     * if XmNadjustLast is True. */
    if (RC_AdjLast(rc))
    {
	for (i = first_in_row; i < MGR_NumChildren(rc); i++)
	{
	    XmRCKidGeometry kid_geometry = &RC_Boxes(rc)[i];
	    Widget kid = kid_geometry->kid;

	    if (!XtIsManaged(kid))
	    {
		continue;
	    }

	    if (!(RC_Type(rc) == XmMENU_OPTION &&
		  XmIsCascadeButtonGadget(kid)))
	    {
		if ((XtHeight(rc) <= current_y + RC_MarginH(rc)) ||
		    (max_height + RC_MarginH(rc) > XtHeight(rc)))
		{
		    kid_geometry->box.height = max_height;
		}
		else
		{
		    kid_geometry->box.height = XtHeight(rc) -
			(current_y + RC_MarginH(rc) + MGR_ShadowThickness(rc));
		}
	    }

	    if (kid == instig && instig_request != NULL)
	    {
		*instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	    }
	}
    }

#if 0
    /* rws 1 May 1998
       The help widget is treated just like all of the other kids
       immediately following the loop treating all of the other kids.
       Otherwise if we needed to wrap just to fit the help widget all of
       the buttons would be too high and the help widget will be tucked
       behind the last widget.
     */
    /* In some cases we don't have a help widget here ...
     * like when it's not managed
     */
    if (RC_HelpPb(rc) && XtIsManaged(RC_HelpPb(rc)) && help_kid_geometry)
    {
	help_kid_geometry->box.x = (XtWidth(rc)
				    - MGR_ShadowThickness(rc)
				    - help_kid_geometry->box.width);
	help_kid_geometry->box.y = current_y;
    }
#endif
}

/* 
 * Horizontal Column
 *  XmNnumColumns does determine geometry here (indicating number of rows !!).
 *      All children (of certain class) get to have the same width.
 */
static void
PreferredSizeHC(Widget rc, XtWidgetGeometry *rcg,
		int *max_width, int *max_height)
{
    int i, nrows;
    int number_per_row;

    DEBUGOUT(_LtDebug(__FILE__, rc, "PreferredSizeHC()\n"));

    if (RC_NCol(rc) == 0)
    {
	/* what the hell.  punt and go to .. */
	PreferredSizeHT(rc, rcg, max_width, max_height);
	return;
    }

    number_per_row = ((_XmGeoCount_kids((CompositeWidget)rc) - 1) /
		      RC_NCol(rc)) + 1;
    *max_width = 0, *max_height = 0;

    nrows = 0;

    /* first, figure out the width of each column and the height of each row */
    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry;
	Widget kid;

	kid_geometry = &RC_Boxes(rc)[i];
	kid = kid_geometry->kid;

	if (!XtIsManaged(kid))
	{
	    continue;
	}

	nrows++;

	if (*max_width < kid_geometry->box.width)
	{
	    *max_width = kid_geometry->box.width;
	}

	if (*max_height < kid_geometry->box.height)
	{
	    *max_height = kid_geometry->box.height;
	}
    }

    if (number_per_row > 1 || nrows > RC_NCol(rc))
    {
    int num_man = nrows;

	nrows = RC_NCol(rc);
	while (nrows * number_per_row >= num_man)
	{
	    nrows--;
	}
	nrows++;
    }

    /* Determine the RC's size */
    rcg->request_mode = CWWidth | CWHeight | CWBorderWidth;
    rcg->width = 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc))
	+ *max_width * number_per_row
	+ RC_Spacing(rc) * (number_per_row - 1);
    rcg->height = 2 * (RC_MarginH(rc) + MGR_ShadowThickness(rc)) +
	RC_Spacing(rc) * (nrows - 1) +
	*max_height * nrows;
    rcg->border_width = XtBorderWidth(rc);
}

/*
 * actually do the HC layout
 */
static void
DoLayoutHC(Widget rc, Widget instig, XtWidgetGeometry *instig_request,
	   int max_width, int max_height)
{
    int i, cnt;
    int number_per_row;
    Dimension current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
    Dimension box_width;

    if (RC_NCol(rc) == 0)
    {
	/* what the hell.  punt and go to .. */
	DoLayoutHT(rc, instig, instig_request, max_width, max_height);
	return;
    }

    number_per_row = ((_XmGeoCount_kids((CompositeWidget)rc) - 1) /
		      RC_NCol(rc)) + 1;
    if (number_per_row < 1)
    {
    	number_per_row = 1;
    }
    if (XtIsRealized(rc) && max_width < (XtWidth(rc) - 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc)) - (RC_Spacing(rc) * (number_per_row - 1))) / number_per_row)
    {
	box_width = (XtWidth(rc) - 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc)) - (RC_Spacing(rc) * (number_per_row - 1))) / number_per_row;
    }
    else
    {
    	box_width = max_width;
    }

    /* now we lay out the children */
    for (i = 0, cnt = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry;
	Widget kid;

	kid_geometry = &RC_Boxes(rc)[i];
	kid = kid_geometry->kid;

	if (!XtIsManaged(kid))
	{
	    continue;
	}

	if (cnt != 0 && (cnt % number_per_row) == 0)
	{
	    current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
	    current_y += max_height + RC_Spacing(rc);
	}

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;

	kid_geometry->box.width = max_width;
	kid_geometry->box.height = max_height;

	if (kid == instig && instig_request != NULL)
	{
	    *instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	}

	current_x += RC_Spacing(rc) + box_width;
	cnt++;
    }

    /* now we fill in the last row, so it takes up the remaining height, 
     * if XmNadjustLast is True. */
    if (RC_AdjLast(rc))
    {
	for (i = MGR_NumChildren(rc) - number_per_row;
	     i < MGR_NumChildren(rc);
	     i++)
	{
	    XmRCKidGeometry kid_geometry;
	    Widget kid;

	    kid_geometry = &RC_Boxes(rc)[i];
	    kid = kid_geometry->kid;

	    if (!XtIsManaged(kid))
	    {
		continue;
	    }

	    kid_geometry->box.height = (XtHeight(rc)
					- (current_y
					   + RC_MarginH(rc)
					   + MGR_ShadowThickness(rc)));

	    kid_geometry->box.width = box_width;
	    if (kid == instig && instig_request != NULL)
	    {
		*instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	    }

	}
    }
}

/*
 * Called for cases :
 *      XmVERTICAL & XmPACK_TIGHT (i.e., pulldowns)
 *      XmMENU_PULLDOWN
 *
 * According to the manual pages, XmNnumColumns should not make a difference
 * in this case.
 *
 * An extra complication is added by TearOffControl. This button is always
 * child 0, and when it's there, RC_TearOffControl is non-NULL.
 */
static void
PreferredSizeVT(Widget rc, XtWidgetGeometry *rcg,
		int *max_width, int *max_height)
{
    int i, cols;
    Dimension current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
    Dimension curw = RC_MarginW(rc) + MGR_ShadowThickness(rc);

    *max_width = 0, *max_height = 0;
    cols = 1;

    /* Calculate the max_width */
    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry = &RC_Boxes(rc)[i];
	Widget kid = kid_geometry->kid;

	if (!XtIsManaged(kid))
	{
	    continue;
	}

	DEBUGOUT(_LtDebug2(__FILE__, rc, kid_geometry->kid,
			   "%p:PreferredSizeVT: kid %s\n", rc,
			   _LtDebugWidgetGeometry2String(&kid_geometry->box)));

	if (RC_FromResize(rc) &&
	    current_y + kid_geometry->box.height > XtHeight(rc))
	{
	    cols++;
	    current_x += *max_width + RC_Spacing(rc);
	    current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
	}

	current_y += kid_geometry->box.height;

	if (kid_geometry->box.width + current_x > curw)
	{
	    curw = kid_geometry->box.width + current_x;
	}

	if (kid_geometry->box.width > *max_width)
	{
	    *max_width = kid_geometry->box.width;
	}

	if (i != MGR_NumChildren(rc) - 1)
	{
	    current_y += RC_Spacing(rc);
	}

	if (current_y > *max_height)
	{
	    *max_height = current_y;
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, rc,
		      "PreferredSizeVT: MaxWidth %d\n", *max_width));

    rcg->request_mode = CWWidth | CWHeight | CWBorderWidth;
    rcg->width = curw + RC_MarginW(rc) + MGR_ShadowThickness(rc);

    if ((!RC_ResizeWidth(rc) || RC_FromResize(rc)) && rcg->width > XtWidth(rc) && XtWidth(rc) > 0)
    {
    	/* rws 25 Oct 1998 (rowcolumn/test42, xephem, xmgrace)
    	   If we are being resized then we can't be any wider than the RC.
    	   In this case split the available width up evenly between all of
    	   the columns.
    	   This, and the next if, should maybe be done in the Layout portion,
    	   since this is a constraint affecting the "preferred" size.  Then
    	   again, it is the preferred size given the constraints????
    	 */
    	rcg->width = XtWidth(rc);
    	*max_width = (XtWidth(rc) 
    	              - 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc)) 
    	              - (RC_Spacing(rc) * (cols - 1))) 
    	             / cols;
    }

    if (*max_height == 0)
    {
	rcg->height = 2 * (RC_MarginH(rc) + MGR_ShadowThickness(rc));
    }
    else
    {
	rcg->height = *max_height + (RC_MarginH(rc) + MGR_ShadowThickness(rc));
    }
    rcg->border_width = XtBorderWidth(rc);
}

/*
 * actually do the vertical tight layout
 */
static void
DoLayoutVT(Widget rc, Widget instig, XtWidgetGeometry *instig_request,
	   int max_width, int max_height)
{
    int i, j, lmw;
    int first_in_column = 0;
    Dimension current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);

    /* Start layout */
    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry = &RC_Boxes(rc)[i];
	Widget kid = kid_geometry->kid;

	if (!XtIsManaged(kid))
	{
	    continue;
	}

	DEBUGOUT(_LtDebug(__FILE__, rc,
			  "Child %p:%s width changed to %d\n",
			  kid, XtName(kid), max_width));

	/*
	 * See if we need to move to the next column.
	 *
	 * This is what the first condition does; the second insures that
	 * we don't do that if this is the only child in the column.
	 */
	if (current_y + kid_geometry->box.height > XtHeight(rc) &&
	    current_y > RC_MarginH(rc) + MGR_ShadowThickness(rc))
	{
	    /* compute the local max_width */
	    lmw = 0;
	    for (j = first_in_column; j < i; j++)
	    {
		if (XtIsManaged(RC_Boxes(rc)[j].kid) &&
		    RC_Boxes(rc)[j].box.width > lmw)
		    lmw = RC_Boxes(rc)[j].box.width;
	    }

	    /* set the local max_width */
	    for (j = first_in_column; j < i; j++)
	    {
		RC_Boxes(rc)[j].box.width = lmw;
	    }

	    /* Move the child to the next column. */
	    current_x += lmw;
	    current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
	    first_in_column = i;
	}

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;

	if (kid == instig && instig_request != NULL)
	{
	    *instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	}

	current_y += RC_Spacing(rc) + kid_geometry->box.height;
    }

    /* compute the local max_width */
    lmw = 0;
    for (j = first_in_column; j < i; j++)
    {
	if (XtIsManaged(RC_Boxes(rc)[j].kid) &&
	    RC_Boxes(rc)[j].box.width > lmw)
	{
	    lmw = RC_Boxes(rc)[j].box.width;
	}
    }

    /* set the local max_width */
    for (j = first_in_column; j < i; j++)
    {
	RC_Boxes(rc)[j].box.width = lmw;
    }

    /* now we fill in the last column, so it takes up the remaining width, 
     * if XmNadjustLast is True. */
    if (RC_AdjLast(rc))
    {
	for (i = first_in_column; i < MGR_NumChildren(rc); i++)
	{
	    XmRCKidGeometry kid_geometry = &RC_Boxes(rc)[i];
	    Widget kid = kid_geometry->kid;

	    if (!XtIsManaged(kid))
	    {
		continue;
	    }

	    if ((XtWidth(rc) <= current_x + RC_MarginW(rc)) ||
		(max_width + RC_MarginW(rc) > XtWidth(rc)))
	    {
		kid_geometry->box.width = max_width;
		DEBUGOUT(_LtDebug(__FILE__, rc,
			  "Child %p:%s AdjLast too wide width changed to %d\n",
				  kid, XtName(kid), max_width));
	    }
	    else
	    {
		kid_geometry->box.width = (XtWidth(rc) -
		       (current_x + RC_MarginW(rc) + MGR_ShadowThickness(rc)));
		DEBUGOUT(_LtDebug(__FILE__, rc,
		      "Child %p:%s AdjLast not too wide width changed to %d\n",
				  kid, XtName(kid), kid_geometry->box.width));
#if 1
		/*
		 * This is a hack for a problem reported by Dave Williss.
		 * A PushButton widget that sets its acceleratorText via
		 * SetValues (i.e. not at initialisation time) doesn't
		 * change its width.
		 *
		 * Not sure why this appears to be needed, also not sure
		 * whether this is the right place for this.
		 * Danny 23/9/2004.
		 *
		 * Addition by Dave 21/10/2004 for fixing label alignment on toggles
		 * in a menu :
		 * Adding margin_top (which is really left margin) to the width seems
		 * counter-intuitive to me, but without it, menu items which are pull-right
		 * don't line up correctly.  If this ends up breaking anything else, it could
		 * probably be changed to only do it if the RowColumn is a menu pane.
		 */
			_XmConfigureObject(kid,
					kid_geometry->box.x + kid_geometry->margin_top,
					kid_geometry->box.y,
					kid_geometry->box.width + kid_geometry->margin_top,
					kid_geometry->box.height,
					kid_geometry->box.border_width);
#endif
	    }

	    if (kid == instig && instig_request != NULL)
	    {
		*instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	    }
	}
    }
}

/*
 * Vertical Column
 *      XmNnumColumns does determine geometry here (and it does refer
 *      to number of columns).
 *      All children (of certain class) get to have the same height.
 *      It looks like separators and tearOffs don't get treated this way
 *      in Motif ...
 *
 * Called when :
 *      XmVERTICAL & XmPACK_COLUMN
 *      XmMENU_POPUP
 */
static void
PreferredSizeVC(Widget rc, XtWidgetGeometry *rcg,
		int *max_width, int *max_height)
{
    int i, ncols;
    int number_per_column;

    DEBUGOUT(_LtDebug(__FILE__, rc,
		      "PreferredSizeVC(ncols %d)\n", RC_NCol(rc)));

    if (RC_NCol(rc) == 0)
    {
	/* what the hell.  punt and go to .. */
	PreferredSizeVT(rc, rcg, max_width, max_height);
	return;
    }

    number_per_column = ((_XmGeoCount_kids((CompositeWidget)rc) - 1) /
			 RC_NCol(rc)) + 1;
    *max_width = 0, *max_height = 0;

    ncols = 0;
    /* first, figure out the height of each row and the width of each column */
    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry = &RC_Boxes(rc)[i];
	Widget kid = kid_geometry->kid;

	kid_geometry = &RC_Boxes(rc)[i];
	kid = kid_geometry->kid;

	if (!XtIsManaged(kid) || XmIsTearOffButton(kid))
	{
	    continue;
	}

	ncols++;
	if (*max_height < kid_geometry->box.height)
	{
	    *max_height = kid_geometry->box.height;
	}

	if (*max_width < kid_geometry->box.width)
	{
	    *max_width = kid_geometry->box.width;
	}
    }

    if (number_per_column > 1 || ncols > RC_NCol(rc))
    {
    int num_man = ncols;

	ncols = RC_NCol(rc);
	while (ncols * number_per_column >= num_man)
	{
	    ncols--;
	}
	ncols++;
    }

    /* Figure out how big we need to be */
    rcg->request_mode = CWWidth | CWHeight | CWBorderWidth;
    rcg->width = 2 * (RC_MarginW(rc) + MGR_ShadowThickness(rc)) +
	RC_Spacing(rc) * (ncols - 1) +
	*max_width * ncols;
    rcg->height = 2 * (RC_MarginH(rc) + MGR_ShadowThickness(rc))
	+ *max_height * number_per_column
	+ RC_Spacing(rc) * (number_per_column - 1);
    rcg->border_width = XtBorderWidth(rc);
}

/*
 * actually do the vertical column layout
 */
static void
DoLayoutVC(Widget rc, Widget instig, XtWidgetGeometry *instig_request,
	   int max_width, int max_height)
{
    int i;
    int number_per_column;
    Dimension current_x = RC_MarginW(rc) + MGR_ShadowThickness(rc);
    Dimension current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);

    DEBUGOUT(_LtDebug(__FILE__, rc,
		      "LayoutVC(ncols %d)\n", RC_NCol(rc)));

    if (RC_NCol(rc) == 0)
    {
	/* what the hell.  punt and go to .. */
	DoLayoutVT(rc, instig, instig_request, max_width, max_height);
	return;
    }

    number_per_column = ((_XmGeoCount_kids((CompositeWidget)rc) - 1) /
			 RC_NCol(rc)) + 1;

    {
    int cnt = 0;
    /* now we lay out the children */
    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kid_geometry;
	Widget kid;

	kid_geometry = &RC_Boxes(rc)[i];
	kid = kid_geometry->kid;

	if (!XtIsManaged(kid))
	{
	    continue;
	}

	if (cnt != 0 && (cnt % number_per_column) == 0)
	{
	    current_x += max_width + RC_Spacing(rc);
	    current_y = RC_MarginH(rc) + MGR_ShadowThickness(rc);
	}

	kid_geometry->box.x = current_x;
	kid_geometry->box.y = current_y;

	kid_geometry->box.width = max_width;
	kid_geometry->box.height = max_height;

	if (kid == instig && instig_request != NULL)
	{
	    *instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	    DEBUGOUT(_LtDebug(__FILE__, rc,
			      "LayoutVC: instig set to %s\n",
			      _LtDebugWidgetGeometry2String(instig_request)));
	}

	current_y += RC_Spacing(rc) + max_height;
	cnt++;
    }
    }

    /*
     * Now we fill in the last column 
     * so it takes up the remaining width if XmNadjustLast is True.
     *
     * (The elements in the last column were also processed above, hence no
     *      x, y, height corrections here.)
     */
    if (RC_AdjLast(rc))
    {
	for (i = MGR_NumChildren(rc) - number_per_column;
	     i < MGR_NumChildren(rc);
	     i++)
	{
	    XmRCKidGeometry kid_geometry;
	    Widget kid;

	    kid_geometry = &RC_Boxes(rc)[i];
	    kid = kid_geometry->kid;

	    if (!XtIsManaged(kid))
	    {
		continue;
	    }

	    if (current_x + RC_MarginW(rc) + MGR_ShadowThickness(rc) <
		XtWidth(rc))
	    {
		kid_geometry->box.width = (XtWidth(rc)
					   - current_x
					   - RC_MarginW(rc)
					   - MGR_ShadowThickness(rc));
	    }

	    if (kid == instig && instig_request != NULL)
	    {
		*instig_request = kid_geometry->box;
		/* T. Straumann: account for border width */
		instig_request->width -= 2*kid_geometry->box.border_width;
		instig_request->height -= 2*kid_geometry->box.border_width;
	        DEBUGOUT(_LtDebug(__FILE__, rc,
			          "LayoutVC: instig set to %s after AdjLast\n",
			          _LtDebugWidgetGeometry2String(instig_request)));
	    }
	}
    }
}

static void
find_largest_option_selection(Widget w, Dimension *width, Dimension *height)
{
    int i;
    Widget child;
    XtWidgetGeometry preferred;
    
    if (w == NULL)
	return;
    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	child = MGR_Children(w)[i];
	if (XtIsManaged(child))
	{
	    if (XmIsCascadeButton(child))
	    {
		find_largest_option_selection(CB_Submenu(child),
					      width, height);
	    }
	    else if (XmIsCascadeButtonGadget(child))
	    {
		find_largest_option_selection(CBG_Submenu(child)
					      , width, height);
	    }
	    else
	    {
		XtQueryGeometry(child, NULL, &preferred);
		if (*width < preferred.width)
		{
		    *width = preferred.width;
		}
		if (*height < preferred.height)
		{
		    *height = preferred.height;
		}
	    }
	}
    }
}

/*
 * This function was a major inefficiency provider of the RC widget -
 * it gets called by setvalues, insert_child, and change_managed;
 * and every time it does a _XmGeoLoadValues on all its children,
 * forgetting all the info from the previous calls ...
 *
 * In the process of fixing this... Danny 23/8/1996
 *
 * Moved all insert_child and delete_child interaction with this sucker either
 *      to delete_child or to a separate function. 25/8/96.
 *
 * Much better now :-)
 *
 * Add in the borderWidth, take it out just before configuring kids rws
 *
 * MLM 4/30/97-5/1/97 rework.
 * WE RELY ON MGR_Children() and RC_Boxes() having a one to one correspondence.
 * (That's maintained by the RCInsertBox/RCDeleteBox functions).  DO NOT CHANGE
 * THIS.
 */
XmRCKidGeometry
_XmRCGetKidGeo(Widget w, Widget instig,
	       XtWidgetGeometry *instig_request,
	       int uniform_border, Dimension border,
	       int uniform_width_margins, int uniform_height_margins,
	       Widget help, Widget toc, int geoType)
{
    int i;
    XmRCKidGeometry kid_geometry = NULL;
    Dimension mmt = 0, mmb = 0;
    XmRCKidGeometry boxes = NULL;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRCGetKidGeo: instig %s:%s, igeo: %s\n",
		      instig
			? XtClass(instig)->core_class.class_name
			: "(null)",
		      instig ? XtName(instig) : "(null)",
		      instig_request
			? _LtDebugWidgetGeometry2String(instig_request)
			: "(no instig request)"));

    boxes = (XmRCKidGeometry)XtCalloc(MGR_NumChildren(w) + 1,
				      sizeof(XmRCKidGeometryRec));

    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	kid_geometry = &boxes[i];
	kid_geometry->kid = MGR_Children(w)[i];

	if (i != RCC_PositionIndex(kid_geometry->kid))
	{
	    _XmError(w, "RCGetKidGeo: PositionIndex doesn't match "
		     "actual position\n");
	}

	if (!XtIsManaged(kid_geometry->kid)) {
		RCC_WasManaged(kid_geometry->kid) = False;
		continue;
	}

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmRCGetKidGeo: doing change for %d %p:%s\n",
			  i, kid_geometry->kid, XtName(kid_geometry->kid)));

	/*
	 * Don't use the instig if we're not in a menu.  This is more
	 * correct (according to the Motif comments), and it helps
	 * the new form algorithm work.
	 */

	/*
	 * But it breaks the layout of xmgr File->Describe menu - the text
	 * widget becomes mutch to small.
	 */
#if 0
	if (RC_Type(w) == XmWORK_AREA)
	{
	    _XmGeoLoadValues(kid_geometry->kid, geoType,
			     NULL, NULL, &kid_geometry->box);
	}
	else
#endif
	{
	    _XmGeoLoadValues(kid_geometry->kid, geoType,
			     instig, instig_request, &kid_geometry->box);
	}

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmRCGetKidGeo: did change for %p:%s : geo %s\n",
			  kid_geometry->kid, XtName(kid_geometry->kid),
			  _LtDebugWidgetGeometry2String(&kid_geometry->box)));

	RCC_WasManaged(kid_geometry->kid) = True;

	if (uniform_border)
	{
	    kid_geometry->box.border_width = border;
	}

	kid_geometry->box.width += (2 * XtBorderWidth(kid_geometry->kid));
	kid_geometry->box.height += (2 * XtBorderWidth(kid_geometry->kid));

	if (mmt < RCC_MarginTop(kid_geometry->kid))
	    mmt = RCC_MarginTop(kid_geometry->kid);
	if (mmb < RCC_MarginBottom(kid_geometry->kid))
	    mmb = RCC_MarginBottom(kid_geometry->kid);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmRCGetKidGeo: kid margins: %d %d\n",
			  RCC_MarginTop(kid_geometry->kid),
			  RCC_MarginBottom(kid_geometry->kid)));

	if (RC_DoMarginAdjust(w) &&
	    (XmIsLabel(kid_geometry->kid) ||
	     XmIsLabelGadget(kid_geometry->kid)))
	{
	    if (RC_Orientation(w) == XmHORIZONTAL)
	    {
		if (XmIsLabel(kid_geometry->kid))
		{
		    kid_geometry->box.height -=
			Lab_MarginTop(kid_geometry->kid) +
			Lab_MarginBottom(kid_geometry->kid);
		}
		else
		{
		    kid_geometry->box.height -=
			LabG_MarginTop(kid_geometry->kid) +
			LabG_MarginBottom(kid_geometry->kid);
		}
	    }
	    else
	    {
		if (XmIsLabel(kid_geometry->kid))
		{
		    kid_geometry->box.width -=
			Lab_MarginLeft(kid_geometry->kid) +
			Lab_MarginRight(kid_geometry->kid);
		}
		else
		{
		    kid_geometry->box.width -=
			LabG_MarginLeft(kid_geometry->kid) +
			LabG_MarginRight(kid_geometry->kid);
		}
	    }
	}
    }

    if (RC_DoMarginAdjust(w))
    {
	for (i = 0; i < MGR_NumChildren(w); i++)
	{
	    kid_geometry = &boxes[i];

	    if (!XtIsManaged(kid_geometry->kid) ||
		((!XmIsLabel(kid_geometry->kid) ||
		  XtClass(kid_geometry->kid) == xmLabelWidgetClass) &&
		 (!XmIsLabelGadget(kid_geometry->kid) ||
		  XtClass(kid_geometry->kid) == xmLabelGadgetClass)))
	    {
		continue;
	    }

	    if (mmt > kid_geometry->margin_top)
	    {
		kid_geometry->margin_top = mmt;
	    }
	    if (mmb > kid_geometry->margin_bottom)
	    {
		kid_geometry->margin_bottom = mmb;
	    }

	    if (RC_Orientation(w) == XmHORIZONTAL)
	    {
		kid_geometry->box.height += mmt + mmb;
	    }
	    else
	    {
		kid_geometry->box.width += mmt + mmb;
	    }
	}
    }
    if (RC_Type(w) == XmMENU_OPTION)
    {
	Widget obg = XmOptionButtonGadget(w);
	Dimension width = 0, height = 0;

	for (i = 0; i < MGR_NumChildren(w); i++)
	{
	    kid_geometry = &boxes[i];
	    if (obg == kid_geometry->kid)
	    {
		break;
	    }
	}
	if (obg == kid_geometry->kid)
	{
	    Widget submenu = CBG_Submenu(obg);

	    if (CBG_Submenu(obg))
	    {
		find_largest_option_selection(submenu, &width, &height);
		width += (LabG_Highlight(obg) << 1)
		    + (LabG_StringDirection(obg) == XmSTRING_DIRECTION_L_TO_R
		       ? LabG_MarginRight(obg) : LabG_MarginLeft(obg))
		    + LabG_Shadow(obg)
		    - Xm3D_ENHANCE_PIXEL;
		if (submenu)
		{
		    width += MGR_ShadowThickness(submenu) << 1;
		}
		height += (LabG_Highlight(obg) << 1);
		/* rowcolumn/test61 */
		if (RC_Orientation(w) == XmHORIZONTAL)
		{
		    height += RCC_MarginTop(obg) + RCC_MarginBottom(obg);
		}
	    }
	    else
	    {
	    	width = XtWidth(obg);
	    	height = XtHeight(obg);
	    }
	    kid_geometry->box.width = width;
	    kid_geometry->box.height = height;
	}
    }

    return boxes;
}

void
_XmRCSetKidGeo(XmRCKidGeometry kg, Widget instigator)
{
    /* Now we actually lay out our children.  */
    while (kg->kid != NULL)
    {
	DEBUGOUT(_LtDebug2(__FILE__, XtParent(kg->kid), kg->kid,
		     "_XmRCSetKidGeo: child set to (%s): was %d %d, parent %dx%d\n",
			  _LtDebugWidgetGeometry2String(&kg->box),
			  XtWidth(kg->kid), XtHeight(kg->kid),
			  XtWidth(XtParent(kg->kid)), XtHeight(XtParent(kg->kid))));

	if (!XtIsManaged(kg->kid))
	{
	    kg++;
	    continue;
	}

	if (kg->kid == instigator)
	{
	    if (kg->box.request_mode & CWX)
	    {
		/* T. Straumann: origin of instigator should coincide with the box origin */
		XtX(instigator) = kg->box.x /*  - kg->box.border_width */;
	    }
	    if (kg->box.request_mode & CWY)
	    {
		/* T. Straumann: origin of instigator should coincide with the box origin */
		XtY(instigator) = kg->box.y /*  - kg->box.border_width */;
	    }
	    if (kg->box.request_mode & CWWidth)
	    {
		XtWidth(instigator) = kg->box.width - 2 * kg->box.border_width;
	    }
	    if (kg->box.request_mode & CWHeight)
	    {
		XtHeight(instigator) = kg->box.height - 2 * kg->box.border_width;
	    }
	    if (kg->box.request_mode & CWBorderWidth)
	    {
		instigator->core.border_width = kg->box.border_width;
	    }
	}
	else if (kg->box.x != XtX(kg->kid) ||
		 kg->box.y != XtY(kg->kid) ||
		 kg->box.width != XtWidth(kg->kid) ||
		 kg->box.height != XtHeight(kg->kid) ||
		 kg->box.border_width != XtBorderWidth(kg->kid))
	{
		/* T. Straumann: account for border width */
	    _XmConfigureObject(kg->kid,
			       kg->box.x,
			       kg->box.y,
			       kg->box.width-2*kg->box.border_width, kg->box.height-2*kg->box.border_width,
			       kg->box.border_width);
	}

	kg++;
    }
}


extern void
_XmRCSetMargins(Widget rc)
{
    int i;

    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	XmRCKidGeometry kg = &RC_Boxes(rc)[i];

	if (RC_DoMarginAdjust(rc) && XtIsManaged(kg->kid) &&
	    ((XmIsLabel(kg->kid) && XtClass(kg->kid) != xmLabelWidgetClass) ||
	     (XmIsLabelGadget(kg->kid) &&
	      XtClass(kg->kid) != xmLabelGadgetClass)))
	{
	    DEBUGOUT(_LtDebug(__FILE__, kg->kid, "Setting margins to %d %d\n",
			      kg->margin_top, kg->margin_bottom));
	    if (RC_Orientation(rc) == XmHORIZONTAL)
	    {
		if (XmIsLabelGadget(kg->kid))
		{
		    _XmAssignLabG_MarginTop((XmLabelGadget)kg->kid,
					    kg->margin_top);
		    _XmReCacheLabG(kg->kid);

		    _XmAssignLabG_MarginBottom((XmLabelGadget)kg->kid,
					       kg->margin_bottom);
		    _XmReCacheLabG(kg->kid);
		}
		else
		{
		    Lab_MarginTop(kg->kid) = kg->margin_top;
		    Lab_MarginBottom(kg->kid) = kg->margin_bottom;
		}
	    }
	    else
	    {
		if (XmIsLabelGadget(kg->kid))
		{
		    _XmAssignLabG_MarginLeft((XmLabelGadget)kg->kid,
					     kg->margin_top);
		    _XmReCacheLabG(kg->kid);

		    _XmAssignLabG_MarginRight((XmLabelGadget)kg->kid,
					      kg->margin_bottom);
		    _XmReCacheLabG(kg->kid);

		    DEBUGOUT(_LtDebug(__FILE__, kg->kid,
			     "Margins now %d %d\n",
			      LabG_MarginLeft(kg->kid),
			      LabG_MarginRight(kg->kid)));
		}
		else
		{
		    Lab_MarginLeft(kg->kid) = kg->margin_top;
		    Lab_MarginRight(kg->kid) = kg->margin_bottom;
		}
	    }
	}
    }
}


/*
 * If CW is non-null, make sure not to resize it.
 *
 * The last parameter returns the RC size, to be used in geometry_manager.
 *  Why ? Because geometry_manager needs to call XmRowColumnLayout in test mode,
 *  and figure out from that whether the geometry change is acceptable. If it
 *  is, then it might need to resize the RC itself as a consequence.
 *  And *that* geometry change is a whole different issue from the one we can do
 *  inside XmRowColumnLayout.
 *
 * The meaning of "mode" :
 *  - test             == 1     means this is a query; nothing should be changed
 *  - RC_FromResize(rc) == 1    means (we're being called from the resize
 *                              method) we cannot resize ourselves, but must
 *                              fit in the size given in XtWidth/XtHeight
 *  - else                      means both Test and Resize are off
 */
extern XtGeometryResult
_XmRCAdjustSize(Widget rc, Widget instig, XtWidgetGeometry *instig_request)
{
    int max_width, max_height;
    XtWidgetGeometry rcg;
    XtGeometryResult ret = XtGeometryNo;

    DEBUGOUT(_LtDebug(__FILE__, rc,
		      "_XmRCAdjustSize: current size %d %d instig %p\n",
		      XtWidth(rc), XtHeight(rc), instig));

    /* No need to do anything else if we don't have any children */
    /* rws 13 Dec 1997
       Not true.  If we once had managed children then our size will
       be much bigger than our preferred size now.
     */
    if (MGR_NumChildren(rc) == 0 /*||
	_XmGeoCount_kids((CompositeWidget)rc) == 0*/)
    {
	return XtGeometryNo;
    }

    DEBUGOUT(_LtDebug(__FILE__, rc, "XmRCAdjustSize (%s) %s\n",
		      _LtDebugRcType2String(RC_Type(rc)),
		      RC_FromResize(rc) ? "FromResize" : ""));

    if (RC_Boxes(rc))
	XtFree((char *)RC_Boxes(rc));

    RC_Boxes(rc) = _XmRCGetKidGeo(rc, instig, instig_request,
				  RC_EntryBorder(rc), RC_EntryBorder(rc),
				  0, 0, NULL, NULL,
				  XmGET_PREFERRED_SIZE);

    /*
     * As you can see the rcg parameter of the PreferredSize*() functions is
     * never NULL
     */
    rcg.request_mode = 0;
    switch (RC_Orientation(rc))
    {
    case XmHORIZONTAL:
	if (RC_Packing(rc) == XmPACK_TIGHT)
	{
	    PreferredSizeHT(rc, &rcg, &max_width, &max_height);
	}
	else if (RC_Packing(rc) == XmPACK_COLUMN)
	{
	    PreferredSizeHC(rc, &rcg, &max_width, &max_height);
	}
	else
	{
	    PreferredSizeNone(rc, &rcg, &max_width, &max_height);
	}
	break;

    case XmVERTICAL:
	if (RC_Packing(rc) == XmPACK_TIGHT)
	{
	    PreferredSizeVT(rc, &rcg, &max_width, &max_height);
	}
	else if (RC_Packing(rc) == XmPACK_COLUMN)
	{
	    PreferredSizeVC(rc, &rcg, &max_width, &max_height);
	}
	else
	{
	    PreferredSizeNone(rc, &rcg, &max_width, &max_height);
	}
	break;

    default:
	_XmError(rc, "EEK!  What kind of RC is this?\n");
    }


    if (max_width == 0)
    {
	max_width = 1;
    }
    if (max_height == 0)
    {
	max_height = 1;
    }

    if (!RC_FromResize(rc))
    {
    /* messagebox/test8, test12 budget */
	DEBUGOUT(_LtDebug(__FILE__, rc,
			  "_XmRCAdjustSize: requesting %s from parent %s\n",
			_LtDebugWidgetGeometry2String(&rcg), XtName(XtParent(rc))));

	if (XtWidth(rc) != 0 && !RC_ResizeWidth(rc) /* && XtIsRealized(rc) */)
	{
	    rcg.request_mode &= ~CWWidth;
	    rcg.width = XtWidth(rc);
	    DEBUGOUT(_LtDebug(__FILE__, rc,
			  "_XmRCAdjustSize: Dis-allow width change requesting %s from parent %s\n",
			_LtDebugWidgetGeometry2String(&rcg), XtName(XtParent(rc))));
	}
	if (XtHeight(rc) != 0 && !RC_ResizeHeight(rc) /* && XtIsRealized(rc) */)
	{
	    rcg.request_mode &= ~CWHeight;
	    rcg.height = XtHeight(rc);
	    DEBUGOUT(_LtDebug(__FILE__, rc,
			  "_XmRCAdjustSize: Dis-allow height change requesting %s from parent %s\n",
			_LtDebugWidgetGeometry2String(&rcg), XtName(XtParent(rc))));
	}
#if 0
	if (!XtIsRealized(rc))
	{
		/* rowcolumn/test1 test25 */
		/* mfm Xquote->Option->Details */
		/*
		if (XtWidth(rc) != 0 && XtWidth(rc) != 1)
		{
			rcg.width = XtWidth(rc);
		}
		if (XtHeight(rc) != 0 && XtHeight(rc) != 1)
		{
			rcg.height = XtHeight(rc);
		}
		*/
	}
#endif

	if ((ret = _XmMakeGeometryRequest(rc, &rcg)) == XtGeometryYes)
	{
	    DEBUGOUT(_LtDebug(__FILE__, rc,
		       "_XmMakeGeometryRequest for RC said yes: size %d %d.\n",
			      XtWidth(rc), XtHeight(rc)));
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, rc,
		       "_XmMakeGeometryRequest for RC %s: size stays %d %d (%s).\n",
			      _LtDebugGeometryResult2String(ret),
			      XtWidth(rc), XtHeight(rc),
			      _LtDebugWidgetGeometry2String(&rcg)));
	}
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, rc,
			  "_XmRCAdjustSize: RC resize is %d x %d %d\n",
			  XtWidth(rc), XtHeight(rc), XtBorderWidth(rc)));
    }

    switch (RC_Orientation(rc))
    {
    case XmHORIZONTAL:
	if (RC_Packing(rc) == XmPACK_TIGHT)
	{
	    DoLayoutHT(rc, instig, instig_request, max_width, max_height);
	}
	else if (RC_Packing(rc) == XmPACK_COLUMN)
	{
	    DoLayoutHC(rc, instig, instig_request, max_width, max_height);
	}
	else
	{
	    /* Nothing to be done for XmPACK_NONE */
	}
	break;

    case XmVERTICAL:
	if (RC_Packing(rc) == XmPACK_TIGHT)
	{
	    DoLayoutVT(rc, instig, instig_request, max_width, max_height);
	}
	else if (RC_Packing(rc) == XmPACK_COLUMN)
	{
	    DoLayoutVC(rc, instig, instig_request, max_width, max_height);
	}
	else
	{
	    /* Nothing to be done for XmPACK_NONE */
	}
	break;

    default:
	_XmError(rc, "EEK!  What kind of RC is this?\n");
    }

#if 0
    /*
     * Dave Williss suggests this fixes his geometry problem
     *  > We have a dialog with an XmRowColumn in it. This has several
     *  > forms, one for each "layer" in a set of images. When the user picks
     *  > an option off a menu to "raise" or "lower" a layer, it sets the
     *  > XmNpositionIndex of that layer's form.
     *  >
     *  > In Lesstif, shuffling the children around this way causes
     *  > some weird effects and doesn't seem to even be consistent.
     *  > Sometimes I can "raise" the lower form and it may or may
     *  > not actually change the other.
     */
    if (instig == NULL)
#endif
    {
	_XmRCSetMargins(rc);
	_XmRCSetKidGeo(RC_Boxes(rc), instig);
    }

    return ret;
}


extern void
_XmRCPreferredSize(Widget rc, XtWidgetGeometry *rcg)
{
    int max_width, max_height;

#if 0
    /* No need to do anything else if we don't have any children */
    /* rws 13 Dec 1997
       Not true.  If we once had managed children then our size will
       be much bigger than our preferred size now.
     */
    if (MGR_NumChildren(rc) == 0 ||
	_XmGeoCount_kids((CompositeWidget)rc) == 0)
    {
	rcg->request_mode = 0;
	return;
    }
#endif

    if (RC_Boxes(rc))
	XtFree((char *)RC_Boxes(rc));

    RC_Boxes(rc) = _XmRCGetKidGeo(rc, NULL, NULL,
				  RC_EntryBorder(rc), RC_EntryBorder(rc),
				  0, 0, NULL, NULL,
				  XmGET_PREFERRED_SIZE);

    /*
     * As you can see the rcg parameter of the PreferredSize*() functions is
     * never NULL
     */
    switch (RC_Orientation(rc))
    {
    case XmHORIZONTAL:
	if (RC_Packing(rc) == XmPACK_TIGHT)
	{
	    PreferredSizeHT(rc, rcg, &max_width, &max_height);
	}
	else if (RC_Packing(rc) == XmPACK_COLUMN)
	{
	    PreferredSizeHC(rc, rcg, &max_width, &max_height);
	}
	else
	{
	    PreferredSizeNone(rc, rcg, &max_width, &max_height);
	}
	break;

    case XmVERTICAL:
	if (RC_Packing(rc) == XmPACK_TIGHT)
	{
	    PreferredSizeVT(rc, rcg, &max_width, &max_height);
	}
	else if (RC_Packing(rc) == XmPACK_COLUMN)
	{
	    PreferredSizeVC(rc, rcg, &max_width, &max_height);
	}
	else
	{
	    PreferredSizeNone(rc, rcg, &max_width, &max_height);
	}
	break;

    default:
	_XmError(rc, "EEK!  What kind of RC is this?\n");
    }
}

/*********************** convenience functions **************************/

static Widget
FindPrivateShell(Widget parent, Widget w)
{
    int i;
    Widget popup = NULL;

    /* As documented in the Xmt Toolkit (Menu.c):
     *
     * Now we need a menu shell.  A menubar or menupane with multiple
     * submenus need only have a single menu shell child, because only
     * one submenu can be popped up at a time.  Therefore, before we
     * go create a menu shell, we go see if one already exists as a popup
     * child of our parent.  We only make this check if the supplied parent
     * widget is a menubar or a menupane. Again, this is what Motif does.
     */
    if (XmIsRowColumn(w) &&
        (RC_Type(w) == XmMENU_BAR || RC_Type(w) == XmMENU_PULLDOWN ||
         RC_Type(w) == XmMENU_POPUP))
    {
        /*
         * Motif does the above test, which effectively means that popup
         * menus never share shells.  I don't know if it is really needed.
         */
        for (i = 0; i < CoreNumPopups(parent); i++) {
            popup = CorePopupList(parent)[i];
            if (XmIsMenuShell(popup) && !CoreBeingDestroyed(popup) &&
                MS_PrivateShell(popup))
	    {
                break;
            }
        }
    }

    return popup;
}


static Widget
MakePrivateShell(Widget parent, char *name, Arg *args, Cardinal num_args)
{
    Widget shell;
    int n;
    Arg new_args[4];
    Arg *shell_args;
    int shell_num_args;
    char *shell_name;

    /* put together a new arg list */
    n = 0;
    XtSetArg(new_args[n], XmNwidth, 5); n++;
    XtSetArg(new_args[n], XmNheight, 5); n++;
    XtSetArg(new_args[n], XmNallowShellResize, True); n++;
    XtSetArg(new_args[n], XmNoverrideRedirect, True); n++;
    shell_args = XtMergeArgLists(args, num_args, new_args, n);
    shell_num_args = num_args + n;
                     
    /* put together the new name */
    shell_name = XtMalloc((name ? strlen(name) : 0) + 7);
    sprintf(shell_name, "popup_%s", name);

    /* create the shell */
    shell = XmCreateMenuShell(parent, shell_name,
                              shell_args, shell_num_args);

    /* free the name and arglist */
    XtFree(shell_name);
    XtFree((char *)shell_args);

    /* mark shell to allow sharing */
    MS_PrivateShell(shell) = True;

    return shell;
}


extern Widget
XmCreateMenuBar(Widget parent, char *name,
		Arg *arglist, Cardinal argcount)
{
    /* menu bar's have the their rowColumnType set to XmMENU_BAR and
       are homogeneous (they only accept CascadeButtons, and 
       CascadeButtonGadgets */

    Widget w;
    Arg myArgList[5];
    int n = 0;

    ArgList combined;

    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_BAR); n++;
    XtSetArg(myArgList[n], XmNorientation, XmHORIZONTAL); n++;
    XtSetArg(myArgList[n], XmNpacking, XmPACK_TIGHT); n++;
    XtSetArg(myArgList[n], XmNisHomogeneous, True); n++;
    XtSetArg(myArgList[n], XmNentryClass, xmCascadeButtonWidgetClass); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       parent,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreateOptionMenu(Widget parent, char *name,
		   Arg *arglist, Cardinal argcount)
{
    /* option menus have the their rowColumnType set to XmMENU_OPTION */
    Widget w;
    Arg myArgList[4];
    int n = 0;

    ArgList combined;

    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_OPTION); n++;
    /* rws 23 Jun 1998
       We take care of setting this in initialize. Setting it here means
       that we cannot pick this up from a resource. So... get rid of it.
     */
    /*
    XtSetArg(myArgList[n], XmNorientation, XmHORIZONTAL); n++;
    */
    XtSetArg(myArgList[n], XmNentryAlignment, XmALIGNMENT_CENTER); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       parent,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreatePopupMenu(Widget parent, char *name,
		  Arg *arglist, Cardinal argcount)
{
    /* popup menus have the their rowColumnType set to XmMENU_POPUP */

    Widget w, ms;
    Arg myArgList[1];
    Arg shell_args[3];
    int shell_ac;
    int n = 0;
    char *popup_name = XtMalloc(strlen("popup_") + strlen(name) + 1);

    ArgList combined;

    strcpy(popup_name, "popup_");
    strcat(popup_name, name);


    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_POPUP);
    n++;

    shell_ac = 0;
    XtSetArg(shell_args[shell_ac], XmNwidth, 10); shell_ac++;
    XtSetArg(shell_args[shell_ac], XmNheight, 10); shell_ac++;
    XtSetArg(shell_args[shell_ac], XmNallowShellResize, True); shell_ac++;

    /* rws 10 Jun 1998
       Mozilla is assuming that some of the args passed in here get set
       on the created shell.  Therefore lets combine the arglists with those
       we want on the shell with those passed in.
       Thanks to Peter Haight <psh1@cornell.edu> for narrowing this one
       down.
     */
    combined = XtMergeArgLists(shell_args, shell_ac, arglist, argcount);

    ms = XtCreatePopupShell(popup_name,
			    xmMenuShellWidgetClass,
			    parent, combined, shell_ac + argcount);

    XtFree((char *)combined);

    XtFree(popup_name);

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       ms,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreatePulldownMenu(Widget parent, char *name,
		     Arg *arglist, Cardinal argcount)
{
    /* pulldown menus have their rowColumnType set to XmMENU_PULLDOWN */
    Widget w, ms;
    Arg myArgList[5];
    int n = 0;
    ArgList combined;
    Widget par;

    /* from the Xmt toolkit (Menu.c):
     *
     * if this menu pane is a submenu of a pulldown or a popup menu, then
     * the programmer probably passed the rowcol as the parent widget.  What
     * we really want is the menu shell that is the parent of that rowcol.
     * This is kludgy, but it is what the Motif XmCreatePulldownMenu() does.
     */
    if (XtParent(parent) && XmIsMenuShell(XtParent(parent)))
    {
	par = XtParent(parent);
    }
    else
    {
	par = parent;
    }

    ms = FindPrivateShell(par, parent);

    if (ms == NULL)
    {
	ms = MakePrivateShell(parent, name, arglist, argcount);
    }

    XtSetArg(myArgList[n], XmNrowColumnType, XmMENU_PULLDOWN); n++;
    /* rws 17 Aug 1999
       These are handled by initialize. Setting them here prevents changing
       these with resources.
    XtSetArg(myArgList[n], XmNorientation, XmVERTICAL); n++;
    XtSetArg(myArgList[n], XmNpacking, XmPACK_TIGHT); n++;
    XtSetArg(myArgList[n], XmNnumColumns, 1); n++;
    */

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       ms,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreateRadioBox(Widget parent, char *name,
		 Arg *arglist, Cardinal argcount)
{
    /* radio boxes have their rowColumnType set to XmWORK_AREA, and their 
       radioBehavior set to true.  The also are homogeneous and accept 
       ToggleButtons, and ToggleButtonGadgets */

    Widget w;
    Arg myArgList[2];
    int n = 0;

    ArgList combined;

    XtSetArg(myArgList[n], XmNrowColumnType, XmWORK_AREA); n++;
    XtSetArg(myArgList[n], XmNradioBehavior, True); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       parent,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

Widget
XmCreateRowColumn(Widget parent, char *name,
		  Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name, xmRowColumnWidgetClass, parent,
			  arglist, argcount);
}

Widget
XmCreateWorkArea(Widget parent, char *name,
		 Arg *arglist, Cardinal argcount)
{
    Widget w;
    Arg myArgList[2];
    int n = 0;

    ArgList combined;

    XtSetArg(myArgList[n], XmNrowColumnType, XmWORK_AREA); n++;

    combined = XtMergeArgLists(myArgList, n, arglist, argcount);

    w = XtCreateWidget(name,
		       xmRowColumnWidgetClass,
		       parent,
		       combined, n + argcount);

    XtFree((char *)combined);

    return w;
}

void
XmMenuPosition(Widget menu, XButtonPressedEvent *event)
{
    int x, y;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "MENU POSITION %p %s %d %d\n", menu, XtName(menu),
		      event->x_root, event->y_root));

	{
	Widget shell;
	Boolean was_torn;

	RCClass_MenuProcs(XtClass(menu))(XmMENU_RESTORE_TEAROFF_TO_MENUSHELL,
				     menu, &shell, &was_torn, event);
	}
    if (!XmIsMenuShell(XtParent(menu)) || RC_Type(menu) != XmMENU_POPUP)
    {
	_XmWarning(menu,
		   "XmMenuPosition called with a non popup menu.\n%s %s",
		   XtClass(menu)->core_class.class_name,
		   XtClass(XtParent(menu))->core_class.class_name);
	return;
    }

    x = event->x_root;
    y = event->y_root;

    if (x + menu->core.width >= WidthOfScreen(XtScreen(menu)))
    {
	x = WidthOfScreen(XtScreen(menu)) - menu->core.width - 1;
    }
    if (y + menu->core.height >= HeightOfScreen(XtScreen(menu)))
    {
	y = HeightOfScreen(XtScreen(menu)) - menu->core.height - 1;
    }

    if (x < 0)
    {
	x = 0;
    }
    if (y < 0)
    {
	y = 0;
    }

    /* should be all that's needed */
    _XmMoveObject(XtParent(menu), x, y);
}

Widget
XmGetTearOffControl(Widget menu)
{
    if (XmIsRowColumn(menu))
    {
	return RC_TearOffControl(menu);
    }
    return NULL;
}

void
XmAddToPostFromList(Widget menu_wid, Widget widget)
{
    /* well, we could either be adding menu_wid to widget's post
     * from list or adding widget to menu_wid's post from list.
     * I'll opt for the latter. */
    if (RC_PostFromList(menu_wid) == NULL)
    {
	RC_PostFromListSize(menu_wid) = 5;

	RC_PostFromList(menu_wid) = (Widget *)XtMalloc(sizeof(Widget) *
						RC_PostFromListSize(menu_wid));
	RC_PostFromCount(menu_wid) = 0;
    }

    RC_PostFromList(menu_wid)[RC_PostFromCount(menu_wid)++] = widget;

    if (RC_PostFromCount(menu_wid) == RC_PostFromListSize(menu_wid))
    {
	RC_PostFromListSize(menu_wid) *= 2;

	RC_PostFromList(menu_wid) =
	    (Widget *)XtRealloc((char *)RC_PostFromList(menu_wid),
				sizeof(Widget) *
				RC_PostFromListSize(menu_wid));
    }
}

void
XmRemoveFromPostFromList(Widget menu_wid, Widget widget)
{
    int i;

    if (RC_PostFromList(menu_wid) == NULL)
	return;			/* should we post an error/warning? */

    for (i = 0; i < RC_PostFromCount(menu_wid); i++)
    {
	if (RC_PostFromList(menu_wid)[i] == widget)
	{
	    int j;

	    /* found it, now slide everything down. */
	    for (j = i; j < RC_PostFromCount(menu_wid) - 1; j++)
	    {
		RC_PostFromList(menu_wid)[j] = RC_PostFromList(menu_wid)[j + 1];
	    }

	    RC_PostFromCount(menu_wid)--;

	    return;
	}
    }
    /* should getting to this point also be an error? */
}


/*
 * The result of XmGetPostedFromWidget when the callback is activated by
 * an accelerator is not defined in the manual.
 */
extern Widget
XmGetPostedFromWidget(Widget menu)
{
    if (XmIsRowColumn(menu))
    {
	/* nedit hack */
	if (RC_LastSelectToplevel(menu) == 0)
		return menu;
	/* end nedit hack */

	return RC_LastSelectToplevel(menu);
    }

    return NULL;
}
