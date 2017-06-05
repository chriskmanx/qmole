/**
 *
 * $Id: Obsolete.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright (C) 1997-2001 LessTif Development Team
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

static const char rcsid[] = "$Id: Obsolete.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <Xm/XmP.h>
#include <Xm/DrawP.h> /* may also be included by XmP.h */
#include <Xm/Text.h>
#include <Xm/TextF.h>

#include <XmI/DebugUtil.h>


int XmTextFieldGetBaseLine(Widget w);
int XmTextGetBaseLine(Widget w);
void _XmChangeNavigationType(Widget w, XmNavigationType nav_type);
void _XmDrawShadowType(Widget w, unsigned int type, Dimension width,
                       Dimension height, Dimension shadow_thickness,
		       Dimension highlight_thickness, GC top_gc, GC bottom_gc);


/* This file contains functions from previous versions that are now obsolete.
   Some inspiration came from the real Motif implementations - since
   those details are rarely documented ... */


static void
GenericMessage(const char *func)
{
   _XmWarning(NULL,
              "%s - This function is considered obsolete. Update your sources!\n",
              func);
}


#define PrintSingleWarning(x) \
  static Boolean FirstCall = True; \
  if (FirstCall) { \
     GenericMessage(x); \
     FirstCall = False; \
  }


/* From where do these two functions come, <1.2 ?! */
extern int
XmTextFieldGetBaseLine(Widget w)
{
   PrintSingleWarning("XmTextFieldGetBaseLine")
   return XmTextFieldGetBaseline(w);
}


extern int
XmTextGetBaseLine(Widget w)
{
   PrintSingleWarning("XmTextGetBaseLine")
   return XmTextGetBaseline(w);
}


extern void
_XmChangeNavigationType(Widget w, XmNavigationType nav_type)
{
   PrintSingleWarning("_XmChangeNavigationType")
}


extern void
_XmDrawShadowType(Widget w, unsigned int type, Dimension width, Dimension height, Dimension shadow_thickness, Dimension highlight_thickness, GC top_gc, GC bottom_gc)
{

   PrintSingleWarning("_XmDrawShadowType")
   _XmDrawShadows(XtDisplay(w),
                  XtWindow(w),
                  top_gc,
                  bottom_gc,
                  highlight_thickness, highlight_thickness,
                  width - (2 * highlight_thickness), height - (2 * highlight_thickness),
                  shadow_thickness,
                  type);
}
