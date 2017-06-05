/*
 *
 * $Id: DropSMgrI.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

static const char rcsid[] = "$Id: DropSMgrI.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>

#include <Xm/DropSMgrP.h>
#include <Xm/DisplayP.h>
#include <XmI/DragDropI.h>
#include <Xm/DragCP.h>
#include <Xm/DropTrans.h>
#include <Xm/XmP.h>

#include <XmI/DebugUtil.h>


#define Offset(f)	XtOffsetOf(XmDropSiteInfoRec, f)
XtResource _XmDSResources[] =
{
    {
	XmNanimationMask, XmCAnimationMask, XmRAnimationMask,
	sizeof(Pixmap), Offset(animationMask),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNanimationPixmap, XmCAnimationPixmap, XmRAnimationPixmap,
	sizeof(Pixmap), Offset(animationPixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNanimationPixmapDepth, XmCAnimationPixmapDepth, XmRCardinal,
	sizeof(Cardinal), Offset(animationPixmapDepth),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNanimationStyle, XmCAnimationStyle, XmRAnimationStyle,
	sizeof(unsigned char), Offset(animationStyle),
	XmRImmediate, (XtPointer)XmDRAG_UNDER_HIGHLIGHT
    },
    {
	XmNdragProc, XmCDragProc, XmRProc,
	sizeof(XtCallbackProc), Offset(dragProc),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropProc, XmCDropProc, XmRProc,
	sizeof(XtCallbackProc), Offset(dropProc),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropRectangles, XmCDropRectangles, XmRRectangleList,
	sizeof(XRectangle *), Offset(dropRectangles),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropSiteActivity, XmCDropSiteActivity, XmRDropSiteActivity,
	sizeof(unsigned char), Offset(dropSiteActivity),
	XmRImmediate, (XtPointer)XmDROP_SITE_ACTIVE
    },
    {
	XmNdropSiteType, XmCDropSiteType, XmRDropSiteType,
	sizeof(unsigned char), Offset(dropSiteType),
	XmRImmediate, (XtPointer)XmDROP_SITE_SIMPLE
    },
    {
	XmNimportTargets, XmCImportTargets, XmRAtomList,
	sizeof(Atom *), Offset(importTargets),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNnumImportTargets, XmCNumImportTargets, XmRCardinal,
	sizeof(Cardinal), Offset(numImportTargets),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNnumDropRectangles, XmCNumDropRectangles, XmRCardinal,
	sizeof(Cardinal), Offset(numDropRectangles),
	XmRImmediate, (XtPointer)1
    },
    {
	XmNdropSiteOperations, XmCDropSiteOperations, XmRDropSiteOperations,
	sizeof(unsigned char), Offset(dropSiteOperations),
	XmRImmediate, (XtPointer)(XmDROP_MOVE | XmDROP_COPY)
    }
};
#undef Offset

int _XmNumDSResources = XtNumber(_XmDSResources);


/* netscape 4.5PR1 wants these; dunno what they're supposed to do -jac */
extern void 
_XmDSIAddChild(void* a, void* b, int c)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmDSIAddChild(%p, %p, %d) - stub!\n",
		      a, b, c));
}


extern void 
_XmDSIRemoveChild(void* a, void* b)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "_XmDSIRemoveChild(%p, %p) - stub!\n",
		      a, b));
}


extern void 
_XmDSIDestroy(void* a, int b)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "_XmDSIDestroy(%p, %d) - stub!\n",
		      a, b));
}


extern int 
_XmDSIGetBorderWidth(void* a)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "_XmDSIGetBorderWidth(%p) - stub!\n",
		      a));
    return 0;
}


extern int 
_XmDSIReplaceChild(void* a)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "_XmDSIReplaceChild(%p) - stub!\n",
		      a));
    return 0;
}


extern int
_XmDSIGetChildPosition(void* a, void* b)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "_XmDSIGetChildPosition(%p) - stub!\n",
		      a, b));
    return 0;
}


extern void 
_XmDSISwapChildren(void* a, void* b, void* c)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "_XmDSISwapChildren(%p, %p, %p) - stub!\n",
		      a, b, c));
}
