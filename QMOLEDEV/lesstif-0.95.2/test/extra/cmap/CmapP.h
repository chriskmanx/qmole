/* $Id: CmapP.h,v 1.1 2002/05/15 10:14:42 amai Exp $ */
/*
 * Copyright 1993,1994 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.  John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
 * regard to this software, including all implied warranties of
 * merchantability and fitness, in no event shall John L. Cwikla or
 * Wolfram Research, Inc be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Author:
 *  John L. Cwikla
 *  X Programmer
 *  Wolfram Research Inc.
 *
 *  cwikla@wri.com
*/

#ifndef _CmapP_h
#define _CmapP_h

/*
** Cmap Widget
*/

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <Xm/XmP.h>
#if (XmREVISON == 2)
#include <Xm/PrimitiveP.h>
#endif /* XmREVISION */
#include "Cmap.h"

typedef struct _CmapPart
{
	XFontStruct *font;
	/* "R:65535 G:65535 B:65535 (255 of 255)" */
	char label[50];	/* Label for rgb string */
	Boolean showUnselectedShadows;
	Boolean wasHighlighted;
	Pixel *mappingTable;
	XColor selectedColor;
	Dimension margin;
	Dimension halfMargin;
	int availColors;
	int usedColors;
	int selected;
	int numX;
	int numY;
	int firstIndex;
	int lastIndex;
	XPoint labelPos;
	XPoint drawPos;
	Dimension labelHeight;
	Dimension labelWidth;
	Dimension lineWidth;
	Atom standardColormap;
	Dimension boxWidth;
	Dimension boxHeight;
	Dimension realBoxWidth;
	Dimension realBoxHeight;
	GC gc;
	GC foregroundGC;
	GC eraseGC;
	XtCallbackList changeCallback;
	Boolean invertedShadows;
	XRectangle *hrects;
	XmFontList fontList;
} CmapPart, *CmapPartPtr;

typedef struct _CmapRec
{
	CorePart core;
	XmPrimitivePart primitive;
	CmapPart cmap;
} CmapRec, *CmapPtr;

typedef struct _CmapClassPart
{
	int empty;
} CmapClassPart;

typedef struct _CmapClassRec
{
	CoreClassPart core_class;	
	XmPrimitiveClassPart primitive_class;
	CmapClassPart cmap_class;
} CmapClassRec, *CmapClassPtr;

extern CmapClassRec cmapClassRec;

#endif /* _CmapP_h */
