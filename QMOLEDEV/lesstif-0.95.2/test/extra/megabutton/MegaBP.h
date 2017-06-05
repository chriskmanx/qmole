/* $Id: MegaBP.h,v 1.1 2002/05/14 23:01:27 dannybackx Exp $ */

/*
 * Copyright 1994 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.    John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose. It is provided "as is" without express or implied warranty.
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

#ifndef _MegaTBP_h
#define _MegaTBP_h

#include <Xm/XmP.h>
#include <Xm/PushBP.h>

#include "MegaB.h"

typedef void (*XmMegaButtonToggleSpaceProc)(Widget _mbw, Dimension *_width, Dimension *_height);
typedef void (*XmMegaButtonToggleDrawProc)(Widget _mbw, Position _x, Position _y, Boolean _on);

#define XmInheritMegaButtonToggleSpaceProc (XmMegaButtonToggleSpaceProc)_XtInherit
#define XmInheritMegaButtonToggleDrawProc (XmMegaButtonToggleDrawProc)_XtInherit


typedef struct _XmMegaButtonClassPart
{
	XmMegaButtonToggleSpaceProc toggleSpaceProc;
	XmMegaButtonToggleDrawProc toggleDrawProc;
} XmMegaButtonClassPart;


typedef struct _XmMegaButtonClassRec
{
	CoreClassPart core_class;
	XmPrimitiveClassPart primitive_class;
	XmLabelClassPart label_class;
	XmPushButtonClassPart push_button_class;
	XmMegaButtonClassPart mega_button_class;
} XmMegaButtonClassRec;

externalref XmMegaButtonClassRec xmMegaButtonWidgetClassRec;

typedef struct _XmMegaButtonPart
{
	int visibleItemCount;
	Boolean hasArrows;
	Boolean savedFillOnSelect;
	Boolean fakeItem;
	Boolean visibleWhenOff;
	XtPointer *cbData;
	XmString *exitems;
	_XmString *_items;
	int itemCount;
	int visiblePos;
	int firstVisibleItem;
	int setPosition;
	int timedScrollDirection;
	int initialDelay;
	int repeatDelay;
	int maxStringPos;
	unsigned char mode;
	Dimension elementHeight;
	Dimension maxStringWidth;
	Dimension stringHeight;
	Position yOffset;
	Position xOffset;
	GC copyGC;
	GC selectGC;
	XtIntervalId timer;
	XtPointer *data;
	XtPointer toggleData;
	int currentSize;
	Boolean fillOnSelect;
	Pixel selectColor;
	int chunkSize;
} XmMegaButtonPart;


/*  Full instance record declaration  */

typedef struct _XmMegaButtonRec
{
	CorePart core;
	XmPrimitivePart primitive;
	XmLabelPart label;
	XmPushButtonPart push_button;
	XmMegaButtonPart mega_button;
} XmMegaButtonRec;


#endif  /* MegaBP_h */
