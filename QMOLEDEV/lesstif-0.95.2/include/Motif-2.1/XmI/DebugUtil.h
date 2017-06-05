/**
 *
 * $Id: DebugUtil.h,v 1.5 2006/04/19 18:42:22 dannybackx Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2002, 2004, 2005 LessTif Development Team
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

#ifndef _XMI_DEBUGUTIL_H
#define _XMI_DEBUGUTIL_H

/* #include <LTconfig.h> */

#include <Xm/XmP.h>

/* initialize debug system */
Boolean _LtDebugInit(void);
/* turn Debugging on/off */
void _LtDebugSet(Boolean flag);
/* toggle Debugging */
void _LtDebugToggle(void);
/* query about debugging */
Boolean _LtDebugQueryState(void);
/*
 * Print a widget tree
 */
void _LtDebugPrintTree(Widget w);
void _LtDebugPrintCompleteTree(Widget w);

/*
 *  State of a widget
 */
const char *_LtDebugState(Widget w);

/*
 * Print an Arg list
 */
void _LtDebugPrintArgList(const char *fn, Widget w, ArgList al, int n, Boolean Get);

/*
 * Convert types into string format
 */
const char *_LtDebugFrameChildType2String(int action);
const char *_LtDebugFocusDetail2String(int type);
const char *_LtDebugFocusMode2String(int type);
const char *_LtDebugEventType2String(int type);
const char *_LtDebugComboBoxType2String(unsigned char type);
const char *_LtDebugGeoAction2String(int action);
const char *_LtDebugDeleteResponse2String(int d);
const char *_LtDebugGeometryResult2String(XtGeometryResult r);
const char *_LtDebugDragAndDropMessageType2String(unsigned char r);
const char *_LtDebugDragType2String(unsigned char r);
const char *_LtDebugWidgetGeometry2String(XtWidgetGeometry *g);

const char *_LtDebugAttachment2String(int i);
const char *_LtDebugMenuFocusOp2String(int f);
const char *_LtDebugMenuEnum2String(int f);
const char *_LtDebugMwmInput2String(int a);
const char *_LtDebugBoolean2String(Boolean b);
const char *_LtDebugXmString2String(XmString xms);
const char *_LtDebugPacking2String(unsigned char p);
const char *_LtDebugRcType2String(unsigned char t);
const char *_LtDebugAlignment2String(int n);
const char *_LtDebugMenuType2String(int n);
const char *_LtDebugNavigability2String(unsigned char n);
const char *_LtDebugHighlightMode2String(int mode);
const char *_LtDebugSelectionPolicy2String(int n);
const char *_LtDebugReason2String(int reason);

const char *_LtDebugFocusChange2String(XmFocusChange c);

const char *_LtDebugNavigationType2String(XmNavigationType nt);
const char *_LtDebugEditMode2String(int n);
const char *_LtDebugSBDisplayPolicy2String(int n);
const char *_LtDebugSBPlacement2String(int n);
const char *_LtDebugListSizePolicy2String(int n);
const char *_LtDebugResizePolicy2String(int n);

const char *_LtDebugScrollingPolicy2String(unsigned char r);
const char *_LtDebugDialogStyle2String(int a);

const char * _LtDebugVisualPolicy2String(int);
const char * _LtDebugScrollPolicy2String(int);
const char * _LtDebugXmStringComponentType2String(int);


/*
 * Debug printing functions
 */
void _LtDebug(const char *fn, Widget w, const char *fmt, ...);
void _LtDebug2(const char *fn, Widget w, Widget c, const char *fmt, ...);
void _LtDebug0(const char *fn, Widget w, const char *fmt, ...);
void _LtDebugPrintString(const char *s);
void _LtDebugAction(const char *, Widget, const String, const String *, const Cardinal *);
void _LtDebugPrintRenderTable(const char *, Widget, char *, XmRenderTable);
void _LtDebugPrintManagedChildren(const char *, Widget, const char *);

#ifdef LESSTIF_PRODUCTION
#define	_LtDebugInDebug(x, y)	False
#define DEBUGOUT(x)

#ifdef	USE_DMALLOC
#undef	USE_DMALLOC
#endif
#else
Boolean _LtDebugInDebug(const char *fn, Widget w);
#define DEBUGOUT(x)	x
#endif /* LESSTIF_PRODUCTION */

/*
 * Some stuff to produce sensible tracing with dmalloc.
 * Check the INSTALL(.html) doc for references about the
 * dmalloc package!
 */
#ifdef WITH_DMALLOC

#include <dmalloc.h>

/* Our Xt*alloc() replacements */
XtPointer   _LtDebugMalloc(const char *f,  int l, Cardinal size);
XtPointer   _LtDebugCalloc(const char *f,  int l, Cardinal count, Cardinal size);
XtPointer   _LtDebugRealloc(const char *f, int l, XtPointer p, Cardinal size);
void        _LtDebugFree(const char *f,    int l, XtPointer p);

#ifdef	XtMalloc
#undef	XtMalloc
#endif
#define	XtMalloc(x)	_LtDebugMalloc(__FILE__, __LINE__, x)
#ifdef	XtCalloc
#undef	XtCalloc
#endif
#define	XtCalloc(x,y)	_LtDebugCalloc(__FILE__, __LINE__, x, y)
#ifdef	XtRealloc
#undef	XtRealloc
#endif
#define	XtRealloc(x,y)	_LtDebugRealloc(__FILE__, __LINE__, x, y)
#ifdef	XtFree
#undef	XtFree
#endif
#define	XtFree(x)	_LtDebugFree(__FILE__, __LINE__, x)

#else
#ifdef WITH_DBMALLOC
#include <dbmalloc.h>
#endif	/* WITH_DBMALLOC */

#endif	/* WITH_DMALLOC */


#endif /* _XMI_DEBUGUTIL_H */
