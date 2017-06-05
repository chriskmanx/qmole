/*
 * SpinButton.h, Interleaf, 16aug93 2:37pm Version 1.1.
 */

/***********************************************************
Copyright 1993 Interleaf, Inc.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose without fee is granted,
provided that the above copyright notice appear in all copies
and that both copyright notice and this permission notice appear
in supporting documentation, and that the name of Interleaf not
be used in advertising or publicly pertaining to distribution of
the software without specific written prior permission.

Interleaf makes no representation about the suitability of this
software for any purpose. It is provided "AS IS" without any
express or implied warranty. 
******************************************************************/

/*
 *  (C) Copyright 1991,1992, 1993
 *  Interleaf, Inc.
 *  Nine Hillside Avenue, Waltham, MA  02154
 */
#ifndef _SpinButton_h
#define _SpinButton_h

#ifndef AA
#define AA(args) ()
#endif

#include <Xm/Xm.h>

#define XmNarrowLayout      "arrowLayout"
#define XmNarrowSensitivity "arrowSensitivity"
#define XmNarrowSize        "arrowSize"
#define XmNposition	    "position"
#define XmNtextField	    "textField"
#define XmNwrap		    "wrap"

#define XmCArrowLayout      "ArrowLayout"
#define XmCArrowSensitivity "ArrowSensitivity"
#define XmCTextField	    "TextField"
#define XmCWrap		    "Wrap"

#define XmRArrowSensitivity "ArrowSensitivity"
#define XmRArrowLayout      "ArrowLayout"

/* Defines for arrowLayout */
#define XmARROWS_FLAT_BEGINNING	0
#define XmARROWS_FLAT_END	1
#define XmARROWS_SPLIT		2
#define XmARROWS_BEGINNING	3
#define XmARROWS_END		4

/* Defines for arrowSensitivity */
#define XmARROWS_SENSITIVE	    0
#define XmARROWS_LEFT_SENSITIVE	    1
#define XmARROWS_RIGHT_SENSITIVE    2
#define XmARROWS_INSENSITIVE	    3

/* defines for childType */
#define XmNUMERIC 0

/* defines for callback reason; XmCR_OK is enum (31) */
#define XmCR_SPIN_UP	100
#define XmCR_SPIN_DOWN	101

typedef struct
{
   int		reason;
   XEvent	*event;
   Widget	widget;
   Boolean	doit;
   int		position;
   XmString	value;
   Boolean	crossed_boundary;
} DtSpinButtonCallbackStruct;

extern WidgetClass dtSpinButtonWidgetClass;

typedef struct _DtSpinButtonClassRec *DtSpinButtonWidgetClass;
typedef struct _DtSpinButtonRec      *DtSpinButtonWidget;

/*
 * External functions which manipulate the SpinButton list of items.
 */
extern Widget DtCreateSpinButton AA((Widget parent, char *name,
				     Arg *arglist, int num_args));
extern void DtSpinButtonAddItem AA((DtSpinButtonWidget spin, XmString item,
				    int pos));
extern void DtSpinButtonDeletePos AA((DtSpinButtonWidget spin, int pos));
extern void DtSpinButtonSetItem AA((DtSpinButtonWidget spin, XmString item));

#endif	/* _SpinButton_h */
