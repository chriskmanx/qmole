/* $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test14.c,v 1.5 2002/05/01 15:39:21 amai Exp $ */

#if 0
From:        "Edward A. Falk" <falconer@best.com>
To:          lesstif@lesstif.org
Subject:     Bug report:  insensitive widgets with pixmap labels not handled correctly
Date:        Fri, 24 Mar 2000 13:11:30 -0800 (PST)

/* Show bug in which a button pixmap is not drawn if the button is
 * not sensitive.
 *
 * Conjecture:  Widget will only draw the labelInsensitivePixmap or nothing
 * at all.  Better behavior would be to draw the labelPixmap overlayed
 * by a 50% screen.  This is what Motif does.

 * rws 30 Mar 2000
 * The dehaviour described above is _not_ what Motif 1.2.4 does. This is
 * probably Motif 2.0 behaviour (which makes more sense).
 */
#endif

#include <stdlib.h>
#include <stdio.h>

#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>

#include <X11/Xmu/Editres.h>

#include "../../common/Test.h"


	/* Xt stuff */

static	XtAppContext	app_ctx ;

static	Widget	toplevel ;
static	Widget	button ;

static	void	workProc(XtPointer, XtIntervalId *) ;

static	String	fallback[] = {
  "*button.labelType:	pixmap",
  "*button.labelPixmap:	/usr/include/X11/bitmaps/xlogo16",
  "*button.sensitive:	False",
  NULL
};

static	int	workCount ;

int
main(int argc, char **argv)
{
	XtSetLanguageProc(NULL,NULL,NULL) ;

	toplevel = XtVaAppInitialize(&app_ctx, "Demos", NULL,0, &argc,argv,
		fallback,NULL) ;

	button = XmCreatePushButton(toplevel, "button", NULL,0) ;
	XtManageChild(button) ;

	XtRealizeWidget(toplevel) ;
{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  305,  386,   28,   28, 0,0,0, /* button */},
};
#else
static XtWidgetGeometry Expected[] = {
	CWWidth | CWHeight, 0, 0, 12, 12, 0,0,0, /* button */
};
#endif

	PrintDetails(toplevel, Expected);
}

	LessTifTestMainLoop(toplevel);

	exit(0) ;
}
