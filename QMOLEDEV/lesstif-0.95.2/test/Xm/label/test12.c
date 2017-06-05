/* $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test12.c,v 1.3 2001/05/15 13:40:54 amai Exp $ */
/*
From:        "Edward A. Falk" <falconer@best.com>
To:          lesstif@lesstif.org
Subject:     Lesstiff 0.89-9 bug report:  Label widget & 1-bit bitmaps
Date:        Fri, 3 Mar 2000 15:22:01 -0800 (PST)

Hello; I am told that this is the right email address for reporting
lesstif bugs.  Here is the first (and most important) of the bugs I
have discovered.

The program below creates a Label widget and sets its
XmNlabelPixmap resource to a 1-bit bitmap.  Under Motif,
it draws correctly.  Under Lesstif, a BadMatch error results
in XCopyArea.


I do not have access to Motif source code, but I examined the
program using xscope and observed the following behavior:

	Motif creates windows
	Program creates bitmap
	Program sets XmNlabelPixmap to this bitmap
	Motif sends GetGeometry request to server
		server responds with 16x16, depth=1
	Motif draws bitmap to window with CopyPlane()

If I change the program to use a pixmap of the same depth as the
window, the behavior is identical, except that Motif executes the
copy with CopyArea() instead.

Motif calls GetGeometry() only once, no matter how many times I
force redraws.  I conclude that Motif is caching the pixmap dimensions
somewhere, probably in a per-pixmap internal cache.

I have examined Lesstif source code and it seems that Lesstif always
uses CopyArea().

p.s.: if you're interested, I have some simple code which I use
for caching pixmap geometry.  You could easily add it to Lesstif.


Test program below.  This suceeds under Motif and fails under
Lesstif.  This is a serious bug, as it's interfering with a
drafting program I'm writing (http://www.best.com/~falconer/Xdraw/)

Thank you for your attention.

	-ed falk, falk@falconer.vip.best.com



*/

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Label.h>
#include <X11/bitmaps/xlogo16>


static	Display	*dpy ;

static	XtAppContext	app_ctx ;
static	Widget	topLevel ;

int
main(int argc, char **argv)
{
	Widget	lbl ;
	Pixmap	xlogo ;

	XtSetLanguageProc(NULL,NULL,NULL) ;

	topLevel = XtVaAppInitialize(&app_ctx, "Test", NULL,0, &argc,argv,
		NULL,NULL) ;

	lbl = XtCreateManagedWidget("lbl",xmLabelWidgetClass, topLevel,NULL,0);

	XtRealizeWidget(topLevel) ;

	xlogo = XCreateBitmapFromData(XtDisplay(lbl), XtWindow(lbl),
		(const char *)xlogo16_bits, xlogo16_width, xlogo16_height) ;

	XtVaSetValues(lbl,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, xlogo,
		0) ;

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  361,  690,   22,   17, 0,0,0, /* lbl */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(topLevel, Expected);
}
LessTifTestMainLoop(topLevel);

	exit(0) ;
}
