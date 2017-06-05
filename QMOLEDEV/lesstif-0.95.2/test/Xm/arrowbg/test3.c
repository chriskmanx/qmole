/* $Header: /cvsroot/lesstif/lesstif/test/Xm/arrowbg/test3.c,v 1.7 2002/05/01 15:39:21 amai Exp $
From:        Eric Howe <mu@clio.trends.ca>
To:          lesstif@hungry.com
Subject:     Arrow Button Gadget Sizing
Date:        Sat, 11 Jul 1998 12:05:03 -0400 (EDT)
*/

/*
 * This example program demonstrates an initial width and height problem
 * with arrow button gadgets.  The default values for the width and height
 * are taken from the Gadget parent and Gadget only asks for enough room
 * to draw shadows.
 */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/ArrowBG.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>

#include "../../common/Test.h"


static void
go_away(Widget w, XtPointer closure, XtPointer call)
{
	exit(EXIT_SUCCESS);
}

#define LABEL \
	"The right arrow gadget should\n" \
	"have a reasonable width, the\n" \
	"bottom arrow gadget should have\n" \
	"a reasonable height."

int
main(int argc, char **argv)
{
	XtAppContext ac;
	Widget       top, a1, a2, form, button;
	XmString     x;

	top = XtVaAppInitialize(&ac, "agsize", NULL, 0, &argc, argv,
				NULL, NULL);

	form = XtVaCreateWidget("form", xmFormWidgetClass, top,
				XmNhorizontalSpacing, 10,
				XmNverticalSpacing,   10,
				NULL);

	a1 = XtVaCreateManagedWidget("arrow1", xmArrowButtonGadgetClass, form,
				XmNtopAttachment,    XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNrightAttachment,  XmATTACH_FORM,
				NULL);
	a2 = XtVaCreateManagedWidget("arrow2", xmArrowButtonGadgetClass, form,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNrightAttachment,  XmATTACH_WIDGET,
				XmNrightWidget,      a1,
				XmNleftAttachment,   XmATTACH_FORM,
				NULL);

	/*
	 * The attachments here allow a1 to choose its own width and
	 * a2 its own height; without the ArrowBG.c patch they will
	 * choose only their shadow sizes (the Gadget default) and
	 * hence, they will be thin lines with shadows.  You can
	 * get the same effect in arrowbg/test1.c if you leave out
	 * the XmNwidth and XmNheight settings when creating the
	 * arrow button gadget.  I could have just used a slightly
	 * hacked version of test1 but that wouldn't have allowed
	 * for the nifty message in the main window.
	 */
	x = XmStringCreateLtoR(LABEL, XmFONTLIST_DEFAULT_TAG);
	button = XtVaCreateManagedWidget("quit", xmPushButtonGadgetClass, form,
				XmNlabelString,      x,
				XmNtopAttachment,    XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_WIDGET,
				XmNbottomWidget,     a2,
				XmNleftAttachment,   XmATTACH_FORM,
				XmNrightAttachment,  XmATTACH_WIDGET,
				XmNrightWidget,      a1,
				NULL);
	XmStringFree(x);
	XtAddCallback(button, XmNactivateCallback, go_away, NULL);

	XtManageChild(form);

	XtRealizeWidget(top);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  251,  117, 0,0,0}, /* form */
   {CWWidth | CWHeight | CWX | CWY,  218,   10,   23,   97, 0,0,0}, /* arrow1 */
   {CWWidth | CWHeight | CWX | CWY,   10,   84,  198,   23, 0,0,0}, /* arrow2 */
   {CWWidth | CWHeight | CWX | CWY,   10,   10,  198,   64, 0,0,0}, /* quit */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(top, Expected);
}
LessTifTestMainLoop(top);

	return EXIT_SUCCESS;
}
