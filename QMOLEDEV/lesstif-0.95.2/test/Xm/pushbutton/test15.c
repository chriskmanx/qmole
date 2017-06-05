/* $Header: /cvsroot/lesstif/lesstif/test/Xm/pushbutton/test15.c,v 1.6 2001/06/18 09:04:00 amai Exp $
From:        Eric Howe <mu@clio.trends.ca>
To:          lesstif@hungry.com
Subject:     Default Button Clipping
Date:        Tue, 14 Jul 1998 01:28:40 -0400 (EDT)
*/

/*
 * This program will show a clipping problem when a push button (or gadget)
 * is forced to be smaller than it wants to be and a default button is
 * being used.
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

#include "../../common/Test.h"


#define LABEL \
	"A big label to show the\n" \
	"\"default button\" clipping\n" \
	"problem blah blah blah blah\n" \
	"blah blah blah blah blah blah"
Widget top;

static void
dothings(Display *dpy, char *title, WidgetClass buttonClass)
{
	Widget    form, b1, b2;
	XmString  x;
	Dimension width, height;

	top = XtVaAppCreateShell(NULL, "DefBut",applicationShellWidgetClass,dpy,
		XmNtitle,    title,
		XmNiconName, title,
		NULL);
	form = XtVaCreateWidget("form", xmFormWidgetClass, top, NULL);

	x = XmStringCreateLtoR(LABEL, XmFONTLIST_DEFAULT_TAG);
	b1 = XtVaCreateManagedWidget("one", buttonClass, form,
		XmNlabelString,      x,
		XmNtopAttachment,    XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment,   XmATTACH_FORM,
		NULL);
	b2 = XtVaCreateManagedWidget("two", buttonClass, form,
		XmNlabelString,      x,
		XmNtopAttachment,    XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNrightAttachment,  XmATTACH_FORM,
		XmNleftAttachment,   XmATTACH_WIDGET,
		XmNleftWidget,       b1,
		NULL);
	XmStringFree(x);
	XtVaSetValues(form, XmNdefaultButton, b1, NULL);

	XtManageChild(form);
	XtRealizeWidget(top);

	XtVaGetValues(top, XmNwidth, &width, XmNheight, &height, NULL);
	width  = (width  * 2) / 3;
	height = (height * 2) / 3;
	XtVaSetValues(top, XmNwidth, width, XmNheight, height, NULL);
}

int
main(int argc, char **argv)
{
	XtAppContext ac;
	Display      *dpy;

	XtSetLanguageProc(NULL, NULL, NULL);
	XtToolkitInitialize();
	ac  = XtCreateApplicationContext();
	dpy = XtOpenDisplay(ac, NULL, NULL, "DefBut", NULL, 0, &argc, argv);

	dothings(dpy, "PushButtonGadget", xmPushButtonGadgetClass);
	dothings(dpy, "PushButtonWidget", xmPushButtonWidgetClass);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  264,   50, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  198,   50, 0,0,0, /* one */
   CWWidth | CWHeight | CWX | CWY,  198,    0,  66,   50, 0,0,0, /* two */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(top, Expected);
}
LessTifTestMainLoop(top);
	return 0;
}
