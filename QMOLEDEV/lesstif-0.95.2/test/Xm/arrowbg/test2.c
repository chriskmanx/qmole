/* $Header: /cvsroot/lesstif/lesstif/test/Xm/arrowbg/test2.c,v 1.6 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>
#include <Xm/XmP.h>
#include <X11/extensions/XTest.h>

#include "../../common/Test.h"


XtIntervalId	time_outs_id;
Widget		toplevel;
Widget		one, two;

void cb(Widget w, XtPointer userData, XtPointer cbs)
{
  XmArrowButtonCallbackStruct *abcs = (XmArrowButtonCallbackStruct *)cbs;

  printf("ArrowBG Activated: "
	"x = %d\ty = %d\trx = %d\try = %d"
	"\tclick count: %d\n",
	((XButtonEvent *)abcs->event)->x,
	((XButtonEvent *)abcs->event)->y,
	((XButtonEvent *)abcs->event)->x_root,
	((XButtonEvent *)abcs->event)->y_root,
	abcs->click_count);
}

static void timeouts_proc(XtPointer client_data, XtIntervalId *timeoutsId)
{
	Display	*dpy;
	Window	mapped_child;
	int	x, y;

	printf("\nReceive TimeOuts:  ");
	dpy = XtDisplay(one);
	XTranslateCoordinates(dpy, XDefaultRootWindow(dpy), XtWindow(one),
		1, 1, &x, &y, &mapped_child);
	XTestFakeMotionEvent(dpy, DefaultScreen(dpy),
		XtX(toplevel) + XtX(one) + XtX(two),
		XtY(toplevel) + XtY(one) + XtY(two), 12);
	printf("x = %d\ty = %d\t", XtX(one), XtY(one));
	printf("rx = %d\try = %d\n",
		XtX(toplevel) + XtX(one),
		XtY(toplevel) + XtY(one));
	XTestFakeButtonEvent(dpy, 1, True, 0);
	XTestFakeButtonEvent(dpy, 1, False, 0);
}

int
main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ABG", NULL, 0, &argc, argv, NULL, NULL);

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel,
				XmNmarginWidth, 10, XmNmarginHeight, 10,
				NULL);

  one = XtVaCreateManagedWidget("One", xmArrowButtonGadgetClass, two,
				XmNwidth, 100, XmNheight, 100, NULL);

  XtAddCallback(one, XmNactivateCallback, cb, NULL);

  XtRealizeWidget(toplevel);
  time_outs_id = XtAppAddTimeOut(app, 1000, timeouts_proc, NULL);

  
{
    static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,   56,   72,  121,  121, 0,0,0}, /* Two */
   {CWWidth | CWHeight | CWX | CWY,   10,   10,  100,  100, 0,0,0}, /* One */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
