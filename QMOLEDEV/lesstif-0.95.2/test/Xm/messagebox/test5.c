/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test5.c,v 1.7 2001/05/15 14:08:34 amai Exp $
   test of selection boxes */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/PushBP.h>
#include <Xm/MessageB.h>
#include <Xm/MessageBP.h>

#include "../../common/Test.h"


char *fallback[] = {
   "*.geometrySlop: 1",
   "*Box*fontList:      -*-times-bold-i-*--24-240-*=chset1,"
   "                    -*-times-bold-i-*--12-120-*=chset2,"
   "                    -misc-fixed-medium-r-*--10-100-*=chset3,"
   "                    fixed",
   NULL
};

Widget toplevel, box, push;

void Push(Widget w, XtPointer client, XtPointer call)
{
	Widget	dialog = (Widget)client;

	XtManageChild(dialog);
	LessTifTestWaitForIt(dialog);
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    6,   39,  253,  138, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   24,   26,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   47,   11,  195,   53, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   74,  253,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   86,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   94,   86,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  177,   86,   64,   41, 0,0,0, /* Help */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(XtParent(dialog), Expected);
}
}

int
main(int argc, char **argv)
{
	XtAppContext	app;
	XmString	xs, xs1, xs2, xs3;
	Arg		args[3];
	int		nargs;

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
			       &argc, argv, fallback, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
		      XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 10,
		      NULL);

	nargs=0;
	xs1 = XmStringCreateLtoR("Yow, buddy, ", "chset1");
        xs2 = XmStringCreateLtoR("this is a test\nof fontlists, too.\n", "chset2");
        xs3 = XmStringCreateLtoR("With three charsets, even.", "chset3");
	xs = XmStringConcat(xs1, xs2);
	xs = XmStringConcat(xs, xs3);
	box = XmCreateQuestionDialog(toplevel, "Box", args, nargs);
	XtVaSetValues(box, XmNmessageString, xs, NULL);

	XtAddCallback(push, XmNactivateCallback, Push, box);

	XtManageChild(push);

	XtRealizeWidget(toplevel);

	/*XdbPrintTree(toplevel);*/

  LessTifTestWaitForIt(toplevel);
  LessTifTestPushButton(push);
  LessTifTestMainLoop(toplevel);

	exit(0);
}
