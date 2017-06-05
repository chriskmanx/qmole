/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test2.c,v 1.6 2001/05/15 14:08:34 amai Exp $
   test of selection boxes */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/PushBP.h>
#include <Xm/MessageB.h>

#include "../../common/Test.h"

Widget toplevel, box, push;

void Push(Widget w, XtPointer client, XtPointer call)
{
  Widget	dialog = (Widget)client;

  XtManageChild(dialog);
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    6,   52,  214,  111, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,   52,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   73,   15,  129,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   47,  214,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   59,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   75,   59,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  139,   59,   64,   41, 0,0,0, /* Help */
};
/* toplevel should be replaced with to correct applicationShell */
    PrintDetails(XtParent(dialog), Expected);
  }
}

int
main(int argc, char **argv)
{
	XtAppContext	app;
	XmString	xms;
	Arg		args[3];
	int		nargs;

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
			       &argc, argv, NULL, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
			XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

	nargs=0;
	xms = XmStringCreateSimple("Yow, buddy");
	XtSetArg(args[nargs], XmNmessageString, xms); nargs++;
	box = XmCreateQuestionDialog(toplevel, "Box", args, nargs);

	XtAddCallback(push, XmNactivateCallback, Push, box);

	XtManageChild(push);

	XtRealizeWidget(toplevel);

	/*XdbPrintTree(toplevel);*/

  LessTifTestWaitForIt(toplevel);
  LessTifTestPushButton(push);
  LessTifTestMainLoop(toplevel);

	exit(0);
}
