/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test9.c,v 1.7 2001/05/15 14:08:34 amai Exp $
   test of selection boxes */

#include <stdlib.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/PushBP.h>
#include <Xm/MessageB.h>

#include "../../common/Test.h"


Widget toplevel, box, push;
XtAppContext	app_context;
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    6,   22,  214,  111, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,   31,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   52,   15,  150,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   47,  214,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   59,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   75,   59,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  139,   59,   64,   41, 0,0,0, /* Help */

   CWWidth | CWHeight            ,    7,   23,  248,  111, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,   26,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   47,   15,  190,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   47,  248,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   59,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   92,   59,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  173,   59,   64,   41, 0,0,0, /* Help */

   CWWidth | CWHeight            ,    7,   23,  290,  111, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,   26,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   47,   15,  232,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   47,  290,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   59,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  113,   59,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  215,   59,   64,   41, 0,0,0, /* Help */

   CWWidth | CWHeight            ,    7,   23,  290,  111, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,   26,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   47,   15,  232,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   47,  290,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   59,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  113,   59,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  215,   59,   64,   41, 0,0,0, /* Help */
};

void FlushQueue(void)
/*- 
  flush all events out of the event queue
-*/
{
    XtInputMask m;

    XFlush(XtDisplay(toplevel));
    while ((m = XtAppPending(app_context)))
	XtAppProcessEvent(app_context, m);
    XmUpdateDisplay(toplevel);
}



void Push(Widget w, XtPointer client, XtPointer call)
{
	Widget	dialog = (Widget)client;
	XmString xmstr;

	XtManageChild(dialog);
	FlushQueue();
	FlushQueue();
	sleep(1);
{
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(XtParent(dialog), Expected);
}

	xmstr=XmStringCreateSimple("I said 'Yow, buddy!'");
	XtVaSetValues(dialog,
		      XmNmessageString, xmstr,
		      NULL);
	XmStringFree(xmstr);
	FlushQueue();
	FlushQueue();
	sleep(1);

{
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(XtParent(dialog), Expected);
}

	xmstr=XmStringCreateSimple("Hey, you!  I said 'Yow, buddy!'");
	XtVaSetValues(dialog,
		      XmNmessageString, xmstr,
		      NULL);
	XmStringFree(xmstr);
	FlushQueue();
	FlushQueue();
	sleep(1);
{
/* toplevel should be replaced with to correct applicationShell */
  PrintDetails(XtParent(dialog), Expected);
}

	xmstr=XmStringCreateSimple("Did you hear me?  I said 'Yow, buddy!'");
	XtVaSetValues(dialog,
		      XmNmessageString, xmstr,
		      NULL);
	XmStringFree(xmstr);
	FlushQueue();
	FlushQueue();
	sleep(1);
{
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

	app_context=app;
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
