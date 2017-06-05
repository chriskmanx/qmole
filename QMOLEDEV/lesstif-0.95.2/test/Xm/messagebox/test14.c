/* test of selection boxes */

#include <Xm/Xm.h>
#include <Xm/PushBP.h>
#include <Xm/MessageB.h>

static char *FallBack[] = {
		"*.geometrySlop: 1",
		NULL
};

Widget toplevel, box, push;

void Push(Widget w, XtPointer client, XtPointer call)
{
	Widget	dialog = (Widget)client;

	XtManageChild(dialog);
}

int
main(int argc, char **argv)
{
	XtAppContext	app;
	XmString	xms;
	Arg		args[3];
	int		nargs;

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
			       &argc, argv, FallBack, NULL);

	nargs=0;
	xms = XmStringCreateSimple("Yow, buddy");
	XtSetArg(args[nargs], XmNmessageString, xms); nargs++;
	XtSetArg(args[nargs], XmNwidth, 300); nargs++;
	XtSetArg(args[nargs], XmNheight, 300); nargs++;
	box = XmCreateMessageBox(toplevel, "Box", args, nargs);
	XtManageChild(box);

	XtRealizeWidget(toplevel);

	/*XdbPrintTree(toplevel);*/

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  300,  300, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   60,  278,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,  137,  300,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  199,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  118,  199,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  225,  199,   64,   41, 0,0,0, /* Help */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

	exit(0);
}
