#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Command.h>

static char *FallBack[] = {
		"*.geometrySlop: 2",
		NULL
};

void Print(Widget w, XtPointer client, XtPointer call)
{
	XmCommandCallbackStruct *p = (XmCommandCallbackStruct *)call;
	char			*t;
	XmString		comtxt;

	XmStringGetLtoR(p->value, XmFONTLIST_DEFAULT_TAG, &t);
	fprintf(stderr, "Command : '%s'\n", t);
	XtFree(t);

	XtVaGetValues(w, XmNcommand, &comtxt, NULL );
	if (!XmStringEmpty(comtxt)) {
		fprintf(stderr, "Yow then\n");
	} else {
		fprintf(stderr, "Yow else\n");
	}
	XmStringGetLtoR(comtxt, XmFONTLIST_DEFAULT_TAG, &t);
	fprintf(stderr, "Yow(%s)\n", t);
	XtFree(t);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, FallBack, NULL);

  box = XmCreateCommand(toplevel, "Box", NULL, 0);

  XtAddCallback(box, XmNcommandEnteredCallback, Print, NULL);
  XtManageChild(box);

  XtRealizeWidget(toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  506,  322,  162,  217, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   12,   12,  138,  135, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  138,  135, 0,0,0, /* ItemsList */
   CWWidth | CWHeight | CWX | CWY,   12,  157,  138,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   12,  174,  138,   31, 0,0,0, /* Text */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}

  LessTifTestMainLoop(toplevel);

  exit(0);
}
