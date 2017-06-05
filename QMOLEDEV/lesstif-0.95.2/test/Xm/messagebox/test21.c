#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>

Widget toplevel;

int
main (int argc, char **argv)
{
	XtAppContext	appc;
	Widget	tmp, tmpe;
	unsigned char pack;
	short cols;

	toplevel = XtVaAppInitialize (&appc, "Boxes", NULL, 0,
	&argc, argv, NULL, NULL);

	tmp = XtVaCreateManagedWidget("question", xmMessageBoxWidgetClass, toplevel,
		XtVaTypedArg, XmNmessageString, XmRString, "question", sizeof(char *),
		XmNdialogType,	XmDIALOG_QUESTION,
		XmNwidth, 200, 
		XmNheight, 100,
	    NULL);

	XtRealizeWidget (toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  200,  152, 0,0,0, /* question */
   CWWidth | CWHeight | CWX | CWY,   11,   11,   56,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   77,   15,  112,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   47,  200,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   59,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  125,   59,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   68,  100,   64,   41, 0,0,0, /* Help */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

	exit (0);
}
