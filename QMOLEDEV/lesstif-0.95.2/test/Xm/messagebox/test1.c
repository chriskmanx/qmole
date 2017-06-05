/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test1.c,v 1.6 2001/05/15 14:08:34 amai Exp $ */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/MessageB.h>

#include "../../common/Test.h"

void HiCB (Widget w, XtPointer client_data, XtPointer call_data);

Widget toplevel;

int
main (int argc, char **argv)
{
  XtAppContext theApp;
  Widget butt1;

  toplevel = XtVaAppInitialize (&theApp, "drawingArea", NULL, 0,
				&argc, argv, NULL, NULL);

  butt1 = XtVaCreateManagedWidget ("Button1", xmPushButtonWidgetClass, toplevel,
				   NULL, 0);

  XtAddCallback (butt1, XmNactivateCallback, HiCB, NULL);

  XtRealizeWidget (toplevel);

  LessTifTestWaitForIt(toplevel);
  LessTifTestPushButton(butt1);
  LessTifTestMainLoop(toplevel);

  exit (0);
}

void
Quit(Widget w, XtPointer client, XtPointer call)
{
    exit(0);
}

void
Again(Widget w, XtPointer client, XtPointer call)
{
    Widget dialog;
    XmString xmstr = XmStringCreateSimple("I have a title"),
	ok = XmStringCreateSimple("Quit"),
	cancel = XmStringCreateSimple("Cancel String");

    Arg al[4];
    int ac;
    
    ac = 0;
    XtSetArg(al[ac], XmNdialogTitle, xmstr); ac++;
    XtSetArg(al[ac], XmNdefaultPosition, False); ac++;
    XtSetArg(al[ac], XmNokLabelString, ok); ac++;		/* initialize */
    dialog = XmCreateMessageDialog(toplevel, "dialog2", al, ac);

    XtVaSetValues(dialog,
		XmNmessageString,	xmstr,
		XmNcancelLabelString,	cancel,			/* set_values */
	NULL);
    
    XtManageChild (dialog);
    XtAddCallback(dialog, XmNokCallback, Quit, NULL);
}

void 
HiCB (Widget w, XtPointer client_data, XtPointer call_data)
{

  static Widget dialog = NULL;
  XmString xmstr;


  if (!dialog) {
     dialog = XmCreateMessageDialog(toplevel, "MyDialog", NULL, 0);
     xmstr = XmStringCreateLtoR("Hello World\n\nIf you hit OK on this dialog, another one"
				      "\nshould appear, positioned to the lower right of this one.",
				      XmFONTLIST_DEFAULT_TAG);
  
  
     XtVaSetValues(dialog, XmNmessageString, xmstr, NULL);
  
     XtAddCallback(dialog, XmNokCallback, Again, NULL);
  }
  XtManageChild (dialog);
  LessTifTestWaitForIt(dialog);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    6,   37,  368,  141, 0,0,0, /* MyDialog */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  346,   56, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   77,  368,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   89,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  152,   89,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  293,   89,   64,   41, 0,0,0, /* Help */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(XtParent(dialog), Expected);
} 

}

