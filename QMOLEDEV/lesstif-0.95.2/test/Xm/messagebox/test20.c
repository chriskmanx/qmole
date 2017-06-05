/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test20.c,v 1.2 2001/05/15 14:20:18 amai Exp $ */
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Label.h>

#include "../../common/Test.h"

Widget toplevel;

int
main (int argc, char **argv)
{
  XtAppContext theApp;
  Widget dialog, rc, label;
  XmString xmstr;

  toplevel = XtVaAppInitialize (&theApp, "drawingArea", NULL, 0,
				&argc, argv, NULL, NULL);
     XtVaSetValues(toplevel,
     	XmNallowShellResize, True,
     	NULL);
     dialog = XmCreateMessageBox(toplevel, "MyDialog", NULL, 0);
     xmstr = XmStringCreateLtoR("Hello World", XmFONTLIST_DEFAULT_TAG);
  
  
     XtVaSetValues(dialog, XmNmessageString, xmstr, NULL);
  
     label = XmCreateLabel(dialog, "label", NULL, 0);
     XtVaSetValues(label,
     	XmNheight, 100,
     	XmNrecomputeSize, False,
     	NULL);
     XtManageChild(label);
  XtManageChild (dialog);


  XtRealizeWidget (toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  219,  392,  214,  212, 0,0,0, /* MyDialog */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  192,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,  148,  214,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  160,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   75,  160,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  139,  160,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   38,  192,  100, 0,0,0, /* label */
   CWWidth | CWHeight            ,  219,  392,  214,  200, 0,0,0, /* MyDialog */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  192,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,  136,  214,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  148,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   75,  148,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  139,  148,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   38,  192,   88, 0,0,0, /* label */
   CWWidth | CWHeight            ,  219,  392,  214,  100, 0,0,0, /* MyDialog */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  192,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   48,  214,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   60,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   75,   60,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  139,   60,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   38,  192,    1, 0,0,0, /* label */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
LessTifTestWaitForIt(toplevel);
LessTifTestResizeWidget(toplevel, 214, 200);
PrintDetails(toplevel, Expected);
LessTifTestWaitForIt(toplevel);
LessTifTestResizeWidget(toplevel, 214, 100);
PrintDetails(toplevel, Expected);
LessTifTestWaitForIt(toplevel);
}
  LessTifTestMainLoop(toplevel);

  exit (0);
}
