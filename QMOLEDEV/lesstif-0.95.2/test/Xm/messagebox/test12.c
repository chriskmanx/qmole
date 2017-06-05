#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Label.h>

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
     xmstr = XmStringCreateLtoR("Hello World\n\nIf you hit OK on this dialog, another one"
				      "\nshould appear, positioned to the lower right of this one.",
				      XmFONTLIST_DEFAULT_TAG);
  
  
     XtVaSetValues(dialog, XmNmessageString, xmstr, NULL);
  
#if 1
     rc = XmCreateRowColumn(dialog, "rc", NULL, 0);
#else
     rc = XmCreateForm(dialog, "rc", NULL, 0);
#endif
     label = XmCreateLabel(rc, "label", NULL, 0);
     XtVaSetValues(label,
     	XmNborderWidth, 1,
     	NULL);
     XtManageChild(label);
     XtManageChild(rc);
  XtManageChild (dialog);


  XtRealizeWidget (toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  368,  176, 0,0,0, /* MyDialog */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  346,   56, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,  112,  368,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  124,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  152,  124,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  293,  124,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   77,  346,   25, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  338,   17, 0,0,0, /* label */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

  exit (0);
}
