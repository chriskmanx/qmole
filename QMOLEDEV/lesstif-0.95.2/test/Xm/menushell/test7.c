/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/menushell/test7.c,v 1.4 1998/09/29 17:04:07 jon Exp $
 */
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>

#include <stdio.h>

Widget label, toplevel, popup, button1, button2;
XtAppContext theApp;

void 
buttonPressEventHandler(Widget w, 
			XtPointer client_data,
			XEvent *e, 
			Boolean *cont)
{
  XButtonEvent *be = (XButtonEvent*)e;

  printf ("Inside buttonPressEventHandler\n");

  if (be->button == 3)
  {
    printf ("  It was button 3\n");
    XmMenuPosition(popup, be);
    
    XtManageChild(popup);
  }
}

int
main(int argc,
     char **argv)
{
  String menuPost = NULL;

  toplevel = XtAppInitialize(&theApp,
			     "test12",
			     NULL, 0,
			     &argc, argv,
			     NULL, NULL, 0);

  label = XtVaCreateManagedWidget("label",
				  xmLabelWidgetClass,
				  toplevel,
				  NULL);

  XtAddEventHandler(label, ButtonPressMask, False,
		    buttonPressEventHandler, NULL);

  popup = XmCreatePopupMenu(label,
			    "popup",
			    NULL, 0);

  XtVaSetValues(popup, XmNwhichButton, 3, NULL);

  button1 = XtVaCreateManagedWidget("button1",
				    xmPushButtonWidgetClass,
				    popup,
				    NULL);

  button2 = XtVaCreateManagedWidget("button2",
				    xmPushButtonWidgetClass,
				    popup,
				    NULL);

  XtRealizeWidget(toplevel);

  {
    static XtWidgetGeometry Expected[] = {
      CWWidth | CWHeight            ,   57,   73,   34,   17, 0,0,0, /* label */
    };
    /* toplevel should be replaced with to correct applicationShell */
    PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);

  exit(0);
}
