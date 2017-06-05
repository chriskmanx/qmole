/** test12 **/

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

  XtVaSetValues(popup,
		XmNmenuPost, "<Button3>",
		NULL);

  XtVaSetValues(popup,
		XmNmenuPost, "<Btn3>",
		NULL);

  XtVaSetValues(popup,
		XmNmenuPost, "Button3>",
		NULL);

  XtVaSetValues(popup,
		XmNmenuPost, "<Button3",
		NULL);

  /* sgi's motif says this is invalid... hmmm */
  XtVaSetValues(popup,
		XmNmenuPost, "<Button3Down>",
		NULL);

  XtVaSetValues(popup,
		XmNmenuPost, "<Btn3Down>",
		NULL);

#if 0
  /* this causes sgi's motif to dump core! 
     I don't think we should duplicate that one :) -- Chris */
  XtVaSetValues(popup,
		XmNmenuPost, "<Btn3Don>",
		NULL);
#endif

  XtVaSetValues(popup,
		XmNmenuPost, "<Btn6Down>",
		NULL);

  XtVaGetValues(popup,
		XmNmenuPost, &menuPost,
		NULL);

  printf ("MenuPost is '%s'\n", menuPost);

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
  	CWWidth | CWHeight,		0,	0,	34,	17,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	102,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	201,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	44,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	102,	44,	96,	38,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
  XtAppMainLoop(theApp);
  */
  exit(0);
}
