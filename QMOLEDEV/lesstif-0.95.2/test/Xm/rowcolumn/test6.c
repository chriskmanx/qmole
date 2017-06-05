/** test6 -- vertical column layout of gadgets with two columns and XmNadjustLast = False.

    resulting layout should be:

    button1 button4
    button2 button5
    button3

    the layout should not change when the window is resized
**/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>

String fallback[] = {
	"*button3.labelString:	This sucker is long",
	NULL
};

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test6", NULL, 0,
				 &argc, argv, fallback, NULL);

    rc = XtVaCreateManagedWidget("rowcolumn",
				 xmRowColumnWidgetClass,
				 toplevel,
				 XmNorientation, XmVERTICAL,
				 XmNpacking, XmPACK_COLUMN,
				 XmNnumColumns, 2,
				 XmNadjustLast, False,
				 NULL);

    button1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonGadgetClass,
				      rc,
				      NULL);

    button2 = XtVaCreateManagedWidget("button2",
				      xmPushButtonGadgetClass,
				      rc,
				      NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonGadgetClass,
				      rc,
				      NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmPushButtonGadgetClass,
				      rc,
				      NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmPushButtonGadgetClass,
				      rc,
				      NULL);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	261,	87,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	126,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	126,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	59,	126,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	132,	3,	126,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	132,	31,	126,	25,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}

