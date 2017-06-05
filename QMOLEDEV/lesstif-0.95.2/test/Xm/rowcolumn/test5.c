/** test1 -- vertical tight layout of gadgets with XmNadjustLast = True.

    resulting layout should be something like this:

    button1
    button2
    button3
    button4
    button5

    with the gadgets being as wide as the window.

    resizing smaller will yield something like this:

    button1 button4
    button2 button5
    button3

    with button4 and button5 taking up any remaining width.
**/

#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushBG.h>

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test5", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rowcolumn",
				 xmRowColumnWidgetClass,
				 toplevel,
				 XmNorientation, XmVERTICAL,
				 XmNpacking, XmPACK_TIGHT,
				 XmNadjustLast, True,
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
  	CWWidth | CWHeight,		0,	0,	60,	143,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	54,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	54,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	59,	54,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	87,	54,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	115,	54,	25,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


