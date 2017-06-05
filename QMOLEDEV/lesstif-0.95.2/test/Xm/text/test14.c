/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test14.c,v 1.6 2001/05/16 13:10:19 amai Exp $ */

#include <stdlib.h>

#include <Xm/ScrolledW.h> 
#include <Xm/Text.h> 

#include "../../common/Test.h"

static char *fallbacks[] =
{
  "*one.editMode: XmMULTI_LINE_EDIT",
  "*one.shadowThickness: 0",
  "*one.highlightThickness: 0",
  "*one.background: red",
  "*oneSW.width: 368",
  "*oneSW.height: 368",
  "*oneSW.scrollingPolicy: XmAUTOMATIC",
    NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv,
			       fallbacks, NULL);

  one = XmCreateScrolledWindow(toplevel, "oneSW", NULL, 0);
  XtManageChild(one);
  one = XmCreateText(one, "one", NULL, 0);
  XtManageChild(one);

  XtRealizeWidget(toplevel);
  LessTifTestWaitForIt(toplevel);

  

{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  368,  368, 0,0,0, /* oneSW */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  364,  364, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  130,   23, 0,0,0, /* one */
   CWWidth | CWHeight | CWX | CWY,  368,    0,   19,  368, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  368,  368,   19, 0,0,0, /* HorScrollBar */ 

   CWWidth | CWHeight            ,   56,   72,  368,  368, 0,0,0, /* oneSW */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  364,  364, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  130,   49, 0,0,0, /* one */
   CWWidth | CWHeight | CWX | CWY,  368,    0,   19,  368, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  368,  368,   19, 0,0,0, /* HorScrollBar */ 
    };
    PrintDetails(toplevel,Expected);
    XSync(XtDisplay(toplevel), False);
    while (XtAppPending(XtWidgetToApplicationContext(toplevel)))
    {
	XtAppProcessEvent(XtWidgetToApplicationContext(toplevel), XtIMAll);
	XFlush(XtDisplay(toplevel));
    }
    XmTextSetString(one, "Line 1\nLine2\n");
    PrintDetails(toplevel,Expected);
};
LessTifTestMainLoop(toplevel);

  exit(0);
}

