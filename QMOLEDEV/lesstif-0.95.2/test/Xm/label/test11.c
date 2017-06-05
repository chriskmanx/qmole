/* $Header */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>

#include "../../common/Test.h"


static char *FallBack[] = {
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget BottomLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  BottomLabel = XmCreateLabel(Shell,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNwidth, 200,
  	XmNheight, 200,
  	NULL);
  XtManageChild(BottomLabel);

  XtRealizeWidget(Shell);
  LessTifTestWaitForIt(Shell);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   51,   73,  200,  200, 0,0,0, /* BottomLabel */
   CWWidth | CWHeight | CWX | CWY,   51,   73,  200,  200, 0,0,0, /* BottomLabel */ 
    };
    PrintDetails(Shell,Expected);
};
  XtVaSetValues(BottomLabel,
  	XmNalignment, XmALIGNMENT_END,
  	NULL);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   51,   73,  200,  200, 0,0,0, /* BottomLabel */
   CWWidth | CWHeight            ,   51,   73,  200,  200, 0,0,0, /* BottomLabel */ 
    };
    PrintDetails(Shell,Expected);
};
  LessTifTestMainLoop(Shell);
  exit(0);
}
