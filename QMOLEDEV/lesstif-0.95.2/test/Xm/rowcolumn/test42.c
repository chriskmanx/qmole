/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test42.c,v 1.3 2001/05/15 14:46:10 amai Exp $ */
#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/Separator.h>

#include "../../common/Test.h"

static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget BottomLabel;
  Widget TopLabel;
  Widget Sep;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateRowColumn(Shell,"RC",NULL,0);
  XtVaSetValues(Form,
  	NULL);

  BottomLabel = XmCreateLabel(Form,"TopLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNalignment, XmALIGNMENT_CENTER,
  	XmNwidth, 52,
  	XmNheight, 17,
  	XmNrecomputeSize, False,
  	NULL);
  XtManageChild(BottomLabel);

  Sep = XmCreateSeparator(Form, "Sep", NULL, 0);
  XtManageChild(Sep);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNalignment, XmALIGNMENT_CENTER,
  	XmNwidth, 70,
  	XmNheight, 30,
  	XmNrecomputeSize, False,
  	NULL);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  673,  241,   76,   61, 0,0,0, /* RC */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   70,   17, 0,0,0, /* TopLabel */
   CWWidth | CWHeight | CWX | CWY,    3,   23,   70,    2, 0,0,0, /* Sep */
   CWWidth | CWHeight | CWX | CWY,    3,   28,   70,   30, 0,0,0, /* BottomLabel */

   CWWidth | CWHeight            ,  673,  241,  144,   61, 0,0,0, /* RC */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   17, 0,0,0, /* TopLabel */
   CWWidth | CWHeight | CWX | CWY,    3,   23,  138,    2, 0,0,0, /* Sep */
   CWWidth | CWHeight | CWX | CWY,    3,   28,  138,   30, 0,0,0, /* BottomLabel */

   CWWidth | CWHeight            ,  673,  241,   80,   61, 0,0,0, /* RC */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   74,   17, 0,0,0, /* TopLabel */
   CWWidth | CWHeight | CWX | CWY,    3,   23,   74,    2, 0,0,0, /* Sep */
   CWWidth | CWHeight | CWX | CWY,    3,   28,   74,   30, 0,0,0, /* BottomLabel */
};

  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);
  LessTifTestResizeWidget(Shell, 144, 61);
  PrintDetails(Shell, Expected);
  LessTifTestWaitForIt(Shell);
  LessTifTestResizeWidget(Shell, 80, 61);
  PrintDetails(Shell, Expected);
}

      LessTifTestMainLoop(Shell);
  exit(0);
}
