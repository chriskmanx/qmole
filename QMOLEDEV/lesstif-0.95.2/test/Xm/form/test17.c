/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test17.c,v 1.6 2001/05/16 09:19:46 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>

#include "../../common/Test.h"


static char *FallBack[] = {
		"*.borderWidth: 1",
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget TopLabel;
  Widget BottomLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  TopLabel = XmCreateLabel(Form,"TopLabel",NULL,0);
  XtVaSetValues(TopLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	NULL);
  XtManageChild(TopLabel);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLabel,
  	XmNtopOffset, 1,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNbottomOffset, 2,
  	NULL);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	72,	41,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	52,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	20,	70,	17,	0,0,0,	/* two */

  	CWWidth | CWHeight,		0,	0,	144,	76,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	52,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	20,	70,	52,	0,0,0,	/* two */
};

  PrintDetails(Shell, Expected);
      LessTifTestWaitForIt(Shell);
      LessTifTestResizeWidget(Shell, 144, 76);
  PrintDetails(Shell, Expected);
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
