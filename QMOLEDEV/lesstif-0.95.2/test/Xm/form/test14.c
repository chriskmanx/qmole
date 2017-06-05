/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test14.c,v 1.6 2001/05/16 09:19:46 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

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
  Widget MiddleLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  TopLabel = XmCreateLabel(Form,"TopLabel",NULL,0);
  XtVaSetValues(TopLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(TopLabel);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(BottomLabel);

  MiddleLabel = XmCreateLabel(Form,"MiddleLabel",NULL,0);
  XtVaSetValues(MiddleLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLabel,
  	XmNtopOffset, 1,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, BottomLabel,
  	XmNbottomOffset, 1,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(MiddleLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	72,	59,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	52,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	40,	70,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	20,	70,	17,	0,0,0,	/* two */

  	CWWidth | CWHeight,		0,	0,	144,	118,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	52,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	99,	70,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	20,	142,	76,	0,0,0,	/* two */
};

  PrintDetails(Shell, Expected);
      LessTifTestWaitForIt(Shell);
      LessTifTestResizeWidget(Shell, 144, 118);
  PrintDetails(Shell, Expected);
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
