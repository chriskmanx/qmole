/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test23.c,v 1.5 2001/05/16 09:23:15 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>

#include "../../common/Test.h"



static String FallBack[] = {
	"*borderWidth: 1",
NULL};

int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget Form;
  Widget LeftLabel;
  Widget RightLabel;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "test23", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(toplevel, "Form", NULL, 0);
  XtVaSetValues(Form,
	XmNresizePolicy, XmRESIZE_GROW,
#if 0
  	XmNwidth, 200,
#else
  	XmNheight, 200,
#endif
  	NULL);

  RightLabel = XmCreateLabel(Form, "RightLabel", NULL,0);
  LeftLabel = XmCreateLabel(Form, "LeftLabel", NULL,0);

#if 0
  XtVaSetValues(RightLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftOffset, 10,
  	XmNleftWidget, LeftLabel,
  	XmNwidth, 90,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNrightOffset, 10,
  	NULL);
  XtManageChild(RightLabel);

  XtVaSetValues(LeftLabel,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNleftOffset, 50,
  	XmNwidth, 100,
  	NULL);
  XtManageChild(LeftLabel);
#else
  XtVaSetValues(RightLabel,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopOffset, 10,
  	XmNtopWidget, LeftLabel,
  	XmNheight, 90,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNbottomOffset, 10,
  	NULL);
  XtManageChild(RightLabel);

  XtVaSetValues(LeftLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNtopOffset, 50,
  	XmNheight, 100,
  	NULL);
  XtManageChild(LeftLabel);
#endif

  XtManageChild(Form);

  XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	66,	200,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	162,	64,	26,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	50,	58,	100,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
      LessTifTestMainLoop(toplevel);

  exit(0);
}
