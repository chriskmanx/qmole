/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test21.c,v 1.6 2001/05/16 09:23:15 amai Exp $ */

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
  Widget Frame;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  TopLabel = XmCreateLabel(Form,"TopLabel",NULL,0);
  XtVaSetValues(TopLabel,
  	XmNheight, 100,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(TopLabel);

  Frame = XmCreateFrame(Form,"Frame",NULL,0);
  XtManageChild(Frame);
  BottomLabel = XmCreateLabel(Frame,"BottomLabel",NULL,0);
  XtVaSetValues(Frame,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, TopLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNbottomWidget, TopLabel,
  	XmNtopOffset, 10,
  	NULL);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	132,	102,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	52,	100,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	54,	77,	76,	23,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	2,	2,	70,	17,	0,0,0,	/* two */

  	CWWidth | CWHeight,		0,	0,	150,	200,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	52,	198,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	54,	175,	76,	23,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	2,	2,	70,	17,	0,0,0,	/* two */
};

  PrintDetails(Shell, Expected);
      LessTifTestWaitForIt(Shell);
      LessTifTestResizeWidget(Shell, 150, 200);
  PrintDetails(Shell, Expected);
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
