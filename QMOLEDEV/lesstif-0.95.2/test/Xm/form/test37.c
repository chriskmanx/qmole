#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>


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
  XtVaSetValues(Form,
  	NULL);

  TopLabel = XmCreateLabel(Form,"TopLabel",NULL,0);
  XtVaSetValues(TopLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNtopOffset, -25,
  	NULL);
  XtManageChild(TopLabel);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	72,	13,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	-25,	52,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	-6,	70,	17,	0,0,0,	/* two */
};

  PrintDetails(Shell, Expected);
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
