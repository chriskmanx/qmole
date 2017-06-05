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
  Widget MiddleTopLabel;
  Widget MiddleBottomLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  TopLabel = XmCreateLabel(Form,"TopLabel",NULL,0);
  MiddleTopLabel = XmCreateLabel(Form,"MiddleTopLabel",NULL,0);
  MiddleBottomLabel = XmCreateLabel(Form,"MiddleBotLabel",NULL,0);
  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);

  XtVaSetValues(TopLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_NONE,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);

  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);

  XtVaSetValues(MiddleTopLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLabel,
  	XmNtopOffset, 1,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, MiddleBottomLabel,
  	XmNbottomOffset, 1,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);

  XtVaSetValues(MiddleBottomLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	XmNtopOffset, 1,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, BottomLabel,
  	XmNbottomOffset, 1,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);

  XtManageChild(TopLabel);
  XtManageChild(MiddleTopLabel);
  XtManageChild(MiddleBottomLabel);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	90,	79,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	88,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	20,	88,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	40,	88,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	60,	88,	17,	0,0,0,	/* two */
};

  PrintDetails(Shell, Expected);
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
