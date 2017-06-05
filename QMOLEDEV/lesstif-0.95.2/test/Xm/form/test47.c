#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>


static char *FallBack[] = {
		"*.borderWidth: 0",
		"*.geometrySlop: 0",
		"*TopLeftLabel.background: red",
		"*TopRightLabel.background: green",
		"*BottomLeftLabel.background: yellow",
		"*BottomRightLabel.background: red",
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget TopTopLeftLabel;
  Widget TopLeftLabel;
  Widget TopRightLabel;
  Widget BottomLeftLabel;
  Widget BottomRightLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  TopTopLeftLabel = XmCreateLabel(Form,"TopTopLeftLabel",NULL,0);
  TopLeftLabel = XmCreateLabel(Form,"TopLeftLabel",NULL,0);
  TopRightLabel = XmCreateLabel(Form,"TopRightLabel",NULL,0);
  BottomLeftLabel = XmCreateLabel(Form,"BottomLeftLabel",NULL,0);
  BottomRightLabel = XmCreateLabel(Form,"BottomRightLabel",NULL,0);
  XtVaSetValues(TopTopLeftLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNleftOffset, 6,
  	NULL);
  XtVaSetValues(TopLeftLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopTopLeftLabel,
  	XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNleftWidget, TopTopLeftLabel,
  	XmNleftOffset, 8,
  	NULL);
  XtVaSetValues(TopRightLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopTopLeftLabel,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, TopLeftLabel,
  	XmNleftOffset, 10,
  	NULL);
  XtVaSetValues(BottomLeftLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopRightLabel,
  	XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNleftWidget, TopRightLabel,
  	NULL);
  XtVaSetValues(BottomRightLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLeftLabel,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, BottomLeftLabel,
  	XmNleftOffset, 12,
  	NULL);
  XtManageChild(TopTopLeftLabel);
  XtManageChild(TopLeftLabel);
  XtManageChild(TopRightLabel);
  XtManageChild(BottomLeftLabel);
  XtManageChild(BottomRightLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	72,	37,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	70,	17,	0,0,0,	/* two */

  	CWWidth | CWHeight,		0,	0,	144,	76,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	0,	19,	70,	17,	0,0,0,	/* two */
};

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  306,   51, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    6,    0,   94,   17, 0,0,0, /* TopTopLeftLabel */
   CWWidth | CWHeight | CWX | CWY,   14,   17,   76,   17, 0,0,0, /* TopLeftLabel */
   CWWidth | CWHeight | CWX | CWY,  100,   17,   82,   17, 0,0,0, /* TopRightLabel */
   CWWidth | CWHeight | CWX | CWY,  100,   34,   94,   17, 0,0,0, /* BottomLeftLabel */
   CWWidth | CWHeight | CWX | CWY,  206,   34,  100,   17, 0,0,0, /* BottomRightLabel */ 
    };
    PrintDetails(Shell,Expected);
};
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
