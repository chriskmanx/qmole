#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>

static char *FallBack[] = {
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget BottomLabel, TopLabel, MiddleLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  TopLabel = XmCreateLabel(Form,"TopLabelTopLabel",NULL,0);
  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(TopLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, BottomLabel,
  	XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNleftWidget, BottomLabel,
  	XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
  	XmNrightWidget, BottomLabel,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(TopLabel);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   70,   34, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   70,   17, 0,0,0, /* TopLabelTopLabel */
   CWWidth | CWHeight | CWX | CWY,    0,   17,   70,   17, 0,0,0, /* BottomLabel */ 
    };
    PrintDetails(Shell,Expected);
};
      LessTifTestMainLoop(Shell);

  exit(0);
}
