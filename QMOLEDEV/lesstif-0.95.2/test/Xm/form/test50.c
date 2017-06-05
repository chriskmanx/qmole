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
		"*.geometrySlop: 1",
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

  Form = XmCreateForm(Shell,"Form",NULL,0);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNy, 50,
  	XmNtopAttachment, XmATTACH_SELF,
  	NULL);
  XtManageChild(BottomLabel);

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
   CWWidth | CWHeight            ,   50,   50,   72,   67, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,   48,   70,   17, 0,0,0, /* BottomLabel */ 
    };
    PrintDetails(Shell,Expected);
};
      LessTifTestWaitForIt(Shell);
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
