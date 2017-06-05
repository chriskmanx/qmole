/* Header$ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>


static char *FallBack[] = {
		"*.geometrySlop: 0",
		"*InsideLabel.background: red",
		"*BottomLabel.background: green",
		"*Form.background: salmon",
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget BottomLabel;
  Widget Form1;
  Widget InsideLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(BottomLabel);

  Form1 = XmCreateForm(Form,"Form1",NULL,0);
  InsideLabel = XmCreateLabel(Form1,"InsideLabel",NULL,0);
  XtVaSetValues(InsideLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(InsideLabel);
  XtVaSetValues(Form1,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, BottomLabel,
  	NULL);
  XtManageChild(Form1);

  XtVaSetValues(Form,
  	XmNmarginWidth, 50,
  	NULL);
  XtManageChild(Form);

  XtRealizeWidget(Shell);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  120,   34, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,   50,   17,   70,   17, 0,0,0, /* BottomLabel */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   70,   17, 0,0,0, /* Form1 */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   70,   17, 0,0,0, /* InsideLabel */ 
    };
    PrintDetails(Shell,Expected);
};
  LessTifTestMainLoop(Shell);
  exit(0);
}
