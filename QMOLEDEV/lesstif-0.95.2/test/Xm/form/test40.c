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
  Widget BottomLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);
  XtVaSetValues(Form,
  	XmNmarginHeight, 5,
  	NULL);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNwidth, 70,
  	XmNheight, 17,
  	XmNrecomputeSize, False,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNtopPosition, 80,
  	XmNbottomAttachment, XmATTACH_POSITION,
  	XmNbottomPosition, 80,
  	NULL);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);

/* Note: the following values are the result of
 * querying the current geometry.
 */

{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   72,   30, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,    5,   70,   17, 0,0,0, /* BottomLabel */ 
    };
    PrintDetails(Shell,Expected);
};
LessTifTestMainLoop(Shell);
  exit(0);
}
