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
		"*.geometrySlop: 2",
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
  	XmNtopAttachment, XmATTACH_POSITION,
  	XmNtopPosition, 80,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNbottomPosition, 100,
  	NULL);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   72,   118, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,    94,   70,   17, 0,0,0, /* BottomLabel */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(Shell, Expected);
}
LessTifTestMainLoop(Shell);
  exit(0);
}
