/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test55.c,v 1.5 2001/05/16 09:37:13 amai Exp $
   simulate an openDX interactor on the control panel switching
   from vertical to horizontal then back.
*/

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
  Widget TopLabel, BottomLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);
  XtVaSetValues(Shell,
  	XmNallowShellResize, True,
  	NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  TopLabel = XmCreateLabel(Form,"TopLabel",NULL,0);
  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(TopLabel,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNheight, 40,
  	NULL);
  XtManageChild(TopLabel);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
  {

  
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   72,   61, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   70,   17, 0,0,0, /* TopLabel */
   CWWidth | CWHeight | CWX | CWY,    0,   19,   70,   40, 0,0,0, /* BottomLabel */
   CWWidth | CWHeight            ,   57,   75,  126,   42, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   52,   40, 0,0,0, /* TopLabel */
   CWWidth | CWHeight | CWX | CWY,   54,    0,   70,   40, 0,0,0, /* BottomLabel */
   CWWidth | CWHeight            ,   57,   75,   72,   61, 0,0,0, /* Form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   70,   17, 0,0,0, /* TopLabel */
   CWWidth | CWHeight | CWX | CWY,    0,   19,   70,   40, 0,0,0, /* BottomLabel */ 
    };
    PrintDetails(Shell,Expected);
  LessTifTestWaitForIt(Shell);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(TopLabel,
  	XmNrightAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, TopLabel,
  	NULL);
  XtVaSetValues(TopLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	NULL);
  
    PrintDetails(Shell,Expected);
  LessTifTestWaitForIt(Shell);

  XtVaSetValues(TopLabel,
  	XmNbottomAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	XmNleftAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(TopLabel,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLabel,
  	XmNleftAttachment, XmATTACH_FORM,
  	NULL);
  
    PrintDetails(Shell,Expected);
  LessTifTestWaitForIt(Shell);
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
