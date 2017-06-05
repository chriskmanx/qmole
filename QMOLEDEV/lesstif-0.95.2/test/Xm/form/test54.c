/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test54.c,v 1.4 2001/05/16 09:37:13 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>

#include "../../common/Test.h"


static char *FallBack[] = {
	        "*StickyMsgTlFo.background: red",
	        "*StickyMsgTlSp.background: green",
	        "*Bu1.background: yellow",
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget StickyMsgTlBase;
  Widget StickyMsgTlFo;
  Widget StickyMsgTlSp;
  Widget Bu1;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);
  XtVaSetValues(Shell,
  	XmNallowShellResize, True,
  	NULL);

  StickyMsgTlBase = XmCreateForm(Shell,"StickyMsgTlBase",NULL,0);

  StickyMsgTlFo = XmCreateLabel(StickyMsgTlBase,"StickyMsgTlFo",NULL,0);
  StickyMsgTlSp = XmCreateLabel(StickyMsgTlBase,"StickyMsgTlSp",NULL,0);

  XtVaSetValues(StickyMsgTlFo,
  	XmNrecomputeSize, False,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_WIDGET,
  	XmNbottomWidget, StickyMsgTlSp,
  	NULL);

  XtVaSetValues(StickyMsgTlSp,
  	XmNrecomputeSize, False,
  	XmNtopAttachment, XmATTACH_NONE,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_FORM,
  	NULL);

  Bu1 = XmCreatePushButton(StickyMsgTlBase,"Bu1",NULL,0);
  XtVaSetValues(Bu1,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNleftOffset, 5,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, StickyMsgTlSp,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNshowAsDefault,1,
  	XmNtopOffset, 5,
  	XmNbottomOffset, 5,
  	NULL);
  XtVaSetValues(Bu1,
  	XmNdefaultButtonShadowThickness, 1,
  	NULL);
  XtVaSetValues(Bu1,
  	XmNshowAsDefault, 1,
  	NULL);

  XtVaSetValues(StickyMsgTlBase,
  	XmNdefaultButton, Bu1,
  	NULL);
  XtVaSetValues(StickyMsgTlSp,
  	XmNbottomOffset, 48,
  	NULL);
  XtManageChild(StickyMsgTlFo);
  XtManageChild(StickyMsgTlSp);
  XtManageChild(Bu1);

  XtManageChild(StickyMsgTlBase);

  XtVaSetValues(StickyMsgTlBase,
  	XmNwidth, 420,
  	XmNheight, 188,
  	NULL);
  XtRealizeWidget(Shell);
  LessTifTestWaitForIt(Shell);
  
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  420,  188, 0,0,0, /* StickyMsgTlBase */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  420,  123, 0,0,0, /* StickyMsgTlFo */
   CWWidth | CWHeight | CWX | CWY,    0,  123,  420,   17, 0,0,0, /* StickyMsgTlSp */
   CWWidth | CWHeight | CWX | CWY,    5,  145,   42,   38, 0,0,0, /* Bu1 */
};
    PrintDetails(Shell,Expected);
};
  LessTifTestMainLoop(Shell);
  exit(0);
}
