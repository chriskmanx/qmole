/* $Header: /cvsroot/lesstif/lesstif/test/Xm/frame/test2.c,v 1.7 2002/01/12 15:02:45 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#ifdef LESSTIF_VERSION
#include <XmI/MacrosI.h> 
#endif
#include <Xm/FrameP.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one, two, three, four, five;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Frame", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, toplevel,
				NULL);

  two = XtVaCreateManagedWidget("OuterLabel", xmLabelWidgetClass, one,
				XmNchildType, XmFRAME_TITLE_CHILD,
				XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                                XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
				NULL);

  four = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, one,
				 XmNshadowType, XmSHADOW_IN,
				 XmNchildType, XmFRAME_WORKAREA_CHILD,
				 NULL);

  three = XtVaCreateManagedWidget("three", xmTextFieldWidgetClass, four,
				  XmNchildType, XmFRAME_WORKAREA_CHILD,
				  NULL);

  five = XtVaCreateManagedWidget("InnerLabel", xmLabelWidgetClass, four,
				 XmNchildType, XmFRAME_TITLE_CHILD,
				 XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
				 XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
				 NULL);

  XtRealizeWidget(toplevel);
#ifdef LESSTIF_VERSION
  printf("(IGNORE_IN_DIFF) PC: %d\n", Frame_ProcessingConstraints(one));
#endif  
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  506,  322,  144,   68, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,   11,    0,   64,   17, 0,0,0, /* OuterLabel */
   CWWidth | CWHeight | CWX | CWY,    1,   17,  142,   50, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,    2,   17,  138,   31, 0,0,0, /* three */
   CWWidth | CWHeight | CWX | CWY,   12,    0,   64,   17, 0,0,0, /* InnerLabel */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

  exit(0);
}
