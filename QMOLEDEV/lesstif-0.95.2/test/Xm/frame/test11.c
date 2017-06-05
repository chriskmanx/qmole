/* $Header: /cvsroot/lesstif/lesstif/test/Xm/frame/test11.c,v 1.1 2004/10/16 16:44:18 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>

int
main(int argc, char **argv)
{
  Widget toplevel, frame1, frame2, inner, outer, one, two, three;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Frame", NULL, 0, &argc, argv, NULL, NULL);

  frame1 = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, toplevel,
				NULL);

  outer = XtVaCreateManagedWidget("OuterLabel", xmLabelGadgetClass, frame1,
	XmNchildType, XmFRAME_TITLE_CHILD,
	XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
        XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
	NULL);

  frame2 = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, frame1,
	XmNshadowType, XmSHADOW_IN,
	XmNchildType, XmFRAME_WORKAREA_CHILD,
	NULL);

  one = XtVaCreateManagedWidget("one", xmTextWidgetClass, frame2,
	XmNchildType, XmFRAME_WORKAREA_CHILD,
	NULL);

  inner = XtVaCreateManagedWidget("InnerLabel", xmLabelWidgetClass, frame2,
	XmNchildType, XmFRAME_TITLE_CHILD,
	XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
	XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
	NULL);
#if 0
  /* Don't specify child type */
  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, frame2,
	XmNwidth,	100,
	XmNheight,	200,
	NULL);
#endif
  /* Do specify child type */
  three = XtVaCreateManagedWidget("three", xmPushButtonWidgetClass, frame2,
  	XmNchildType,	XmFRAME_GENERIC_CHILD,
	XmNwidth,	200,
	XmNheight,	100,
	NULL);

  XtRealizeWidget(toplevel);
  
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
