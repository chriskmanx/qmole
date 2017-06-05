/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test8.c,v 1.8 2002/05/01 15:39:21 amai Exp $ */

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


void Push(Widget w, XtPointer client, XtPointer call)
{
	int		ac;
	Arg		al[5];
	Widget		fd, fr, f2, l, tf, sep, b1, b2, b3, b4, f3;
	XmString	xms;

	fd = XmCreateFormDialog(w, "formdialog", NULL, 0);
	fr = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, fd,
			XmNshadowType,		XmSHADOW_OUT,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		0,
			XmNrightOffset,		0,
		NULL);
	f2 = XtVaCreateManagedWidget ("form 2", xmFormWidgetClass, fr,
		NULL);
	xms = XmStringCreateLtoR("URL To Open: ", XmSTRING_DEFAULT_CHARSET);
	l = XtVaCreateManagedWidget("label", xmLabelWidgetClass, f2,
			XmNlabelString,		xms,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_NONE,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_NONE,
			XmNtopOffset,		14,
			XmNbottomOffset,	0,
			XmNleftOffset,		10,
			XmNrightOffset,		0,
		NULL);
	XmStringFree(xms);

	ac = 0;
	XtSetArg(al[ac], XmNwidth, 310); ac++;
	tf = XmCreateTextField(f2, "textfield", al, ac);
	XtManageChild(tf);
	XtVaSetValues(tf,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_NONE,
			XmNleftAttachment,	XmATTACH_WIDGET,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		0,
			XmNleftWidget,		l,
			XmNrightOffset,		0,
		NULL);

	sep = XmCreateSeparatorGadget(f2, "separator", NULL, 0);
	XtManageChild(sep);

	f3 = XtVaCreateManagedWidget("form3", xmFormWidgetClass, f2,
			XmNverticalSpacing,	8,
			XmNfractionBase,	4,
			XmNtopAttachment,	XmATTACH_NONE,
			XmNtopOffset,		0,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNbottomOffset,	0,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		0,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNrightOffset,		0,
		NULL);

	XtVaSetValues(sep,
			XmNtopAttachment,	XmATTACH_WIDGET,
			XmNtopOffset,		10,
			XmNtopWidget,		tf,
			XmNbottomAttachment,	XmATTACH_WIDGET,
			XmNbottomWidget,	f3,
			XmNbottomOffset,	0,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		0,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNrightOffset,		0,
		NULL);

	b1 = XtVaCreateManagedWidget("b1", xmPushButtonWidgetClass, f3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNrightAttachment,	XmATTACH_POSITION,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		8,
			XmNrightOffset,		4,
			XmNleftPosition,	0,
			XmNrightPosition,	1,
		NULL);
	b2 = XtVaCreateManagedWidget("b2", xmPushButtonWidgetClass, f3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNrightAttachment,	XmATTACH_POSITION,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		4,
			XmNrightOffset,		4,
			XmNleftPosition,	1,
			XmNrightPosition,	2,
		NULL);
	b3 = XtVaCreateManagedWidget("b3", xmPushButtonWidgetClass, f3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNrightAttachment,	XmATTACH_POSITION,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		4,
			XmNrightOffset,		4,
			XmNleftPosition,	2,
			XmNrightPosition,	3,
		NULL);
	b4 = XtVaCreateManagedWidget("b4", xmPushButtonWidgetClass, f3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			XmNleftAttachment,	XmATTACH_POSITION,
			XmNrightAttachment,	XmATTACH_POSITION,
			XmNtopOffset,		0,
			XmNbottomOffset,	0,
			XmNleftOffset,		4,
			XmNrightOffset,		8,
			XmNleftPosition,	3,
			XmNrightPosition,	4,
		NULL);

	XtManageChild(fd);
  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,		0,	0,	406,	72,	0,0,0,	/* Form */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	406,	72,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	2,	2,	402,	68,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	10,	14,	82,	17,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	92,	0,	310,	31,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	41,	402,	2,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	43,	402,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	8,	0,	89,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	105,	0,	92,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	205,	0,	93,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	306,	0,	88,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	406,	72,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	2,	2,	402,	68,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	10,	14,	82,	17,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	92,	0,	310,	31,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	41,	402,	2,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	43,	402,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	8,	0,	89,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	105,	0,	92,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	205,	0,	93,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	306,	0,	88,	25,	0,0,0,	/* two */},
};

  PrintDetails(fd, Expected);
  }
}

/*
static void
StructureNotifyHandler(Widget w, Boolean *mapped, XEvent *event, Boolean *cont)
{
	switch(event->type)
	{
	case UnmapNotify:
		*mapped = False;
		break;
	case MapNotify:
		*mapped = True;
		break;
	default:
		break;
	}
}
*/

/*
void LessTifTestPushButton(Widget w)
{
  XtCallActionProc(w, "ArmAndActivate", NULL, NULL, 0);
  XSync(XtDisplay(w), False);
  while (XtAppPending(XtWidgetToApplicationContext(w)))
  {
    XtAppProcessEvent(XtWidgetToApplicationContext(w), XtIMAll);
    XFlush(XtDisplay(w));
  }
}
*/

int
main(int argc, char **argv)
{
  Widget	top, w;
  XtAppContext	app;

  XtSetLanguageProc(NULL, NULL, NULL);

  top = XtVaAppInitialize(&app, "Form", NULL, 0, &argc, argv, NULL, NULL);

  w = XtCreateManagedWidget("Push", xmPushButtonWidgetClass, top,
	NULL, 0);
  XtAddCallback(w, XmNactivateCallback, Push, 0);

  XtRealizeWidget(top);
  LessTifTestWaitForIt(top);
  LessTifTestPushButton(w);
  LessTifTestMainLoop(top);
  exit(0);
}
