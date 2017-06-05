/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test6.c,v 1.8 2002/05/01 15:39:21 amai Exp $ */
/*
 * Geometry should look like Danny's phone tool.
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeBG.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/List.h>

#include "../../common/Test.h"


void Quit(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Suicide ... \n");
	exit(0);
}

void Focus(Widget w, XtPointer client, XtPointer call)
{
	char		*inout = (char *)client;
	static int	n = 0;

	fprintf(stderr, "Widget %s : focus %s\n", XtName(w), inout);

	if (strcmp(inout, "in") == 0)
		n++;
	else
		n--;
	if (n >= 2) {
#if 1
		fprintf(stderr, "Two widgets have focus\n");
#else
		abort("help");
#endif
	}
}

int
main(int argc, char **argv)
{
  Widget	toplevel, form, mb, tf, sl, st, cb, menu, pb;
  XtAppContext	app;
  Arg		al[10];
  int		ac;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Phone", NULL, 0, &argc, argv, NULL, NULL);

  form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
		XmNwidth,		450,
		XmNheight,		300,
		XmNresizable,		True,
		XmNfractionBase,	3,
	NULL);

  mb = XtVaCreateManagedWidget("mb", xmRowColumnWidgetClass, form,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		0,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		0,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNrightOffset,		0,
		XmNrowColumnType,	XmMENU_BAR,
	NULL);

  tf = XtVaCreateManagedWidget("tf", xmTextFieldWidgetClass, form,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopOffset,		0,
		XmNtopWidget,		mb,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		0,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNrightOffset,		0,
	NULL);

  ac = 0;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNtopWidget, tf); ac++;
  XtSetArg(al[ac], XmNtopOffset, 0); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNleftOffset, 0); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_POSITION); ac++;
  XtSetArg(al[ac], XmNrightPosition, 1); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 0); ac++;
  sl = XmCreateScrolledList(form, "sl", al, ac);
  XtManageChild(sl);

  ac = 0;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNtopWidget, tf); ac++;
  XtSetArg(al[ac], XmNtopOffset, 0); ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(al[ac], XmNleftWidget, XtParent(sl)); ac++;
  XtSetArg(al[ac], XmNleftOffset, 0); ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNrightOffset, 0); ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
  XtSetArg(al[ac], XmNbottomOffset, 0); ac++;
  st = XmCreateScrolledText(form, "st", al, ac);
  XtManageChild(st);

  ac = 0;
  menu = XmCreatePulldownMenu(mb, "menu", al, ac);

  cb = XtVaCreateManagedWidget("cb", xmCascadeButtonGadgetClass, mb,
		XmNsubMenuId,	menu,
	NULL);

  pb = XtVaCreateManagedWidget("quit", xmPushButtonGadgetClass, menu,
	NULL);
  XtAddCallback(pb, XmNactivateCallback, Quit, NULL);

  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,		0,	0,	450,	300,	0,0,0,	/* Form */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	450,	31,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	5,	5,	28,	21,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	31,	450,	31,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	62,	150,	238,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	150,	238,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	150,	62,	300,	238,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	223,	300,	15,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	300,	219,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	0,	0,	0,0,0,	/* two */},
};

  PrintDetails(toplevel, Expected);
  }

  XtAddCallback(st, XmNfocusCallback, Focus, "in");
  XtAddCallback(st, XmNlosingFocusCallback, Focus, "out");
  XtAddCallback(tf, XmNfocusCallback, Focus, "in");
  XtAddCallback(tf, XmNlosingFocusCallback, Focus, "out");

      LessTifTestMainLoop(toplevel);

  exit(0);
}
