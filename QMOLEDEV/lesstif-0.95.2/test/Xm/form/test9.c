/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test9.c,v 1.8 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>

#include "../../common/Test.h"


void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

void Longer(Widget w, XtPointer client, XtPointer call)
{
	Widget	l = (Widget)client;
	XmString	x, y;

	fprintf(stderr, "Longer: resizing the label ...\n");

	XtVaGetValues(l, XmNlabelString, &x, NULL);
	y = XmStringConcat(x, x);
	XtVaSetValues(l, XmNlabelString, y, NULL);
	XmStringFree(y);
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc1, rc2, rc3, w;
    Arg		al[10];
    int		ac, i, j;
    Widget	f, b, l;
    XmString	x;

    toplevel = XtVaAppInitialize(&theApp, "rc-test1", NULL, 0,
				 &argc, argv, NULL, NULL);

/* toplevel */
    ac = 0;
    XtSetArg(al[ac], XmNisAligned, False); ac++;
    rc1 = XmCreateRowColumn(toplevel, "top-rc", al, ac);
    XtManageChild(rc1);

    w = XmCreatePushButton(rc1, "Quit", NULL, 0);
    XtManageChild(w);
    XtAddCallback(w, XmNactivateCallback, Quit, 0);

/* Copy of xephem "BottomRC" */
    ac = 0;
    XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
    rc2 = XmCreateRowColumn(rc1, "rc", al, ac);
    XtManageChild(rc2);

    for (j=0; j<3; j++) {
	char	s[20];

	sprintf(s, "rc-%d", j+1);

	ac = 0;
	XtSetArg(al[ac], XmNisAligned, False); ac++;
	XtSetArg(al[ac], XmNadjustLast, False); ac++;
	XtSetArg(al[ac], XmNspacing, 3); ac++;
	rc3 = XmCreateRowColumn(rc2, s, al, ac);
	XtManageChild(rc3);

	for (i=0; i<3; i++) {
	    char	s[20];

	    sprintf(s, "button-%d", i+1);
	    if (i == 0 && j == 0) {

		f = XmCreateForm(rc3, "form", NULL, 0);
		XtManageChild(f);

		ac = 0;
		XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
		XtSetArg(al[ac], XmNalignment, XmALIGNMENT_END); ac++;
		b = XmCreatePushButton(f, "button", al, ac);
		XtManageChild(b);

		ac = 0;
		XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
		XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
		XtSetArg(al[ac], XmNrightWidget, b); ac++;
		XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
		l = XmCreateLabel(f, "MainLabel", al, ac);
		XtManageChild(l);

		x = XmStringCreateSimple("This is the label");
		XtVaSetValues(l, XmNlabelString, x, NULL);
		XmStringFree(x);

		XtAddCallback(b, XmNactivateCallback, Longer, l);
	    } else {
		(void) XtVaCreateManagedWidget(s, xmPushButtonWidgetClass, rc3, NULL);
	    }
	}
    }

    XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,		0,	0,	310,	127,	0,0,0,	/* Form */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	3,	304,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	31,	304,	93,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	3,	160,	87,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	3,	154,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	106,	0,	48,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	106,	17,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	31,	154,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	59,	154,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	166,	3,	66,	87,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	3,	60,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	31,	60,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	59,	60,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	235,	3,	66,	87,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	3,	60,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	31,	60,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	3,	59,	60,	25,	0,0,0,	/* two */},
};

  PrintDetails(toplevel, Expected);
  }
        LessTifTestMainLoop(toplevel);
    exit(0);
}
