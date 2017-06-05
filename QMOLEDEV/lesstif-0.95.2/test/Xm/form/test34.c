#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>


/*
 * Cope with bugs in XmString implementation
 */
#define	XMSTRING_BAD

static String fallback[] = {
	"*XmForm.background:		blue",
	"*XmRowColumn.background:	red",
	"*XmPushButton.background:	green",
	NULL
};

void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

void Longer(Widget w, XtPointer client, XtPointer call)
{
	Widget	l = (Widget)client;
	XmString	x, y;
#ifdef	XMSTRING_BAD
	static XmString	xms;
	static int	inited = 0;
#endif

	fprintf(stderr, "Calling 'Longer' to resize the label ...\n");

#ifdef	XMSTRING_BAD
	if (! inited) {
		inited++;

		y = xms = XmStringCreateSimple("This is a long string !");
	} else {
		x = xms;
		y = xms = XmStringConcat(xms, xms);
		XmStringFree(x);
	}
#else
	XtVaGetValues(l, XmNlabelString, &x, NULL);
	y = XmStringConcat(x, x);
#endif
	XtVaSetValues(l, XmNlabelString, y, NULL);
#ifndef	XMSTRING_BAD
	XmStringFree(y);
#endif
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, bb, rc3, w;
    Arg		al[10];
    int		ac, i;
    Widget	f, b, l;
    XmString	x;

    toplevel = XtVaAppInitialize(&theApp, "rc-test1", NULL, 0,
				 &argc, argv, fallback, NULL);

/* toplevel */
    ac = 0;
    XtSetArg(al[ac], XmNwidth, 300); ac++;
    XtSetArg(al[ac], XmNheight, 300); ac++;
    bb = XmCreateForm(toplevel, "bb", al, ac);
    XtManageChild(bb);

    ac = 0;
    XtSetArg(al[ac], XmNx, 10); ac++;
    XtSetArg(al[ac], XmNy, 10); ac++;
    w = XmCreatePushButton(bb, "Quit", al, ac);
    XtManageChild(w);
    XtAddCallback(w, XmNactivateCallback, Quit, 0);

	ac = 0;
	XtSetArg(al[ac], XmNisAligned, False); ac++;
	XtSetArg(al[ac], XmNadjustLast, False); ac++;
	XtSetArg(al[ac], XmNspacing, 3); ac++;
	XtSetArg(al[ac], XmNleftOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopOffset, 10); ac++;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopWidget, w); ac++;
	rc3 = XmCreateRowColumn(bb, "rc", al, ac);
	{
	int offset;
		XtVaGetValues(rc3,
			XmNbottomOffset, &offset,
			NULL);
		printf("offset %i\n",offset);
	}
	XtManageChild(rc3);

	for (i=0; i<3; i++) {
	    char	s[20];

	    sprintf(s, "button-%d", i+1);
	    if (i == 0) {

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

    XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	300,	300,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	10,	10,	36,	25,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	45,	160,	87,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	154,	25,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	106,	0,	48,	25,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	106,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	154,	25,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	59,	154,	25,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
        LessTifTestMainLoop(toplevel);

    exit(0);
}
