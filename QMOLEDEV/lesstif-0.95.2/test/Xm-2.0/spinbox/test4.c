/*
 * The test suggested in the Motif manual pages
 */
#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/SpinB.h>
#include <Xm/Text.h>

#include "../../common/Test.h"


Widget		toplevel, spb, bb, day, month, year;

void ModifyVerify(Widget w, XtPointer client, XtPointer call)
{
	XmSpinBoxCallbackStruct	*cbp = (XmSpinBoxCallbackStruct *)call;

	fprintf(stderr, "ModifyVerify: Widget %s position %d\n",
		XtName(cbp->widget), cbp->position);
}

int
main(int argc, char **argv)
{
	XtAppContext	app;
	XmString	item;
	Arg			al[10];
	int			ac;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "SpinBox", NULL, 0,
		&argc, argv, NULL, NULL);

#ifdef	USE_BB
	bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
	XtManageChild(bb);
#else
	bb = toplevel;
#endif

	ac = 0;
	spb = XmCreateSpinBox(bb, "spinbox", al, ac);
	XtAddCallback(spb, XmNvalueChangedCallback, ModifyVerify, NULL);

	ac = 0;
	XtSetArg(al[ac], XmNspinBoxChildType, XmNUMERIC); ac++;
	day = XmCreateText(spb, "day", al, ac);
	XtManageChild(day);
	ac = 0;
	XtSetArg(al[ac], XmNspinBoxChildType, XmNUMERIC); ac++;
	month = XmCreateText(spb, "month", al, ac);
	XtManageChild(month);
	ac = 0;
	XtSetArg(al[ac], XmNspinBoxChildType, XmNUMERIC); ac++;
	year = XmCreateText(spb, "year", al, ac);
	XtManageChild(year);

	XtManageChild(spb);

	XtRealizeWidget(toplevel);

	LessTifTestMainLoop(toplevel);

	exit(0);
}
