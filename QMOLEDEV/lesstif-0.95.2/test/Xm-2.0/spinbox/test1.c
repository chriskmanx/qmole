#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/Form.h>
#include <Xm/SpinB.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


void ModifyVerify(Widget w, XtPointer client, XtPointer call)
{
	XmSpinBoxCallbackStruct	*cbp = (XmSpinBoxCallbackStruct *)call;

	fprintf(stderr, "ModifyVerify: position %d\n", cbp->position);
}

int
main(int argc, char **argv)
{
	Widget		toplevel, spb, bb, list, tf;
	XtAppContext	app;
	XmString	item;
	Arg			al[10];
	int			ac;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "SpinBox", NULL, 0,
		&argc, argv, NULL, NULL);

	bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
	XtManageChild(bb);

	ac = 0;
	spb = XmCreateSpinBox(bb, "spinbox", al, ac);
	XtAddCallback(spb, XmNvalueChangedCallback, ModifyVerify, NULL);

	ac = 0;
	XtSetArg(al[ac], XmNspinBoxChildType, XmNUMERIC); ac++;
	XtSetArg(al[ac], XmNdecimalPoints, 1); ac++;
	tf = XmCreateTextField(spb, "tf", al, ac);
	XtManageChild(tf);

	XtManageChild(spb);

	XtRealizeWidget(toplevel);

	LessTifTestMainLoop(toplevel);

	exit(0);
}
