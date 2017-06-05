#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/SpinB.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


String fallback[] = {
	"*SpinBoxChildType:	string",
	NULL
};

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
	XmString	items[3];
	Arg			al[10];
	int			ac;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "SpinBox", NULL, 0,
		&argc, argv, fallback, NULL);

	bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
	XtManageChild(bb);

	ac = 0;
	spb = XmCreateSpinBox(bb, "spinbox", al, ac);
	XtAddCallback(spb, XmNvalueChangedCallback, ModifyVerify, NULL);

	items[0] = XmStringCreateSimple("eins");
	items[1] = XmStringCreateSimple("zwei");
	items[2] = XmStringCreateSimple("zaufen");

	ac = 0;
#if 0
	XtSetArg(al[ac], XmNspinBoxChildType, XmSTRING); ac++;
#endif
	XtSetArg(al[ac], XmNvalues, items); ac++;
	XtSetArg(al[ac], XmNnumValues, 3); ac++;
	tf = XmCreateTextField(spb, "tf", al, ac);
	XtManageChild(tf);

	XtManageChild(spb);

	XtRealizeWidget(toplevel);

	LessTifTestMainLoop(toplevel);

	exit(0);
}
