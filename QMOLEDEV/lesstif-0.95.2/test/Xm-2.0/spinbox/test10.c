/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/spinbox/test10.c,v 1.1 2003/09/18 19:56:47 dannybackx Exp $
 *
 * Demonstrate the issue reported in E-mail :
 *
 * Also please note I've left loads of "Xm"s off names below.
 *
 * There is a discrepancy between Motif and Lesstif (rel 0.93.36) over the 
 * "position" value in SpinBoxes which is apparent if the child type is 
 * numeric and the "minimumValue" is non-zero.
 *
 * With "positionType" set to POSITION_VALUE (the default) the value 
 * displayed is "minimumValue + position" on LessTif and just "position" on 
 * Motif.
 *
 * The description on O'Reilly 6B p968 in my copy is a little ambiguous but 
 * it seems to me that the Motif version is correct and LessTif is behaving 
 * like Motif would do with "positionType" set to POSITION_INDEX (which 
 * makes no difference with LessTif).
 */

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
	XtSetArg(al[ac], XmNpositionType, XmPOSITION_VALUE); ac++;
	XtSetArg(al[ac], XmNdecimalPoints, 0); ac++;
	XtSetArg(al[ac], XmNminimum, 5); ac++;
	XtSetArg(al[ac], XmNposition, 3); ac++;
	tf = XmCreateTextField(spb, "tf", al, ac);
	XtManageChild(tf);

	ac = 0;
	XtSetArg(al[ac], XmNspinBoxChildType, XmNUMERIC); ac++;
	XtSetArg(al[ac], XmNpositionType, XmPOSITION_INDEX); ac++;
	XtSetArg(al[ac], XmNdecimalPoints, 0); ac++;
	XtSetArg(al[ac], XmNminimum, 5); ac++;
	XtSetArg(al[ac], XmNposition, 3); ac++;
	tf = XmCreateTextField(spb, "tf", al, ac);
	XtManageChild(tf);

	XtManageChild(spb);

	XtRealizeWidget(toplevel);

	LessTifTestMainLoop(toplevel);

	exit(0);
}
