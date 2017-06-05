/* $Header: /cvsroot/lesstif/lesstif/test/Xm/bulletinboard/test9.c,v 1.5 2001/06/18 08:10:14 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>


XtAppContext app;
Widget toplevel, box, push, tf;

void
activate(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Activated\n");
}

void
pushme(Widget w, XtPointer client, XtPointer call)
{
	box = XmCreateBulletinBoardDialog(toplevel, "Box", NULL, 0);
	tf = XmCreateTextField(box, "tf", NULL, 0);
	XtAddCallback(tf, XmNactivateCallback, activate, NULL);
	XtManageChild(tf);
	XtManageChild(box);
}

int
main(int argc, char **argv)
{

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
		&argc, argv, NULL, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
			XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

	XtAddCallback(push, XmNactivateCallback, pushme, NULL);

	XtManageChild(push);

	XtRealizeWidget(toplevel);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   66,   25, 0,0,0, /* push */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
	exit(0);
}
