/* $Header: /cvsroot/lesstif/lesstif/test/Xm/selectionbox/test6.c,v 1.5 2001/06/18 14:05:31 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


XtAppContext app;
Widget toplevel, box, push, tf;

int which = 0;

void
ok(Widget w, XtPointer client, XtPointer call)
{
	XmSelectionBoxCallbackStruct *p = (XmSelectionBoxCallbackStruct *)call;
	char	*s = NULL;

	XmStringGetLtoR(p->value, XmSTRING_DEFAULT_CHARSET, &s);
	fprintf(stderr, "Ok '%s'\n", s);
}

void
activate(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Activated\n");
}

void
pushme(Widget w, XtPointer client, XtPointer call)
{
	int argc;
	Arg args[3];
	Widget menu, pane1, cascade1, but;

	switch (which)
	{
	case 0:
	default:
		argc = 0;
		XtSetArg(args[argc], XmNchildPlacement, XmPLACE_TOP); argc++;
		box = XmCreateSelectionDialog(toplevel, "Box", args, argc);
		tf = XmCreateTextField(box, "tf", NULL, 0);
		XtAddCallback(tf, XmNactivateCallback, activate, NULL);
		XtAddCallback(box, XmNokCallback, ok, NULL);
		XtManageChild(tf);
		XtManageChild(box);
		break;

	case 1:
		argc = 0;
		XtSetArg(args[argc], XmNchildPlacement,
			 XmPLACE_ABOVE_SELECTION); argc++;
		box = XmCreateSelectionDialog(toplevel, "Box", args, argc);
		tf = XmCreateTextField(box, "tf", NULL, 0);
		XtAddCallback(tf, XmNactivateCallback, activate, NULL);
		XtAddCallback(box, XmNokCallback, ok, NULL);
		XtManageChild(tf);
		XtManageChild(box);
		break;

	case 2:
		argc = 0;
		XtSetArg(args[argc], XmNchildPlacement,
			 XmPLACE_BELOW_SELECTION); argc++;
		box = XmCreateSelectionDialog(toplevel, "Box", args, argc);
		tf = XmCreateTextField(box, "tf", NULL, 0);
		XtAddCallback(tf, XmNactivateCallback, activate, NULL);
		XtAddCallback(box, XmNokCallback, ok, NULL);
		XtManageChild(tf);
		XtManageChild(box);
		break;

	case 3:
		box = XmCreateSelectionDialog(toplevel, "Box", NULL, 0);
#if 0
		XtVaCreateManagedWidget("button0", xmPushButtonWidgetClass,
					box, NULL);
#endif
		menu=XmCreateMenuBar(box,"Menubar", NULL, 0);
		XtManageChild(menu);
		pane1 = XmCreatePulldownMenu(menu, "pane1", NULL, 0);
		cascade1 = XtVaCreateManagedWidget("cascade1",
						   xmCascadeButtonWidgetClass,
						   menu,
						   XmNsubMenuId, pane1,
						   NULL);
		XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass,
					pane1, NULL);
		XtManageChild(box);
		break;

	case 4:
		box = XmCreateSelectionDialog(toplevel, "Box", NULL, 0);
		but=XmSelectionBoxGetChild(box, XmDIALOG_OK_BUTTON);
		if (but)
			XtUnmanageChild(but);
		but=XmSelectionBoxGetChild(box, XmDIALOG_HELP_BUTTON);
		if (but)
			XtUnmanageChild(but);
		but=XmSelectionBoxGetChild(box, XmDIALOG_CANCEL_BUTTON);
		if (but)
			XtUnmanageChild(but);
		but=XmSelectionBoxGetChild(box, XmDIALOG_APPLY_BUTTON);
		if (but)
			XtUnmanageChild(but);
		XtVaCreateManagedWidget("button0", xmPushButtonWidgetClass,
					box, NULL);
		XtManageChild(box);
		break;
	}
}

int
main(int argc, char **argv)
{
	if (argc > 1)
	    which = atoi(argv[1]);

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
		&argc, argv, NULL, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
			XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

	XtAddCallback(push, XmNactivateCallback, pushme, NULL);

	XtManageChild(push);

	XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   66,   25, 0,0,0, /* push */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

	exit(0);
}
