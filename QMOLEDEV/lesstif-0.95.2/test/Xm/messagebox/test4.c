/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test4.c,v 1.5 2001/05/15 14:08:34 amai Exp $
   test of selection boxes */

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/MenuShell.h>
#include <Xm/RepType.h>
#include <Xm/SelectioB.h>

#include <Xm/XmP.h>
#include <Xm/MessageB.h>

#include "../../common/Test.h"


Widget toplevel, box, push;

void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

void MakeDialog(Widget w, XtPointer client, XtPointer call)
{
	Widget	p = XtParent(XtParent(XtParent(w)));
	Widget	dialog;
	Arg	al[10];
	int	ac;

	ac = 0;
	XtSetArg(al[ac], XmNdefaultPosition, False); ac++;
	XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL); ac++;
	XtSetArg(al[ac], XmNtitle, "xephem Query"); ac++;
	dialog = XmCreateQuestionDialog(p, "Box", al, ac);
	XtAddCallback(dialog, XmNokCallback, Quit, NULL);
	XtManageChild(dialog);
}


char *fallback[] = {
	"*tearOffModel:				tear_off_enabled",
	"*cascade1.labelString:			Menu",
	"*cascade1.mnemonic:			M",
	"*cascade2.labelString:			Other",
	"*cascade2.mnemonic:			t",
	"*cascade3.labelString:			Deep",
	"*cascade3.mnemonic:			D",
	"*XmPushButtonGadget.labelString:	gadget",
	NULL	/* The end */
};

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget cascade1;
    Widget pane1;
    Widget w;
    Arg		al[5];
    int		ac;

/* Install converter to make the command line indicated above work */
    XmRepTypeInstallTearOffModelConverter();

/* Toplevel and Menu Bar */
    toplevel = XtVaAppInitialize(&theApp, "test1", NULL, 0, &argc, argv, fallback, NULL);
    rc = XmCreateMenuBar(toplevel, "menubar", NULL, 0);

/* First Menu */
    ac = 0;
    XtSetArg(al[ac], XmNnumColumns, 2); ac++;
    pane1 = XmCreatePulldownMenu(rc, "pane1", al, ac);

    cascade1 = XtVaCreateManagedWidget("cascade1", xmCascadeButtonWidgetClass, rc,
		XmNsubMenuId,	pane1,
	NULL);

    w = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass, pane1,
	NULL);
    XtAddCallback(w, XmNactivateCallback, MakeDialog, NULL);

    XtManageChild(rc);
    XtRealizeWidget(toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,   50,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* cascade1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    exit(0);
}
