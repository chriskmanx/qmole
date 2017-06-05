/* $Header: /cvsroot/lesstif/lesstif/test/Xm/cascadebuttong/test2.c,v 1.6 2001/05/23 15:00:02 amai Exp $
 * You should also run this with
 *	test1 -xrm "*tearOffModel: tear_off_enabled"
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/RepType.h>
#include <Xm/SelectioB.h>

#include <Xm/XmP.h>

char *fallback[] = {
	"*tearOffModel:			tear_off_enabled",
	"*cascade1.labelString:		Menu",
	"*cascade1.mnemonic:		M",
	"*cascade2.labelString:		Other",
	"*cascade2.mnemonic:		t",
	"*cascade3.labelString:		Deep",
	"*cascade3.mnemonic:		D",
	NULL	/* The end */
};

void PrintIt(Widget w)
{
	fprintf(stderr, "Widget %s - geo %d %d %d %d\n",
		XtName(w), XtX(w), XtY(w), XtWidth(w), XtHeight(w));
}

void pb_activate_callback(Widget w, XtPointer clientData, XtPointer callData)
{
	Widget	pf;

	fprintf(stderr, "* Widget = %s - Activated\n", XtName(w));

	pf = XmGetPostedFromWidget(XtParent(w));

	if (pf)
		fprintf(stderr, "Posted from 0x%X (%s)\n", (unsigned int)pf, XtName(pf));

/*
 * Suspicions about windows being too large ...
 */
	PrintIt(w);			/* The button */
	PrintIt(XtParent(w));		/* The RC */
	PrintIt(XtParent(XtParent(w)));	/* The shell */
}

void pb_arm_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("* Widget = %s - Armed\n", XtName(w));
}

void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

Widget	pd;

void PopUp(Widget w, XtPointer client, XtPointer call)
{
	XtManageChild(pd);
}

int main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget	toplevel, rc;
    Widget	cascade1, cascade2;
    Widget	pane1, pane2, pane3;
    Widget	w, ms;
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

    cascade1 = XtVaCreateManagedWidget("cascade1", xmCascadeButtonGadgetClass, rc,
		XmNsubMenuId,	pane1,
	NULL);

    w = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass, pane1,
	NULL);
    XtAddCallback(w, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(w, XmNarmCallback, pb_arm_callback, NULL);

    w = XtVaCreateManagedWidget("button2", xmPushButtonGadgetClass, pane1,
	NULL);
    XtAddCallback(w, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(w, XmNarmCallback, pb_arm_callback, NULL);

    w = XtVaCreateManagedWidget("button3", xmPushButtonWidgetClass, pane1,
	NULL);
    XtAddCallback(w, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(w, XmNarmCallback, pb_arm_callback, NULL);

/* Second Menu */
    cascade2 = XtVaCreateManagedWidget("cascade2", xmCascadeButtonGadgetClass, rc,
	NULL);

    ms = XtCreatePopupShell("ms1", xmMenuShellWidgetClass, rc,
		NULL, 0);

    pane2 = XtVaCreateWidget("pane2", xmRowColumnWidgetClass, ms,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		XmNx,			0,
		XmNy,			0,
		XmNwidth,		91,
		XmNheight,		25,
	NULL);

    w = XtVaCreateManagedWidget("dummy", xmPushButtonWidgetClass, pane2,
	NULL);

    XtVaSetValues(cascade2,
		XmNsubMenuId, pane2,
	NULL);

    ms = XtCreatePopupShell("ms2", xmMenuShellWidgetClass, pane2,
		NULL, 0);

    pane3 = XtVaCreateWidget("pane3", xmRowColumnWidgetClass, ms,
		XmNrowColumnType,	XmMENU_PULLDOWN,
	NULL);

    w = XtVaCreateManagedWidget("cascade3", xmCascadeButtonGadgetClass, pane2,
		XmNsubMenuId,		pane3,
	NULL);

    w = XtVaCreateManagedWidget("dummy", xmPushButtonWidgetClass, pane3,
	NULL);


    pd = XmCreatePromptDialog(w, "prompt", NULL, 0);

    XtAddCallback(pd, XmNokCallback, Quit, NULL);
    XtAddCallback(w, XmNactivateCallback, PopUp, NULL);

    XtManageChild(rc);
    XtRealizeWidget(toplevel);
    

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   96,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* cascade1 */
   CWWidth | CWHeight | CWX | CWY,   45,    5,   46,   21, 0,0,0, /* cascade2 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
    exit(0);
}
