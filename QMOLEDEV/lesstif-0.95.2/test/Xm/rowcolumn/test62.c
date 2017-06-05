/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test62.c,v 1.3 2001/05/15 14:46:10 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/CascadeB.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/PushBP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumn.h>
#include <Xm/SelectioB.h>
#include <Xm/DrawingA.h>

#include "../../common/Test.h"

Widget toplevel, box, push;
XtAppContext	app_context;
int DialogDone=False,DialogReturn;

char *fallback[] = {
	"*tearOffModel:				tear_off_enabled",
	"*cascade1.labelString:			File",
	"*cascade1.mnemonic:			F",
	"*Edit.mnemonic:			E",
	"*Help.mnemonic:			H",
	"*XmPushButtonGadget.labelString:	gadget",
	"*XmDrawingArea.geometry:               200x200",
	NULL	/* The end */
};

int
main(int argc, char **argv)
{
    Widget rc;
    Widget cascade1;
    Widget pane1;
    Widget w;
    Arg		al[5];
    int		ac;

/* Install converter to make the command line indicated above work */
    XmRepTypeInstallTearOffModelConverter();

/* Toplevel and Menu Bar */
    toplevel = XtVaAppInitialize(&app_context, "test1", NULL, 0, &argc, argv, fallback, NULL);
    rc = XmCreateMenuBar(toplevel, "menubar", NULL, 0);


/* First Menu */
    ac = 0;
    XtSetArg(al[ac], XmNnumColumns, 2); ac++;
    XtSetArg(al[ac], XmNpacking, XmPACK_COLUMN); ac++;
    pane1 = XmCreatePulldownMenu(rc, "pane1", al, ac);

    cascade1 = XtVaCreateManagedWidget("cascade1",
				       xmCascadeButtonWidgetClass, rc, 
				       XmNsubMenuId,	pane1,
				       NULL);

    w = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass, pane1, NULL);
    w = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass, pane1, NULL);
    w = XtVaCreateManagedWidget("button3", xmPushButtonWidgetClass, pane1, NULL);


    XtManageChild(rc);
    XtRealizeWidget(toplevel);

    LessTifTestWaitForIt(toplevel);
    LessTifTestBtn1Down(cascade1);

    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   99,  104,   56, 0,0,0, /* pane1 */
   CWWidth | CWHeight | CWX | CWY,    2,   12,   50,   21, 0,0,0, /* button1 */
   CWWidth | CWHeight | CWX | CWY,    2,   33,   50,   21, 0,0,0, /* button2 */
   CWWidth | CWHeight | CWX | CWY,   52,   12,   50,   21, 0,0,0, /* button3 */ 
    };
    PrintDetails(pane1,Expected);
};

    LessTifTestMainLoop(toplevel);

    exit(0);
}
