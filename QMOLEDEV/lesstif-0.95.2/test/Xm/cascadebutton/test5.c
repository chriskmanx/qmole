/*
 * Use cascadebutton as a push button
 */
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

char *fallback[] = {
	"*cascade1.labelString:			Menu",
	"*cascade1.mnemonic:			M",
	NULL	/* The end */
};

void PrintIt(Widget w)
{
	fprintf(stderr, "Widget %s activated\n", XtName(w));
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget cascade1;
    Arg		al[5];
    int		ac;

/* Install converter to make the command line indicated above work */
    XmRepTypeInstallTearOffModelConverter();

/* Toplevel and Menu Bar */
    toplevel = XtVaAppInitialize(&theApp, "test5", NULL, 0, &argc, argv,
		fallback, NULL);
    rc = XmCreateMenuBar(toplevel, "menubar", NULL, 0);

    cascade1 = XtVaCreateManagedWidget("cascade1", xmCascadeButtonWidgetClass,
		rc, NULL);

    XtAddCallback(cascade1,
	XmNactivateCallback,
	(XtCallbackProc) PrintIt,
	NULL);

    XtManageChild(rc);
    XtRealizeWidget(toplevel);
    
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   50,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* cascade1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

    exit(0);
}

