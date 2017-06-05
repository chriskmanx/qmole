/*
 *
 * demo.c - Xmp widget demo application
 *
 */

#include <stdio.h>
#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>
#include "XmpMrm.h"

static void ManageCb();
static void ExitCb();

static MrmHierarchy mrmId;
static MrmCode mrmClass;
static char *mrmFiles[]={"demo.uid"};
static MRMRegisterArg mrmNames[] = {
        {"ManageCb", (caddr_t)ManageCb },
        {"ExitCb", (caddr_t)ExitCb }
};

static XtAppContext appContext;
static Display *display;
static Widget shell;
static Widget appMain;

int main(argc, argv)
    int argc;
    char **argv;
{
    MrmInitialize ();
    XmpMrmInitialize();	/* register Xmp widgets with Mrm */
    XtToolkitInitialize();
    appContext = XtCreateApplicationContext();
    display = XtOpenDisplay(appContext, NULL, "demo", "Demo", NULL, 0,
						&argc, argv);
    if (display == NULL) {
	fprintf(stderr, "Can't open display\n");
	exit(1);
    }
    shell = XtAppCreateShell("demo", NULL, applicationShellWidgetClass,
						display, NULL, 0);
    if (MrmOpenHierarchy (1, mrmFiles, NULL, &mrmId) != MrmSUCCESS) exit(1);
    MrmRegisterNames(mrmNames, XtNumber(mrmNames));
    MrmFetchWidget (mrmId, "appMain", shell, &appMain, &mrmClass);
    XtManageChild(appMain);
    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);
    exit(0);
}



/*
 * ManageCb - Manage widget passed as client_data.
 */
static void ManageCb(w, id, cb)
    Widget w;
    String id;
    XtPointer cb;
{
    XtManageChild (XtNameToWidget(shell, id));
}


/*
 * ExitCb - Exit application.
 */
static void ExitCb(w, cd, cb)
    Widget w;
    XtPointer cd;
    XtPointer cb;
{
    exit(0);
}


