#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/MainW.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <stdio.h>

String fallback[] = {
        "*XmMainWindow.CommandWindowLocation:   command_above_workspace",
        "*XmToggleButton.set:                   on",
        "*XmToggleButton.selectColor:           red",
        "*Command Above Work.background:        yellow",
        "*message.background:                   green",
        "*command.background:                   blue",
	"*mainWindowMarginWidth:		30",
	"*mainWindowMarginHeight:		10",
        NULL
};

void quit(Widget w, XtPointer client, XtPointer call)
{
        exit(0);
}

void doit(Widget w, XtPointer client, XtPointer call)
{
        XmToggleButtonCallbackStruct *cbp =
                        (XmToggleButtonCallbackStruct *)call;
        Widget  mw = (Widget)client;

        if (cbp->set) {
            fprintf(stderr, "====> setting to XmCOMMAND_ABOVE_WORKSPACE\n");
            XtVaSetValues(mw,
                        XmNcommandWindowLocation, XmCOMMAND_ABOVE_WORKSPACE,
                NULL);
        } else {
            fprintf(stderr, "====> setting to XmCOMMAND_BELOW_WORKSPACE\n");
            XtVaSetValues(mw,
                        XmNcommandWindowLocation, XmCOMMAND_BELOW_WORKSPACE,
                NULL);
        }
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget shell, toplevel, menubar, filepane;
    Widget cascade, quitb, cmd, toggle, mb;

    shell = XtVaAppInitialize(&theApp, "mainW", NULL, 0,
                                 &argc, argv, fallback, NULL);

    toplevel = XmCreateMainWindow(shell, "MainWindow", NULL, 0);
    XtManageChild(toplevel);

    menubar = XmCreateMenuBar(toplevel,
                              "menuBar",
                              NULL,0);
    XtManageChild(menubar);

    filepane = XmCreatePulldownMenu(menubar,
                                    "pane",
                                    NULL,0);

    cascade = XtVaCreateManagedWidget("File",
                                     xmCascadeButtonWidgetClass,
                                     menubar,
                                     XmNsubMenuId, filepane,
                                     NULL);

    quitb = XtVaCreateManagedWidget("Close",
                                      xmPushButtonWidgetClass,
                                      filepane,
                                      NULL);
    XtAddCallback(quitb, XmNactivateCallback, quit, NULL);

    mb = XtVaCreateManagedWidget("message", xmLabelWidgetClass, toplevel, NULL);
    XtVaSetValues(toplevel, XmNmessageWindow, mb, NULL);

    cmd = XtVaCreateManagedWidget("command", xmLabelWidgetClass, toplevel, NULL)
;

    toggle = XtVaCreateManagedWidget("Command Above Work",
        xmToggleButtonWidgetClass, toplevel, NULL);
    XtAddCallback(toggle, XmNvalueChangedCallback, doit, toplevel);

    XmMainWindowSetAreas(toplevel, menubar, cmd, NULL, NULL, toggle);

    XtRealizeWidget(shell);



/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  193,  110, 0,0,0, /* MainWindow */
   CWWidth | CWHeight | CWX | CWY,   30,   10,  133,   31, 0,0,0, /* menuBar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,   30,   83,  133,   17, 0,0,0, /* message */
   CWWidth | CWHeight | CWX | CWY,   30,   41,  133,   17, 0,0,0, /* command */
   CWWidth | CWHeight | CWX | CWY,   30,   58,  133,   25, 0,0,0, /* Command Above Work */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(shell, Expected);
}
LessTifTestMainLoop(shell);
    exit(0);
}


