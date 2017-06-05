/* $Header: /cvsroot/lesstif/lesstif/test/Xm/cascadebutton/test4.c,v 1.6 2002/04/17 20:15:45 amai Exp $ */
#include <stdlib.h>
#include <stdio.h>

#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/CascadeB.h>
#include <Xm/MenuShell.h>
#include <Xm/RepType.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>

#include <Xm/XmP.h>

char *fallback[] = {
        "*tearOffModel:                         tear_off_enabled",
        "*cascade1.labelString:                 Menu",
        "*cascade1.mnemonic:                    M",
        "*cascade2.labelString:                 Other",
        "*cascade2.mnemonic:                    t",
        "*cascade3.labelString:                 Deep",
        "*cascade3.mnemonic:                    D",
        "*XmPushButtonGadget.labelString:       gadget",
        "*button1.labelString:                  Button 1",
        "*button1.mnemonic:                     B",
        "*button1.acceleratorText:              Ctrl-B",
        "*button1.accelerator:                  Ctrl<Key>b",
        NULL    /* The end */
};

void PrintIt(Widget w, XtPointer client, XtPointer call)
{
        fprintf(stderr, "Widget %s: %s\n", XtName(w), (char *)client);
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, mw, rc, form;
    Widget cascade1;
    Widget pane1;
    Widget w;
    Arg         al[10];
    int         ac;


/* Install converter to make the command line indicated above work */
    XmRepTypeInstallTearOffModelConverter();

/* Toplevel and Menu Bar */
    toplevel = XtVaAppInitialize(&theApp, "test1", NULL, 0, &argc, argv, fallback, NULL);

    ac = 0;
    XtSetArg(al[ac], XmNshowSeparator, True); ac++;
    mw = XmCreateMainWindow(toplevel, "mainwindow", al, ac);
    XtManageChild(mw);

    rc = XmCreateMenuBar(mw, "menubar", NULL, 0);
    XtManageChild(rc);

/* First Menu */
    ac = 0;
    XtSetArg(al[ac], XmNnumColumns, 2); ac++;
    pane1 = XmCreatePulldownMenu(rc, "pane1", al, ac);

    cascade1 = XtVaCreateManagedWidget("cascade1", xmCascadeButtonWidgetClass, rc,
                XmNsubMenuId,   pane1,
        NULL);

    w = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass, pane1,
        NULL);

    w = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass, pane1,
        NULL);

    w = XtVaCreateManagedWidget("button3", xmPushButtonWidgetClass, pane1,
        NULL);

/* Form */
    form = XmCreateForm(mw, "form", NULL, 0);
    XtManageChild(form);

/* Arrow Widget */
    ac = 0;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNwidth, 100); ac++;
    XtSetArg(al[ac], XmNheight, 100); ac++;
    w = XmCreateArrowButton(form, "arrow", al, ac);
    XtManageChild(w);
    XtAddCallback(w, XmNactivateCallback, PrintIt, "Arrow button activated");

/* Arrow Widget */
    ac = 0;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNleftWidget, w); ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNwidth, 100); ac++;
    XtSetArg(al[ac], XmNheight, 100); ac++;
    w = XmCreateArrowButtonGadget(form, "arrow g", al, ac);
    XtManageChild(w);
    XtAddCallback(w, XmNactivateCallback, PrintIt, "Arrow gadget activated");

    XtRealizeWidget(toplevel);
    
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  352,  366,  200,  133, 0,0,0, /* mainwindow */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  200,    2, 0,0,0, /* Separator1 */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  200,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* cascade1 */
   CWWidth | CWHeight | CWX | CWY,    0,   33,  200,  100, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,  100, 0,0,0, /* arrow */
   CWWidth | CWHeight | CWX | CWY,  100,    0,  100,  100, 0,0,0, /* arrow g */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

    exit(0);
}
