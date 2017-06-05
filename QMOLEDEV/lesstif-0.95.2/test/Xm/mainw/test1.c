#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/MenuShell.h>
#include <Xm/MainW.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget shell, toplevel, menubar, filepane;
    Widget button, button1, button2, sep;
    Widget saveAsPane, button3, button4, button5;
    Widget button6, button7, editpane;
    Widget one, two, three;

    shell = XtVaAppInitialize(&theApp, "mainW", NULL, 0,
				 &argc, argv, NULL, NULL);

    toplevel = XmCreateMainWindow(shell, "MainWindow", NULL, 0);
    XtManageChild(toplevel);

    menubar = XmCreateMenuBar(toplevel,
			      "menuBar",
			      NULL,0);

    filepane = XmCreatePulldownMenu(menubar,
				    "pane",
				    NULL,0);

    button = XtVaCreateManagedWidget("File",
				     xmCascadeButtonGadgetClass,
				     menubar,
				     XmNsubMenuId, filepane,
				     NULL);

    editpane = XmCreatePulldownMenu(menubar,
				    "pane2",
				    NULL, 0);

    button6 = XtVaCreateManagedWidget("Edit",
				      xmCascadeButtonGadgetClass,
				      menubar,
				      XmNsubMenuId, editpane,
				      NULL);

    button7 = XtVaCreateManagedWidget("Cut",
				      xmPushButtonGadgetClass,
				      editpane,
				      NULL);

    button1 = XtVaCreateManagedWidget("Open",
				      xmPushButtonGadgetClass,
				      filepane,
				      NULL);

    sep = XtVaCreateManagedWidget("sep",
				  xmSeparatorGadgetClass,
                                  filepane,
                                  NULL);

    button2 = XtVaCreateManagedWidget("Close",
				      xmPushButtonWidgetClass,
				      filepane,
				      NULL);

    saveAsPane = XmCreatePulldownMenu(filepane,
				      "save_as",
				      NULL, 0);

    button3 = XtVaCreateManagedWidget("Save As",
				      xmCascadeButtonWidgetClass,
				      filepane,
				      XmNsubMenuId, saveAsPane,
				      NULL);

    button4 = XtVaCreateManagedWidget("MS Word",
				      xmPushButtonWidgetClass,
				      saveAsPane,
				      NULL);

    button5 = XtVaCreateManagedWidget("LaTeX",
				      xmPushButtonWidgetClass,
				      saveAsPane,
				      NULL);

    XtManageChild(menubar);

    one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				  NULL);

    two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_NONE,
				  XmNrightAttachment, XmATTACH_FORM,
				  NULL);

    three = XtVaCreateManagedWidget("three", xmPushButtonWidgetClass, one,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget, two,
				    NULL);

    XmMainWindowSetAreas(toplevel, menubar, NULL, NULL, NULL, one);

    XtRealizeWidget(shell);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,   90,   56, 0,0,0, /* MainWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   90,   31, 0,0,0, /* menuBar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,   45,    5,   40,   21, 0,0,0, /* Edit */
   CWWidth | CWHeight | CWX | CWY,    0,   31,   90,   25, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,   60,    0,   30,   25, 0,0,0, /* two */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   60,   25, 0,0,0, /* three */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(shell, Expected);
}
  LessTifTestMainLoop(shell);

    exit(0);
}

