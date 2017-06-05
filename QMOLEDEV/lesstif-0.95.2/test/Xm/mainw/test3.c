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
#include <Xm/MessageB.h>
#include <Xm/Command.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget shell, toplevel, menubar, filepane;
    Widget button, button1, button2, sep;
    Widget saveAsPane, button3, button4, button5;
    Widget button6, button7, editpane;
    Widget mb, com;
    Widget one, two, three;
    XmString xmstr;

    shell = XtVaAppInitialize(&theApp, "mainW", NULL, 0,
				 &argc, argv, NULL, NULL);

    toplevel = XmCreateMainWindow(shell, "MainWindow", NULL, 0);
    XtManageChild(toplevel);

    menubar = XmCreateMenuBar(toplevel,
			      "menuBar",
			      NULL,0);
    XtManageChild(menubar);

    filepane = XmCreatePulldownMenu(menubar,
				    "pane",
				    NULL,0);

    xmstr = XmStringCreateSimple("File");
    
    button = XtVaCreateManagedWidget("File",
				     xmCascadeButtonWidgetClass,
				     menubar,
				     XmNsubMenuId, filepane,
                                     XmNlabelString, xmstr,
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

    mb = XtVaCreateManagedWidget("mb", xmMessageBoxWidgetClass, toplevel, NULL);
    XtVaSetValues(toplevel, XmNmessageWindow, mb, NULL);

    com = XtVaCreateManagedWidget("com", xmCommandWidgetClass, toplevel, NULL);

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

    XmMainWindowSetAreas(toplevel, menubar, com, NULL, NULL, one);

    XtRealizeWidget(shell);

/* toplevel should be replaced with to correct applicationShell */

{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,  212,  356, 0,0,0, /* MainWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  212,   31, 0,0,0, /* menuBar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,   45,    5,   40,   21, 0,0,0, /* Edit */
   CWWidth | CWHeight | CWX | CWY,    0,  269,  212,   87, 0,0,0, /* mb */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  192,    4, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   24,  212,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   36,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   74,   36,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  138,   36,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  212,  213, 0,0,0, /* com */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  192,  135, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  192,  135, 0,0,0, /* ItemsList */
   CWWidth | CWHeight | CWX | CWY,   10,  155,  192,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   10,  172,  192,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,  244,  212,   25, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,  182,    0,   30,   25, 0,0,0, /* two */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  182,   25, 0,0,0, /* three */ 
    };
    PrintDetails(shell,Expected);
};
  LessTifTestMainLoop(shell);

    exit(0);
}

