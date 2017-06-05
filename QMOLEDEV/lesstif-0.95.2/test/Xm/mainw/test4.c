#include <Xm/XmP.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/MenuShell.h>
#include <Xm/MainWP.h>
#include <Xm/MessageB.h>
#include <Xm/Command.h>
#include <stdio.h>

void
dump_sw(char *where, Widget w)
{
    XmMainWindowWidget mw  = (XmMainWindowWidget) w;
    
    printf(where);
    printf("Given W: %d H: %d\n", mw->swindow.GivenWidth,
	   mw->swindow.GivenHeight);
    printf("Area W: %d H: %d\n", mw->swindow.AreaWidth,
	   mw->swindow.AreaHeight);
    printf("MWArea W: %d H: %d\n", mw->mwindow.AreaWidth,
	   mw->mwindow.AreaHeight);
    /*
    printf("Pad W: %d H: %d\n", mw->swindow.WidthPad(w),
	   mw->swindow.HeightPad);
	   */
    printf("Offset X: %d Y: %d\n", mw->swindow.XOffset, mw->swindow.YOffset);
    printf("Spacing: %d\n", mw->swindow.pad);
    printf("Has: %d %d\n", mw->swindow.hasHSB, mw->swindow.hasVSB);
    printf("HSB: %d %d %d %d\n", mw->swindow.hsbX, mw->swindow.hsbY,
	                         mw->swindow.hsbWidth, mw->swindow.hsbHeight);
    printf("VSB: %d %d %d %d\n", mw->swindow.vsbX, mw->swindow.vsbY,
	                         mw->swindow.vsbWidth, mw->swindow.vsbHeight);
    /*
    printf("HSB values: %d %d %d %d\n", mw->swindow.hmin, mw->hmax,
	                         mw->swindow.hOrigin, mw->swindow.hExtent);
    printf("VSB values: %d %d %d %d\n", mw->swindow.vmin, mw->vmax,
	                         mw->swindow.vOrigin, mw->swindow.vExtent);
	                         */
    printf("Done.\n\n");
}

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
    XtVaSetValues(toplevel, XmNshowSeparator, True, NULL);
    XtManageChild(toplevel);
dump_sw("after init\n", toplevel);

    menubar = XmCreateMenuBar(toplevel,
			      "menuBar",
			      NULL,0);
dump_sw("after add menu\n", toplevel);
    XtManageChild(menubar);
dump_sw("after manage menu\n", toplevel);

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
dump_sw("after create message\n", toplevel);
    XtVaSetValues(toplevel, XmNmessageWindow, mb, NULL);
dump_sw("after set message\n", toplevel);

    com = XtVaCreateManagedWidget("com", xmCommandWidgetClass, toplevel, NULL);
dump_sw("after create command\n", toplevel);

    one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				  NULL);
dump_sw("after create form\n", toplevel);

    two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_NONE,
				  XmNrightAttachment, XmATTACH_FORM,
				  NULL);
dump_sw("after create two\n", toplevel);

    three = XtVaCreateManagedWidget("three", xmPushButtonWidgetClass, one,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget, two,
				    NULL);
dump_sw("after create three\n", toplevel);

    XmMainWindowSetAreas(toplevel, menubar, com, NULL, NULL, one);
dump_sw("after set areas\n", toplevel);

    XtRealizeWidget(shell);
dump_sw("after realize\n", toplevel);

/* toplevel should be replaced with to correct applicationShell */

{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,  212,  362, 0,0,0, /* MainWindow */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  212,    2, 0,0,0, /* Separator1 */
   CWWidth | CWHeight | CWX | CWY,    0,  246,  212,    2, 0,0,0, /* Separator2 */
   CWWidth | CWHeight | CWX | CWY,    0,  273,  212,    2, 0,0,0, /* Separator3 */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  212,   31, 0,0,0, /* menuBar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,   45,    5,   40,   21, 0,0,0, /* Edit */
   CWWidth | CWHeight | CWX | CWY,    0,  275,  212,   87, 0,0,0, /* mb */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  192,    4, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   24,  212,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   36,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   74,   36,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  138,   36,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    0,   33,  212,  213, 0,0,0, /* com */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  192,  135, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  192,  135, 0,0,0, /* ItemsList */
   CWWidth | CWHeight | CWX | CWY,   10,  155,  192,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   10,  172,  192,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,  248,  212,   25, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,  182,    0,   30,   25, 0,0,0, /* two */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  182,   25, 0,0,0, /* three */ 
    };
    PrintDetails(shell,Expected);
};
  LessTifTestMainLoop(shell);

    exit(0);
}

