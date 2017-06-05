/*
 * Copyright 1989, 1992 O'Reilly and Associates, Inc.
 * See ../Copyright for complete rights and liability information.
 */

/* 
 *  xmainwindow.c - main window with help and quit
 */

/*
 *  So that we can use fprintf:
 */
#include <stdio.h>

/* 
 * Standard Toolkit include files:
 */
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/RepType.h>
/*
 * Public include files for widgets used in this file.
 */
#include <Xm/PushB.h>
#include <Xm/MainWP.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/CascadeB.h>
#include <Xm/Command.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>

/* 
 * callback to pop up help dialog widget 
 */
/*ARGSUSED*/
void ShowHelp(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget dialog = (Widget) client_data;
    XtManageChild(dialog);
}

/*
 * quit button callback function
 */
/*ARGSUSED*/
void Quit(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{ 
    exit(0); 
}

int
main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel, mainWindow, menuBar, frame;
#if 0
    Widget command;
#endif
    Widget fileButton, fileMenu, quit, helpButton, helpMenu, help, helpBox;
    Widget temp;

    /*
     * Register the default language procedure
     */
    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
            &app_context,       /* Application context */
            "Mainw8",	        /* application class name */
            NULL, 0,            /* command line option list */
            &argc, argv,        /* command line args */
            NULL,               /* for missing app-defaults file */
            NULL);              /* terminate varargs list */

    /* create main window */
    mainWindow = XtVaCreateManagedWidget(
            "mainWindow",               /* widget name */
            xmMainWindowWidgetClass,    /* widget class */
            topLevel,                   /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* register converter for tearoff menus */
    XmRepTypeInstallTearOffModelConverter();

    /* create menu bar along top inside of main window */
    menuBar = XmCreateMenuBar(
            mainWindow, /* parent widget*/
            "menuBar",  /* widget name */
            NULL,       /* no arguments needed */
            0);         /* no arguments needed */
    XtManageChild(menuBar);

    frame = XtVaCreateManagedWidget(
            "frame",            /* widget name */
            xmLabelWidgetClass, /* widget class */
            mainWindow,         /* parent widget*/
            NULL);              /* terminate varargs list */
#if 0
    command = XtVaCreateManagedWidget(
            "command",            /* widget name */
            xmCommandWidgetClass, /* widget class */
            mainWindow,         /* parent widget*/
            NULL);              /* terminate varargs list */
#endif
    /*  Set MainWindow areas */
/*    XmMainWindowSetAreas (mainWindow, menuBar, NULL, NULL, NULL,
frame); */

    /*
     *  CREATE FILE MENU AND CHILDREN
     */

    /* create button that will pop up the menu */
    fileButton = XtVaCreateManagedWidget(
            "fileButton",               /* widget name */
            xmCascadeButtonWidgetClass, /* widget class */
            menuBar,                    /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* create menu (really a Shell widget and RowColumn widget combo) */
    fileMenu = XmCreatePulldownMenu(
            menuBar,    /* parent widget*/
            "fileMenu", /* widget name */
            NULL,       /* no argument list needed */
            0);         /* no argument list needed */

    /* create the quit button up in the menu */
    quit = XtVaCreateManagedWidget(
            "quit",                     /* widget name */
            xmPushButtonWidgetClass,    /* widget class */
            fileMenu,                   /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* 
     * Specify which menu fileButton will pop up.
     */
    XtVaSetValues(fileButton,
		  XmNsubMenuId, fileMenu,
		  NULL);

    /* arrange for quit button to call function that exits. */
    XtAddCallback(quit, XmNactivateCallback, Quit, 0);

    /*
     *  CREATE HELP BUTTON AND BOX
     */

    /* create button that will bring up help menu */
    helpButton = XtVaCreateManagedWidget(
            "helpButton",               /* widget name */
            xmCascadeButtonWidgetClass, /* widget class */
            menuBar,                    /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* tell menuBar which is the help button (will be specially
positioned) */
    XtVaSetValues(menuBar,
		  XmNmenuHelpWidget, helpButton,
		  NULL);

    /* create menu (really a Shell widget and RowColumn widget combo) */
    helpMenu = XmCreatePulldownMenu(
            menuBar,    /* parent widget*/
            "helpMenu", /* widget name */
            NULL,       /* no argument list needed */
            0);         /* no argument list needed */

    /* create the help button up in the menu */
    help = XtVaCreateManagedWidget(
            "help",                     /* widget name */
            xmPushButtonWidgetClass,    /* widget class */
            helpMenu,                   /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* 
     * Specify which menu helpButton will pop up.
     */
    XtVaSetValues(helpButton,
		  XmNsubMenuId, helpMenu,
		  NULL);

    /* create popup that will contain help */
    helpBox = XmCreateMessageDialog(
	    help,       /* parent widget*/
            "helpBox",  /* widget name   */
            NULL,       /* no arguments needed */
            0);         /* no arguments needed */

    temp = XmMessageBoxGetChild (helpBox, XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild (temp);
    temp = XmMessageBoxGetChild (helpBox, XmDIALOG_HELP_BUTTON);
    XtUnmanageChild (temp);

    /* arrange for getHelp button to pop up helpBox */
    XtAddCallback(help, XmNactivateCallback, ShowHelp, helpBox);

    XtRealizeWidget(topLevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  162,   48, 0,0,0, /* mainWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  162,   31, 0,0,0, /* menuBar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   76,   21, 0,0,0, /* fileButton */
   CWWidth | CWHeight | CWX | CWY,   81,    5,   76,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  162,   17, 0,0,0, /* frame */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(topLevel, Expected);
}
  LessTifTestMainLoop(topLevel);

    exit(0);
}
