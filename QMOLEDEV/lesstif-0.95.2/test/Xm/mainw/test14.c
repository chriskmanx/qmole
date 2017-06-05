/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/mainw/test14.c,v 1.10 2001/05/15 14:08:33 amai Exp $
 * Simpler version of test12.c.  Demonstrate problems with menu wrapping code.
 *
 * This seems to work the same in motif and in lesstif.
 */
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/RepType.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/MainWP.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/CascadeB.h>
#include <Xm/Command.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>

#include "../../common/Test.h"


Widget GXTextIn, GXTextOut, GXStatus;
#define INITIAL_WIDTH 400
static String fallback_resources[] =
{
    "*background: tan",
    "*fontList: *-*-*-medium-r-*-*-*-100-*-*-p-*-*-*",
    "*tearOffModel: TEAR_OFF_ENABLED",
    "*GLWidget.width: 400",
    "*GLWidget.height: 200",
    "*GXStatus.height: 30",
    NULL
};

static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,  444,   30, 0,0,0, /* menuBar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   61,   20, 0,0,0, /* fileButton */
   CWWidth | CWHeight | CWX | CWY,   66,    5,   70,   20, 0,0,0, /* Edit dummy */
   CWWidth | CWHeight | CWX | CWY,  136,    5,   88,   20, 0,0,0, /* Options dummy */
   CWWidth | CWHeight | CWX | CWY,  224,    5,   94,   20, 0,0,0, /* Detonate dummy */
   CWWidth | CWHeight | CWX | CWY,  318,    5,   84,   20, 0,0,0, /* Launch dummy */
   CWWidth | CWHeight | CWX | CWY,  402,    5,   37,   20, 0,0,0, /* Help */
   CWWidth | CWHeight            ,  508,  524,  400,   65, 0,0,0, /* menuBar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   61,   20, 0,0,0, /* fileButton */
   CWWidth | CWHeight | CWX | CWY,   66,    5,   70,   20, 0,0,0, /* Edit dummy */
   CWWidth | CWHeight | CWX | CWY,  136,    5,   88,   20, 0,0,0, /* Options dummy */
   CWWidth | CWHeight | CWX | CWY,  224,    5,   94,   20, 0,0,0, /* Detonate dummy */
   CWWidth | CWHeight | CWX | CWY,    5,   30,   84,   30, 0,0,0, /* Launch dummy */
   CWWidth | CWHeight | CWX | CWY,  358,   30,   37,   30, 0,0,0, /* Help */ 
};

void ShowHelp(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
  if (XmToggleButtonGetState(w))
    XtManageChild(GXStatus);
  else
    XtUnmanageChild(GXStatus);
}

void Quit(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{ 
    exit(0); 
}

void GXSetStatus(char *str) 
/* display str on status line */
{
  char *buf=str;
  if (!str) 
    buf=" ";
  {
    XmString xmstr;
    if (!GXStatus)
      return;
    xmstr=XmStringCreateLocalized(buf);
    XtVaSetValues(GXStatus,
		  XmNlabelString,xmstr,
		  NULL);
    XmStringFree(xmstr);
  }
}

void GXInitCommand(Widget parent, Boolean TEXTPORT)
/*-
 * Initialize the command area. if textport is true, grab standard output
 * and standard error and send them to a text output widget
-*/
{
    Arg args[20];
    int n;
    Widget w;

    /* make the text IO region */
    if (TEXTPORT) {
	GXTextOut = XmCreateScrolledText(parent, "GXTextOut", NULL, 0);
#ifndef __linux__
	/* FIXME */
	XmTextSetAddMode(GXTextOut, TRUE);
#endif
	XtVaSetValues(GXTextOut,
		      XmNskipAdjust, TRUE,
		      XmNeditMode, XmMULTI_LINE_EDIT,
		      XmNeditable, FALSE,
		      XmNtraversalOn, FALSE,
		      XmNwordWrap, TRUE,
		      XmNhighlightOnEnter, False,
		      XmNhighlightThickness, 0,
		      XmNrows, 6,
		      NULL);
	XtVaSetValues(XtParent(GXTextOut),
		      XmNskipAdjust, TRUE,
		      XmNleftOffset, 5,
		      XmNrightOffset, 5,
		      XmNbottomOffset, 5,
		      XmNhighlightOnEnter, FALSE,
		      XmNhighlightThickness, 0,
		      XmNresizeWidth, TRUE,
		      XmNresizeHeight, FALSE,
		      NULL);
	XtVaGetValues(XtParent(GXTextOut),
		      XmNhorizontalScrollBar, &w,
		      NULL);
	XtUnmanageChild(w);	/* kill off unneeded scrollbar */
	XtManageChild(GXTextOut);

    }

    /*- make the command Widget -*/
    GXTextIn = XmCreateTextField(parent, "GXTextIN", NULL, 0);
    XtVaSetValues(GXTextIn,
		  XmNhighlightOnEnter, False,
		  XmNhighlightThickness, 0,
		  XmNpaneMaximum, 30,
		  XmNpaneMinimum, 30,
		  XmNwidth, INITIAL_WIDTH,
		  NULL);
    XtManageChild(GXTextIn);
#if (XmVersion>=1002)		/* motif 1.2 or higher */
    XtVaSetValues(parent,
		  XmNinitialFocus, GXTextIn,
		  NULL);
#endif
}

int main(argc, argv)
int argc;
char **argv;
{
  XtAppContext app_context;
  Widget topLevel, mainWindow, menuBar, frame;
  Display *GXDisplay;
  Widget command, paned, GLWidget;

  Widget fileButton, fileMenu, quit, helpButton, helpMenu, help, helpBox;
  Widget temp;
  int n;
  Arg args[10];

  /* if compiling on a SGI, allow for non-default visuals. */
  XtToolkitInitialize();
  app_context = XtCreateApplicationContext();
  XtAppSetFallbackResources(app_context, fallback_resources);
  GXDisplay = XtOpenDisplay(app_context, NULL, "Mainw12", "Mainw12",
			    NULL, 0, &argc, argv);
  if (!GXDisplay) {
    printf("Couldn't open display. \n");
    exit(-1);
  }

  n = 0;
  topLevel = XtAppCreateShell("Mainw12", "Mainw12",
			      applicationShellWidgetClass,
			      GXDisplay, args, n);

  /* register converter for tearoff menus */
  XmRepTypeInstallTearOffModelConverter();

  /* create menu bar along top inside of main window */
  menuBar = XmCreateMenuBar(
			    topLevel, /* parent widget*/
			    "menuBar",  /* widget name */
			    NULL,       /* no arguments needed */
			    0);         /* no arguments needed */
  XtManageChild(menuBar);


  /*
   *  CREATE FILE MENU AND CHILDREN
   */

  /* create button that will pop up the menu */
  fileButton = XtVaCreateManagedWidget(
				       "fileButton",               /* widget name */
				       xmCascadeButtonWidgetClass, /* widget class */
				       menuBar,                    /* parent widget*/
				       XmNwidth, 61,
				       XmNheight, 20,
				       XmNrecomputeSize, False,
				       NULL);                      /* terminate varargs list */

  XtVaCreateManagedWidget(
			  "Edit dummy",               /* widget name */
			  xmCascadeButtonWidgetClass, /* widget class */
			  menuBar,                    /* parent widget*/
				       XmNwidth, 70,
				       XmNheight, 20,
				       XmNrecomputeSize, False,
			  NULL);                      /* terminate varargs list */

  XtVaCreateManagedWidget(
			  "Options dummy",               /* widget name */
			  xmCascadeButtonWidgetClass, /* widget class */
			  menuBar,                    /* parent widget*/
				       XmNwidth, 88,
				       XmNheight, 20,
				       XmNrecomputeSize, False,
			  NULL);                      /* terminate varargs list */

  XtVaCreateManagedWidget(
			  "Detonate dummy",               /* widget name */
			  xmCascadeButtonWidgetClass, /* widget class */
			  menuBar,                    /* parent widget*/
				       XmNwidth, 94,
				       XmNheight, 20,
				       XmNrecomputeSize, False,
			  NULL);                      /* terminate varargs list */

  XtVaCreateManagedWidget(
			  "Launch dummy",               /* widget name */
			  xmCascadeButtonWidgetClass, /* widget class */
			  menuBar,                    /* parent widget*/
				       XmNwidth, 84,
				       XmNheight, 20,
				       XmNrecomputeSize, False,
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
				       "Help",             /* widget name */
				       xmCascadeButtonWidgetClass, /* widget class */
				       menuBar,                   /* parent widget*/
				       XmNwidth, 37,
				       XmNheight, 20,
				       XmNrecomputeSize, False,
				       NULL);                     /* terminate varargs list */

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
				 "status line",                     /* widget name */
				 xmToggleButtonWidgetClass,    /* widget class */
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
  XtAddCallback(help, XmNvalueChangedCallback, ShowHelp, helpBox);

  XtRealizeWidget(topLevel);

    
  PrintDetails(topLevel,Expected);
  LessTifTestWaitForIt(topLevel);
  LessTifTestResizeWidget(topLevel, 400, 65);
  PrintDetails(topLevel,Expected);
  LessTifTestMainLoop(topLevel);

  exit(0);
}

