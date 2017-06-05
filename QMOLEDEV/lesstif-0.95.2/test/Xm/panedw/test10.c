/* $Header: /cvsroot/lesstif/lesstif/test/Xm/panedw/test10.c,v 1.4 2002/05/03 12:03:41 amai Exp $
From:        dbl@ics.com (David B. Lewis)
To:          lesstif@hungry.com
Subject:     bug in LessTif 0.87.0
Date:        Mon, 23 Nov 1998 11:29:54 -0500

SPARC Solaris 2.4 system, X11R5 (from OpenWindows).

There appear to be general problems with the XmPanedWindow, as this simple
test shows. If you move the sashes around such that two "blue" guider lines
appear (i.e. you move a sash past another sash, such that the mouse now
controls the placement of both sashes), the lines are not correctly unerased
when the sash is released. I think also the final sizing of the widgets is
different from the Open Group version of the widget, but I'm not sure.
*/


/* simple test program */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>

#ifdef EPAK
#include <Xi/XiEPakAll.h>
#endif

#include "../../common/Test.h"

static int done = 0;

static char *explanation[] = {
"Test this proposition (array of XmString):",
};

static void doExplanation(void)
{
	int i;
	for (i = 0; i<XtNumber(explanation); i++) printf("%s\n",explanation[i]);
}

static void quitCB(Widget w, XtPointer client, XtPointer call) 
{
	done = 1;
}

static void createQuit(Widget quit_parent)
{
	Widget button = XmCreatePushButton(quit_parent, "quit", NULL, 0);
	XtManageChild(button);
	XtAddCallback(button, XmNactivateCallback, quitCB, (XtPointer)NULL);
}

static void createScreen(Widget parent)
{
	Widget top = XmCreatePanedWindow(parent, "pane", NULL,0);
	XtManageChild(top);

	{
		char *labelArray[]= 
		{"Label One", "Label Two", "Label Three", "Label Four", 
			"Label Five"};
		int i;

		for (i=0; i < XtNumber(labelArray); i++)
		{
			Arg     al[10];
			int     ac;
			Widget button;
			XmString tcs;
			
			tcs = XmStringCreateLocalized(labelArray[i]);
			ac = 0;
			XtSetArg(al[ac], XmNlabelString, tcs); ac++;
			button= XmCreatePushButton(top,"button",al,ac);
			XtManageChild(button);
			XmStringFree(tcs);
		}


	}

	createQuit(top);
}

#define CLASS "Test"

int main (argc,argv)
int  argc;
char *argv[];
{
	XtAppContext app_context;
	Widget app_shell;
	Display *display;

	doExplanation();

	XtToolkitInitialize ();
	app_context = XtCreateApplicationContext();
	display = XtOpenDisplay (app_context, NULL, argv[0], CLASS,
			NULL, 0, &argc, argv);
	if (!display)
	{
		XtWarning ("can't open display, exiting...");
		exit (0);
	}

	app_shell = XtAppCreateShell (NULL, CLASS,
			applicationShellWidgetClass, display, NULL, 0);
	XtVaSetValues(app_shell,XmNallowShellResize, True, NULL);

	/* create application */
	createScreen(app_shell);
	
	XtRealizeWidget(app_shell);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   84,  196, 0,0,0, /* pane */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   78,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    3,   36,   78,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    3,   69,   78,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    3,  102,   78,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    3,  135,   78,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    3,  168,   78,   25, 0,0,0, /* quit */
   CWWidth | CWHeight | CWX | CWY,   64,  159,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,  163,   84,    2, 0,0,0, /* separator */
   CWWidth | CWHeight | CWX | CWY,   64,  126,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,  130,   84,    2, 0,0,0, /* separator */
   CWWidth | CWHeight | CWX | CWY,   64,   93,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   97,   84,    2, 0,0,0, /* separator */
   CWWidth | CWHeight | CWX | CWY,   64,   60,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   64,   84,    2, 0,0,0, /* separator */
   CWWidth | CWHeight | CWX | CWY,   64,   27,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   31,   84,    2, 0,0,0, /* separator */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(app_shell, Expected);
}
	LessTifTestMainLoop(app_shell);

	/*	Process events, unwrapping correctly.  */
	/*
	while (!done)
	{
		XEvent event;
		XtAppNextEvent(app_context, &event);
        	XtDispatchEvent(&event);
	}

	XtDestroyWidget(app_shell);
	XtDestroyApplicationContext(app_context);
	*/
	exit(0);
}
