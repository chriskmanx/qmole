/* $Id: test1.c,v 1.1 2002/05/14 23:01:27 dannybackx Exp $ */
/*
 * Copyright 1994 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.	John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose.	It is provided "as is" without express or implied warranty.
 *
 * John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
 * regard to this software, including all implied warranties of
 * merchantability and fitness, in no event shall John L. Cwikla or
 * Wolfram Research, Inc be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Author:
 *	John L. Cwikla
 *	X Programmer
 *	Wolfram Research Inc.
 *
 *	cwikla@wri.com
*/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Core.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeBG.h>

#include <stdio.h>

#include "MegaB.h"

#define APPNAME "MegaBTest"
#define APPCLASS "MegaBTest"

#define ICOUNT 200

static void activate(Widget _w, XtPointer _nil, XtPointer _call)
{
	char *string;
	XmMegaButtonCallbackStruct *mtbcs = (XmMegaButtonCallbackStruct *)_call;

	XmStringGetLtoR(mtbcs->string, XmSTRING_DEFAULT_CHARSET, &string);
	printf("%d %s\n", mtbcs->pos, string);
	XmStringFree(mtbcs->string);
	XtFree(string);
}


int
main(int argc, char **argv)
{
	Widget toplevel, menubar, popup, menub, megatb, pulldown;
	XtAppContext app;
	Display *theDisplay;
	Arg warg[8];
	int n, i;
	XmString xmstrings[ICOUNT];
	char buffer[100];

	XtToolkitInitialize();
	app = XtCreateApplicationContext();
	
	theDisplay = XtOpenDisplay (app, NULL, APPNAME, APPCLASS, 
		NULL, 0, &argc, argv);

	if (!theDisplay)
	{
		printf("%s: can't open display, exiting...", APPNAME);
		exit (0);
	}

	toplevel = XtAppCreateShell (APPNAME, APPCLASS,
		applicationShellWidgetClass, theDisplay, NULL, 0);

	menubar = XmCreateMenuBar(toplevel, "menubar", NULL, 0);
	XtManageChild(menubar);
	menub = XtCreateManagedWidget("button", xmCascadeButtonGadgetClass, menubar, NULL, 0);
	pulldown = XmCreatePulldownMenu(menubar, "pulldown", NULL, 0);

    n = 0;
    XtSetArg(warg[n], XmNsubMenuId, pulldown); n++;
    XtSetValues(menub, warg, n);

    menub = XtCreateManagedWidget("cascade", xmCascadeButtonGadgetClass, pulldown, NULL, 0);
    popup = XmCreatePulldownMenu(pulldown, "popup", NULL, 0);

	n = 0;
	XtSetArg(warg[n], XmNsubMenuId, popup); n++;
	XtSetValues(menub, warg, n);

	for(i=0;i<ICOUNT;i++)
	{
        sprintf(buffer, "Button %d", i);
        xmstrings[i] = XmStringCreateSimple(buffer);
    }

    n = 0;
    XtSetArg(warg[n], XmNitems, xmstrings); n++;
    XtSetArg(warg[n], XmNitemCount, ICOUNT); n++;
	XtSetArg(warg[n], XmNbuttonMode, XmMODE_TOGGLE_BUTTON); n++;
    megatb = XtCreateManagedWidget("_megaButton", xmMegaButtonWidgetClass, popup, warg, n);

	XtAddCallback(megatb, XmNactivateCallback, activate, NULL);

    menub = XtCreateManagedWidget("cascade", xmCascadeButtonGadgetClass, pulldown, NULL, 0);
    popup = XmCreatePulldownMenu(pulldown, "popup", NULL, 0);

    n = 0;
    XtSetArg(warg[n], XmNsubMenuId, popup); n++;
    XtSetValues(menub, warg, n);

    n = 0;
	XtSetArg(warg[n], XmNbuttonMode, XmMODE_PUSH_BUTTON); n++;
    megatb = XtCreateManagedWidget("_megaButton", xmMegaButtonWidgetClass, popup, warg, n);

	for(i=0;i<ICOUNT;i++)
		XmMegaButtonAddItem(megatb, xmstrings[i], 0, NULL);

    XtAddCallback(megatb, XmNactivateCallback, activate, NULL);

	XtRealizeWidget(toplevel);

	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,   62,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   52,   21, 0,0,0, /* button */ 
    };
    PrintDetails(toplevel,Expected);
};
	LessTifTestMainLoop(toplevel);
	exit(0);
}
