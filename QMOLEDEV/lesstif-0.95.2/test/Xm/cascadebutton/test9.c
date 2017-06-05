/* $Header: /cvsroot/lesstif/lesstif/test/Xm/cascadebutton/test9.c,v 1.2 2001/05/23 13:17:15 amai Exp $
From:        "Edward A. Falk" <falconer@best.com>
To:          lesstif@lesstif.org
Subject:     Cascade button bug report -- set menu id causes redraw
Date:        Mon, 20 Mar 2000 14:38:54 -0800 (PST)
*/

/* Show bug in which a Cascade button undraws itself when the
 * application sets its menu.
 *
 * This program creates the cascade button and then later comes back
 * and assigns a menu to it.  (This test program is modelled after an
 * application in which the larger menus are created in a work procedure.)
 *
 * When the cascade button is created, it appears correctly.  Later,
 * when a menu is assigned to it, it disappears from the screen and
 * then later re-appears.
 *
 * Conjecture: the set_values() function in CascadeB.c thinks that
 * changing the menu requires a redraw, and is clearing its window.
 * I suppose that this might be required for Option menus, but it shouldn't
 * be needed for Pulldown menus.
 *
 * Observation:  single-stepping through the code shows that setting
 * the submenu id causes this expression to evaluate as True:
 *
 *	if (CB_CascadePixmap(old) != CB_CascadePixmap(new_w) ||
 *	    (CB_Submenu(new_w)
 *	     ? CB_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP
 *	     : CB_ArmedPixmap(new_w)) ||
 *	    (CB_ArmedPixmap(new_w) && Lab_Font(old) != Lab_Font(new_w)))
 *
 * which sets refresh_needed = True;
 *
 *
 * Work-around:  Instead of creating the entire menu in a work procedure,
 * create an empty menu at the start, and only create the menu items in
 * the work procedure.  The cascade button should be made insensitive
 * first, to prevent the user from popping the menu up before it's ready.
 *
 * Update: no, that doesn't work either; setting the cascade button
 * insensitive causes it to appear in the wrong size.
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>

#include <X11/Xmu/Editres.h>


	/* Xt stuff */

static	XtAppContext	app_ctx ;

static	Widget	topLevel ;
static	Widget	mainWindow ;
static	Widget	menubar ;
static	Widget	menubutton ;
static	Widget	menu ;

static	void	workProc(XtPointer, XtIntervalId *) ;

static	String	fallback[] = {
  "*filebutton.labelString:	File",
  "*openitem.labelString:	Open",
  "*quititem.labelString:	Quit",
  NULL
};

static	int	workCount ;

int
main(int argc, char **argv)
{
	XtSetLanguageProc(NULL,NULL,NULL) ;

	topLevel = XtVaAppInitialize(&app_ctx, "Demos", NULL,0, &argc,argv,
		fallback,NULL) ;

	XSynchronize(XtDisplay(topLevel), True) ;

	/*
	XtAddEventHandler(topLevel, 0, True, _XEditResCheckMessages, NULL);
	*/

	mainWindow = XtVaCreateWidget("mainWindow",
		xmMainWindowWidgetClass, topLevel,
		NULL) ;

	menubar = XmVaCreateSimpleMenuBar(mainWindow, "menubar", 0) ;

	menubutton = XmCreateCascadeButton(menubar, "filebutton", NULL,0) ;
	XtManageChild(menubutton) ;

	XtManageChild(menubar) ;
	XtManageChild(mainWindow) ;

	XtRealizeWidget(topLevel) ;

	/* Create menu in a work proc.  This gives the application
	 * a chance to appear on the screen first.
	 */

	XtAppAddTimeOut(app_ctx, 5000, workProc, NULL) ;

	LessTifTestMainLoop(topLevel);

	exit(0) ;
}


	/* this work proc is called 5 seconds after the main window
	 * appears on the screen.  It creates a menu for the
	 * cascade button, then waits a few more seconds before
	 * returning.
	 */

static	void
workProc(XtPointer client, XtIntervalId *id)
{
	Widget		item ;

	menu = XmCreatePulldownMenu(menubar, "filemenu", NULL,0) ;
	XtVaSetValues(menubutton, XmNsubMenuId, menu, NULL) ;

	item = XmCreatePushButton(menu, "quititem", NULL,0) ;
	XtManageChild(item) ;

	sleep(5) ;
}
