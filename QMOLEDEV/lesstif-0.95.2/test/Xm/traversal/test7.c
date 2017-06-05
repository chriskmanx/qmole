/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#include <Xm/Xm.h>
#include <stdio.h>

XtAppContext    app_context;
Display        *display;	/* Display             */

Widget          appshell = (Widget) NULL;

void            toggleb_traverse_back();

void 
set_focus(toggle_button, id)
	XtPointer       toggle_button;
	XtIntervalId   *id;
{
	/* Traverse back to the toggle button */
	XmProcessTraversal((Widget) toggle_button,
			   XmTRAVERSE_CURRENT);
}

void 
set_toggle_traversal(toggle_button)
	Widget          toggle_button;
{
	XtAddEventHandler(toggle_button, FocusChangeMask, False,
			  toggleb_traverse_back, NULL);
}

void 
toggleb_traverse_back(toggle_button, client_data, event, cont)
	Widget          toggle_button;
	XtPointer       client_data;
	XEvent         *event;
	Boolean        *cont;
{
	/* Force focus back to an unset toggle button */
	/*
	 * Set zero interval timer to force focus back after
	 * XmProcessTraversal() has finished its processing
	 */
	if (event->type == FocusOut && !XmToggleButtonGetState
	    (toggle_button))
		XtAppAddTimeOut(XtWidgetToApplicationContext(toggle_button), 0,
				set_focus, (XtPointer) toggle_button);
}
void 
create_appshell(display, app_name, app_argc, app_argv)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
{
	Widget          children[5];	/* Children to manage */
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */
	Widget          rowcol = (Widget) NULL;
	Widget          b1 = (Widget) NULL;
	Widget          textfield = (Widget) NULL;
	Widget          b2 = (Widget) NULL;
	Widget          toggleb = (Widget) NULL;
	Widget          b3 = (Widget) NULL;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Input Focus"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	XtSetArg(al[ac], XmNkeyboardFocusPolicy, XmEXPLICIT); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	rowcol = XmCreateRowColumn(appshell, "rowcol", al, ac);
	b1 = XmCreatePushButton(rowcol, "b1", al, ac);
	textfield = XmCreateTextField(rowcol, "textfield", al, ac);
	b2 = XmCreatePushButton(rowcol, "b2", al, ac);
	toggleb = XmCreateToggleButton(rowcol, "toggleb", al, ac);
	set_toggle_traversal(toggleb);
	b3 = XmCreatePushButton(rowcol, "b3", al, ac);

	children[ac++] = b1;
	children[ac++] = textfield;
	children[ac++] = b2;
	children[ac++] = toggleb;
	children[ac++] = b3;
	XtManageChildren(children, ac);
	ac = 0;
	XtManageChild(rowcol);
}
int 
main(argc, argv)
	int             argc;
	char          **argv;
{
	XtSetLanguageProc((XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL);
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
	display = XtOpenDisplay(app_context, NULL, argv[0], "XApplication",
				NULL, 0, &argc, argv);
	if (!display) {
		printf("%s: can't open display, exiting...\n", argv[0]);
		exit(-1);
	}
	create_appshell(display, argv[0], argc, argv);
	XtRealizeWidget(appshell);
	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  144,  149, 0,0,0, /* rowcol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   25, 0,0,0, /* b1 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,  138,   31, 0,0,0, /* textfield */
   CWWidth | CWHeight | CWX | CWY,    3,   65,  138,   25, 0,0,0, /* b2 */
   CWWidth | CWHeight | CWX | CWY,    3,   93,  138,   25, 0,0,0, /* toggleb */
   CWWidth | CWHeight | CWX | CWY,    3,  121,  138,   25, 0,0,0, /* b3 */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);
	exit(0);
}
