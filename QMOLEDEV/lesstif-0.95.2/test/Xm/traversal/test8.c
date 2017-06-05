/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

XtAppContext    app_context;
Display        *display;	/* Display             */

void            toggleb_traverse();

static XtActionsRec toggleb_actions[] = {
	{"toggleb_traverse", toggleb_traverse}
};

static char     toggleb_translations[] = {
"<Key>osfBeginLine:                            toggleb_traverse(home)\n\
<Key>osfUp:                         toggleb_traverse(up)\n\
<Key>osfDown:                           toggleb_traverse(down)\n\
<Key>osfLeft:                           toggleb_traverse(left)\n\
<Key>osfRight:                          toggleb_traverse(right)\n\
s ~m ~a <Key>Tab:                           toggleb_traverse(next_tab)\n\
~m ~a <Key>Tab:                         toggleb_traverse(prev_tab)"
};

void 
set_toggle_traversal(toggle_button)
	Widget          toggle_button;
{
	XtAppAddActions(XtWidgetToApplicationContext(toggle_button),
			toggleb_actions, XtNumber(toggleb_actions));
	XtOverrideTranslations(toggle_button, XtParseTranslationTable
			       (toggleb_translations));
}

void 
toggleb_traverse(toggle_button, event, params, num_params)
	Widget          toggle_button;
	XEvent         *event;
	String         *params;
	Cardinal       *num_params;
{
	/* Prevent keyboard traversal from an unset toggle button */
	if ((*num_params != 1) || !XmToggleButtonGetState(toggle_button))
		return;

	/* Use XmProcessTraversal() to move focus */
	if (!strcmp(params[0], "home"))
		XmProcessTraversal(toggle_button, XmTRAVERSE_HOME);
	else if (!strcmp(params[0], "up"))
		XmProcessTraversal(toggle_button, XmTRAVERSE_UP);
	else if (!strcmp(params[0], "down"))
		XmProcessTraversal(toggle_button, XmTRAVERSE_DOWN);
	else if (!strcmp(params[0], "left"))
		XmProcessTraversal(toggle_button, XmTRAVERSE_LEFT);
	else if (!strcmp(params[0], "right"))
		XmProcessTraversal(toggle_button, XmTRAVERSE_RIGHT);
	else if (!strcmp(params[0], "next_tab"))
		XmProcessTraversal(toggle_button, XmTRAVERSE_NEXT_TAB_GROUP);
	else if (!strcmp(params[0], "prev_tab"))
		XmProcessTraversal(toggle_button, XmTRAVERSE_PREV_TAB_GROUP);
}
Widget          appshell = (Widget) NULL;

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
