/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <stdio.h>

Widget          appshell = (Widget) NULL;
Widget          label = (Widget) NULL;


void 
create_msg_shell(parent, screen_num)
	Widget          parent;
	int             screen_num;
{
	Display        *display = XtDisplay(parent);
	Arg             al[64];
	register int    ac = 0;
	Widget          msg_shell;
	Widget          msg;

	XtSetArg(al[ac], XmNallowShellResize, True);
	ac++;
	XtSetArg(al[ac], XmNscreen, XScreenOfDisplay(display, screen_num));
	ac++;
	XtSetArg(al[ac], XmNvisual, XDefaultVisual(display, screen_num));
	ac++;
	XtSetArg(al[ac], XmNcolormap, XDefaultColormap(display,
						       screen_num));
	ac++;
	XtSetArg(al[ac], XmNdepth, XDefaultDepth(display, screen_num));
	ac++;
	msg_shell = XmCreateDialogShell(parent, "msg_shell", al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNdialogType, XmDIALOG_INFORMATION);
	ac++;
	msg = XmCreateMessageBox(msg_shell, "msg", al, ac);
}
void 
create_appshell(display, app_name, app_argc, app_argv)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
{
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */
	XmString        xmstring;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Main Window"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	xmstring = XmStringCreateLtoR("Message dialog will be on another\nscreen, if there is one", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	label = XmCreateLabel(appshell, "label", al, ac);
	ac = 0;
	XmStringFree(xmstring);
	XtManageChild(label);
}

XtAppContext    app_context;
Display        *display;

int 
main(argc, argv)
	int             argc;
	char          **argv;
{
	int             i;
	int             screens;
	int             default_screen;

	XtSetLanguageProc((XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL);
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
	display = XtOpenDisplay(app_context, NULL, argv[0], "XApplication",
				NULL, 0, &argc, argv);
	if (!display) {
		printf("%s: can't open display, exiting...\n", argv[0]);
		exit(-1);
	}
	/*
	 * Create the ApplicationShell on the default screen, and an
	 * XmDialogShell with an XmMessageBox on all the other screens (if
	 * any)
	 */
	screens = ScreenCount(display);
	if (screens == 1) {
		printf("%s: server only has one screen, exiting...\n", argv[0]);
		exit(0);
	}
	default_screen = DefaultScreen(display);
	create_appshell(display, argv[0], argc, argv);
	for (i = 0; i < screens; i++) {
		if (i != default_screen) {
			printf("Creating message shell on screen %d\n", i);
			create_msg_shell(appshell, i);
		}
	}

	XtRealizeWidget(appshell);
	
	/* NO geometry (unless you have 2 screens) */
	LessTifTestMainLoop(appshell);
	exit(0);
}
