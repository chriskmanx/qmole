/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Label.h>
#include <stdio.h>

Widget          appshell = (Widget) NULL;
Widget          label = (Widget) NULL;

void 
set_icon_pixmap(shell)
	Widget          shell;
{
	Pixmap          new_pixmap;

	new_pixmap = XmGetPixmapByDepth(XtScreen(shell),
					"/usr/include/X11/bitmaps/mailfull",
					1, 0, 1);
	XtVaSetValues(shell, XmNiconPixmap, new_pixmap, NULL);
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
	XtSetArg(al[ac], XmNtitle, "Icon Pixmap"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	xmstring = XmStringCreateLtoR("Iconise me!", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	label = XmCreateLabel(appshell, "label", al, ac);
	XmStringFree(xmstring);

	XtManageChild(label);

	/* Set icon on the shell */
	set_icon_pixmap(appshell);
}

XtAppContext    app_context;
Display        *display;	/* Display             */

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
   CWWidth | CWHeight            ,   50,   50,   70,   17, 0,0,0, /* label */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);
	exit(0);
}
