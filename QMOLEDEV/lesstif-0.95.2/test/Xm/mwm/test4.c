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
set_icon_window(shell, pixmap)
	Widget          shell;
	Pixmap          pixmap;
{
	Window          icon_win, root_win;
	int             x, y;
	unsigned int    width, height;
	unsigned int    border_width;
	unsigned int    depth;
	/* Get width, height etc. from the pixmap */
	XGetGeometry(XtDisplay(shell), pixmap,
		     &root_win, &x, &y,
		     &width, &height, &border_width, &depth);
	/* Create window and set background pixmap */
	icon_win = XCreateSimpleWindow(XtDisplay(shell),
				    root_win, 0, 0, width, height, 0, 0, 0);
	XSetWindowBackgroundPixmap(XtDisplay(shell), icon_win, pixmap);
	XtVaSetValues(shell, XmNiconWindow, icon_win, NULL);
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
	Pixel           fg, bg;
	Pixmap          pixmap;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Icon Window"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	xmstring = XmStringCreateLtoR("Iconise me!", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	label = XmCreateLabel(appshell, "label", al, ac);
	XmStringFree(xmstring);

	XtManageChild(label);

	/*
	* Get a pixmap, and use it as the background for the icon
	* window. Take the colours from the label widget
	*/
	ac = 0;
	XtSetArg(al[ac], XmNforeground, &fg); ac++;
	XtSetArg(al[ac], XmNbackground, &bg); ac++;
	XtGetValues(label, al, ac);
	pixmap = XmGetPixmap(XtScreen(appshell),
					"/usr/include/X11/bitmaps/mailfull",
					fg, bg);
	set_icon_window(appshell, pixmap);
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
