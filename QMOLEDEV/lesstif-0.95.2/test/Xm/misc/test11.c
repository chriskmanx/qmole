/*
 **LIBS: -lXm -lXt -lX11
 ** keysym handling
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/DialogS.h>
#include <Xm/Label.h>
#include <stdio.h>

Widget shell = (Widget) NULL;

void 
keys(widget, event, params, num_params)
	Widget          widget;
	XEvent         *event;
	String         *params;
	Cardinal       *num_params;
{
	char            strbuf[128];
	int             num_bytes;
	KeySym          ks;
	XComposeStatus  stat;
	Modifiers       mods;

	printf("Keycode 0x%x modifiers 0x%x\n", event->xkey.keycode,
	       event->xkey.state);
	num_bytes = XLookupString(&(event->xkey), strbuf, 127, &ks, &stat);
	printf("X keysym 0x%x (%s)\n", (unsigned int)ks, XKeysymToString(ks));
	strbuf[num_bytes] = '\0';
	printf("X keystring %s\n", strbuf);
	XtTranslateKeycode(XtDisplay(widget), event->xkey.keycode,
			   event->xkey.state, &mods, &ks);
	printf("Xt keysym 0x%x (%s)\n", (unsigned int)ks, XKeysymToString(ks));
	printf("\n");
}

static XtActionsRec keys_actions[] = {
	{"keys", keys}
};

void 
reg_keys(w)
	Widget          w;
{
	XtAppAddActions(XtWidgetToApplicationContext(w), keys_actions,
			XtNumber(keys_actions));
}

void 
create_shell(display, app_name, app_argc, app_argv)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
{
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */
	XmString        xmstring;
	Widget          bullboard = (Widget) NULL;
	Widget          label = (Widget) NULL;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	shell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	bullboard = XmCreateBulletinBoard(shell, "bullboard", al, ac);
	ac = 0;
	xmstring = XmStringCreateLtoR("Key entry", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	label = XmCreateLabel(bullboard, "label", al, ac);
	ac = 0;
	XmStringFree(xmstring);
	/*
	 * Register action proc for all keystrokes and set up translations on
	 * XmLabel to invoke it.
	 */
	reg_keys(shell);
	XtUninstallTranslations(label);
	XtOverrideTranslations(label, XtParseTranslationTable("<Key>: keys()"));
	XtManageChild(label);
	XtManageChild(bullboard);
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
	create_shell(display, argv[0], argc, argv);
	XtRealizeWidget(shell);
	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   79,   38, 0,0,0, /* bullboard */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   58,   17, 0,0,0, /* label */ 
    };
    PrintDetails(shell,Expected);
};
	LessTifTestMainLoop(shell);
	exit(0);
}
