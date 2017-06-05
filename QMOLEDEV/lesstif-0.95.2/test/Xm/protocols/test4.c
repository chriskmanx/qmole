/*
**LIBS: -lXm -lXt -lX11
*/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Label.h>
#include <stdio.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>

Widget appshell  = (Widget) NULL;

extern void     cb_handle_mwmspecial();

void 
set_mwmspecial_cb(app_shell)
	Widget          app_shell;
{
	char            buf[64];

	Atom            xa__MOTIF_WM_MESSAGES;
	Atom            xa__MYAPPLICATION_SPECIAL;
	/* Convert protocol and message name to atom */
	xa__MOTIF_WM_MESSAGES = XmInternAtom(XtDisplay(app_shell),
					     "_MOTIF_WM_MESSAGES", False);
	xa__MYAPPLICATION_SPECIAL = XmInternAtom(XtDisplay(app_shell),
					   "_MYAPPLICATION_SPECIAL", False);
	/* Add protocol callback */
	XmAddProtocolCallback(app_shell, xa__MOTIF_WM_MESSAGES,
			      xa__MYAPPLICATION_SPECIAL,
			      cb_handle_mwmspecial, (XtPointer) NULL);

	/* Set mwmMenu to add extra item to window menu */
	sprintf(buf, "Special _S Ctrl<Key>s f.send_msg %ld",
		xa__MYAPPLICATION_SPECIAL);
	XtVaSetValues(app_shell, XmNmwmMenu, buf, NULL);
}

void
cb_handle_mwmspecial(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	char           *s;
	XmAnyCallbackStruct *cd;
	XClientMessageEvent *cm;

	cd = (XmAnyCallbackStruct *) call_data;
	fprintf(stderr, "Callback reason is %d\n", cd->reason);
	if (cd->event->type == ClientMessage) {
		fprintf(stderr, "type is ClientMessage\n");
		cm = &(cd->event->xclient);
		s = XmGetAtomName(XtDisplay(w), cm->message_type);
		fprintf(stderr, "message_type is %s\n", s);
		XFree(s);
		s = XmGetAtomName(XtDisplay(w), cm->data.l[0]);
		fprintf(stderr, "message is %s\n", s);
		XFree(s);
	} else
		fprintf(stderr, "Message type is %d\n", cd->event->type);
	/* Won't happen */
}

void create_appshell (display, app_name, app_argc, app_argv)
Display *display;
char *app_name;
int app_argc;
char **app_argv;
{
	Arg al[64];                    /* Arg List */
	register int ac = 0;           /* Arg Count */
	Widget label = (Widget)NULL;
	XmString xmstring;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	XtSetArg(al[ac], XmNtitle, "Private protocol handler"); ac++;
	appshell = XtAppCreateShell ( app_name, "XApplication", applicationShellWidgetClass, display, al, ac );
	ac = 0;
	xmstring = XmStringCreateLtoR("Select Special from mwm menu", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	label = XmCreateLabel ( appshell, "label", al, ac );
	XmStringFree(xmstring);
	XtManageChild ( label);

	/* Set up callback for private protocol*/
	set_mwmspecial_cb(appshell);
}

XtAppContext app_context;
Display *display;       /*  Display             */

int main (argc,argv)
int    argc;
char            **argv;
{
	XtSetLanguageProc ( (XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL );
	XtToolkitInitialize ();
	app_context = XtCreateApplicationContext ();
	display = XtOpenDisplay (app_context, NULL, argv[0], "XApplication",
				 NULL, 0, &argc, argv);
	if (!display)
	{
	    printf("%s: can't open display, exiting...\n", argv[0]);
	    exit (-1);
	}
	create_appshell ( display, argv[0], argc, argv );
	XtRealizeWidget (appshell);
	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  172,   17, 0,0,0, /* label */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);
	exit (0);
}

