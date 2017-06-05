/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/protocols/test2.c,v 1.3 2002/05/03 12:03:41 amai Exp $
 */

#include <stdlib.h>
#include <stdio.h>

#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/Shell.h>

#include <Xm/XmP.h>
#include <Xm/DialogS.h>
#include <Xm/Label.h>
#include <X11/ShellP.h>


#include "../../common/Test.h"


Widget appshell  = (Widget) NULL;

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/VendorSEP.h>
#include <Xm/AtomMgr.h>
#include <stdio.h>

void     cb_handle_delete();
void     cb_handle_delete2();

void 
set_delete_cb(app_shell)
	Widget          app_shell;
{
	Atom            xa_WM_DELETE_WINDOW;

#if 1
	/* Disable default deleteResponse */
	XtVaSetValues(app_shell, XmNdeleteResponse, XmDO_NOTHING,
		      NULL);
#endif

	/* Convert message name to atom */
	xa_WM_DELETE_WINDOW = XmInternAtom(XtDisplay(app_shell),
					   "WM_DELETE_WINDOW", False);
	/* Add protocol callback */
	XmAddWMProtocolCallback(app_shell, xa_WM_DELETE_WINDOW,
				cb_handle_delete, (XtPointer) NULL);
	XmAddWMProtocolCallback(app_shell, xa_WM_DELETE_WINDOW,
				cb_handle_delete2, (XtPointer) NULL);
}

void 
cb_handle_delete(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	char           *s;
	XmAnyCallbackStruct *cd;
	XClientMessageEvent *cm;

	cd = (XmAnyCallbackStruct *) call_data;
	if (cd == NULL) {
		fprintf(stderr, "NULL call_data !!\n");
		return;
	}
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

void 
cb_handle_delete2(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
    printf("callback2\n");
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
	XtSetArg(al[ac], XmNtitle, "WM_DELETE_WINDOW handler"); ac++;
	appshell = XtAppCreateShell ( app_name, "XApplication", applicationShellWidgetClass, display, al, ac );
	ac = 0;
	xmstring = XmStringCreateLtoR("Close me!", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	label = XmCreateLabel ( appshell, "label", al, ac );
	XmStringFree(xmstring);
	XtManageChild ( label);

	/* Set up callback for WM_DELETE_WINDOW */
	set_delete_cb(appshell);
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
   CWWidth | CWHeight            ,   57,   73,   58,   17, 0,0,0, /* label */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);
	exit (0);
}
