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

Widget appshell  = (Widget) NULL;


void     cb_handle_saveyrself();

void 
set_saveyrself_cb(app_shell)
	Widget          app_shell;
{
	Atom            xa_WM_SAVE_YOURSELF;
	/* Disable default deleteResponse */
	XtVaSetValues(app_shell, XmNdeleteResponse, XmDO_NOTHING,
		      NULL);
	/* Convert message name to atom */
	xa_WM_SAVE_YOURSELF = XmInternAtom(
				   XtDisplay(app_shell), "WM_SAVE_YOURSELF",
					   False);
	/* Add protocol callback */
	XmAddWMProtocolCallback(app_shell, xa_WM_SAVE_YOURSELF,
				cb_handle_saveyrself, (XtPointer) NULL);
}

void 
cb_handle_saveyrself(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	char          **restart_argv;
	int             restart_argc;
	/* Save internal state and work out restart command */
	printf("Saving myself...\n");
	restart_argc = 1;
	restart_argv=(char **)XtMalloc(restart_argc*sizeof(char *));
	restart_argv[0] = "main";

	/* Update WM_COMMAND property */
		XSetCommand(XtDisplay(w), XtWindow(w), restart_argv, restart_argc);
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
	XtSetArg(al[ac], XmNtitle, "WM_SAVE_YOURSELF handler"); ac++;
	appshell = XtAppCreateShell ( app_name, "XApplication", applicationShellWidgetClass, display, al, ac );
	ac = 0;
	xmstring = XmStringCreateLtoR("Close me!", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	label = XmCreateLabel ( appshell, "label", al, ac );
	XmStringFree(xmstring);
	XtManageChild ( label);

	/* Set up callback for WM_SAVE_YOURSELF */
	set_saveyrself_cb(appshell);
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
   CWWidth | CWHeight            ,   50,   50,   58,   17, 0,0,0, /* label */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);
	exit (0);
}

