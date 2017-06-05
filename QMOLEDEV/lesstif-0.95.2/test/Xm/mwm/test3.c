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

Widget appshell  = (Widget) NULL;

void 
check_wm_state(shell)
	Widget          shell;
{
	Display        *display;
	Window          window;
	Atom            xa_WM_STATE;
	unsigned long  *property = NULL;
	unsigned long   nitems;
	unsigned long   leftover;
	Atom            actual_type;
	int             actual_format;

	printf("WM state is ");
	display = XtDisplay(shell);
	window = XtWindow(shell);
	/* Get WM_STATE property from window */
	xa_WM_STATE = XInternAtom(display, "WM_STATE", False);
	XGetWindowProperty(display, window, xa_WM_STATE, 0L, 1, False,
			   xa_WM_STATE, &actual_type, &actual_format,
			   &nitems, &leftover, (unsigned char **) &property);
	if (property == NULL)
		printf("Unknown\n");
	else if (*property == IconicState)
		printf("Iconic\n");
	else if (*property == NormalState)
		printf("Normal\n");
	else if (*property == WithdrawnState)
		printf("Withdrawn\n");
	/* Tidy up */
	if (property != NULL)
		XFree((char *) property);
}
void 
check_map_state(shell)
	Widget          shell;
{
	Display        *display;
	Window          window;
	XWindowAttributes win_attr;

	printf("Map state is ");
	display = XtDisplay(shell);
	window = XtWindow(shell);
	XGetWindowAttributes(display, window, &win_attr);
	if (win_attr.map_state == IsUnmapped)
		printf("Unmapped\n");
	else if (win_attr.map_state == IsUnviewable)
		printf("Unviewable\n");
	if (win_attr.map_state == IsViewable)
		printf("Viewable\n");
}
void 
watch_maps(shell, client_data, event, cont)
	Widget          shell;
	XtPointer       client_data;
	XEvent         *event;
	Boolean        *cont;
{
	if (event->type == MapNotify)
		printf("Map state changed, window is Normal\n");
	else if (event->type == UnmapNotify)
		printf("Map state changed, window is Iconified\n");
	else return;

	/* Two other ways to check the state */
	check_map_state(shell);
	check_wm_state(shell);
}
void 
set_handler(shell)
	Widget          shell;
{
	XtAddEventHandler(shell, StructureNotifyMask,
			  False, watch_maps, (XtPointer) NULL);
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

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell ( app_name, "XApplication", applicationShellWidgetClass, display, al, ac );
	ac = 0;
	label = XmCreateLabel ( appshell, "label", al, ac );
	XtManageChild ( label);

	/* Set up event handler to detect map/unmap of shell */
	set_handler(appshell);
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
   CWWidth | CWHeight            ,   50,   50,   34,   17, 0,0,0, /* label */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);
	exit (0);
}

