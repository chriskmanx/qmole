#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <stdio.h>

int	save_argc;
char	**save_argv;

void
save_state(Widget w, XtPointer cd, XtPointer cbs)
{
	Widget top = (Widget)cd;

	fprintf(stderr, "Save State\n");
	XSetCommand(XtDisplay(top), XtWindow(top), save_argv, save_argc);
}

void
Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

int
main(int argc, char *argv[])
{
	Widget		toplevel, button;
	XtAppContext	app;
	Atom		wm_save_yourself;

	save_argv = (char **)XtMalloc(argc * sizeof(char *));
	for (save_argc=0; save_argc < argc; save_argc++)
		if (! strcmp(argv[save_argc], "-restart")) {
			argc -= 2;
			save_argc--;
		} else
			save_argv[save_argc] = strcpy(XtMalloc(strlen(argv[save_argc])+1),
				argv[save_argc]);

	toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0, &argc, argv, NULL,
			XmNwidth,	100,
			XmNheight,	100,
		NULL);

	wm_save_yourself = XmInternAtom(XtDisplay(toplevel), "WM_SAVE_YOURSELF", False);
	XmAddWMProtocols(toplevel, &wm_save_yourself, 1);
	XmAddWMProtocolCallback(toplevel, wm_save_yourself, save_state, toplevel);

	button = XtVaCreateManagedWidget("Quit", xmPushButtonWidgetClass, toplevel,
		NULL);
	XtAddCallback(button, XmNactivateCallback, Quit, NULL);

	XtRealizeWidget(toplevel);
	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,  100,  100, 0,0,0, /* Quit */ 
    };
    PrintDetails(	toplevel ,Expected);
};
   LessTifTestMainLoop(	toplevel );
	exit(0);
}
