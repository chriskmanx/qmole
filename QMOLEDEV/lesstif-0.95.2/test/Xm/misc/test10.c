/* just like test9, only by eventhandler than by translation */
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>

void 
handle_drag(widget, client_data, event, cont)
	Widget          widget;
	XtPointer       client_data;
	XEvent         *event;
	Boolean        *cont;
{
	/* Handle dragging of a label in a drawing area */

	static int      x_offset, y_offset;
	Position        x, y;

	switch (event->type) {
	case ButtonPress:
		/*
		 * Work out the offset used to translate between root window
		 * coordinates (available in the event structures) and the
		 * position of top/left of the label and remember it
		 */
		if (event->xbutton.button != Button1)
			break;
		XtVaGetValues(widget, XmNx, &x, XmNy, &y, NULL);
		x_offset = (int) x - event->xbutton.x_root;
		y_offset = (int) y - event->xbutton.y_root;
		break;

	case MotionNotify:
		/* Mouse has moved, move the label in response */
		x = event->xbutton.x_root + x_offset;
		y = event->xbutton.y_root + y_offset;
		XtVaSetValues(widget, XmNx, x, XmNy, y, NULL);
		break;
	}
	return;
}

int
main(argc, argv)
	int             argc;
	char          **argv;
{
	XtAppContext    app_context;
	Display        *display;
	Widget          shell;
	Widget          drawarea;
	Widget          drag_label;
	Arg             al[3];
	int             ac;
	char           *app_name = argv[0];
	XmString        xmstring;

	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();

	/* XtOpenDisplay() builds the resource database */
	display = XtOpenDisplay(app_context, NULL, "name", "Class", NULL, 0,
				&argc, argv);

	ac = 0;
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	shell = XtAppCreateShell(app_name, "XApplication",
			      applicationShellWidgetClass, display, al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNwidth, 400); ac++;
	XtSetArg(al[ac], XmNheight, 400); ac++;
	XtSetArg(al[ac], XmNresizePolicy, XmRESIZE_GROW); ac++;
	drawarea = XmCreateDrawingArea(shell, "drawarea", al, ac);
	ac = 0;
	xmstring = XmStringCreateLtoR("Drag me!",
				  (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	drag_label = XmCreateLabel(drawarea, "drag_label", al, ac);

	XmStringFree(xmstring);

	/* Add event handler */
	XtAddEventHandler (drag_label, ButtonPressMask|Button1MotionMask,
				    False, handle_drag, NULL);

	XtManageChild(drag_label);
	XtManageChild(drawarea);

	XtRealizeWidget(shell);
	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  400,  400, 0,0,0, /* drawarea */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   52,   17, 0,0,0, /* drag_label */ 
    };
    PrintDetails(shell,Expected);
};
	LessTifTestMainLoop(shell);

	exit(0);
}
