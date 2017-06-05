/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test9.c,v 1.3 2002/04/17 16:22:01 amai Exp $
   Actions */

#include <stdlib.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>
#include <Xm/Label.h>

void 
handle_drag(widget, event, params, num_params)
	Widget          widget;
	XEvent         *event;
	String         *params;
	Cardinal       *num_params;
{
	/* Handle dragging of a label in a drawing area */

	static int      x_offset, y_offset;
	Position        x, y;

	if (*num_params != 1)
		return;
	if (!strcmp(params[0], "start")) {
		/*
		 * Work out the offset used to translate between root window
		 * coordinates (available in the event structures) and the
		 * position of top/left of the label and remember it
		 */
		if (event->type != ButtonPress)
			return;
		XtVaGetValues(widget, XmNx, &x, XmNy, &y, NULL);
		x_offset = (int) x - event->xbutton.x_root;
		y_offset = (int) y - event->xbutton.y_root;
		return;
	}
	if (!strcmp(params[0], "move")) {
		/* Mouse has moved, move the label in response */
		if (event->type != MotionNotify)
			return;
		x = event->xbutton.x_root + x_offset;
		y = event->xbutton.y_root + y_offset;
		XtVaSetValues(widget, XmNx, x, XmNy, y, NULL);
		return;
	}
	return;
}
static XtActionsRec drag_actions[] = {
	{"handle_drag", handle_drag}
};

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

	XtAppAddActions(app_context, drag_actions, XtNumber(drag_actions));
	XtAugmentTranslations(drag_label, XtParseTranslationTable(
								  "<Btn1Down>:                   handle_drag(start)\n\
		<Btn1Motion>:                   handle_drag(move)"));
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
