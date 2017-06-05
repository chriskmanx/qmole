/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test7.c,v 1.8 2001/05/23 14:30:30 amai Exp $
** jumpscroll
*/

#include <stdlib.h>
#include <stdio.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/XmP.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledWP.h>
#ifdef LESSTIF_VERSION
#include <XmI/XmI.h>
#endif

typedef struct canvas_s {
	Widget          da;
	Widget          hsb;
	Widget          vsb;
	int             width, height, x_offset, y_offset;
	GC              gc;
}               canvas_t, *canvas_p;


/*
 * XmScrollBar callbacks just cause redrawing of entire area. * They store
 * the XmScrollBar values in the canvas structure * for use in the expose
 * callback.
 */

void
hsb_changed(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	canvas_p        canvas = (canvas_p) client_data;
	XClearArea(XtDisplay(canvas->da), XtWindow(canvas->da),
		   0, 0, 0, 0, True);
	canvas->x_offset = ((XmScrollBarCallbackStruct *) call_data)->value;
}

void
vsb_changed(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	canvas_p        canvas = (canvas_p) client_data;
	XClearArea(XtDisplay(canvas->da), XtWindow(canvas->da),
		   0, 0, 0, 0, True);
	canvas->y_offset = ((XmScrollBarCallbackStruct *) call_data)->value;
}

/*
 * Adjust one XmScrollBar to suit proportions of XmDrawingArea * and the
 * notional canvas.
 */

void
adjust_scrollbar(sbar, size, visible_size, value)
	Widget          sbar;
	int             size, *value;
	Dimension       visible_size;
{
	/* Assume that minimum is 0 */
	int             slider_size = visible_size;
	int             maximum = size;

	XtVaGetValues(sbar, XmNvalue, value, NULL);
	if (slider_size > maximum)
		slider_size = maximum;
	if (*value > maximum - slider_size)
		*value = maximum - slider_size;
	XtVaSetValues(sbar, XmNvalue, *value,
		      XmNsliderSize, slider_size,
		      XmNmaximum, maximum, NULL);
}

/*
 * Set XmScrollBars to suit canvas and XmDrawingArea
 */
void
adjust_size(canvas)
	canvas_p        canvas;
{
	Dimension       width, height;

	XtVaGetValues(canvas->da, XmNwidth, &width,
		      XmNheight, &height, NULL);
	adjust_scrollbar(canvas->vsb, canvas->height, height,
			 &canvas->y_offset);
	adjust_scrollbar(canvas->hsb, canvas->width, width,
			 &canvas->x_offset);
}

/*
 * XmDrawingArea resize callback
 */

void
resize(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	canvas_p        canvas = (canvas_p) client_data;
	adjust_size(canvas);
}

void
expand(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	/* Double the size of the (notional) canvas */
	canvas_p canvas = (canvas_p)client_data;
	canvas->width=2*canvas->width;
	canvas->height=2*canvas->height;
	adjust_size(canvas);
	/*
	* Generate expose event for entire window (strictly
	* only needed if XmDrawingArea is larger than canvas.
	*/
	XClearArea(XtDisplay(canvas->da), XtWindow(canvas->da),
		   0, 0, 0, 0, True);
}

static void
set_clip(canvas, x, y, width, height)
	canvas_p        canvas;
	int             x, y, width, height;
{
	XRectangle      rect;
	rect.x = x;
	rect.y = y;
	rect.width = width;
	rect.height = height;
	XSetClipRectangles(XtDisplay(canvas->da), canvas->gc, 0, 0, &rect, 1,
			   YSorted);
}

void 
create_gc(canvas_p canvas)
{
	if (canvas->gc == 0) {
		XGCValues       values;
		/* Create GC with foreground taken from the XmDrawingArea */
		XtVaGetValues(canvas->da,
			      XmNforeground, &values.foreground, NULL);
		canvas->gc = XCreateGC(XtDisplay(canvas->da),
				       XtWindow(canvas->da),
				       GCForeground, &values);
	}
}

void 
expose(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	int             x, y;
	XExposeEvent   *e =
	    &((XmDrawingAreaCallbackStruct *) call_data)->event->xexpose;
	canvas_p        canvas = (canvas_p) client_data;
	Display        *display = XtDisplay(canvas->da);

	if (!canvas->gc)
		adjust_size(canvas);
	create_gc(canvas);
	set_clip(canvas, e->x, e->y, e->width, e->height);
	x = ((canvas->x_offset + e->x) / 100) * 100;
	while (x < canvas->width
	       && x < canvas->x_offset + e->x + e->width) {
		y = ((canvas->y_offset + e->y) / 20) * 20;
		while (y < canvas->height
		       && y < canvas->y_offset + e->y + e->height + 10) {
			char buf[24];
			sprintf(buf, "%d,%d", x, y);
			XDrawString(display, XtWindow(canvas->da), canvas->gc,
			    x - canvas->x_offset, y - canvas->y_offset + 10,
				    buf, strlen(buf));
			y = y + 20;
		}
		x = x + 100;
	}
}



void
dump_sw(const char *where, Widget w)
{
#ifdef LESSTIF_VERSION
#if 0
/* amai: not sure who messed up things here.
         In any case this is nonsense, it may work by chance
	 however ... */
/* Toggle this for 2.0.  Manager has popup handler added in 2.0 */
    w = (Widget)((char *)w + 4);
#endif
    puts(where);
    printf("Given W: %d H: %d\n", SW_GivenWidth(w), SW_GivenHeight(w));
    printf("Area W: %d H: %d\n", SW_CWWidth(w), SW_CWHeight(w));
    printf("Pad W: %d H: %d\n", SW_MarginWidth(w), SW_MarginHeight(w));
    printf("Offset X: %d Y: %d\n", SW_CWX(w), SW_CWY(w));
    printf("Spacing: %d\n", SW_Spacing(w));
    printf("Has: %d %d\n", SW_HasHSB(w), SW_HasVSB(w));
    printf("HSB: %d %d %d %d\n", SW_HSBX(w), SW_HSBY(w),
	                         SW_HSBWidth(w), SW_HSBHeight(w));
    printf("VSB: %d %d %d %d\n", SW_VSBX(w), SW_VSBY(w),
	                         SW_VSBWidth(w), SW_VSBHeight(w));
    printf("HSB values: %d %d %d %d\n", SW_HSBMinimum(w), SW_HSBMaximum(w),
	                         SW_HSBValue(w), SW_HSBSliderSize(w));
    printf("VSB values: %d %d %d %d\n", SW_VSBMinimum(w), SW_VSBMaximum(w),
	                         SW_VSBValue(w), SW_VSBSliderSize(w));
    printf("Done.\n\n");
#endif
}

Widget
create_appshell(display, app_name, app_argc, app_argv, canvas)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
	canvas_p        canvas;
{
	Widget          appshell = (Widget) NULL;
	Widget          form = (Widget) NULL;
	Widget          bigger = (Widget) NULL;
	Widget          scrollw = (Widget) NULL;
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */
	XmString        xmstring;
	Widget          children[3];

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Application Scrolling"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	XtSetArg(al[ac], XmNwidth, 200); ac++;
	XtSetArg(al[ac], XmNheight, 200); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	form = XmCreateForm(appshell, "form", al, ac);
	ac = 0;
	xmstring = XmStringCreateLtoR("Expand drawing", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	bigger = XmCreatePushButton(form, "bigger", al, ac);
	XmStringFree(xmstring);

	/*
	 * New callback to expand canvas and new code to set initial size.
	 */
	XtAddCallback(bigger, XmNactivateCallback, expand,
		      (XtPointer) canvas);
	canvas->width = 200;
	canvas->height = 200;

	/*
	 * Code modified to create XmScrolledWindow with APPLICATION_DEFINED
	 * scrolling policy and to create XmScrollBars with appropriate
	 * callbacks.
	 */
	ac = 0;
	XtSetArg(al[ac], XmNscrollingPolicy, XmAPPLICATION_DEFINED); ac++;
	scrollw = XmCreateScrolledWindow(form, "scrollw", al, ac);
	dump_sw("After create\n", scrollw);
	/*
	XtRealizeWidget(scrollw);
	*/
	dump_sw("After realize\n", scrollw);
	ac = 0;
	XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
	canvas->hsb = XmCreateScrollBar(scrollw, "horscrollbar", al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNorientation, XmVERTICAL); ac++;
	canvas->vsb = XmCreateScrollBar(scrollw, "vertscrollbar", al, ac);
	ac = 0;
	canvas->da = XmCreateDrawingArea(scrollw, "da", al, ac);
	/* Callbacks on XmScrollBars */
	XtAddCallback(canvas->hsb, XmNvalueChangedCallback, hsb_changed,
		      (XtPointer) canvas);
	XtAddCallback(canvas->vsb, XmNvalueChangedCallback, vsb_changed,
		      (XtPointer) canvas);
	/*
	** Expose callback on XmDrawingArea, as before, and new
	** resize callback to change XmScrollBars when needed.
	*/
	XtAddCallback(canvas->da, XmNexposeCallback, expose,
		      (XtPointer) canvas);
	XtAddCallback(canvas->da, XmNresizeCallback, resize,
		      (XtPointer) canvas);
	/* End of modified code */

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetValues(bigger, al, ac);

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopWidget, bigger); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetValues(scrollw, al, ac);
	ac = 0;
	children[ac++] = canvas->da;
	children[ac++] = canvas->hsb;
	children[ac++] = canvas->vsb;
	XtManageChildren(children, ac);
	ac = 0;
	children[ac++] = bigger;
	children[ac++] = scrollw;
	XtManageChildren(children, ac);
	ac = 0;
	XtManageChild(form);
	return appshell;
}

XtAppContext app_context;
Display *display;       /*  Display             */

int main (argc,argv)
int    argc;
char            **argv;
{
	canvas_t canvas;
	Widget appshell;
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
	canvas.gc = 0;
	appshell = create_appshell(display, "XApplication", argc, &argv, &canvas);
	XtRealizeWidget(appshell);

  
	
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  200,  200, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   96,   25, 0,0,0, /* bigger */
   CWWidth | CWHeight | CWX | CWY,    0,   25,  200,  175, 0,0,0, /* scrollw */
   CWWidth | CWHeight | CWX | CWY,    0,  160,  181,   15, 0,0,0, /* horscrollbar */
   CWWidth | CWHeight | CWX | CWY,  185,    0,   15,  156, 0,0,0, /* vertscrollbar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  181,  156, 0,0,0, /* da */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(appshell, Expected);
}
LessTifTestMainLoop(appshell);

	exit (0);
}

