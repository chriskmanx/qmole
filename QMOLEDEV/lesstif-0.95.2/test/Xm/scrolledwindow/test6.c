/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test6.c,v 1.8 2001/05/18 12:41:55 amai Exp $
** autoscroll
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


Widget          scrollw = (Widget) NULL;

typedef struct canvas_s {
	Widget          da;
	Widget          hsb;
	Widget          vsb;
	int             width, height, x_offset, y_offset;
	GC              gc;
} canvas_t, *canvas_p;


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
create_gc(canvas)
	canvas_p        canvas;
{
	if (canvas->gc == 0) {
		XGCValues       values;
		/* Create GC with foreground taken from the XmDrawingArea */
		XtVaGetValues(canvas->da,
			      XmNforeground, &values.foreground, 0);
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
	XExposeEvent   *e = &((XmDrawingAreaCallbackStruct *) call_data)
	->event->xexpose;
	canvas_p        canvas = (canvas_p) client_data;
	Display        *display = XtDisplay(canvas->da);

	create_gc(canvas);
	/* Set clipping region to be the part of the window that was exposed */
	set_clip(canvas, e->x, e->y, e->width, e->height);
	/*
	 * Draw all the coordinate pairs that are (wholly or partly) within
	 * the exposed area
	 */
	x = (e->x / 100) * 100;
	while (x < e->x + e->width) {
		y = (e->y / 20) * 20;
		while (y < e->y + e->height + 10) {
			char            buf[24];
			sprintf(buf, "%d,%d", x, y);
			XDrawString(display, XtWindow(canvas->da), canvas->gc,
				    x, y + 10, buf, strlen(buf));
			y = y + 20;
		}
		x = x + 100;
	}
}


void
dump_sw(char *where, Widget w)
{
#ifdef LESSTIF_VERSION
#if 0
/* what the h... is "4"? I would say "42" is probably not worse ... */
/* Toggle this for 2.0.  Manager has popup handler added in 2.0 */
    w = (Widget)((char *)w + 4);
#endif
    printf(where);
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
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */
	XmString        xmstring;
	Widget          children[2];

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Automatic Scrolling"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	form = XmCreateForm(appshell, "form", al, ac);
	ac = 0;
	xmstring = XmStringCreateLtoR("Expand drawing", (XmStringCharSet) XmFONTLIST_DEFAULT_TAG);
	XtSetArg(al[ac], XmNlabelString, xmstring); ac++;
	bigger = XmCreatePushButton(form, "bigger", al, ac);
	XmStringFree(xmstring);
	ac = 0;
	XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
	scrollw = XmCreateScrolledWindow(form, "scrollw", al, ac);
        dump_sw("after create\n", scrollw);
	ac = 0;
	XtSetArg(al[ac], XmNwidth, 1000); ac++;
	XtSetArg(al[ac], XmNheight, 1000); ac++;
	canvas->da = XmCreateDrawingArea(scrollw, "da", al, ac);
        dump_sw("after create child\n", scrollw);
	XtAddCallback(canvas->da, XmNexposeCallback, expose, canvas);

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
        dump_sw("after set attachments\n", scrollw);
	ac = 0;
	children[ac++] = canvas->da;
	XtManageChildren(children, ac);
        dump_sw("after manage children\n", scrollw);
	ac = 0;
	children[ac++] = bigger;
	children[ac++] = scrollw;
	XtManageChildren(children, ac);
        dump_sw("after manage\n", scrollw);
	ac = 0;
	XtManageChild(form);
        dump_sw("after manage parent\n", scrollw);
	return appshell;
}

XtAppContext app_context;
Display *display;       /*  Display             */

String fallback[] = {
	"*XmScrolledWindow*background:	yellow",
	"*XmDrawingArea*background:	red",
	NULL
};

int main (int    argc, char **argv)
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

	XtAppSetFallbackResources(app_context, fallback);

	canvas.gc = 0;
	appshell = create_appshell(display, "XApplication", argc, &argv, &canvas);
	XtRealizeWidget(appshell);
        dump_sw("after realize\n", scrollw);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,  100,  125, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   96,   25, 0,0,0, /* bigger */
   CWWidth | CWHeight | CWX | CWY,    0,   25,  100,  100, 0,0,0, /* scrollw */
   CWWidth | CWHeight | CWX | CWY,    4,    4,   69,   69, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0, 1000, 1000, 0,0,0, /* da */
   CWWidth | CWHeight | CWX | CWY,   81,    0,   19,   77, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,   81,   77,   19, 0,0,0, /* HorScrollBar */ 
    };
    PrintDetails(appshell,Expected);
};
  LessTifTestMainLoop(appshell);

	exit (0);
}

