/*
 * Print simplistic text oriented stuff through XmPrint and Xp.
 * This test should work both with Motif and LessTif.
 */
static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/print/test6.c,v 1.5 2001/01/19 19:17:45 dannybackx Exp $";

#include <stdlib.h>
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/extensions/Print.h>
#include <Xm/XmAll.h>

int		event_base, error_base;
XPContext	pctxt;
Display		*pdpy;
Widget		shell;

/*
 * Use fallback resources - for now - to try to get a reasonable size text font.
 * This means use a BIG font for printing.
 */
static char *fallback[] = {
  "*XmPrintShell*fontList: -*-courier-medium-r-normal--*-120-300-300-*-*-iso8859-1",
  "*XmPrintShell*XmDrawnButton.foreground: black",
  "*XmPrintShell*XmDrawnButton.background: white",
  "*XmPrintShell*XmForm.background: green",
  NULL
};


Widget CreateWindow(Widget top, Display *dpy, Screen *scr);

void EndJobCB(Widget w, XtPointer client, XtPointer call)
{
	XtDestroyWidget(shell);
	fprintf(stderr, "XpDestroyContext\n");
	XpDestroyContext(pdpy, pctxt);
	XtCloseDisplay(pdpy);
}

void PrintPageCB(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "PrintPageCB()\n");
}

void DrawCB(Widget w, XtPointer client, XtPointer call)
{
	XmFontList	fl = 0;
	XmString	xms;
	Dimension	wid;
	GC		gc;
	XGCValues	gcv;
	Pixel		fg, bg;

	fprintf(stderr, "Redraw button\n");
	XtVaGetValues(w,
			XmNfontList,	&fl,
			XmNwidth,	&wid,
			XmNforeground,	&fg,
			XmNbackground,	&bg,
		NULL);
	if (fl == 0) {
		fprintf(stderr, "No fontlist\n");
		return;
	}

	xms = XmStringCreateSimple("This is the text");

	/* Use xlib to get a font */
	gcv.font = XLoadFont(XtDisplay(w),
		"-*-courier-medium-r-normal--*-120-300-300-m-*-iso8859-1");
	if (gcv.font == 0) {
		fprintf(stderr, "Bad font !!\n");
	}

	fprintf(stderr, "test6: XCreateGC()\n");
	gcv.foreground = fg;
	gcv.background = bg;
	gc = XCreateGC(XtDisplay(w), XtWindow(w), GCForeground|GCBackground|GCFont, &gcv);

	XmStringDraw(XtDisplay(w), XtWindow(w), fl,
		xms, gc,
		600, 200, wid,					/* FIX ME */
		XmALIGNMENT_CENTER, XmHORIZONTAL, NULL);

	XmStringFree(xms);

	fprintf(stderr, "test6: XpEndPage\n");
	XpEndPage(XtDisplay(shell));

	fprintf(stderr, "test6: XpEndJob\n");
	XpEndJob(XtDisplay(shell));
}

Widget CreateWindow(Widget top, Display *dpy, Screen *scr)
{
	Widget				db, form;
	Arg				al[20];
	int				ac;
	Dimension			width, height;

	/*
	 * Here we are sure that printer name, print display, screen are
	 * initialized, and a print context has been created.
	 */
	width = WidthOfScreen(scr);
	height = HeightOfScreen(scr);

	fprintf(stderr, "OKCallback()\nXmPrintSetup(wid %d ht %d)\n",
		width, height);

	ac = 0;
	XtSetArg(al[ac], XmNheight, height); ac++;
	XtSetArg(al[ac], XmNwidth, width); ac++;
	shell = XmPrintSetup(top, scr, "Print", al, ac);

	XtAddCallback(shell, XmNendJobCallback, EndJobCB, NULL);

	/* start job must precede XpGetDocumentData in XmPrintToFile */
	fprintf(stderr, "test6: XpStartJob\n");
	XpStartJob(XtDisplay(shell), XPSpool);

	/*
	 * Now we can get the size of the shell, create widgets, etc.
	 */
	XtVaGetValues(shell,
			XmNheight,	&height,
			XmNwidth,	&width,
		NULL);
	/*
	 * Create a form widget as big as the page
	 */
	ac=0;
	XtSetArg(al[ac], XmNheight, height); ac++;
	XtSetArg(al[ac], XmNwidth, width); ac++;
	form = XmCreateForm(shell, "form", al, ac);
	XtManageChild(form);

	fprintf(stderr, "test6: XmCreateDrawnButton\n");
	ac=0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_POSITION); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_POSITION); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_POSITION); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_POSITION); ac++;
	XtSetArg(al[ac], XmNtopPosition, 30); ac++;
	XtSetArg(al[ac], XmNbottomPosition, 60); ac++;
	XtSetArg(al[ac], XmNleftPosition, 30); ac++;
	XtSetArg(al[ac], XmNrightPosition, 60); ac++;
	db = XmCreateDrawnButton(form, "db", al, ac);
	XtManageChild(db);
	XtAddCallback(db, XmNexposeCallback, DrawCB, NULL);

	fprintf(stderr, "test6: XtRealizeWidget()\n");
	XtRealizeWidget(shell);

	fprintf(stderr, "test6: XpStartPage\n");
	XpStartPage(XtDisplay(shell), XtWindow(shell));

	fprintf(stderr, "test6: XmRedisplayWidget\n");
	XmRedisplayWidget(shell);

	/* EndPage and EndJob should *not* happen here, this causes
	 *	the Xprt not to output text at all !!
	 * Xprt silently ignores any drawings and text sent to it
	 *	except when between StartPage/EndPage requests.
	 * So the EndPage must occur in the callback function that
	 *	draws stuff.
	 */
	return shell;
}

int
main(int argc, char *argv[])
{
	XtAppContext	appc;
	Widget		top, arrow, w;
	XPPrinterList	plist;
	int		nlist, i;
	Screen		*pscreen;
	char		*s;

	top = XtVaAppInitialize(&appc, "test6", NULL, 0,
                               &argc, argv, fallback, NULL);

	arrow = XtVaCreateManagedWidget("Button1", xmArrowButtonWidgetClass, top, 
			XmNarrowDirection,	XmARROW_UP,
		NULL);

	pdpy = XtOpenDisplay(appc,
		/* Display String */	":1",
		"print",
		"Print",
		NULL,
		0,
		&argc, argv);
	if (!pdpy) {
		fprintf(stderr, "Cannot connect to :1\n");
		exit(1);
	}
	fprintf(stderr, "Video dpy %p Print dpy %p\n",
		XtDisplay(top), pdpy);

/* Do we have an Xprt ? */
	if (!XpQueryExtension(pdpy, &event_base, &error_base)) {
		fprintf(stderr, "No XpExtension\n");
		exit(1);
	}

/* Obtain list of printers */
	plist = XpGetPrinterList(pdpy, "", &nlist);

	if (nlist)
		for (i=0; i<nlist; i++)
			fprintf(stderr, "Printer[%d] is '%s' (%s)\n",
				i, plist[i].name,
				plist[i].desc);
	else
		fprintf(stderr, "XpGetPrinterList() : No printers\n");

	pctxt = XpCreateContext(pdpy, plist[0].name);
	XpFreePrinterList(plist);
	fprintf(stderr, "test6: XpCreateContext -> %p\n", (XtPointer)pctxt);

	XpSetContext(pdpy, pctxt);

	pscreen = XpGetScreenOfContext(pdpy, pctxt);

/* Get supported resolutions */
	s = XpGetOneAttribute(pdpy, pctxt, XPDocAttr, "printer-resolutions-supported");
	if (s)
		fprintf(stderr, "Resolutions supported [%s]\n", s);

/* Get printer resolution */
	s = XpGetOneAttribute(pdpy, pctxt, XPDocAttr, "default-printer-resolution");
	if (s && strlen(s)) {
		fprintf(stderr, "Default Printer Resolution is [%s]\n", s);
	} else {
		fprintf(stderr, "Couldn't get default printer resolution\n");
		exit(0);
	}

/* Go on and create windows/widgets */

	XtRealizeWidget(top);

	w = CreateWindow(top, pdpy, pscreen);

	LessTifTestMainLoop(top);
	exit(0);
}
