/*
 * The most important goal of this test is to figure out how to
 *	get a callback which is triggered by an event from Xprt
 *	after we use XpStartJob().
 * This program now works with Motif, but not yet with LessTif.
 */
static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/print/test1.c,v 1.10 2001/01/19 19:17:45 dannybackx Exp $";

#include <stdlib.h>
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/extensions/Print.h>
#include <Xm/XmAll.h>

static char *fallback[] = {
  "*XmPrintShell*fontList: -*-courier-medium-r-normal--*-120-300-300-*-*-iso8859-1",
  NULL
};


int		event_base, error_base;
XPContext	pctxt;
Display		*pdpy;

void StartJobCB(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "StartJobCB()\n");
}

void EndJobCB(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "EndJobCB()\n");
}

void PdmCB(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "PdmCB()\n");
}

void PrintPageCB(Widget w, XtPointer client, XtPointer call)
{
	XmPrintShellCallbackStruct *p = (XmPrintShellCallbackStruct *)call;

	fprintf(stderr, "PrintPageCB()\n");

	p->last_page = True;
}

Widget CreateWindow(Widget top, Display *dpy, Screen *scr)
{
	Widget	w, fsb;
	Arg	al[10];
	int	ac;

	ac = 0;
	XtSetArg(al[ac], XmNheight, 1600); ac++;
	XtSetArg(al[ac], XmNwidth, 1000); ac++;

	fprintf(stderr, "test1: XmPrintSetup()\n");
	w = XmPrintSetup(top, scr, "print_shell", al, ac);
	XtAddCallback(w, XmNpageSetupCallback, PrintPageCB, 0);
	XtAddCallback(w, XmNstartJobCallback, StartJobCB, 0);
	XtAddCallback(w, XmNendJobCallback, EndJobCB, 0);
	XtAddCallback(w, XmNpdmNotificationCallback, PdmCB, 0);

	fprintf(stderr, "test1: XpStartJob\n");
	XpStartJob(pdpy, XPSpool);

	fprintf(stderr, "XmCreateFileSelectionBox()\n");
	fsb = XmCreateFileSelectionBox(w, "fsd", al, ac);
	XtManageChild(fsb);

	return w;
}

int
main(int argc, char *argv[])
{
	XtAppContext	appc;
	Widget		top, arrow, w;
	XPPrinterList	plist;
	int		nlist, i;
	Screen		*pscreen;
	Window		win;

	top = XtVaAppInitialize(&appc, "drawingArea", NULL, 0,
                               &argc, argv, fallback, NULL);

	arrow = XtVaCreateManagedWidget("Button1", xmArrowButtonWidgetClass, top, 
			XmNarrowDirection,	XmARROW_UP,
		NULL);

	XtRealizeWidget(top);

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

	if (!XpQueryExtension(pdpy, &event_base, &error_base)) {
		fprintf(stderr, "No XpExtension\n");
		exit(1);
	}

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
	fprintf(stderr, "test1: XpCreateContext -> %p\n", (XtPointer)pctxt);

	XpSetContext(pdpy, pctxt);

	pscreen = XpGetScreenOfContext(pdpy, pctxt);
	w = CreateWindow(top, pdpy, pscreen);

	fprintf(stderr, "test1: XtRealizeWidget()\n");
	XtRealizeWidget(w);
	win = XtWindow(w);

	LessTifTestMainLoop(top);
	exit(0);
}
