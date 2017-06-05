/*
 * Simple application that uses the Xp API.
 * No XmPrint, no DtPrint, no Print Dialog Manager are used.
 *
 * Show an XmFileSelectionBox to figure out how to show e.g.
 *	scrollbars in a sensible way.
 *
 * Maybe this should look at $XPSERVERLIST instead of just
 *	using :1 as the printer display.
 */
static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/print/test3.c,v 1.10 2001/01/19 19:17:45 dannybackx Exp $";

#include <stdlib.h>
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/extensions/Print.h>
#include <Xm/XmAll.h>

Display		*pdpy;
XPContext	pctxt;

static char *fallback[] = {
  "*XmPrintShell*fontList: -*-courier-medium-r-normal--*-120-300-300-*-*-iso8859-1",
  NULL
};

int
main(int argc, char *argv[])
{
	XtAppContext	appc;
	Widget		top, arrow, print, fsb;
	XPPrinterList	plist;
	int		nlist, i;
	Screen		*pscreen;
	Window		win;
	Arg	al[10];
	int	ac;

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

	XpSetContext(pdpy, pctxt);
	XpStartJob(pdpy, XPSpool);
	pscreen = XpGetScreenOfContext(pdpy, pctxt);

	ac = 0;
	XtSetArg(al[ac], XmNheight, 1600); ac++;
	XtSetArg(al[ac], XmNwidth, 1000); ac++;

	print = XmPrintSetup(top, pscreen, "print_shell", al, ac);

	fsb = XmCreateFileSelectionBox(print, "fsd", al, ac);
	XtManageChild(fsb);

	XtRealizeWidget(print);
	win = XtWindow(print);

	XpStartPage(pdpy, win);

	XmRedisplayWidget(fsb);

	XpEndPage(pdpy);
	XpEndJob(pdpy);

	LessTifTestMainLoop(top);
	exit(0);
}
