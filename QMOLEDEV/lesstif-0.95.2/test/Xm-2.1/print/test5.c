/*
 * Print multiple pages from XmPrint, no DtPrint.
 *
 * This test should work both with Motif and LessTif.
 */
static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/print/test5.c,v 1.8 2001/01/19 19:17:45 dannybackx Exp $";

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <X11/Intrinsic.h>
#include <X11/extensions/Print.h>
#include <Xm/XmAll.h>

int		event_base, error_base;
XPContext	pctxt;
Display		*pdpy;
Widget		shell, tw;
short		nrows = 80;
int		nlines = 481, npages, page, i;

/*
 * Use fallback resources - for now - to tro to get a reasonable size text font.
 * This means use a BIG font for printing.
 */
static char *fallback[] = {
#if 0
 -adobe-courier-medium-r-normal--199-120-1200-1200-p-1190-iso8859-1
 -compugraphic-courier-medium-r-normal--8782-2500-2540-2540-m-52910-iso8859-1
  "*fontList: -adobe-courier-medium-r-normal--200-*-100-100-m-*-iso8859-1",
  "*fontList: -adobe-courier-medium-r-normal--*-120-300-300-m-*-iso8859-1",
#endif
  "*fontList: -*-courier-medium-r-normal--*-120-300-300-*-*-iso8859-1",
  "*XmText.foreground: black",
  "*XmText.background: white",
  "*XmText.marginWidth: 100",
  "*XmText.marginHeight: 300",
  "*XmForm.background: green",
  "*XmArrowButton.background: red",
  "*XmArrowButton.foreground: yellow",
  "*XmPushButton.background: black",
  "*XmPushButton.foreground: green",
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
	fprintf(stderr, "PrintPageCB(top is %d)\n",
		XmTextGetTopCharacter(tw));

	XpEndPage(XtDisplay(shell));

	/* Next page */
	page ++;

	if (page < npages) {
		XmTextScroll(tw, nrows);
		XpStartPage(XtDisplay(shell), XtWindow(shell));
		XmRedisplayWidget(shell);
	} else {
		XpEndJob(XtDisplay(shell));
	}
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

	top = XtVaAppInitialize(&appc, "test5", NULL, 0,
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
	fprintf(stderr, "test5: XpCreateContext -> %p\n", (XtPointer)pctxt);

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

/* Try to set paper size */
/* This doesn't seem to have any effect */
	XpSetAttributes(pdpy, pctxt, XPDocAttr,
		"default-medium: iso-a4", XPAttrReplace);
	XpSetAttributes(pdpy, pctxt, XPJobAttr,
		"default-medium: iso-a4", XPAttrReplace);
	XpSetAttributes(pdpy, pctxt, XPPageAttr,
		"default-medium: iso-a4", XPAttrReplace);
#if 0
	XpSetAttributes(pdpy, pctxt, XPPrinterAttr,
		"default-medium: iso-a4", XPAttrReplace);
	XpSetAttributes(pdpy, pctxt, XPServerAttr,
		"default-medium: iso-a4", XPAttrReplace);
#endif

/* Go on and create windows/widgets */

	XtRealizeWidget(top);

	w = CreateWindow(top, pdpy, pscreen);

	LessTifTestMainLoop(top);
	exit(0);
}

Widget CreateWindow(Widget top, Display *dpy, Screen *scr)
{
	Widget				form;
	FILE				*f;
	static char			buffer[64000];
	Arg				al[20];
	int				ac;
	unsigned short			wid, ht;
	XRectangle			rect;
	Dimension			width, height;

	/*
	 * Here we are sure that printer name, print display, screen are
	 * initialized, and a print context has been created.
	 */
	fprintf(stderr, "OKCallback()\n\n");

	if (XpGetPageDimensions(pdpy, pctxt, &wid, &ht, &rect)) {
		fprintf(stderr, "Paper size is %d %d\n", wid, ht);
	} else {
		fprintf(stderr, "test5: failed to get Page Dimensions\n");
	}

	ac = 0;
	/* Size from paper size */
	XtSetArg(al[ac], XmNheight, ht); ac++;
	XtSetArg(al[ac], XmNwidth, wid); ac++;
	shell = XmPrintSetup(top, scr, "Print", al, ac);

	XtAddCallback(shell, XmNendJobCallback, EndJobCB, NULL);
	XtAddCallback(shell, XmNpageSetupCallback, PrintPageCB, NULL);

	/* start job must precede XpGetDocumentData in XmPrintToFile */
	fprintf(stderr, "test5: XpStartJob\n");
	XpStartJob(XtDisplay(shell), XPSpool);

	/*
	 * Now we can get the size of the shell, create widgets, etc.
	 */
	XtVaGetValues(shell,
			XmNheight,	&height,
			XmNwidth,	&width,
		NULL);
	/*
	 * Create a text widget as big as the page
	 */
	fprintf(stderr, "test5: XmCreateText(wid %d ht %d)\n",
		width, height);

	ac=0;
	XtSetArg(al[ac], XmNheight, height); ac++;
	XtSetArg(al[ac], XmNwidth, width); ac++;
	form = XmCreateForm(shell, "form", al, ac);
	XtManageChild(form);

	/* Re-use settings from above */
	XtSetArg(al[ac], XmNeditMode, XmMULTI_LINE_EDIT); ac++;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	tw = XmCreateText(form, "tw", al, ac);
	XtManageChild(tw);

	/*
	 * Get some data in here - the LGPL text (you probably
	 * need to be in the LessTif source tree for this to work).
	 */
	for (i=0; i<10; i++) {
		f = fopen("COPYING.LIB", "r");
		if (f)
			break;
		chdir("..");
	}
	if (f) {
		fread(buffer, 1, sizeof(buffer), f);
		fclose(f);
	}
	XmTextSetString(tw, buffer);
	XmTextShowPosition(tw, 0);

	fprintf(stderr, "test5: XtRealizeWidget()\n");
	XtRealizeWidget(shell);

	/* Get data to calculate number of pages to print */
	XtVaGetValues(tw,
			XmNrows,		&nrows,
			XmNtotalLines,		&nlines,
		NULL);

	/* Calculate number of pages to print */
	npages = nlines / nrows;
	page = 0;

	fprintf(stderr, "Text lines %d rows %d pages %d\n",
		nlines, nrows, npages);

	XpStartPage(XtDisplay(shell), XtWindow(shell));
	XmRedisplayWidget(shell);

	return shell;
}
