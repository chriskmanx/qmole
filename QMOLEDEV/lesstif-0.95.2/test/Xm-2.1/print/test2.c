/*
 * Simple application that uses the XmPrint API.
 * This application uses the DtPrint and the XmPrint functions,
 * but without a print dialog manager.
 *
 * Print a large text widget by use of the XmNpageSetupCallback to
 * not only print a page but also trigger printing of the next page.
 *
 * test/Xm-2.1/print/test4.c uses another approach.
 */
static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/print/test2.c,v 1.18 2001/03/09 16:20:34 dannybackx Exp $";

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <Xm/XmAll.h>
#include <Dt/Print.h>
void OKCallback(Widget w, XtPointer client, XtPointer call);

static void popup(Widget w, XtPointer client, XtPointer call)
{
	Widget	pd = (Widget)client;

	if (pd)
		XtManageChild(pd);
}

/* test2 from here */
int		event_base, error_base;
XPContext	pctxt;
Display		*pdpy;
Widget		shell, tw;
short		nrows = 80;
int		nlines = 481, npages, page, i;

int		SaveArgc;
char		**SaveArgv;

/*
 * Use fallback resources - for now - to tro to get a reasonable size text font.
 * This means use a BIG font for printing.
 */
static char *fallback[] = {
  "*XmPrintShell*fontList: -*-courier-medium-r-normal--*-120-300-300-*-*-iso8859-1",
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
	fprintf(stderr, "EndJobCB:XpDestroyContext\n");
	XpDestroyContext(pdpy, pctxt);
	XtCloseDisplay(pdpy);
}

void PrintPageCB(Widget w, XtPointer client, XtPointer call)
{
	XmPrintShellCallbackStruct *cbp = (XmPrintShellCallbackStruct *)call;

	/* Next page */
	page ++;

	if (page < npages) {
		fprintf(stderr, "PrintPageCB page %d/%d (top is %d)\n",
			page, npages, (int)XmTextGetTopCharacter(tw));

		XmTextScroll(tw, nrows);
	} else {
		fprintf(stderr, "PrintPageCB last pg (top is %d)\n",
			(int)XmTextGetTopCharacter(tw));
		cbp->last_page = True;
	}
}

void OKCallback(Widget w, XtPointer client, XtPointer call)
{
	DtPrintSetupCallbackStruct	*pbs = call;
	Widget				form;
	FILE				*f;
	static char			buffer[64000];
	Arg				al[20];
	int				ac;
	unsigned short			wid, ht;
	XRectangle			rect;
	Dimension			width, height;
	Screen				*scr;

	/* Get the info from the widget */
	pdpy = pbs->print_data->print_display;
	pctxt = pbs->print_data->print_context;
	scr = DefaultScreenOfDisplay(pdpy);

	fprintf(stderr, "OKCallback(pctxt %p)\n", (XtPointer)pctxt);

	/*
	 * Here we are sure that printer name, print display, screen are
	 * initialized, and a print context has been created.
	 */
	if (XpGetPageDimensions(pdpy, pctxt, &wid, &ht, &rect)) {
		fprintf(stderr, "Paper size is %d %d\n", wid, ht);
	} else {
		fprintf(stderr, "test2: failed to get Page Dimensions\n");
		exit(1);
	}

	ac = 0;
	/* Size from paper size */
	XtSetArg(al[ac], XmNheight, ht); ac++;
	XtSetArg(al[ac], XmNwidth, wid); ac++;
	shell = XmPrintSetup(w, scr, "Print", al, ac);

	XtAddCallback(shell, XmNendJobCallback, EndJobCB, NULL);
	XtAddCallback(shell, XmNpageSetupCallback, PrintPageCB, NULL);

	/* start job must precede XpGetDocumentData in XmPrintToFile */
	fprintf(stderr, "test2: XpStartJob\n");
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
	fprintf(stderr, "test2: XmCreateText(wid %d ht %d)\n",
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

	fprintf(stderr, "test2: XtRealizeWidget()\n");
	XtRealizeWidget(shell);

	/* Get data to calculate number of pages to print */
	XtVaGetValues(tw,
			XmNrows,		&nrows,
			XmNtotalLines,		&nlines,
		NULL);

	/* Calculate number of pages to print */
	npages = nlines / nrows + 1;
	page = 0;

	fprintf(stderr, "Text lines %d rows %d pages %d\n",
		nlines, nrows, npages);
}

int
main(int argc, char *argv[])
{
	XtAppContext	appc;
	Widget		top, arrow, pd;

	top = XtVaAppInitialize(&appc, "drawingArea", NULL, 0,
                               &argc, argv, fallback, NULL);
	SaveArgv = argv;
	SaveArgc = argc;

	arrow = XtVaCreateManagedWidget("Button1", xmArrowButtonWidgetClass, top, 
			XmNarrowDirection,	XmARROW_UP,
		NULL);
	XtRealizeWidget(top);

	pd = DtCreatePrintSetupDialog(top, "dialog", NULL, 0);
	XtManageChild(pd);
	XtAddCallback(pd, DtNprintCallback, OKCallback, NULL);

	XtAddCallback(arrow, XmNactivateCallback, popup, pd);

	LessTifTestMainLoop(top);
	exit(0);
}
