/*
 * Simple application that uses the XmPrint API.
 * This application uses the DtPrint and the XmPrint functions,
 * but without a print dialog manager.
 *
 * Print a large text widget from one PrintOKCallback function,
 * as described in the documentation on XmRedisplayWidget().
 * This means that the complete code for printing the text is
 * structured as a single loop in that PrintOKCallback function.
 *
 * test/Xm-2.1/print/test2.c is similar to this but uses another approach.
 * With that other approach, the code to print all of the text is
 * an interaction between a callback function and the Xprt, so a
 * loop is not immediately visible when reading the code.
 */
static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/print/test4.c,v 1.7 2001/03/09 16:20:34 dannybackx Exp $";

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <Xm/XmAll.h>
#include <Dt/Print.h>

static void popup(Widget w, XtPointer client, XtPointer call)
{
	Widget	pd = (Widget)client;

	if (pd)
		XtManageChild(pd);
}

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

void Redraw(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Redraw()\n");
}

void FinishPrintToFile(Display		*display,
			XPContext	context,
			XPGetDocStatus	status,
			XPointer	client)

{
	fprintf(stderr, "FinishPrintToFile()\n");
}

void OKCallback(Widget w, XtPointer client, XtPointer call)
{
	DtPrintSetupCallbackStruct	*pbs = call;
	Widget				form, shell, tw;
	FILE				*f;
	static char			buffer[64000];
	Arg				al[20];
	int				ac;
	unsigned short			wid, ht;
	XRectangle			rect;
	Dimension			width, height;
	Screen				*scr;
	short				nrows = 80;
	int				nlines = 481, npages, page, i;
	XPContext			pctxt;
	Display				*pdpy;
	int				save_data = XPSpool;


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
		fprintf(stderr, "test4: failed to get Page Dimensions\n");
		exit(1);
	}

	ac = 0;
	/* Size from paper size */
	XtSetArg(al[ac], XmNheight, ht); ac++;
	XtSetArg(al[ac], XmNwidth, wid); ac++;
	shell = XmPrintSetup(w, scr, "Print", al, ac);

	if (pbs->print_data->destination == DtPRINT_TO_FILE)
		save_data = XPGetData;

	/* start job must precede XpGetDocumentData in XmPrintToFile */
	fprintf(stderr, "test4: XpStartJob\n");
	XpStartJob(XtDisplay(shell), save_data);

	/* Maintain sequence StartJob - GetDocument */
	XFlush(XtDisplay(shell));

	if (pbs->print_data->destination == DtPRINT_TO_FILE)
		XmPrintToFile(XtDisplay(shell), pbs->print_data->dest_info,
			FinishPrintToFile, NULL);
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
	fprintf(stderr, "test4: XmCreateText(wid %d ht %d)\n",
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

	fprintf(stderr, "test4: XtRealizeWidget()\n");
	XtRealizeWidget(shell);

	/* Get data to calculate number of pages to print */
	XtVaGetValues(tw,
			XmNrows,		&nrows,
			XmNtotalLines,		&nlines,
		NULL);

	/* Calculate number of pages to print */
	npages = nlines / nrows + 1;

	fprintf(stderr, "Text lines %d rows %d pages %d\n",
		nlines, nrows, npages);

	for (page = 0; page < npages; page++) {
		XpStartPage(XtDisplay(shell), XtWindow(shell));

		fprintf(stderr, "test4: XmRedisplayWidget\n");
		XmRedisplayWidget(tw);	/* works !! */

		/* Calling XmRedisplayWidget() only works for
		 * widgets that have an expose() method. This simple
		 * point is documented in the XmRedisplayWidget docs.
		 * Not so obvious consequence is what to do with other
		 * widgets. It seems to be up to the application
		 * programmer to figure out which widget to use.
		 *
		 * Calling it on a shell or on a form won't work.
		XmRedisplayWidget(shell);
		 */

		XpEndPage(XtDisplay(shell));

		if (XmIsText(tw))
			XmTextScroll(tw, nrows);
	}
	XpEndJob(XtDisplay(shell));
}

int
main(int argc, char *argv[])
{
	XtAppContext	appc;
	Widget		top, arrow, pd;

	top = XtVaAppInitialize(&appc, "drawingArea", NULL, 0,
                               &argc, argv, fallback, NULL);

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
