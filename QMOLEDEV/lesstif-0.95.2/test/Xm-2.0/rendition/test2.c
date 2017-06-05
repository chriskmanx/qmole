/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/rendition/test2.c,v 1.4 2002/05/01 15:23:26 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Label.h>
#include <Xm/LabelP.h>


#include "../../common/Test.h"

/*
typedef struct {
    int reason;
    XEvent *event;
    XmRendition rendition;
    char *font_name;
    XmRenderTable render_table;
    XmStringTag tag;
} XmDisplayCallbackStruct;
*/

static void NoFontCB(Widget w, XtPointer client, XtPointer call)
{
	XmDisplayCallbackStruct	*cbp = (XmDisplayCallbackStruct *)call;

	fprintf(stderr, "No-font-callback\n");
}

static void NoRenditionCB(Widget w, XtPointer client, XtPointer call)
{
	XmDisplayCallbackStruct	*cbp = (XmDisplayCallbackStruct *)call;


	fprintf(stderr, "No-rendition-callback\n");
}

int
main(int argc, char **argv)
{
	Widget toplevel, one, d;
	XtAppContext app;
	XmFontList fontlist;

	XmString xmstr1 = XmStringCreateLtoR("\n\nHere is a\n\n", "MY_FONT1");
	XmString xmstr2 = XmStringCreate("different font", "MY_FONT");
	XmString xmstr3 = XmStringCreate("accelerator", "MY_FONT");
	XmStringContext context;
	char *text;
	XmStringCharSet tag;
	XmStringDirection dir;
	Boolean sep;

	XmString xmstr = XmStringConcat(xmstr1, xmstr2);

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

	d = XmGetXmDisplay(XtDisplay(toplevel));
	XtAddCallback(d, XmNnoFontCallback, NoFontCB, NULL);
	XtAddCallback(d, XmNnoRenditionCallback, NoRenditionCB, NULL);

	fontlist = XmFontListAppendEntry(NULL,
		XmFontListEntryCreate("MY_FONT",
			XmFONT_IS_FONT,
			XLoadQueryFont(XtDisplay(toplevel),
			"-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));
	fontlist = XmFontListAppendEntry(fontlist,
		XmFontListEntryCreate("MY_FONT1",
			XmFONT_IS_FONT,
			XLoadQueryFont(XtDisplay(toplevel),
			"-adobe-helvetica-bold-r-normal--17-0-75-75-p-*-iso8859-1")));

	one = XtVaCreateManagedWidget("One", xmLabelWidgetClass, toplevel,
		XmNfontList, fontlist,
		XmNlabelString, xmstr,
		XmNacceleratorText, xmstr3,
		NULL);

	XtRealizeWidget(toplevel);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  112,   58, 0,0,0}, /* One */
};
/* toplevel should be replaced with to correct applicationShell */
	PrintDetails(toplevel, Expected);
}

	LessTifTestMainLoop(toplevel);
	exit(0);
}
