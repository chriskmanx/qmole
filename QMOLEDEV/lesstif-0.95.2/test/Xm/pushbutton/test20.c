/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/pushbutton/test20.c,v 1.1 2005/06/25 06:08:49 dannybackx Exp $
 */
#include <stdio.h>
#include <Xm/XmAll.h>

static char *FallBack[] = {
		"*.geometrySlop: 1",
		"*red: red",
		"*blue: blue",
		NULL
};

typedef struct appres {
	Pixel	red;
	Pixel	blue;
} *appresp, appres;
appres Appres;

XtResource res[] = {
	{ "red", "Red", XmRPixel, sizeof(Pixel), XtOffset(appresp, red),
		XmRImmediate, (XtPointer)1},
	{ "blue", "Blue", XmRPixel, sizeof(Pixel), XtOffset(appresp, blue),
		XmRImmediate, (XtPointer)2}
};

int
main(int argc, char **argv)
{
	Widget toplevel, one, rc;
	XtAppContext app;
	XmString xmstr1 = XmStringCreateLtoR("Here Is A\nLabel", "MY_FONT");
	XmFontList	fontlist;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, FallBack, NULL);
	XtGetApplicationResources(toplevel, &Appres, res, XtNumber(res), NULL, 0);

	rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel, NULL);

	fontlist = XmFontListAppendEntry(NULL,
			XmFontListEntryCreate("MY_FONT", XmFONT_IS_FONT,
				XLoadQueryFont(XtDisplay(toplevel),
				"-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

	fprintf(stderr, "Red is %08p, blue is %08p\n", Appres.red, Appres.blue);

	one = XtVaCreateManagedWidget("One", xmPushButtonWidgetClass, rc,
			XmNlabelString, xmstr1,
			XmNfontList,	fontlist,
			XtNborderWidth, 20,
			XmNforeground, Appres.red,
			XmNbackground, Appres.blue,
			XtVaTypedArg, XmNborderColor, XmRString, "black", strlen("black"),
			NULL);

	XtRealizeWidget(toplevel);
	XtAppMainLoop(app);
	exit(0);
}
