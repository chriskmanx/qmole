/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/rendition/test3.c,v 1.3 2002/05/01 15:23:26 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmAll.h>

#include "../../common/Test.h"


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

/* ConvertStringToPixel()
** A utility function to convert a color name to a Pixel 
*/
Pixel ConvertStringToPixel (Widget widget, char *name)
{
        XrmValue from_value, to_value; /* For resource conversion */

        from_value.addr = name;
        from_value.size = strlen( name ) + 1;
        to_value.addr   = NULL;
        XtConvertAndStore (widget, XmRString, &from_value, XmRPixel, &to_value);

        if (to_value.addr) {
		return (*((Pixel*) to_value.addr)) ;
        }

	return XmUNSPECIFIED_PIXEL ;
}

/*
** A convenient structure to hold the data
** for creating various renditions
*/
typedef struct RenditionData_s
{
	char *tag;
	char *color;
	char *font;
} RenditionData_t;

#define MAX_COLUMNS    4

RenditionData_t rendition_data[MAX_COLUMNS] =
{
	{ "one",   "red",    "fixed"                                                      },
	{ "two",   "green",  "-adobe-helvetica-bold-r-normal--10-100-75-75-*-*-iso8859-1" },
	{ "three", "blue",   "bembo-bold"                                                 },
	{ "four",  "orange", "-adobe-*-medium-i-normal--24-240-75-75-*-*-iso8859-1"       }
};

/*
** Arbitrary data to display in the List
*/
static char *poem[] =
{
	"Mary", "had a",           "little",    "lamb",
	"Its",  "fleece",          "was white", "as snow",
	"And",  "everywhere that", "Mary",      "went",
	"The",  "lamb was",        "sure",      "to follow",
	(char *) 0
};

/*
** CreateListData(): routine to convert the
** poem into an array of compound strings
*/
XmStringTable CreateListData (int *count)
{
	XmStringTable table  = (XmStringTable) 0 ;
	int           line   = 0 ;
	int           column = 0 ;
	int           index  = 0 ;
	XmString      entry ;
	XmString      row =NULL;
	XmString      tmp =NULL;
	XmString      tab;

	tab = XmStringComponentCreate (XmSTRING_COMPONENT_TAB, 0, NULL);

	while (poem[index] != (char *) 0) {
		/* create a compound string, using the rendition tag */
		entry = XmStringGenerate ((XtPointer) poem[index], 
					  NULL, 
					  XmCHARSET_TEXT, 
					  rendition_data[column].tag) ;

		if (row != (XmString)NULL) {
			tmp = XmStringConcat (row, tab) ;
			XmStringFree (row) ;
			row = XmStringConcatAndFree (tmp, entry) ;
		}
		else {
			row = entry ;
		}

		++column ;
		
		if (column == MAX_COLUMNS) {
			if (table == (XmStringTable) 0) {
				table = (XmStringTable) XtMalloc((unsigned) 1 * sizeof (XmString)) ;
			}
			else {
				table = (XmStringTable) XtRealloc((char *) table, (unsigned) (line + 1) * sizeof (XmString)) ;
			}

			table[line++] = row ;
			row           = (XmString) 0 ;
			column        = 0 ;
		}

		index++ ;
	}

	XmStringFree (tab) ;

	table[line] = (XmString) 0 ;

	*count = line ;

	return table ;
}


int
main(int argc, char **argv)
{
	Widget		toplevel, one, d;
	XtAppContext	app;
	XmFontList	fontlist;
	XmString	xms;
	Arg           args[10];
	XmTab         tabs[MAX_COLUMNS];
	XmTabList     tablist;
	XmRendition   renditions[MAX_COLUMNS];
	XmRenderTable rendertable;
	XmStringTable xmstring_table;
	int           xmstring_count;
	Pixel         pixels[MAX_COLUMNS];
	int           n, i;

	XtSetLanguageProc(NULL, NULL, NULL);
	toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

	d = XmGetXmDisplay(XtDisplay(toplevel));
	XtAddCallback(d, XmNnoFontCallback, NoFontCB, NULL);
	XtAddCallback(d, XmNnoRenditionCallback, NoRenditionCB, NULL);

	/* Create some colors */
	for (i = 0 ; i < MAX_COLUMNS ; i++) {
		pixels[i] = ConvertStringToPixel (toplevel, rendition_data[i].color) ;
	}

	/* Create tab stops for columnar output */
	for (i = 0 ; i < MAX_COLUMNS ; i++) {
		tabs[i] = XmTabCreate ((float) 1.5, 
				       XmINCHES, 
				       ((i == 0) ? XmABSOLUTE : XmRELATIVE),
				       XmALIGNMENT_BEGINNING, 
				       ".") ;
	}

	/* Create a tablist table which contains the tabs */
	tablist = XmTabListInsertTabs (NULL, tabs, XtNumber (tabs), 0) ;

	/* Create some multi-font/color renditions, and use the tablist */
	/* This will be inherited if we use it on the first rendition   */
	for (i = 0 ; i < MAX_COLUMNS ; i++) {
		n = 0 ;

		if (i == 0) {
			XtSetArg (args[n], XmNtabList, tablist); n++;
		}

		XtSetArg (args[n], XmNrenditionForeground, pixels[i]); n++;
		XtSetArg (args[n], XmNfontName, rendition_data[i].font); n++;
		XtSetArg (args[n], XmNfontType, XmFONT_IS_FONT); n++;
		renditions[i] = XmRenditionCreate (toplevel, rendition_data[i].tag, args, n);
	}

	/* Create the Render Table */
	rendertable = XmRenderTableAddRenditions (NULL, renditions, XtNumber (renditions), XmMERGE_NEW) ;

	/* Create the multi-column data for the list */

	xmstring_table = CreateListData(&xmstring_count) ;

	xms = xmstring_table[0];
	for (i=1; i<xmstring_count; i++) {
		XmString	s, x;
		s = XmStringSeparatorCreate();
		x = XmStringConcat(xms, s);
		xms = XmStringConcat(x, xmstring_table[i]);
	}

	one = XtVaCreateManagedWidget("One", xmLabelWidgetClass, toplevel,
		XmNalignment,	XmALIGNMENT_BEGINNING,
		XmNrenderTable,	rendertable,
		XmNlabelString, xms,
		NULL);

	XtRealizeWidget(toplevel);

#if 0
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
#endif

	LessTifTestMainLoop(toplevel);
	exit(0);
}
