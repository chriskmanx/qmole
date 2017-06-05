/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/rendition/test4.c,v 1.8 2002/10/09 21:22:33 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmAll.h>

#include "../../common/Test.h"

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

String fallback[] = {
	"*One.renderTable:		one, two, three, four",
/* This currently works THIS IS A HACK, THE STUFF BELOW SHOULD WORK */
	"*one.fontName:			fixed",
	"*One*one.renditionForeground:	red",
	"*one*renditionForeground:	yellow",
	"*two.fontName:			helvetica",
	"*two.fontSize:			10",
	"*two.renditionForeground:	green",
	"*three.fontName:		bembo",
	"*three.fontStyle:		bold",
	"*three.renditionForeground:	blue",
	"*four.foundry:			adobe",
//	"*four.fontName:		bembo",
	"*four.fontSize:		24",
//	"*four.fontName:		helvetica",
//	"*four.fontSize:		10",
	"*four.renditionForeground:	black",
	"*tabList:			1.5in, +1.5in, +1.5in, +1.5in",
#if 0
/* The rest should but doesn't */
	"*One.renderTable.one.fontName:			fixed",
	"*One.renderTable.one.renditionForeground:	red",
	"*One.renderTable.two.fontName:			-adobe-helvetica-bold-r-normal--10-100-75-75-*-*-iso8859-1",
	"*One.renderTable.two.renditionForeground:	green",
	"*One.renderTable.three.fontName:		bembo-bold",
	"*One.renderTable.three.renditionForeground:	blue",
	"*One.renderTable.four.fontName:		-adobe-*-medium-i-normal--24-240-75-75-*-*-iso8859-1",
	"*One.renderTable*tabList:			1.5in, +1.5in, +1.5in, +1.5in",
#endif
	NULL
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
	toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv,
		fallback, NULL);

	d = XmGetXmDisplay(XtDisplay(toplevel));

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
