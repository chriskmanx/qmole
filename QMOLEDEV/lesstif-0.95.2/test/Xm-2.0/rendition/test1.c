/* $Id: test1.c,v 1.3 2002/01/16 00:48:04 dannybackx Exp $ */

#if 0
Article: 68453 of comp.windows.x.motif
Message-ID: <3A15422D.986F04B@ist.co.uk>
Date: Fri, 17 Nov 2000 14:35:25 +0000
From: Antony Fountain <af@ist.co.uk>
Reply-To: af@ist.co.uk
Organization: Imperial Software Technology
X-Mailer: Mozilla 4.75 [en] (X11; U; SunOS 5.6 sun4u)
X-Accept-Language: en
MIME-Version: 1.0
Newsgroups: comp.windows.x.motif
To: kgandhi@my-deja.com
Subject: Re: Colored Items in ScrolledList Widget
References: <8updrj$a95$1@nnrp1.deja.com>
Content-Type: multipart/mixed;
 boundary="------------AABEF9C00BE38D33C59E0864"
NNTP-Posting-Host: isbalham.ist.co.uk
X-Trace: 17 Nov 2000 14:34:40 GMT, isbalham.ist.co.uk
Lines: 333
Path: news.tu-darmstadt.de!news.hrz.uni-kassel.de!news-fra1.dfn.de!news.tele.dk!193.190.198.17!newsfeeds.belnet.be!news.belnet.be!psinet-eu-nl!psiuk-p4!uknet!psiuk-n!nnrp1.news.uk.psi.net!isbalham.ist.co.uk
Xref: news.tu-darmstadt.de comp.windows.x.motif:68453

This is a multi-part message in MIME format.
--------------AABEF9C00BE38D33C59E0864
Content-Type: text/plain; charset=us-ascii
Content-Transfer-Encoding: 7bit

kgandhi@my-deja.com wrote:

> I have been informed that Motif 2.0 and higher allows for different
> colored items in a ScrolledList widget due to the fact that XmStrings
> (of which items are created) allow colors. I have checked a Reference
> Manual for Motif 2.1 and do not find any function to create XmStrings
> with color arguments. How exactly does one code in a specific colored
> item which is different than the XmNbackground and XmNforeground color
> resource for a ScrolledList?
>
> Sent via Deja.com http://www.deja.com/
> Before you buy.

The way this works is as follows:

You need to create a render table which has some renditions in it

that have the XmNrenditionForeground/XmNrenditionBackground resources set.

You then need to ensure that you wrap whatever segments you want colored

in your compound string with XmSTRING_COMPONENT_RENDITION_BEGIN/

XmSTRING_COMPONENT_RENDITION_END segments such that the XmSTRING_COMPONENT_RENDITION_BEGIN

tag matches the relevent rendition. XmStringGenerate will do this for you.

Then apply the render table to the widget where you are drawing the compound strings

(the list).

Basic outline:

    Create a render table with a "red" rendition...

    XmRendition red_rendition;

    XmRendition renditions[MAX_RENDITIONS];

    XmRenderTable table;

    Arg args[8];

    Cardinal n, renditions_count = 0;

    Pixel red_pixel = ...; /* Whatever: XtConvertAndStore() ... */

    n = 0;

    XtSetArg (args[n], XmNrenditionForeground, red_pixel); n++;

    ... /* Other rendition arguments */

    red_rendition = XmRenditionCreate (some_widget, "red", args, n);

    renditions[renditions_count++] = red_rendition;

    /* Add any other renditions */

    ...

    /* Create the render table using the renditions */

    render_table = XmRenderTableAddRenditions (NULL, renditions, renditions_count, XmMERGE_NEW);

    /* Now create your compound strings... */

    XmString red_segment = XmStringGenerate ((XtPointer) "This should be red",

                                             NULL, /* Or some segment tag */

                                             XmCHARSET_TEXT,

                                             "red"); /* Must match the rendition tag */

    /* Create rest of compound strings */

    ...

    /* Apply the render table to the list */

    XtVaSetValues (list, XmNrenderTable, render_table, NULL);

    I hope this is of some help. The above is sketchy. Enclosed is an example

    program which gives a multi-column, multi-font, multi-colored list widget

    using the Motif 2.1 render table/tablist routines.

    Yours faithfully,

    --
************************************************************
 Antony Fountain            Principal Software Engineer
                            Imperial Software Technology

 120 Hawthorne Ave., Suite 101        252 Kings Road
 Palo Alto, CA  94301                 Reading RG1 4HP U.K.
 Tel: (650) 688-0200                  Tel: +44 118 958 7055
 Fax: (650) 688-1054                  Fax: +44 118 958 9005

 af@ist.co.uk                         http://www.ist.co.uk/
************************************************************


#endif



/* rendered_list.c: illustrates all the features of
** render tables and renditions by creating a 
** multi-column, multi-font, multi-color List widget.
*/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/List.h>

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


#if 0
	tab = XmStringComponentCreate (XmSTRING_COMPONENT_TAB, NULL, 0);
#endif

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
main (int argc, char *argv[])
{
	Widget        toplevel, rowcol, list;
	XtAppContext  app;
	Arg           args[10];
	XmTab         tabs[MAX_COLUMNS];
	XmTabList     tablist;
	XmRendition   renditions[MAX_COLUMNS];
	XmRenderTable rendertable;
	XmStringTable xmstring_table;
	int           xmstring_count;
	Pixel         pixels[MAX_COLUMNS];
	int           n, i;

	XtSetLanguageProc (NULL, NULL, NULL);
	toplevel = XtVaOpenApplication (&app, "Demos", NULL, 0, &argc, argv, NULL, 
					sessionShellWidgetClass, NULL);

	rowcol = XmCreateRowColumn (toplevel, "rowcol", NULL, 0) ;

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

	xmstring_table = CreateListData (&xmstring_count) ;

	/* Create the List, using the render table */
	n = 0;
	XtSetArg (args[n], XmNrenderTable, rendertable); n++;
	XtSetArg (args[n], XmNitems, xmstring_table); n++;
	XtSetArg (args[n], XmNitemCount, xmstring_count); n++;
	XtSetArg (args[n], XmNwidth, 400); n++;
	XtSetArg (args[n], XmNvisibleItemCount, xmstring_count + 1); n++;
	list = XmCreateScrolledList (rowcol, "list", args, n);
	XtManageChild (list);

	/* Free the memory now the widget has the data */
	/* First, the compound strings */
	for (i = 0 ; i < xmstring_count ; i++)
		XmStringFree (xmstring_table[i]) ;
	XtFree((char *) xmstring_table) ;

	/* Secondly, the XmTab objects */
	for ( i = 0 ; i < XtNumber (tabs) ; i++)
		XmTabFree (tabs[i]);
	
	/* Thirdly, the XmTabList object */
	XmTabListFree (tablist) ;

	/* Fourthly, the XmRendition objects */
	for (i = 0 ; i < XtNumber (renditions) ; i++)
		XmRenditionFree (renditions[i]) ;

	/* Lastly, the XmRenderTable object */
	XmRenderTableFree (rendertable) ;

	XtManageChild (rowcol);
	XtRealizeWidget (toplevel);

#if 1
        LessTifTestMainLoop(toplevel);
#else
	XtAppMainLoop (app);
#endif
	
	exit(0); /* avoid compiler warnings */
}
