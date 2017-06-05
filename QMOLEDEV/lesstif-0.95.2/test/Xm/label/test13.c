/* $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test13.c,v 1.4 2001/05/15 13:40:54 amai Exp $ */

/*
From:        "Edward A. Falk" <falconer@best.com>
To:          lesstif@lesstif.org
Subject:     Lesstif 0.89-9 bug: string resource converter
Date:        Fri, 3 Mar 2000 15:33:23 -0800 (PST)

Hello; this is a bug report on a minor bug I've encountered
in Motif string handling.  I think it's in the String-to-XmString
resource converter.

The following program creates a label widget.  The resources
file looks like this:

	*multiLbl.labelString: Multi\n\
                      line\n\
                      label

Under Motif, the label looks like this:

	+---------+
	|  Multi  |
	|     line|
	|    label|
	+---------+

Under Lesstif, the label looks like this:

	+---------+
	|  Multi  |
	|NN   line|
	|NN  label|
	+---------+

Where 'N' is actually a funky "NL" graphic from the default font.


Program follows:
*/

/* Show bug in resource converter; the String => XmString converter is
 * not handling newlines or leading spaces correctly.  Compile this same
 * program under Motif to see the difference.
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Label.h>

static	XtAppContext	app_ctx ;
static	Widget	topLevel ;

static char *FallBack[] = {
	"*multiLbl.labelString: Multi\\n\
                      line\\n\
                      label",
	NULL
};

int
main(int argc, char **argv)
{
	XtSetLanguageProc(NULL,NULL,NULL) ;

	topLevel = XtVaAppInitialize(&app_ctx, "Stringbug", NULL,0, &argc,argv,
		FallBack,NULL) ;

	XtCreateManagedWidget("multiLbl", xmLabelWidgetClass,
		topLevel,NULL,0);

	XtRealizeWidget(topLevel) ;

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  166,   43, 0,0,0, /* multiLbl */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(topLevel, Expected);
}
LessTifTestMainLoop(topLevel);
	/*
	XtAppMainLoop(app_ctx) ;
	*/

	exit(0) ;
}
