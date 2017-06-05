/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test21.c,v 1.2 2002/03/30 17:18:02 amai Exp $ */
/*  demonstrates a problem within _XmTextInvalidate()
    --> SF [Bug #124809]
   writtten by "TYLER, Paul" <pct@ansto.gov.au>
     
   amai:
   Strange SF bug number?!
   See  SF Bugs [ 447163 ] test/Xm/text/test20 fails

   Motif 1.2 displays that text line from the beginning
   and sets cursor to second row.
*/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/Text.h>

/* timeout: */
#define seconds 4

void
handle_timer(XtPointer client_data, XtIntervalId *id)
{
	exit(0);
}


int
main(int argc,char *argv[])
{
	Arg al[2];
	int ac;
	XtAppContext context;
	Widget toplevel, w;
	XtIntervalId timer;
	char *lines = "This is a text line\n";


	/* create the toplevel shell */
	toplevel = XtAppInitialize(&context, "", NULL, 0,
	                           &argc, argv, NULL, NULL, 0);

	/* create text widget */
	ac=0;
	XtSetArg(al[ac], XmNheight, 100); ac++;
	XtSetArg(al[ac], XmNwidth,  200); ac++;
	w=XmCreateText(toplevel, "text", al, ac);
	XtManageChild(w);
	XtRealizeWidget(toplevel);

	timer = XtAppAddTimeOut(context, seconds*1000, handle_timer, NULL);

	XmTextSetString(w, lines);
	XmTextSetInsertionPosition(w, XmTextGetLastPosition(w));

	printf("program will exit automatically in %i seconds\n", seconds);
	
	XtAppMainLoop(context);
	
	exit(0);
}
