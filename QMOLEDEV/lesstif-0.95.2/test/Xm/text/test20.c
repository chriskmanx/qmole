/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test20.c,v 1.6 2002/03/30 17:18:02 amai Exp $ */
/*  demonstrates a problem within _XmTextInvalidate()
    --> SF [Bug #124809]
   writtten by "TYLER, Paul" <pct@ansto.gov.au>
  
   amai:
      Strange SF bug number?!
      See  SF Bugs [ 447163 ] test/Xm/text/test20 fails
      
      Earlier we had a SEGFAULT around in that routine.
      See test21 for more info
*/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/Text.h>

/* timeout: */
#define seconds 7

const char *lines[] = 	{
	"This is line 1\n",
	"--------\n",
	"		And line 3 is a very very very very longish line\n",
	"--	Maybe the tabs are an issue\n",
	"--	Though this version  crashes\n",
	"**	way   before\n",
	"***	the lines with the tabs. So..\n",
	"%%	I have  no  idea\n",
	"%%	what is  causing\n",
	"^^	this problem. I hope\n",
	"^^	this little\n",
	"&&	program  will   help\n",
	"@	find the problem....\n",
	"$$	Running out of fill..........\n",
	"!!	So that will have to do\n",
	NULL
};


void
handle_timer(XtPointer client_data, XtIntervalId *id)
{
	exit(0);
}


int
main(int argc,char *argv[])
{
	Arg al[2];
	int ac, i;
	char str[4096];
	XtAppContext context;
	Widget toplevel, w;
	XtIntervalId timer;


	/* create the toplevel shell */
	toplevel = XtAppInitialize(&context, "", NULL, 0,
	                           &argc, argv, NULL, NULL, 0);

	/* create text widget */
	ac=0;
	XtSetArg(al[ac], XmNheight, 200); ac++;
	XtSetArg(al[ac], XmNwidth,  200); ac++;
	w=XmCreateText(toplevel, "text", al, ac);
	XtManageChild(w);
	XtRealizeWidget(toplevel);

	timer = XtAppAddTimeOut(context, seconds*1000, handle_timer, NULL);

	strcpy(str, "");
	for ( i=0 ; lines[i] != NULL ; i++ )
	{
		strcat(str, lines[i]);
		XmTextSetString(w, str);
		XmTextSetInsertionPosition(w, XmTextGetLastPosition(w));
	}

	printf("program will exit automatically in %i seconds\n", seconds);
	
	XtAppMainLoop(context);
	
	exit(0);
}
