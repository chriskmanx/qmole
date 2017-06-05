/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test4.c,v 1.7 2002/05/01 15:54:45 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/List.h>

#include "../../common/Test.h"

char *days[] = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
		     "Friday", "Saturday" };

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, listw;
    XmStringTable str_days;
    int i;
    
    toplevel = XtVaAppInitialize(&app, "EXTENDED", NULL, 0,
				 &argc, argv, NULL, NULL);

    str_days = (XmStringTable) XtMalloc(7 * sizeof(XmString*));
    for(i=0; i<7; ++i)
	str_days[i] = XmStringCreateSimple(days[i]);

    listw = XtVaCreateManagedWidget( "test_of_list",
				    xmListWidgetClass, toplevel,
				    XmNselectionPolicy, XmEXTENDED_SELECT,
				    XmNitemCount, 7,
				    XmNvisibleItemCount, 7,
				    XmNitems, str_days,
				    NULL );

    for(i=0; i<7; ++i)
	XmStringFree(str_days[i]);
    XtFree((XtPointer)str_days);
  
    XtRealizeWidget(toplevel);

    XdbPrintTree(toplevel);


{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   64,  119, 0,0,0, /* test_of_list */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    exit(0);
}
