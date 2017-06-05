/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test6.c,v 1.7 2002/05/01 15:54:45 amai Exp $ */

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
    
    toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
				 &argc, argv, NULL, NULL);

    str_days = (XmStringTable) XtMalloc(7 * sizeof(XmString*));
    for(i=0; i<7; ++i)
	str_days[i] = XmStringCreateSimple(days[i]);

    listw = XmCreateScrolledList(toplevel, "test_of_scrolled_list", NULL, 0);

    XtVaSetValues(listw, XmNitemCount, 7, XmNvisibleItemCount, 5,
		  XmNtopItemPosition, 2, XmNitems, str_days, 
                  XmNselectionPolicy, XmBROWSE_SELECT,
                  NULL);

    XtManageChild(listw);

    for(i=0; i<7; ++i)
	XmStringFree(str_days[i]);
    XtFree((XtPointer)str_days);
  
    XtRealizeWidget(toplevel);

    XdbPrintTree(toplevel);


{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,   83,   87, 0,0,0, /* test_of_scrolled_listSW */
   CWWidth | CWHeight | CWX | CWY,   68,    0,   15,   87, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   64,   87, 0,0,0, /* test_of_scrolled_list */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    exit(0);
}
