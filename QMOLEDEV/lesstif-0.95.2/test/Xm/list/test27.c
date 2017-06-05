/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test27.c,v 1.1 2004/09/26 14:11:57 dannybackx Exp $ */
#include <stdlib.h>
#include <stdio.h>

#include <Xm/ListP.h>

#include "../../common/Test.h"


char *days[] = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
		     "Friday", "Saturday" };
XmStringTable str_days;

void output_list_cbs(Widget w, XtPointer client_data, XtPointer call_data)
{
	int	i = 7;
	while (i--) {
		XmListDeleteItem(w, str_days[i]);
	}
	XmListAddItem(w, XmStringCreateSimple("Yow"), 0);
}

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, listw;
    int i;
    
    toplevel = XtVaAppInitialize(&app, "BROWSE", NULL, 0,
				 &argc, argv, NULL, NULL);

    str_days = (XmStringTable) XtMalloc(7 * sizeof(XmString*));
    for(i=0; i<7; ++i)
	str_days[i] = XmStringCreateSimple(days[i]);

    listw = XtVaCreateManagedWidget( "test_of_list",
				    xmListWidgetClass, toplevel,
				    XmNselectionPolicy, XmBROWSE_SELECT,
				    XmNitemCount, 7,
				    XmNvisibleItemCount, 7,
				    XmNitems, str_days,
				    NULL );

    XtAddCallback(listw, XmNdefaultActionCallback, output_list_cbs, NULL);
    XtAddCallback(listw, XmNbrowseSelectionCallback, output_list_cbs, NULL);

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
