/* 
   $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test9.c,v 1.6 2002/05/01 15:54:45 amai Exp $
 
   Same as scrolledwindow/test14.c except it shows the sizing 
   DEFECT was in the List widget not the ScrolledWindow widget. 
*/


#include <stdlib.h>
#include <stdio.h>

#include <Xm/ListP.h>

#include "../../common/Test.h"

char *days[] = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
		     "Friday", "Saturday" };

void output_list_cbs(Widget w, XtPointer client_data, XtPointer call_data)
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *)call_data;
    String buf;

    switch (cbs->reason)
    {
    case XmCR_DEFAULT_ACTION:
	printf ("Default Action Callback\n");
	/* the only valid items are:
               reason
               item
               item_length
               item_position
               event 
         */
	XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &buf);
	printf ("   Item         : %s\n", buf);
	printf ("   Item Position: %d\n", cbs->item_position);
	printf ("   Item Length  : %d\n", cbs->item_length);

	break;
    case XmCR_BROWSE_SELECT:
	printf ("Browse Select Callback\n");
	/* the only valid items are:
               reason
               item
               item_length
               item_position
               event 
         */
	XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &buf);
	printf ("   Item         : %s\n", buf);
	printf ("   Item Position: %d\n", cbs->item_position);
	printf ("   Item Length  : %d\n", cbs->item_length);
	break;
    case XmCR_SINGLE_SELECT:
	printf ("Single Select Callback\n");
	/* the only valid items are:
               reason
               item
               item_length
               item_position
               event 
         */
	XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &buf);
	printf ("   Item         : %s\n", buf);
	printf ("   Item Position: %d\n", cbs->item_position);
	printf ("   Item Length  : %d\n", cbs->item_length);
	break;
    case XmCR_MULTIPLE_SELECT:
	printf ("Multiple Select Callback\n");
	break;
    case XmCR_EXTENDED_SELECT:
	printf ("Extended Select Callback\n");
	break;
    }
}

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, listw;
    XmStringTable str_days;
    int i;
    Arg al[3];
    int ac = 0;
    
    toplevel = XtVaAppInitialize(&app, "BROWSE", NULL, 0,
				 &argc, argv, NULL, NULL);

    str_days = (XmStringTable) XtMalloc(7 * sizeof(XmString*));
    for(i=0; i<7; ++i)
	str_days[i] = XmStringCreateSimple(days[i]);

    
    XtSetArg(al[ac], XmNselectionPolicy, XmBROWSE_SELECT); ac++;
    XtSetArg(al[ac], XmNitemCount, 7); ac++;
    XtSetArg(al[ac], XmNitems, str_days); ac++;

    listw = XmCreateList( toplevel, "list", al, ac);
    XtManageChild(listw);

    XtAddCallback(listw, XmNdefaultActionCallback, output_list_cbs, NULL);
    XtAddCallback(listw, XmNbrowseSelectionCallback, output_list_cbs, NULL);
    XtAddCallback(listw, XmNbrowseSelectionCallback, XdbPrintTreeCB, toplevel);

    for(i=0; i<7; ++i)
	XmStringFree(str_days[i]);
    XtFree((XtPointer)str_days);
  
    XtRealizeWidget(toplevel);

    XdbPrintTree(toplevel);


{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   64,   23, 0,0,0, /* list */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    exit(0);
}
