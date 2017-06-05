/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test19.c,v 1.3 2001/06/18 08:45:30 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/List.h>

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, listw;
    XmStringTable str_days;
    int i;
    int ac;
    Arg al[4];
    
    toplevel = XtVaAppInitialize(&app, "SINGLE", NULL, 0,
				 &argc, argv, NULL, NULL);

    ac = 0;
    XtSetArg(al[ac], XmNvisibleItemCount, 8); ac++;
    XtSetArg(al[ac], XmNselectionPolicy, XmBROWSE_SELECT); ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT); ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
    listw = XmCreateScrolledList(toplevel, "test_of_list", al, ac);
    XtManageChild(listw);

    XtRealizeWidget(toplevel);


{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,   72,  135, 0,0,0, /* test_of_listSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   72,  135, 0,0,0, /* test_of_list */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

    exit(0);
}
