/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test8.c,v 1.8 2002/05/01 15:39:21 amai Exp $ */
/* test how lesstif handles a list with no elements -- DEFECT 1*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/List.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, listw;
    
    toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
				 &argc, argv, NULL, NULL);

    listw = XtVaCreateManagedWidget( "test_of_list",
				    xmListWidgetClass, toplevel,
				    NULL );

    XtRealizeWidget(toplevel);

    XdbPrintTree(toplevel);

{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,   57,   73,   16,   23, 0,0,0}, /* test_of_list */
};
#else
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   18,   26, 0,0,0, /* test_of_list */
};
#endif
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    exit(0);
}
