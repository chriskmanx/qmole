/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test7.c,v 1.7 2002/05/01 15:54:45 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/List.h>
#include <Xm/BulletinB.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget	toplevel, listw, bb;
    Arg		al[5];
    int		ac;
    
    toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
				 &argc, argv, NULL, NULL);

    bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, toplevel,
		XmNwidth,	200,
		XmNheight,	200,
	NULL);

    ac = 0;
    XtSetArg(al[ac], XmNvisibleItemCount, 5); ac++;
    XtSetArg(al[ac], XmNx, 20); ac++;
    XtSetArg(al[ac], XmNy, 20); ac++;
    listw = XmCreateScrolledList(bb, "list", al, ac);
    XtManageChild(listw);

    XtRealizeWidget(toplevel);

    XdbPrintTree(toplevel);


{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  200,  200, 0,0,0, /* bb */
   CWWidth | CWHeight | CWX | CWY,   20,   20,   48,   87, 0,0,0, /* listSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   48,   87, 0,0,0, /* list */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    exit(0);
}
