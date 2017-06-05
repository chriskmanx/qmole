/* $Header: /cvsroot/lesstif/lesstif/test/Xm/selectionbox/test8.c,v 1.5 2001/06/18 14:05:31 amai Exp $
   test of selection boxes */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/SelectioB.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
  String Contents[] = {"one", "two", "three"};
  XtAppContext app;
  Widget toplevel, box;
  XmString Table[XtNumber(Contents)];
  int i;
  Arg args[2];
  int n;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  for (i = 0; i < XtNumber(Contents); i++)
  {
  	Table[i] = XmStringCreateSimple(Contents[i]);
  }
  n = 0;
  XtSetArg(args[n], XmNlistItems, Table); n++;
  XtSetArg(args[n], XmNlistItemCount, XtNumber(Contents)); n++;
  box = XmCreateSelectionBox(toplevel, "Box", args, n);

  XtManageChild(box);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  220,  297, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  198,   17, 0,0,0, /* Items */
   CWWidth | CWHeight | CWX | CWY,   11,   28,  198,  135, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  198,  135, 0,0,0, /* ItemsList */
   CWWidth | CWHeight | CWX | CWY,   11,  173,  198,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   11,  190,  198,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,  231,  220,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  243,   66,   43, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   77,  243,   66,   43, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  143,  243,   66,   43, 0,0,0, /* Help */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
