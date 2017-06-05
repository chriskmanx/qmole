/* $Id: test12.c,v 1.7 2002/05/01 15:54:45 amai Exp $
   Test 12.c: Test if width can directly be changed when sizePolicy is XmCONSTANT
   Result:  The width can directly be set prior to a realize.
*/
#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/ListP.h>

#include "../../common/Test.h"

int
main(int argc, char **argv)
{
  Widget toplevel, bb, list;
  XtAppContext app;
  XmString item;
  Arg args[10];
  int n = 0;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ComBox", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
  XtManageChild(bb);

  XtSetArg(args[n], XmNlistSizePolicy, XmCONSTANT); n++;
  list = XmCreateList(bb, "combo", args, n);

  fprintf(stderr, "Tree after creating list\n");
  XdbPrintCompleteTree(toplevel);
  XdbPrintResources(list);

#if 0
  putenv("DEBUGSOURCES=List.c");
#endif

  /* use list routine to add items */
  item = XmStringCreateSimple("Item 1");
  XmListAddItem(list,item,0);

#if 0
  putenv("DEBUGSOURCES=");
#endif

  fprintf(stderr, "Tree after adding list item 1\n");
  XdbPrintCompleteTree(toplevel);
  XdbPrintResources(list);

  /* lets see if the geometry will change as the items get bigger */
  item = XmStringCreateSimple("Item 12");
  XmListAddItem(list,item,0);
  item = XmStringCreateSimple("Item 123");
  XmListAddItem(list,item,0);
  item = XmStringCreateSimple("Item 1234");
  XmListAddItem(list,item,0);
  item = XmStringCreateSimple("Item 12345");
  XmListAddItem(list,item,0);

  XtManageChild(list);

  fprintf(stderr, "Tree after list is managed\n");
  XdbPrintResources(list);
  XdbPrintCompleteTree(toplevel);
  
  /* attempt to resize after realize */
  XtVaSetValues(list, XmNwidth, 100, NULL);
  
  XdbPrintCompleteTree(toplevel);

  XtRealizeWidget(toplevel);

  fprintf(stderr, "Tree after toplevel is realized\n");
  XdbPrintCompleteTree(toplevel);
  XdbPrintResources(list);

  XdbPrintCompleteTree(toplevel);

  /* attempt to resize after realize */
  XtVaSetValues(list, XmNwidth, 200, NULL);

  XdbPrintCompleteTree(toplevel);


{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  121,   44, 0,0,0, /* bb */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  100,   23, 0,0,0, /* combo */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

  exit(0);
}

