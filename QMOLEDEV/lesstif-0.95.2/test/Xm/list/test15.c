/* $Id: test15.c,v 1.8 2002/05/01 15:54:45 amai Exp $
   Test 15.c: Test if various things that effect the geometry when sizePolicy 
              is XmRESIZE_IF_POSSIBLE
   Notes: Geometry does not change after widget is realized
*/
#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/ListP.h>
#include <Xm/PushBP.h>

#include "../../common/Test.h"

void
onActivate(Widget w, XtPointer foo1, XtPointer foo2)
{
   Widget list = (Widget) foo1;

}

int
main(int argc, char **argv)
{
  Widget toplevel, bb, list, pb;
  XtAppContext app;
  XmString item;
  Arg args[10];
  int n = 0;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ComBox", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
  XtManageChild(bb);

  /* this is variable by default */
  XtSetArg(args[n], XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE); n++;
  list = XmCreateScrolledList(bb, "combo", args, n);

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

  n = 0;
  XtSetArg(args[n], XmNy, XtY(list)+ XtHeight(list) + 40); n++;
  pb = XmCreatePushButton(bb,"Delete", args, n);
  XtManageChild(pb);
  XtAddCallback(pb,XmNactivateCallback, onActivate, (XtPointer) list);

  fprintf(stderr, "Tree after list is managed\n");
  XdbPrintResources(list);
  XdbPrintCompleteTree(toplevel);

#if 0
  /* attempt to resize after realize */
  XtVaSetValues(list, XmNwidth, 100, NULL);
#endif

  fprintf(stderr, "Before deleting all items\n");
  XdbPrintCompleteTree(toplevel);

  XmListDeleteAllItems(list);

  fprintf(stderr, "After deleting all items\n");
  XdbPrintCompleteTree(toplevel);

  XtRealizeWidget(toplevel);

  fprintf(stderr, "Tree after toplevel is realized\n");
  XdbPrintCompleteTree(toplevel);

  XdbPrintResources(list);

  XdbPrintCompleteTree(toplevel);

  XdbPrintCompleteTree(toplevel);

  item = XmStringCreateSimple("Item 12345678901234567890");
  XmListAddItem(list,item,0);

  XdbPrintCompleteTree(toplevel);



/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   69,   99, 0,0,0, /* bb */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   16,   42, 0,0,0, /* comboSW */
   CWWidth | CWHeight | CWX | CWY,    0,   27,   16,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   16,   23, 0,0,0, /* combo */
   CWWidth | CWHeight | CWX | CWY,   10,   63,   48,   25, 0,0,0, /* Delete */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}

