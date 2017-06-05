#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/GrabShell.h>
#include <Xm/List.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"

/*
This tests if the grab shell widget can contain multiple children.
*/
Widget grabshell;

void
check_geometry()
{
  static int result_index = 0;

  static XtWidgetGeometry Expected[] =
  {
/* result test 0 */
   CWWidth | CWHeight            ,    0,    0,   46,   23, 0,0,0, /* list */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   42,   25, 0,0,0, /* hello */
/* result test 1 */
   CWWidth | CWHeight            ,    2,    2,   46,   23, 0,0,0, /* list */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   42,   25, 0,0,0, /* hello */
};

  if (result_index <= 1)
  {
     /* Only have results for two test.  Must record more results before
        doing any further testing otherwise you start looking at bogus
        memory locations.
      */
     PrintDetails(grabshell, Expected);
     result_index ++;
  }
}

void
onPopup(Widget w, XtPointer mydata, XtPointer cbs)
{
  check_geometry();
}

void onActivate(Widget w, XtPointer mydata, XtPointer cbs)
{
  Widget grabshell = (Widget) mydata;

  XtPopupSpringLoaded(grabshell);
}

int
main(int argc, char **argv)
{
  Widget toplevel, bb, list, pressme, pb;
  XtAppContext app;
  XmString item;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "GrabShell", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);

  
  pressme = XmCreatePushButton(bb,"Press me and let's see what the grab shell does.",NULL,0);
  
  XtManageChild(pressme);

  XtManageChild(bb);

  grabshell = XmCreateGrabShell(bb, "grab", NULL, 0);
  list = XmCreateList(grabshell, "list", NULL, 0);

  /* use list routine to add items */
  item = XmStringCreateSimple("Item 1");
  XmListAddItem(list,item,0);
  item = XmStringCreateSimple("Item 2");
  XmListAddItem(list,item,0);
  item = XmStringCreateSimple("Item 3");
  XmListAddItem(list,item,0);
  item = XmStringCreateSimple("Item 4");
  XmListAddItem(list,item,0);
  item = XmStringCreateSimple("Item 5");
  XmListAddItem(list,item,0);

  XtManageChild(list);

  pb = XmCreatePushButton(grabshell,"hello", NULL, 0);
  XtManageChild(pb);

  /* setup callback for an activate action */
  XtAddCallback(pressme, 
                XmNactivateCallback,
                (XtCallbackProc) onActivate, 
                (XtPointer) grabshell);

  XtAddCallback(grabshell,
                XmNpopupCallback,
                (XtCallbackProc) onPopup,
                (XtPointer) grabshell);

  XtRealizeWidget(toplevel);

  check_geometry();

  LessTifTestMainLoop(toplevel);

  exit(0);
}

