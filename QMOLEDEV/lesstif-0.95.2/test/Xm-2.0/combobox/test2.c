/* $Id: test2.c,v 1.4 2002/05/01 15:47:31 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/ComboBoxP.h>
#include <Xm/PushB.h>
#include <Xm/List.h>

#include "../../common/Test.h"

#if !defined(CB_List)
#define CB_List(w) \
        (((XmComboBoxWidget)(w))->combo_box.list)
#endif

/* This test sees how the combobox insert child routine handles 
   adding one child.

   RESULT: This test exposes a bug with the OSF/Motif 2.0 
   combobox.  The insert child routine should spit out an error
   but instead puts the push button at position 0,0 in the combobox.  
   This is probably because the child combobox count is still at three.  
   Also, this tells us the layout routine only handles the textfield and 
   the scrolled window only and does not loop through its child list when
   doing the layout.  Run test3 to see the error it should spit
   out.
 */

void
check_geometry(Widget w)
{
   static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
{  CWWidth | CWHeight            ,   10,   10,  386,   94, 0,0,0 }, /* combo */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  374,   50, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    6,   56,  374,   32, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  359,    0,   15,   32, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  359,   32, 0,0,0 }, /* List */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,   76,   44, 0,0,0 }, /* push */
};

#if 0
   PrintDetails2(w, NULL);
#else
  if (result_index <= 0)
  {
     PrintDetails2(w, Expected);
     fflush(stdout);
     result_index ++;
  }
#endif
}

int
main(int argc, char **argv)
{
  Widget toplevel, widget, bb, list, label, push;
  XtAppContext app;
  XmString item;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ComBox", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
  XtManageChild(bb);

  widget = XmCreateComboBox(bb, "combo", NULL, 0);

  /* add a push button to the combobox */
  push = XmCreatePushButton(widget, "push", NULL, 0);
  XtManageChild(push);

  /* use list routine to add items */
  list = CB_List(widget);

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

  XmComboBoxUpdate(widget);

  XtManageChild(widget);

  XtRealizeWidget(toplevel);

  check_geometry(widget);

  LessTifTestMainLoop(toplevel);

  exit(0);
}
