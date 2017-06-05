/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/combobox/test14.c,v 1.1 2002/05/24 18:38:04 dannybackx Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/ComboBoxP.h>
#include <Xm/List.h>

#include "../../common/Test.h"

#if !defined(CB_List)
#define CB_List(w) \
        (((XmComboBoxWidget)(w))->combo_box.list)
#endif


void
check_geometry(Widget w)
{
   static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
{  CWWidth | CWHeight            ,   10,   10,  425,   62, 0,0,0 }, /* combo */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  374,   50, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  421,   36, 0,0,0 }, /* GrabShell */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  417,   32, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  402,    0,   15,   32, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  402,   32, 0,0,0 }, /* List */
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
  Widget toplevel, widget, bb;
  XtAppContext app;
  XmString item;
  Arg args[10];
  int n = 0;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ComBox", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
  XtManageChild(bb);

  n = 0;
  XtSetArg(args[n], XmNvisibleItemCount, 0); n++;
  widget = XmCreateDropDownComboBox(bb, "combo", args, n);
  XtManageChild(widget);

  item = XmStringCreateSimple("Item 1");
  XmComboBoxAddItem(widget,item,0, TRUE);
  XmStringFree(item);
  item = XmStringCreateSimple("Item 2");
  XmComboBoxAddItem(widget,item,0, TRUE);
  XmStringFree(item);
  item = XmStringCreateSimple("Item 3");
  XmComboBoxAddItem(widget,item,0, TRUE);
  XmStringFree(item);
  item = XmStringCreateSimple("Item 4");
  XmComboBoxAddItem(widget,item,0, TRUE);
  XmStringFree(item);
  item = XmStringCreateSimple("Item 5");
  XmComboBoxAddItem(widget,item,0, TRUE);
  XmStringFree(item);
#if 0
  XmComboBoxUpdate(widget);
#endif
  XtRealizeWidget(toplevel);

#if 1
  XdbPrintResources(CB_ListShell(widget));
  XdbPrintResources(widget);
#endif

  check_geometry(widget);

  LessTifTestMainLoop(toplevel);

  exit(0);
}

