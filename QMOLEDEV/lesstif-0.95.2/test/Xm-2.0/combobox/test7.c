/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/combobox/test7.c,v 1.8 2002/05/01 15:47:31 amai Exp $ */
/* test for XmQUICK_NAVIGATE and various geometry defects */

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

void onSelect(Widget w, XtPointer foo, XtPointer foobar)
{
   XdbPrintResources(w);
   foo = foo;
   foobar = foobar;
}

void
check_geometry(Widget w)
{
   static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
{  CWWidth | CWHeight            ,  107,  124,  446,   83, 0,0,0 }, /* test7.motif */
{  CWWidth | CWHeight            ,  107,  124,  446,   83, 0,0,0 }, /* bb */
{  CWWidth | CWHeight | CWX | CWY,   10,   10,  425,   62, 0,0,0 }, /* combo */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  374,   50, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  421,   36, 0,0,0 }, /* GrabShell */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  417,   32, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  402,    0,   15,   32, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  402,   32, 0,0,0 }, /* List */
/* result test 1 */
{  CWWidth | CWHeight            ,  107,  124,  446,   83, 0,0,0 }, /* test7.motif */
{  CWWidth | CWHeight            ,  107,  124,  446,   83, 0,0,0 }, /* bb */
{  CWWidth | CWHeight | CWX | CWY,   10,   10,  425,   62, 0,0,0 }, /* combo */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  374,   50, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  421,   36, 0,0,0 }, /* GrabShell */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  417,   32, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  402,    0,   15,   32, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  402,   32, 0,0,0 }, /* List */
/* result test 2 */
{  CWWidth | CWHeight            ,  107,  124,  446,   83, 0,0,0 }, /* test7.motif */
{  CWWidth | CWHeight            ,  107,  124,  446,   83, 0,0,0 }, /* bb */
{  CWWidth | CWHeight | CWX | CWY,   10,   10,  425,   62, 0,0,0 }, /* combo */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  374,   50, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  421,   36, 0,0,0 }, /* GrabShell */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  417,   32, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  402,    0,   15,   32, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  402,   32, 0,0,0 }, /* List */
/* result test 3 */
{  CWWidth | CWHeight            ,  107,  124,  446,   83, 0,0,0 }, /* test7.motif */
{  CWWidth | CWHeight            ,  107,  124,  446,   83, 0,0,0 }, /* bb */
{  CWWidth | CWHeight | CWX | CWY,   10,   10,  425,   62, 0,0,0 }, /* combo */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  374,   50, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  421,   36, 0,0,0 }, /* GrabShell */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  417,   32, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  402,    0,   15,   32, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  402,   32, 0,0,0 }, /* List */
};

#if 0
   PrintDetails2(w, NULL);
#else
  if (result_index <= 3)
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
  Widget toplevel, widget, bb, list;
  XtAppContext app;
  XmString item;
  Arg args[10];
  int n = 0;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ComBox", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
  XtManageChild(bb);

  fprintf(stderr,"XmQUICK_NAVIGATE=%d\n",XmQUICK_NAVIGATE);
  XtSetArg(args[n], XmNmatchBehavior, XmQUICK_NAVIGATE); n++;

#if 0
  XtSetArg(args[n], XmNarrowSize, 40); n++;
  XtSetArg(args[n], XmNarrowSpacing, 5); n++;
  XtSetArg(args[n], XmNmarginWidth, 4); n++;
  XtSetArg(args[n], XmNmarginHeight, 4); n++;
#endif
  widget = XmCreateDropDownComboBox(bb, "combo", args, n);

  XtAddCallback(widget, XmNselectionCallback, (XtCallbackProc) onSelect, NULL);

#if 0
  /* causes a core dump and reports a warning */
  XtDestroyWidget(CB_EditBox(widget));
  /* causes a core dump */
  XtDestroyWidget(CB_List(widget));
  /* reports warning if list or edit box are unmanaged */
  XtUnmanageChild(CB_List(widget));
  XtUnmanageChild(CB_EditBox(widget));
  /* does not report warning if scrolled window is unmanaged */
  XtUnmanageChild(CB_ScrolledW(widget));
#endif

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

  fprintf(stderr, "After XmComboBoxUpdate\n");
  check_geometry(toplevel);

  XtManageChild(widget);
  fprintf(stderr, "After XtManageChild\n");

  check_geometry(toplevel);

  XtRealizeWidget(toplevel);

  fprintf(stderr, "After XtRealizeWidget\n");
  check_geometry(toplevel);

#if 0
  XdbPrintResources(CB_ListShell(widget));

  XdbPrintResources(widget);
#endif

  XdbPrintResources(list);

  check_geometry(toplevel);

  LessTifTestMainLoop(toplevel);

  exit(0);
}

