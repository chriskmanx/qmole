/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/combobox/test6.c,v 1.7 2002/05/01 15:47:31 amai Exp $ */

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
   char		*s;
   XmString	xms;

   XdbPrintResources(w);
   foo = foo;
   foobar = foobar;

   XtVaGetValues(w, XmNselectedItem, &xms, NULL);

   fprintf(stderr, "GetValues(%s) -> %p\n", XtName(w), xms);

   s = (char *)XmStringUnparse(xms, NULL, XmCHARSET_TEXT,
		XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
   fprintf(stderr, "XmStringUnparse -> %s\n", s);
   XtFree(s);
}

void
check_geometry(Widget w)
{
   static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
{  CWWidth | CWHeight            ,   10,   10,  458,   82, 0,0,0 }, /* combo */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  374,   70, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  454,   36, 0,0,0 }, /* GrabShell */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  450,   32, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  435,    0,   15,   32, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  435,   32, 0,0,0 }, /* List */
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
  Widget toplevel, widget, bb, list;
  XtAppContext app;
  XmString item;
  Arg args[10];
  int n = 0;
  char *s;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ComBox", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
  XtManageChild(bb);

  XtSetArg(args[n], XmNarrowSize, 70); n++;
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

  s = (char *)XmStringUnparse(item, NULL, XmCHARSET_TEXT,
		XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  fprintf(stderr, "XmStringUnparse -> %s\n", s);
  XtFree(s);

  XmComboBoxUpdate(widget);

  XtManageChild(widget);

  XtRealizeWidget(toplevel);

#if 1
  XdbPrintResources(CB_ListShell(widget));

  XdbPrintResources(widget);
#endif

  check_geometry(widget);

  LessTifTestMainLoop(toplevel);

  exit(0);
}

