/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/combobox/test11.c,v 1.5 2002/05/01 15:47:31 amai Exp $ */
/* Purpose: Test the XmNlayoutDirection resource (CG only) */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/RowColumn.h>
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
{  CWWidth | CWHeight            ,    0,    0,  418,   58, 0,0,0 }, /* combo */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  370,   46, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  414,   42, 0,0,0 }, /* GrabShell */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  410,   38, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  395,    0,   15,   38, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  395,   38, 0,0,0 }, /* List */
};

#if 1
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

void print_direction(Widget w)
{
   XmDirection dir;

   XtVaGetValues(w, XmNlayoutDirection, &dir, NULL);

   switch(dir)
   {
   case XmDEFAULT_DIRECTION: 
      printf("XmDEFAULT_DIRECTION\n");
      break;
   case XmRIGHT_TO_LEFT_TOP_TO_BOTTOM:
      printf("XmRIGHT_TO_LEFT_TOP_TO_BOTTOM\n");
      break;
   case XmLEFT_TO_RIGHT_TOP_TO_BOTTOM: 
      printf("XmLEFT_TO_RIGHT_TOP_TO_BOTTOM\n");
      break;
   case XmRIGHT_TO_LEFT_BOTTOM_TO_TOP: 
      printf("XmRIGHT_TO_LEFT_BOTTOM_TO_TOP\n");
      break;
   case XmLEFT_TO_RIGHT_BOTTOM_TO_TOP:
      printf("XmLEFT_TO_RIGHT_BOTTOM_TO_TOP\n");
      break;
   case XmTOP_TO_BOTTOM_RIGHT_TO_LEFT:
      printf("XmTOP_TO_BOTTOM_RIGHT_TO_LEFT\n");
      break;
   case XmTOP_TO_BOTTOM_LEFT_TO_RIGHT:
      printf("XmTOP_TO_BOTTOM_LEFT_TO_RIGHT\n");
      break;
   case XmBOTTOM_TO_TOP_RIGHT_TO_LEFT:
      printf("XmBOTTOM_TO_TOP_RIGHT_TO_LEFT\n");
      break;
   case XmBOTTOM_TO_TOP_LEFT_TO_RIGHT: 
      printf("XmBOTTOM_TO_TOP_LEFT_TO_RIGHT\n");
      break;
   case XmTOP_TO_BOTTOM: 
      printf("XmTOP_TO_BOTTOM\n");
      break;
   case XmBOTTOM_TO_TOP:
      printf("XmBOTTOM_TO_TOP\n");
      break;
   case XmRIGHT_TO_LEFT: 
      printf("XmRIGHT_TO_LEFT\n");
      break;
   case XmLEFT_TO_RIGHT:
      printf("XmLEFT_TO_RIGHT\n");
      break;
   default:
      printf("Unknown received value %d\n",dir);
   } 
}

void direction_test(Widget parent, XmDirection dir)
{
  Widget widget, list;
  XmString item;
  Arg args[10];
  int n;
  static char *foo = "Hello World";

  n = 0;
  XtSetArg(args[n], XmNlayoutDirection, dir); n++;
  XtSetArg(args[n], XmNuserData, foo); n++;
  widget = XmCreateDropDownList(parent, "combo", args, n);

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

  print_direction(widget);
}

int
main(int argc, char **argv)
{
  Widget toplevel, bb;
  XtAppContext app;
  Arg args[10];
  int n;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ComBox", NULL, 0, &argc, argv, NULL, NULL);

  n = 0;
  XtSetArg(args[n], XmNnumColumns, 2); n++;
  XtSetArg(args[n], XmNpacking, XmPACK_COLUMN); n++;
  bb = XmCreateRowColumn(toplevel, "bb", args, n);
  XtManageChild(bb);

  direction_test(bb,(XmDirection) XmDEFAULT_DIRECTION); 
  direction_test(bb,(XmDirection) XmRIGHT_TO_LEFT_TOP_TO_BOTTOM); 
  direction_test(bb,(XmDirection) XmLEFT_TO_RIGHT_TOP_TO_BOTTOM); 
  direction_test(bb,(XmDirection) XmRIGHT_TO_LEFT_BOTTOM_TO_TOP); 
  direction_test(bb,(XmDirection) XmLEFT_TO_RIGHT_BOTTOM_TO_TOP); 
  direction_test(bb,(XmDirection) XmTOP_TO_BOTTOM_RIGHT_TO_LEFT); 
  direction_test(bb,(XmDirection) XmTOP_TO_BOTTOM_LEFT_TO_RIGHT); 
  direction_test(bb,(XmDirection) XmBOTTOM_TO_TOP_RIGHT_TO_LEFT); 
  direction_test(bb,(XmDirection) XmBOTTOM_TO_TOP_LEFT_TO_RIGHT); 
  direction_test(bb,(XmDirection) XmTOP_TO_BOTTOM); 
  direction_test(bb,(XmDirection) XmBOTTOM_TO_TOP); 
  direction_test(bb,(XmDirection) XmRIGHT_TO_LEFT); 
  direction_test(bb,(XmDirection) XmLEFT_TO_RIGHT); 

  XtRealizeWidget(toplevel);

#if 0
  XdbPrintResources(CB_ListShell(widget));
  XdbPrintResources(widget);
  check_geometry(bb);
#endif

  LessTifTestMainLoop(toplevel);

  exit(0);
}

