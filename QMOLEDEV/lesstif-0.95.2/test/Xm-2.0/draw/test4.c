/* $Id: test4.c,v 1.3 2002/05/01 15:47:32 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinBP.h>
#include <Xm/DrawnBP.h>
#include <Xm/DrawP.h>

#include "../../common/Test.h"


#ifndef Prim_BottomShadowGC
#define Prim_BottomShadowGC(w) \
    (((XmPrimitiveWidget)(w))->primitive.bottom_shadow_GC)
#endif

#ifndef Prim_TopShadowGC
#define Prim_TopShadowGC(w) \
    (((XmPrimitiveWidget)(w))->primitive.top_shadow_GC)
#endif

#ifndef MGR_BackgroundGC
#define MGR_BackgroundGC(w) \
       (((XmManagerWidget)(w))->manager.background_GC)
#endif

void
onExpose(Widget w, XtPointer udata, XtPointer cdata)
{
  Widget mgr = (Widget) udata;

  /* draw into window */
  XmeDrawArrow(XtDisplay(w), 
               XtWindow(w), 
               Prim_BottomShadowGC(w), /* bottom gc */
               Prim_TopShadowGC(w),    /* top gc */
               MGR_BackgroundGC(mgr), /* fill gc */
               5, 5, 22, 22, 1, XmARROW_DOWN);
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

  XtSetArg(args[n], XmNwidth, 100); n++;
  XtSetArg(args[n], XmNheight, 100); n++;
  widget = XmCreateDrawnButton(bb, "drawnButton", args, n);

  XtAddCallback(widget, XmNexposeCallback, (XtCallbackProc) onExpose,
                (XtPointer)bb);

  XtManageChild(widget);

  XtRealizeWidget(toplevel);

  LessTifTestMainLoop(toplevel);

  exit(0);
}
