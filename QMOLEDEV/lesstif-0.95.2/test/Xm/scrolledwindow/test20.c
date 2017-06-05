/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test20.c,v 1.2 2001/05/23 14:38:38 amai Exp $ */
#if 0
From:        Martin Simmons <martin@harlequin.co.uk>
To:          lesstif@hungry.com
Subject:     XmScrolledWindow shrinks to 1x1
Date:        Tue, 27 Jul 99 15:12:14 BST

Certain sequences of scrollbar manage/unmanage operations triggered by XmList
can cause the XmScrolledWindow to shrink to 1x1 (0x0 actually, but it corrects
itself).  I tracked this down to _XmScrolledWPreferredSize() being called from
XmScrolledWindows change_managed method: in the bad case, SW_FromResize is
set to True, which can happen when the XmScrolledWindow is configuring its
children from its resize() method as below:

#0  _XmScrolledWPreferredSize (w=0x8059b58, child=0x0, childgeom=0x0, vals=0xbffff488) at ScrolledW.c:1910
#1  0x400db71c in change_managed (w=0x8059b58) at ScrolledW.c:1306
#2  0x4018776d in UnmanageChildren ()
#3  0x4018781a in XtUnmanageChildren ()
#4  0x401878e3 in XtUnmanageChild ()
#5  0x4008e6de in _XmListSetSBManageHoriz (w=0x80688b0, manage_changed=0xbffff5ad "") at List.c:1392
#6  0x4008ea3b in _XmListInitScrollBars (w=0x80688b0, horiz=0x1, vert=0x1) at List.c:1500
#7  0x4009167c in resize (w=0x80688b0) at List.c:2940
#8  0x40034b30 in ResizeWrapper (w=0x80688b0, IntentedWrapperDepth=0x4) at BaseClass.c:1261
#9  0x40033d0b in ResizeWrapper4 (w=0x80688b0) at BaseClass.c:575
#10 0x401828b2 in XtConfigureWidget ()
#11 0x4006c9f0 in _XmConfigureObject (g=0x80688b0, x=0x0, y=0x0, width=0x64, height=0x51, border_width=0x0) at GadgetUtil.c:174
#12 0x400dec1d in _XmScrolledWConfigureChildren (w=0x8059b58, child=0x0, childgeom=0x0, vals=0xbffff71c) at ScrolledW.c:2761
#13 0x400dac6c in resize (w=0x8059b58) at ScrolledW.c:923
#14 0x40034b30 in ResizeWrapper (w=0x8059b58, IntentedWrapperDepth=0x6) at BaseClass.c:1261
#15 0x40033d53 in ResizeWrapper6 (w=0x8059b58) at BaseClass.c:577
#16 0x401828b2 in XtConfigureWidget ()
#17 0x4018269a in XtResizeWidget ()
#18 0x40192786 in Resize ()
#19 0x40034b30 in ResizeWrapper (w=0x80651d0, IntentedWrapperDepth=0x5) at BaseClass.c:1261
#20 0x40033d2f in ResizeWrapper5 (w=0x80651d0) at BaseClass.c:576
#21 0x4019337c in EventHandler ()
#22 0x4017f6dc in XtDispatchEventToWidget ()
#23 0x4017feb7 in _XtDefaultDispatcher ()
#24 0x401801da in XtDispatchEvent ()


Here is a patch to 0.88.9, which moves the width/height setting from resize()
to _XmScrolledWPreferredSize() so that it happens in the reentrant cases as
well:

------------------------------------------------------------------------
--- orig/lesstif-0.88.9/lib/Xm/ScrolledW.c	Mon Jun 14 18:22:18 1999
+++ lesstif-0.88.9/lib/Xm/ScrolledW.c	Tue Jul 27 11:03:41 1999
@@ -915,9 +915,6 @@
     /* resize not chained */
     _XmScrolledWPreferredSize(w, NULL, NULL, &vals);
 
-    vals.SwW = XtWidth(w);
-    vals.SwH = XtHeight(w);
-
     _XmScrolledWLayout(w, NULL, NULL, &vals);
 
     _XmScrolledWConfigureChildren(w, NULL, NULL, &vals);
@@ -1907,7 +1904,7 @@
 	/* T. Straumann: at creation time, their scrolledw isn't really
 	 *				 variable but it sets the size (scrolledwindow/test19).
 	 */
-		if (XtIsRealized(w))
+		if (XtIsRealized(w) && !SW_FromResize(w))
 		{
 		vals->SwW = 0;
 		vals->SwH = 0;
------------------------------------------------------------------------


My test case is below (its a little weird because the widget hierarchy was
adapted from a pd widget implementation weve been using).  Note that the size
only goes wrong after the event loop has run.

With lesstif 0.89.9:
$ scrolledw-resize
>>>>>> Setting items
<<<<<< done
>>>>>> Calling XtPopup
<<<<<< done
before event processing:
 list width 28 height 121
 shell width 28 height 140
after event processing:
 list width 1 height 134
 shell width 1 height 1
$ 

With Motif 2.1.10:
$ scrolledw-resize
>>>>>> Setting items
<<<<<< done
>>>>>> Calling XtPopup
<<<<<< done
before event processing:
 list width 28 height 134
 shell width 28 height 134
after event processing:
 list width 28 height 134
 shell width 28 height 134
$ 


-------------------------- scrolledw-resize.c --------------------------
#endif

#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <stdio.h>
#include <stdlib.h>

#include "../../common/Test.h"

Widget toplevel, shell, list;

int
main(int argc, char **argv)
{
  XtAppContext app;
  Arg args[10];
  int n;
  Dimension list_height;
  Dimension lw, lh, sw, sh;
  XmString items[1];

  toplevel = XtVaAppInitialize(&app, "Scrolledw-resize", NULL, 0, &argc, argv, NULL, NULL);
  XtManageChild(XmCreateLabel(toplevel, "ScrolledW:test20", NULL, 0));
  XtRealizeWidget(toplevel);

  shell = XtVaCreatePopupShell("shell", 
			       overrideShellWidgetClass,
			       toplevel,
			       XmNwidth, 152,
			       XmNheight, 1,
			       XmNborderWidth, 0,
			       XmNallowShellResize, True,
			       NULL, 0);
  XtRealizeWidget(shell);

  n = 0;
  XtSetArg(args[n], XmNlistMarginHeight, 0); n++;
  XtSetArg(args[n], XmNlistMarginWidth, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE); n++;
  XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmAS_NEEDED); n++;
  XtSetArg(args[n], XmNvisibleItemCount, 10); n++;
  list = XmCreateScrolledList(shell, "list", args, n);
  XtManageChild(list);

  items[0] = XmStringCreateLocalized("abcd");
  n = 0;
  XtSetArg(args[n], XmNitems, items); n++;
  XtSetArg(args[n], XmNitemCount, 1); n++;
  printf(">>>>>> Setting items\n");
  XtSetValues(list, args, n);
  printf("<<<<<< done\n");

  printf(">>>>>> Calling XtPopup\n");
  XtPopup(shell, XtGrabNone);
  printf("<<<<<< done\n");

  XtVaGetValues(list, XmNwidth, &lw, XmNheight, &lh, NULL);
  XtVaGetValues(shell, XmNwidth, &sw, XmNheight, &sh, NULL);
  printf("before event processing:\n");
  printf(" list width %d height %d\n", lw, lh);
  printf(" shell width %d height %d\n", sw, sh);

  LessTifTestWaitForIt(shell);
  while(XtAppPending(app)) {
    XEvent event;
    XtAppNextEvent(app, &event);
    XtDispatchEvent(&event);
  }  

  XtVaGetValues(list, XmNwidth, &lw, XmNheight, &lh, NULL);
  XtVaGetValues(shell, XmNwidth, &sw, XmNheight, &sh, NULL);
  printf("after event processing:\n");
  printf(" list width %d height %d\n", lw, lh);
  printf(" shell width %d height %d\n", sw, sh);
  {
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   28,  140, 0,0,0, /* listSW */
   CWWidth | CWHeight | CWX | CWY,    0,  125,   28,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   28,  121, 0,0,0, /* list */
};

  PrintDetails(shell, Expected);
  }

  LessTifTestMainLoop(toplevel);
  exit(0);
}
