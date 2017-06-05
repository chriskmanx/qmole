/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test22.c,v 1.4 2002/05/01 15:54:45 amai Exp $ */
#if 0
From:        Pavel Roskin <pavel_roskin@geocities.com>
To:          lesstif@hungry.com
Subject:     Fix for color changing in XmList
Date:        Sat, 24 Jul 1999 16:25:58 +0400 (EEST)

--- lib/Xm/List.c	Fri Jun 18 03:28:30 1999
+++ lib/Xm/List.c	Sat Jul 24 15:33:11 1999
@@ -3561,6 +3561,20 @@
 	CreateHighlightGC(new_w);
     }
 
+    if (XtBackground(old) != XtBackground(new_w) ||
+	Prim_Foreground(old) != Prim_Foreground(new_w))
+    {
+	XtReleaseGC(new_w, List_NormalGC(new_w));
+	XtReleaseGC(new_w, List_InsensitiveGC(new_w));
+	XtReleaseGC(new_w, List_InverseGC(new_w));
+	XtReleaseGC(new_w, List_HighlightGC(new_w));
+
+	CreateNormalGC(new_w);
+	CreateInsensitiveGC(new_w);
+	CreateInverseGC(new_w);
+	CreateHighlightGC(new_w);
+    }
+
     if (need_newgeo)
     {
 	_XmListSetGeometry(new_w);
--- test/Xm/list/Makefile.am	Sun Jun 13 01:43:25 1999
+++ test/Xm/list/Makefile.am	Sat Jul 24 15:40:05 1999
@@ -7,7 +7,7 @@
 AUX_OBJS =
 noinst_PROGRAMS = test1  test2  test3  test4  test5  test6  test7  test8 \
 	          test9  test10 test11 test12 test13 test14 test15 test16 \
-	          test17 test18 test19 test20 test21
+	          test17 test18 test19 test20 test21 test22
 
 LTTEST  = $(top_builddir)/common/libLtTest.a
 
--- /dev/null	Wed May  6 00:32:27 1998
+++ test/Xm/list/test22.c	Sat Jul 24 15:44:10 1999
@@ -0,0 +1,58 @@
#endif

/* Test for setting foreground and background after initialization */
/* Background and foreground colors are swapped */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/ListP.h>

#include "../../common/Test.h"

char *days[] = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
		     "Friday", "Saturday" };

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, listw;
    XmStringTable str_days;
    Pixel bg, fg;
    int i;
    
    toplevel = XtVaAppInitialize(&app, "BROWSE", NULL, 0,
				 &argc, argv, NULL, NULL);

    str_days = (XmStringTable) XtMalloc(7 * sizeof(XmString*));
    for(i=0; i<7; ++i)
	str_days[i] = XmStringCreateSimple(days[i]);

    listw = XtVaCreateManagedWidget( "test_of_list",
				    xmListWidgetClass, toplevel,
				    XmNselectionPolicy, XmBROWSE_SELECT,
				    XmNitemCount, 7,
				    XmNvisibleItemCount, 7,
				    XmNitems, str_days,
				    NULL );

    /* Swap background and foreground colors */
    XtVaGetValues(listw, XmNforeground, &fg, XmNbackground, &bg, NULL);
    XtVaSetValues(listw, XmNforeground, bg, XmNbackground, fg, NULL);

    for(i=0; i<7; ++i)
	XmStringFree(str_days[i]);
    XtFree((XtPointer)str_days);
  
    XtRealizeWidget(toplevel);

    XdbPrintTree(toplevel);


{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   64,  119, 0,0,0, /* test_of_list */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    exit(0);
}
