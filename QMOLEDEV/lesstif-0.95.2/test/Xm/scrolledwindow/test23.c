
/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test23.c,v 1.1 2004/10/19 21:58:05 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/ScrolledW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget sw, rc, button;
  int i;

  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  sw  = XtVaCreateManagedWidget("sw", 
                                xmScrolledWindowWidgetClass, 
                                toplevel, 
                                XmNscrollingPolicy, XmAUTOMATIC,
                                NULL);

  rc = XtVaCreateManagedWidget("da",
                               xmRowColumnWidgetClass,
                               sw,
                               NULL);

  for (i=0; i<30; i++) {
	  char t[20];
	  sprintf(t, "button %d", i);
	  button = XtVaCreateManagedWidget(t, xmPushButtonWidgetClass, rc, NULL);
  }

  XtRealizeWidget(toplevel);
  LessTifTestMainLoop(toplevel);

  exit(0);
}
