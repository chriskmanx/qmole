/* $Header: /cvsroot/lesstif/lesstif/test/Xm/baseclass/test2.c,v 1.3 2001/06/11 08:26:30 amai Exp $
 * fast subclass test
 */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/BaseClassP.h>
#include <Xm/Label.h>
/* believe it or not, we just tested the fast subclassing.  Just to make sure */
#include <Xm/XmP.h>
#include <Xm/PushBP.h>

#include "../../common/Test.h"

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("One", xmPushButtonWidgetClass, toplevel, NULL);

  if (XmIsLabel(one))
	printf("IS LABEL\n");
  else
	printf("IS NOT LABEL\n");

  XtRealizeWidget(toplevel);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   30,   25, 0,0,0, /* One */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
