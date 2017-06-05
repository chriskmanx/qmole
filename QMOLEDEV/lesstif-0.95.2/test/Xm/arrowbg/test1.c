/* $Header: /cvsroot/lesstif/lesstif/test/Xm/arrowbg/test1.c,v 1.6 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>

#include "../../common/Test.h"

void
cb(Widget w, XtPointer userData, XtPointer cbs) {
  XmArrowButtonCallbackStruct *abcs = (XmArrowButtonCallbackStruct *)cbs;

  printf("ArrowBG Activated: click count: %d\n", abcs->click_count);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one,two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "ABG", NULL, 0, &argc, argv, NULL, NULL);

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel,
				XmNmarginWidth, 10, XmNmarginHeight, 10,
				NULL);

  one = XtVaCreateManagedWidget("One", xmArrowButtonGadgetClass, two,
				XmNwidth, 100, XmNheight, 100, NULL);

  XtAddCallback(one, XmNactivateCallback, cb, NULL);

  XtRealizeWidget(toplevel);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  121,  121, 0,0,0}, /* Two */
   {CWWidth | CWHeight | CWX | CWY,   10,   10,  100,  100, 0,0,0}, /* One */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

  exit(0);
}
