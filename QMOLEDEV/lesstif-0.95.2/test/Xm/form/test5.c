/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test5.c,v 1.7 2002/05/01 15:39:21 amai Exp $ */
/**
 *
 * form5.c
 *
 **/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

#include "../../common/Test.h"


void
callback(Widget w,
         XtPointer p1,
         XtPointer p2)
{
  XtVaSetValues(w,
                XmNwidth, 50,
                NULL);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Form1", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				XmNresizable, True, NULL);

  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_NONE,
				XmNrightAttachment, XmATTACH_FORM,
				NULL);

  XtAddCallback(two, XmNactivateCallback, callback, NULL);

  XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,		0,	0,	30,	25,	0,0,0,	/* Form */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	30,	25,	0,0,0,	/* two */},
};

  PrintDetails(toplevel, Expected);
  }
      LessTifTestMainLoop(toplevel);

  exit(0);
}
