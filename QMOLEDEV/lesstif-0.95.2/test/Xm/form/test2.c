/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test2.c,v 1.7 2002/05/01 15:39:21 amai Exp $ */
/**
 *
 * form2.c
 *
 **/

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>

#include "../../common/Test.h"


char *fallback[] = {
	"*XmForm.marginWidth:	45",
	"*XmForm.marginHeight:	30",
	"*XmForm.background:	dark slate blue",
	"*XmForm.?.background:	sea green",
	"*foreground:		yellow",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Form1", NULL, 0, &argc, argv, fallback, NULL);

  one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				XmNfractionBase, 4,
				NULL);

  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNtopAttachment, XmATTACH_POSITION,
				XmNtopPosition, 1,
				XmNbottomAttachment, XmATTACH_POSITION,
				XmNbottomPosition, 2,
				XmNleftAttachment, XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				NULL);

  XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,              0,  0,120,100, 0,0,0,	/* Form */},
  	{CWWidth | CWHeight | CWX | CWY, 45,25, 30, 25, 0,0,0,	/* two */},
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
