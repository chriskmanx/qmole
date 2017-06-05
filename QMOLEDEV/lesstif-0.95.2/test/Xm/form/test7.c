/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test7.c,v 1.7 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
  Widget	top, form, w;
  XtAppContext	app;
  int		i, j;
  char		n[5];

  XtSetLanguageProc(NULL, NULL, NULL);

  top = XtVaAppInitialize(&app, "Form", NULL, 0, &argc, argv, NULL, NULL);

/* Note : no size given ! */
  form = XtVaCreateManagedWidget("form", xmFormWidgetClass, top,
		XmNfractionBase,	3,
	NULL);

  for (i=0; i<3; i++)
    for (j=0; j<3; j++) {
      sprintf(n, "b%d%d", i, j);
      w = XtVaCreateManagedWidget(n, xmPushButtonWidgetClass, form,
		XmNtopAttachment,	XmATTACH_POSITION,
		XmNbottomAttachment,	XmATTACH_POSITION,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNtopPosition,		i,
		XmNbottomPosition,	i+1,
		XmNleftPosition,	j,
		XmNrightPosition,	j+1,
	NULL);
    }

  XtRealizeWidget(top);
  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,		0,	0,	90,	75,	0,0,0,	/* Form */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	0,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	30,	0,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	60,	0,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	25,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	30,	25,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	60,	25,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	0,	50,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	30,	50,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	60,	50,	30,	25,	0,0,0,	/* two */},
};

  PrintDetails(top, Expected);
  }
      LessTifTestMainLoop(top);
  exit(0);
}
