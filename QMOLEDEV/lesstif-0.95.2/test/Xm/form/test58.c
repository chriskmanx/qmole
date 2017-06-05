/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test58.c,v 1.3 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>

#include "../../common/Test.h"

static char *FallBack[] = {
		"*.borderWidth: 1",
		"*.geometrySlop: 1",
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget BottomLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
  {
  static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  278,  598,   72,   19, 0,0,0, /* Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   70,   17, 0,0,0, /* BottomLabel */},

   {CWWidth | CWHeight            ,  278,  598,  144,   76, 0,0,0, /* Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,  142,   17, 0,0,0, /* BottomLabel */},
};

  PrintDetails(Shell, Expected);
      LessTifTestWaitForIt(Shell);
      LessTifTestResizeWidget(Shell, 144, 76);
  PrintDetails(Shell, Expected);
      LessTifTestWaitForIt(Shell);
  {
  XtWidgetGeometry geo;
  XtWidgetGeometry expected_geo = {
	CWWidth | CWHeight, 0, 0, 72, 19, 0, 0, 0,
};

  	XtQueryGeometry(Form, NULL, &geo);
  	fprintf(stderr, "%i(%i) %ix%i%+i%+i(%ix%i%+i%+i)   ",
  		geo.request_mode,
  		expected_geo.request_mode,
  		geo.width,
  		geo.height,
  		geo.x,
  		geo.y,
  		expected_geo.width,
  		expected_geo.height,
  		expected_geo.x,
  		expected_geo.y);
  	if (geo.request_mode != expected_geo.request_mode ||
  	    geo.width != expected_geo.width ||
  	    geo.height != expected_geo.height ||
  	    geo.width != expected_geo.width ||
  	    geo.x != expected_geo.x ||
  	    geo.y != expected_geo.y)
	{
	    fprintf(stderr, "Bad\n");
	    GlobalErrors++;
	}
	else
	{
	    fprintf(stderr, "Good\n");
	}
  }
  }
      LessTifTestMainLoop(Shell);
  exit(0);
}
