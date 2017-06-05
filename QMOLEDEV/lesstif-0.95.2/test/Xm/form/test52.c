/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test52.c,v 1.3 2001/05/16 09:37:13 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <X11/CompositeP.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/FormP.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/SeparatoG.h>

#include "../../common/Test.h"


static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

XtGeometryHandler OldGeometryManager;
XtSetValuesFunc OldConstraintSetValues;

XtGeometryResult NewGeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
	printf("NewGeomtryManager(%s) - start\n", XtName(w));
	return (*OldGeometryManager)(w, request, reply);
}

Boolean NewConstraintSetValues(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	printf("NewConstraintSetValues(%s)\n", XtName(new_w));
	return (*OldConstraintSetValues)(current, request, new_w, args, num_args);
}


int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Form;
  Widget BottomLabel;

  OldGeometryManager = xmFormClassRec.composite_class.geometry_manager;
  xmFormClassRec.composite_class.geometry_manager = (void *)NewGeometryManager;
  OldConstraintSetValues = xmFormClassRec.constraint_class.set_values;
  xmFormClassRec.constraint_class.set_values = (void *)NewConstraintSetValues;
  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);
  XtVaSetValues(Shell,
  	XmNallowShellResize, True,
  	NULL);

  Form = XmCreateForm(Shell,"Form",NULL,0);

  BottomLabel = XmCreateLabel(Form,"BottomLabel",NULL,0);
  XtVaSetValues(BottomLabel,
  	XmNbottomAttachment, XmATTACH_POSITION,
  	XmNbottomPosition, 50,
  	NULL);
  XtManageChild(BottomLabel);

  XtManageChild(Form);

  XtRealizeWidget(Shell);
      /*
      PrintDetails(Shell, NULL);
      */
      LessTifTestWaitForIt(Shell);
    printf("SetValues()\n");
      XtVaSetValues(BottomLabel,
      	XmNleftOffset, 50,
      	/*
      	XmNheight, 50,
      	*/
      	NULL);
    printf("Resize()\n");
      XtVaSetValues(Form,
      	XmNheight, 140,
      	NULL);
      LessTifTestMainLoop(Shell);
  exit(0);
}
