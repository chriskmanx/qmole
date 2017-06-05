/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test22.c,v 1.1 2004/07/18 18:52:51 dannybackx Exp $
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include "../../common/Test.h"

char *	fallback[] = {
	"*XmForm.background:			yellow",
	"*XmPushButton.background:		orange",
	"*XmScrolledWindowWidget.background:	green",
	"*.background:				blue",
	NULL
};

Widget toplevel, form, sw, f1, f2, b1, b2;

void doit(Widget w, XtPointer client, XtPointer call)
{
	Widget				f = (Widget)client;
	XmToggleButtonCallbackStruct	*cbp = (XmToggleButtonCallbackStruct *)call;

	if (cbp->set) {
		XtUnmanageChild(f2);
		XtManageChild(f1);
	} else {
		XtUnmanageChild(f1);
		XtManageChild(f2);
	}
}

int main(int argc, char **argv)
{
	XtAppContext app;

	XtSetLanguageProc(NULL, NULL, NULL);
	toplevel = XtAppInitialize(&app, "Label", NULL, 0, &argc, argv, fallback, NULL, 0);

	form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
		  XmNheight,	300,
		  XmNwidth,	500,
		  NULL);

	sw  = XtVaCreateManagedWidget("sw", 
                                xmScrolledWindowWidgetClass, 
                                form, 
                                XmNscrollingPolicy, XmAUTOMATIC,
                                XmNscrollBarPlacement, XmBOTTOM_RIGHT,
				XmNtopAttachment,	XmATTACH_FORM,
				XmNbottomAttachment,	XmATTACH_POSITION,
				XmNbottomPosition,	80,
				XmNleftAttachment,	XmATTACH_FORM,
				XmNrightAttachment,	XmATTACH_FORM,
                                NULL);

	f1 = XtVaCreateManagedWidget("f1",
                                xmFormWidgetClass,
                                sw,
                                XmNwidth,	30,
				XmNheight,	30,
                                NULL);

	f2 = XtVaCreateWidget("f2",
                                xmFormWidgetClass,
                                sw,
				XmNx,		50,
				XmNy,		50,
                                XmNwidth,	50,
				XmNheight,	50,
                                NULL);

	b1 = XtVaCreateManagedWidget("b1", xmToggleButtonWidgetClass, form,
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		sw,
		  XmNbottomAttachment,	XmATTACH_FORM,
		  XmNleftAttachment,	XmATTACH_FORM,
		  XmNleftOffset,	20,
		  XmNrightAttachment,	XmATTACH_NONE,
		  XmNset,		1,
		  NULL);
	XtAddCallback(b1, XmNvalueChangedCallback, doit, (XtPointer)f1);

	XtRealizeWidget(toplevel);
	LessTifTestMainLoop(toplevel);
	exit(0);
}
