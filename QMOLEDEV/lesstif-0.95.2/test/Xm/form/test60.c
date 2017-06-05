/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test60.c,v 1.1 2002/06/15 16:31:07 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


XtAppContext appcontext;
Widget toplevel, form;

int main(int argc, char *argv[])
{
	Widget	a, b;

	XtSetLanguageProc(NULL, NULL, NULL);
	toplevel = XtVaAppInitialize(&appcontext, "Test", NULL, 0, &argc, argv,
				     NULL, NULL);

/* Create main window */
	form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
		XmNfractionBase,	7,
		XmNwidth,		350,
		XmNheight,		100,
		NULL);

	a = XtVaCreateManagedWidget("label a", xmPushButtonWidgetClass, form,
		XmNx,			50,
		XmNwidth,		100,
		XmNtopAttachment,	XmATTACH_POSITION,
		XmNtopPosition,		1,
		XmNleftAttachment,	XmATTACH_SELF,
		XmNrightAttachment,	XmATTACH_SELF,
		XmNbottomAttachment,	XmATTACH_POSITION,
		XmNbottomPosition,	5,
		NULL);

	b = XtVaCreateManagedWidget("label b", xmPushButtonWidgetClass, form,
		XmNx,			200,
		XmNwidth,		100,
		XmNtopAttachment,	XmATTACH_POSITION,
		XmNtopPosition,		1,
		XmNleftAttachment,	XmATTACH_SELF,
		XmNrightAttachment,	XmATTACH_SELF,
		XmNbottomAttachment,	XmATTACH_POSITION,
		XmNbottomPosition,	5,
		NULL);
	XtManageChild(form);

	XtRealizeWidget(toplevel);

    {
/* Note: the following values are the result of
 * querying the current geometry.
 */
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  350,  100, 0,0,0, /* mainwindow */},
   {CWWidth | CWHeight | CWX | CWY,   50,   14,  100,   57, 0,0,0, /* menubar */},
   {CWWidth | CWHeight | CWX | CWY,  200,   14,  100,   57, 0,0,0, /* button_0 */},
};

	PrintDetails(toplevel, Expected);
    }

	/*
	XtAppMainLoop(appcontext);
	*/
	LessTifTestMainLoop(toplevel);

	return(0);
}
