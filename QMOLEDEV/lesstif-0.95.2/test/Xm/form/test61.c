/*
 * Try to reproduce bug #531123 - nedit statistics line problems
 *
 * http://sourceforge.net/tracker/index.php?func=detail&aid=531123&group_id=8596&atid=108596
 *
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test61.c,v 1.1 2002/07/30 20:12:30 dannybackx Exp $ 
 */

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

void disappear(Widget w, XtPointer client, XtPointer call)
{
	/* This makes the widget disappear by unmanaging itself. */
	XtUnmanageChild(w);
}

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
		XmNresizePolicy,	XmRESIZE_NONE,
		NULL);

	a = XtVaCreateManagedWidget("a", xmPushButtonWidgetClass, form,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		10,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		10,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	3,
		XmNbottomAttachment,	XmATTACH_FORM,
		XmNbottomOffset,	10,
		NULL);
	XtAddCallback(a, XmNactivateCallback, disappear, NULL);

	b = XtVaCreateManagedWidget("b", xmPushButtonWidgetClass, form,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		10,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		a,
		XmNleftOffset,		10,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNbottomAttachment,	XmATTACH_FORM,
		XmNbottomOffset,	10,
		NULL);
	XtManageChild(form);

	XtRealizeWidget(toplevel);

	LessTifTestWaitForIt(toplevel);
//	LessTifTestPushButton(a);
	LessTifTestMainLoop(toplevel);
#if 0
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
#endif
	return(0);
}
