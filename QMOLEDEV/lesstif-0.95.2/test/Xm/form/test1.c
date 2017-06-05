/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test1.c,v 1.11 2002/05/01 15:39:21 amai Exp $*/
/**
 *
 * form1.c
 *
 **/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/FormP.h>

#include "../../common/Test.h"


char *fallback[] = {
	"*XmForm.marginWidth:	45",
	"*XmForm.marginHeight:	30",
	"*XmForm.background:	dark slate blue",
	"*XmForm.?.background:	sea green",
	"*foreground:		yellow",
	NULL
};

void
focus(Widget w, XtPointer data, XtPointer cbs)
{
    printf("focus moved\n");
}

void
buttonPopup(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    printf("Widget: %s : BUTTON3! %i %i\n", XtName(w),
    		((XButtonEvent *)event)->x,
    		((XButtonEvent *)event)->y);
}

static char but_trans[] = "\
#override <Btn3Down> : buttonPopup()\n";

static char form_trans[] = "\
#override <Btn3Down> : buttonPopup()\n";

static XtActionsRec app_actions[] = {
	{ "buttonPopup", buttonPopup }
};

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Form1", NULL, 0, &argc, argv, fallback, NULL);

  XtAppAddActions(app, app_actions, XtNumber(app_actions));
  printf("Shell class >%s<\n",toplevel->core.widget_class->core_class.class_name);

  one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
				XtNtranslations,
				XtParseTranslationTable(form_trans), NULL);
  XtAddCallback(one, XmNfocusCallback, focus, NULL);

#if 1
  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_FORM,
				/*
				XtNtranslations,
				XtParseTranslationTable(but_trans),
				*/
				NULL);
  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget, two,
				XmNrightAttachment, XmATTACH_FORM,
				XtNtranslations,
				XtParseTranslationTable(but_trans),
				NULL);
#else
  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNleftAttachment, XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				XmNtopAttachment, XmATTACH_FORM,
				NULL);
  two = XtVaCreateManagedWidget("two", xmPushButtonWidgetClass, one,
				XmNleftAttachment, XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget, two,
				XmNbottomAttachment, XmATTACH_FORM,
				NULL);
#endif

  XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	{CWWidth | CWHeight,		0,	0,	150,	85,	0,0,0,	/* Form */},
  	{CWWidth | CWHeight | CWX | CWY,	45,	30,	30,	25,	0,0,0,	/* two */},
  	{CWWidth | CWHeight | CWX | CWY,	75,	30,	30,	25,	0,0,0,	/* two */},
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);

  exit(0);
}
