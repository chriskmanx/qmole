/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test13.c,v 1.12 2002/05/01 15:39:21 amai Exp $
 */

#include <stdio.h>
#include <stdlib.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/Text.h>

#include "../../common/Test.h"


Widget          appshell = (Widget) NULL;

void 
create_appshell(display, app_name, app_argc, app_argv)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
{
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */
	Widget form;
	Widget label1, label2, label3;
	Widget text1, text2, text3;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Column layout using position attachments"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;

	form = XmCreateForm(appshell, "form", al, ac);
	XtVaSetValues(form,
		XmNhorizontalSpacing, 12,
		XmNverticalSpacing, 12,
		NULL);
	label1 = XmCreateLabel(form, "label1", al, ac);
	XtVaSetValues(label1,
		XmNlabelString, XmStringCreateSimple("Label"),
		NULL);
	text1 = XmCreateText(form, "text1", al, ac);
	label2 = XmCreateLabel(form, "label2", al, ac);
	XtVaSetValues(label2,
		XmNlabelString, XmStringCreateSimple("Long label"),
		NULL);
	text2 = XmCreateText(form, "text2", al, ac);
	label3 = XmCreateLabel(form, "label3", al, ac);
	XtVaSetValues(label3,
		XmNlabelString, XmStringCreateSimple("L"),
		NULL);
	text3 = XmCreateText(form, "text3", al, ac);
	XtVaSetValues(label1,
		      XmNtopAttachment,
		      XmATTACH_OPPOSITE_WIDGET,
		      XmNtopOffset, 0,
		      XmNtopWidget, text1,
		      XmNbottomAttachment,
		      XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomOffset, 0,
		      XmNbottomWidget, text1,
		      XmNleftAttachment, XmATTACH_FORM,
		      NULL);

	XtVaSetValues(text1,
		      XmNtopAttachment, XmATTACH_FORM,
		      XmNbottomAttachment, XmATTACH_NONE,
		      XmNleftAttachment, XmATTACH_POSITION,
		      XmNleftPosition, 35,
		      XmNrightAttachment, XmATTACH_FORM,
		      NULL);

	XtVaSetValues(label2,
		      XmNtopAttachment,
		      XmATTACH_OPPOSITE_WIDGET,
		      XmNtopOffset, 0,
		      XmNtopWidget, text2,
		      XmNbottomAttachment,
		      XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomOffset, 0,
		      XmNbottomWidget, text2,
		      XmNleftAttachment, XmATTACH_FORM,
#if 1
		      XmNrightAttachment, XmATTACH_WIDGET,
		      XmNrightWidget, text2,
#else
		      XmNrightAttachment, XmATTACH_POSITION,
		      XmNrightPosition, 35,
#endif
		      NULL);

	XtVaSetValues(text2,
		      XmNtopAttachment, XmATTACH_WIDGET,
		      XmNtopWidget, text1,
		      XmNbottomAttachment, XmATTACH_NONE,
		      XmNleftAttachment, XmATTACH_POSITION,
		      XmNleftPosition, 35,
		      XmNrightAttachment, XmATTACH_FORM,
		      NULL);

	XtVaSetValues(label3,
		      XmNtopAttachment,
		      XmATTACH_OPPOSITE_WIDGET,
		      XmNtopOffset, 0,
		      XmNtopWidget, text3,
		      XmNbottomAttachment,
		      XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomOffset, 0,
		      XmNbottomWidget, text3,
		      XmNleftAttachment, XmATTACH_FORM,
		      XmNrightAttachment, XmATTACH_NONE,
		      NULL);

	XtVaSetValues(text3,
		      XmNtopAttachment, XmATTACH_WIDGET,
		      XmNtopWidget, text2,
		      XmNbottomAttachment, XmATTACH_NONE,
		      XmNleftAttachment, XmATTACH_POSITION,
		      XmNleftPosition, 35,
		      XmNrightAttachment, XmATTACH_FORM,
		      NULL);

	XtManageChild(label1);
	XtManageChild(label2);
	XtManageChild(label3);
	XtManageChild(text1);
	XtManageChild(text2);
	XtManageChild(text3);
	XtManageChild(form);
}



XtAppContext    app_context;
Display        *display;	/* Display             */

int 
main(argc, argv)
	int             argc;
	char          **argv;
{
	/* Pick up resources from current directory */
	putenv("XAPPLRESDIR=.");

	XtSetLanguageProc((XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL);
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
	display = XtOpenDisplay(app_context, NULL, argv[0], "Form13",
				NULL, 0, &argc, argv);
	if (!display) {
		printf("%s: can't open display, exiting...\n", argv[0]);
		exit(-1);
	}
	create_appshell(display, argv[0], argc, argv);
	XtRealizeWidget(appshell);
  {
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  214,  384,  248,  129, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,   12,   12,   34,   31, 0,0,0, /* label1 */},
   {CWWidth | CWHeight | CWX | CWY,   87,   12,  149,   31, 0,0,0, /* text1 */},
   {CWWidth | CWHeight | CWX | CWY,   12,   55,   63,   31, 0,0,0, /* label2 */},
   {CWWidth | CWHeight | CWX | CWY,   87,   55,  149,   31, 0,0,0, /* text2 */},
   {CWWidth | CWHeight | CWX | CWY,   12,   98,   10,   31, 0,0,0, /* label3 */},
   {CWWidth | CWHeight | CWX | CWY,   87,   98,  149,   31, 0,0,0, /* text3 */},
};
#else
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	251,	129,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	12,	12,	34,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	88,	12,	151,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	12,	55,	64,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	88,	55,	151,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	12,	98,	10,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	88,	98,	151,	31,	0,0,0,	/* two */
};
#endif

  LessTifTestSetSlop(appshell, 2);
  PrintDetails(appshell, Expected);
  }
	    LessTifTestMainLoop(appshell);
	exit(0);
}
