/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <stdio.h>


Widget          appshell = (Widget) NULL;
extern int putenv();

void
create_appshell(display, app_name, app_argc, app_argv)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
{
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */
	Dimension       widest, width, spacing;
	Widget          form;
	Widget          label1, label2, label3;
	Widget          text1, text2, text3;

	XtSetArg(al[ac], XmNallowShellResize, TRUE);
	ac++;
	XtSetArg(al[ac], XmNtitle, "Column layout using calculated offsets");
	ac++;
	XtSetArg(al[ac], XmNargc, app_argc);
	ac++;
	XtSetArg(al[ac], XmNargv, app_argv);
	ac++;
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

	ac = 0;
	XtSetArg(al[ac], XmNwidth, &width); ac++;
	XtGetValues(label1, al, ac);
	widest = width;
	XtGetValues(label2, al, ac);
	if (width > widest)
		widest = width;
	XtGetValues(label1, al, ac);
	if (width > widest)
		widest = width;
	XtVaGetValues(form, XmNhorizontalSpacing, &spacing, NULL);

	XtVaSetValues(label1,
		      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNtopOffset, 10,
		      XmNtopWidget, text1,
		      XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomOffset, 0,
		      XmNbottomWidget, text1,
		      XmNleftAttachment, XmATTACH_FORM,
		      NULL);

	XtVaSetValues(text1,
		      XmNtopAttachment, XmATTACH_FORM,
		      XmNbottomAttachment, XmATTACH_NONE,
		      XmNleftAttachment, XmATTACH_FORM,
		      XmNleftOffset, widest + spacing,
		      XmNrightAttachment, XmATTACH_FORM,
		      NULL);

	XtVaSetValues(label2,
		      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNtopOffset, 0,
		      XmNtopWidget, text2,
		      XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomOffset, 0,
		      XmNbottomWidget, text2,
		      XmNleftAttachment, XmATTACH_FORM,
		      NULL);

	XtVaSetValues(text2,
		      XmNtopAttachment, XmATTACH_WIDGET,
		      XmNtopWidget, text1,
		      XmNbottomAttachment, XmATTACH_NONE,
		      XmNleftAttachment, XmATTACH_FORM,
		      XmNleftOffset, widest + spacing,
		      XmNrightAttachment, XmATTACH_FORM,
		      NULL);

	XtVaSetValues(label3,
		      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNtopOffset, 0,
		      XmNtopWidget, text3,
		      XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomOffset, 0,
		      XmNbottomWidget, text3,
		      XmNleftAttachment, XmATTACH_FORM,
		      NULL);

	XtVaSetValues(text3,
		      XmNtopAttachment, XmATTACH_WIDGET,
		      XmNtopWidget, text2,
		      XmNbottomAttachment, XmATTACH_NONE,
		      XmNleftAttachment, XmATTACH_FORM,
		      XmNleftOffset, widest + spacing,
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
	display = XtOpenDisplay(app_context, NULL, argv[0], "Form12",
				NULL, 0, &argc, argv);
	if (!display) {
		printf("%s: can't open display, exiting...\n", argv[0]);
		exit(-1);
	}
	create_appshell(display, argv[0], argc, argv);
	XtRealizeWidget(appshell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	226,	129,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	12,	22,	34,	21,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	76,	12,	138,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	12,	55,	64,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	76,	55,	138,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	12,	98,	10,	31,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	76,	98,	138,	31,	0,0,0,	/* two */
};

  PrintDetails(appshell, Expected);
  }
	    LessTifTestMainLoop(appshell);
	exit(0);
}
