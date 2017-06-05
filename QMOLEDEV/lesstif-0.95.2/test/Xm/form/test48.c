/*
To:          LessTif <lesstif@hungry.com>
From:        Edouard Parmelan <Edouard.Parmelan@quadratec.fr>
Subject:     XmForm and XmATTACH_OPPOSITE_WIDGET
Date:        Mon, 15 Mar 1999 12:57:59 +0100

Rick,


I have found a bug with XmForm and XmATTACH_OPPOSITE_WIDGET.

In my application, I have three Widgets label1, label2 and label3
in a form.  [In the real life, label1 and label2 are XmRowColumn.]

The attachment of label2 are XmATTACH_OPPOSITE_WIDGET for top
and bottom to label1, I want the same height for the two labels.

label3 have top attachment to label2.

In my application, lable2 have a smallest height than label1 and
the height of the form is wrong, label3 is layout outside the
form :(

The result with Motif is correct.


If I change the top attachment of label3 to label1, it's correct
with Lesstif (and Motif too).


I write a small test case what show this bug.  Run without argument,
the label3 is outside the form (and don't show).  You can run it
with the parameter ``workaround'' to see the correct form.


Hope this help,
egp
-- 
Edouard G. Parmelan                         Ingenieur Developpeur
Quadratec - Parc Club "Orsay Universite" - 14/16,rue Jean Rostand
91893 Orsay Cedex - FRANCE               Phone (+33)1 69 33 20 80
Email: edouard.parmelan@quadratec.fr
*/
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
	Widget          top, form;
	Widget          label1, label2, label3, label4;
	Boolean		workaround = False;

	if ((app_argc == 2) && (strcmp(app_argv[1], "workaround") == 0))
	    workaround = True;

	XtSetArg(al[ac], XmNallowShellResize, TRUE);
	ac++;
	XtSetArg(al[ac], XmNtitle, "top and bottom XmATTACH_OPPOSITE_WIDGET");
	ac++;
	XtSetArg(al[ac], XmNargc, app_argc);
	ac++;
	XtSetArg(al[ac], XmNargv, app_argv);
	ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);

	ac = 0;

	top = XmCreateForm(appshell, "top", al, ac);
	XtManageChild(top);

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
	form = XmCreateForm(top, "form", al, ac);
	XtManageChild(form);

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
  	XtSetArg(al[ac], XmNwidth, 50); ac++;
  	XtSetArg(al[ac], XmNheight, 100); ac++;
	label1 = XmCreateLabel(form, "label1", al, ac);
	XtManageChild(label1);

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopWidget, label1); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftWidget, label1); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomWidget, label1); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
  	XtSetArg(al[ac], XmNwidth, 50); ac++;
  	XtSetArg(al[ac], XmNheight, 50); ac++;
	label2 = XmCreateLabel(form, "label2", al, ac);
	XtManageChild(label2);

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	if (workaround) {
	    XtSetArg(al[ac], XmNtopWidget, label1); ac++;
	}
	else {
	    XtSetArg(al[ac], XmNtopWidget, label2); ac++;
	}
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  	XtSetArg(al[ac], XmNwidth, 50); ac++;
  	XtSetArg(al[ac], XmNheight, 25); ac++;
	label3 = XmCreateLabel(form, "label3", al, ac);
	XtManageChild(label3);
	
	
	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNtopWidget, form); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
  	XtSetArg(al[ac], XmNwidth, 50); ac++;
  	XtSetArg(al[ac], XmNheight, 25); ac++;
	label4 = XmCreateLabel(top, "label4", al, ac);
	XtManageChild(label4);
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
	display = XtOpenDisplay(app_context, NULL, argv[0], "testEGP",
				NULL, 0, &argc, argv);
	if (!display) {
		printf("%s: can't open display, exiting...\n", argv[0]);
		exit(-1);
	}
	create_appshell(display, argv[0], argc, argv);
	XtRealizeWidget(appshell);
{
static XtWidgetGeometry Expected[] = {
CWWidth | CWHeight,		0,	0,	100,	150,	0,0,0,	/* top */
CWWidth | CWHeight | CWX | CWY,	0,	0,	100,	125,	0,0,0,	/* form */
CWWidth | CWHeight | CWX | CWY,	0,	0,	50,	100,	0,0,0,	/* label1 */
CWWidth | CWHeight | CWX | CWY,	50,	0,	50,	100,	0,0,0,	/* label2 */
CWWidth | CWHeight | CWX | CWY,	0,	100,	100,	25,	0,0,0,	/* label3 */
CWWidth | CWHeight | CWX | CWY,	0,	125,	50,	25,	0,0,0,	/* label4 */
};

    PrintDetails(appshell, Expected);
}
	LessTifTestMainLoop(appshell);
	exit(0);

}
