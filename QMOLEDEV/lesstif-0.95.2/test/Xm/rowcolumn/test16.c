/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

#include <stdio.h>

Widget          edit_popup = (Widget)NULL;

void 
post_edit_menu(w, event, params, num_params)
	Widget          w;
	XEvent         *event;
	String         *params;
	Cardinal       *num_params;
{
	/* Position the MenuPane (which actually positions the XmMenuShell) */
	XmMenuPosition(edit_popup, (XButtonPressedEvent *)event);
	/* Managing the MenuPane makes the XmMenuShell pop up */
	XtManageChild(edit_popup);
}

/* Action table for XtAppAddActions() */
XtActionsRec    my_actions[] = {
	{"PostEditMenu", post_edit_menu},
};

XtAppContext    app_context;
Display        *display;	/* Display */

int
main(argc, argv)
	int             argc;
	char          **argv;
{
	Widget          appshell;
	Widget          form;
	Widget          text;
	Widget          cut_b, copy_b, paste_b;
	Widget          children[3];	/* Children to manage */
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */

	XtSetLanguageProc((XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL);
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
	display = XtOpenDisplay(app_context, NULL, argv[0], "XApplication",
				NULL, 0, &argc, argv);
	if (!display) {
		printf("%s: can't open display, exiting...\n", argv[0]);
		exit(-1);
	}
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Popup Menu"); ac++;
	XtSetArg(al[ac], XmNargc, argc); ac++;
	XtSetArg(al[ac], XmNargv, argv); ac++;
	appshell = XtAppCreateShell(argv[0], "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	form = XmCreateForm(appshell, "form", al, ac);
	ac = 0;
	XtManageChild(form);

	/* Create XmText widget (managed later) */
	ac = 0;
	text = XmCreateText(form, "text", al, ac);
	XtSetArg(al[ac], XmNmenuPost, "<Button3>"); ac++;
	/* Create but do not manage the MenuPane (and XmMenuShell) */
	edit_popup = XmCreatePopupMenu(text, "edit_popup", al, ac);
	ac = 0;
	/* Create and manage the XmPushButtons */
	cut_b = XmCreatePushButton(edit_popup, "cut_b", al, ac);
	copy_b = XmCreatePushButton(edit_popup, "copy_b", al, ac);
	paste_b = XmCreatePushButton(edit_popup, "paste_b", al, ac);
	children[ac++] = cut_b;
	children[ac++] = copy_b;
	children[ac++] = paste_b;
	XtManageChildren(children, ac);
	/*
	 * Add actions and translation to invoke post * action on appropriate
	 * event
	 */
	XtAppAddActions(app_context, my_actions, XtNumber(my_actions));
	XtOverrideTranslations(text, XtParseTranslationTable
			       ("<Btn3Down>:PostEditMenu()"));
	XtManageChild(text);

	XtRealizeWidget(appshell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	138,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	138,	31,	0,0,0,
};

  PrintDetails(appshell, Expected);
  }
    LessTifTestMainLoop(appshell);
    /*
	XtAppMainLoop(app_context);
	*/
	exit(0);
}
