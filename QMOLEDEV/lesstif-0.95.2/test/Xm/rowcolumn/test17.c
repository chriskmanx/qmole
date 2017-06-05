/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <stdio.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/Label.h>

Widget          appshell = (Widget) NULL;
Widget          form = (Widget) NULL;


void doit(Widget w, XtPointer client, XtPointer call)
{
	static int	count = 1;
	XmString	xms;
	char		text1[16], text2[16];

	sprintf(text1, "Ctrl<Key>%c", 'A' + count);
	sprintf(text2, "Ctrl-%c", 'A' + count);

	count++;

	xms = XmStringCreateSimple(text2);
	XtVaSetValues(w,
			XmNacceleratorText,	xms,
			XmNaccelerator,		text1,
			NULL);
	XmStringFree(xms);
}

void
create_pulldown(form)
	Widget          form;
{
	Arg             al[1];
	int             ac;
	Widget          menubar;
	Widget          file_cascade;
	Widget          file_pulldown;
	Widget          new_b, open_b;
	Widget          save_cascade;
	Widget          save_pulldown;
	Widget          source_b, text_b, binary_b;
	Widget          children[3];

	ac = 0;
	menubar = XmCreateMenuBar(form, "menubar", al, ac);

	file_pulldown = XmCreatePulldownMenu(menubar, "file_pulldown", al, ac);
	save_pulldown = XmCreatePulldownMenu(file_pulldown, "save_pulldown",
					     al, ac);

	source_b = XmCreatePushButton(save_pulldown, "source_b", al, ac);
	text_b = XmCreatePushButton(save_pulldown, "text_b", al, ac);
	binary_b = XmCreatePushButton(save_pulldown, "binary_b", al, ac);
	children[ac++] = source_b;
	children[ac++] = text_b;
	children[ac++] = binary_b;
	XtManageChildren(children, ac);
	ac = 0;



	XtManageChild(source_b = XmCreateLabel(file_pulldown, "A Label", al, ac));
	new_b = XmCreatePushButton(file_pulldown, "new_b", al, ac);
	open_b = XmCreatePushButton(file_pulldown, "open_b", al, ac);
	XtSetArg(al[ac], XmNsubMenuId, save_pulldown); ac++;

	save_cascade = XmCreateCascadeButton(file_pulldown, "save_cascade",
					     al, ac);
	ac = 0;
	children[ac++] = new_b;
	children[ac++] = open_b;
	children[ac++] = save_cascade;
	XtManageChildren(children, ac);
	ac = 0;

	XtSetArg(al[ac], XmNsubMenuId, file_pulldown); ac++;
	file_cascade = XmCreateCascadeButton(menubar, "file_cascade", al, ac);
	ac = 0;
	XtManageChild(file_cascade);
	XtManageChild(menubar);

	XtAddCallback(source_b, XmNactivateCallback, doit, NULL);
	XtAddCallback(text_b, XmNactivateCallback, doit, NULL);
	XtAddCallback(binary_b, XmNactivateCallback, doit, NULL);
	XtAddCallback(new_b, XmNactivateCallback, doit, NULL);
	XtAddCallback(open_b, XmNactivateCallback, doit, NULL);
}
void 
create_appshell(display, app_name, app_argc, app_argv)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
{
	Widget          children[1];	/* Children to manage */
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Pulldown Menu"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	form = XmCreateForm(appshell, "form", al, ac);
	ac = 0;
	XtManageChild(form);
}

XtAppContext    app_context;
Display        *display;	/* Display             */

int 
main(argc, argv)
	int             argc;
	char          **argv;
{
	XtSetLanguageProc((XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL);
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
	display = XtOpenDisplay(app_context, NULL, argv[0], "XApplication",
				NULL, 0, &argc, argv);
	if (!display) {
		printf("%s: can't open display, exiting...\n", argv[0]);
		exit(-1);
	}
	create_appshell(display, argv[0], argc, argv);
	create_pulldown(form);
	XtRealizeWidget(appshell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	98,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	98,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	5,	5,	88,	21,	0,0,0,
};

  PrintDetails(appshell, Expected);
  }
    LessTifTestMainLoop(appshell);
    /*
	XtAppMainLoop(app_context);
	*/
	exit(0);
}
