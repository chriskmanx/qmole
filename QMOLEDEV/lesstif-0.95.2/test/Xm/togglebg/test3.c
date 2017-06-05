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
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/Label.h>
#include <Xm/ToggleBG.h>

Widget          appshell = (Widget) NULL;
Widget          form = (Widget) NULL;


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
	ac = 0;

	XtManageChild(source_b = XmCreateLabel(file_pulldown, "A Long Long Long Label", al, ac));
	new_b = XmCreateToggleButtonGadget(file_pulldown, "new_b", al, ac);
	XtVaSetValues(new_b,
		XmNvisibleWhenOff, True,
		XmNacceleratorText, XmStringCreateSimple("AccText"),
		NULL);
	open_b = XmCreateToggleButtonGadget(file_pulldown, "open_b", al, ac);
	XtVaSetValues(open_b,
		XmNvisibleWhenOff, True,
		XmNacceleratorText, XmStringCreateSimple("AccText"),
		NULL);
	save_cascade = XmCreatePushButtonGadget(file_pulldown, "push_b", al, ac);
	XtVaSetValues(save_cascade,
		XmNacceleratorText, XmStringCreateSimple("AccText"),
		NULL);
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
