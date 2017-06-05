/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <stdlib.h>
#include <stdio.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>

Widget          appshell = (Widget) NULL;
Widget          form = (Widget) NULL;


void
create_option(form)
	Widget          form;
{
	Arg             al[4];
	int             ac = 0;
	Widget          option_pulldown;
	Widget          option_menu;
	XmString        tstring;
	Widget          circle_b, line_b, rect_b;
	Widget          children[3];
	XrmValue        from_value, to_value;

	/* Create and populate the PulldownMenu */
	option_pulldown = XmCreatePulldownMenu(form, "option_pulldown", al, ac);
	/* Create and manage the XmPushButtons for the menu items */
	ac = 0;
	XtSetArg(al[ac], XmNaccelerator, "Ctrl<Key>C"); ac++;
	tstring = XmStringCreateLocalized("Cntrl C");
	XtSetArg(al[ac], XmNacceleratorText, tstring); ac++;
	circle_b = XmCreatePushButton(option_pulldown, "circle_b", al, ac);
	ac = 0;
	XmStringFree(tstring);
	XtSetArg(al[ac], XmNaccelerator, "Ctrl<Key>L"); ac++;
	tstring = XmStringCreateLocalized("Cntrl L");
	XtSetArg(al[ac], XmNacceleratorText, tstring); ac++;
	line_b = XmCreatePushButton(option_pulldown, "line_b", al, ac);
	ac = 0;
	XmStringFree(tstring);
	XtSetArg(al[ac], XmNaccelerator, "Ctrl<Key>R"); ac++;
	tstring = XmStringCreateLocalized("Cntrl R");
	XtSetArg(al[ac], XmNacceleratorText, tstring); ac++;
	rect_b = XmCreatePushButton(option_pulldown, "rect_b", al, ac);
	ac = 0;
	XmStringFree(tstring);
	children[ac++] = circle_b;
	children[ac++] = line_b;
	children[ac++] = rect_b;
	XtManageChildren(children, ac);
	ac = 0;

	/* A quick example of string to keysym conversion for the mnemonic... */
	from_value.addr = (XtPointer) "S";
	from_value.size = strlen((char *) from_value.addr) + 1;
	to_value.addr = NULL;
	XtConvertAndStore(form, XmRString, &from_value, XmRKeySym,
			  &to_value);
	if (to_value.addr) {
		XtSetArg(al[ac], XmNmnemonic, *(unsigned int *) to_value.addr); ac++;
	} else {
	   fprintf(stderr, "XtConvertAndStore() failed\n");
	}
	/* Create the OptionMenu and associate the PulldownMenu with it */
	tstring = XmStringCreateLocalized("Shape");
	XtSetArg(al[ac], XmNlabelString, tstring); ac++;
	XtSetArg(al[ac], XmNsubMenuId, option_pulldown); ac++;
	XtSetArg(al[ac], XmNmenuHistory, line_b); ac++;
	option_menu = XmCreateOptionMenu(form, "option_menu", al, ac);
	ac = 0;
	XmStringFree(tstring);
	XtManageChild(option_menu);
}


void 
create_appshell(display, app_name, app_argc, app_argv)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
{
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Option Menu"); ac++;
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
	create_option(form);
	XtRealizeWidget(appshell);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	185,	35,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	185,	35,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	34,	29,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	40,	3,	142,	29,	0,0,0,
};

  PrintDetails(appshell, Expected);
  }
    LessTifTestMainLoop(appshell);
    /*
	XtAppMainLoop(app_context);
	*/
	exit(0);
}
