/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/XmP.h>
#include <Xm/LabelP.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/MainW.h>

#include <stdio.h>

Widget          main_menubar = (Widget) NULL;
Widget          about_b = (Widget) NULL;

void 
help_map_callback(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	/*
	 * The MenuPane is being mapped. Set the about button to be *
	 * sensitive if and only if it is being accessed from main_shell
	 */
	Boolean         got_about = (XmGetPostedFromWidget(w) == main_menubar);
	XtSetSensitive(about_b, got_about);

	printf("MR: %d\n", Lab_MarginRight(about_b));
}

XtAppContext    app_context;
Display        *display;	/* Display */

int 
main(argc, argv)
	int             argc;
	char          **argv;
{
	Arg al[64];
	int ac = 0;
	Widget main_shell;
	Widget main_win;
	Widget help_pulldown;
	XmString tstring;
	Widget help_b;
	Widget file_cascade;
	Widget edit_cascade;
	Widget main_help_cascade;
	Widget print_shell;
	Widget print_form;
	Widget print_menubar;
	Widget print_cascade;
	Widget print_help_cascade;
	Widget children[2];

	/* Create main window */

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
	XtSetArg(al[ac], XmNtitle, "Shared Pulldown Menu"); ac++;
	XtSetArg(al[ac], XmNargc, argc); ac++;
	XtSetArg(al[ac], XmNargv, argv); ac++;

	main_shell = XtAppCreateShell(argv[0], "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	main_win = XmCreateMainWindow(main_shell, "main_win", al, ac);

	/* Create MenuBar and PulldownMenu in main window */
	main_menubar = XmCreateMenuBar(main_win, "main_menubar", al, ac);
	help_pulldown = XmCreatePulldownMenu(main_menubar,
					     "help_pulldown", al, ac);
	XtSetArg(al[ac], XmNaccelerator, "Ctrl<Key>A"); ac++;
	tstring = XmStringCreateLocalized("Ctrl A");
	XtSetArg(al[ac], XmNacceleratorText, tstring);
	ac++;
	about_b = XmCreatePushButton(help_pulldown, "about_b", al, ac);
	ac = 0;
	XmStringFree(tstring);
	XtSetArg(al[ac], XmNaccelerator, "Ctrl<Key>H");
	ac++;
	tstring = XmStringCreateLocalized("Ctrl H");
	XtSetArg(al[ac], XmNacceleratorText, tstring);
	ac++;
	help_b = XmCreatePushButton(help_pulldown, "help_b", al, ac);
	ac = 0;
	XmStringFree(tstring);

	/* Set up map callback to control availability of about button */
	XtAddCallback(help_pulldown, XmNmapCallback,
		      help_map_callback, NULL);
	children[ac++] = about_b;
	children[ac++] = help_b;
	XtManageChildren(children, ac);
	ac = 0;
	file_cascade = XmCreateCascadeButton ( main_menubar, "file_cascade", al, ac );
	edit_cascade = XmCreateCascadeButton ( main_menubar, "edit_cascade", al, ac );

	/* Associate PulldownMenu with XmCascadeButton in main_menubar */
	XtSetArg(al[ac], XmNsubMenuId, help_pulldown);
	ac++;
	main_help_cascade = XmCreateCascadeButton(main_menubar,
					       "main_help_cascade", al, ac);
	XtManageChild(main_help_cascade);
	XtManageChild(main_menubar);
	XtManageChild(main_win);

	/* Create print dialog */
	ac = 0;
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	print_shell = XmCreateDialogShell ( main_shell, "print_shell", al, ac );
	ac = 0;
	print_form = XmCreateForm ( print_shell, "print_form", al, ac );
	print_menubar = XmCreateMenuBar ( print_form, "print_menubar", al, ac );
	print_cascade = XmCreateCascadeButton ( print_menubar, "print_cascade", al, ac );

	/* The print help cascade shares the help pulldown */
	XtSetArg(al[ac], XmNsubMenuId, help_pulldown);
	ac++;
	print_help_cascade = XmCreateCascadeButton(print_menubar,
					      "print_help_cascade", al, ac);
	ac = 0;
	XtManageChild(print_help_cascade);
	XtManageChild(print_cascade);
	XtManageChild( print_menubar );
	XtManageChild( print_form );

	XtRealizeWidget(main_shell);
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  426,  362,  128,   31, 0,0,0, /* main_win */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  128,   31, 0,0,0, /* main_menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,  118,   21, 0,0,0, /* main_help_cascade */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(main_shell, Expected);
}
LessTifTestMainLoop(main_shell);
	exit (0);
}
