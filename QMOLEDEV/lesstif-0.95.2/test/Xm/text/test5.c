/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test5.c,v 1.5 2001/04/27 09:50:54 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/ScrollBar.h>
#include <Xm/Text.h>


Widget   appshell, text_widget;
XtAppContext    app_context;
Display        *display;

int             infid;

void input(XtPointer cdata, int *source, XtInputId *id);


void 
timer(XtPointer cdata, XtIntervalId   *id)
{
	/* Reinstate the input callback */
	XtAppAddInput(app_context, infid, (XtPointer)XtInputReadMask, input, NULL);
}


void input(XtPointer cdata, int *source, XtInputId *id)
{
	char            inbuf[256];
	int             i;
	static int      textlength = 0;

	i = read(*source, inbuf, 255);
	if (i == 0) {
		/* At EOF - remove input callback and replace with timer */
		XtRemoveInput(*id);
		XtAppAddTimeOut(app_context, 250, timer, NULL);
	} else {
		/* Append input to text widget */
		inbuf[i] = '\0';
		XmTextInsert(text_widget, textlength, inbuf);
		textlength += i;
		XmTextShowPosition(text_widget, textlength);
	}
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
	Widget          appshellform = (Widget) NULL;
	Widget          scrtext = (Widget) NULL;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	appshellform = XmCreateForm(appshell, "appshellform", al, ac);
	ac = 0;
	XtSetArg(al[ac], XmNcolumns, 30); ac++;
	XtSetArg(al[ac], XmNeditMode, XmMULTI_LINE_EDIT); ac++;
	XtSetArg(al[ac], XmNrows, 15); ac++;
	text_widget = XmCreateScrolledText(appshellform, "text_widget", al, ac);
	ac = 0;
	scrtext = XtParent(text_widget);

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetValues(scrtext, al, ac);

	XtManageChild(text_widget);
	XtManageChild(appshellform);
}


int 
main(argc, argv)
	int             argc;
	char          **argv;
{
  char *fname;
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

	if (argc < 2) {
	  fname="./Makefile";  /* should be present in builddir ... */
	} else {
	  fname=argv[1];
	}
	  
	infid = open(fname, O_RDONLY);
	if (infid < 0) 
	{
	    fname="text/test5.c";
	    infid = open(fname, O_RDONLY);
	    if (infid < 0) 
	    {
		perror("open");
		exit(1);
	    }
	}
	XtAppAddInput(app_context, infid, (XtPointer)XtInputReadMask, input, NULL);

	XtRealizeWidget(appshell);

	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  217,  232, 0,0,0, /* appshellform */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  217,  232, 0,0,0, /* text_widgetSW */
   CWWidth | CWHeight | CWX | CWY,    0,  217,  198,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  202,    0,   15,  213, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  198,  213, 0,0,0, /* text_widget */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);
	exit(0);
}
