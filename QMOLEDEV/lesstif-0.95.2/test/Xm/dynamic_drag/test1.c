/*
 * *LIBS: -lXm -lXt -lX11
 */

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DragDrop.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <stdio.h>

#include "bitmap.h"

#include <X11/bitmaps/Excl>
#include <X11/bitmaps/FlipHoriz>
#include <X11/bitmaps/FlipVert>
#include <X11/bitmaps/Left>
#include <X11/bitmaps/Right>
#include <X11/bitmaps/Up>
#include <X11/bitmaps/Down>
#include <X11/bitmaps/Fold>
#include <X11/bitmaps/Term>
#include <X11/bitmaps/woman>

Widget          appshell = (Widget) NULL;
extern void import_bitmap();
extern void do_expose();
extern void do_input();
extern void do_resize();
extern void drag_proc();

Bitmap_t bitmaps[] = {
	{ "Excl", Excl_bits, Excl_width, Excl_height, 0 },
	{ "FlipHoriz", FlipHoriz_bits, FlipHoriz_width, FlipHoriz_height, 0 },
	{ "FlipVert", FlipVert_bits, FlipVert_width, FlipVert_height, 0 },
	{ "Left", Left_bits, Left_width, Left_height },
	{ "Right", Right_bits, Right_width, Right_height },
	{ "Up", Up_bits, Up_width, Up_height },
	{ "Down", Down_bits, Down_width, Down_height },
	{ "Fold", Fold_bits, Fold_width, Fold_height, 0 },
	{ "Term", Term_bits, Term_width, Term_height, 0 },
	{ "woman", sorceress_bits, sorceress_width, sorceress_height, 0 },
	{ 0, 0, 0, 0, 0 }
};

void 
create_appshell(display, app_name, app_argc, app_argv)
	Display        *display;
	char           *app_name;
	int             app_argc;
	char          **app_argv;
{
	Widget          form = (Widget) NULL;
	Widget          da = (Widget) NULL;
	Widget          button = (Widget) NULL;
	Widget          children[2];	/* Children to manage */
	Arg             al[64];	/* Arg List */
	register int    ac = 0;	/* Arg Count */
	Atom            import_targets[2];	/* For Drop Site targets */
	XRectangle      rect[1];
	Widget          xmdisplay;

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "Drag and Drop"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell(app_name, "XApplication", applicationShellWidgetClass, display, al, ac);

	/* Set protocol style to get dynamic drag protocol */
	ac = 0;
	xmdisplay = XmGetXmDisplay(display);
	XtSetArg(al[ac], XmNdragInitiatorProtocolStyle, XmDRAG_PREFER_DYNAMIC); ac++;
	XtSetValues(xmdisplay, al, ac);

	ac = 0;
	XtSetArg(al[ac], XmNwidth, 300); ac++;
	XtSetArg(al[ac], XmNheight, 250); ac++;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	XtSetArg(al[ac], XmNresizePolicy, XmRESIZE_GROW); ac++;
	form = XmCreateForm(appshell, "form", al, ac);
	ac = 0;
	da = XmCreateDrawingArea(form, "da", al, ac);
	XtAddCallback(da, XmNexposeCallback, do_expose, NULL);
	XtAddCallback(da, XmNresizeCallback, do_resize, NULL);
	XtAddCallback(da, XmNinputCallback, do_input, NULL);

	/* Create XmPushButton and register as drop site */
	XtSetArg(al[ac], XmNwidth, 100); ac++;
	XtSetArg(al[ac], XmNheight, 100); ac++;
	XtSetArg(al[ac], XmNlabelType, XmPIXMAP); ac++;
	button = XmCreatePushButton(form, "button", al, ac);
	ac = 0;

	import_targets[0] = XA_STRING;
	import_targets[1] = XA_PIXMAP;
	XtSetArg(al[ac], XmNimportTargets, import_targets); ac++;
	XtSetArg(al[ac], XmNnumImportTargets, 2); ac++;
	XtSetArg(al[ac], XmNdropProc, import_bitmap); ac++;
	XtSetArg(al[ac], XmNdropSiteOperations, XmDROP_COPY); ac++;

	/* Register drop site rectangle */
	rect[0].x = 10;
	rect[0].y = 10;
	rect[0].width = 30;
	rect[0].height = 30;
	XtSetArg(al[ac], XmNdropRectangles, rect); ac++;
	XtSetArg(al[ac], XmNnumDropRectangles, 1); ac++;
	XtSetArg(al[ac], XmNdragProc, drag_proc); ac++;
	XtSetArg(al[ac], XmNanimationStyle, XmDRAG_UNDER_SHADOW_OUT); ac++;
	XmDropSiteRegister (button, al, ac);

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNbottomWidget, button); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetValues(da, al, ac);
	ac = 0;

	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE); ac++;
	XtSetValues(button, al, ac);
	ac = 0;
	children[ac++] = da;
	children[ac++] = button;
	XtManageChildren(children, ac);
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
	XtRealizeWidget(appshell);
	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  300,  250, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  300,  150, 0,0,0, /* da */
   CWWidth | CWHeight | CWX | CWY,    0,  150,  100,  100, 0,0,0, /* button */ 
    };
    PrintDetails(appshell,Expected);
};
	LessTifTestMainLoop(appshell);
	exit(0);
}
