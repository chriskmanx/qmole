/* $Id: test1.c,v 1.3 2001/05/15 13:51:56 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Container.h>
#include <Xm/IconG.h>

#include "../../common/Test.h"

void
check_geometry(Widget w)
{
   static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
{  CWWidth | CWHeight            ,   10,   10,  386,   94, 0,0,0 }, /* widget */
{  CWWidth | CWHeight | CWX | CWY,    6,    6,  374,   50, 0,0,0 }, /* Text */
{  CWWidth | CWHeight | CWX | CWY,    6,   56,  374,   32, 0,0,0 }, /* ListSW */
{  CWWidth | CWHeight | CWX | CWY,  359,    0,   15,   32, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  359,   32, 0,0,0 }, /* List */
};

#if 0
   PrintDetails2(w, NULL);
#else
  if (result_index <= 0)
  {
     PrintDetails2(w, Expected);
     fflush(stdout);
     result_index ++;
  }
#endif
}

int
main(int argc, char **argv)
{
	Display		*dpy;
	Widget		toplevel, widget, bb, list;
	XtAppContext	app;
	XmString	item;
	Pixmap		p;
	Pixel		fg, bg;
	Arg		al[5];
	int		ac;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "Container", NULL, 0,
		&argc, argv, NULL, NULL);

	bb = XmCreateContainer(toplevel, "bb", NULL, 0);
/*	bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0); */
	XtManageChild(bb);

	dpy = XtDisplay(toplevel);

	fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(dpy));
	bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(dpy));

	p = XmGetPixmap(DefaultScreenOfDisplay(dpy),
		"woman",
		fg, bg);

	ac = 0;
	XtSetArg(al[ac], XmNsmallIconPixmap, p); ac++;
	widget = XmCreateIconGadget(bb, "widget", al, ac);

	XtManageChild(widget);

	XtRealizeWidget(toplevel);

/*	check_geometry(widget); */

	LessTifTestMainLoop(toplevel);

	exit(0);
}
