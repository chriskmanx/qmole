/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/container/test3.c,v 1.2 2002/05/03 12:03:42 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/Container.h>
#include <Xm/IconG.h>

#include "../../common/Test.h"

#ifndef	LESSTIF_VERSION
#include <X11/Xmu/Editres.h>
#endif

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
	Widget		toplevel, c, widget, parent;
	XtAppContext	app;
	Pixmap		p;
	Pixel		fg, bg;
	Arg		al[5];
	int		ac;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "Container", NULL, 0,
		&argc, argv, NULL, NULL);

#ifndef	LESSTIF_VERSION
	XtAddEventHandler(toplevel, (EventMask)0, True,
		(XtEventHandler)_XEditResCheckMessages, NULL);
#endif
	ac = 0;
	XtSetArg(al[ac], XmNwidth, 200); ac++;
	XtSetArg(al[ac], XmNheight, 200); ac++;
	c = XmCreateBulletinBoard(toplevel, "bb", al, ac);
	XtManageChild(c);

	ac = 0;
	XtSetArg(al[ac], XmNx, 30); ac++;
	XtSetArg(al[ac], XmNy, 30); ac++;
	parent = XmCreateIconGadget(c, "tree-1", al, ac);
	XtManageChild(parent);

	ac = 0;
	XtSetArg(al[ac], XmNx, 30); ac++;
	XtSetArg(al[ac], XmNy, 70); ac++;
	parent = XmCreateIconGadget(c, "child-3", al, ac);
	XtManageChild(parent);

	XtRealizeWidget(toplevel);
#if 0
	check_geometry(toplevel);
#endif
	LessTifTestMainLoop(toplevel);

	exit(0);
}
