/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/container/test4.c,v 1.2 2001/12/19 20:38:41 dannybackx Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
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

static void doit(Widget w, XtPointer client, XtPointer call)
{
	XmContainerSelectCallbackStruct	*cbp = (XmContainerSelectCallbackStruct *)call;
	char				*txt = (char *)client;
	int				i;

	if (cbp->selected_item_count == 0) {
		fprintf(stderr, "Callback(%s) - no selection\n", txt);
	} else {
		fprintf(stderr, "Callback(%s) - %d items selected :\n", txt, cbp->selected_item_count);
		for (i=0; i<cbp->selected_item_count; i++) {
			fprintf(stderr, "\t%s\n", XtName(cbp->selected_items[i]));
		}
	}	
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
	XtSetArg(al[ac], XmNlayoutType, XmOUTLINE); ac++;
	c = XmCreateContainer(toplevel, "container", al, ac);
	XtManageChild(c);

	XtAddCallback(c, XmNconvertCallback, doit, XmNconvertCallback);
	XtAddCallback(c, XmNdefaultActionCallback, doit, XmNdefaultActionCallback);
	XtAddCallback(c, XmNdestinationCallback, doit, XmNdestinationCallback);
	XtAddCallback(c, XmNoutlineChangedCallback, doit, XmNoutlineChangedCallback);
	XtAddCallback(c, XmNselectionCallback, doit, XmNselectionCallback);

	dpy = XtDisplay(toplevel);

	fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(dpy));
	bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(dpy));

	p = XmGetPixmap(DefaultScreenOfDisplay(dpy),
		"woman",
		fg, bg);

	ac = 0;
	XtSetArg(al[ac], XmNsmallIconPixmap, p); ac++;
	XtSetArg(al[ac], XmNoutlineState, XmEXPANDED); ac++;
	parent = XmCreateIconGadget(c, "tree-1", al, ac);
	XtManageChild(parent);

	ac = 0;
	XtSetArg(al[ac], XmNentryParent, parent); ac++;
	widget = XmCreateIconGadget(c, "child-1", al, ac);
	XtManageChild(widget);

	ac = 0;
	XtSetArg(al[ac], XmNentryParent, parent); ac++;
	XtSetArg(al[ac], XmNoutlineState, XmEXPANDED); ac++;
	widget = XmCreateIconGadget(c, "child-2", al, ac);
	XtManageChild(widget);

	/* An additional level */
	parent = widget;
	ac = 0;
	XtSetArg(al[ac], XmNentryParent, parent); ac++;
	widget = XmCreateIconGadget(c, "child-3", al, ac);
	XtManageChild(widget);

	ac = 0;
	XtSetArg(al[ac], XmNentryParent, parent); ac++;
	widget = XmCreateIconGadget(c, "child-4", al, ac);
	XtManageChild(widget);

	/* Second tree */
	ac = 0;
	XtSetArg(al[ac], XmNsmallIconPixmap, p); ac++;
	XtSetArg(al[ac], XmNoutlineState, XmEXPANDED); ac++;
	parent = XmCreateIconGadget(c, "tree-2", al, ac);
	XtManageChild(parent);

	ac = 0;
	XtSetArg(al[ac], XmNentryParent, parent); ac++;
	widget = XmCreateIconGadget(c, "child-5", al, ac);
	XtManageChild(widget);

	XtRealizeWidget(toplevel);
#if 0
	check_geometry(toplevel);
#endif
	LessTifTestMainLoop(toplevel);

	exit(0);
}
