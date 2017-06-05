/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test20.c,v 1.3 2001/05/15 13:51:56 amai Exp $ */
/*
From:        Eric Howe <mu@trends.net>
To:          lesstif@lesstif.org
Subject:     Re: list sizing and mgv's page list
Date:        Sun, 22 Nov 1998 14:59:24 -0500 (EST)
*/

/*
 * Mini simulation of the mgv main window.  This program demonstrates
 * the (classic?) page list resizing bug.  Clicking in the list will
 * switch between a wide and thin set of entries; in Motif, the page
 * list will grow/shrink based on how wide the entries are.
 *
 * Also, this will demonstrate another sizing problem:  if you make the
 * window too short, the page list's vertical scrollbar isn't changing
 * it's thumb size.
 *
 *	--mu@trends.net, 1998.11.22
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/List.h>
#include <Xm/Form.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"


#define ITEMS 5
static XmString thin_items[ITEMS];
static XmString wide_items[ITEMS];
static Boolean using_thin;
Widget list;

static void
cleanup(Widget w, XtPointer closure, XtPointer call)
{
	int i;
	for(i = 0; i < ITEMS; ++i) {
		XmStringFree(thin_items[i]);
		XmStringFree(wide_items[i]);
	}
}

static void
dothings(Widget w, XtPointer closure, XtPointer call)
{
	XmString *items = using_thin ? wide_items : thin_items;
	XmListDeleteAllItems(list);
	XtVaSetValues(list, XmNitems, items, XmNitemCount, ITEMS, NULL);
	using_thin = !using_thin;

	/*
	 * Now I remember, I came up with this grusome hack to get the
	 * page list to resize, sigh.  See page.c:mgv_page_labels in
	 * the mgv source if you want to full version.
	 *
	 * In the mgv sources I have
	 *
	 *	XtVaSetValues(XtParent(w),
	 *		XmNrightAttachment, XmATTACH_FORM,
	 *		NULL);
	 *
	 * between these two calls.  I'm pretty certain that it is there
	 * for a reason but taking it out has no effect in Motif 2.0.1 so
	 * it may just be a historic artifact or a Motif 1.2 thing.
	 *
	 * BTW, does anyone know the "right" way to get this effect?  This
	 * seems to be a little bogus to me.
	 *
	 * NOTE:  even without this hack you can get the page list to
	 * change to the "right" size by resizing the main window (width
	 * or height).  This resize-the-window trick works with Motif
	 * but not LessTif (LessTif resizes the scrolled window).
	 */
	XtUnmanageChild(XtParent(list));
	XtManageChild(XtParent(list));
}

int
main(int argc, char **argv)
{
	XtAppContext ac;
	Widget top, sw, form;
	Arg a[19];
	int n;
	char buf[32];

	top = XtVaAppInitialize(&ac, "Blech", NULL, 0, &argc, argv, NULL, NULL);

	for(n = 0; n < ITEMS; ++n) {
		sprintf(buf, "item%d", n + 1);
		thin_items[n] = XmStringCreateLocalized(buf);
	}
	for(n = 0; n < ITEMS; ++n) {
		sprintf(buf, "ITEMITEM%d", n + 1);
		wide_items[n] = XmStringCreateLocalized(buf);
	}

	form = XtVaCreateWidget("form", xmFormWidgetClass, top, NULL);
	XtAddCallback(form, XmNdestroyCallback, cleanup, NULL);

	n = 0;
	XtSetArg(a[n], XmNrightAttachment, XmATTACH_FORM);	++n;
	XtSetArg(a[n], XmNtopAttachment, XmATTACH_FORM);	++n;
	XtSetArg(a[n], XmNbottomAttachment, XmATTACH_FORM);	++n;
	XtSetArg(a[n], XmNscrollBarDisplayPolicy, XmSTATIC);	++n;
	XtSetArg(a[n], XmNvisualPolicy, XmVARIABLE);		++n;
	XtSetArg(a[n], XmNresizable, True);			++n;
	XtSetArg(a[n], XmNselectionPolicy, XmBROWSE_SELECT);	++n;
	XtSetArg(a[n], XmNresizePolicy, XmRESIZE_IF_POSSIBLE);	++n;
	list = XmCreateScrolledList(form, "list", a, n);
	XtAddCallback(list, XmNbrowseSelectionCallback, dothings, NULL);
	XtManageChild(list);

	using_thin = True;
	XtVaSetValues(list, XmNitems, thin_items, XmNitemCount, ITEMS, NULL);

	n = 0;
	XtSetArg(a[n], XmNtopAttachment, XmATTACH_FORM);	++n;
	XtSetArg(a[n], XmNbottomAttachment, XmATTACH_FORM);	++n;
	XtSetArg(a[n], XmNleftAttachment, XmATTACH_FORM);	++n;
	XtSetArg(a[n], XmNscrollBarDisplayPolicy, XmSTATIC);	++n;
	XtSetArg(a[n], XmNrightAttachment, XmATTACH_WIDGET);	++n;
	XtSetArg(a[n], XmNrightWidget, XtParent(list));		++n;
	XtSetArg(a[n], XmNscrollingPolicy, XmAUTOMATIC);	++n;
	XtSetArg(a[n], XmNwidth, 100);	++n;
	XtSetArg(a[n], XmNheight, 100);	++n;
	sw = XmCreatePushButton(form, "sw", a, n);
	XtAddCallback(sw, XmNactivateCallback, dothings, NULL);
	XtManageChild(sw);

	XtManageChild(form);
	XtRealizeWidget(top);
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  669,  583,  159,  100, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,  100,    0,   59,  100, 0,0,0, /* listSW */
   CWWidth | CWHeight | CWX | CWY,   44,    0,   15,  100, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   40,  100, 0,0,0, /* list */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,  100, 0,0,0, /* sw */

   CWWidth | CWHeight            ,  669,  583,  159,  100, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,   76,    0,   83,  100, 0,0,0, /* listSW */
   CWWidth | CWHeight | CWX | CWY,   68,    0,   15,  100, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   64,  100, 0,0,0, /* list */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   76,  100, 0,0,0, /* sw */
};
/* toplevel should be replaced with to correct applicationShell */

	PrintDetails(top, Expected);
	LessTifTestWaitForIt(top);
	LessTifTestPushButton(sw);
	LessTifTestWaitForIt(top);
	PrintDetails(top, Expected);
}
	LessTifTestMainLoop(top);
	/*
	XtAppMainLoop(ac);
	*/
	return 0;
}
