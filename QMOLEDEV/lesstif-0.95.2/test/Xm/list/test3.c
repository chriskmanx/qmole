/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test3.c,v 1.11 2002/05/01 15:54:45 amai Exp $
 * More versatile test.
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include <Xm/Xm.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>

#include "../../common/Test.h"

char *days[] = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
		     "Friday", "Saturday" };

Widget	list;

void ShowSelect(Widget w, XtPointer client, XtPointer call)
{
	XmListCallbackStruct	*cbp = (XmListCallbackStruct *)call;
	char			*s = (char *)client;
	int			i;

	switch (cbp->reason)
	{
	case XmCR_SINGLE_SELECT:
		fprintf(stderr, "ShowSelect(%s): #%d", s, 1);
		fprintf(stderr, " %d", cbp->item_position);
		fprintf(stderr, "\n");
		break;
	case XmCR_MULTIPLE_SELECT:
	case XmCR_EXTENDED_SELECT:
		fprintf(stderr, "ShowSelect(%s): #%d", s, cbp->selected_item_count);
		for (i=0; i<cbp->selected_item_count; i++) 
			fprintf(stderr, " %d", cbp->selected_item_positions[i]);
		fprintf(stderr, "\n");
		break;
	}
}

void change_list(Widget w, XtPointer client, XtPointer call)
{
	XtRemoveAllCallbacks(list, XmNsingleSelectionCallback);
	XtRemoveAllCallbacks(list, XmNmultipleSelectionCallback);
	XtRemoveAllCallbacks(list, XmNbrowseSelectionCallback);
	XtRemoveAllCallbacks(list, XmNextendedSelectionCallback);

	XtVaSetValues(list, XmNselectionPolicy, client, NULL);

	XtAddCallback(list, XmNsingleSelectionCallback, ShowSelect, (XtPointer)"XmSINGLE_SELECT");
	XtAddCallback(list, XmNmultipleSelectionCallback, ShowSelect, (XtPointer)"XmMULTIPLE_SELECT");
	XtAddCallback(list, XmNbrowseSelectionCallback, ShowSelect, (XtPointer)"XmBROWSE_SELECT");
	XtAddCallback(list, XmNextendedSelectionCallback, ShowSelect, (XtPointer)"XmEXTENDED_SELECT");
}

Widget	tf;

void position(Widget w, XtPointer client, XtPointer call)
{
	char	*s;
	int	n;

	XtVaGetValues(tf, XmNvalue, &s, NULL);
	n = atoi(s);
	/*
	XtFree(s);
	*/

	XmListSelectPos(list, n, False);
}

void deselect(Widget w, XtPointer client, XtPointer call)
{
	XmListDeselectPos(list, -1);
}

void replace(Widget w, XtPointer client, XtPointer call)
{
	XmString	l[1];

	l[0] = XmStringCreateSimple("Yow");

	XmListReplaceItemsPos(list, l, 1, 3);
}

void talk(Widget w, XtPointer client, XtPointer call)
{
	char	*s = (char *)client;

	fprintf(stderr, "Callback %s\n", s);
}

int
main(int argc, char **argv)
{
	XtAppContext	app;
	Widget		top, bb, w, pd, om, msb, pb;
	XmStringTable	str_days;
	int		i;
	Arg		al[10];
	int		ac;

	top = XtVaAppInitialize(&app, "MULTIPLE", NULL, 0,
		&argc, argv, NULL, NULL);

	bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, top,
		NULL);

	/* option menu */
	pd = XmCreatePulldownMenu(bb, "pulldown", NULL, 0);
	w = XtVaCreateManagedWidget("XmSINGLE_SELECT",
		xmPushButtonWidgetClass, pd, NULL);
	XtAddCallback(w, XmNactivateCallback, change_list, (XtPointer)XmSINGLE_SELECT);

	w = XtVaCreateManagedWidget("XmBROWSE_SELECT",
		xmPushButtonWidgetClass, pd, NULL);
	XtAddCallback(w, XmNactivateCallback, change_list, (XtPointer)XmBROWSE_SELECT);

	msb = XtVaCreateManagedWidget("XmMULTIPLE_SELECT",
		xmPushButtonWidgetClass, pd, NULL);
	XtAddCallback(w, XmNactivateCallback, change_list, (XtPointer)XmMULTIPLE_SELECT);

	w = XtVaCreateManagedWidget("XmEXTENDED_SELECT",
		xmPushButtonWidgetClass, pd, NULL);
	XtAddCallback(w, XmNactivateCallback, change_list, (XtPointer)XmEXTENDED_SELECT);

	ac = 0;
	XtSetArg(al[ac], XmNsubMenuId, pd); ac++;
	XtSetArg(al[ac], XmNmenuHistory, msb); ac++;
	XtSetArg(al[ac], XmNx, 200); ac++;
	XtSetArg(al[ac], XmNy, 10); ac++;
	om = XmCreateOptionMenu(bb, "option", al, ac);
	XtManageChild(om);

	/* textfield */
	tf = XtVaCreateManagedWidget("textfield", xmTextFieldWidgetClass, bb,
			XmNx,	200,
			XmNy,	100,
		NULL);
	XtAddCallback(tf, XmNactivateCallback, position, NULL);
	XtAddCallback(tf, XmNfocusCallback, talk, XmNfocusCallback);
	XtAddCallback(tf, XmNlosingFocusCallback, talk, XmNlosingFocusCallback);

	pb = XtVaCreateManagedWidget("deselect", xmPushButtonWidgetClass, bb,
			XmNx,	200,
			XmNy,	150,
		NULL);
	XtAddCallback(pb, XmNactivateCallback, deselect, NULL);

	pb = XtVaCreateManagedWidget("replace", xmPushButtonWidgetClass, bb,
			XmNx,	120,
			XmNy,	150,
		NULL);
	XtAddCallback(pb, XmNactivateCallback, replace, NULL);

	/* list */
	str_days = (XmStringTable) XtMalloc(7 * sizeof(XmString*));
	for(i=0; i<7; ++i)
		str_days[i] = XmStringCreateSimple(days[i]);

	list = XtVaCreateManagedWidget( "test_of_list",
			xmListWidgetClass,	bb,
			XmNselectionPolicy,	XmMULTIPLE_SELECT,
			XmNitemCount,		7,
			XmNvisibleItemCount,	7,
			XmNitems,		str_days,
		NULL );
	XtAddCallback(list, XmNsingleSelectionCallback, ShowSelect, (XtPointer)"XmSINGLE_SELECT");
	XtAddCallback(list, XmNmultipleSelectionCallback, ShowSelect, (XtPointer)"XmMULTIPLE_SELECT");
	XtAddCallback(list, XmNbrowseSelectionCallback, ShowSelect, (XtPointer)"XmBROWSE_SELECT");
	XtAddCallback(list, XmNextendedSelectionCallback, ShowSelect, (XtPointer)"XmEXTENDED_SELECT");

	for(i=0; i<7; ++i)
		XmStringFree(str_days[i]);
	XtFree((XtPointer)str_days);

	XtRealizeWidget(top);


{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  363,  186, 0,0,0, /* bb */
   CWWidth | CWHeight | CWX | CWY,  200,   10,  152,   35, 0,0,0, /* option */
   CWWidth | CWHeight | CWX | CWY,    3,    3,    4,   29, 0,0,0, /* OptionLabel */
   CWWidth | CWHeight | CWX | CWY,   10,    3,  139,   29, 0,0,0, /* OptionButton */
   CWWidth | CWHeight | CWX | CWY,  200,  100,  138,   31, 0,0,0, /* textfield */
   CWWidth | CWHeight | CWX | CWY,  200,  150,   60,   25, 0,0,0, /* deselect */
   CWWidth | CWHeight | CWX | CWY,  120,  150,   54,   25, 0,0,0, /* replace */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   64,  119, 0,0,0, /* test_of_list */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(top, Expected);
}
  LessTifTestMainLoop(top);

	exit(0);
}
