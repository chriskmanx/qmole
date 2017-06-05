/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test28.c,v 1.2 2004/08/21 08:37:40 dannybackx Exp $ */
/*
> There is an error in XmList...
> 
> Starting condition: You have a list with one item and it's selected.
> Call XmListAddItemUnselected() to add another item before it.
> Then XmListDeletePos() to delete the old item.
> 
> Problem: When adding the new item, it does not adjust the
> List_SelectedIndices, so the list still thinks item 1 (the first
> one) is highlighted when actually it's item 2.
> 
> Then when XmListDeletePos() is called, it fails to find the
> item being deleted (2) in SelectedIndices and so 
> List_SelectedItemCount() never gets decremented
> 
> Later a call to XmListGetSelectedPos thinks that the
> List_SelectedItemCount == 1, and allocates (but doesn't 
> clear) an array of that many.  It then ignores the 
> List_SelectedIndices (they're wrong anyway) and goes 
> through every item, looking for ones where selected is
> true.  It finds none because it was deleted.  As a result, the
> caller is told that there is one item selected and the index
> of the selected item is random garbage.
> 
>  -- Dave Williss
*/
#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmAll.h>

#include "../../common/Test.h"

Widget		toplevel, listw, form, b1, b2, b3;

void Doit1(Widget w, XtPointer client, XtPointer call)
{
	XmString	xms;

	xms = XmStringCreateSimple("new item");
	XmListAddItemUnselected(listw, xms, 1);
}

void Doit2(Widget w, XtPointer client, XtPointer call)
{
	int	sic = 1234;

	XmListDeletePos(listw, 2);

	XtVaGetValues(listw, XmNselectedItemCount, &sic, NULL);
	fprintf(stderr, "SelectedItemCount => %d\n", sic);
}

void Doit3(Widget w, XtPointer client, XtPointer call)
{
	int	nselected, i;
	int	*selected = NULL;

	(void)XmListGetSelectedPos(listw, &selected, &nselected);
	fprintf(stderr, "Selected Item Count => %d\n", nselected);
	fprintf(stderr, "Selected items : {");
	for (i=0; i<nselected; i++) {
		fprintf(stderr, "%d ", selected[i]);
	}
	fprintf(stderr, "}\n");
}

int main(int argc, char **argv)
{
	XtAppContext app;
	XmStringTable str_days;
	int i;

	toplevel = XtVaAppInitialize(&app, "BROWSE", NULL, 0, &argc, argv, NULL, NULL);
	form = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
		NULL);
	b1 = XtVaCreateManagedWidget("b1", xmPushButtonWidgetClass, form,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNtopAttachment,	XmATTACH_FORM,
		NULL);
	XtAddCallback(b1, XmNactivateCallback, Doit1, NULL);
		
	b2 = XtVaCreateManagedWidget("b2", xmPushButtonWidgetClass, form,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		b1,
		XmNleftOffset,		10,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNtopAttachment,	XmATTACH_FORM,
		NULL);
	XtAddCallback(b2, XmNactivateCallback, Doit2, NULL);

	b3 = XtVaCreateManagedWidget("b3", xmPushButtonWidgetClass, form,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		b2,
		XmNleftOffset,		10,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNtopAttachment,	XmATTACH_FORM,
		NULL);
	XtAddCallback(b3, XmNactivateCallback, Doit3, NULL);

	str_days = (XmStringTable) XtMalloc(7 * sizeof(XmString*));
	str_days[0] = XmStringCreateSimple("one");

	listw = XtVaCreateManagedWidget( "test_of_list", xmListWidgetClass, form,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		b1,
		XmNtopOffset,		5,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNselectionPolicy, XmBROWSE_SELECT,
		XmNitemCount, 1,
		XmNvisibleItemCount, 7,
		XmNitems, str_days,
		NULL );

	XtRealizeWidget(toplevel);

	/* Dave's sequence */
	XmListSelectPos(listw, 1, False);

	XtAppMainLoop(app);

	exit(0);
}
