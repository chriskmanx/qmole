/* test of selection boxes */

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/SelectioB.h>
#include <Xm/PushBP.h>

Widget toplevel, box, push;

void Push(Widget w, XtPointer client, XtPointer call)
{
	Widget	dialog = (Widget)client;

	XtManageChild(dialog);
}

void Print(Widget w, XtPointer client, XtPointer call)
{
    XmSelectionBoxCallbackStruct	*cbp = (XmSelectionBoxCallbackStruct *)call;
    char				*s;
    XmStringTable			li = NULL;
    XmString				xms;

    if (XmStringGetLtoR(cbp->value, XmFONTLIST_DEFAULT_TAG, &s)) {
	fprintf(stderr, "Selection : %s\n", s);
	XtFree(s);
    } else
	fprintf(stderr, "Could not convert\n");

    XtVaGetValues(box,
		XmNlistItems, &li,
		XmNtextString, &xms,
	NULL);
}

int
main(int argc, char **argv)
{
	XtAppContext app;
	XmString	items[3];

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
			       &argc, argv, NULL, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
			XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

	box = XmCreateSelectionDialog(toplevel, "Box", NULL, 0);

	items[0] = XmStringCreateSimple("Yow");
	items[1] = XmStringCreateSimple("This is longer");
	items[2] = XmStringCreateSimple("End of it");

	XtVaSetValues(box,
			XmNlistItems,		items,
			XmNlistItemCount,	3,
		NULL);

	XtAddCallback(box, XmNokCallback, Print, NULL);
	XtAddCallback(push, XmNactivateCallback, Push, box);

	XtManageChild(push);

	XtRealizeWidget(toplevel);


  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   66,   25, 0,0,0, /* push */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

	exit(0);
}
/* test of selection boxes */

