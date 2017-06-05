/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test21.c,v 1.5 2002/05/01 15:54:45 amai Exp $
   Test for XmListAddItem */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

#include "../../common/Test.h"


Widget toplevel, listw, formw, button1;

int current_item = 0;

void button_cb1(Widget w, XtPointer clientData, XtPointer callData)
{
    char string[100];
    XmString xmstr;

    if (XtIsManaged(listw))
    {
	XtUnmanageChild(listw);
    }
    else
    {
	XtManageChild(listw);
    }
    sprintf (string, "XmScrolledList Item %d", current_item);

    xmstr = XmStringCreateSimple(string);

    XmListAddItemUnselected(listw, xmstr, 0);

    XmStringFree(xmstr);

    current_item ++;
}


char *fallbacks[] =
{
    "*listTest.allowShellResize: True",
    "*list.listSizePolicy: XmCONSTANT",
    "*list.visibleItemCount: 5",
    "*list.scrollBarDisplayPolicy: XmSTATIC",
    "*list.selectionPolicy: XmBROWSE_SELECT",
    "*button.labelString: Manage List",
    NULL
};

int
main(int argc, char **argv)
{
    XtAppContext app;
    Arg arg[1];
    
    toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
				 &argc, argv, fallbacks, NULL);

    formw = XmCreateForm(toplevel, "form", NULL, 0);
    button1 = XmCreatePushButton(formw, "button", NULL, 0);

    listw = XmCreateScrolledList(formw, "list", arg, 0); 

    XtVaSetValues(XtParent(listw),
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_WIDGET,
                  XmNbottomWidget, button1,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);

    XtVaSetValues(button1,
		  XmNtopAttachment, XmATTACH_NONE,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);

    XtManageChild(formw);
    XtManageChild(button1);

    XtRealizeWidget(toplevel);

    XtAddCallback(button1, XmNactivateCallback, button_cb1, NULL);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  100,  125, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,  100,  100,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,  100, 0,0,0, /* listSW */
   CWWidth | CWHeight | CWX | CWY,  100,    0,   15,  104, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  100,  104,   15, 0,0,0, /* HorScrollBar */

   CWWidth | CWHeight            ,  415,  388,   78,  131, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,  106,   78,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   78,  106, 0,0,0, /* listSW */
   CWWidth | CWHeight | CWX | CWY,   63,    0,   15,   87, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,   91,   59,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   59,   87, 0,0,0, /* list */

   CWWidth | CWHeight            ,  415,  388,   78,  131, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,  106,   78,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   78,  106, 0,0,0, /* listSW */
   CWWidth | CWHeight | CWX | CWY,   78,    0,   15,   87, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  106,   59,   15, 0,0,0, /* HorScrollBar */
};

PrintDetails(toplevel, Expected);
LessTifTestWaitForIt(toplevel);
LessTifTestPushButton(button1);
PrintDetails(toplevel, Expected);
LessTifTestWaitForIt(toplevel);
LessTifTestPushButton(button1);
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    exit(0);
}
