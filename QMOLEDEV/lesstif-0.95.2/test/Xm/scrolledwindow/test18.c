/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test18.c,v 1.3 2001/05/23 14:38:38 amai Exp $
 * create a scrolled window with a drawing area inside... resize the
 * drawing area when the pushbutton is clicked (also draw a line
 * to show the clipping area)
 */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>

#include "../../common/Test.h"

Widget toplevel;
Widget form;
Widget scrollw;
Widget execute_button;
Widget da;

unsigned long valuemask;
XGCValues values;
GC gc;

static char *FallBack[] = {
		"*mycanvas.background: red",
		"*.geometrySlop: 0",
		NULL
};

void
cb (
    Widget w,
    XtPointer d,
    XtPointer c)
{
	if (XtIsManaged(da))
	{
	XtUnmanageChild(da);
	}
	else
	{
	XtManageChild(da);
	}
}


int main (
    int argc,
    char **argv)
{
    Arg al[64];
    int ac;	
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "test11", NULL, 0,
			    &argc, argv, FallBack, NULL);

    form = XmCreateForm( toplevel, "myform", NULL, 0);
    XtManageChild(form);

    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);  ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
    execute_button = XmCreatePushButton ( form, "execute_button", al, ac );
    XtAddCallback(execute_button, XmNactivateCallback, cb, NULL);
    XtManageChild(execute_button);

    ac = 0;
    XtSetArg(al[ac], XmNwidth, 500);  ac++;
    XtSetArg(al[ac], XmNheight, 600);  ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET);  ac++;
    XtSetArg(al[ac], XmNtopWidget, execute_button);  ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);  ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);  ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);  ac++;
    XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC);  ac++;
    scrollw = XmCreateScrolledWindow ( form, "myscroll", al, ac );
    XtManageChild(scrollw);

    da = XtVaCreateManagedWidget("mycanvas", xmLabelWidgetClass, 
				 scrollw, 	
				 XmNwidth, 100,
				 XmNheight, 100,
				 NULL);
    XtRealizeWidget(toplevel);

{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  391,  500,  625, 0,0,0, /* myform */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  500,   25, 0,0,0, /* execute_button */
   CWWidth | CWHeight | CWX | CWY,    0,   25,  500,  600, 0,0,0, /* myscroll */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  496,  596, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,  100, 0,0,0, /* mycanvas */
   CWWidth | CWHeight | CWX | CWY,  500,    0,   19,  600, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  600,  500,   19, 0,0,0, /* HorScrollBar */ 

   CWWidth | CWHeight            ,  508,  391,  500,  625, 0,0,0, /* myform */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  500,   25, 0,0,0, /* execute_button */
   CWWidth | CWHeight | CWX | CWY,    0,   25,  500,  600, 0,0,0, /* myscroll */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  496,  596, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,  100, 0,0,0, /* mycanvas */
   CWWidth | CWHeight | CWX | CWY,  500,    0,   19,  600, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  600,  500,   19, 0,0,0, /* HorScrollBar */ 

   CWWidth | CWHeight            ,  508,  391,  500,  625, 0,0,0, /* myform */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  500,   25, 0,0,0, /* execute_button */
   CWWidth | CWHeight | CWX | CWY,    0,   25,  500,  600, 0,0,0, /* myscroll */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  496,  596, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,  100, 0,0,0, /* mycanvas */
   CWWidth | CWHeight | CWX | CWY,  500,    0,   19,  600, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  600,  500,   19, 0,0,0, /* HorScrollBar */ 
    };
    PrintDetails(toplevel,Expected);
  LessTifTestWaitForIt(toplevel);

  LessTifTestPushButton(execute_button);
  LessTifTestWaitForIt(toplevel);
  PrintDetails(toplevel,Expected);

  LessTifTestPushButton(execute_button);
  LessTifTestWaitForIt(toplevel);
  PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
