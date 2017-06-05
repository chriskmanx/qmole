/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test35.c,v 1.6 2001/05/15 14:46:10 amai Exp $ */

/* Simulate a problem in Mosaic 2.7b5 */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
 

Widget	a4, us;

#define	XmxSetToggleButton(w, s) \
	XmToggleButtonSetState (w, s, False)


void print_print_a4_cb(Widget w, XtPointer client, XtPointer call)
{
	XmToggleButtonCallbackStruct
		*p = (XmToggleButtonCallbackStruct *)call;

	fprintf(stderr, "print_print_a4_cb(%s)\n", p->set ? "True" : "False");
 
        XmxSetToggleButton(a4,
                           !XmToggleButtonGetState(a4));
        XmxSetToggleButton(us,
                           !XmToggleButtonGetState(us));
}

void print_print_us_cb(Widget w, XtPointer client, XtPointer call)
{
	XmToggleButtonCallbackStruct
		*p = (XmToggleButtonCallbackStruct *)call;

	fprintf(stderr, "print_print_us_cb(%s)\n", p->set ? "True" : "False");

        XmxSetToggleButton(a4,
                           !XmToggleButtonGetState(a4));
        XmxSetToggleButton(us,
                           !XmToggleButtonGetState(us));
}

/* The tuggle buttons callback */
void toggle_cb(Widget, XtPointer, XtPointer);

 
int
main(int argc, char *argv[]){
 
   XtAppContext app;
   Widget toplevel;
   Widget radio_box, w;
 
   toplevel = XtVaAppInitialize(&app, "bug", NULL, 0,
       &argc, argv, NULL,
       NULL);
 
 
   radio_box = XmCreateRadioBox(toplevel, "rbox", NULL, 0);
 
   /* create button 1 */
   a4 = XtVaCreateManagedWidget("a4",
      xmToggleButtonGadgetClass, radio_box,
      XmNset, False,
      NULL);
   XtAddCallback(a4,
      XmNvalueChangedCallback, print_print_a4_cb,
      (XtPointer)1);
 
 
   /* create button 2 */
   us = XtVaCreateManagedWidget("us",
      xmToggleButtonGadgetClass, radio_box,
      XmNset, True,
      NULL);
   XtAddCallback(us,
      XmNvalueChangedCallback, print_print_us_cb,
      (XtPointer)2);
 
   XtManageChild(radio_box);
   XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	43,	59,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	37,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	37,	25,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
   XtAppMainLoop(app);
   */
  exit(0); 
}
