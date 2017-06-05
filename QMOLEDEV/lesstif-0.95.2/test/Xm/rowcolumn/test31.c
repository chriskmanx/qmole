/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test31.c,v 1.6 2001/05/15 14:46:10 amai Exp $
 
The following code shows a bug in the Radio Button code.
The bug is that toggle buttons in a radio box get incorrect 
callbacks.
 
The following program demonstrates the problem.
 
---------------------+-------------------+-----------------
Action               | Correct Behavior  | lesstif 971005
---------------------+-------------------+-----------------
Start Program.       |                   |
---------------------+-------------------+-----------------
User Presses Button2 | "Button 1 clear"  | "Button 2 set"
                     | "Button 2 set"    |
---------------------+-------------------+-----------------
User presses Button2 | "Button 2 set"    | "Button 2 clear"
   again.            | "Button 2 set"    |
---------------------+-------------------+-----------------
 
Note that the radio button is painted correctly (it looks
pressed in or pressed out when it is supposed to be.)  It
is only the callbacks that get called incorrectly.
 
 
Keep up the great work with lesstif!
 
-Paul
 
paul.wilkins@analog.com
 
*/
 
#include <stdlib.h> 
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
 
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
   w = XtVaCreateManagedWidget("button1",
      xmToggleButtonGadgetClass, radio_box,
      XmNset, True,
      NULL);
   XtAddCallback(w,
      XmNvalueChangedCallback, toggle_cb,
      (XtPointer)1);
 
 
   /* create button 2 */
   w = XtVaCreateManagedWidget("button2",
      xmToggleButtonGadgetClass, radio_box,
      XmNset, False,
      NULL);
   XtAddCallback(w,
      XmNvalueChangedCallback, toggle_cb,
      (XtPointer)2);
 
   XtManageChild(radio_box);
   XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	73,	59,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	67,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	31,	67,	25,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
   XtAppMainLoop(app);
   */
  exit(0); 
}
 
/*
 * Show not only the parameter to the callback function but also
 * the current widget state.
 */
void toggle_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
   XmToggleButtonCallbackStruct *state =
      (XmToggleButtonCallbackStruct *) call_data;
   Boolean	x = XmToggleButtonGetState(w);
 
   fprintf(stderr, "Widget %s\t", x ? "set" : "clear");
   fprintf(stderr, "Button %d ", (int)client_data);
 
   if(state->set){
      fprintf(stderr, "set\n");
   } else {
      fprintf(stderr, "clear\n");
   }
}
