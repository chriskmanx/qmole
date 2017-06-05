/* $Id: test5.c,v 1.4 2001/01/02 14:52:25 amai Exp $ */
/* amai: for me this seems to be a good candidate to
         demonstrate the problem of 
	 SF [Bug #114541 ] rounding error in Scale
	 
   Note that using the mouse is not a precise method here;
   I could not adjust the widget to all values I desired.
   Try the keyboard instead (left/right arrows)!
   At first we have to make sure that each slider position
   corresponds to an unique scale value ...
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/ScaleP.h>


void dragCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)callData;

    printf ("dragCallback: %d\n", cbs->value);
}

void valueChangedCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)callData;

    printf ("valueChangedCallback: %d\n", cbs->value);    
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  unsigned int maximum;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("sb", xmScaleWidgetClass, toplevel, 
                                XmNorientation, XmHORIZONTAL, 
                                XmNscaleWidth, 100,
				XmNshowValue, True,
				XmNwidth, 1000,
				NULL);

  XtAddCallback(one, XmNdragCallback, dragCallback, NULL);
  XtAddCallback(one, XmNvalueChangedCallback, valueChangedCallback, NULL);

  XtRealizeWidget(toplevel);

  XtVaGetValues(one, XmNmaximum, &maximum, NULL);

  printf ("one.maximum = %u\n", maximum);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  100,   35, 0,0,0, /* sb */
   CWWidth | CWHeight | CWX | CWY,    0,   16,  100,   19, 0,0,0, /* Scrollbar */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
