/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scale/test19.c,v 1.2 2001/10/15 19:50:53 amai Exp $
  
 See SF Patch [ #470440 ] Xm/scale/test19.c description misleading
     SF Patch [ #463347 ] Set values for XmNscaleMultiple
 
 With LessTif 0.92.32 compiled from source on a Red Hat 6.2 system (x86), 
 changing an XmScale widget's XmNscaleMultiple resource did
 not change the widget's behavior.  The attached program demonstrates this.
 
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Scale.h>
#include <X11/keysym.h>

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
main(int argc, char *argv[])
{
  Widget toplevel, one;
  XtAppContext app;
  int value;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("sb", xmScaleWidgetClass, toplevel, 
                                XmNorientation, XmHORIZONTAL, 
                                XmNscaleWidth, 100, NULL);

  XtAddCallback(one, XmNdragCallback, dragCallback, NULL);
  XtAddCallback(one, XmNvalueChangedCallback, valueChangedCallback, NULL);

  XtRealizeWidget(toplevel);
  XtVaSetValues(one, XmNscaleMultiple, 2, NULL);
  LessTifTestWaitForIt(toplevel);

  LessTifTestKeyPress(one, XK_Right, ControlMask);
  XtVaGetValues(one, XmNvalue, &value, NULL);

  exit((value == 2) ? 0 : 1);
}
