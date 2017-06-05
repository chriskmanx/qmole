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

static char *Fallback[] = {
	"*XmScale.background: red",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  unsigned int maximum;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, Fallback, 
  	XmNwidth, 100,
  	NULL);

  one = XtVaCreateManagedWidget("sb", xmScaleWidgetClass, toplevel, 
                                XmNorientation, XmVERTICAL, 
                                XmNscaleHeight, 100, 
                                XmNwidth, 100,
                                NULL);

  XtAddCallback(one, XmNdragCallback, dragCallback, NULL);
  XtAddCallback(one, XmNvalueChangedCallback, valueChangedCallback, NULL);

  XtRealizeWidget(toplevel);

  XtVaGetValues(one, XmNmaximum, &maximum, NULL);

  printf ("one.maximum = %u\n", maximum);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  100,  100, 0,0,0, /* sb */
   CWWidth | CWHeight | CWX | CWY,   81,    0,   19,  100, 0,0,0, /* Scrollbar */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
