#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/ScrollBar.h>

void dragCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)callData;

    printf ("dragCallback: %d\n", cbs->value);
}

void decrementCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("decrementCallback\n");
}

void incrementCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("incrementCallback\n");    
}
void pageDecrementCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("pageDecrementCallback\n");    
}

void pageIncrementCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("pageIncrementCallback\n");    
}

void valueChangedCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)callData;

    printf ("valueChangedCallback: %d\n", cbs->value);    
}

int
main(int argc, char **argv)
{
    Widget toplevel, one;
    XtAppContext app;
    unsigned int maximum;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "Scroll", NULL, 0, &argc, argv, NULL, NULL);

    one = XtVaCreateManagedWidget("sb", xmScrollBarWidgetClass, toplevel, 
				  XmNorientation, XmHORIZONTAL,
				  XmNshowArrows, False,
				  XmNprocessingDirection, XmMAX_ON_LEFT,
				  XmNminimum, 0, XmNmaximum, 1000000000,
				  XmNwidth, 100,
				  XmNsliderSize, 300000000,
				  XmNvalue, 540000000,
				  NULL);

    XtAddCallback(one, XmNdecrementCallback, decrementCallback, NULL);
    XtAddCallback(one, XmNdragCallback, dragCallback, NULL);
    XtAddCallback(one, XmNincrementCallback, incrementCallback, NULL);
    XtAddCallback(one, XmNpageDecrementCallback, pageDecrementCallback, NULL);
    XtAddCallback(one, XmNpageIncrementCallback, pageIncrementCallback, NULL);
    XtAddCallback(one, XmNvalueChangedCallback, valueChangedCallback, NULL);

    XtRealizeWidget(toplevel);

    XtVaGetValues(one, XmNmaximum, &maximum, NULL);

    printf ("one.maximum = %u\n", maximum);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  100,   15, 0,0,0, /* sb */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

    exit(0);
}
