#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/ScaleP.h>

void dragCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("dragCallback\n");
}

void valueChangedCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    printf ("valueChangedCallback\n");    
}

int
main(int argc, char **argv)
{
    Widget toplevel, one;
    XtAppContext app;
    unsigned int maximum;
    XmString str;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, NULL, NULL);

    str = XmStringCreateLtoR("Test Scale", XmFONTLIST_DEFAULT_TAG);
    one = XtVaCreateManagedWidget("sb", xmScaleWidgetClass, toplevel, 
				  XmNorientation, XmVERTICAL, 
				  XmNscaleHeight, 100,
				  XmNtitleString, str,
				  NULL);

    XtAddCallback(one, XmNdragCallback, dragCallback, NULL);
    XtAddCallback(one, XmNvalueChangedCallback, valueChangedCallback, NULL);

    XtRealizeWidget(toplevel);

    XtVaGetValues(one, XmNmaximum, &maximum, NULL);

    printf ("one.maximum = %u\n", maximum);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   87,  100, 0,0,0, /* sb */
   CWWidth | CWHeight | CWX | CWY,   19,    0,   64,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   19,  100, 0,0,0, /* Scrollbar */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

    exit(0);
}

