#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/ScaleP.h>
#include <Xm/Form.h>

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
  Widget toplevel, one, two;
  XtAppContext app;
  int maximum;
  XmString str;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("form", xmFormWidgetClass, toplevel,
                                NULL);

  str = XmStringCreateLtoR("Test Scale", XmFONTLIST_DEFAULT_TAG);

  two = XtVaCreateManagedWidget("sb", xmScaleWidgetClass, one, 
                                XmNtopAttachment,  XmATTACH_FORM,
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNrightAttachment,  XmATTACH_FORM,
                                XmNbottomAttachment,  XmATTACH_FORM,
                                XmNorientation, XmHORIZONTAL, 
				XmNshowValue, True,
#if 0
                                XmNscaleWidth, 100,
#endif
				XmNtitleString, str,
				NULL);

  XtAddCallback(two, XmNdragCallback, dragCallback, NULL);
  XtAddCallback(two, XmNvalueChangedCallback, valueChangedCallback, NULL);

  XtRealizeWidget(toplevel);

  XtVaGetValues(two, XmNmaximum, &maximum, NULL);

  printf ("one.maximum = %i\n", maximum);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  104,   56, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  104,   56, 0,0,0, /* sb */
   CWWidth | CWHeight | CWX | CWY,    0,   35,   64,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    0,   16,  104,   19, 0,0,0, /* Scrollbar */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
