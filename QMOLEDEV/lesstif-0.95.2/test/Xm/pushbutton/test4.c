/* test for pushbutton callbacks */

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <stdio.h>

void
activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    XmPushButtonCallbackStruct *cbs = (XmPushButtonCallbackStruct *)call_data;
    printf ("Activated: click_count: %d\n", cbs->click_count);
}

void
arm_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf ("Armed\n");
}

void
disarm_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    printf ("Disarmed\n");
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XmString xmstr1 = XmStringCreateLtoR("Here Is A Label\nHere\nIs\nA\nLabel", XmFONTLIST_DEFAULT_TAG);

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel,
				XmNlabelString, xmstr1,
				XmNmultiClick, XmMULTICLICK_KEEP, NULL);

  XtAddCallback(one, XmNactivateCallback, activate_callback, NULL);
  XtAddCallback(one, XmNarmCallback, arm_callback, NULL);
  XtAddCallback(one, XmNdisarmCallback, disarm_callback, NULL);

  XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	102,	77,	0,0,0,	/* Form */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
