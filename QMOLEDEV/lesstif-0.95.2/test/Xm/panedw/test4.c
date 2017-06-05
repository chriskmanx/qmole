#include <Xm/Xm.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 

Widget button1, button;
Widget toplevel, field, pane;

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextFieldSetString(field, "Hello");
  if (!XtIsManaged(button1)) {
	XtManageChild(button1);
  }
  else {
	XtUnmanageChild(button1);
  }
}

int
main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  pane = XtVaCreateManagedWidget("pane", xmPanedWindowWidgetClass,
                                 toplevel, NULL);

  field = XtVaCreateManagedWidget("field",xmTextFieldWidgetClass,
                                  pane, 
#ifdef SKIP_ADJUST
				  XmNskipAdjust, True,
#endif
                                  NULL);  

  button1 = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
	                            pane, 
#ifdef SKIP_ADJUST
				  XmNskipAdjust, True,
#endif
				    NULL);

  button = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass,
                                   pane, 
#ifdef SKIP_ADJUST
				  XmNskipAdjust, True,
#endif
                                   NULL);

  XtAddCallback(button, XmNactivateCallback, activate_callback, NULL);

  XtRealizeWidget(toplevel);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  144,  103, 0,0,0, /* pane */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   31, 0,0,0, /* field */
   CWWidth | CWHeight | CWX | CWY,    3,   42,  138,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,    3,   75,  138,   25, 0,0,0, /* button2 */
   CWWidth | CWHeight | CWX | CWY,  124,   66,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   70,  144,    2, 0,0,0, /* separator */
   CWWidth | CWHeight | CWX | CWY,  124,   33,   10,   10, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   37,  144,    2, 0,0,0, /* separator */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

