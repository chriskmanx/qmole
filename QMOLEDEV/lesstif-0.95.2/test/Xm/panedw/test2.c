#include <Xm/Xm.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h> 

Widget toplevel, field, pane, button;

void activate_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmTextFieldSetString(field, "Hello");
}

int
main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  pane = XtVaCreateManagedWidget("pane", xmPanedWindowWidgetClass, toplevel,
				 XmNsashHeight, 30,
                                 NULL);

  field = XtVaCreateManagedWidget("field",xmTextFieldWidgetClass,
                                  pane, 
                                  NULL);  

  button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass,
                                   pane, 
                                   NULL);

  XtAddCallback(button, XmNactivateCallback, activate_callback, NULL);

  XtRealizeWidget(toplevel);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  144,   70, 0,0,0, /* pane */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   31, 0,0,0, /* field */
   CWWidth | CWHeight | CWX | CWY,    3,   42,  138,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,  124,   23,   10,   30, 0,0,0, /* sash */
   CWWidth | CWHeight | CWX | CWY,    0,   37,  144,    2, 0,0,0, /* separator */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

