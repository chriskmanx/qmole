/* test for vendor shell mwm functions */

#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/MwmUtil.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", 
                               NULL, 0, 
                               &argc, argv, NULL, 
                               XmNmwmFunctions, MWM_FUNC_RESIZE | MWM_FUNC_MINIMIZE,
                               NULL);

  one = XtVaCreateManagedWidget("One", xmLabelWidgetClass, 
                                toplevel, NULL);

  XtRealizeWidget(toplevel);
  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   22,   17, 0,0,0, /* One */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
