/* test for vendor shell decorations */

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
                               XmNmwmDecorations, MWM_DECOR_BORDER | MWM_DECOR_TITLE,
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
