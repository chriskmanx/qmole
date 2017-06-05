#include <Xm/Xm.h>
#include <Xm/ArrowB.h>
#include <stdio.h>

void
cb(Widget w, XtPointer userData, XtPointer cbs) {
  XmArrowButtonCallbackStruct *abcs = (XmArrowButtonCallbackStruct *)cbs;

  printf("ArrowButton ACTIVATE: click_count: %d\n", abcs->click_count);
}

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt1;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  butt1= XtVaCreateManagedWidget("Button1", xmArrowButtonWidgetClass, toplevel, 
				NULL);

  XtAddCallback(butt1, XmNactivateCallback, cb, NULL);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   23,   23, 0,0,0, /* Button1 */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
