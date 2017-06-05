#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>

void push_callback(Widget w, XtPointer clientData, XtPointer callData)
{
   Dimension width, height;
   Position x, y;

   XtVaGetValues(w,
                 XmNwidth, &width,
                 XmNheight, &height,
                 NULL);

  XtVaSetValues(w, 
                XmNwidth, width + 10,
                XmNheight, height + 10,
                NULL);

   XtVaGetValues(w,
                 XmNx, &x,
                 XmNy, &y,
                 NULL);

  XtVaSetValues(w, 
                XmNx, x + 10,
                XmNy, y + 10,
                NULL);
}

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea, button;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmDrawingAreaWidgetClass,
                                       toplevel,
	                               NULL);

  button = XtVaCreateManagedWidget("button",
                                   xmPushButtonGadgetClass,
                                   drawingArea,
                                   NULL);

  XtAddCallback(button, XmNactivateCallback, push_callback, NULL);

  XtRealizeWidget(toplevel);
  
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  506,  322,   68,   45, 0,0,0, /* drawingArea */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   48,   25, 0,0,0, /* button */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}

  LessTifTestMainLoop(toplevel);

  exit(0);
}
