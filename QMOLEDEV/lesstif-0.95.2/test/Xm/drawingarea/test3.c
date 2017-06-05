/* test for margin width's and height's -- DEFECT 61 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>


int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea, button, other_button;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmDrawingAreaWidgetClass,
                                       toplevel,
	                               NULL);

  button = XtVaCreateManagedWidget("button",
                                   xmPushButtonWidgetClass,
                                   drawingArea,
                                   NULL);

  other_button = XtVaCreateManagedWidget("button2",
                                         xmPushButtonWidgetClass,
                                         drawingArea,
                                         XmNx, 100,
                                         XmNy, 100,
                                         NULL);

  XtRealizeWidget(toplevel);
  
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  506,  322,  164,  135, 0,0,0, /* drawingArea */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   48,   25, 0,0,0, /* button */
   CWWidth | CWHeight | CWX | CWY,  100,  100,   54,   25, 0,0,0, /* button2 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}

  LessTifTestMainLoop(toplevel);

  exit(0);
}
