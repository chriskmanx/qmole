#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmDrawingAreaWidgetClass,
                                       toplevel,
	                               NULL);

  XtRealizeWidget(toplevel);
  
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  506,  322,    1,    1, 0,0,0, /* drawingArea */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}

  LessTifTestMainLoop(toplevel);

  XtAppMainLoop(theApp);

  exit(0);
}
