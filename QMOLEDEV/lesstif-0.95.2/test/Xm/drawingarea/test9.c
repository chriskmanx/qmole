#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Label.h>

static char *FallBack[] = {
		"*buttonbutton.background: red",
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, drawingArea, button;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, FallBack, NULL);
  XtVaSetValues(toplevel, 
  	XmNallowShellResize, True,
  	NULL);

  drawingArea= XtVaCreateManagedWidget("drawingArea",
                                       xmDrawingAreaWidgetClass,
                                       toplevel,
                                       XmNwidth, 100,
                                       XmNheight, 100,
	                               NULL);

  button = XtVaCreateManagedWidget("buttonbutton",
                                   xmLabelWidgetClass,
                                   drawingArea,
                                   NULL);

  XtRealizeWidget(toplevel);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  284,  552,  100,  100, 0,0,0, /* drawingArea */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   76,   17, 0,0,0, /* buttonbutton */
   CWWidth | CWHeight            ,  284,  552,  106,   37, 0,0,0, /* drawingArea */
   CWWidth | CWHeight | CWX | CWY,   20,   10,   76,   17, 0,0,0, /* buttonbutton */
   CWWidth | CWHeight            ,  284,  552,  230,   37, 0,0,0, /* drawingArea */
   CWWidth | CWHeight | CWX | CWY,   20,   10,  200,   17, 0,0,0, /* buttonbutton */
};
/* toplevel should be replaced with to correct applicationShell */
  
PrintDetails(toplevel, Expected);
LessTifTestWaitForIt(toplevel);
  XtVaSetValues(button,
  	XmNx, 20,
  	NULL);
PrintDetails(toplevel, Expected);
LessTifTestWaitForIt(toplevel);
  XtVaSetValues(button,
  	XmNwidth, 200,
  	NULL);
PrintDetails(toplevel, Expected);
LessTifTestWaitForIt(toplevel);
}

  LessTifTestMainLoop(toplevel);

  exit(0);
}
