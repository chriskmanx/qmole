#include <Xm/Xm.h>
#include <Xm/ArrowB.h>

void HiCB(Widget w,XtPointer client_data,XtPointer call_data);

int dir = 0;
Widget butt1;

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  butt1= XtVaCreateManagedWidget("Button1", xmArrowButtonWidgetClass, toplevel, 
				XmNarrowDirection,dir,
				NULL);

  XtAddCallback(butt1,XmNactivateCallback,HiCB,NULL);

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

void HiCB(Widget w,XtPointer client_data,XtPointer call_data)
{
 	dir++;
	
	dir %= 4;

	XtVaSetValues(butt1, XmNarrowDirection, dir, NULL);
}
