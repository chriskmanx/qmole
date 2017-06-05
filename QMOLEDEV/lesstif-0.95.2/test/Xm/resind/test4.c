
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;
  Dimension w,h;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "PBG1", NULL, 0, &argc, argv, NULL,
		NULL);

  two = XtVaCreateManagedWidget("Two", xmFormWidgetClass, toplevel,
				NULL);

  one = XtVaCreateManagedWidget("One", xmLabelWidgetClass, two,
				NULL);
  XtVaSetValues(two,
		XmNunitType, Xm1000TH_INCHES,
		XmNwidth, 2000, XmNheight, 2000,
		NULL);

  XtVaSetValues(one,
		XmNunitType, Xm1000TH_INCHES,
		XmNwidth, 1000, XmNheight, 1000,
		NULL);

  /* test export */
  XtVaGetValues(two, XmNwidth, &w, XmNheight, &h, NULL);

  printf("Width: %d Height: %d\n", w, h);

  XtRealizeWidget(toplevel);

  
{
    int width, height;
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50, 1987, 1987, 0,0,0, /* Two */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  987,  987, 0,0,0, /* One */ 
    };
width = XmConvertUnits(toplevel, XmHORIZONTAL,Xm1000TH_INCHES, 2000, XmPIXELS);
width = XmConvertUnits(toplevel, XmHORIZONTAL,XmPIXELS, width, Xm1000TH_INCHES);
Expected[0].width = width;
height = XmConvertUnits(toplevel, XmHORIZONTAL,Xm1000TH_INCHES, 2000, XmPIXELS);
height = XmConvertUnits(toplevel, XmHORIZONTAL,XmPIXELS, height, Xm1000TH_INCHES);
Expected[0].height = height;
width = XmConvertUnits(toplevel, XmHORIZONTAL,Xm1000TH_INCHES, 1000, XmPIXELS);
width = XmConvertUnits(toplevel, XmHORIZONTAL,XmPIXELS, width, Xm1000TH_INCHES);
Expected[1].width = width;
height = XmConvertUnits(toplevel, XmHORIZONTAL,Xm1000TH_INCHES, 1000, XmPIXELS);
height = XmConvertUnits(toplevel, XmHORIZONTAL,XmPIXELS, height, Xm1000TH_INCHES);
Expected[1].height = height;
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
