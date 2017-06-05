
#include <stdio.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Dimension w,h;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "PBG1", NULL, 0, &argc, argv, NULL,
		NULL);


  one = XtVaCreateManagedWidget("One", xmPushButtonWidgetClass, toplevel,
				NULL);
  XtVaSetValues(one,
		XmNunitType, Xm1000TH_INCHES,
		XmNwidth, 1000, XmNheight, 1000,
		NULL);

  /* test export */
  XtVaGetValues(one, XmNwidth, &w, XmNheight, &h, NULL);

  printf("Width: %d Height: %d\n", w, h);

  XtRealizeWidget(toplevel);

  
{
    int width, height;
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  987,  987, 0,0,0, /* One */ 
    };
width = XmConvertUnits(toplevel, XmHORIZONTAL,Xm1000TH_INCHES, 1000, XmPIXELS);
width = XmConvertUnits(toplevel, XmHORIZONTAL,XmPIXELS, width, Xm1000TH_INCHES);
Expected[0].width = width;
height = XmConvertUnits(toplevel, XmHORIZONTAL,Xm1000TH_INCHES, 1000, XmPIXELS);
height = XmConvertUnits(toplevel, XmHORIZONTAL,XmPIXELS, height, Xm1000TH_INCHES);
Expected[0].height = height;
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
