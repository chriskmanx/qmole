/* test for multi line labels */

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;
  int in1000, mm100, pts100;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, 
                               XmNunitType, Xm1000TH_INCHES,
                               NULL);

  printf("in1000 to mm100: %d\n", mm100 = XmConvertUnits(toplevel, XmHORIZONTAL,
						 Xm1000TH_INCHES, 1000,
						 Xm100TH_MILLIMETERS));
  printf("mm100 to in1000: %d\n", XmConvertUnits(toplevel, XmHORIZONTAL,
						 Xm100TH_MILLIMETERS, mm100,
						 Xm1000TH_INCHES));
  printf("pts100 to mm100: %d\n", pts100 = XmConvertUnits(toplevel, XmHORIZONTAL,
						 Xm100TH_POINTS, 7200,
						 Xm100TH_MILLIMETERS));
  printf("mm100 to pts100: %d\n", XmConvertUnits(toplevel, XmHORIZONTAL,
						 Xm100TH_MILLIMETERS, pts100,
						 Xm100TH_POINTS));
  printf("pts100 to in1000: %d\n", in1000 = XmConvertUnits(toplevel, XmHORIZONTAL,
						 Xm100TH_POINTS, 7200,
						 Xm1000TH_INCHES));
  printf("in1000 to pts100: %d\n", XmConvertUnits(toplevel, XmHORIZONTAL,
						 Xm1000TH_INCHES, in1000,
						 Xm100TH_POINTS));
  printf("100 PIXELS to 100TH_MILLIMETERS: %d\n", XmConvertUnits(toplevel, XmHORIZONTAL,
						 XmPIXELS, 100,
						 Xm100TH_MILLIMETERS));

  XtRealizeWidget(toplevel);

  
  /* NO geometry */
  LessTifTestMainLoop(  toplevel );

  exit(0);
}
