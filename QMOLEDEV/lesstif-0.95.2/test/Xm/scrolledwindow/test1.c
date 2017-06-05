/* $Header: /cvsroot/lesstif/lesstif/test/Xm/scrolledwindow/test1.c,v 1.7 2001/06/18 14:22:06 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ScrollBar.h>
#include <Xm/DrawingA.h>
#ifdef LESSTIF_VERSION
#include <XmI/XmI.h>
#endif

#include "../../common/Test.h"


void
dump_sw(char *where, Widget w)
{
#ifdef LESSTIF_VERSION
#if 0
/* Toggle this for 2.0.  Manager has popup handler added in 2.0 */
    w = (Widget)((char *)w + 4);
#endif
    printf(where);
    printf("Given W: %d H: %d\n", SW_GivenWidth(w), SW_GivenHeight(w));
    printf("Area W: %d H: %d\n", SW_CWWidth(w), SW_CWHeight(w));
    printf("Pad W: %d H: %d\n", SW_MarginWidth(w), SW_MarginHeight(w));
    printf("Offset X: %d Y: %d\n", SW_CWX(w), SW_CWY(w));
    printf("Spacing: %d\n", SW_Spacing(w));
    printf("Has: %d %d\n", SW_HasHSB(w), SW_HasVSB(w));
    printf("HSB: %d %d %d %d\n", SW_HSBX(w), SW_HSBY(w),
	                         SW_HSBWidth(w), SW_HSBHeight(w));
    printf("VSB: %d %d %d %d\n", SW_VSBX(w), SW_VSBY(w),
	                         SW_VSBWidth(w), SW_VSBHeight(w));
    printf("HSB values: %d %d %d %d\n", SW_HSBMinimum(w), SW_HSBMaximum(w),
	                         SW_HSBValue(w), SW_HSBSliderSize(w));
    printf("VSB values: %d %d %d %d\n", SW_VSBMinimum(w), SW_VSBMaximum(w),
	                         SW_VSBValue(w), SW_VSBSliderSize(w));
    printf("Done.\n\n");
#endif
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget sw, hsb, vsb, da;

  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  sw  = XtVaCreateManagedWidget("sw", 
                                xmScrolledWindowWidgetClass, 
                                toplevel, 
                                XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                                XmNscrollBarPlacement, XmBOTTOM_RIGHT,
                                NULL);
  dump_sw("after create\n", sw);

  hsb = XtVaCreateManagedWidget("hsb",
                                xmScrollBarWidgetClass,
                                sw,
                                XmNorientation, XmHORIZONTAL,
                                NULL);
  dump_sw("after create HSB\n", sw);

  vsb = XtVaCreateManagedWidget("vsb",
                                xmScrollBarWidgetClass,
                                sw,
                                XmNorientation, XmVERTICAL,
                                NULL); 
  dump_sw("after create VSB\n", sw);
  
  da = XtVaCreateManagedWidget("da",
                               xmDrawingAreaWidgetClass,
                               sw,
                               NULL);
  dump_sw("after create DA\n", sw);

  XmScrolledWindowSetAreas(sw, hsb, vsb, da);
  dump_sw("after SetAreas\n", sw);

  XtRealizeWidget(toplevel);

  dump_sw("after realize\n", sw);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,   19,   19, 0,0,0, /* sw */
   CWWidth | CWHeight | CWX | CWY,    0,    4,   1,    15, 0,0,0, /* hsb */
   CWWidth | CWHeight | CWX | CWY,    4,    0,   15,    1, 0,0,0, /* vsb */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    1,    1, 0,0,0, /* da */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
