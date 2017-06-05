#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ArrowB.h>
#ifdef LESSTIF_VERSION
#include <XmI/MacrosI.h>
#endif

Widget toplevel, sw, ab;
Dimension st;

void Doit(Widget w, XtPointer client, XtPointer call)
{
#ifdef LESSTIF_VERSION
  XtVaGetValues(sw, XmNshadowThickness, &st, NULL);
  printf("ST: %d: CLIP GEO: %d %d %d %d\n",
	 st,
	 SW_CWX(sw), SW_CWY(sw), SW_CWWidth(sw), SW_CWHeight(sw));
  printf("HSB GEO: %d %d %d %d VSB GEO: %d %d %d %d\n",
	 SW_HSBX(sw), SW_HSBY(sw), SW_HSBWidth(sw), SW_HSBHeight(sw),
	 SW_VSBX(sw), SW_VSBY(sw), SW_VSBWidth(sw), SW_VSBHeight(sw));
#endif
}

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

int main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  sw  = XtVaCreateManagedWidget("sw", xmScrolledWindowWidgetClass, toplevel, 
		XmNscrollingPolicy,	XmAUTOMATIC,
		XmNscrollBarPlacement,	XmBOTTOM_RIGHT,
		XmNwidth,		100,
		XmNheight,		100,
	NULL);
  dump_sw("after create\n", sw);

  ab = XtVaCreateManagedWidget("ab", xmArrowButtonWidgetClass, sw,
		XmNwidth,	300,
		XmNheight,	300,
	NULL);
  dump_sw("after create child\n", sw);

  XtAddCallback(ab, XmNactivateCallback, Doit, NULL);

  XtRealizeWidget(toplevel);
  dump_sw("after realize\n", sw);



  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,  100,  100, 0,0,0, /* sw */
   CWWidth | CWHeight | CWX | CWY,    4,    4,   69,   69, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  300,  300, 0,0,0, /* ab */
   CWWidth | CWHeight | CWX | CWY,   81,    0,   19,   77, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,   81,   77,   19, 0,0,0, /* HorScrollBar */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
