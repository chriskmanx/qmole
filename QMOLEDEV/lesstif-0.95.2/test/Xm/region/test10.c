/* test for _XmRegionDrawShadow */

#include <Xm/XmP.h>
#include <Xm/ManagerP.h>
#include <Xm/DrawingAP.h>
#ifdef LESSTIF_REVISION
#include <XmI/MacrosI.h>
#endif
#include <stdio.h>

extern void print_region();

Widget toplevel, da;
XtAppContext app;

#ifdef LESSTIF_REVISION
void
exposeCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmRegion region1 = _XmRegionCreate();
  XRectangle rect;

  printf ("\tRegion 1 -- _XmRegionCreate()\n");
  print_region(region1);

  rect.x = rect.y = 5;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  rect.x = rect.y = 35;
  rect.width = rect.height = 100;

  _XmRegionUnionRectWithRegion(&rect,
			       region1, region1);

  printf ("\tRegion 1 -- after two _XmUnionRectWithRegion's\n");
  print_region(region1);

  _XmRegionDrawShadow(XtDisplay(da),
		      XtWindow(da),
		      MGR_TopShadowGC(da),
		      MGR_BottomShadowGC(da),
		      region1,
		      0, 2, XmSHADOW_IN);
}
#endif

int
main(int argc,
     char **argv) 
{
#ifdef LESSTIF_REVISION
  toplevel = XtVaAppInitialize(&app, 
			       "Region", 
			       NULL, 0, 
			       &argc, argv, 
			       NULL, NULL);

  da = XmCreateDrawingArea(toplevel, 
			   "da", 
			   NULL, 0);

  XtManageChild(da);

  XtAddCallback(da, XmNexposeCallback, exposeCallback, NULL);

  XtRealizeWidget(toplevel);
  
  /* since only runs under lesstif, can't get expected geometry */
  LessTifTestMainLoop(  toplevel );
#else
  printf("This test only works under LessTif since it uses lesstif internals\n");
#endif
  exit(0);
}
