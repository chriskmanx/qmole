#include <Xm/XmP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/RowColumnP.h>
#include <Xm/DrawingA.h>
#ifdef LESSTIF_VERSION
#include <XmI/XmI.h>
#endif

int
main(int argc, char **argv)
{
  Widget toplevel;
  Widget sw, da, rc;

  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  rc = XtVaCreateManagedWidget("rc",xmRowColumnWidgetClass,
								toplevel,
								NULL);

  /* create a 'VARIABLE' scrolledwin */
  sw  = XtVaCreateManagedWidget("sw", 
                                xmScrolledWindowWidgetClass, 
                                rc, 
                                XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                                XmNvisualPolicy, XmVARIABLE,
								XmNwidth,200,
								XmNheight,100,
                                NULL);

  /* the child is smaller, but at creation time their
   * `variable' scrolledw doesn't shrink.
   */
  da = XtVaCreateManagedWidget("da",
                               xmDrawingAreaWidgetClass,
                               sw,
							   XmNwidth,100,
							   XmNheight,100,
                               NULL);

  XmScrolledWindowSetAreas(sw, NULL, NULL, da);

  /* create another 'VARIABLE' scrolledwin */
  sw  = XtVaCreateManagedWidget("sw", 
                                xmScrolledWindowWidgetClass, 
                                rc, 
                                XmNscrollingPolicy, XmAPPLICATION_DEFINED,
                                XmNvisualPolicy, XmVARIABLE,
								XmNwidth,200,
								XmNheight,100,
                                NULL);

  /* this time, the child is bigger, but at creation time their
   * `variable' scrolledw doesn't grow.
   */
  da = XtVaCreateManagedWidget("da",
                               xmDrawingAreaWidgetClass,
                               sw,
							   XmNwidth,300,
							   XmNheight,300,
                               NULL);

  XmScrolledWindowSetAreas(sw, NULL, NULL, da);

  XtRealizeWidget(toplevel);

{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight ,               4,  50,   206,   209, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,   3,   200,   100, 0,0,0, /* sw */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    200,    100, 0,0,0, /* da */ 
   CWWidth | CWHeight | CWX | CWY,    3,   106,   200,   100, 0,0,0, /* sw */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    200,    100, 0,0,0, /* da */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
