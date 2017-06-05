/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/imagecache/test1.c,v 1.3 2001/07/09 20:28:47 amai Exp $
 * test for XmGetPixmap and reference counting
 *
 */

#include <stdlib.h>
 
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

XtAppContext theApp;
Pixmap pix, pix2;
Widget toplevel, pb1, pb2,rc;

int
main(int argc,
     char **argv)
{
    toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc = XtVaCreateManagedWidget("rc",
                                 xmRowColumnWidgetClass,
                                 toplevel, 
                                 XmNorientation, XmHORIZONTAL,
                                 NULL);
  
    pix = XmGetPixmap(DefaultScreenOfDisplay(XtDisplay(toplevel)),
		      "xlogo64",
		      BlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))),
		      WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))));

    pix2 = XmGetPixmapByDepth(DefaultScreenOfDisplay(XtDisplay(toplevel)),
			      "xlogo64",
			      BlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))),
			      WhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))),
			      DefaultDepthOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel))));

    pb1 = XtVaCreateManagedWidget("pb1",
				  xmPushButtonWidgetClass,
				  rc,
				  XmNlabelType, XmPIXMAP,
				  XmNlabelPixmap, pix,
				  NULL);
				  
    pb2 = XtVaCreateManagedWidget("pb2",
				  xmPushButtonWidgetClass,
				  rc,
				  XmNlabelType, XmPIXMAP,
				  XmNlabelPixmap, pix2,
				  NULL);

    XtRealizeWidget(toplevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  161,   82, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   76,   76, 0,0,0, /* pb1 */
   CWWidth | CWHeight | CWX | CWY,   82,    3,   76,   76, 0,0,0, /* pb2 */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );
    exit(0);
}
