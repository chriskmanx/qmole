#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawnB.h>

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt;
  Pixmap pixmap;
  Pixel fg, bg;

  toplevel = XtVaAppInitialize(&theApp, "test3", NULL, 0,
			       &argc, argv, NULL, NULL);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel)));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel)));

  pixmap = XmGetPixmap(DefaultScreenOfDisplay(XtDisplay(toplevel)),
		       "xlogo64", fg, bg);
  if (pixmap == XmUNSPECIFIED_PIXMAP)
	printf("Pixmap unspecified\n");

  butt= XtVaCreateManagedWidget("Button1", xmDrawnButtonWidgetClass, toplevel, 
				XmNwidth, 100,
				XmNheight, 100,
			        XmNshadowThickness, 4,
			        XmNshadowType, XmSHADOW_ETCHED_OUT,
				XmNlabelType, XmPIXMAP,
				XmNlabelPixmap, pixmap,
				NULL);


  XtRealizeWidget(toplevel);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  100,  100, 0,0,0, /* Button1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
