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

  toplevel = XtVaAppInitialize(&theApp, "toggle1", NULL, 0,
			       &argc, argv, NULL, NULL);

  butt= XtVaCreateManagedWidget("Button1", xmDrawnButtonWidgetClass, toplevel, 
				XmNwidth, 100,
				XmNheight, 100,
				XmNpushButtonEnabled, True,
				NULL);

  XtRealizeWidget(toplevel);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel)));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel)));

  pixmap = XmGetPixmap(XtScreen(butt), "xlogo64", fg, bg);

  XtVaSetValues(butt, XmNlabelPixmap, pixmap, XmNlabelType, XmPIXMAP, NULL);


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
