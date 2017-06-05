/* test for showAsDefault and defaultButtonShadowThickness resources */

#include <Xm/Xm.h>
#include <Xm/PushB.h>

Display *theDisplay;
Window theRootWindow;
Pixmap Pix;
Widget toplevel;

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Pixel fg,bg;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel, 
				XmNshowAsDefault, 1,
				XmNdefaultButtonShadowThickness, 10,
				NULL);

  XtRealizeWidget(toplevel);

  theDisplay = XtDisplay(toplevel);
  theRootWindow = XDefaultRootWindow(theDisplay);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(theDisplay));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(theDisplay));

  Pix = XmGetPixmap(DefaultScreenOfDisplay(theDisplay),
                    "xlogo64",
                    fg, bg);

  XtVaSetValues(one,
                XmNlabelType,XmPIXMAP,
	        XmNlabelPixmap,Pix, 
                NULL);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   78,   73, 0,0,0, /* One */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
