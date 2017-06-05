/* the window should show a string, upon button press resize to show a
 * pixmap.  The highlight should show correctly as the cursor enters
 * and leaves the window.
 */

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/LabelP.h>


static void
mytest(Widget w, XtPointer p, XtPointer s)
{
    Pixel fg, bg;
    int height, width;
    Pixmap pixmap;

    printf("hi\n");

    XtVaGetValues(w,
		  XmNwidth, &width,
		  XmNheight, &height,
		  NULL);
    printf("height == %d, width == %d\n", height, width);

    fg = XBlackPixelOfScreen(XtScreen(w));
    bg = XWhitePixelOfScreen(XtScreen(w));
    pixmap = XmGetPixmap(XtScreen(w), "xlogo64", fg, bg);

    XtVaSetValues(w,
		  XmNwidth, 0,
		  XmNheight, 0,
		  XmNlabelPixmap, pixmap,
		  XmNlabelType,XmPIXMAP,
		  NULL);

    XtVaGetValues(w,
		  XmNwidth, &width,
		  XmNheight, &height,
		  NULL);
    printf("height == %d, width == %d\n", height, width);
}

String fallbacks[] = {"Hello*allowShellResize: True",
		      "Hello*highlightThickness: 10",
		      NULL};

int
main(int argc, char **argv)
{
    Widget toplevel, button;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Hello", NULL, 0,
				 &argc, argv, fallbacks, NULL);

    button =
	XtVaCreateManagedWidget("pushmePlease", xmPushButtonWidgetClass,
				toplevel, NULL);

    XtAddCallback(button, XmNactivateCallback, mytest, NULL);

    XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	100,	41,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	45,	30,	30,	25,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
    XtAppMainLoop(app);
    */
    exit(0);
}
