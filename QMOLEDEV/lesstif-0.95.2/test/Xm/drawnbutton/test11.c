#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawnB.h>

#include <X11/Xft/Xft.h>

void HiCB(Widget w,XtPointer client_data,XtPointer call_data);

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt;

  toplevel = XtVaAppInitialize(&theApp, "drawn", NULL, 0,
                              &argc, argv, NULL, NULL);

  butt= XtVaCreateManagedWidget("Button1", xmDrawnButtonWidgetClass, toplevel, 
                               XmNwidth, 100,
                               XmNheight, 100,
                               NULL);

  XtAddCallback(butt,XmNarmCallback,HiCB,NULL);
  XtAddCallback(butt,XmNdisarmCallback,HiCB,NULL);
  XtAddCallback(butt,XmNactivateCallback,HiCB,NULL);
  XtAddCallback(butt,XmNexposeCallback,HiCB,NULL);
  XtAddCallback(butt,XmNresizeCallback,HiCB,NULL);

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

void draw(Widget w, Window win, Dimension width, Dimension height, Pixel pix)
{
       GC drawGC;

       drawGC = XtAllocateGC(w, 0,0, NULL, 0,0);
       XSetForeground(XtDisplayOfObject(w), drawGC, pix);
       XDrawRectangle(XtDisplayOfObject(w), win, drawGC,
                      10, 10, width - 20, height - 20);
       XtReleaseGC(w, drawGC);
{
	/* Xft based stuff */
	static XftFont		*f = NULL;
	static XftColor		*color = NULL, c;
	static XftDraw		*draw;
	XftChar8		*s = "Yow baby";
	int			len = 8,
				x = 20,
				y = 40;
	static char		*fn = NULL;
	static int		one = 0;

	if (! fn)
		fn = XtNewString("-*-chancery l-*-*-*-*-*-120-*-*-*-*-iso8859-1");
	if (! one) {
		one++;
		XftInit(NULL);
	}
	if (! f)
		f = XftFontOpenXlfd(XtDisplay(w),
			XScreenNumberOfScreen(XtScreen(w)), fn);
#if 0
			"-*-luxi serif-*-*-*-*-*-120-*-*-*-*-iso8859-1");
#endif
	if (! f)
		f = XftFontOpenXlfd(XtDisplay(w),
			XScreenNumberOfScreen(XtScreen(w)),
			"fixed");
	if (! f)
		return;

	if (!color) {
		color = &c;
		color->color.red = 100;
		color->color.green = 0;
		color->color.blue = 0;
		color->color.alpha = 0xFFFF;
		XtVaGetValues(w, XmNforeground, &color->pixel, NULL);
		color->pixel = pix;
	}

	if (!draw)
		draw = XftDrawCreate(XtDisplay(w),
			win, DefaultVisualOfScreen(XtScreen(w)),
			DefaultColormapOfScreen(XtScreen(w)));

	XftDrawString8(draw, color, f, x, y, s, len);
//	XftFontClose(XtDisplay(w), f);
}
}

void HiCB(Widget w,XtPointer client_data,XtPointer call_data)
{
       XmDrawnButtonCallbackStruct *cbs = (XmDrawnButtonCallbackStruct *)call_data;
       Dimension width, height;
       Pixel color;

       XtVaGetValues(w,
                     XmNwidth, &width,
                     XmNheight, &height,
                     XmNforeground, &color,
                     NULL);
       switch (cbs->reason) {
       case XmCR_ARM:
               printf("Widget armed\n");
               break;
       case XmCR_DISARM:
               printf("Widget disarmed\n");
               break;
       case XmCR_ACTIVATE:
               printf("Widget activated\n");
               break;
       case XmCR_EXPOSE:
               printf("Widget exposed\n");
               draw(w, cbs->window, width, height, color);
               break;
       case XmCR_RESIZE:
               printf("Widget resized\n");
               draw(w, cbs->window, width, height, color);
               break;
       default:
               printf("callback for unknown reason\n");
       }
}

