#include <stdio.h>
#include <Xm/XmP.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>

#define TEST_CBS 1

void HiCB(Widget w,XtPointer client_data,XtPointer call_data);


int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt;
  Pixmap Pix;
  Display *theDisplay;
  Window theRootWindow;
  Pixel fg,bg;
  Dimension mt, mb, ml, mr, mw, mh, st, ht;
  XtWidgetGeometry geo;

  toplevel = XtVaAppInitialize(&theApp, "toggle1", NULL, 0,
			       &argc, argv, NULL, NULL);

  butt= XtVaCreateManagedWidget("Button1", xmToggleButtonWidgetClass, toplevel, 
#if 0
				XmNindicatorOn, False,
#endif
				XmNshadowThickness, 5,
				XmNfillOnSelect, True,
				NULL);

  XtAddCallback(butt,XmNvalueChangedCallback,HiCB,NULL);

  XtRealizeWidget(toplevel);

  theDisplay = XtDisplay(toplevel);
  theRootWindow = XDefaultRootWindow(theDisplay);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(theDisplay));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(theDisplay));

  Pix = XmGetPixmap(DefaultScreenOfDisplay(theDisplay),
                    "xlogo64",
                    fg, bg);
  if (Pix == XmUNSPECIFIED_PIXMAP)
	printf("PIX IS UNSPECIFIED\n");

  XtVaGetValues(butt,
		XmNmarginTop, &mt, XmNmarginBottom, &mb,
		XmNmarginLeft, &ml, XmNmarginRight, &mr,
		XmNmarginWidth, &mw, XmNmarginHeight, &mh,
		XmNshadowThickness, &st, XmNhighlightThickness, &ht,
		NULL);
printf("%d %d %d %d %d %d %d %d\n",
	mt, mb, ml, mr, mw, mh, st, ht);

  XtVaSetValues(butt,
#if 0
	        XmNlabelPixmap,Pix, 
#endif
                XmNlabelType, XmPIXMAP,
                NULL);

  Pix = XmGetPixmap(DefaultScreenOfDisplay(theDisplay),
                    "woman",
                    fg, bg);

  XtVaGetValues(butt,
		XmNmarginTop, &mt, XmNmarginBottom, &mb,
		XmNmarginLeft, &ml, XmNmarginRight, &mr,
		XmNmarginWidth, &mw, XmNmarginHeight, &mh,
		XmNshadowThickness, &st, XmNhighlightThickness, &ht,
		NULL);
printf("%d %d %d %d %d %d %d %d\n",
	mt, mb, ml, mr, mw, mh, st, ht);

  XtVaSetValues(butt,
	        XmNselectPixmap, Pix, 
                NULL);

  XtQueryGeometry(butt, NULL, &geo);
printf("toggle wants: %d %d has %d %d\n",
	geo.width, geo.height, XtWidth(butt), XtHeight(butt));

  XtVaGetValues(butt,
		XmNmarginTop, &mt, XmNmarginBottom, &mb,
		XmNmarginLeft, &ml, XmNmarginRight, &mr,
		XmNmarginWidth, &mw, XmNmarginHeight, &mh,
		XmNshadowThickness, &st, XmNhighlightThickness, &ht,
		NULL);
printf("%d %d %d %d %d %d %d %d\n",
	mt, mb, ml, mr, mw, mh, st, ht);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  387,  402,   77,   45, 0,0,0, /* Button1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

  exit(0);
}

void HiCB(Widget w,XtPointer client_data,XtPointer call_data)
{
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call_data;

    printf("Toggle Me and I'm Yours: %d\n", cbs->set);

#if TEST_CBS
    cbs->set = False;
#endif
}
