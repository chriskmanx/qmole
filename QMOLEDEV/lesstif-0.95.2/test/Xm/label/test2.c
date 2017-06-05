#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <stdio.h>

Display *theDisplay;
Window theRootWindow;
Pixmap Pix;
Widget toplevel;

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget label;
  Pixel fg,bg;
  XmFontList fontlist;

  toplevel = XtVaAppInitialize(&theApp, "drawingArea", NULL, 0,
			       &argc, argv, NULL, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  fontlist = XmFontListAppendEntry(fontlist,
			   XmFontListEntryCreate("MY_FONT1",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-r-normal--17-0-75-75-p-*-iso8859-1")));

#if 0
  label= XtVaCreateManagedWidget("Button1", xmLabelWidgetClass, toplevel, XmNfontList, fontlist,
				NULL);
#else
  label= XtVaCreateManagedWidget("Button1", xmLabelWidgetClass, toplevel,
				NULL);
#endif
  XtRealizeWidget(toplevel);

  theDisplay = XtDisplay(toplevel);
  theRootWindow = XDefaultRootWindow(theDisplay);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(theDisplay));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(theDisplay));

  Pix = XmGetPixmap(DefaultScreenOfDisplay(theDisplay),
                    "xlogo64",
                    fg, bg);
  if (Pix == XmUNSPECIFIED_PIXMAP) {
	printf("NO PIXMAP!\n");
	exit(0);
  }

  XtVaSetValues(label,
	        XmNlabelPixmap,Pix, 
                XmNlabelType,XmPIXMAP,
                NULL);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	46,	17,	0,0,0,	/* Form */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(theApp);
  */

  exit(0);
}
