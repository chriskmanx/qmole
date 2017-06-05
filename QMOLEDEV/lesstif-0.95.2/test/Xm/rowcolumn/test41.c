/* $Id: test41.c,v 1.3 2000/08/29 21:59:21 dannybackx Exp $ */
/*
To:          lesstif@lesstif.org
From:        Alexander Mai <mai@migdal.ikp.physik.tu-darmstadt.de>
Subject:     pixmap on pushbutton
*/

/*
   should display a bitmap on a pushbutton
*/

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>

#define xlogo16_width 16
#define xlogo16_height 16
static char xlogo16_bits[] = {
   0x0f, 0x80, 0x1e, 0x80, 0x3c, 0x40, 0x78, 0x20, 0x78, 0x10, 0xf0, 0x08,
   0xe0, 0x09, 0xc0, 0x05, 0xc0, 0x02, 0x40, 0x07, 0x20, 0x0f, 0x20, 0x1e,
   0x10, 0x1e, 0x08, 0x3c, 0x04, 0x78, 0x02, 0xf0};

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Pixel   foreground, background;
    Display *display;
    Pixmap ptmp;
    Widget toplevel, rc;
    Widget pane1;
    Widget button1, button2;
    Arg args[10];
    int n;

    toplevel = XtVaAppInitialize(&theApp, "bmtest", NULL, 0,
                                 &argc, argv, NULL, NULL);

    XtSetArg(args[0], XmNforeground, &foreground);
    XtSetArg(args[1], XmNbackground, &background);
    XtGetValues(toplevel, args, 2);

    display = XtDisplay(toplevel);

    pane1 = XmCreatePulldownMenu(toplevel,
                                 "pulldown",
                                 NULL, 0);

    ptmp = XCreatePixmapFromBitmapData(display,
                        DefaultRootWindow(display),
                        xlogo16_bits, xlogo16_width, xlogo16_height,
                        foreground,
                        background,
                        DefaultDepth(display,DefaultScreen(display)));

    n = 0;
    XtSetArg(args[n], XmNlabelType, XmPIXMAP);  n++;
    XtSetArg(args[n], XmNlabelPixmap, ptmp);  n++;

    button1 = XtVaCreateManagedWidget("button1",
                                      xmPushButtonWidgetClass,
                                      pane1,
                                      NULL);
    XtSetValues (button1, args, n);

    button2 = XtVaCreateManagedWidget("button2",
                                      xmPushButtonWidgetClass,
                                      pane1,
                                      NULL);
    XtSetValues (button2, args, n);

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pane1); n++;
    XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized("Option:")); n++;

    rc = XmCreateOptionMenu(toplevel,
                            "option",
                            args, n);

    XtManageChild(rc);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	108,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	46,	32,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	52,	3,	53,	32,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    exit(0);
}
