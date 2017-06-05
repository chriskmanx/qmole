/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test3.c,v 1.3 2002/04/17 16:32:01 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/DrawingA.h>


GC gc;
XmFontList fl;
XmString xmstr;

void
expose(Widget w, XtPointer clientData, XtPointer callData)
{
    XmStringDraw(XtDisplay(w),
		 XtWindow(w),
		 fl,
		 xmstr,
		 gc,
		 10,10,
		 100,
		 XmALIGNMENT_CENTER,
		 XmSTRING_DIRECTION_L_TO_R,
		 NULL);

    XmStringDrawImage(XtDisplay(w),
		      XtWindow(w),
		      fl,
		      xmstr,
		      gc,
		      10,50,
		      100,
		      XmALIGNMENT_CENTER,
		      XmSTRING_DIRECTION_L_TO_R,
		      NULL);
}

int 
main(int argc,
     char **argv)
{
    Widget toplevel, da;
    XtAppContext app;
    XGCValues values;
    XFontStruct *fs;

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0, &argc, argv, NULL, NULL);

    da = XmCreateDrawingArea(toplevel, "da", NULL, 0);

    XtAddCallback(da, XmNexposeCallback, expose, NULL);

    XtManageChild(da);

    XtRealizeWidget(toplevel);

    values.foreground = BlackPixelOfScreen(XtScreen(toplevel));
    values.background = WhitePixelOfScreen(XtScreen(toplevel));

    gc = XtGetGC(toplevel, GCForeground | GCBackground, &values);

    /* you must have a valid font in the GC _entry_ before calling XmStringDraw */
    fs = XLoadQueryFont(XtDisplay(toplevel), "fixed");
    XSetFont(XtDisplay(toplevel), gc, fs->fid);

    fl = _XmGetDefaultFontList(toplevel, XmTEXT_FONTLIST);

    xmstr = XmStringCreate("Hello World", XmFONTLIST_DEFAULT_TAG);

    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,    1,    1, 0,0,0, /* da */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );
    exit(0);
}
