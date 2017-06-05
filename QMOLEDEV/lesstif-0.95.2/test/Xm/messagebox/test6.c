/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test6.c,v 1.6 2001/05/15 14:08:34 amai Exp $ */

#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>

#include "../../common/Test.h"


Widget toplevel;

int
main (int argc, char **argv)
{
	XtAppContext	appc;
	Widget	rc, tmp, tmpe;

	toplevel = XtVaAppInitialize (&appc, "Boxes", NULL, 0,
	&argc, argv, NULL, NULL);
#if 0
    XtAddEventHandler(toplevel, (EventMask)0, True, _XEditResCheckMessages, NULL);
#endif

	rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel,
	    NULL);
	tmp = XtVaCreateManagedWidget("question", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "question", sizeof(char *),
		XmNdialogType,	XmDIALOG_QUESTION,
	    NULL);

	tmp = XtVaCreateManagedWidget("message", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "message", sizeof(char *),
		XmNdialogType,	XmDIALOG_MESSAGE,
	    NULL);
	tmp = XtVaCreateManagedWidget("error", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "error", sizeof(char *),
		XmNdialogType,	XmDIALOG_ERROR,
	    NULL);
	tmp = XtVaCreateManagedWidget("work", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "work", sizeof(char *),
		XmNdialogType,	XmDIALOG_WORKING,
	    NULL);
	tmp = XtVaCreateManagedWidget("information", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "information", sizeof(char *),
		XmNdialogType,	XmDIALOG_INFORMATION,
	    NULL);
	tmpe = XtVaCreateManagedWidget("warning", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "warning", sizeof(char *),
		XmNdialogType,	XmDIALOG_WARNING,
	    NULL);

	XtRealizeWidget (toplevel);

#if 0
    {
/*
 * don't ask, you don't want to know
 */
	Pixmap pix, npix;
	unsigned int tmpi, tmpw, tmph;
	int tmpx, tmpy;
	Window tmpwin;
	unsigned depth;
	GC gc;
	XGCValues values;


	XtVaGetValues(XmMessageBoxGetChild(tmpe, XmDIALOG_SYMBOL_LABEL),
		      XmNlabelPixmap, &pix,
		      XmNforeground, &values.foreground,
		      XmNbackground, &values.background,
		      NULL);
	XGetGeometry(XtDisplay(tmpe),
                 pix,
                 &tmpwin,
                 &tmpx, &tmpy,
                 &tmpw, &tmph,
                 &tmpi,&depth);

	npix =  XCreatePixmap(XtDisplay(tmpe), RootWindowOfScreen(XtScreen(tmpe)), tmpw, tmph, 1);

	values.function = GXcopyInverted;
        gc = XCreateGC(XtDisplay(tmpe), npix,
			 GCForeground|GCBackground|GCFunction, &values);

            XCopyPlane(XtDisplay(tmpe),
                       pix,
                       npix,
                       gc,
                       0,0,
                       tmpw, tmph,
                       0,0,
                       2);

	XWriteBitmapFile(XtDisplay(toplevel), "error.xbm", npix, tmpw, tmph, 0, 0);
    }
#endif

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  218,  667, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  212,  109, 0,0,0, /* question */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   60,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   80,   14,  121,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   46,  212,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   58,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   74,   58,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  138,   58,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  115,  212,  100, 0,0,0, /* message */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  192,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   37,  212,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   49,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   74,   49,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  138,   49,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  218,  212,  107, 0,0,0, /* error */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   75,   24, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   95,   13,  106,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   44,  212,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   56,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   74,   56,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  138,   56,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  328,  212,  110, 0,0,0, /* work */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   85,   27, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,  105,   15,   96,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   47,  212,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   59,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   74,   59,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  138,   59,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  441,  212,  111, 0,0,0, /* information */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   32,   28, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   52,   15,  149,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   48,  212,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   60,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   74,   60,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  138,   60,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  555,  212,  109, 0,0,0, /* warning */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   40,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   60,   14,  141,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   46,  212,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   58,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   74,   58,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  138,   58,   64,   41, 0,0,0, /* Help */
};

/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

	exit (0);
}
