/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test3.c,v 1.6 2001/05/15 14:08:34 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

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
	unsigned char pack;
	short cols;

	toplevel = XtVaAppInitialize (&appc, "Boxes", NULL, 0,
	&argc, argv, NULL, NULL);

	rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel,
		XmNnumColumns,	2,
	    NULL);
	tmp = XtVaCreateManagedWidget("question", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "question", sizeof(char *),
		XmNdialogType,	XmDIALOG_QUESTION,
		XmNwidth, 200, XmNheight, 100,
	    NULL);

	tmp = XtVaCreateManagedWidget("message", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "message", sizeof(char *),
		XmNdialogType,	XmDIALOG_MESSAGE,
		XmNwidth, 200, XmNheight, 100,
	    NULL);
	tmp = XtVaCreateManagedWidget("error", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "error", sizeof(char *),
		XmNdialogType,	XmDIALOG_ERROR,
		XmNwidth, 200, XmNheight, 100,
	    NULL);
	tmp = XtVaCreateManagedWidget("work", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "work", sizeof(char *),
		XmNdialogType,	XmDIALOG_WORKING,
		XmNwidth, 200, XmNheight, 100,
	    NULL);
	tmp = XtVaCreateManagedWidget("information", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "information", sizeof(char *),
		XmNdialogType,	XmDIALOG_INFORMATION,
		XmNwidth, 200, XmNheight, 100,
	    NULL);
	tmpe = XtVaCreateManagedWidget("warning", xmMessageBoxWidgetClass, rc,
		XtVaTypedArg, XmNmessageString, XmRString, "warning", sizeof(char *),
		XmNdialogType,	XmDIALOG_WARNING,
		XmNwidth, 200, XmNheight, 100,
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
	XtVaGetValues(rc, XmNpacking, &pack, XmNnumColumns, &cols, NULL);
	printf("pack: %d cols: %d\n", pack, cols);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  206,  913, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  200,  150, 0,0,0, /* question */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   56,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   76,   14,  113,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   46,  200,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   58,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  126,   58,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   68,   99,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  156,  200,  141, 0,0,0, /* message */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  180,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   37,  200,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   49,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  126,   49,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   68,   90,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  300,  200,  148, 0,0,0, /* error */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   70,   24, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   90,   13,   99,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   44,  200,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   56,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  126,   56,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   68,   97,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  451,  200,  151, 0,0,0, /* work */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   80,   27, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,  100,   15,   89,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   47,  200,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   59,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  126,   59,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   68,  100,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  605,  200,  152, 0,0,0, /* information */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   30,   28, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   50,   15,  140,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   48,  200,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   60,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  126,   60,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   68,  101,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,    3,  760,  200,  150, 0,0,0, /* warning */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   37,   26, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   57,   14,  132,   17, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   46,  200,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   10,   58,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  126,   58,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   68,   99,   64,   41, 0,0,0, /* Help */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

	exit (0);
}
