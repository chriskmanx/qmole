/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/menushell/test6.c,v 1.6 2001/05/15 14:08:34 amai Exp $
 * Simulate the Mosaic splash window
 */
 
#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"

Widget toplevel, push;
Widget splash, sform, label;

XtAppContext appc;

char *fallback[] = {
	"*sform.background:	red",
	"*label.background:	yellow",
	NULL
};

int
main(int argc,
     char **argv)
{
        toplevel = XtVaAppInitialize(&appc, "menushell1",
                NULL, 0, &argc, argv, fallback, NULL);

	push = XtVaCreateManagedWidget("push", xmPushButtonWidgetClass,
		toplevel, NULL);

        splash = XtVaCreatePopupShell("Hello, World!", xmMenuShellWidgetClass, toplevel,
                XmNwidth,               100,
                XmNheight,              100,
                XmNx,                   100,
                XmNy,                   100,
                XmNallowShellResize,    False,
        NULL);

        sform = XtVaCreateManagedWidget("sform", xmRowColumnWidgetClass, splash,
                XmNheight,              100,
                XmNwidth,               100,
                XmNx,                   100,
                XmNy,                   100,
        NULL);

        label = XtVaCreateManagedWidget("label", xmLabelWidgetClass, sform,
#ifdef  notdef
                XmNlabelType,   XmPIXMAP,
                XmNlabelPixmap, splashpix,
#endif
                XmNalignment,   XmALIGNMENT_CENTER,
                XmNx,           100,
                XmNy,           100,
        NULL);

        XtPopup(splash, XtGrabNone);
	XtRealizeWidget(toplevel);
	
	exit(0);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   36,   25, 0,0,0, /* push */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

}
