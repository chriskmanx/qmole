/* $Header: /cvsroot/lesstif/lesstif/test/Xm/frame/test7.c,v 1.3 2002/01/12 15:02:45 amai Exp $ */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one, two, three, four, five;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Frame", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, toplevel,
				NULL);

  two = XtVaCreateManagedWidget("Title", xmLabelWidgetClass, one,
				XmNchildType, XmFRAME_TITLE_CHILD,
				XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                                XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
                                XmNwidth, 90,
                                XmNheight, 30,
                                XmNrecomputeSize, False,
				NULL);

  XtVaCreateManagedWidget("WorkArea", xmLabelWidgetClass, one,
                                XmNwidth, 90,
                                XmNheight, 30,
                                XmNrecomputeSize, False,
				NULL);

  XtRealizeWidget(toplevel);
  
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  112,   61, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,   11,    0,   90,   30, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    1,   30,  110,   30, 0,0,0, /* WorkArea */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}

  LessTifTestMainLoop(toplevel);

  exit(0);
}
