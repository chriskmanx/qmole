/* $Header: /cvsroot/lesstif/lesstif/test/Xm/frame/test8.c,v 1.6 2002/01/12 15:02:45 amai Exp $ */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>

#include "../../common/Test.h"

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
/* toplevel should be replaced with to correct applicationShell */

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  366,  402,   92,   48, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,   11,    0,   34,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    1,   17,   90,   30, 0,0,0, /* WorkArea */

   CWWidth | CWHeight            ,  366,  402,   92,   48, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,   47,    0,   34,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    1,   17,   90,   30, 0,0,0, /* WorkArea */

   CWWidth | CWHeight            ,  366,  402,  144,   76, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,   99,    0,   34,   17, 0,0,0, /* Title */
   CWWidth | CWHeight | CWX | CWY,    1,   17,  142,   58, 0,0,0, /* WorkArea */
};

    PrintDetails(toplevel,Expected);
      LessTifTestWaitForIt(toplevel);
      XtVaSetValues(two,
	  XmNchildHorizontalAlignment, XmALIGNMENT_END,
	  NULL);

    PrintDetails(toplevel,Expected);
      LessTifTestWaitForIt(toplevel);
      LessTifTestResizeWidget(toplevel, 144, 76);

    PrintDetails(toplevel,Expected);
}

  LessTifTestMainLoop(toplevel);

  exit(0);
}
