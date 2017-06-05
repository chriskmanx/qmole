/* $Header: /cvsroot/lesstif/lesstif/test/Xm/frame/test10.c,v 1.1 2002/01/12 15:02:45 amai Exp $ */

/* Try to track down a problem with NEdit: 
   Startup size is completly messed up.
   Not finished yet, obviously ... */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>

int
main(int argc, char **argv)
{
  Widget toplevel, frame, text;
  XtAppContext app;
  int rows=60;
  int cols=30;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Frame", NULL, 0, &argc, argv, NULL, NULL);

  frame = XtVaCreateWidget("frame", xmFrameWidgetClass, toplevel,
				NULL);

  text = XtVaCreateManagedWidget("text", xmTextWidgetClass, frame,
#if 1
                          XmNrows, rows, XmNcolumns, cols,
#endif
				  NULL);

  XtManageChild(frame);
  
  XtRealizeWidget(toplevel);
  
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  506,  322,  144,   68, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,   11,    0,   64,   17, 0,0,0, /* OuterLabel */
   CWWidth | CWHeight | CWX | CWY,    1,   17,  142,   50, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,    2,   17,  138,   31, 0,0,0, /* three */
   CWWidth | CWHeight | CWX | CWY,   12,    0,   64,   17, 0,0,0, /* InnerLabel */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}

  LessTifTestMainLoop(toplevel);

  exit(0);
}
