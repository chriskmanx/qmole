/* 
 $Header: /cvsroot/lesstif/lesstif/test/Xm/mainw/test9.c,v 1.5 2002/05/03 12:03:41 amai Exp $
============
!Resource file: Test-prog
*XmMainWindow.width: 850
*XmMainWindow.height: 330
============
 */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/RepType.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>

#include "../../common/Test.h"

static Widget top;
static XtAppContext app_context;
static Widget main_window;
static Widget menubar;


int
main(int argc, char *argv[]) {
  Widget rc, frame1, label;

  XtSetLanguageProc(NULL, NULL, NULL);
  
  top = XtVaAppInitialize(&app_context,
                          "Test-prog",
                          NULL, 0,
                          &argc, argv,
                          NULL,
                          NULL);
  
  main_window = 
    XtVaCreateManagedWidget("mainwin", xmMainWindowWidgetClass, top, NULL);
  
  XmRepTypeInstallTearOffModelConverter();
  
  menubar = XmCreateMenuBar(main_window, "menubar", NULL, 0);
  XtManageChild(menubar);

  XmMainWindowSetAreas(main_window,menubar,NULL,NULL,NULL,NULL);

  rc = XtVaCreateWidget("rc", xmRowColumnWidgetClass, main_window, NULL);

  XtVaSetValues(main_window, XmNworkWindow, rc, NULL);
  XtManageChild(rc);

  frame1 = XtVaCreateWidget("frame1",
                                   xmFrameWidgetClass,
                                   rc,
                                   NULL);
  XtManageChild(frame1);

  XtRealizeWidget(top);
  
  label = XtVaCreateManagedWidget("Rampaging Bulldozers",
                                         xmLabelWidgetClass,
                                         frame1,
                                         NULL);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,   16,   32, 0,0,0, /* mainwin */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   16,   16, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    0,   16,   16,   16, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   10,    4, 0,0,0, /* frame1 */
   CWWidth | CWHeight | CWX | CWY,    2,    2,    6,    1, 0,0,0, /* Rampaging Bulldozers */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(top, Expected);
}
  LessTifTestMainLoop(top);

  exit(0);
}
