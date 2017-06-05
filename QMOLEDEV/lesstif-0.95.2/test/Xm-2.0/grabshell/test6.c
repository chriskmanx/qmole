#include <stdio.h>
#include <stdlib.h>

#include <Xm/XmP.h>
#include <Xm/BulletinB.h>
#include <Xm/GrabShell.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"


/*
This tests resizes of the grab shell and the effect on its child.
*/

Widget grabshell;

void
check_geometry()
{
  static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
{  CWWidth | CWHeight            ,    0,    0,    0,    0, 0,0,0 }, /* grab */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,   42,   25, 0,0,0 }, /* hello */
/* result test 1 */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,   46,   29, 0,0,0 }, /* grab */
{  CWWidth | CWHeight | CWX | CWY,    2,    2,   42,   25, 0,0,0 }, /* hello */
/* result test 2 */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,   92,   58, 0,0,0 }, /* grab */
{  CWWidth | CWHeight | CWX | CWY,    2,    2,   88,   54, 0,0,0 }, /* hello */
/* result test 3 */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  184,  116, 0,0,0 }, /* grab */
{  CWWidth | CWHeight | CWX | CWY,    2,    2,  180,  112, 0,0,0 }, /* hello */
/* result test 4 */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  368,  232, 0,0,0 }, /* grab */
{  CWWidth | CWHeight | CWX | CWY,    2,    2,  364,  228, 0,0,0 }, /* hello */
}; 

#if 0
  PrintDetails2(grabshell, NULL);
#else
  if (result_index <= 4)
  {
     PrintDetails2(grabshell, Expected);
     fflush(stdout);
     result_index ++;
  }
#endif
}

void
onPopup(Widget w, XtPointer mydata, XtPointer cbs)
{
  check_geometry();
}

void onActivate(Widget w, XtPointer mydata, XtPointer cbs)
{
  static int time_to_grow;

  Widget grabshell = (Widget) mydata;

  if(!time_to_grow)
  {
     /* next time we will grow */
     time_to_grow++;
  }
  else
  {
     /* time to grow */
     XmeConfigureObject(grabshell,
                        XtX(grabshell),
                        XtY(grabshell),
                        XtWidth(grabshell)*2,
                        XtHeight(grabshell)*2,
                        XtBorderWidth(grabshell));
  }

  XtPopupSpringLoaded(grabshell);

}

int
main(int argc, char **argv)
{
  int i;
  Widget toplevel, bb, pressme, pb;
  XtAppContext app;
  XmString item;
  Arg args[5];

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "GrabShell", NULL, 0, &argc, argv, NULL, NULL);

  bb = XmCreateBulletinBoard(toplevel, "bb", NULL, 0);
  
  pressme = XmCreatePushButton(bb,"Press me and let's see what the grab shell does.",NULL,0);
  
  XtManageChild(pressme);

  XtManageChild(bb);

  i = 0;
  grabshell = XmCreateGrabShell(bb, "grab", args, i);

  i = 0;
  pb = XmCreatePushButton(grabshell,"hello", args, i);
  XtManageChild(pb);

  /* setup callback for an activate action */
  XtAddCallback(pressme, 
                XmNactivateCallback,
                (XtCallbackProc) onActivate, 
                (XtPointer) grabshell);

  XtAddCallback(grabshell,
                XmNpopupCallback,
                (XtCallbackProc) onPopup,
                (XtPointer) grabshell);

  XtRealizeWidget(toplevel);

  check_geometry();

  LessTifTestMainLoop(toplevel);

  exit(0);
}

