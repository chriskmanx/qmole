#include <stdio.h>
#include <stdlib.h>

#include <Xm/BulletinB.h>
#include <Xm/GrabShell.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>

#include "../../common/Test.h"

/*
This tests synchronous grabs and owner events. 
*/

Widget grabshell;

void
check_geometry()
{
  static int result_index = 0;

  static XtWidgetGeometry Expected[] =
  {
/* result test 0 */
   CWWidth | CWHeight            ,    0,    0,   70,   17, 0,0,0, /* Hello World */
   CWWidth | CWHeight | CWX | CWY,    0,  100,   42,   25, 0,0,0, /* hello */
/* result test 1 */
   CWWidth | CWHeight            ,    2,    2,   70,   17, 0,0,0, /* Hello World */
   CWWidth | CWHeight | CWX | CWY,    0,  100,   42,   25, 0,0,0, /* hello */
};

  if (result_index <= 1)
  {
     /* Only have results for two test.  Must record more results before
        doing any further testing otherwise you start looking at bogus
        memory locations.
      */
#if 0
     PrintDetails(grabshell, NULL);
#endif
     PrintDetails(grabshell, Expected);
     result_index ++;
  }
}

void
onPopup(Widget w, XtPointer mydata, XtPointer cbs)
{
  check_geometry();
}

void onActivate(Widget w, XtPointer mydata, XtPointer cbs)
{
  Widget grabshell = (Widget) mydata;

  XtPopupSpringLoaded(grabshell);

#if 0
  XdbPrintCompleteTree(grabshell);
#endif
}

void GrabShellResources(Widget w)
{
  int grabStyle = -1;
  Boolean ownerEvents, saveUnder, overrideRedirect;
  Dimension shadowThickness;

  XtVaGetValues(w,
                XmNgrabStyle, &grabStyle, 
                XtNoverrideRedirect, &overrideRedirect,
                XmNownerEvents, &ownerEvents,
                XtNsaveUnder, &saveUnder,
                XmNshadowThickness, &shadowThickness,
                NULL);


  fprintf(stderr,"BEGIN_RESOURCE_DUMP\n");
  fprintf(stderr,"GrabStyle (%d)\n",grabStyle);
  fprintf(stderr,"OverrideRedirect (%d)\n",(int) overrideRedirect);
  fprintf(stderr,"OwnerEvents (%d)\n",(int) ownerEvents);
  fprintf(stderr,"SaveUnder (%d)\n",(int) saveUnder);
  fprintf(stderr,"ShadowThickness (%d)\n",(int) shadowThickness);
  fprintf(stderr,"END_RESOURCE_DUMP\n");
}

int
main(int argc, char **argv)
{
  int i;
  Widget toplevel, bb, label, pressme, pb;
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
  XtSetArg(args[i], XmNownerEvents, True); i++;
  XtSetArg(args[i], XmNgrabStyle, GrabModeSync); i++;
  grabshell = XmCreateGrabShell(bb, "grab", args, i);
  label = XmCreateLabel(grabshell, "Hello World", NULL, 0);

  XtManageChild(label);

  i = 0;
  XtSetArg(args[i], XmNy, 100); i++;
  XtSetArg(args[i], XmNx, 0); i++;
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

#if 1
  GrabShellResources(grabshell);
#endif

  check_geometry();

  LessTifTestMainLoop(toplevel);

  exit(0);
}

