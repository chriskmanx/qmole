/*  $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test10.c,v 1.7 2002/05/01 15:39:21 amai Exp $
 * Mostly a duplicate of test4.  I've added carriage returns to the
 * strings.  Notice the different result from XmStringCreateLtoR and
 * XmStringCreateSimple, particularly the little yen symbol instead of a
 * carriage return.  This apparently is correct behavior, as it's the same
 * under motif and lesstif.
 * 
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
  Widget toplevel, rc, w;
  XtAppContext app;
  Arg	al[10];
  int	ac;
  XmString	xms;

  XtSetLanguageProc(NULL, NULL, NULL);
  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel, NULL);

  ac = 0;
  xms = XmStringCreateLtoR("This came out of XmStringCreateLtoR\n    at initialize", XmSTRING_DEFAULT_CHARSET);
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  XmStringFree(xms);

  ac = 0;
  xms = XmStringCreateSimple("This came out of XmStringCreateSimple\n    at initialize");
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  XmStringFree(xms);

  ac = 0;
  xms = XmStringCreateLocalized("This came out of XmStringCreateLocalized\n    at initialize");
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  XmStringFree(xms);

  ac = 0;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  xms = XmStringCreateLtoR("This came out of XmStringCreateLtoR\n    with setvalues", XmSTRING_DEFAULT_CHARSET);
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  XtSetValues(w, al, ac);
  XmStringFree(xms);

  ac = 0;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  xms = XmStringCreateSimple("This came out of XmStringCreateSimple\n    with setvalues");
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  XtSetValues(w, al, ac);
  XmStringFree(xms);

  XtRealizeWidget(toplevel);
  

{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  191,  329,  346,  142, 0,0,0, /* rc */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,  340,   30, 0,0,0, /* label */},
   {CWWidth | CWHeight | CWX | CWY,    3,   36,  340,   17, 0,0,0, /* label */},
   {CWWidth | CWHeight | CWX | CWY,    3,   56,  340,   30, 0,0,0, /* label */},
   {CWWidth | CWHeight | CWX | CWY,    3,   89,  340,   30, 0,0,0, /* label */},
   {CWWidth | CWHeight | CWX | CWY,    3,  122,  340,   17, 0,0,0, /* label */},
};
#else
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  358,  129, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  352,   30, 0,0,0, /* label */
   CWWidth | CWHeight | CWX | CWY,    3,   36,  352,   17, 0,0,0, /* label */
   CWWidth | CWHeight | CWX | CWY,    3,   56,  352,   17, 0,0,0, /* label */
   CWWidth | CWHeight | CWX | CWY,    3,   76,  352,   30, 0,0,0, /* label */
   CWWidth | CWHeight | CWX | CWY,    3,  109,  352,   17, 0,0,0, /* label */
};
#endif
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
 
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
