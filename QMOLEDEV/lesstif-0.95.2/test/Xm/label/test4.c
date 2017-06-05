/* $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test4.c,v 1.5 2001/06/18 08:33:07 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>

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
  xms = XmStringCreateLtoR("This came out of XmStringCreateLtoR at initialize", XmSTRING_DEFAULT_CHARSET);
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  XmStringFree(xms);

  ac = 0;
  xms = XmStringCreateSimple("This came out of XmStringCreateSimple at initialize");
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  XmStringFree(xms);

  ac = 0;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  xms = XmStringCreateLtoR("This came out of XmStringCreateLtoR with setvalues", XmSTRING_DEFAULT_CHARSET);
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  XtSetValues(w, al, ac);
  XmStringFree(xms);

  ac = 0;
  w = XtCreateManagedWidget("label", xmLabelWidgetClass, rc, al, ac);
  xms = XmStringCreateSimple("This came out of XmStringCreateSimple with setvalues");
  XtSetArg(al[ac], XmNlabelString, xms); ac++;
  XtSetValues(w, al, ac);
  XmStringFree(xms);

  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	322,	83,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	316,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	23,	316,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	43,	316,	17,	0,0,0,	/* two */
  	CWWidth | CWHeight | CWX | CWY,	3,	63,	316,	17,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
