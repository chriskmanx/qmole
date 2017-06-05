/* $Id: test17.c,v 1.3 2000/09/27 21:48:31 amai Exp $ */

/* test16+test17: Test set for XmNfileFilterStyle resource */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/FileSB.h>

#define OK        1
#define CANCEL    2

XtAppContext context;

Widget toplevel;
Widget dialog;

void 
dialogCB (Widget w, int client_data, XmFileSelectionBoxCallbackStruct *call_data)
{
  exit(0);
}


int
main (int argc, char *argv[])
{
  Arg al[10];
  int ac;

  toplevel = XtAppInitialize (&context, "", NULL, 0,
			      &argc, argv, NULL, NULL, 0);

  ac = 0;
#if XmVERSION > 1
  fprintf(stdout, "Using XmNfileFilterStyle = XmFILTER_NONE\n");
  XtSetArg(al[ac], XmNfileFilterStyle, XmFILTER_NONE);
  ac++;
#endif
  dialog = XmCreateFileSelectionDialog (toplevel, "dialog", al, ac);
  XtAddCallback (dialog, XmNokCallback, (XtCallbackProc)dialogCB, NULL);
  XtAddCallback (dialog, XmNcancelCallback,  (XtCallbackProc)dialogCB, NULL);
  XtManageChild (dialog);

  XtRealizeWidget (toplevel);
  LessTifTestMainLoop (toplevel);
  exit(0);
}
