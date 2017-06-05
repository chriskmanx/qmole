/* $Id: test11.c,v 1.1 2000/11/24 13:46:44 amai Exp $ */
/* amai:
   XmNap versions <= 0.1.19 crash due xms being NULL.
   I didn't check any further, just assembled this test 
   which is hopefully equivalent to the problem with XmNAp ... */


#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/SelectioB.h>


int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box;
  Arg a[10];
  int n;
  XmString xms;
  String tmp;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  n = 0;
  box = XmCreateSelectionBox(toplevel, "Box", a, n);

  XtManageChild(box);

  XtRealizeWidget(toplevel);
  
  XtVaGetValues(box, XmNapplyLabelString, &xms, NULL);

  if (xms) {
    /* XmStringGetLtoR(xms, XmFONTLIST_DEFAULT_TAG, &tmp);
    fprintf(stdout, "xms!=%s\n", xms); */
    fprintf(stdout, "xms!=NULL\n");
    exit(0);
  }
  else {
    fprintf(stdout, "xms=NULL\n");
    exit(1);
  }
}
