/* $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test20.c,v 1.1 2001/06/18 13:55:02 amai Exp $ */

/* picked up from Usenet:

   On Linux (Lesstif 0.92 - current version) the program exits, but on
   Irix 6.5 it works fine.  Am I missing something simple here?
*/


#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/FileSB.h>

#include "../../common/Test.h"


int
main (int argc, char *argv[])
{
  Arg al[1];
  int ac;
  XtAppContext context;
  Widget toplevel, dialog, form, work;

  toplevel = XtAppInitialize (&context, "", NULL, 0,
			      &argc, argv, NULL, NULL, 0);


  dialog = XmCreateFileSelectionDialog (toplevel, "filesb", NULL, 0);

  work=XmFileSelectionBoxGetChild(dialog, XmDIALOG_WORK_AREA);
#if 1
  /* try to add one "manually": */
  if (!work)
    form=XtVaCreateManagedWidget("form", xmFormWidgetClass, dialog,
                                 NULL);

  work=XmFileSelectionBoxGetChild(dialog, XmDIALOG_WORK_AREA);
#endif
  
  if (!work) {
     printf("work==NULL\n");
     exit(1);
  } else  if (work!=form) {
     printf("form != work\n");
     exit(2);
  } else {
     /* Motif 1.2 does so */
     printf("form == work\n");  
#if 0
     XtManageChild(dialog);
     XtRealizeWidget (toplevel);
     LessTifTestMainLoop (toplevel);
#endif
     exit(0);
  }
}
