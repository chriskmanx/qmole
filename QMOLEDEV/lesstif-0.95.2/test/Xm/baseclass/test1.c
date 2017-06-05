/* $Header: /cvsroot/lesstif/lesstif/test/Xm/baseclass/test1.c,v 1.3 2001/06/11 08:26:30 amai Exp $ 
 * location detection of Motif routines 
 */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/BaseClassP.h>

#include "../../common/Test.h"

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  /* Motif will dump core if this is after the initialize,
   * but if you put this first, you're ok */
  /* MLM so will lesstif, now:) */
  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  _XmInitializeExtensions();

  printf("XmQmotif: %s\n", XrmQuarkToString(XmQmotif));

  XtRealizeWidget(toplevel);
  
  /* NO GEOMETRY */
  LessTifTestMainLoop(  toplevel );

  exit(0);
}
