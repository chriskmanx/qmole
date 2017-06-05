/* $Header: /cvsroot/lesstif/lesstif/test/Xm/vendor/test5.c,v 1.2 2001/03/18 11:28:34 rwscott Exp $
   test for correct linking order.
   Should be linked incorrectly: -lXt -lXm */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/MwmUtil.h>

void
error_handler(String msg)
{
    fprintf(stderr, "%s\n", msg);
    return;
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Boolean brc=True;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", 
                               NULL, 0, 
                               &argc, argv, NULL, 
                               XmNmwmDecorations, MWM_DECOR_BORDER | MWM_DECOR_TITLE,
                               NULL);

#ifdef LESSTIF_VERSION
  XtAppSetErrorHandler(app, (XtErrorHandler)error_handler);
  /* This test has to be in front of any further "activities */
  brc=_LtCheckClassOfVendorShell(toplevel);
#endif

  one = XtVaCreateManagedWidget("One", xmLabelWidgetClass, 
                                toplevel, NULL);

  XtRealizeWidget(toplevel);

  exit(brc ? 1 : 0);

}
