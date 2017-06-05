/* $Header: /cvsroot/lesstif/lesstif/test/Xm/vendor/test4.c,v 1.2 2001/10/16 10:27:48 amai Exp $
   
   Test for correct linking order.
   Should be linked correctly: -lXm -lXt 
   See FAQ and lib/Xm/Vendor.c for more details
*/

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#ifdef LESSTIF_VERSION
#include <XmI/XmI.h>
#endif


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
                               NULL);

#ifdef LESSTIF_VERSION
  /* This test has to be in front of any further "activities */
  brc=_LtCheckClassOfVendorShell(toplevel);
#endif

  one = XtVaCreateManagedWidget("One", xmLabelWidgetClass, 
                                toplevel, NULL);

  XtRealizeWidget(toplevel);

  exit( (int)!brc);

}
