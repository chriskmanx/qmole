/* $Header: /cvsroot/lesstif/lesstif/test/Xm/messagebox/test22.c,v 1.1 2001/08/28 14:32:11 amai Exp $
   SF Bug [ #456130 ] Dialog management differs from Motif

   "The Motif man pages state that the dialog should be
    shown/hidden by calling XtManageChild for the
    MessageBox widget. With LessTif it seems to be
    necessary to explicitly manage the DialogShell parent
    as well.
    The problem may be in the handling of the
    XtNmappedWhenManaged resource"
*/

#include <stdlib.h>

#include <Xm/MessageB.h>
#include <Xm/PushB.h>


static Widget toplevel;

void
openDialogCB (Widget w, XtPointer clientData, XtPointer callData)
{
  Widget dialogBox = XmCreateInformationDialog (toplevel,
						"TestDialog",
						NULL, 0);

  /* Don't map so that we can determine sizes etc.  before the                                                                                                        
     dialog is displayed. */
  XtVaSetValues (XtParent (dialogBox), XtNmappedWhenManaged, False, NULL);
  XtManageChild (dialogBox);
  /* Get some size resources from the managed MessageBox */
  XtVaSetValues (XtParent (dialogBox), XtNmappedWhenManaged, True, NULL);

  /* LessTif seems to need the parent to be managed. */
#if 0
  XtManageChild (XtParent (dialogBox));
#endif
}


int
main (int argc, char **argv)
{
  XtAppContext app;
  Widget button;

  toplevel =
    XtVaOpenApplication (&app, "Test", NULL, 0,
			 &argc, argv, NULL,
			 applicationShellWidgetClass, NULL);

  button =
    XtVaCreateManagedWidget ("Show\nDialog", xmPushButtonWidgetClass,
			     toplevel,
			     NULL);
  XtAddCallback (button, XmNactivateCallback, openDialogCB, NULL);

  XtRealizeWidget (toplevel);
  
  /* XtAppMainLoop (app); */

  LessTifTestMainLoop(toplevel);
  
  return 0;
}
