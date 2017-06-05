/** test26 -- an option menu.
**/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/MessageB.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/ArrowB.h>
#include <Xm/ToggleB.h>
#include <stdio.h>

char *AccountTypes[] =
{"Type_1",
 "Type_2",
 "Type_3",
 "Type_4",
 "Type_5",
 NULL};

static Widget 
CreateAccountDialog (Widget W)
{
  Widget AccountDialog;
  Widget Type;
  Widget PB;
  char **type;
  Widget Pulldown;

  AccountDialog = XmCreateMessageBox (W, "AccountDialog", NULL, 0);

  Pulldown = XmCreatePulldownMenu (AccountDialog, "Pulldown", NULL, 0);
  type = AccountTypes;
  while (*type != NULL)
  {

    PB = XmCreatePushButton (Pulldown, *type, NULL, 0);
    XtManageChild (PB);
    type++;
  }
  Type = XmCreateOptionMenu (AccountDialog, "Type", NULL, 0);
  XtVaSetValues (Type,
		 XmNsubMenuId, Pulldown,
		 NULL);
  XtManageChild (Type);

  PB = XmCreatePushButton (AccountDialog, "Apply", NULL, 0);
  XtVaSetValues (AccountDialog,
#if 1
/* add this and you get the cache problem??? */
		 XmNdefaultButton, PB,
#endif
		 NULL);
  XtManageChild (PB);
  return (AccountDialog);
}

int
main (int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel, rc;

  toplevel = XtVaAppInitialize (&theApp, "rc-test7", NULL, 0,
				&argc, argv, NULL, NULL);

  rc = CreateAccountDialog (toplevel);
  XtManageChild (rc);

  XtRealizeWidget (toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	278,	130,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	0,	0,	4,	4,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	11,	11,	256,	4,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	0,	66,	278,	2,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	11,	78,	64,	41,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	139,	78,	64,	41,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	203,	78,	64,	41,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	11,	25,	256,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	4,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	10,	3,	73,	25,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	75,	78,	64,	41,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
  XtAppMainLoop (theApp);
  */
  exit (0);
}
