/* test for sharing a menu shell across two pulldowns. */
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/RowColumnP.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <stdio.h>

Widget toplevel, cascade, cascade2;
Widget rc, menuShell, menu, menu2;
Widget pushb1, pushb2;

XtAppContext theApp;

int
main(int argc,
     char **argv)
{
  toplevel = XtAppInitialize(&theApp, "menushell1",
                             NULL, 0, &argc, argv, NULL, NULL, 0);

  rc = XmCreateMenuBar(toplevel,
		       "menubar",
		       NULL, 0);
  

  menu = XmCreatePulldownMenu(rc,
			      "menu",
			      NULL, 0);

  pushb1 = XtVaCreateManagedWidget("pushb1",
				   xmPushButtonWidgetClass,
				   menu,
				   NULL);

  menu2 = XtVaCreateManagedWidget("menu2",
                                  xmRowColumnWidgetClass,
				  XtParent(menu),
				  XmNrowColumnType, XmMENU_PULLDOWN,
                                  NULL);

  pushb2 = XtVaCreateManagedWidget("pushb2",
				   xmPushButtonWidgetClass,
				   menu2,
				   NULL);

  cascade = XtVaCreateManagedWidget("cascade1",
				    xmCascadeButtonWidgetClass,
				    rc,
				    XmNsubMenuId, menu,
				    NULL);
  
  cascade2 = XtVaCreateManagedWidget("cascade2",
				     xmCascadeButtonWidgetClass,
				     rc,
				     XmNsubMenuId, menu2,
				     NULL);

  XtManageChild(rc);
  XtManageChild(menu);

  XtRealizeWidget(toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  138,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   64,   21, 0,0,0, /* cascade1 */
   CWWidth | CWHeight | CWX | CWY,   69,    5,   64,   21, 0,0,0, /* cascade2 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

  exit(0);
}
