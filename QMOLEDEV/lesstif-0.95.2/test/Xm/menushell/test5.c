/* test to see if you can have a child that's not a row column. */
#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/PushB.h>
#include <stdio.h>

Widget toplevel;
Widget menuShell, notmenu;

XtAppContext theApp;

int
main(int argc,
     char **argv)
{
  toplevel = XtAppInitialize(&theApp, "menushell1",
                             NULL, 0, &argc, argv, NULL, NULL, 0);

  menuShell = XtVaCreateWidget("menuShell1",
                               xmMenuShellWidgetClass,
                               toplevel,
                               XmNwidth, 10, XmNheight, 10,
                               NULL);

  notmenu = XtVaCreateManagedWidget("menu",
				    xmPushButtonWidgetClass,
				    menuShell,
				    NULL);

  return 0;
}
