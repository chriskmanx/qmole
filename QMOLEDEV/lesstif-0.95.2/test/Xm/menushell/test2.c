/* test to see if menu shell's get realized in their initialize
   proc */

#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/RowColumnP.h>
#include <stdio.h>

Widget toplevel;
Widget menuShell,popup;

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

  popup = XmCreatePopupMenu(toplevel, "popup",
                            NULL, 0);

  printf ("XtWindow(popup) = %08x; Geometry = %dx%d\n", 
          (unsigned int)XtWindow(popup),
          XtWidth(popup), XtHeight(popup));

  return 0;
}
