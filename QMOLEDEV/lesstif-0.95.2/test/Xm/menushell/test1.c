/* test to see if menu shell's get realized in their initialize
   proc */

#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <stdio.h>

Widget toplevel;
Widget menuShell;

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

  printf ("XtWindow(menuShell) = %08x\n", (unsigned int)XtWindow(menuShell));

  menuShell = XtVaCreateWidget("menuShell1", 
                               xmMenuShellWidgetClass,
                               toplevel,
                               XmNallowShellResize, False,
                               NULL);

  printf ("XtWindow(menuShell) = %08x\n", (unsigned int)XtWindow(menuShell));

  return 0;
}
