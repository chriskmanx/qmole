/**
 *
 * test if mwm is running
 *
 **/

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <stdio.h>

int
main(int argc, 
     char **argv)
{
    Widget toplevel, label;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Mwm1", NULL, 0, &argc, argv, NULL, NULL);

    label = XmCreateLabel(toplevel, 
			  "label",
			  NULL, 0);

    XtManageChild(label);

    XtRealizeWidget(toplevel);

    if (XmIsMotifWMRunning(toplevel))
	printf ("mwm is running\n");
    else
	printf ("mwm is not running\n");

    exit(0);
}
