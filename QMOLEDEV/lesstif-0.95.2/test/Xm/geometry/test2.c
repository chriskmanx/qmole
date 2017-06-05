#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include "Trivial.h"
#include <stdio.h>

Widget toplevel, triv, A, B, C, D;
XtAppContext theApp;

int
main(int argc,
     char **argv)
{
    toplevel = XtVaAppInitialize(&theApp, "menu", NULL, 0,
				 &argc, argv, NULL, NULL);

    triv = XtVaCreateManagedWidget("Triv",
				   xmTrivialWidgetClass,
				   toplevel, NULL);

    A = XtVaCreateManagedWidget("A",
				xmPushButtonWidgetClass,
				triv,
				NULL);

    B = XtVaCreateManagedWidget("B",
				xmPushButtonWidgetClass,
				triv,
				NULL);

    C = XtVaCreateManagedWidget("C",
				xmPushButtonWidgetClass,
				triv,
				NULL);

    D = XtVaCreateManagedWidget("D",
				xmPushButtonWidgetClass,
				triv,
				NULL);

    XtRealizeWidget(toplevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   94,   47, 0,0,0, /* Triv */
   CWWidth | CWHeight | CWX | CWY,   11,   11,   18,   25, 0,0,0, /* A */
   CWWidth | CWHeight | CWX | CWY,   29,   11,   18,   25, 0,0,0, /* B */
   CWWidth | CWHeight | CWX | CWY,   47,   11,   18,   25, 0,0,0, /* C */
   CWWidth | CWHeight | CWX | CWY,   65,   11,   18,   25, 0,0,0, /* D */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

    exit(0);
}
