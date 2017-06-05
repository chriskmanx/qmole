#include <stdio.h>
#include <Xm/XmP.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <stdlib.h>

static char *FallBack[] = {
		"*.geometrySlop: 1",
		NULL
};

int
main(int argc, char **argv)
{
    Widget toplevel, one;
    XtAppContext app;
    int num_tickmarks;
    int i;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "Scale", NULL, 0, &argc, argv, FallBack, NULL);

    if (argc > 1)
	num_tickmarks = atoi(argv[1]);
    else
	num_tickmarks = 10;

    one = XtVaCreateManagedWidget("sc", xmScaleWidgetClass, toplevel,
				  NULL);

    for (i = 0; i < num_tickmarks; i++) {
	Widget l;

	l = XtVaCreateManagedWidget("-", xmLabelWidgetClass, one,
				    NULL);
    }

    XtRealizeWidget(toplevel);

    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,   29,  192, 0,0,0, /* sc */
   CWWidth | CWHeight | CWX | CWY,   10,    0,   19,  192, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,    0,   11,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,   28,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,   45,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,   62,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,   79,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,   96,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,  113,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,  130,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,  147,   10,   17, 0,0,0, /* - */
   CWWidth | CWHeight | CWX | CWY,    0,  164,   10,   17, 0,0,0, /* - */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );

    exit(0);
}
