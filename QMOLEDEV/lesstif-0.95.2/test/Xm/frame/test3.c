/* $Header: /cvsroot/lesstif/lesstif/test/Xm/frame/test3.c,v 1.7 2001/05/15 13:29:23 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/PushB.h>
#include <Xm/FrameP.h>
#ifdef LESSTIF_VERSION
#include <XmI/MacrosI.h>
#endif
Widget toplevel,
       canvas, 
       canvas_frame;


int main(int argc, 
    char *argv[])
{
    Arg wargs[100];   
    int n = 0;        

    toplevel = XtInitialize(argv[0], "test3", NULL, 0, &argc, argv); 

    n = 0;
    XtSetArg(wargs[n], XtNwidth, 500); n++;
    XtSetArg(wargs[n], XtNheight, 500); n++;
    XtSetArg(wargs[n], XtNy, 100); n++;
    XtSetArg(wargs[n], XtNx, 200); n++;

    canvas_frame = XtCreateManagedWidget("canvas_frame", 
					 xmFrameWidgetClass, toplevel, 
					 wargs, n);

    canvas = XtCreateManagedWidget("canvas", xmPushButtonWidgetClass,
				   canvas_frame, NULL, 0);

    XtRealizeWidget(toplevel);
#ifdef LESSTIF_VERSION
    printf("(IGNORE_IN_DIFF) PC: %d\n", Frame_ProcessingConstraints(canvas_frame));
#endif  
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  506,  322,  500,  500, 0,0,0, /* canvas_frame */
   CWWidth | CWHeight | CWX | CWY,    1,    1,  498,  498, 0,0,0, /* canvas */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}

  LessTifTestMainLoop(toplevel);

    exit(0);
}


