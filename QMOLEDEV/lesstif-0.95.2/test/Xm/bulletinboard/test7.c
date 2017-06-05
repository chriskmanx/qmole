#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>

Widget toplevel,
       bb,     
       canvas, 
       canvas_frame;


int
main(int argc, char **argv)
{
    Arg wargs[100];   
    int n = 0;        

    toplevel = XtInitialize(argv[0], "test7", NULL, 0, &argc, argv); 

    bb = XtCreateManagedWidget("board", xmBulletinBoardWidgetClass,
			       toplevel, NULL, 0 );

    n = 0;
    XtSetArg(wargs[n], XtNwidth, 500); n++;
    XtSetArg(wargs[n], XtNheight, 500); n++;
    XtSetArg(wargs[n], XtNy, 100); n++;
    XtSetArg(wargs[n], XtNx, 200); n++;
    canvas = XtCreateManagedWidget("canvas", xmPushButtonWidgetClass,
				   bb, wargs,n);

    XtRealizeWidget(toplevel);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  711,  611, 0,0,0, /* board */
   CWWidth | CWHeight | CWX | CWY,  200,  100,  500,  500, 0,0,0, /* canvas */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}


