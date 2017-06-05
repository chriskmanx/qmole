#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DrawnB.h>

void HiCB(Widget w,XtPointer client_data,XtPointer call_data);

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt;

  toplevel = XtVaAppInitialize(&theApp, "drawn", NULL, 0,
                              &argc, argv, NULL, NULL);

  butt= XtVaCreateManagedWidget("Button1", xmDrawnButtonWidgetClass, toplevel, 
                               XmNwidth, 100,
                               XmNheight, 100,
                               NULL);

  XtAddCallback(butt,XmNarmCallback,HiCB,NULL);
  XtAddCallback(butt,XmNdisarmCallback,HiCB,NULL);
  XtAddCallback(butt,XmNactivateCallback,HiCB,NULL);
  XtAddCallback(butt,XmNexposeCallback,HiCB,NULL);
  XtAddCallback(butt,XmNresizeCallback,HiCB,NULL);

  XtRealizeWidget(toplevel);
  
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  100,  100, 0,0,0, /* Button1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}

void draw(Widget w, Window win, Dimension width, Dimension height, Pixel color)
{
       GC drawGC;

       drawGC = XtAllocateGC(w,
                             0,0,
                             NULL,
                             0,0);

       XSetForeground(XtDisplayOfObject(w),
                      drawGC,
                      color);

       /* NOTE: the skip is per Motif -- no effort is made to avoid the
          shadows */
       XDrawRectangle(XtDisplayOfObject(w), win, drawGC,
                      10, 10, width - 20, height - 20);

       XtReleaseGC(w, drawGC);
}

void HiCB(Widget w,XtPointer client_data,XtPointer call_data)
{
       XmDrawnButtonCallbackStruct *cbs = (XmDrawnButtonCallbackStruct *)call_data;
       Dimension width, height;
       Pixel color;

       XtVaGetValues(w,
                     XmNwidth, &width,
                     XmNheight, &height,
                     XmNforeground, &color,
                     NULL);
       switch (cbs->reason) {
       case XmCR_ARM:
               printf("Widget armed\n");
               break;
       case XmCR_DISARM:
               printf("Widget disarmed\n");
               break;
       case XmCR_ACTIVATE:
               printf("Widget activated\n");
               break;
       case XmCR_EXPOSE:
               printf("Widget exposed\n");
               draw(w, cbs->window, width, height, color);
               break;
       case XmCR_RESIZE:
               printf("Widget resized\n");
               draw(w, cbs->window, width, height, color);
               break;
       default:
               printf("callback for unknown reason\n");
       }
}

