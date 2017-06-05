/*
 *  when a the mouse is moved into the drawing area, keystrokes should
 *  be seen; when in the menu area, the shouldn't be seen.
 */

#include <Xm/RowColumn.h>
#include <Xm/RowColumnP.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>
#include <stdio.h>

Widget rowcol, canvas;

void
cb(Widget w, XtPointer data, XtPointer cbs) {
}

static void
process_key(Widget w, XtPointer p, XEvent *ev, Boolean *b)
{
   printf("process_key\n");
}

int
main(int argc, char **argv)
{
  Widget toplevel;
  XtAppContext app;
  XmString button1 = XmStringCreateLocalized("button number 1");
  XmString button2 = XmStringCreateLocalized("button number 2");
  XmString button3 = XmStringCreateLocalized("button number 3");

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "RowColumn", NULL, 0, &argc, argv,
NULL, NULL);

  canvas = XtVaCreateManagedWidget(
            "canvas", xmDrawingAreaWidgetClass, toplevel,
            XmNwidth, 100,
            XmNheight, 200,
            NULL);

  rowcol = XmVaCreateSimpleMenuBar(canvas, "radioBox",
                                   XmVaCASCADEBUTTON, button1, 'b',
                                   XmVaCASCADEBUTTON, button2, 'u',
                                   XmVaCASCADEBUTTON, button3, 't',
                                   XmNbuttonCount, 8,
                                   NULL);

  XmVaCreateSimplePulldownMenu(rowcol, "b_menu", 0, cb,
                               XmVaPUSHBUTTON, button1, 'b', NULL, NULL,
                               NULL);

  XtAddEventHandler(canvas, KeyPressMask | KeyReleaseMask, 0,
process_key, NULL);

  XmStringFree(button1);
  XmStringFree(button2);
  XmStringFree(button3);

  XtManageChild(rowcol);

  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	100,	200,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	10,	10,	328,	31,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	5,	5,	106,	21,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	111,	5,	106,	21,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	217,	5,	106,	21,	0,0,0,
};

  PrintDetails(toplevel, Expected);
  }
    LessTifTestMainLoop(toplevel);
    /*
  XtAppMainLoop(app);
  */

  exit(0);
}

