#include <Xm/Xm.h>
#include "SpinButton.h"

int
main(int argc, char **argv)
{
  Widget toplevel, widget;
  XtAppContext app;
  XmString item;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  widget = DtCreateSpinButton(toplevel, "spinb", NULL, 0);

  item = XmStringCreateSimple("Item 3");
  DtSpinButtonAddItem(widget, item, 3);
  item = XmStringCreateSimple("Item 2");
  DtSpinButtonAddItem(widget, item, 2);
  item = XmStringCreateSimple("Item 1");
  DtSpinButtonAddItem(widget, item, 1);

  XtManageChild(widget);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  150,   31, 0,0,0, /* spinb */
   CWWidth | CWHeight | CWX | CWY,   15,    3,  132,   25, 0,0,0, /* spinb_TF */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   12,   12, 0,0,0, /* spinb_Up */
   CWWidth | CWHeight | CWX | CWY,    3,   15,   12,   12, 0,0,0, /* spinb_Down */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
