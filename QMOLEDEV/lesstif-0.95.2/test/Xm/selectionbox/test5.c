/* test of selection boxes */

#include <Xm/Xm.h>
#include <Xm/SelectioB.h>
#include <Xm/PushB.h>

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box, dummy, real;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  box = XmCreateSelectionBox(toplevel, "Box", NULL, 0);

  dummy = XtVaCreateWidget("dummy", xmPushButtonWidgetClass, box, NULL);
  real = XtVaCreateManagedWidget("Gyre", xmPushButtonWidgetClass, box, NULL);

  XtManageChild(box);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  286,  297, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  264,   17, 0,0,0, /* Items */
   CWWidth | CWHeight | CWX | CWY,   11,   28,  264,  135, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  264,  135, 0,0,0, /* ItemsList */
   CWWidth | CWHeight | CWX | CWY,   11,  173,  264,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   11,  190,  264,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,  231,  286,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  243,   66,   43, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  143,  243,   66,   43, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  209,  243,   66,   43, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   77,  243,   66,   43, 0,0,0, /* Gyre */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
