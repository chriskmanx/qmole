/* $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test5.c,v 1.10 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/CascadeB.h>
#include <Xm/FileSB.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>

#include "../../common/Test.h"

#include "mkdirtree.h"

Widget GXCreateFileBox(Widget parent);

static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

Widget GXTopWidget, GXClipBox;
int main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget label;

  make_tmp_dir_tree();
  GXTopWidget= XtVaAppInitialize(&theApp, "test1", NULL, 0, &argc, argv,
				 FallBack, NULL); 
  label = XmCreateLabel(GXTopWidget, "Label", NULL, 0);
  XtManageChild(label);
  XtManageChild(GXCreateFileBox(GXTopWidget));
  XtRealizeWidget(GXTopWidget);

  
    
{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  222,  339,   34,   17, 0,0,0, /* Label */},
};
#else
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   34,   17, 0,0,0, /* Label */ 
};
#endif
    PrintDetails(GXTopWidget,Expected);
};
  LessTifTestMainLoop(GXTopWidget);

  XtAppMainLoop(theApp);
  exit(0);
}

Widget
GXCreateFileBox(Widget parent)
/*-
 * create a FileSelectionBox child of parent  
-*/
{
    Arg args[20];
    int n = 0;
    Widget filew, menu;
    Widget work;

    n = 0;
    /* XtSetArg(args[n], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL);  n++; */
    filew = XmCreateFileSelectionDialog(parent, "FileBox", args, n);
    set_path(filew);
    /* these calls are here to force the menu bar to the top, but it fails
       under lesstif */
    work = XmCreateRowColumn(filew, "FileBoxWork", NULL, 0);
    XtManageChild(work);
    work = XmCreateSeparator(filew, "Separator", NULL, 0);
    XtManageChild(work);
    XtManageChild(menu=XmCreateMenuBar(filew, "Menu Bar",NULL,0));
    XtManageChild(XmCreateCascadeButton(menu,"Button",NULL,0));
    return (filew);
}


