/*
 * test/Xm/filesb/test1.c - print value of FileSB resources from a callback
 * $Id: test15.c,v 1.5 2002/05/01 15:39:21 amai Exp $
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/FileSBP.h>

#include "../../common/Test.h"

#include "mkdirtree.h"


static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box;

  make_tmp_dir_tree();

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, FallBack, NULL);
    XtVaSetValues(toplevel,
    	XmNallowShellResize, True,
    	NULL);

  box=XmCreateFileSelectionBox(toplevel,"Box",NULL,0);
  set_path(box);
  {
  	XtVaSetValues(XmFileSelectionBoxGetChild(box, XmDIALOG_OK_BUTTON),
  		XmNwidth, 200,
  		XmNrecomputeSize, False,
  		NULL);
  }
  XtManageChild(box);

  XtManageChild(box);

  XtRealizeWidget(toplevel);
    
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  822,  372, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,  727,   69,   83,   17, 0,0,0, /* Items */
   CWWidth | CWHeight | CWX | CWY,  727,   86,   83,  154, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,   68,    0,   15,  135, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  139,   64,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   64,  135, 0,0,0, /* ItemsList */
   CWWidth | CWHeight | CWX | CWY,   11,  250,  800,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   11,  267,  800,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,  308,  822,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  320,  200,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,  211,  320,  200,   41, 0,0,0, /* Apply */
   CWWidth | CWHeight | CWX | CWY,  411,  320,  200,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  611,  320,  200,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  800,   17, 0,0,0, /* FilterLabel */
   CWWidth | CWHeight | CWX | CWY,   11,   69,  706,   17, 0,0,0, /* Dir */
   CWWidth | CWHeight | CWX | CWY,   11,   28,  800,   31, 0,0,0, /* FilterText */
   CWWidth | CWHeight | CWX | CWY,   11,   86,  706,  154, 0,0,0, /* DirListSW */
   CWWidth | CWHeight | CWX | CWY,  691,    0,   15,  135, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  139,  687,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  687,  135, 0,0,0, /* DirList */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

  exit(0);
}
