/*
 $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test10.c,v 1.10 2002/05/01 15:39:21 amai Exp $
From:        Karsten Jensen <kbwj@diku.dk>
To:          lesstif@hungry.com
Subject:     Re: FileSelection patch
Date:        Mon, 29 Jun 1998 16:26:12 +0000

Rick Scott wrote:
> 
> I have to disagree with the changes to FileSB.c.  As you click the mouse
> in the various elements the default button does not change, hence the use
> of the dynamic default button.  The default button remains the same only the
> result of pressing return changes.  If the problem is anywhere I suspect it
> is in FileSB.c, since it is the only widget with this behaviour.  Any of the
> other dialogs pressing return will activate the default button.  Only
> FileSB does not always press the default button when return is pressed.



Rick, try this little program.

Karsten

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

void
cb(Widget w, XtPointer a, XtPointer b) {
    Widget fs = (Widget) a;
    Widget default_button;

    XtVaGetValues(fs, XmNdefaultButton, &default_button, NULL);
    printf("%s The name of the default_button is now %s %s\n", XtName(w),
	   XrmQuarkToString(default_button->core.xrm_name), XtName(default_button));


}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box, filter_text, text, list, dir_list;
  make_tmp_dir_tree();

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, FallBack, NULL);

  box=XmCreateFileSelectionBox(toplevel,"Box",NULL,0);
  set_path(box);
  XtAddCallback(box, XmNfocusCallback, cb, box);
  filter_text = XmFileSelectionBoxGetChild(box, XmDIALOG_FILTER_TEXT);
  XtAddCallback(filter_text, XmNfocusCallback, cb, box);
  text = XmFileSelectionBoxGetChild(box, XmDIALOG_TEXT);
  XtAddCallback(text, XmNfocusCallback, cb, box);
  dir_list = XmFileSelectionBoxGetChild(box, XmDIALOG_DIR_LIST);
  XtAddCallback(dir_list, XmNbrowseSelectionCallback, cb, box);
  list = XmFileSelectionBoxGetChild(box, XmDIALOG_LIST);
  XtAddCallback(list, XmNbrowseSelectionCallback, cb, box);

  XtAddCallback(box, XmNokCallback, cb, NULL);

  XtManageChild(box);

  XtRealizeWidget(toplevel);

  
    
{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  278,  372, 0,0,0, /* Box */},
   {CWWidth | CWHeight | CWX | CWY,  183,   69,   83,   17, 0,0,0, /* Items */},
   {CWWidth | CWHeight | CWX | CWY,  183,   86,   83,  154, 0,0,0, /* ItemsListSW */},
   {CWWidth | CWHeight | CWX | CWY,   68,    0,   15,  135, 0,0,0, /* VertScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,  139,   64,   15, 0,0,0, /* HorScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   64,  135, 0,0,0, /* ItemsList */},
   {CWWidth | CWHeight | CWX | CWY,   11,  250,  256,   17, 0,0,0, /* Selection */},
   {CWWidth | CWHeight | CWX | CWY,   11,  267,  256,   31, 0,0,0, /* Text */},
   {CWWidth | CWHeight | CWX | CWY,    0,  308,  278,    2, 0,0,0, /* Separator */},
   {CWWidth | CWHeight | CWX | CWY,   11,  320,   64,   41, 0,0,0, /* OK */},
   {CWWidth | CWHeight | CWX | CWY,   75,  320,   64,   41, 0,0,0, /* Apply */},
   {CWWidth | CWHeight | CWX | CWY,  139,  320,   64,   41, 0,0,0, /* Cancel */},
   {CWWidth | CWHeight | CWX | CWY,  203,  320,   64,   41, 0,0,0, /* Help */},
   {CWWidth | CWHeight | CWX | CWY,   11,   11,  256,   17, 0,0,0, /* FilterLabel */},
   {CWWidth | CWHeight | CWX | CWY,   11,   69,  162,   17, 0,0,0, /* Dir */},
   {CWWidth | CWHeight | CWX | CWY,   11,   28,  256,   31, 0,0,0, /* FilterText */},
   {CWWidth | CWHeight | CWX | CWY,   11,   86,  162,  154, 0,0,0, /* DirListSW */},
   {CWWidth | CWHeight | CWX | CWY,  147,    0,   15,  135, 0,0,0, /* VertScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,  139,  143,   15, 0,0,0, /* HorScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,  143,  135, 0,0,0, /* DirList */},
};
#else
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  286,  374, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,  191,   69,   83,   17, 0,0,0, /* Items */
   CWWidth | CWHeight | CWX | CWY,  191,   86,   83,  154, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,   68,    0,   15,  135, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  139,   64,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   64,  135, 0,0,0, /* ItemsList */
   CWWidth | CWHeight | CWX | CWY,   11,  250,  264,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   11,  267,  264,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,  308,  286,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  320,   66,   43, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   77,  320,   66,   43, 0,0,0, /* Apply */
   CWWidth | CWHeight | CWX | CWY,  143,  320,   66,   43, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  209,  320,   66,   43, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  264,   17, 0,0,0, /* FilterLabel */
   CWWidth | CWHeight | CWX | CWY,   11,   69,  170,   17, 0,0,0, /* Dir */
   CWWidth | CWHeight | CWX | CWY,   11,   28,  264,   31, 0,0,0, /* FilterText */
   CWWidth | CWHeight | CWX | CWY,   11,   86,  170,  154, 0,0,0, /* DirListSW */
   CWWidth | CWHeight | CWX | CWY,  155,    0,   15,  135, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  139,  151,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  151,  135, 0,0,0, /* DirList */ 
};
#endif
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

