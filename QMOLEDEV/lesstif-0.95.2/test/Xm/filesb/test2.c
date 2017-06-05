/* $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test2.c,v 1.10 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"
#include "mkdirtree.h"

static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

void cb(Widget w, XtPointer client, XtPointer call)
{
  XmString *st1, *st2, *st3, t1, t2, test;
  int c1, c2, c3, i;
  XmFileSelectionBoxWidget box = (XmFileSelectionBoxWidget)w;
  char *txt;

  XtVaGetValues((Widget)box, XmNdirListItems, &st1, XmNdirListItemCount, &c1, NULL);
  XtVaGetValues((Widget)box, XmNfileListItems, &st2, XmNfileListItemCount, &c2, NULL);
  XtVaGetValues((Widget)box, XmNlistItems, &st3, XmNlistItemCount, &c3, NULL);
  printf("c1 c2 c3 %d %d %d\n", c1, c2, c3);
  for (i = 0; i < c1; i++) {
	XmStringGetLtoR(st1[i], XmFONTLIST_DEFAULT_TAG, &txt);
	printf("dirList[%d] = '%s'\n", i, txt);
  }
  for (i = 0; i < c2; i++) {
	XmStringGetLtoR(st2[i], XmFONTLIST_DEFAULT_TAG, &txt);
	printf("fileList[%d] = '%s'\n", i, txt);
  }
  for (i = 0; i < c3; i++) {
	XmStringGetLtoR(st3[i], XmFONTLIST_DEFAULT_TAG, &txt);
	printf("listList[%d] = '%s'\n", i, txt);
  }
  t2 = NULL;

  t1 = NULL; txt = NULL;

  XtVaGetValues((Widget)box, XmNpattern, &t1, XmNtextString, &t2, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("pattern: %s\n", txt);
  if (t2) {
    XmStringGetLtoR(t2, XmFONTLIST_DEFAULT_TAG, &txt);
    printf("text: %s\n", txt);
  } else
    printf("text - problem !\n");

  t1 = NULL; txt = NULL;

  XtVaGetValues((Widget)box, XmNdirectory, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("directory: %s\n", txt);

  t1 = NULL; txt = NULL;

  XtVaGetValues((Widget)box, XmNdirMask, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirMask: %s\n", txt);

  t1 = NULL; txt = NULL;

  XtVaGetValues((Widget)box, XmNdirSpec, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirSpec: %s\n", txt);

  test = XmStringCreateSimple("test it out");

  t1 = NULL; txt = NULL;

  XtVaSetValues((Widget)box, XmNtextString, test, NULL);
  XtVaGetValues((Widget)box, XmNdirSpec, &t1, NULL);
  XmStringGetLtoR(t1, XmFONTLIST_DEFAULT_TAG, &txt);
  printf("dirSpec: %s\n", txt);
}

void
pushme(Widget w, XtPointer client, XtPointer call) {
  XtManageChild(client);
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget toplevel, box, push;

  make_tmp_dir_tree();


  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, FallBack, NULL);

  push = XmCreatePushButton(toplevel, "push", NULL, 0);
  XtVaSetValues(push,
		XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

  box = XmCreateFileSelectionDialog(toplevel, "Box", NULL, 0);
  XtAddCallback(box, XmNokCallback, cb, NULL);
  set_path(box);
  XtAddCallback(push, XmNactivateCallback, pushme, box);

  XtManageChild(push);

  XtRealizeWidget(toplevel);

  LessTifTestWaitForIt(toplevel);
  LessTifTestPushButton(push);

  
  
    
{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  166,  198,  278,  372, 0,0,0, /* Box */},
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
   CWWidth | CWHeight            ,    6,   22,  286,  374, 0,0,0, /* Box */
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
    PrintDetails(box,Expected);
};
  LessTifTestMainLoop(toplevel);

  XtAppMainLoop(app);
  exit(0);
}
