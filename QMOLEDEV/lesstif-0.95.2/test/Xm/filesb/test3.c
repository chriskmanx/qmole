/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test3.c,v 1.16 2002/05/01 15:39:21 amai Exp $
 *
 * Test initially created to simulate a problem in nedit.
 * Then augmented to observe callback structures.
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"

#include "mkdirtree.h"

static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

XtAppContext app;
Widget toplevel, box, push;

int	popdown;


static char *
XdbXmString2String(XmString xms)
{
    char *s = NULL;

#ifdef LesstifVersion
    if (xms == (XmString)XmUNSPECIFIED)
    {
	return "XmUNSPECIFIED";
    }
#endif
    XmStringGetLtoR(xms, XmFONTLIST_DEFAULT_TAG, &s);

    if (s == NULL)
    {
	return "(null)";
    }

    return s;
}

void
cb(Widget w, XtPointer client, XtPointer call)
{
	XmFileSelectionBoxCallbackStruct *cbp =
		(XmFileSelectionBoxCallbackStruct *)call;

	/* Print stuff */
	fprintf(stderr, "Callback Structure :\n");
	/* rws can't use the Xdb functions in test code unless they are
	   duplicated here!!!!
	fprintf(stderr, "\tReason %s, event %p\n",
		XdbReason2String(cbp->reason),
		cbp->event);
		*/
	fprintf(stderr, "\tvalue '%s', length %d\n",
		XdbXmString2String(cbp->value),
		cbp->length);
	fprintf(stderr, "\tmask '%s', length %d\n",
		cbp->mask ? XdbXmString2String(cbp->mask) : "(null)",
		cbp->mask_length);
	fprintf(stderr, "\tdir '%s', length %d\n",
		cbp->dir ? XdbXmString2String(cbp->dir) : "(null)",
		cbp->dir_length);
	fprintf(stderr, "\tpattern '%s', length %d\n",
		cbp->pattern ? XdbXmString2String(cbp->pattern) : "(null)",
		cbp->pattern_length);

	/* Trigger popping down */
	popdown = 1;
}

void
pushme(Widget w, XtPointer client, XtPointer call)
{
	box = XmCreateFileSelectionDialog(toplevel, "Box", NULL, 0);
	XtAddCallback(box, XmNokCallback, cb, NULL);

	popdown = 0;
	XtManageChild(box);
	set_path(box);
	
    
{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  202,  167,  278,  372, 0,0,0, /* Box */},
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
	/*
	while (popdown == 0) {
	        XtAppProcessEvent(XtWidgetToApplicationContext(w), XtIMAll);
	}

	fprintf(stderr, "Destroying the FSB\n");
	XtDestroyWidget(box);
	*/
}

int
main(int argc, char **argv)
{

  make_tmp_dir_tree();
	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
		&argc, argv, FallBack, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
			XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

	XtAddCallback(push, XmNactivateCallback, pushme, NULL);

	XtManageChild(push);

	XtRealizeWidget(toplevel);

  LessTifTestWaitForIt(toplevel);
  LessTifTestPushButton(push);


  LessTifTestMainLoop(toplevel);

	exit(0);
}
