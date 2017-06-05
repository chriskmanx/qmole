/* $Id: test18.c,v 1.4 2001/05/15 14:20:18 amai Exp $ */

#if 0
From:        "Eric C. Newton" <ecn@smart.net>
To:          Rick Scott <rwscott@omnisig.com>
Subject:     Re: mem overwrite in MessageB.c
Date:        Sun, 15 Nov 1998 08:40:15 -0500
Cc:          lesstif@hungry.com


Rick Scott writes:
 > I would be interested in seeing the intimate details of how your 
 > MessageBox is put together.  _XmGeoMatrixAlloc does alloc nrows + 1, so
 > it would seem that in your case the number of rows that we are calculating
 > is not the same as the number of rows that we are laying out to.  If you
 > could rip the relevent MessageBox code out into a separate little app, like
 > the existing tests, it would help alot.

My guess is that it is off by one because I am unmanaging the Ok button. 
Here is the application stripped down to the minimum.

-Eric
#endif


#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/MessageB.h>
#include <Xm/Form.h>

#include "../../common/Test.h"

static Widget	dialog = NULL;

static Widget CreateConfigWindow (Widget w)
{

    if (! dialog)
    {
	Widget	form;
	dialog = XmCreateMessageDialog (w, "ConfigWindow", 0, 0);
	XtUnmanageChild (XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON));
	form = XmCreateForm (dialog, "form", 0, 0);
	XtManageChild (form);
	XtManageChild (dialog);
    }
    return (dialog);
}

int main(int argc, char**argv)
{
    Widget top_level;
    void * foo = malloc(10);	/* reference malloc to get efence */
    Widget pb;

    top_level = XtVaAppInitialize (NULL, "Testit", NULL, 0,
				   &argc, argv, NULL, NULL);
    pb = XmCreatePushButton(top_level, "test", 0, 0);
    XtAddCallback(pb, XmNactivateCallback, (XtCallbackProc)CreateConfigWindow, NULL);
    XtManageChild(pb);
    XtRealizeWidget(top_level);
    LessTifTestWaitForIt(top_level);
    LessTifTestPushButton(pb);

    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    6,   57,  154,  102, 0,0,0, /* ConfigWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  132,    4, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   36,  154,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   48,   66,   43, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,   77,   48,   66,   43, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   25,  132,    1, 0,0,0, /* form */ 
    };
    PrintDetails(dialog,Expected);
};
    LessTifTestMainLoop(top_level);

    return 0;
}


