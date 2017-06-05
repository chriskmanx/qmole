/* $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test7.c,v 1.11 2002/05/01 15:39:21 amai Exp $ */

/*
Date: Sat, 13 Dec 1997 14:44:46 -0600 (CST)
From: "Jon A. Christopher" <jon@quorum.tamu.edu>
To: Lesstif Mailing List <lesstif@Hungry.COM>
Subject: improper geometry handling in FileSelectionDialog
Message-ID: <Pine.SGI.3.95.971213143416.21158A-100000@quorum.tamu.edu>
MIME-Version: 1.0
Content-Type: TEXT/PLAIN; charset=US-ASCII
Resent-Message-ID: <"yI1WnC.A.GQF.QQvk0"@terror.hungry.com>
Resent-From: lesstif@Hungry.COM
X-Mailing-List: <lesstif@Hungry.COM> archive/latest/700
X-Loop: lesstif@Hungry.COM
Precedence: list
Resent-Sender: lesstif-request@Hungry.COM
Resent-Bcc:

Hello,

I've noticed that FileSelectionDialogs do not handle geometry correctly
under certain cases.  If there's a menubar in the dialog, but no managed
buttons in the menu bar, any action (filter, double clicking a directory,
etc) causes the dialog to grow larger.  SGI Motif 1.2 doesn't exhibit this
behavior.  

In the attached sample code, first press Cancel in the dialog. This unmanages the
button in the menubar.  Next press filter, or double click on a directory
name, and watch the dialog box grow!

Stepping throught the code for FileSB.c, I've isolated the problem to a
XtSetValues call in defaultDirSearchProc(), but it may simply be showing
up there because the display is only updated at that time.  Sorry, no
patch.

-jon
*/

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
Widget menubar,button;

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
   {CWWidth | CWHeight            ,   57,   73,   34,   17, 0,0,0}, /* Label */ 
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

static void CancelCB()
{
  if (XtIsManaged(button))
    XtUnmanageChild(button);
  else
    XtManageChild(button);
}

Widget
GXCreateFileBox(Widget parent)
/*-
 * create a FileSelectionBox child of parent  
-*/
{
    Arg args[2];
    int n = 0;
    Widget filew, menu;
    Widget work;

    n = 0;
    /* XtSetArg(args[n], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL);  n++; */
    filew = XmCreateFileSelectionDialog(parent, "FileBox", args, n);
    XtAddCallback(filew,XmNcancelCallback, (XtCallbackProc)CancelCB, NULL);
    set_path(filew);
    /* these calls are here to force the menu bar to the top, but it fails
       under lesstif */
    work = XmCreateRowColumn(filew, "FileBoxWork", NULL, 0);
    XtManageChild(work);
    work = XmCreateSeparator(filew, "Separator", NULL, 0);
    XtManageChild(work);
    XtManageChild(menu=XmCreateMenuBar(filew, "Menu Bar",NULL,0));
    XtManageChild(button=XmCreateCascadeButton(menu,"Button",NULL,0));
    menubar=menu;
    return (filew);
}
