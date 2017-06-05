/*
Received: from magi.com for rwscott@magi.com
 with iSTAR POP Server istar-vpopper (v1.13 12.2.1996) Sat Dec 13 22:53:25 1997
X-From_: lesstif-request@hungry.com Sat Dec 13 18:45:01 1997
Received: from terror.hungry.com [169.131.1.215
	by mail.magi.com with esmtp (Exim 1.80 #5)
	id 0xh1Ey-00054H-00; Sat, 13 Dec 1997 18:45:00 -0500
Received: (from slist@localhost
	by terror.hungry.com (8.8.7/8.8.7) id PAA05965;
	Sat, 13 Dec 1997 15:40:29 -0800 (PST)
Resent-Date: Sat, 13 Dec 1997 15:40:29 -0800 (PST)
Date: Sat, 13 Dec 1997 17:35:32 -0600 (CST)
From: "Jon A. Christopher" <jon@quorum.tamu.edu>
To: Lesstif Mailing List <lesstif@Hungry.COM>
Subject: bugs in torn-off menus.
Message-ID: <Pine.SGI.3.95.971213153800.21283A-100000@quorum.tamu.edu>
MIME-Version: 1.0
Content-Type: TEXT/PLAIN; charset=US-ASCII
Resent-Message-ID: <"qmhA-D.A.SWB.Ywxk0"@terror.hungry.com>
Resent-From: lesstif@Hungry.COM
X-Mailing-List: <lesstif@Hungry.COM> archive/latest/702
X-Loop: lesstif@Hungry.COM
Precedence: list
Resent-Sender: lesstif-request@Hungry.COM
Resent-Bcc:

Hello,

The behavior of torn off menus is not quite right.  Specifically, the
pixmap of ToggleButtons is not updated *the first time* the button is
clicked after it's torn off.  After that, it *is* updated, but this has
the effect of having the pixmap exactly out of sync with the true state of
the button, because of the first failed update.

Further, if a menu is torn off, dragging over the cascade button for
that menu in it's parent menu par unposts the torn off version, which is
different from SGI's motif at least.

It appears that the arm callback is not being called when the menu is torn
off.  This may be the cause of the problem.

The test program below demonstrates the problem.  Pay particular attention
to the "State of" messages when the menu is torn off.

By the way, is anyone working on trying to get tear off menus for more
than the first level?


-jon
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/RowColumnP.h>
#include <Xm/PushBP.h>
#include <Xm/ToggleB.h>
#include <Xm/CascadeBP.h>
#if (XmVersion>=1002)		/* motif 1.2 or higher */
#include <Xm/RepType.h>
#endif

#include "../../common/Test.h"

static String fallbacks[]= {
  "*tearOffModel: TEAR_OFF_ENABLED",
  NULL
};

void toggle_cb(Widget w, XtPointer client, XtPointer call)
{
  printf("State of %s is %s\n",XtName(w),
	 (XmToggleButtonGetState(w)?"On":"Off"));
}

void pb_activate_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    unsigned char x;

    XtVaGetValues(XtParent(w), XmNpacking, &x, NULL);
    fprintf(stderr, "* Widget = %s - Activated\n", XtName(w));
    fprintf(stderr, "# RC Packing is %d\n", x);
}

void pb_arm_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    fprintf(stderr, "* Widget = %s - Armed\n", XtName(w));
}

void pb_disarm_callback(Widget w, XtPointer clientData, XtPointer callData)
{
    fprintf(stderr, "* Widget = %s - Disarm\n", XtName(w));
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc;
    Widget cascade1, cascade2, cascade3;
    Widget pane1, pane2, pane3;
    Widget button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test7", NULL, 0,
				 &argc, argv, fallbacks, NULL);

#if (XmVersion>=1002)		/* motif 1.2 or higher */
    XmRepTypeInstallTearOffModelConverter();
#endif
    rc = XmCreateMenuBar(toplevel,
			 "menubar",
			 NULL, 0);

    pane1 = XmCreatePulldownMenu(rc,
				 "pane1",
				 NULL, 0);

    pane2 = XmCreatePulldownMenu(rc,
				 "pane2",
				 NULL, 0);

    cascade1 = XtVaCreateManagedWidget("File",
				       xmCascadeButtonWidgetClass,
				       rc,
				       XmNsubMenuId, pane1,
				       XmNmnemonic, 'F',
				       NULL);

    cascade2 = XtVaCreateManagedWidget("Edit",
				       xmCascadeButtonWidgetClass,
				       rc,
				       XmNsubMenuId, pane2,
				       XmNmnemonic, 'E',
				       NULL);

    button1 = XtVaCreateManagedWidget("Quit",
				      xmPushButtonWidgetClass,
				      pane1,
				      XmNmnemonic, 'Q',
				      NULL);

    pane3 = XmCreatePulldownMenu(pane1,
                                 "pane3",
                                 NULL, 0);

    cascade3 = XtVaCreateManagedWidget("cascade3",
				       xmCascadeButtonWidgetClass,
				       pane1,
				       XmNsubMenuId, pane3,
				       NULL);
                                       
    button2 = XtVaCreateManagedWidget("button2",
				      xmPushButtonWidgetClass,
				      pane3,
				      NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      pane3,
				      NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmToggleButtonWidgetClass,
				      pane2,
				      NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmToggleButtonWidgetClass,
				      pane2,
				      NULL);

    XtAddCallback(button1, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button2, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button3, XmNactivateCallback, pb_activate_callback, NULL);
    XtAddCallback(button4, XmNvalueChangedCallback, toggle_cb,  NULL); 
    XtAddCallback(button5, XmNvalueChangedCallback, toggle_cb,  NULL); 
    XtAddCallback(button1, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button2, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button3, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button4, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button5, XmNarmCallback, pb_arm_callback, NULL);
    XtAddCallback(button1, XmNdisarmCallback, pb_disarm_callback, NULL);
    XtAddCallback(button2, XmNdisarmCallback, pb_disarm_callback, NULL);
    XtAddCallback(button3, XmNdisarmCallback, pb_disarm_callback, NULL);
    XtAddCallback(button4, XmNdisarmCallback, pb_disarm_callback, NULL);
    XtAddCallback(button5, XmNdisarmCallback, pb_disarm_callback, NULL);

    XtManageChild(rc);

    XtRealizeWidget(toplevel);

    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,   90,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,   45,    5,   40,   21, 0,0,0, /* Edit */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );
   exit(0);
   
}
