/* $Header: /cvsroot/lesstif/lesstif/test/Xm/selectionbox/test10.c,v 1.4 2001/06/18 14:05:31 amai Exp $
From:        "Randall L. Ricklefs" <rlr@almagest.as.utexas.edu>
To:          lesstif@hungry.com
Subject:     Selection Box Bug
Date:        Tue, 29 Jun 1999 10:49:34 -0500 (CDT)
cc:          "Randall L. Ricklefs" <rlr@almagest.as.utexas.edu>

Greetings!

The Selection box widget in Lesstif 88.1 will not allow me to unmanage the 
dialog list, which I can do under Motif .nd is a documented feature. As a 
test program, I have included a modified version of SelectionBox test6.c. 
When run as "test6 4" it seg faults when the main window button is pushed 
and gives the following trace.

#0  0x402b2a22 in bcmp ()
#1  0x40035b48 in geometry_manager (w=0x806cc40, desired=0xbffff514, 
    allowed=0x0) at BulletinBoard.c:913
#2  0x4006f36c in _XmHandleGeometryManager (w=0x806be88, instigator=0x806cc40, 
    desired=0xbffff514, allowed=0xbffff4dc, policy=2 '\002', 
    cachePtr=0x806bfbc, 
    createMatrix=0x400dc304 <_XmSelectionBoxGeoMatrixCreate>)
    at GeoUtils.c:2002
#3  0x4003598c in handle_geometry_manager (w=0x806cc40, desired=0xbffff514, 
    allowed=0xbffff4dc, mat_make=0x400dc304 <_XmSelectionBoxGeoMatrixCreate>)
    at BulletinBoard.c:888
#4  0x40035bb6 in geometry_manager (w=0x806cc40, desired=0xbffff514, 
    allowed=0xbffff4dc) at BulletinBoard.c:930
#5  0x40033b98 in GeometryHandlerWrapper (w=0x806cc40, request=0xbffff514, 
    reply=0xbffff4dc, IntentedWrapperDepth=7) at BaseClass.c:1323
#6  0x40032e6b in GeometryHandlerWrapper7 (w=0x806cc40, request=0xbffff514, 
    reply=0xbffff4dc) at BaseClass.c:590
#7  0x4019ebde in _XtMakeGeometryRequest ()
#8  0x4019ef02 in XtMakeGeometryRequest ()
#9  0x4006f5e0 in _XmMakeGeometryRequest (w=0x806cc40, desired=0xbffff514)
    at GeoUtils.c:2102
#10 0x400d5550 in realize (w=0x806cc40, value_mask=0xbffff574, 
    attributes=0xbffff578) at ScrolledW.c:1036
#11 0x401a21bb in RealizeWidget ()
#12 0x401a2375 in RealizeWidget ()
#13 0x401a2464 in XtRealizeWidget ()
#14 0x40047633 in change_managed (w=0x806a3e8) at DialogS.c:340
#15 0x401a5153 in ManageChildren ()
#16 0x401a530e in XtManageChildren ()
#17 0x401a53fb in XtManageChild ()
#18 0x80496d8 in pushme (w=0x80688f8, client=0x0, call=0xbffff910)
    at test6.c:109
#19 0x40190b24 in XtCallCallbackList ()
#20 0x400ae6e0 in Activate (w=0x80688f8, event=0xbffffabc, params=0x0, 
    num_params=0x805b370) at PushB.c:1070
#21 0x401bde9b in HandleActions ()
#22 0x401be81e in HandleComplexState ()
#23 0x401be8db in _XtTranslateEvent ()
#24 0x4019bcbf in XtDispatchEventToWidget ()
#25 0x4019c6fd in _XtDefaultDispatcher ()
#26 0x4019c975 in XtDispatchEvent ()
#27 0x4019cd5d in XtAppMainLoop ()
#28 0x804b050 in LessTifTestMainLoop (w=0x805ce30) at Test.c:507
#29 0x80497c6 in main (argc=2, argv=0xbffffb58) at test6.c:141
(gdb) 

The test program, as modified:
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/SelectioB.h>

#include  "../../common/Test.h"


XtAppContext app;
Widget toplevel, box, push, tf;

int which = 0;

void
ok(Widget w, XtPointer client, XtPointer call)
{
	XmSelectionBoxCallbackStruct *p = (XmSelectionBoxCallbackStruct *)call;
	char	*s = NULL;

	XmStringGetLtoR(p->value, XmSTRING_DEFAULT_CHARSET, &s);
	fprintf(stderr, "Ok '%s'\n", s);
}

void
activate(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Activated\n");
}

void
pushme(Widget w, XtPointer client, XtPointer call)
{
	int argc;
	Arg args[3];
	Widget menu, pane1, cascade1, but;

	box = XmCreateSelectionDialog(toplevel, "Box", NULL, 0);
	but=XmSelectionBoxGetChild(box, XmDIALOG_OK_BUTTON);
	if (but)
		XtUnmanageChild(but);
	but=XmSelectionBoxGetChild(box, XmDIALOG_HELP_BUTTON);
	if (but)
		XtUnmanageChild(but);
	but=XmSelectionBoxGetChild(box, XmDIALOG_CANCEL_BUTTON);
	if (but)
		XtUnmanageChild(but);
	but=XmSelectionBoxGetChild(box, XmDIALOG_APPLY_BUTTON);
	if (but)
		XtUnmanageChild(but);
	but=XmSelectionBoxGetChild(box, XmDIALOG_LIST);
	if (but)
		XtUnmanageChild((but));
printf("but=%d %s\n",but, XtName(but));
	XtVaCreateManagedWidget("button0", xmPushButtonWidgetClass,
				box, NULL);
	XtManageChild(box);
}

int
main(int argc, char **argv)
{
	if (argc > 1)
	    which = atoi(argv[1]);

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
		&argc, argv, NULL, NULL);

	push = XmCreatePushButton(toplevel, "push", NULL, 0);
	XtVaSetValues(push,
			XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);

	XtAddCallback(push, XmNactivateCallback, pushme, NULL);

	XtManageChild(push);

	XtRealizeWidget(toplevel);
	LessTifTestWaitForIt(toplevel);
	LessTifTestPushButton(push);
	LessTifTestWaitForIt(box);

  

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  300,  266,  160,  160, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  138,   17, 0,0,0, /* Items */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    1,    1, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,   11,   89,  138,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   11,  106,  138,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,  147,  160,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   38,  138,   41, 0,0,0, /* button0 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(box, Expected);
}
LessTifTestMainLoop(toplevel);
	exit(0);
}

/*
Diff from original:

--- /prod/contrib/src/lesstif-0.88.1/test/Xm/selectionbox/test6.c	Tue Jun 29 08:26:06 1999
+++ test6.c	Tue Jun 29 08:25:26 1999
@@ -101,7 +101,6 @@
 		if (but)
 			XtUnmanageChild(but);
 		but=XmSelectionBoxGetChild(box, XmDIALOG_LIST);
-		but=XmSelectionBoxGetChild(box, XmDIALOG_LIST_LABEL);
 		if (but)
 			XtUnmanageChild(but);
 printf("but=%d\n",but);

Any ideas?

Thanks.

Randy

P.S. Your team is doing a great job! Thanks for all your efforts.

 -----------------------    Randy Ricklefs    ------------------------
                       rlr@astro.as.utexas.edu
              McDonald Observatory,  University of Texas
   Phone:(512) 471-1342    Austin TX 78712       FAX:  (512)471-6016



*/
